
/******************************************************************************
* MODULE     : ns_utilities.mm
* DESCRIPTION: Utilities for Aqua
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "ns_utilities.h"
#include "ns_widget.h"
#include "dictionary.hpp"
#include "converter.hpp"
#include "analyze.hpp"
#include "wencoding.hpp"
#include "hashset.hpp"
#include "hashmap.hpp"
#include "tm_timer.hpp"
#include <dlfcn.h>
#include <cxxabi.h>
#include <math.h>
#include <stdlib.h>

#define SCREEN_PIXEL (PIXEL)
// (a double: a float lost the points of a tall document, #661)
const double invpix =  1.0/SCREEN_PIXEL;

coord4 from_nsrect (NSRect rect)
{
  // A rectangle of a flipped view (the y coordinate goes down), as
  // from_qrect in the Qt interface
  SI c1, c2, c3, c4;
  c1 = rect.origin.x*SCREEN_PIXEL;
  c2 = -(rect.origin.y+rect.size.height)*SCREEN_PIXEL;
  c3 = (rect.origin.x+rect.size.width)*SCREEN_PIXEL;
  c4 = -rect.origin.y*SCREEN_PIXEL;
  return coord4 (c1, c2, c3, c4);
}

NSRect to_nsrect(coord4 p)
{
  // (the sizes in double: those of extents beyond +-2^30 overflowed an SI)
  return NSMakeRect (p.x1*invpix, -p.x4*invpix,
                     ((double) p.x3 - p.x1 + SCREEN_PIXEL - 1)*invpix,
                     ((double) p.x4 - p.x2 + SCREEN_PIXEL - 1)*invpix);
}

NSPoint to_nspoint (coord2 p)
{
	return NSMakePoint (p.x1*invpix, -p.x2*invpix);
}

NSSize to_nssize (coord2 p)
{
	return NSMakeSize (p.x1*invpix, p.x2*invpix);
}

NSSize to_nssize (SI w, SI h)
{
  return NSMakeSize (w*invpix, h*invpix);
}

coord2 from_nspoint (NSPoint pt)
{
	SI c1, c2;
	c1 = pt.x*SCREEN_PIXEL;
	c2 = -pt.y*SCREEN_PIXEL;
	return coord2 (c1,c2)	;
}

coord2 from_nssize (NSSize s)
{
	SI c1, c2;
	c1 = s.width*SCREEN_PIXEL;
	c2 = s.height*SCREEN_PIXEL;
	return coord2 (c1,c2)	;
}

/******************************************************************************
 * Warnings for the conversions of strings (#621)
 ******************************************************************************/

// a string as it can be printed: the bytes which are not printable ASCII as
// \xNN, at most 160 of them
static string
ns_escaped (string s) {
  static const char* hex= "0123456789abcdef";
  string r;
  int n= min (N(s), 160);
  for (int i= 0; i < n; i++) {
    unsigned char c= (unsigned char) s[i];
    if (c >= 32 && c < 127 && c != '\\') r << ((char) c);
    else r << "\\x" << hex[c >> 4] << hex[c & 15];
  }
  if (n < N(s)) r << "...";
  return r;
}

// the function of the code at addr (the caller of a conversion), if known
static string
ns_function_at (void* addr) {
  Dl_info info;
  if (addr == NULL || dladdr (addr, &info) == 0 || info.dli_sname == NULL)
    return "?";
  int status= 0;
  char* d= abi::__cxa_demangle (info.dli_sname, NULL, NULL, &status);
  string r (status == 0 && d != NULL ? d : info.dli_sname);
  if (d != NULL) free (d);
  return r;
}

// a conversion of a string which failed, or which took its fallback: once
// for each function and string (and at most 50 times), on the channel of
// the debug messages of this interface (the terminal and the console)
static void
ns_conversion_warning (const char* fun, const char* what, string s,
                       void* caller) {
  static hashset<string> seen;
  static int count= 0;
  // (the count first: the set does not grow once the warnings stop)
  if (count > 50) return;
  if (count == 50) {
    count++;
    debug_aqua << "further warnings for the conversions of strings are "
               << "not shown\n";
    return;
  }
  string key= string (fun) * ":" * s;
  if (seen->contains (key)) return;
  seen->insert (key);
  count++;
  debug_aqua << fun << ": " << what << ", for \"" << ns_escaped (s)
             << "\" (called from " << ns_function_at (caller) << ")\n";
}

static string
ns_lossy (NSString* s) {
  NSData* d= [s dataUsingEncoding: NSUTF8StringEncoding
              allowLossyConversion: YES];
  if (d == nil) return "";
  return string ((const char*) [d bytes], (int) [d length]);
}

/******************************************************************************
 * Conversions of strings
 ******************************************************************************/

/*! UTF-8, or else the cork encoding (as the translations, which are in the
 cork encoding: see #468); never nil, which AppKit does not take */
NSString *to_nsstring (string s)
{
	c_string p = c_string (s);
	NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
	if (!nss) {
	  // (a string in the cork encoding where UTF-8 is expected: its caller
	  // should use to_nsstring_utf8 or to_label)
	  ns_conversion_warning ("to_nsstring", "not UTF-8, taken as cork", s,
	                         __builtin_return_address (0));
	  c_string q = c_string (cork_to_utf8 (s));
	  nss = [NSString stringWithCString:q encoding:NSUTF8StringEncoding];
	}
	if (!nss || ([nss length] == 0 && N(s) > 0))
	  ns_conversion_warning ("to_nsstring", "no text for it", s,
	                         __builtin_return_address (0));
	return nss? nss: @"";
}

string from_nsstring(NSString *s)
{
	const char *cstr = s? [s cStringUsingEncoding:NSUTF8StringEncoding]: NULL;
	if (s && !cstr)
	  ns_conversion_warning ("from_nsstring", "not convertible to UTF-8",
	                         ns_lossy (s), __builtin_return_address (0));
	return cstr? utf8_to_cork(string((char*)cstr)): string ("");
}

/*! The UTF-8 bytes, without conversion to the cork encoding: for the names
 of files (as in the Qt interface, see also mac_app.mm) */
string
from_nsstring_utf8 (NSString *s) {
  if (!s) return "";
  const char *cstr= [s UTF8String];
  if (!cstr)
    ns_conversion_warning ("from_nsstring_utf8", "not convertible to UTF-8",
                           ns_lossy (s), __builtin_return_address (0));
  return cstr? string ((char*) cstr): string ("");
}


/*! From the cork encoding, or UTF-8 already (the names of files may be in
 both: see to_qstring in the Qt interface) */
NSString *to_nsstring_utf8(string s)
{
  if (!looks_utf8 (s) || looks_ascii (s) || looks_universal (s))
    s = cork_to_utf8 (s);
  c_string p = c_string (s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  if (!nss || ([nss length] == 0 && N(s) > 0))
    ns_conversion_warning ("to_nsstring_utf8", "no text for it", s,
                           __builtin_return_address (0));
  return nss? nss: @"";
}

string
ns_translate (string s) {
  string out_lan= get_output_language ();
  return tm_var_encode (translate (s, "english", out_lan));
}


tm_ostream&
operator << (tm_ostream& out, NSRect rect) {
  return out << "(" << rect.origin.x << "," << rect.origin.y << ","
  << rect.size.width << "," << rect.size.height << ")";
}

tm_ostream&
operator << (tm_ostream& out, NSSize size) {
  return out << "("  << size.width << "," << size.height << ")";
}



tm_ostream&
operator << (tm_ostream& out, coord4 c) {
  out << "[" << c.x1 << "," << c.x2 << "," << c.x3 << "," << c.x4 << "]";
  return out;
}

tm_ostream&
operator << (tm_ostream& out, coord2 c) {
  out << "[" << c.x1 << "," << c.x2 << "]";
  return out;
}

/******************************************************************************
 * Lengths of the widgets (see qt_decode_length)
 ******************************************************************************/

NSSize
ns_decode_length (string width, string height, NSSize ref) {
  // The size given by the lengths (in points), from the default size ref:
  // w and h are multiples of the default width and height, em and px are
  // absolute (an em of 14 points)
  NSSize size= ref;
  string w_unit, h_unit;
  double w_len, h_len;
  parse_length (width, w_len, w_unit);
  parse_length (height, h_len, h_unit);
  if      (w_unit == "w" ) size.width= w_len * ref.width;
  else if (w_unit == "h" ) size.width= w_len * ref.height;
  else if (w_unit == "em") size.width= 14.0 * w_len;
  else if (w_unit == "px") size.width= w_len;
  if      (h_unit == "w" ) size.height= h_len * size.width;
  else if (h_unit == "h" ) size.height= h_len * ref.height;
  else if (h_unit == "em") size.height= 14.0 * h_len;
  else if (h_unit == "px") size.height= h_len;
  if (!ns_check_size (size, "ns_decode_length")) {
    debug_aqua << "  (the lengths " << width << " and " << height << ")\n";
    // (not a number: AppKit would raise an exception)
    if (!isfinite (size.width) || !isfinite (size.height)) return ref;
  }
  return size;
}

/******************************************************************************
 * Labels of the widgets
 ******************************************************************************/

NSString*
to_label (string s) {
  // Menu and widget labels are in the cork or in the utf8 encoding
  if (looks_utf8 (s) && !(looks_ascii (s) || looks_universal (s)))
    return to_nsstring (s);
  return to_nsstring_utf8 (s);
}

string
from_label (NSString* s) {
  // Inputs are returned in the cork encoding, like in the Qt interface
  // (from_nsstring converts already)
  return from_nsstring (s);
}

/******************************************************************************
 * Exceptions caught at the entry points from AppKit (#621)
 ******************************************************************************/

extern string the_report; // (basic.cpp)

// the exceptions which come again (at each update): reported the first
// time, and then only counted, at the 10th, 100th... time, while they come
// again within ns_report_window; the key is the place, the message and the
// functions of the backtrace (see ns_report_key), so that distinct errors
// with the same generic message (such as "widget expected") are reported
// each. At most ns_report_keys keys are kept.
static const time_t ns_report_window= 10000; // ms
static const int    ns_report_keys  = 200;

// the message and the names of the functions of the backtrace of a report
// of tm_throw (get_crash_report), without what changes from one time to
// the next (the date, the system information, the status of the editor,
// the addresses and offsets)
static string
ns_report_key (string msg, string report) {
  string key= msg;
  int i= search_forwards ("Backtrace of C++ stack:", report);
  if (i < 0) return key;
  int n= N(report), frames= 0;
  i= search_forwards ("\n", i, report);
  while (i >= 0 && i < n && frames < 24) {
    int j= search_forwards ("\n", i + 1, report);
    if (j < 0) j= n;
    string line= report (i + 1, j);
    int a= search_forwards (" : ", line);
    if (a >= 0) {
      int b= search_backwards (" + ", line);
      key << "\n" << line (a + 3, b > a? b: N(line));
      frames++;
    }
    i= j;
  }
  return key;
}

static bool
ns_report_again (const char* where, string what) {
  static hashmap<string,int> seen (0);
  static hashmap<string,int> last (0);  // (as int: texmacs_time () in ms)
  static int keys= 0;
  string key= string (where) * "\n" * what;
  if (!seen->contains (key) && ++keys > ns_report_keys) {
    // (a bound: the counts start again)
    seen= hashmap<string,int> (0); last= hashmap<string,int> (0); keys= 1;
  }
  int now= (int) texmacs_time ();
  int n= seen[key] + 1;
  if (n > 1 && now - last[key] > (int) ns_report_window) n= 1;
  seen (key)= n;
  last (key)= now;
  if (n == 1) return true;
  int p= 10;
  while (p < n && p < 1000000000) p *= 10;
  if (n == p) {
    int i= 0;
    while (i < N(what) && what[i] != '\n') i++;
    failed_error << "Cocoa: the same exception again in " << where << " ("
                 << n << " times): " << what (0, i) << LF;
  }
  return false;
}

void
ns_report_exception (const char* where, NSException* e) {
  try {
    string what= from_nsstring_utf8 ([e name]) * ", " *
                 from_nsstring_utf8 ([e reason]);
    if (!ns_report_again (where, what)) return;
    failed_error << "Cocoa: an Objective-C exception was caught in " << where
                 << ": " << what << LF;
    NSArray* st= [e callStackSymbols];
    string trace;
    for (NSUInteger i= 0; i < [st count] && i < 30; i++)
      trace << "  " << from_nsstring_utf8 ([st objectAtIndex: i]) << "\n";
    if (N(trace) > 0) failed_error << "Backtrace of the exception:\n" << trace;
  }
  catch (...) {}
}

void
ns_report_exception (const char* where, string msg) {
  // (the report of tm_throw, with its backtrace, is printed already; a
  // string thrown otherwise is the message)
  try {
    if (N (the_exception) == 0) the_exception= msg;
    if (!ns_report_again (where, ns_report_key (msg, the_report))) {
      the_exception= ""; the_report= "";
      return;
    }
    failed_error << "Cocoa: an error was caught in " << where << LF;
    handle_exceptions ();
  }
  catch (...) {}
}

/******************************************************************************
 * Sizes and positions given to AppKit (#621)
 ******************************************************************************/

static const double ns_huge= 100000.0; // points

bool
ns_check_size (NSSize size, const char* where) {
  // false (and a warning) for a size which AppKit would refuse (not a
  // number) or which is surely wrong (negative, or huge)
  double w= size.width, h= size.height;
  if (isfinite (w) && isfinite (h) && w >= 0 && h >= 0 &&
      w <= ns_huge && h <= ns_huge) return true;
  debug_aqua << where << ": a bad size (" << w << ", " << h << ")\n";
  return false;
}


static bool
ns_first_time (const char* where) {
  // (a bound on the places: they are those of the source)
  static hashset<string> seen;
  string key (where);
  if (seen->contains (key) || N(seen) > 1000) return false;
  seen->insert (key);
  return true;
}

static bool
ns_bad_length (double x, double bound) {
  return !isfinite (x) || x < 0 || x > bound;
}

static double
ns_good_length (double x, double bound) {
  return !isfinite (x) || x < 0? 0.0: (x > bound? bound: x);
}

static bool
ns_bad_coordinate (double x, double bound) {
  return !isfinite (x) || fabs (x) > bound;
}

static double
ns_good_coordinate (double x, double bound) {
  return !isfinite (x)? 0.0: (x > bound? bound: (x < -bound? -bound: x));
}

bool
ns_fix_size (NSSize& s, const char* where, double bound) {
  if (!ns_bad_length (s.width, bound) && !ns_bad_length (s.height, bound))
    return false;
  NSSize r= NSMakeSize (ns_good_length (s.width, bound),
                        ns_good_length (s.height, bound));
  bool first= ns_first_time (where);
  if (first)
    debug_aqua << where << ": a bad size for AppKit (" << (double) s.width
               << ", " << (double) s.height << "), replaced by ("
               << (double) r.width << ", " << (double) r.height << ")\n";
  s= r;
  return first;
}

bool
ns_fix_point (NSPoint& p, const char* where, double bound) {
  if (!ns_bad_coordinate (p.x, bound) && !ns_bad_coordinate (p.y, bound))
    return false;
  NSPoint r= NSMakePoint (ns_good_coordinate (p.x, bound),
                          ns_good_coordinate (p.y, bound));
  bool first= ns_first_time (where);
  if (first)
    debug_aqua << where << ": a bad position for AppKit (" << (double) p.x
               << ", " << (double) p.y << "), replaced by ("
               << (double) r.x << ", " << (double) r.y << ")\n";
  p= r;
  return first;
}

NSSize
ns_safe_size (NSSize s, const char* where, double bound) {
  ns_fix_size (s, where, bound);
  return s;
}

NSPoint
ns_safe_point (NSPoint p, const char* where, double bound) {
  ns_fix_point (p, where, bound);
  return p;
}

NSRect
ns_safe_rect (NSRect r, const char* where, double bound) {
  ns_fix_point (r.origin, where, bound);
  ns_fix_size (r.size, where, bound);
  return r;
}

CGFloat
ns_safe_length (CGFloat x, const char* where, double bound) {
  if (!ns_bad_length (x, bound)) return x;
  CGFloat r= ns_good_length (x, bound);
  if (ns_first_time (where))
    debug_aqua << where << ": a bad length for AppKit (" << (double) x
               << "), replaced by " << (double) r << "\n";
  return r;
}
