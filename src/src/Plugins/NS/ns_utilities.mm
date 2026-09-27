
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

#define SCREEN_PIXEL (PIXEL)
const float invpix =  1.0/SCREEN_PIXEL;

coord4 from_nsrect (NSRect rect)
{
  SI c1, c2, c3, c4;
  c1 = rect.origin.x*SCREEN_PIXEL;
  c2 = rect.origin.y*SCREEN_PIXEL;
  c3 = (rect.origin.x+rect.size.width+SCREEN_PIXEL-1)*SCREEN_PIXEL;
  c4 = (rect.origin.y+rect.size.height+SCREEN_PIXEL-1)*SCREEN_PIXEL;
  return coord4 (c1, c2, c3, c4);
}

NSRect to_nsrect(coord4 p)
{
	return NSMakeRect (p.x1*invpix, -p.x4*invpix,
                     (p.x3-p.x1+SCREEN_PIXEL-1)*invpix, (p.x4-p.x2+SCREEN_PIXEL-1)*invpix);
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

NSString *to_nsstring (string s)
{
	c_string p = c_string (s);
	NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
	return nss;
}

string from_nsstring(NSString *s)
{
	const char *cstr = [s cStringUsingEncoding:NSUTF8StringEncoding];
	return utf8_to_cork(string((char*)cstr));
}


NSString *to_nsstring_utf8(string s)
{
  s = cork_to_utf8 (s);
  c_string p = c_string (s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  return nss;
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
  return utf8_to_cork (from_nsstring (s));
}
