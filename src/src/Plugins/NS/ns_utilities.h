
/******************************************************************************
* MODULE     : ns_utilities.h
* DESCRIPTION: Utilities for Aqua
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NS_UTILITIES_H
#define NS_UTILITIES_H

#include "MacOS/mac_cocoa.h"
#include "message.hpp"
#include <exception>

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

NSRect to_nsrect (coord4 p);
NSPoint to_nspoint (coord2 p);
NSSize to_nssize (coord2 p);
NSSize to_nssize (SI w, SI h);
coord4 from_nsrect (NSRect rect);
coord2 from_nspoint (NSPoint pt);
coord2 from_nssize (NSSize s);
NSString *to_nsstring (string s);
NSString *to_nsstring_utf8 (string s);
string from_nsstring (NSString *s);
string from_nsstring_utf8 (NSString *s);
string ns_translate (string s);

/******************************************************************************
 * Type checking
 ******************************************************************************/
#pragma mark type checking

inline void
check_type_void (blackbox bb, slot s) {
  if (!is_nil (bb)) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> inline void
check_type_id (int type_id, slot s) {
  if (type_id != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> void
check_type (blackbox bb, slot s) {
  if (type_box (bb) != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T1, class T2> inline void
check_type (blackbox bb, string s) {
  check_type<pair<T1,T2> > (bb, s);
}


/*! the run-loop should exit when the number of windows is zero */
extern int nr_windows;

/******************************************************************************
 * Some debugging infrastucture
 ******************************************************************************/
extern tm_ostream& operator << (tm_ostream& out, NSRect rect);
extern tm_ostream& operator << (tm_ostream& out, NSSize size);

tm_ostream& operator << (tm_ostream& out, coord4 c);
tm_ostream& operator << (tm_ostream& out, coord2 c);

// deprecated, use check_type<T>(bb, slot) instead
//#define TYPE_CHECK(b) ASSERT (b, "type mismatch")

NSSize ns_decode_length (string width, string height, NSSize ref);
NSString* to_label (string s);
string from_label (NSString* s);

#define NOT_IMPLEMENTED(x) \
{ if (DEBUG_QT) debug_qt << x << " not implemented yet.\n"; }

/******************************************************************************
 * Failures made visible (#621)
 ******************************************************************************/

void ns_report_exception (const char* where, NSException* e);
void ns_report_exception (const char* where, string msg);
bool ns_check_size (NSSize size, const char* where);
bool ns_check_point (NSPoint pt, const char* where);

/*! An entry point from AppKit into TeXmacs (the events, an action, a timer,
 a delegate): an exception, of Objective-C or of C++ (the ASSERTs and FAILEDs
 of TeXmacs), is reported as an error and the entry point returns normally,
 instead of aborting TeXmacs (#619) or being dropped by AppKit without a
 word (#468). The C++ handlers are outside the Objective-C one, which only
 takes the NSExceptions. */
#define NS_ENTRY_BEGIN try { @try {
#define NS_ENTRY_END(where)                                                 \
  } @catch (NSException* ns_entry_e) {                                      \
    ns_report_exception (where, ns_entry_e); } }                            \
  catch (string& ns_entry_s) { ns_report_exception (where, ns_entry_s); }   \
  catch (std::exception& ns_entry_x) {                                      \
    ns_report_exception (where, string (ns_entry_x.what ())); }             \
  catch (...) {                                                             \
    ns_report_exception (where, string ("an unknown C++ exception")); }


#endif // NS_UTILITIES_H

