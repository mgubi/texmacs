
/******************************************************************************
* MODULE     : basic.hpp
* DESCRIPTION: see basic.cpp
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*              (C) 2011 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BASIC_H
#define BASIC_H
#include "fast_alloc.hpp"
#include <math.h>

#ifdef HAVE_INTPTR_T
#ifdef OS_SUN
#include <inttypes.h>
#else
#include <stdint.h>
#endif
#else
typedef long intptr_t;
#endif

#ifdef OS_WIN32
#define LESSGTR
#else
#define LESSGTR <>
#endif

#define TM_DEBUG(x)
typedef int SI;
typedef unsigned int SN;
typedef short HI;
typedef unsigned short HN;
typedef char QI;
typedef unsigned char QN;
#ifdef OS_WIN32
typedef __int64 DI;
#else
typedef long long int DI;
#endif
typedef void* pointer;

/******************************************************************************
* debugging
******************************************************************************/

#if (defined OS_WIN32 || defined __SUNPRO_CC || defined __clang__)
#define STACK_NEW_ARRAY(name,T,size) T* name= tm_new_array<T> (size)
#define STACK_DELETE_ARRAY(name) tm_delete_array (name)
#else
#define STACK_NEW_ARRAY(name,T,size) T name[size]
#define STACK_DELETE_ARRAY(name)
#endif

enum { DEBUG_FLAG_AUTO, DEBUG_FLAG_VERBOSE, DEBUG_FLAG_EVENTS,
       DEBUG_FLAG_STD, DEBUG_FLAG_IO, DEBUG_FLAG_BENCH,
       DEBUG_FLAG_HISTORY, DEBUG_FLAG_QT, DEBUG_FLAG_KEYBOARD,
       DEBUG_FLAG_PACKRAT, DEBUG_FLAG_FLATTEN, DEBUG_FLAG_CORRECT };
bool debug (int which, bool write_flag= false);
int  debug_off ();
void debug_on (int status);
class string;
void debug_set (string s, bool flag);
bool debug_get (string s);
#define DEBUG_AUTO (debug (DEBUG_FLAG_AUTO))
#define DEBUG_VERBOSE (debug (DEBUG_FLAG_VERBOSE))
#define DEBUG_EVENTS (debug (DEBUG_FLAG_EVENTS))
#define DEBUG_STD (debug (DEBUG_FLAG_STD))
#define DEBUG_IO (debug (DEBUG_FLAG_IO))
#define DEBUG_BENCH (debug (DEBUG_FLAG_BENCH))
#define DEBUG_HISTORY (debug (DEBUG_FLAG_HISTORY))
#define DEBUG_QT (debug (DEBUG_FLAG_QT))
#define DEBUG_KEYBOARD (debug (DEBUG_FLAG_KEYBOARD))
#define DEBUG_PACKRAT (debug (DEBUG_FLAG_PACKRAT))
#define DEBUG_FLATTEN (debug (DEBUG_FLAG_FLATTEN))
#define DEBUG_CORRECT (debug (DEBUG_FLAG_CORRECT))

void tm_failure (const char* msg);
#ifdef DEBUG_ASSERT
#include <assert.h>
#define ASSERT(cond,msg) { if (!(cond)) { tm_failure (msg); assert (cond); } }
#define FAILED(msg) { tm_failure (msg); assert (false); }
#else
#define ASSERT(cond,msg) { if (!(cond)) { tm_failure (msg); } }
#define FAILED(msg) { tm_failure (msg); }
#endif

/******************************************************************************
* miscellaneous routines
******************************************************************************/

inline SI min (SI i, SI j) { if (i<j) return i; else return j; }
inline SI max (SI i, SI j) { if (i>j) return i; else return j; }
inline DI min (DI i, DI j) { if (i<j) return i; else return j; }
inline DI max (DI i, DI j) { if (i>j) return i; else return j; }
inline double min (double i, double j) { if (i<j) return i; else return j; }
inline double max (double i, double j) { if (i>j) return i; else return j; }
inline int hash (int i) { return i; }
inline int hash (pointer ptr) {
  return ((int) ((intptr_t) ptr)) + (((int) ((intptr_t) ptr)) % 19); }
inline int copy (int x) { return x; }
inline SI as_int (double x) { return (SI) floor (x + 0.5); }
inline double tm_round (double x) { return floor (x + 0.5); }

enum display_control { INDENT, UNINDENT, HRULE, LF };
tm_ostream& operator << (tm_ostream& out, display_control ctrl);

bool gui_is_x ();
bool gui_is_qt ();
bool os_win32 ();
bool os_mingw ();
bool os_macos ();
bool use_macos_fonts ();
const char* default_look_and_feel ();


/******************************************************************************
 * type id factory
 ******************************************************************************/

template<typename T>
struct type_helper {
  static int id;
  static T   init;
};

int new_type_identifier ();
template<typename T> int type_helper<T>::id  = new_type_identifier ();
template<typename T> T   type_helper<T>::init= T ();

/******************************************************************************
 * base classes
 ******************************************************************************/

//  tm_base is the base common class for all the texmacs objects

class tm_base {
};

// auxiliary class to gather statistics about objects

template <class T>
class tm_stats  {
public:
  static int alive;
  static int created;
  tm_stats () { TM_DEBUG(alive++); TM_DEBUG(created++); }
protected:
  ~tm_stats () { TM_DEBUG(alive--); }
};

template <class T> int tm_stats<T>::alive (0);
template <class T> int tm_stats<T>::created (0);


template <class T> class tm_ptr; 
template <class T> class tm_null_ptr; 
template <class T> class tm_abs_ptr; 
template <class T> class tm_abs_null_ptr; 
template <class TT, class B>  class tm_ext_ptr;
template <class TT, class B>  class tm_ext_null_ptr;

// trait for reference counting memory handling

template <class T>
class tm_obj : public tm_base, public tm_stats<T> {
	int ref_count;
  
protected:
	inline tm_obj (): ref_count (0) { TM_DEBUG(concrete_count++); }
  inline ~tm_obj () { TM_DEBUG(concrete_count--); }
	inline void inc_ref () { ref_count++; } 
	inline void dec_ref () { if (0 == --ref_count) static_cast<T*>(this)->destroy(); } 
  inline void destroy () { tm_delete (static_cast<T*>(this)); }

public:
  inline int get_ref_count () { return ref_count; } 

  template <class TT> friend class tm_ptr;
  template <class TT> friend class tm_null_ptr;
  template <class TT> friend class tm_abs_ptr;
  template <class TT> friend class tm_abs_null_ptr;
  template <class TT, class B> friend class tm_ext_ptr;
  template <class TT, class B> friend class tm_ext_null_ptr;
};


template <class T>
class tm_ptr {
  T *rep_;
protected:	
  inline tm_ptr (T* p) : rep_ (p) { rep_->inc_ref(); }
  inline T* rep() const { return rep_; }
public:
  inline tm_ptr (const tm_ptr<T>& x) : rep_(x.rep_) { rep_->inc_ref(); }
  inline ~tm_ptr() { rep_->dec_ref(); }
  inline tm_ptr& operator=(tm_ptr<T> x) {  x.rep_->inc_ref();  rep_->dec_ref(); rep_=x.rep_; return *this; }
  inline T* operator->() { return rep_; }
};

template <class T> class tm_null_ptr;
template <class T>  bool is_nil (tm_null_ptr<T> p);

template <class T>
class tm_null_ptr {
  T *rep_;
protected:	
  inline tm_null_ptr (T* p) : rep_ (p) { if (rep_) rep_->inc_ref(); }
  inline T* rep() const { return rep_; }
public:
  inline tm_null_ptr () : rep_ (NULL) {  }
  inline tm_null_ptr (const tm_null_ptr<T>& x) : rep_(x.rep_) { if (rep_)  rep_->inc_ref(); }
  inline ~tm_null_ptr() { if (rep_)  rep_->dec_ref(); }
  inline tm_null_ptr& operator=(tm_null_ptr<T> x) {  if (x.rep_) x.rep_->inc_ref();  if (rep_) rep_->dec_ref(); rep_=x.rep_; return *this; }
  inline T* operator->() { return rep_; }
  friend bool is_nil <> (tm_null_ptr<T> p);
  template <class TT, class BB> friend class tm_ext_null_ptr;
};

template <class T>
inline bool is_nil (tm_null_ptr<T> p) { return (p.rep() == NULL); }

template <class T>
class tm_abs_null_ptr : public tm_null_ptr<T> {
public:
  inline tm_abs_null_ptr (T* p=NULL) : tm_null_ptr<T> (p) {  }
};

template <class T>
class tm_abs_ptr : public tm_ptr<T> {
public:
  inline tm_abs_ptr (T* p) : tm_ptr<T> (p) {  }
};

template <class T, class B>
class tm_ext_null_ptr : public tm_abs_null_ptr<T> {
public:
  inline tm_ext_null_ptr (T* p=NULL) : tm_abs_null_ptr<T> (p) {  }
  inline tm_ext_null_ptr (const B& x) : tm_abs_null_ptr<T> (static_cast<T*>(x.rep())) {}
  operator B () { return B (this->rep()); }
};

template <class T, class B>
class tm_ext_ptr : public tm_abs_ptr<T> {
public:
  inline tm_ext_ptr (T* p) : tm_abs_ptr<T> (p) {  }
  inline tm_ext_ptr (const B& x) : tm_abs_ptr<T> (static_cast<T*>(x.rep())) {}
  operator B () { return B (this->rep()); }
};

#endif // defined BASIC_H
