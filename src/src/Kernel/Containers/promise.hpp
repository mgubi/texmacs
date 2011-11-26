
/******************************************************************************
* MODULE     : promise.hpp
* DESCRIPTION: promises
* COPYRIGHT  : (C) 2007  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PROMISE_H
#define PROMISE_H
#include "tree.hpp"

class tree;
template<class T> class promise_rep;
template<class T> class promise;
template<class T> tm_ostream& operator << (tm_ostream& out, promise<T> cmd);
template<class T>  bool operator == (promise<T> mw1, promise<T> mw2);

template<class T>
class promise_rep: public tm_obj<promise_rep<T> > {
public:
  inline promise_rep () {}
  inline virtual ~promise_rep () {}
  inline virtual tm_ostream& print (tm_ostream& out);
  virtual T eval () = 0;
};

template<class T>
class promise : public tm_abs_null_ptr<promise_rep<T> > {
public:
  promise(promise_rep<T>* p=NULL) : tm_abs_null_ptr<promise_rep<T> >(p) {}
  inline T operator () ();
  friend tm_ostream& operator << LESSGTR (tm_ostream& out, promise<T> cmd);
  friend bool operator ==  LESSGTR (promise<T> mw1, promise<T> mw2);
};

#define TMPL template<class T>
TMPL inline tm_ostream& promise_rep<T>::print (tm_ostream& out) {
  return out << "promise"; }
TMPL inline T promise<T>::operator () () {
  return this->rep()->eval (); }
TMPL inline bool operator == (promise<T> mw1, promise<T> mw2) {
  return mw1.rep() == mw2.rep(); }
TMPL inline tm_ostream& operator << (tm_ostream& out, promise<T> cmd) {
  if (is_nil (cmd)) return out << "(null)"; else return cmd->print(out); }
#undef TMPL

#endif // defined PROMISE_H
