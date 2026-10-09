
/******************************************************************************
* MODULE     : list.cpp
* DESCRIPTION: linked lists with reference counting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LIST_CC
#define LIST_CC
#include "list.hpp"

/******************************************************************************
* output and convertion
******************************************************************************/

template<class T> tm_ostream&
operator << (tm_ostream& out, list<T> l) {
  out << "[";
  if (!is_nil (l)) {
    out << " " << l->item;
    l=l->next;
  }
  while (!is_nil (l)) {
    out << ", " << l->item;
    l=l->next;
  }
  return out << " ]";
}

template<class T> T&
list<T>::operator [] (int i) {
  // (the functions of this file do not call themselves: a long list is
  // not a deep stack, which a browser gives little of to a worker)
  list<T>* p= this;
  for (; i>0; i--) {
    ASSERT (p->rep != NULL, "list too short");
    p= &(p->rep->next);
  }
  ASSERT (p->rep != NULL, "list too short");
  return p->rep->item;
}

template<class T> list<T>::operator tree () {
  list<T> l;
  int i, n=N(*this);
  tree t (TUPLE, n);
  for (i=0, l=*this; i<n; i++, l=l->next)
    t[i]= as_tree (l->item);
  return t;
}

/******************************************************************************
* insertion and suppression
******************************************************************************/

template<class T> list<T>&
operator << (list<T>& l, T item) {
  list<T>* p= &l;
  while (!is_nil (*p)) p= &((*p)->next);
  *p= list<T> (item, list<T> ());
  return l;
}

template<class T> list<T>&
operator << (list<T>& l1, list<T> l2) {
  list<T>* p= &l1;
  while (!is_nil (*p)) p= &((*p)->next);
  *p= l2;
  return l1;
}

template<class T> list<T>&
operator >> (T item, list<T>& l) {
  return (l= list<T> (item, l));
}

template<class T> list<T>&
operator << (T& item, list<T>& l) {
  item= l->item;
  l   = l->next;
  return l;
}

template<class T> T
last_item (list<T> l) {
  ASSERT (!is_nil (l), "empty path");
  while (!is_nil (l->next)) l= l->next;
  return l->item;
}

template<class T> T&
access_last (list<T>& l) {
  ASSERT (!is_nil (l), "empty path");
  list<T>* p= &l;
  while (!is_nil ((*p)->next)) p= &((*p)->next);
  return (*p)->item;
}

template<class T> list<T>&
suppress_last (list<T>& l) {
  ASSERT (!is_nil (l), "empty path");
  list<T>* p= &l;
  while (!is_nil ((*p)->next)) p= &((*p)->next);
  *p= list<T> ();
  return l;
}

/******************************************************************************
* tests
******************************************************************************/

template<class T> bool
strong_equal (list<T> l1, list<T> l2) {
  return l1.rep == l2.rep;
}

template<class T> bool
operator == (list<T> l1, list<T> l2) {
  for (; !is_nil (l1) && !is_nil (l2); l1= l1->next, l2= l2->next)
    if (!(l1->item == l2->item)) return false;
  return is_nil (l1) == is_nil (l2);
}

template<class T> bool
operator != (list<T> l1, list<T> l2) {
  for (; !is_nil (l1) && !is_nil (l2); l1= l1->next, l2= l2->next)
    if (l1->item != l2->item) return true;
  return is_nil (l1) != is_nil (l2);
}

template<class T> bool
operator < (list<T> l1, list<T> l2) {
  for (; !is_nil (l1) && !is_nil (l2); l1= l1->next, l2= l2->next)
    if (!(l1->item == l2->item)) return false;
  return !is_nil (l2);
}

template<class T> bool
operator <= (list<T> l1, list<T> l2) {
  for (; !is_nil (l1) && !is_nil (l2); l1= l1->next, l2= l2->next)
    if (!(l1->item == l2->item)) return false;
  return is_nil (l1);
}

/******************************************************************************
* computations with list<T> structures
******************************************************************************/

template<class T> int
N (list<T> l) {
  int n= 0;
  for (; !is_nil (l); l= l->next) n++;
  return n;
}

template<class T> list<T>
copy (list<T> l) {
  return reverse (reverse (l));
}

template<class T> list<T>
operator * (list<T> l1, T x) {
  list<T> r= x;
  for (list<T> b= reverse (l1); !is_nil (b); b= b->next)
    r= list<T> (b->item, r);
  return r;
}

template<class T> list<T>
operator * (list<T> l1, list<T> l2) {
  list<T> r= copy (l2);
  for (list<T> b= reverse (l1); !is_nil (b); b= b->next)
    r= list<T> (b->item, r);
  return r;
}

template<class T> list<T>
head (list<T> l, int n) {
  list<T> b;
  for (; n>0; n--) {
    ASSERT (!is_nil (l), "list too short to get the head");
    b= list<T> (l->item, b);
    l= l->next;
  }
  return reverse (b);
}

template<class T> list<T>
tail (list<T> l, int n) {
  for (; n>0; n--) {
    ASSERT (!is_nil (l), "list too short to get the tail");
    l=l->next;
  }
  return l;
}

template<class T> list<T>
reverse (list<T> l) {
  list<T> r;
  while (!is_nil(l)) {
    r= list<T> (l->item, r);
    l=l->next;
  }
  return r;
}

template<class T> list<T>
remove (list<T> l, T what) {
  list<T> b;
  for (; !is_nil (l); l= l->next)
    if (!(l->item == what)) b= list<T> (l->item, b);
  return reverse (b);
}

template<class T> bool
contains (list<T> l, T what) {
  for (; !is_nil (l); l= l->next)
    if (l->item == what) return true;
  return false;
}

#endif // defined LIST_CC
