
/******************************************************************************
* MODULE     : lazy_typeset.hpp
* DESCRIPTION: Lazy typesetting of various primitives
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LAZY_TYPESET_H
#define LAZY_TYPESET_H
#include "formatter.hpp"
#include "Format/line_item.hpp"

struct lazy_document_rep: public lazy_rep {
  array<lazy> par;   // the paragraphs

  lazy_document_rep (edit_env env, tree t, path ip);
  inline operator tree () { return "Document"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_document : public tm_ext_null_ptr<lazy_document_rep, lazy> {
public:
  lazy_document(lazy_document_rep *p=NULL) : tm_ext_null_ptr<lazy_document_rep, lazy>(p) {}
  inline lazy_document (edit_env env, tree t, path ip):
    tm_ext_null_ptr<lazy_document_rep, lazy> (tm_new<lazy_document_rep> (env, t, ip)) {  }
};

struct lazy_surround_rep: public lazy_rep {
  array<line_item> a;    // left surrounding
  array<line_item> b;    // right surrounding
  lazy             par;  // the surrounded paragraph

  lazy_surround_rep (edit_env env, tree t, path ip);
  lazy_surround_rep (array<line_item> a, array<line_item> b, lazy p, path ip);
  inline operator tree () { return "Surround"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_surround : public tm_ext_null_ptr<lazy_surround_rep, lazy> {
public:
  lazy_surround(lazy_surround_rep *p=NULL) : tm_ext_null_ptr<lazy_surround_rep, lazy>(p) { }
  inline lazy_surround (edit_env env, tree t, path ip):
    tm_ext_null_ptr<lazy_surround_rep, lazy> (tm_new<lazy_surround_rep> (env, t, ip)) {  }
  inline lazy_surround (array<line_item> a, array<line_item> b,
			lazy par, path ip):
    tm_ext_null_ptr<lazy_surround_rep, lazy> (tm_new<lazy_surround_rep> (a, b, par, ip)) { }
};

#endif // defined LAZY_TYPESET_H
