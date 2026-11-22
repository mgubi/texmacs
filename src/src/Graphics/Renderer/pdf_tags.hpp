
/******************************************************************************
* MODULE     : pdf_tags.hpp
* DESCRIPTION: the structure of a tagged PDF, as the PDF writers build it
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* What the PDF writers (mupdf_pdf_renderer, pdf_hummus_renderer) share of a
* tagged PDF (docs/pdf-tagging.md): the structure elements of what they
* draw, which the tagger of print_doc gives for the place of each box; the
* blocks of marked content of the pages (the operators to write); the
* children of each element in the order of the document; the parent tree.
* What is left to a writer is to write the operators and the objects.
*
******************************************************************************/

#ifndef PDF_TAGS_H
#define PDF_TAGS_H
#include "renderer.hpp"
#include "hashset.hpp"

// A structure element: its standard type, its alternate description (""
// for none) and its integer attributes (the spans of a cell, owner Table)
struct pdf_tag_elem {
  string role, alt;
  array<string> akeys;
  array<int> avals;
  int parent;           // -1 for the document
  pdf_tag_elem (): parent (-1) {}
};

// A child of an element: another element, a marked content (MCID id of a
// page) or an annotation (id, the writer's own number for it); pos and seq
// give the order
struct pdf_tag_kid {
  int  parent, kind;    // kind: 0 element, 1 marked content, 2 annotation
  int  id, page;
  path pos;
  int  seq;
  pdf_tag_kid (): parent (-1), kind (0), id (0), page (0), seq (0) {}
};

class pdf_tags_rep {
public:
  tagger_rep* tagger;
  array<pdf_tag_elem> elems;   // elems[0], when there is one, is Document
  array<pdf_tag_kid>  kids;
  array<array<int> >  page_mcids;   // the element of each MCID of a page

private:
  hashmap<string,int> index;   // the elements, by their keys
  hashset<int> rows;           // the rows whose cells were made
  int  block;                  // -2: none open, -1: an artifact, else its element
  array<int> mcids;            // the element of each MCID of the page
  bool have;                   // the last place resolved:
  path last_ip, last_pos;      // ... the box, the place of its content
  int  last_elem;              // ... and its innermost element
  int  seq;

  int  add (tag_node nd, int parent);

public:
  int  span;                   // the depth of the ActualText spans

  pdf_tags_rep (tagger_rep* t);
  void resolve (path ip);
  int  element () { return last_elem; }
  path position () { return last_pos; }

  // The operators which put the drawing which follows, of the box of place
  // ip and of kind 0 (text), 1 (lines, fills) or 2 (images), on page pg, in
  // its block of marked content; "" when it is in the open one. A writer
  // ends its text object before them: a block may not cross one.
  string mark (path ip, int kind, int pg);
  // the operators which close the open block (at the end of a page)
  string close ();
  void   end_page ();

  // The element of an annotation of the box of place ip: its Link, or a
  // Link (Annot) of its own in its element; and the annotation as a child
  int  link_element (path ip, path& pos);
  int  annot_element (path ip, path& pos);
  void add_annotation (int e, int id, int pg, path pos);

  // the children of each element, in the order of the document
  array<array<int> > children ();
};

typedef pdf_tags_rep* pdf_tags;

// the XMP metadata of a tagged PDF (UTF-8 title and author), with the part
// of PDF/UA it follows
string pdf_tags_xmp (string title, string author, string producer);

#endif // PDF_TAGS_H
