
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

// the keys of the parent tree of the annotations come after those of the
// pages, of which there are fewer
static const int pdf_tag_annot_key0= 1000000;
// and those of the pictures whose structure is grafted come after those
static const int pdf_tag_stream_key0= 2000000;

// A structure element: its standard type, its alternate description (""
// for none) and its integer attributes (the spans of a cell, owner Table)
struct pdf_tag_elem {
  string role, alt;
  array<string> akeys;
  array<int> avals;
  int parent;           // -1 for the document
  bool own_alt;         // the alternative text is the user's
  bool picture;         // the element of an image (not a drawing)
  pdf_tag_elem (): parent (-1), own_alt (false), picture (false) {}
};

// The structure of a picture which is itself a tagged PDF, as read from it
// by a writer, to be grafted in place of its Figure: its elements (their
// standard types, alternative texts and parents, -1 for none) and the
// content of each in order (an element i >= 0, or the marked content of
// MCID m in the content of the picture, -1-m)
struct pdf_tag_graft {
  array<string> roles, alts;
  array<int> parents;
  array<array<int> > content;
  array<int> tops;      // the elements of no parent, in order
  int mcids;            // the MCIDs are below
  pdf_tag_graft (): mcids (0) {}
};

// A child of an element: another element, a marked content (MCID id of a
// page) or an annotation (id, the writer's own number for it); pos and seq
// give the order
struct pdf_tag_kid {
  int  parent, kind;    // kind: 0 element, 1 marked content, 2 annotation,
                        // 3 marked content of a grafted picture (in its
                        // form, stream: the number of the graft)
  int  id, page;
  path pos;
  int  seq;
  int  stream;
  pdf_tag_kid (): parent (-1), kind (0), id (0), page (0), seq (0),
                  stream (-1) {}
};

class pdf_tags_rep {
public:
  tagger_rep* tagger;
  array<pdf_tag_elem> elems;   // elems[0], when there is one, is Document
  array<pdf_tag_kid>  kids;
  array<array<int> >  page_mcids;   // the element of each MCID of a page
  array<array<int> >  stream_mcids; // ... of each MCID of a grafted picture

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

  // Whether the picture of the box of place ip may have its structure
  // grafted: a Figure of no alternative text of the user's
  bool can_graft (path ip);
  // The structure g of the picture of the box of place ip, on page pg, in
  // place of its Figure (a Div then): the operators which close the open
  // block (the picture is drawn in none: it has its own); stream is the
  // number of the graft, whose form gets the key pdf_tag_stream_key0 +
  // stream in the parent tree
  string graft (path ip, int pg, pdf_tag_graft g, int& stream);

  // the children of each element, in the order of the document
  array<array<int> > children ();
};

typedef pdf_tags_rep* pdf_tags;

// The standard type of the type s of an element of a grafted picture
// (after its role map): a Document is a Div in ours, a heading a paragraph
// (its level would not follow those of the document); NULL for a type
// which is not standard (C: the writers' readers use it in fz_try)
const char* pdf_graft_role (const char* s);

// The structure read from a picture: its elements (parent, -1 for none,
// standard type, alternative text) and the content of each (owner, -1 for
// the root; val: an element, or -1-MCID), in order. The elements of no
// marked content are left out; keep: the MCIDs of those which remain
pdf_tag_graft pdf_graft_prune (array<int> parent, array<string> role,
                               array<string> alt, array<int> owner,
                               array<int> val, int mcids, hashset<int>& keep);

// The content of a stream of a picture without its marked content (BMC,
// BDC, EMC), but optional content (BDC /OC): a picture which is itself a
// tagged PDF, whose tags and artifacts would nest in its Figure. open
// carries the blocks from a stream to the next (the streams of a page:
// whether each was kept); changed says whether anything was taken out.
string pdf_strip_marked_content (string s, array<bool>& open, bool& changed);
// The same for a grafted picture: its artifacts and the marked content of
// the MCIDs keep (those of its elements) stay; any other marked content is
// an artifact
// (props: the MCIDs of the names of the properties of its resources); ok
// says whether all that the result paints is tagged or an artifact, its
// blocks balanced: else the picture is not grafted
string pdf_graft_marked_content (string s, hashset<int> keep,
                                 hashmap<string,int> props, bool& changed,
                                 bool& ok);

// the XMP metadata of a tagged PDF (UTF-8 title and author), with the part
// of PDF/UA it follows
string pdf_tags_xmp (string title, string author, string producer);

#endif // PDF_TAGS_H
