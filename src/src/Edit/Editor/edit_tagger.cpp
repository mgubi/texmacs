
/******************************************************************************
* MODULE     : edit_tagger.cpp
* DESCRIPTION: the structure of a document, for a tagged PDF
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* A tagging renderer (mupdf_pdf_renderer) asks, for each box it draws,
* which structure elements hold it (docs/pdf-tagging.md). The box knows the
* place of its node in the document (its inverse path); the elements are
* the nodes on the way from the body down to that place which have a
* structure type: the tags of the table pdf-tag-roles (tm-print.scm), and
* the paragraphs, the children of a document. The text a macro writes (the
* number of a section, "Theorem 1.", the bullet of an item) has the place
* of the macro, and belongs to its element; a box which comes from no place
* of the body (a header, a footer, a page number) has no element and is an
* artifact.
*
******************************************************************************/

#include "renderer.hpp"
#include "tree.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "scheme.hpp"

class pdf_tagger_rep: public tagger_rep {
  tree   body;     // the body of the document
  path   rp;       // its place in the tree of the buffer
  string lang, ttl;
  hashmap<string,string> roles;      // by tag; "" for none
  hashmap<string,string> child_roles;
  hashmap<string,string> alts;       // by key of element
  hashmap<int,int> levels;           // the level of each rank of heading
  hashmap<string,string> heading_of; // the role of each heading, by key
  int    last_level;                 // that of the last heading met

public:
  pdf_tagger_rep (tree body, path rp, string lang, string title);
  array<tag_node> resolve (path ip, path& pos);
  array<tag_node> row_cells (path pos);
  string language () { return lang; }
  string title () { return ttl; }

private:
  string role_of (string tag);
  string child_role_of (string tag, int i, int n);
  string alt_of (string key, tree t, string role);
  void   collect_ranks (tree t, hashset<int>& ranks);
  string heading (string key, string role);
  string cell_spans (array<tree> trail, int row, int col);
};

pdf_tagger_rep::pdf_tagger_rep (tree body2, path rp2, string lang2,
                                string title2):
  body (body2), rp (rp2), lang (lang2), ttl (title2),
  roles ("?"), child_roles ("?"), alts (""), levels (0),
  heading_of (""), last_level (0)
{
  // the ranks of the headings which the document uses become H1, H2...
  // in their order, so that no level is skipped (PDF/UA)
  hashset<int> ranks;
  collect_ranks (body, ranks);
  array<int> r;
  iterator<int> it= iterate (ranks);
  while (it->busy ()) r << it->next ();
  for (int i=1; i<N(r); i++)
    for (int j=i; j>0 && r[j] < r[j-1]; j--) {
      int t= r[j]; r[j]= r[j-1]; r[j-1]= t; }
  for (int i=0; i<N(r); i++) levels (r[i])= min (i + 1, 6);
}

string
pdf_tagger_rep::role_of (string tag) {
  string r= roles [tag];
  if (r == "?") {
    r= as_string (call ("pdf-tag-role", object (tag)));
    roles (tag)= r;
  }
  return r;
}

string
pdf_tagger_rep::child_role_of (string tag, int i, int n) {
  string key= tag * "#" * as_string (i) * "#" * as_string (n);
  string r= child_roles [key];
  if (r == "?") {
    r= as_string (call ("pdf-tag-child-role", object (tag), object (i),
                        object (n)));
    child_roles (key)= r;
  }
  return r;
}

string
pdf_tagger_rep::alt_of (string key, tree t, string role) {
  if (alts->contains (key)) return alts [key];
  string r= as_string (call ("pdf-tag-alt", object (t), object (role)));
  if (N(r) == 0) r= role;   // PDF/UA wants one, whatever it says
  alts (key)= r;
  return r;
}

void
pdf_tagger_rep::collect_ranks (tree t, hashset<int>& ranks) {
  if (is_atomic (t)) return;
  string r= role_of (as_string (L(t)));
  if (starts (r, "H:")) ranks->insert (as_int (r (2, N(r))));
  if (r == "Formula" || r == "Figure") return;
  for (int i=0; i<N(t); i++) collect_ranks (t[i], ranks);
}

// The level of a heading: that of its rank, but at most one deeper than the
// heading before it (PDF/UA), the headings being met in the order of the
// pages
string
pdf_tagger_rep::heading (string key, string role) {
  if (heading_of->contains (key)) return heading_of [key];
  int rank= as_int (role (2, N(role)));
  int lv= levels [rank];
  if (lv <= 0) lv= 1;
  lv= min (lv, last_level + 1);
  last_level= lv;
  string r= "H" * as_string (lv);
  heading_of (key)= r;
  return r;
}

// The spans of the cell (row, col) of the table which ends trail (the
// table, then the formats around it, inside out), as attributes; the
// formats inside apply after those outside, and a negative index counts
// from the end
string
pdf_tagger_rep::cell_spans (array<tree> trail, int row, int col) {
  tree tab= trail[0];
  int nr= N(tab), nc= (nr > 0 && is_compound (tab[0])? N(tab[0]): 0);
  string rs= "1", cs= "1";
  for (int k= N(trail) - 1; k >= 1; k--) {
    tree f= trail[k];
    for (int i=0; i+1<N(f); i++) {
      tree w= f[i];
      if (!is_compound (w, "cwith", 6) || !is_atomic (w[4])) continue;
      int r1= as_int (w[0]), r2= as_int (w[1]);
      int c1= as_int (w[2]), c2= as_int (w[3]);
      if (r1 < 0) r1 += nr + 1; if (r2 < 0) r2 += nr + 1;
      if (c1 < 0) c1 += nc + 1; if (c2 < 0) c2 += nc + 1;
      if (row + 1 < r1 || row + 1 > r2 || col + 1 < c1 || col + 1 > c2)
        continue;
      if (w[4] == "cell-row-span" && is_atomic (w[5])) rs= w[5]->label;
      if (w[4] == "cell-col-span" && is_atomic (w[5])) cs= w[5]->label;
    }
  }
  string r;
  if (as_int (cs) > 1) r << "ColSpan=" << as_string (as_int (cs));
  if (as_int (rs) > 1) r << (N(r) > 0? " ": "") << "RowSpan="
                         << as_string (as_int (rs));
  return r;
}

static bool
has_document_child (tree t) {
  if (is_atomic (t)) return false;
  for (int i=0; i<N(t); i++)
    if (is_document (t[i])) return true;
  return false;
}

// the item k places from the end of p (0: the last one)
static int
last_index (path p, int k) {
  int n= N(p);
  path q= p;
  for (int i=0; i < n-1-k && !is_nil (q); i++) q= q->next;
  return is_nil (q)? 0: q->item;
}

array<tag_node>
pdf_tagger_rep::resolve (path ip, path& pos) {
  array<tag_node> r;
  // the text a macro writes has the place of the macro, after the marks
  // of a decoration; a box of no place of the body is an artifact
  path q= ip;
  while (!is_nil (q) && q->item < 0) q= q->next;
  if (is_nil (q)) return r;
  path p= reverse (q);
  if (!(rp <= p)) return r;
  path s= p / rp;
  pos= s;
  r << tag_node ("/", "Document", path ());
  tree   t   = body;
  path   cur;
  array<tree> above;           // the nodes on the way, the body first
  string last= "Document";
  bool   li_body= false;     // in a list item: its body was opened
  string li_key;
  path   li_pos;
  while (!is_nil (s) && is_compound (t)) {
    int i= s->item;
    if (i < 0 || i >= N(t)) break;
    tree parent= t;
    above << parent;
    t= t[i]; cur= cur * i; s= s->next;
    // a list item is a label (the item tag) and a body
    if (last == "LI" && !li_body) {
      string lb= is_atomic (t)? string (""): as_string (L(t));
      if (lb == "item" || lb == "item*" || lb == "bibitem" ||
          lb == "bibitem*") {
        r << tag_node (as_string (cur), "Lbl", cur);
        return r;
      }
      // (at the place of its first part, after the label)
      r << tag_node (li_key * "/b", "LBody", cur);
      li_body= true;
      last= "LBody";
    }
    // every child of the document of a list is an item (L may hold only
    // LI); one which is not a line of text (an equation) is all its body
    if (last == "L" && is_document (parent) && !is_document (t)) {
      // (the node may be an element too, a list in a list: another key)
      string key= as_string (cur) * "/i";
      r << tag_node (key, "LI", cur);
      last= "LI"; li_body= false; li_key= key; li_pos= cur;
      if (is_atomic (t)) break;
      if (!is_compound (t, "concat")) {
        r << tag_node (key * "/b", "LBody", cur);
        li_body= true;
        last= "LBody";
      }
      else continue;
    }
    // (a paragraph or a caption may be a mere string)
    string role= child_role_of (as_string (L(parent)), i, N(parent));
    if (role == "" && is_compound (t)) role= role_of (as_string (L(t)));
    if (role == "" && is_document (parent) && !is_document (t)) {
      if (last == "L") role= "LI";
      else role= has_document_child (t)? "Div": "P";
    }
    // a table which no tabular holds (one which a macro makes)
    if (role == "" && is_compound (t, "table") && last != "Table")
      role= "Table";
    if (role == "") {
      if (is_atomic (t)) break;
      continue;
    }
    string key= as_string (cur);
    if (starts (role, "H:")) role= heading (key, role);
    string alt;
    if (role == "Formula" || role == "Figure") alt= alt_of (key, t, role);
    tag_node nd (key, role, cur, alt);
    if (role == "TD" && N(above) >= 2 && is_compound (above[N(above)-2],
                                                     "table")) {
      // the spans of the cell, from the formats around its table
      array<tree> trail;
      trail << above[N(above)-2];
      for (int k= N(above)-3; k >= 0 && is_compound (above[k], "tformat");
           k--)
        trail << above[k];
      int row= last_index (cur, 1), col= i;
      nd.attrs= cell_spans (trail, row, col);
    }
    r << nd;
    last= role;
    if (role == "LI") { li_body= false; li_key= key; li_pos= cur; }
    // a formula or a figure is a whole, with its description
    if (role == "Formula" || role == "Figure" || is_atomic (t)) break;
  }
  if (last == "LI" && !li_body) r << tag_node (li_key * "/b", "LBody", li_pos);
  return r;
}

array<tag_node>
pdf_tagger_rep::row_cells (path pos) {
  array<tag_node> r;
  // the table and the formats around it, as resolve finds them
  array<tree> above;
  tree t= body;
  for (path q= pos; !is_nil (q); q= q->next) {
    if (is_atomic (t) || q->item < 0 || q->item >= N(t)) return r;
    above << t;
    t= t[q->item];
  }
  if (!is_compound (t, "row") || N(above) < 1 ||
      !is_compound (above[N(above)-1], "table")) return r;
  array<tree> trail;
  trail << above[N(above)-1];
  for (int k= N(above)-2; k >= 0 && is_compound (above[k], "tformat"); k--)
    trail << above[k];
  tree tab= trail[0];
  int row= last_index (pos, 0);
  // the cells covered by the span of another one
  hashset<int> covered;
  for (int i=0; i<N(tab); i++)
    for (int j=0; is_compound (tab[i]) && j<N(tab[i]); j++) {
      string sp= cell_spans (trail, i, j);
      if (sp == "" || covered->contains (i * 10000 + j)) continue;
      int rs= 1, cs= 1;
      array<string> av= tokenize (sp, " ");
      for (int a=0; a<N(av); a++) {
        if (starts (av[a], "RowSpan=")) rs= as_int (av[a] (8, N(av[a])));
        if (starts (av[a], "ColSpan=")) cs= as_int (av[a] (8, N(av[a])));
      }
      for (int di=0; di<rs; di++)
        for (int dj=0; dj<cs; dj++)
          if (di > 0 || dj > 0) covered->insert ((i + di) * 10000 + j + dj);
    }
  for (int j=0; j<N(t); j++) {
    if (covered->contains (row * 10000 + j)) continue;
    path cp= pos * j;
    tag_node nd (as_string (cp), "TD", cp);
    nd.attrs= cell_spans (trail, row, j);
    r << nd;
  }
  return r;
}

tagger_rep*
make_pdf_tagger (tree body, path rp, string lang, string title) {
  return tm_new<pdf_tagger_rep> (body, rp, lang, title);
}
