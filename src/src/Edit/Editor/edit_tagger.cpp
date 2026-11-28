
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
#include "env.hpp"
#include "tree.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include "scheme.hpp"
#include "converter.hpp"

class pdf_tagger_rep: public tagger_rep {
  tree   body;     // the body of the document
  path   rp;       // its place in the tree of the buffer
  string lang, ttl;
  hashmap<string,string> roles;      // by tag; "" for none
  hashmap<string,string> child_roles;
  hashmap<string,string> alts;       // by key of element
  hashset<string> own_alts;          // those which are the user's (image-alt)
  hashmap<string,string> heading_of; // the role of each heading, by key
  array<int> open_ranks;             // the ranks of the headings above
  int    appendix_rank;              // that of an appendix: chapter or section
  hashmap<string,hashset<int> > covered;  // the cells a span covers, by table

public:
  pdf_tagger_rep (tree body, path rp, string lang, string title);
  array<tag_node> resolve (path ip, path& pos);
  array<tag_node> row_cells (path pos);
  string language () { return lang; }
  string title () { return ttl; }

private:
  bool   child (tree t, int i, tree& c);
  bool   holds_heading (tree t);
  string role_of (string tag);
  string child_role_of (string tag, int i, int n);
  string alt_of (string key, tree t, string role, array<tree> above,
                 path cur);
  void   collect_ranks (tree t, hashset<int>& ranks);
  string heading (string key, string role);
  string cell_spans (array<tree> trail, int row, int col);
};

pdf_tagger_rep::pdf_tagger_rep (tree body2, path rp2, string lang2,
                                string title2):
  body (body2), rp (rp2), lang (lang2), ttl (title2),
  roles ("?"), child_roles ("?"), alts (""),
  heading_of (""), appendix_rank (3)
{
  // the ranks of the headings which the document uses: an appendix is a
  // chapter in a book, a section elsewhere
  hashset<int> ranks;
  collect_ranks (body, ranks);
  appendix_rank= (ranks->contains (2)? 2: 3);
}

// The child i of t, if any
bool
pdf_tagger_rep::child (tree t, int i, tree& c) {
  if (is_atomic (t)) return false;
  if (i < 0 || i >= N(t)) return false;
  c= t[i];
  return true;
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
pdf_tagger_rep::alt_of (string key, tree t, string role, array<tree> above,
                        path cur) {
  if (alts->contains (key)) return alts [key];
  if (role == "Figure") {
    // the alternative text of a picture: the variable image-alt around it
    // (<with|image-alt|...|<image|...>>), else the caption of its figure,
    // else its file
    for (int k= N(above) - 1; k >= 0; k--) {
      tree w= above[k];
      if (!is_compound (w, "with")) continue;
      for (int j=0; j+1<N(w); j+=2)
        if (w[j] == "image-alt" && is_atomic (w[j+1]) && N(w[j+1]->label) > 0) {
          string r= cork_to_utf8 (w[j+1]->label);
          alts (key)= r;
          own_alts->insert (key);
          return r;
        }
    }
    for (int k= N(above) - 1; k >= 0; k--) {
      tree f= above[k];
      if (!is_compound (f) || N(f) < 2) continue;
      string fl= as_string (L(f));
      if (child_role_of (fl, N(f) - 1, N(f)) != "Caption") continue;
      // (a picture in the caption itself is not described by it)
      path q= cur;
      for (int j=0; j<k && !is_nil (q); j++) q= q->next;
      if (!is_nil (q) && q->item == N(f) - 1) continue;
      string r= as_string (call ("pdf-tag-text", object (f[N(f)-1])));
      if (N(r) > 0) { alts (key)= r; return r; }
    }
  }
  string r= as_string (call ("pdf-tag-alt", object (t), object (role)));
  if (N(r) == 0) r= role;   // PDF/UA wants one, whatever it says
  alts (key)= r;
  return r;
}

void
pdf_tagger_rep::collect_ranks (tree t, hashset<int>& ranks) {
  if (is_atomic (t)) return;
  string r= role_of (as_string (L(t)));
  if (starts (r, "H:") && r != "H:A") ranks->insert (as_int (r (2, N(r))));
  if (r == "Formula" || r == "Figure") return;
  for (int i=0; i<N(t); i++) collect_ranks (t[i], ranks);
}

// The level of a heading, the headings being met in the order of the
// pages: one deeper than the headings above it, those of a smaller rank
// met before it and not closed since, so that a level is never skipped
// (PDF/UA) and headings of a rank have the same level under the same one
string
pdf_tagger_rep::heading (string key, string role) {
  if (heading_of->contains (key)) return heading_of [key];
  string rk= role (2, N(role));
  int rank= (rk == "A"? appendix_rank: as_int (rk));
  while (N(open_ranks) > 0 && open_ranks[N(open_ranks)-1] >= rank)
    open_ranks->resize (N(open_ranks) - 1);
  open_ranks << rank;
  string r= "H" * as_string (min (N(open_ranks), 6));
  heading_of (key)= r;
  return r;
}

// The columns of a table: as many as its shortest row has (table_rep)
static int
table_columns (tree tab) {
  int nc= -1;
  for (int i=0; i<N(tab); i++)
    if (is_compound (tab[i])) nc= (nc < 0? N(tab[i]): min (nc, N(tab[i])));
  return max (nc, 0);
}

// The spans of the cell (row, col) of the table which ends trail (the
// table, then the formats around it, inside out), as attributes; the
// formats inside apply after those outside, and a negative index counts
// from the end
string
pdf_tagger_rep::cell_spans (array<tree> trail, int row, int col) {
  tree tab= trail[0];
  int nr= N(tab), nc= table_columns (tab);
  string rs= "1", cs= "1";
  bool breakable= false;
  for (int k= N(trail) - 1; k >= 1; k--) {
    tree f= trail[k];
    for (int i=0; i+1<N(f); i++) {
      tree w= f[i];
      if (is_compound (w, "twith", 2) && w[0] == "table-hyphen")
        breakable= (w[1] == "y");
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
  // (a span does not go past the table; a table broken across pages has
  // no rows of several rows, as in table_rep::typeset)
  if (breakable) rs= "1";
  int ncs= min (as_int (cs), nc - col), nrs= min (as_int (rs), nr - row);
  string r;
  if (ncs > 1) r << "ColSpan=" << as_string (ncs);
  if (nrs > 1) r << (N(r) > 0? " ": "") << "RowSpan=" << as_string (nrs);
  return r;
}

static bool
is_item (tree t) {
  return is_compound (t, "item") || is_compound (t, "item*") ||
         is_compound (t, "bibitem") || is_compound (t, "bibitem*");
}

// whether a paragraph of a list begins a new item: it has an item, maybe
// after markup which draws nothing (vspace*, label, assign...)
static bool
begins_with_item (tree t) {
  if (is_item (t)) return true;
  if (!is_compound (t, "concat")) return false;
  for (int i=0; i<N(t); i++) {
    if (is_item (t[i])) return true;
    // (only markup which draws nothing may come before it)
    if (!(is_compound (t[i], "vspace") || is_compound (t[i], "vspace*") ||
          is_compound (t[i], "label") || is_compound (t[i], "assign") ||
          is_compound (t[i], "no-break-here") ||
          is_compound (t[i], "no-break-here*") ||
          is_compound (t[i], "no-page-break") ||
          is_compound (t[i], "no-page-break*") ||
          is_compound (t[i], "pause") || is_compound (t[i], "flag")))
      return false;
  }
  return false;
}

// whether a paragraph holds a heading (<section|..><label|..>): it is no
// paragraph then, but where the heading is
bool
pdf_tagger_rep::holds_heading (tree t) {
  if (!is_compound (t, "concat")) return false;
  for (int i=0; i<N(t); i++)
    if (is_compound (t[i]) && starts (role_of (as_string (L(t[i]))), "H:"))
      return true;
  return false;
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
  bool decoration= !is_nil (q) && q->item < 0;
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
    tree parent= t, c;
    if (!child (t, i, c)) break;
    above << parent;
    t= c; cur= cur * i; s= s->next;
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
    if (last == "L" && !is_document (t) &&
        (is_document (parent) || role_of (as_string (L(parent))) == "L")) {
      // a paragraph which does not begin with an item continues the item
      // before it (a second paragraph, a pause): it is in its body
      int j= i;
      while (j > 0 && !begins_with_item (parent[j])) j--;
      if (j < i && begins_with_item (parent[j])) {
        path ip= path_up (cur) * j;
        string ikey= as_string (ip) * "/i";
        r << tag_node (ikey, "LI", ip);
        r << tag_node (ikey * "/b", "LBody", ip * 1);
        last= "LBody"; li_body= true; li_key= ikey; li_pos= ip;
        string role= (is_compound (t) && has_document_child (t))? "Div": "P";
        if (is_compound (t) && role_of (as_string (L(t))) != "")
          role= role_of (as_string (L(t)));
        if (starts (role, "H:")) role= heading (as_string (cur), role);
        string alt;
        if (role == "Formula" || role == "Figure")
          alt= alt_of (as_string (cur), t, role, above, cur);
        tag_node fn (as_string (cur), role, cur, alt);
        fn.own_alt= own_alts->contains (as_string (cur));
        fn.picture= is_compound (t, "image");
        r << fn;
        last= role;
        if (is_atomic (t) || role == "Formula" || role == "Figure") break;
        continue;
      }
      // (the node may be an element too, a list in a list: another key)
      string key= as_string (cur) * "/i";
      r << tag_node (key, "LI", cur);
      last= "LI"; li_body= false; li_key= key; li_pos= cur;
      if (is_atomic (t)) break;
      // an item alone in its paragraph (<item*|Term>) is the label
      if (is_item (t)) {
        r << tag_node (as_string (cur), "Lbl", cur);
        return r;
      }
      if (!is_compound (t, "concat")) {
        r << tag_node (key * "/b", "LBody", cur);
        li_body= true;
        last= "LBody";
      }
      else continue;
    }
    // an included document, given to the include* of the include macro
    // (print_doc), is in place of the include*, which is no element
    if (is_given_inclusion (t)) continue;
    // (a paragraph or a caption may be a mere string)
    string role= child_role_of (as_string (L(parent)), i, N(parent));
    if (role == "" && is_compound (t)) role= role_of (as_string (L(t)));
    // (the document of a table in the form of a block holds its format)
    if (role == "" && is_document (parent) && !is_document (t) &&
        last != "Table" && last != "TR" && !holds_heading (t)) {
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
    if (role == "Formula" || role == "Figure")
      alt= alt_of (key, t, role, above, cur);
    tag_node nd (key, role, cur, alt);
    nd.own_alt= own_alts->contains (key);
    nd.picture= is_compound (t, "image");
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
  // the title which the macro of a Sect writes (References, Contents) is
  // its heading
  if (decoration && last == "Sect" && is_nil (s) &&
      N(r) > 0 && r[N(r)-1].pos == pos) {
    string key= r[N(r)-1].key * "/h";
    r << tag_node (key, heading (key, "H:A"), pos);
  }
  return r;
}

array<tag_node>
pdf_tagger_rep::row_cells (path pos) {
  array<tag_node> r;
  // the table and the formats around it, as resolve finds them
  array<tree> above;
  tree t= body;
  for (path q= pos; !is_nil (q); q= q->next) {
    tree c;
    if (!child (t, q->item, c)) return r;
    above << t;
    t= c;
  }
  if (!is_compound (t, "row") || N(above) < 1 ||
      !is_compound (above[N(above)-1], "table")) return r;
  array<tree> trail;
  trail << above[N(above)-1];
  for (int k= N(above)-2; k >= 0 && is_compound (above[k], "tformat"); k--)
    trail << above[k];
  tree tab= trail[0];
  int row= last_index (pos, 0);
  // the cells covered by the span of another one, once for the table
  string tkey= as_string (path_up (pos));
  bool known= this->covered->contains (tkey);
  hashset<int> covered;   // (a set of its own: the default one is shared)
  if (known) covered= this->covered [tkey];
  else this->covered (tkey)= covered;
  for (int i=0; i<N(tab) && !known; i++)
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
  int nc= table_columns (tab);
  for (int j=0; j<N(t) && j<nc; j++) {
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

/******************************************************************************
* The pictures without an alternative text (pdf-tag-check, tm-print.scm)
******************************************************************************/

// The images of t (at cur in the body) which the tagger makes Figures of
// their own without an alternative text of the user's: those which a
// tagged PDF describes by a caption or a file name. Each is resolved as
// the export resolves what it draws, so that the check and the export
// agree (an image in a formula or a drawing is not a Figure of its own).
// What a tagged PDF would not typeset as such (inactive markup, raw data)
// and the includes (checked in their own files) are not looked into.
static void
pictures_without_alt (pdf_tagger_rep& tg, tree t, path cur, array<path>& r) {
  if (is_atomic (t)) return;
  if (is_compound (t, "inactive") || is_compound (t, "inactive*") ||
      is_compound (t, "var-inactive") || is_compound (t, "raw-data") ||
      is_compound (t, "include"))
    return;
  if (is_compound (t, "image")) {
    path pos;
    array<tag_node> a= tg.resolve (reverse (cur), pos);
    if (N(a) > 0) {
      tag_node nd= a[N(a)-1];
      if (nd.role == "Figure" && nd.picture && !nd.own_alt && nd.pos == cur)
        r << cur;
    }
    return;
  }
  for (int i=0; i<N(t); i++) pictures_without_alt (tg, t[i], cur * i, r);
}

array<path>
pdf_tag_pictures_without_alt (tree body) {
  pdf_tagger_rep tg (body, path (), "", "");
  array<path> r;
  pictures_without_alt (tg, body, path (), r);
  return r;
}
