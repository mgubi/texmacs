
/******************************************************************************
* MODULE     : pdf_tags.cpp
* DESCRIPTION: the structure of a tagged PDF, as the PDF writers build it
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pdf_tags.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include <string.h>

pdf_tags_rep::pdf_tags_rep (tagger_rep* t):
  tagger (t), index (-1), block (-2), have (false), last_elem (-1), seq (0),
  span (0) {}

// a new element, a child of parent (-1: none), with its attributes
int
pdf_tags_rep::add (tag_node nd, int parent) {
  int e= N(elems);
  pdf_tag_elem el;
  el.role= nd.role;
  el.alt = nd.alt;
  array<string> av= tokenize (nd.attrs, " ");
  for (int j=0; j<N(av); j++) {
    int eq= search_forwards ("=", av[j]);
    if (eq <= 0) continue;
    el.akeys << av[j] (0, eq);
    el.avals << as_int (av[j] (eq+1, N(av[j])));
  }
  el.parent= parent;
  el.own_alt= nd.own_alt;
  el.picture= nd.picture;
  elems << el;
  index (nd.key)= e;
  if (parent >= 0) {
    pdf_tag_kid k;
    k.parent= parent; k.kind= 0; k.id= e; k.pos= nd.pos; k.seq= seq++;
    kids << k;
  }
  return e;
}

// The elements of the box of place ip, their chain registered
void
pdf_tags_rep::resolve (path ip) {
  if (have && ip == last_ip) return;
  path pos;
  array<tag_node> chain= tagger->resolve (ip, pos);
  int parent= -1;
  for (int i=0; i<N(chain); i++) {
    tag_node& nd= chain[i];
    int e= index [nd.key];
    if (e < 0) e= add (nd, parent);
    parent= e;
    // a row has all its cells, also those which draw nothing, but not
    // those which the span of another one covers (PDF/UA)
    if (nd.role == "TR" && !rows->contains (e)) {
      rows->insert (e);
      array<tag_node> cells= tagger->row_cells (nd.pos);
      for (int j=0; j<N(cells); j++)
        if (index [cells[j].key] < 0) add (cells[j], e);
    }
  }
  last_ip  = ip;
  last_pos = pos;
  last_elem= parent;
  have= true;
}

string
pdf_tags_rep::mark (path ip, int kind, int pg) {
  if (span > 0) return "";
  resolve (ip);
  int e= last_elem;
  // the lines and fills of a formula or a picture are theirs; the others
  // (rules, borders, backgrounds) are artifacts
  if (e >= 0 && kind == 1 &&
      elems[e].role != "Formula" && elems[e].role != "Figure") e= -1;
  if (e < 0 && block == -1) return "";
  if (e >= 0 && e == block) return "";
  string r;
  if (block != -2) r << "EMC\n";
  if (e < 0) r << "/Artifact BMC\n";
  else {
    int mcid= N(mcids);
    mcids << e;
    pdf_tag_kid k;
    k.parent= e; k.kind= 1; k.id= mcid; k.page= pg;
    k.pos= last_pos; k.seq= seq++;
    kids << k;
    r << "/" << elems[e].role << " <</MCID " << as_string (mcid) << ">> BDC\n";
  }
  block= e;
  return r;
}

string
pdf_tags_rep::close () {
  if (block == -2) return "";
  block= -2;
  return "EMC\n";
}

void
pdf_tags_rep::end_page () {
  page_mcids << mcids;
  mcids= array<int> ();
  block= -2;
}

int
pdf_tags_rep::link_element (path ip, path& pos) {
  resolve (ip);
  pos= last_pos;
  int e;
  for (e= last_elem; e >= 0; e= elems[e].parent)
    if (elems[e].role == "Link") return e;
  if (N(elems) == 0) return -1;
  // a link of no hlink (the call of a footnote, its way back): a Link
  // element of its own, in the element of its text (PDF/UA)
  int parent= (last_elem >= 0? last_elem: 0);
  return add (tag_node ("link:" * as_string (N(elems)), "Link", pos), parent);
}

int
pdf_tags_rep::annot_element (path ip, path& pos) {
  resolve (ip);
  pos= last_pos;
  if (N(elems) == 0) return -1;
  int parent= (last_elem >= 0? last_elem: 0);
  return add (tag_node ("annot:" * as_string (N(elems)), "Annot", pos), parent);
}

void
pdf_tags_rep::add_annotation (int e, int id, int pg, path pos) {
  if (e < 0) return;
  pdf_tag_kid k;
  k.parent= e; k.kind= 2; k.id= id; k.page= pg; k.pos= pos; k.seq= seq++;
  kids << k;
}

bool
pdf_tags_rep::can_graft (path ip) {
  resolve (ip);
  int e= last_elem;
  // (the Figure of the picture itself, not one which holds more: a
  // drawing whose pictures are among its lines and formulas)
  return e >= 0 && elems[e].role == "Figure" && elems[e].picture &&
         !elems[e].own_alt;
}

string
pdf_tags_rep::graft (path ip, int pg, pdf_tag_graft g, int& stream) {
  resolve (ip);
  int e= last_elem;
  stream= N(stream_mcids);
  string r;
  if (block != -2) r << "EMC\n";
  block= -2;
  if (e < 0) { stream_mcids << array<int> (); return r; }
  // (a Figure has an alternative text, which hides its children)
  elems[e].role= "Div";
  elems[e].alt = "";
  int n= N(g.roles);
  array<int> el (n);
  for (int i=0; i<n; i++) {
    pdf_tag_elem x;
    x.role= g.roles[i];
    x.alt = g.alts[i];
    elems << x;
    el[i]= N(elems) - 1;
  }
  for (int i=0; i<n; i++)
    elems[el[i]].parent= (g.parents[i] < 0? e: el[g.parents[i]]);
  array<int> mc (g.mcids);
  for (int m=0; m<g.mcids; m++) mc[m]= -1;
  // the children in their order: they all have the place of the picture
  for (int t=0; t<N(g.tops); t++) {
    pdf_tag_kid k;
    k.parent= e; k.kind= 0; k.id= el[g.tops[t]]; k.pos= last_pos;
    k.seq= seq++;
    kids << k;
  }
  for (int i=0; i<n; i++)
    for (int j=0; j<N(g.content[i]); j++) {
      int c= g.content[i][j];
      pdf_tag_kid k;
      k.parent= el[i]; k.pos= last_pos; k.seq= seq++;
      if (c >= 0) { k.kind= 0; k.id= el[c]; }
      else {
        k.kind= 3; k.id= -1-c; k.page= pg; k.stream= stream;
        if (k.id < g.mcids) mc[k.id]= el[i];
      }
      kids << k;
    }
  stream_mcids << mc;
  return r;
}

static bool
tag_path_less (path a, path b) {
  while (!is_nil (a) && !is_nil (b)) {
    if (a->item != b->item) return a->item < b->item;
    a= a->next; b= b->next;
  }
  return is_nil (a) && !is_nil (b);
}

static bool
kid_less (const pdf_tag_kid& a, const pdf_tag_kid& b) {
  if (tag_path_less (a.pos, b.pos)) return true;
  if (tag_path_less (b.pos, a.pos)) return false;
  return a.seq < b.seq;
}

array<array<int> >
pdf_tags_rep::children () {
  int n= N(elems);
  array<array<int> > r (n);
  for (int i=0; i<N(kids); i++) r[kids[i].parent] << i;
  for (int e=0; e<n; e++) {
    array<int>& a= r[e];
    for (int i=1; i<N(a); i++)   // insertion sort: the lists are short
      for (int j=i; j>0 && kid_less (kids[a[j]], kids[a[j-1]]); j--) {
        int t= a[j]; a[j]= a[j-1]; a[j-1]= t; }
  }
  return r;
}

static string
xml_escape (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '<') r << "&lt;";
    else if (s[i] == '>') r << "&gt;";
    else if (s[i] == '"') r << "&quot;";
    else r << s[i];
  return r;
}

static inline bool
pdf_white (char c) {
  return c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f' ||
         c == '\0';
}

static inline bool
pdf_delimiter (char c) {
  return c == '(' || c == ')' || c == '<' || c == '>' || c == '[' ||
         c == ']' || c == '{' || c == '}' || c == '/' || c == '%';
}

// The end of an inline image whose BI ends at i: after its EI. Its data is
// skipped by its length (/L, or its size when it is not filtered), as a
// reader does: the data may hold "EI" itself; else up to the first EI
// between white spaces
static int
skip_inline_image (string s, int i) {
  int n= N(s);
  int id= i;
  while (id+1 < n && !(s[id] == 'I' && s[id+1] == 'D' &&
                       pdf_white (s[id-1]) &&
                       (id+2 == n || pdf_white (s[id+2])))) id++;
  if (id+1 >= n) return n;
  // the dictionary of the image, as name value pairs
  array<string> toks;
  for (int j= i; j < id; ) {
    if (pdf_white (s[j])) { j++; continue; }
    int k= j + 1;
    if (s[j] == '[') { while (k < id && s[k] != ']') k++; k++; }
    else if (s[j] == '(') { while (k < id && s[k] != ')') k++; k++; }
    else while (k < id && !pdf_white (s[k]) && !(s[k] == '/' && k > j) &&
                s[k] != '[' && s[k] != '(') k++;
    toks << s (j, min (k, id));
    j= k;
  }
  long long w= -1, h= -1, bpc= 8, comps= 1, len= -1;
  bool filtered= false, mask= false;
  for (int k=0; k+1<N(toks); k+=2) {
    string key= toks[k], v= toks[k+1];
    if (key == "/W" || key == "/Width") w= as_int (v);
    else if (key == "/H" || key == "/Height") h= as_int (v);
    else if (key == "/BPC" || key == "/BitsPerComponent") bpc= as_int (v);
    else if (key == "/L" || key == "/Length") len= as_int (v);
    else if (key == "/F" || key == "/Filter") filtered= (v != "[]");
    else if (key == "/IM" || key == "/ImageMask") mask= (v == "true");
    else if (key == "/CS" || key == "/ColorSpace") {
      if (v == "/RGB" || v == "/DeviceRGB" || v == "/CalRGB") comps= 3;
      else if (v == "/CMYK" || v == "/DeviceCMYK") comps= 4;
      else comps= 1;
    }
  }
  if (mask) { bpc= 1; comps= 1; }
  int start= id + 3;   // (ID and one white space)
  // (in 64 bits, and a length beyond the content is no length)
  if (len < 0 && !filtered && w > 0 && h > 0 && bpc > 0 &&
      w <= 1000000 && h <= 1000000 && bpc <= 32)
    len= h * ((w * comps * bpc + 7) / 8);
  int j= start;
  if (len >= 0 && start <= n && len <= (long long) (n - start))
    j= start + (int) len;
  // the EI after the data
  if (j < 0) j= 0;
  while (j+1 < n && !(s[j] == 'E' && s[j+1] == 'I' &&
                      (j == 0 || pdf_white (s[j-1])) &&
                      (j+2 == n || pdf_white (s[j+2])))) j++;
  return min (n, j + 2);
}

// The operators of a content stream, each with the start of its operands
// (the end of the operator before), its own place, its first operand
struct pdf_content_op {
  int seg, start, end;
  string op, first;
};

static array<pdf_content_op>
pdf_content_ops (string s) {
  array<pdf_content_op> r;
  int n= N(s), i= 0;
  int seg= 0;
  string first;
  bool have_first= false;
  while (i < n) {
    char c= s[i];
    if (pdf_white (c)) { i++; continue; }
    if (c == '%') { while (i < n && s[i] != '\n' && s[i] != '\r') i++; continue; }
    int start= i;
    if (c == '(') {
      int level= 0;
      for (; i < n; i++) {
        if (s[i] == '\\') { i++; continue; }
        if (s[i] == '(') level++;
        else if (s[i] == ')' && --level == 0) { i++; break; }
      }
    }
    else if (c == '<' && i+1 < n && s[i+1] == '<') i += 2;
    else if (c == '>' && i+1 < n && s[i+1] == '>') i += 2;
    else if (c == '<') { while (i < n && s[i] != '>') i++; i++; }
    else if (c == '[' || c == ']' || c == '{' || c == '}' || c == ')' ||
             c == '>') i++;
    else if (c == '/') {
      i++;
      while (i < n && !pdf_white (s[i]) && !pdf_delimiter (s[i])) i++;
    }
    else {
      while (i < n && !pdf_white (s[i]) && !pdf_delimiter (s[i])) i++;
      string tok= s (start, i);
      bool number= (N(tok) > 0 && (is_digit (tok[0]) || tok[0] == '-' ||
                                   tok[0] == '+' || tok[0] == '.'));
      if (!number && tok != "true" && tok != "false" && tok != "null") {
        // an operator (an inline image goes to its EI)
        if (tok == "BI") i= skip_inline_image (s, i);
        pdf_content_op o;
        o.seg= seg; o.start= start; o.end= min (i, n);
        o.op= tok; o.first= (have_first? first: string (""));
        r << o;
        seg= o.end;
        have_first= false;
        first= "";
        continue;
      }
    }
    if (i > n) i= n;
    if (i <= start) i= start + 1;
    if (!have_first) { first= s (start, min (i, n)); have_first= true; }
  }
  return r;
}

string
pdf_strip_marked_content (string s, array<bool>& open, bool& changed) {
  array<pdf_content_op> ops= pdf_content_ops (s);
  string r;
  int last= 0;
  for (int k=0; k<N(ops); k++) {
    pdf_content_op& o= ops[k];
    bool keep= true;
    if (o.op == "BMC" || (o.op == "BDC" && o.first != "/OC")) {
      open << false; keep= false;
    }
    else if (o.op == "BDC") open << true;
    else if (o.op == "EMC") {
      keep= (N(open) > 0 && open[N(open)-1]);
      if (N(open) > 0) open->resize (N(open) - 1);
    }
    if (keep) r << s (o.seg, o.end);
    else { changed= true; r << "\n"; }
    last= o.end;
  }
  r << s (last, N(s));
  return r;
}

static const char* pdf_standard_roles[]= {
  "Part", "Art", "Sect", "Div", "BlockQuote", "Caption", "TOC", "TOCI",
  "Index", "NonStruct", "Private", "P", "L", "LI", "Lbl", "LBody", "Table",
  "TR", "TH", "TD", "THead", "TBody", "TFoot", "Span", "Quote", "Note",
  "Reference", "BibEntry", "Code", "Link", "Annot", "Ruby", "RB", "RT", "RP",
  "Warichu", "WT", "WP", "Figure", "Formula", "Form", NULL };

const char*
pdf_graft_role (const char* s) {
  if (s == NULL) return NULL;
  if (!strcmp (s, "Document")) return "Div";
  if (!strcmp (s, "H") || (s[0] == 'H' && s[1] >= '1' && s[1] <= '6' &&
                           s[2] == '\0')) return "P";
  for (int i=0; pdf_standard_roles[i] != NULL; i++)
    if (!strcmp (s, pdf_standard_roles[i])) return pdf_standard_roles[i];
  return NULL;
}

pdf_tag_graft
pdf_graft_prune (array<int> parent, array<string> role, array<string> alt,
                 array<int> owner, array<int> val, int mcids,
                 hashset<int>& keep) {
  int n= N(parent);
  array<bool> has (n);
  for (int i=0; i<n; i++) has[i]= false;
  for (int j=0; j<N(val); j++)
    if (val[j] < 0 && owner[j] >= 0) has[owner[j]]= true;
  for (int i=n-1; i>=0; i--)   // (an element comes after its parent)
    if (has[i] && parent[i] >= 0) has[parent[i]]= true;
  array<int> index (n);
  pdf_tag_graft g;
  for (int i=0; i<n; i++) {
    index[i]= -1;
    if (!has[i]) continue;
    index[i]= N(g.roles);
    string a= alt[i];
    if (N(a) == 0 && (role[i] == "Figure" || role[i] == "Formula")) a= role[i];
    g.roles << role[i];
    g.alts << a;
    g.parents << (parent[i] < 0? -1: index[parent[i]]);
    g.content << array<int> ();
  }
  for (int j=0; j<N(val); j++) {
    int o= owner[j], v= val[j];
    if (v >= 0) {
      if (index[v] < 0) continue;
      if (o < 0) g.tops << index[v];
      else if (index[o] >= 0) g.content[index[o]] << index[v];
    }
    else if (o >= 0 && index[o] >= 0 && !keep->contains (-1-v)) {
      // (an MCID is the content of one element: the first)
      g.content[index[o]] << v;
      keep->insert (-1-v);
    }
  }
  // (the parent tree has an entry up to the last MCID kept)
  (void) mcids;
  g.mcids= 0;
  iterator<int> it= iterate (keep);
  while (it->busy ()) { int m= it->next (); if (m + 1 > g.mcids) g.mcids= m + 1; }
  return g;
}

// The MCID of a BDC of operands s: the /MCID of its dictionary of
// properties, or of the one its name gives (props: those of the
// resources); -1 if none. (Strings are skipped: an /MCID in a string is
// not one.)
static int
operand_mcid (string s, hashmap<string,int> props) {
  int n= N(s), i= 0, depth= 0, names= 0;
  bool want= false;
  while (i < n) {
    char c= s[i];
    if (pdf_white (c)) { i++; continue; }
    if (c == '(') {
      int level= 0;
      for (; i < n; i++) {
        if (s[i] == '\\') { i++; continue; }
        if (s[i] == '(') level++;
        else if (s[i] == ')' && --level == 0) { i++; break; }
      }
      want= false; continue;
    }
    if (c == '<' && i+1 < n && s[i+1] == '<') { depth++; i += 2; continue; }
    if (c == '>' && i+1 < n && s[i+1] == '>') { depth--; i += 2; continue; }
    if (c == '<') { while (i < n && s[i] != '>') i++; i++; want= false; continue; }
    int start= i;
    i++;
    while (i < n && !pdf_white (s[i]) && !pdf_delimiter (s[i])) i++;
    string tok= s (start, i);
    if (depth == 0 && c == '/') {
      // the tag, then the name of the properties
      if (++names == 2) return props->contains (tok) ? props [tok]: -1;
      continue;
    }
    if (want) {
      if (N(tok) > 0 && N(tok) <= 9 && is_digit (tok[0])) return as_int (tok);
      want= false;
    }
    if (depth == 1 && tok == "/MCID") want= true;
  }
  return -1;
}

// Whether the content t of a grafted picture is all tagged: each painting
// operator in kept marked content or in an artifact, never one in the
// other, every block closed (else the picture is not grafted)
static bool
graft_content_ok (string t, hashset<int> keep, hashmap<string,int> props) {
  array<pdf_content_op> ops= pdf_content_ops (t);
  array<int> stack;   // 1 tagged, 2 artifact, 0 other
  for (int k=0; k<N(ops); k++) {
    pdf_content_op& o= ops[k];
    string op= o.op;
    if (op == "BDC" || op == "BMC") {
      int kind= 0;
      if (o.first == "/Artifact") kind= 2;
      else if (op == "BDC") {
        int m= operand_mcid (t (o.seg, o.start), props);
        if (m >= 0 && keep->contains (m)) kind= 1;
        else if (m >= 0) return false;   // (an MCID of no element)
      }
      for (int j=0; j<N(stack); j++)
        if ((stack[j] == 1 && kind == 2) || (stack[j] == 2 && kind == 1))
          return false;
      stack << kind;
    }
    else if (op == "EMC") {
      if (N(stack) == 0) return false;
      stack->resize (N(stack) - 1);
    }
    else if (op == "Tj" || op == "TJ" || op == "'" || op == "\"" ||
             op == "Do" || op == "BI" || op == "sh" || op == "f" ||
             op == "F" || op == "f*" || op == "B" || op == "B*" ||
             op == "b" || op == "b*" || op == "S" || op == "s") {
      bool marked= false;
      for (int j=0; j<N(stack); j++) if (stack[j] != 0) marked= true;
      if (!marked) return false;
    }
  }
  return N(stack) == 0;
}

string
pdf_graft_marked_content (string s, hashset<int> keep,
                          hashmap<string,int> props, bool& changed,
                          bool& ok) {
  array<pdf_content_op> ops= pdf_content_ops (s);
  int n= N(ops);
  // the marked content of the MCIDs of no element: an artifact, unless it
  // holds marked content which is kept, or is in some (tagged content may
  // not be in an artifact, nor an artifact in it): then it is taken out;
  // the same for the picture's own artifacts there
  array<int> kind (n);     // for a BDC or a BMC: 0 kept, 1 artifact, 2 out
  array<int> stack;
  array<bool> holds (n), inside (n), artifact (n);
  for (int k=0; k<n; k++) {
    kind[k]= 0; holds[k]= false; inside[k]= false; artifact[k]= false; }
  for (int k=0; k<n; k++) {
    pdf_content_op& o= ops[k];
    if (o.op == "BDC" || o.op == "BMC") {
      bool kept= false;
      for (int j=0; j<N(stack); j++)
        if (kind[stack[j]] == 3) inside[k]= true;
      if (o.first == "/Artifact") artifact[k]= true;
      else if (o.op == "BDC") {
        int m= operand_mcid (s (o.seg, o.start), props);
        if (m >= 0 && keep->contains (m)) kept= true;
        else if (m >= 0) kind[k]= 1;
      }
      if (kept) {
        for (int j=0; j<N(stack); j++) holds[stack[j]]= true;
        kind[k]= 3;   // (kept: 0 below)
      }
      stack << k;
    }
    else if (o.op == "EMC" && N(stack) > 0) stack->resize (N(stack) - 1);
  }
  for (int k=0; k<n; k++) {
    if (kind[k] == 3) kind[k]= 0;
    else if (kind[k] == 1 && (holds[k] || inside[k])) kind[k]= 2;
    else if (artifact[k] && (holds[k] || inside[k])) kind[k]= 2;
  }
  string r;
  int last= 0;
  stack= array<int> ();
  for (int k=0; k<n; k++) {
    pdf_content_op& o= ops[k];
    int how= 0;
    if (o.op == "BDC" || o.op == "BMC") { how= kind[k]; stack << how; }
    else if (o.op == "EMC") {
      if (N(stack) == 0) how= 2;   // (a stray EMC)
      else {
        how= stack[N(stack)-1];
        stack->resize (N(stack) - 1);
        if (how == 1) how= 0;   // (its EMC stays)
      }
    }
    if (how == 0) r << s (o.seg, o.end);
    else if (how == 1) { r << "\n/Artifact BMC"; changed= true; }
    else { r << "\n"; changed= true; }
    last= o.end;
  }
  r << s (last, N(s));
  ok= graft_content_ok (r, keep, props);
  return r;
}

string
pdf_tags_xmp (string title, string author, string producer) {
  string xmp;
  xmp << "<?xpacket begin=\"\xef\xbb\xbf\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n"
      << "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">\n"
      << "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n"
      << "<rdf:Description rdf:about=\"\" "
      << "xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\n"
      << "<dc:format>application/pdf</dc:format>\n"
      << "<dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">"
      << xml_escape (title) << "</rdf:li></rdf:Alt></dc:title>\n";
  if (N(author) > 0)
    xmp << "<dc:creator><rdf:Seq><rdf:li>" << xml_escape (author)
        << "</rdf:li></rdf:Seq></dc:creator>\n";
  xmp << "</rdf:Description>\n"
      << "<rdf:Description rdf:about=\"\" "
      << "xmlns:pdfuaid=\"http://www.aiim.org/pdfua/ns/id/\">\n"
      << "<pdfuaid:part>1</pdfuaid:part>\n</rdf:Description>\n"
      << "<rdf:Description rdf:about=\"\" "
      << "xmlns:pdf=\"http://ns.adobe.com/pdf/1.3/\">\n"
      << "<pdf:Producer>" << xml_escape (producer) << "</pdf:Producer>\n"
      << "</rdf:Description>\n"
      << "<rdf:Description rdf:about=\"\" "
      << "xmlns:xmp=\"http://ns.adobe.com/xap/1.0/\">\n"
      << "<xmp:CreatorTool>" << xml_escape (producer) << "</xmp:CreatorTool>\n"
      << "</rdf:Description>\n</rdf:RDF>\n</x:xmpmeta>\n"
      << "<?xpacket end=\"w\"?>";
  return xmp;
}
