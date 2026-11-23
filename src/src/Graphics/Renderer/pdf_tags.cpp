
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

string
pdf_strip_marked_content (string s, array<bool>& open, bool& changed) {
  string r;
  int n= N(s), i= 0;
  int seg= 0;              // the start of the operands of the next operator
  string first;            // its first operand
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
        // an operator
        if (tok == "BI") {
          // an inline image: its data, up to EI, as it is
          int j= i;
          while (j+2 < n && !(pdf_white (s[j]) && s[j+1] == 'E' &&
                              s[j+2] == 'I' &&
                              (j+3 == n || pdf_white (s[j+3])))) j++;
          i= min (n, j + 3);
          r << s (seg, i);
        }
        else if (tok == "BMC" || (tok == "BDC" && first != "/OC")) {
          open << false;
          changed= true;
          r << "\n";
        }
        else if (tok == "BDC") {
          open << true;
          r << s (seg, i);
        }
        else if (tok == "EMC") {
          bool kept= (N(open) > 0 && open[N(open)-1]);
          if (N(open) > 0) open->resize (N(open) - 1);
          if (kept) r << s (seg, i);
          else { changed= true; r << "\n"; }
        }
        else r << s (seg, i);
        seg= i;
        have_first= false;
        first= "";
        continue;
      }
    }
    if (i <= start) i= start + 1;
    if (!have_first) { first= s (start, i); have_first= true; }
  }
  r << s (seg, n);
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
