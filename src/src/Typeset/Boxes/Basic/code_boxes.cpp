
/******************************************************************************
* MODULE     : code_boxes.cpp
* DESCRIPTION: boxes for the lines of source code (the tag code-lines)
* COPYRIGHT  : (C) 2026  the TeXmacs contributors
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "font.hpp"
#include "language.hpp"
#include "Boxes/construct.hpp"
#include "analyze.hpp"
#include "colors.hpp"
#include "code_lines.hpp"

/******************************************************************************
* A code box shows the lines i0, i0+1... of a document of strings (the child of
* <code-lines|<document|line|...>>) as one box, without a box per line or per
* token, and nothing is made for a line until it is drawn or the cursor is
* in it. Its box paths are (k, pos): the line i0+k of the document, at the
* position pos in its string.
*
* A line which is wider than the box is broken into rows (soft line breaks):
* after the last space which fits, or where the row is full when a word does
* not fit (code_breaks). The box has the starts of the rows of these lines
* only (tall_k, tall_st: a sorted list), and the depth which their rows add
* before each of them (tall_cum). The rows of a line are 'rpitch' from each other (the
* lines of a paragraph), the lines 'pitch' (the paragraphs of a document);
* the first baseline is at 0.
*
* The geometry of a row (the positions of the cursor in it) is the one of a
* text box of the row, made when it is needed; the colours of the syntax are
* asked to the lexer of the block when a line is drawn (see code_lines.hpp):
* from the state of the language at the start of the line, in one pass over
* it, and kept for the last lines drawn.
*
* An edit changes the tree before the next typesetting: the box keeps the
* texts of the wrapped lines with their rows, and draws the other lines as
* they are (a line has one row whatever its text), and none past the end.
*
* A box of one row (in paper mode, see bridge_code.cpp: the rows are the
* lines of the page) holds the line itself, and its path is the one which
* the line keeps (obtain_ip), which is renumbered in place when lines are
* inserted or removed before it: the box of a line which did not change can
* be used again in the next pass. Its box paths are (0, pos).
******************************************************************************/

/******************************************************************************
* Soft line breaks
******************************************************************************/

array<int>
code_breaks (string s, font fn, SI width) {
  // the starts of the rows of s, at most width wide
  array<int> r;
  r << 0;
  int n= N(s);
  if (n == 0 || width <= 0) return r;
  SI* xpos= tm_new_array<SI> (n+1);
  fn->get_xpositions (s, xpos);
  if (xpos[n] <= width) { tm_delete_array (xpos); return r; }
  int a= 0, i= 0, after_space= -1;
  while (i < n) {
    int j= i;
    tm_char_forwards (s, j);
    if (j > n) j= n;
    if (xpos[j] - xpos[a] > width && i > a) {
      int b= (after_space > a? after_space: i);
      r << b;
      a= b;
      after_space= -1;
      continue; // the character at i on the new row
    }
    if (s[i] == ' ') after_space= j;
    i= j;
  }
  tm_delete_array (xpos);
  return r;
}

/******************************************************************************
* The colours of the syntax (see code_lines.hpp)
******************************************************************************/

// the colours kept: the lines, and their runs in all
#define CODE_RUNS_LINES 512
#define CODE_RUNS_BUDGET 1000000
// how far the states are made again beyond the line asked, after an edit
#define CODE_STATES_AHEAD 1000

static inline bool
same_text (string a, string b) {
  // (the string of a line which did not change is the same string)
  return a.operator-> () == b.operator-> () || a == b;
}

code_lexer_rep::code_lexer_rep (language lan2):
  lan (lan2), stateful (lan2->has_line_states ()), valid (0),
  fix_from (-1), fix_to (-1), kept_next (0), kept_size (0) {}

code_lexer::code_lexer (language lan): rep (tm_new<code_lexer_rep> (lan)) {}

void
code_lexer_rep::reset () {
  states= array<int> ();
  valid= 0;
  fix_from= fix_to= -1;
  kept.clear ();
  kept_next= kept_size= 0;
}

void
code_lexer_rep::inserted (int pos, int nr) {
  int n= N(states);
  if (pos < 0 || pos > n || nr < 0) { reset (); return; }
  array<int> r (n + nr);
  for (int i=0; i<pos; i++) r[i]= states[i];
  for (int i=0; i<nr; i++) r[pos+i]= 0;
  for (int i=pos; i<n; i++) r[i+nr]= states[i];
  states= r;
  if (valid > pos) valid += nr;
  if (fix_from >= pos) fix_from += nr;
  if (fix_to >= pos) fix_to += nr;
  // (the new lines are made, and the one after them is compared)
  if (pos < valid) {
    fix_from= (fix_from < 0? pos: min (fix_from, pos));
    fix_to  = max (fix_to, pos + nr - 1);
  }
  for (size_t k=0; k<kept.size (); k++)
    if (kept[k].line >= pos) kept[k].line += nr;
}

void
code_lexer_rep::removed (int pos, int nr) {
  int n= N(states);
  if (pos < 0 || nr < 0 || pos + nr > n) { reset (); return; }
  array<int> r (n - nr);
  for (int i=0; i<pos; i++) r[i]= states[i];
  for (int i=pos+nr; i<n; i++) r[i-nr]= states[i];
  states= r;
  if (valid >= pos + nr) valid -= nr;
  else if (valid > pos) valid= pos;
  if (fix_from >= pos + nr) fix_from -= nr;
  else if (fix_from >= pos) fix_from= pos;
  if (fix_to >= pos + nr) fix_to -= nr;
  else if (fix_to >= pos) fix_to= pos - 1;
  // (the line before is made again: the one after it gets its state)
  if (pos > 0 && pos - 1 < valid) {
    fix_from= (fix_from < 0? pos - 1: min (fix_from, pos - 1));
    fix_to  = max (fix_to, pos - 1);
  }
  for (size_t k=0; k<kept.size (); k++)
    if (kept[k].line >= pos + nr) kept[k].line -= nr;
    else if (kept[k].line >= pos) kept[k].line= -1;
}

void
code_lexer_rep::changed (int i) {
  if (i < 0 || i >= N(states)) { reset (); return; }
  if (i < valid) {
    fix_from= (fix_from < 0? i: min (fix_from, i));
    fix_to  = max (fix_to, i);
  }
}

int
code_lexer_rep::line_out (int j) {
  // the state after the line j
  array<int> a, b;
  array<string> c;
  tree t= doc[j];
  if (is_atomic (t) && N(t->label) > CODE_COLOR_MAX) return states[j];
  return lan->colour_line (t, states[j], a, b, c);
}

int
code_lexer_rep::state_at (int i) {
  if (!stateful || i <= 0) return 0;
  int n= N(doc);
  if (N(states) != n) {
    states= array<int> (n);
    valid= 0;
    fix_from= fix_to= -1;
  }
  if (i >= n) return 0;
  if (fix_from >= 0) {
    // the states after the lines which changed, up to the first which is
    // the same as before (or a while after the line asked)
    int j= fix_from;
    while (j + 1 < valid) {
      int out= line_out (j);
      if (j >= fix_to && out == states[j+1]) break;
      states[j+1]= out;
      j++;
      if (j > fix_to && j > i + CODE_STATES_AHEAD) { valid= j + 1; break; }
    }
    fix_from= fix_to= -1;
  }
  if (valid == 0) { states[0]= 0; valid= 1; }
  while (valid <= i) {
    states[valid]= line_out (valid - 1);
    valid++;
  }
  return states[i];
}

code_runs_entry&
code_lexer_rep::runs (int i, string text) {
  static code_runs_entry plain;
  tree t= (i >= 0 && i < N(doc)? doc[i]: tree (""));
  if (!is_atomic (t) || !same_text (t->label, text) ||
      N(text) > CODE_COLOR_MAX) {
    plain.line= -1; plain.text= text; plain.state= 0;
    plain.starts= array<int> (); plain.ends= array<int> ();
    plain.cols= array<string> ();
    plain.starts << 0; plain.ends << N(text); plain.cols << string ("");
    return plain;
  }
  int st= state_at (i);
  for (size_t k=0; k<kept.size (); k++) {
    code_runs_entry& e= kept[k];
    if (e.line == i && e.state == st &&
        e.text.operator-> () == text.operator-> ())
      return e;
  }
  code_runs_entry e;
  e.line= i; e.text= text; e.state= st;
  (void) lan->colour_line (t, st, e.starts, e.ends, e.cols);
  // (kept within the budget: the oldest go first)
  if ((int) kept.size () < CODE_RUNS_LINES) {
    kept.push_back (code_runs_entry ());
    kept_next= (int) kept.size () - 1;
  }
  else kept_next= (kept_next + 1) % CODE_RUNS_LINES;
  kept_size -= N(kept[kept_next].starts);
  kept[kept_next]= e;
  kept_size += N(e.starts);
  for (int k= (kept_next + 1) % (int) kept.size ();
       kept_size > CODE_RUNS_BUDGET && k != kept_next;
       k= (k + 1) % (int) kept.size ()) {
    kept_size -= N(kept[k].starts);
    kept[k]= code_runs_entry ();
    kept[k].line= -1;
  }
  return kept[kept_next];
}

/******************************************************************************
* The boxes
******************************************************************************/

static array<int>
code_one_row () {
  static array<int> r;
  if (N(r) == 0) r << 0;
  return r;
}

struct code_box_rep: public box_rep {
  tree     doc;    // the document of the lines (or the line: single)
  int      i0, n;  // the lines i0, ..., i0+n-1 of doc
  font     fn;
  pencil   pen;
  code_lexer lx;   // the colours of the syntax
  hashmap<string,color> cols; // the colours given as variables
  SI       pitch;  // the distance between two lines,
  SI       rpitch; // and between two rows of a line
  // The special lines (by their number in the box, in order): the lines
  // which have several rows (their texts and the starts of their rows when
  // the box was made), and the lines with markup (a box of the line, made
  // from its line items: tall_box, placed at the line). The depth which they
  // add (rows, and what a line with markup has above and below a row:
  // tall_pre, tall_post) is in tall_cum before each of them, and in all at
  // tall_cum[N(tall_k)]
  array<int>         tall_k;
  array<string>      tall_txt;
  array<array<int> > tall_st;
  array<box>         tall_box;
  array<SI>          tall_pre, tall_post;
  array<long long>   tall_cum;
  bool     single; // one row [ra, rb) of the line doc (and ip is its path),
  int      ra, rb; // whose text was one_text
  string   one_text;

  code_box_rep (path ip, tree doc, int i0, int n,
                font fn, pencil pen, code_lexer lx,
                hashmap<string,color> cols, SI pitch, SI rpitch,
                array<int> tall_k, array<string> tall_txt,
                array<array<int> > tall_st, array<box> tall_box, SI width);
  code_box_rep (path lip, tree one, string text, int ra, int rb, font fn,
                pencil pen, code_lexer lx, hashmap<string,color> cols,
                SI width);
  operator tree () { return "code lines"; }

  // lines and rows. The tree changes before the next typesetting: the box
  // draws the special lines as they were, and the others as they are, as
  // long as they are there
  int    nr_lines () { return n; }
  tree   line_tree (int k) {
    if (single) return doc;
    int i= i0 + k;
    return (i < N(doc)? doc[i]: tree ("")); }
  int    tall_lower (int k) {
    // the number of the special lines before the line k
    int lo= 0, hi= N(tall_k);
    while (lo < hi) {
      int mid= (lo + hi) / 2;
      if (tall_k[mid] < k) lo= mid + 1; else hi= mid;
    }
    return lo; }
  int    tall_index (int k) {
    int j= tall_lower (k);
    return (j < N(tall_k) && tall_k[j] == k)? j: -1; }
  bool   rich (int k) {
    if (single) return false;
    int j= tall_index (k);
    return j >= 0 && !is_nil (tall_box[j]); }
  box    rich_box (int k) { return tall_box[tall_index (k)]; }
  string line (int k) {
    if (single) return one_text;
    int j= tall_index (k);
    if (j >= 0) return tall_txt[j];
    tree t= line_tree (k);
    return is_atomic (t)? t->label: string (""); }
  path   line_ip (int k) {
    return single? ip: descend (descend (ip, 0), i0+k); }
  array<int> starts (int k) {
    if (single) { array<int> r; r << ra; return r; }
    int j= tall_index (k);
    if (j >= 0 && is_nil (tall_box[j])) return tall_st[j];
    return code_one_row (); }
  int    nr_rows (int k) { return N(starts (k)); }
  int    row_end (int k, int i) {
    if (single) return rb;
    array<int> st= starts (k);
    return i+1 < N(st)? st[i+1]: N (line (k)); }
  int    row_of (int k, int pos) {
    array<int> st= starts (k);
    int i= N(st) - 1;
    while (i > 0 && st[i] > pos) i--;
    return i; }
  // the depth of the baseline of the row i of the line k below the first,
  // and the top of the line
  long long depth (int k, int i) {
    int j= tall_lower (k);
    long long extra= (N(tall_cum) == 0? 0: tall_cum[j]);
    if (j < N(tall_k) && tall_k[j] == k) extra += tall_pre[j];
    return (long long) k * pitch + extra + (long long) i * rpitch; }
  long long top (int k) {
    int j= tall_index (k);
    return depth (k, 0) - fn->y2 - (j >= 0? tall_pre[j]: 0); }
  SI     base (int k, int i) { return (SI) -depth (k, i); }
  void   row_at (SI y, int& k, int& i);
  box    row_box (int k, int i) {
    int a= starts (k) [i], b= row_end (k, i);
    return text_box (line_ip (k), a, line (k) (a, b), fn, pen); }

  // The lines are not children for what goes through all the children
  // (subnr is 0: drawing, positioning), but a box path (k, ...) leads to a
  // box of the line k: made when it is asked for (its rows), or the box of
  // a line with markup, at its place
  box    subbox (int k);

  void      display (renderer ren);
  path      find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path      find_lip ();
  path      find_rip ();
  path      find_left_box_path ();
  path      find_right_box_path ();
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
};

code_box_rep::code_box_rep (path ip2, tree doc2, int i02, int n2, font fn2,
                            pencil pen2, code_lexer lx2,
                            hashmap<string,color> cols2, SI pitch2,
                            SI rpitch2, array<int> tall_k2,
                            array<string> tall_txt2,
                            array<array<int> > tall_st2,
                            array<box> tall_box2, SI width):
  box_rep (ip2), doc (doc2), i0 (i02), n (max (n2, 1)),
  fn (fn2), pen (pen2), lx (lx2), cols (cols2), pitch (pitch2),
  rpitch (rpitch2), tall_k (tall_k2), tall_txt (tall_txt2),
  tall_st (tall_st2), tall_box (tall_box2), single (false), ra (0), rb (0)
{
  int t= N(tall_k);
  tall_pre = array<SI> (t);
  tall_post= array<SI> (t);
  for (int j=0; j<t; j++) {
    box b= tall_box[j];
    tall_pre[j] = (is_nil (b)? 0: max ((SI) 0, b->y2 - fn->y2));
    tall_post[j]= (is_nil (b)? 0: max ((SI) 0, fn->y1 - b->y1));
  }
  if (t > 0) {
    tall_cum= array<long long> (t + 1);
    tall_cum[0]= 0;
    for (int j=0; j<t; j++)
      tall_cum[j+1]= tall_cum[j] + tall_pre[j] + tall_post[j] +
                     (long long) (N(tall_st[j]) - 1) * rpitch;
  }
  // the lines with markup at their places
  for (int j=0; j<t; j++)
    if (!is_nil (tall_box[j])) {
      tall_box[j]->x0= 0;
      tall_box[j]->y0= base (tall_k[j], 0);
    }
  int jl= tall_index (n-1);
  x1= x3= 0;
  x2= x4= width;
  y1= y3= base (n-1, nr_rows (n-1) - 1) + fn->y1 -
          (jl >= 0? tall_post[jl]: 0);
  y2= y4= fn->y2 + (tall_index (0) >= 0? tall_pre[tall_index (0)]: 0);
  for (int j=0; j<t; j++)
    if (!is_nil (tall_box[j])) {
      box b= tall_box[j];
      x1= min (x1, b->x1); x2= max (x2, b->x2);
      x3= min (x3, b->x0 + b->x3); x4= max (x4, b->x0 + b->x4);
      y3= min (y3, b->y0 + b->y3); y4= max (y4, b->y0 + b->y4);
    }
}

code_box_rep::code_box_rep (path lip, tree one, string text, int ra2,
                            int rb2, font fn2, pencil pen2, code_lexer lx2,
                            hashmap<string,color> cols2, SI width):
  box_rep (lip), doc (one), i0 (0), n (1),
  fn (fn2), pen (pen2), lx (lx2), cols (cols2), pitch (0), rpitch (0),
  single (true), ra (ra2), rb (rb2), one_text (text)
{
  x1= x3= 0;
  x2= x4= width;
  y1= y3= fn->y1;
  y2= y4= fn->y2;
}

void
code_box_rep::row_at (SI y, int& k, int& i) {
  // the row whose band (from half way to the row above to half way to the
  // row below) has y
  if (single) { k= 0; i= 0; return; }
  long long q= - (long long) y;
  long long gap= (long long) pitch - (fn->y2 - fn->y1);
  if (gap < 0) gap= 0;
  // the last line whose top (half the gap above it) is above q
  int lo= 0, hi= n-1;
  while (lo < hi) {
    int mid= (lo + hi + 1) / 2;
    if (top (mid) - gap / 2 <= q) lo= mid; else hi= mid - 1;
  }
  k= lo;
  if (rich (k)) { i= 0; return; }
  long long rgap= (long long) rpitch - (fn->y2 - fn->y1);
  if (rgap < 0) rgap= 0;
  long long d= q - (depth (k, 0) - fn->y2 - rgap / 2);
  long long ri= d / (rpitch > 0? rpitch: 1);
  int r= nr_rows (k);
  if (ri < 0) ri= 0;
  if (ri > r-1) ri= r-1;
  i= (int) ri;
}

box
code_box_rep::subbox (int k) {
  k= max (0, min (k, nr_lines () - 1));
  if (single) {
    box b= row_box (0, 0);
    b->x0= 0; b->y0= 0;
    return b;
  }
  if (rich (k)) return rich_box (k);
  array<int> tk;
  array<string> tt;
  array<array<int> > ts;
  array<box> tb;
  if (nr_rows (k) > 1) { tk << 0; tt << line (k); ts << starts (k); tb << box (); }
  box b= tm_new<code_box_rep> (ip, doc, i0+k, 1, fn, pen, lx, cols,
                               pitch, rpitch, tk, tt, ts, tb, x2);
  b->x0= 0; b->y0= base (k, 0);
  return b;
}

/******************************************************************************
* Display: only the visible rows, coloured as the language says
******************************************************************************/

void
code_box_rep::display (renderer ren) {
  SI cx1, cy1, cx2, cy2;
  ren->get_clipping (cx1, cy1, cx2, cy2);
  int ka, ia, kb, ib;
  row_at (cy2 + (fn->y2 - fn->y1), ka, ia); // (a row more on each side:
  row_at (cy1 - (fn->y2 - fn->y1), kb, ib); // those partly visible)
  color fg= pen->get_color ();
  for (int k= ka; k <= kb; k++) {
    if (rich (k)) {
      // a line with markup: its box, which draws its children
      rectangles rs;
      rich_box (k)->redraw (ren, path (), rs);
      continue;
    }
    tree t= line_tree (k);
    string s= line (k);
    if (!is_atomic (t) || N(s) == 0) continue;
    // (the number of the line in the document: for one row, the path of
    // the line has it)
    int li= (single? (is_nil (ip)? 0: ip->item): i0 + k);
    code_runs_entry& c= lx->runs (li, s);
    array<SI> xpos (N(s) + 1);
    fn->get_xpositions (s, A(xpos));
    array<int> st= starts (k);
    int lo= (k == ka? ia: 0), hi= (k == kb? ib: N(st) - 1);
    for (int i= lo; i <= hi && i < N(st); i++) {
      int a= st[i], b= row_end (k, i);
      SI y= base (k, i);
      // (the runs are in order: the first which ends after a)
      int r0= 0, r1= N(c.ends);
      while (r0 < r1) {
        int mid= (r0 + r1) / 2;
        if (c.ends[mid] <= a) r0= mid + 1; else r1= mid;
      }
      for (int r=r0; r<N(c.starts) && c.starts[r] < b; r++) {
        int s1= max (c.starts[r], a), s2= min (c.ends[r], b);
        if (s1 >= s2) continue;
        string col= c.cols[r];
        color cl= (col == ""? fg:
                   cols->contains (col)? cols[col]: named_color (col));
        ren->set_pencil (pencil (cl));
        // (a space is not drawn: the font has a visible space there)
        int u= s1;
        while (u < s2) {
          while (u < s2 && s[u] == ' ') u++;
          int v= u;
          while (v < s2 && s[v] != ' ') v++;
          if (v > u) fn->draw (ren, s (u, v), xpos[u] - xpos[a], y);
          u= v;
        }
      }
    }
  }
}

/******************************************************************************
* Cursor and selection, by the row and then by a text box of the row (or by
* the box of a line with markup)
******************************************************************************/

path
code_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  int k, i;
  row_at (y, k, i);
  if (rich (k)) {
    box b= rich_box (k);
    return path (k, b->find_box_path (x - b->x0, y - b->y0, delta, force,
                                      found));
  }
  int a= starts (k) [i];
  box rb= row_box (k, i);
  path bp= rb->find_box_path (x, y - base (k, i), delta, force, found);
  found= true;
  return path (k, a + (is_nil (bp)? 0: bp->item));
}

path
code_box_rep::find_lip () {
  if (rich (0)) return rich_box (0)->find_lip ();
  return descend (line_ip (0), single? ra: 0);
}

path
code_box_rep::find_rip () {
  int k= nr_lines () - 1;
  if (rich (k)) return rich_box (k)->find_rip ();
  return descend (line_ip (k), single? rb: N (line (k)));
}

path
code_box_rep::find_left_box_path () {
  if (rich (0)) return path (0, rich_box (0)->find_left_box_path ());
  return path (0, single? ra: 0);
}

path
code_box_rep::find_right_box_path () {
  int k= nr_lines () - 1;
  if (rich (k)) return path (k, rich_box (k)->find_right_box_path ());
  return path (k, single? rb: N (line (k)));
}

path
code_box_rep::find_box_path (path p, bool& found) {
  // p: a path in the edit tree; the lines are at reverse (ip) * (0, i, ...)
  found= false;
  path me= reverse (ip);
  if (!is_accessible (ip)) return path (0, 0);
  if (single) {
    // p: the path of the line, and a position in the row (its end only for
    // the last row of the line: else it is the start of the next row)
    if (N(p) == N(me) + 1 && path_up (p) == me) {
      int pos= last_item (p);
      if (pos >= ra && (pos < rb || (pos == rb && rb == N (line (0))))) {
        found= true;
        return path (0, pos);
      }
    }
    return path (0, ra);
  }
  if (N(p) >= N(me) + 2 && head (p, N(me)) == me && p[N(me)] == 0) {
    int i= p[N(me)+1];
    if (i >= i0 && i < i0 + nr_lines ()) {
      int k= i - i0;
      if (rich (k)) return path (k, rich_box (k)->find_box_path (p, found));
      if (N(p) == N(me) + 3) {
        found= true;
        return path (k, max (0, min (last_item (p), N (line (k)))));
      }
    }
  }
  if (N(p) == N(me) + 1 && path_up (p) == me) {
    found= true;
    if (last_item (p) == 0) return find_left_box_path ();
    return find_right_box_path ();
  }
  return path (0, 0);
}

path
code_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) {
    if (bp->item == 0) return reverse (find_lip ());
    return reverse (find_rip ());
  }
  int k= max (0, min (bp->item, nr_lines () - 1));
  if (rich (k))
    return rich_box (k)->find_tree_path (is_nil (bp->next)? path (0):
                                                             bp->next);
  int pos= is_nil (bp->next)? 0: bp->next->item;
  int lo= (single? ra: 0), hi= (single? rb: N (line (k)));
  return reverse (descend (line_ip (k), max (lo, min (pos, hi))));
}

cursor
code_box_rep::find_cursor (path bp) {
  if (is_atom (bp))
    bp= (bp->item == 0? find_left_box_path (): find_right_box_path ());
  int k= max (0, min (bp->item, nr_lines () - 1));
  if (rich (k)) {
    box b= rich_box (k);
    cursor cu= b->find_cursor (is_nil (bp->next)? path (0): bp->next);
    cu->ox += b->x0;
    cu->oy += b->y0;
    return cu;
  }
  int pos= is_nil (bp->next)? 0: bp->next->item;
  int i= row_of (k, pos);
  int a= starts (k) [i];
  cursor cu= row_box (k, i)->find_cursor (path (max (0, pos - a)));
  cu->oy += base (k, i);
  return cu;
}

selection
code_box_rep::find_selection (path lbp, path rbp) {
  if (is_atom (lbp)) lbp= find_left_box_path ();
  if (is_atom (rbp)) rbp= find_right_box_path ();
  int k1= lbp->item, k2= rbp->item;
  if (k1 == k2 && rich (k1)) {
    box b= rich_box (k1);
    selection sel= b->find_selection (lbp->next, rbp->next);
    return selection (translate (sel->rs, b->x0, b->y0),
                      find_tree_path (lbp), find_tree_path (rbp));
  }
  int p1= is_nil (lbp->next)? 0: lbp->next->item;
  int p2= is_nil (rbp->next)? 0: rbp->next->item;
  if (k1 > k2 || (k1 == k2 && !rich (k1) && p1 > p2))
    return find_selection (rbp, lbp);
  if (k1 == k2 && row_of (k1, p1) == row_of (k2, p2)) {
    int r= row_of (k1, p1), a= starts (k1) [r];
    selection sel= row_box (k1, r)->find_selection (path (p1 - a),
                                                     path (p2 - a));
    return selection (translate (sel->rs, 0, base (k1, r)),
                      find_tree_path (lbp), find_tree_path (rbp));
  }
  // from the first cursor to the right, the rows between, from the left to
  // the second cursor
  cursor c1= find_cursor (lbp), c2= find_cursor (rbp);
  SI top= fn->y2, bot= fn->y1;
  rectangles rs (rectangle (x1, c2->oy + bot, c2->ox, c2->oy + top));
  if (c2->oy + top < c1->oy + bot)
    rs= rectangles (rectangle (x1, c2->oy + top, x2, c1->oy + bot), rs);
  rs= rectangles (rectangle (c1->ox, c1->oy + bot, x2, c1->oy + top), rs);
  return selection (rs, find_tree_path (lbp), find_tree_path (rbp));
}

/******************************************************************************
* Interface
******************************************************************************/

box
code_box (path ip, tree doc, int i0, int n, font fn, pencil pen,
          code_lexer lx, hashmap<string,color> cols, SI pitch, SI rpitch,
          array<int> tall_k, array<string> tall_txt,
          array<array<int> > tall_st, array<box> tall_box, SI width) {
  return tm_new<code_box_rep> (ip, doc, i0, n, fn, pen, lx, cols,
                               pitch, rpitch, tall_k, tall_txt, tall_st,
                               tall_box, width);
}

box
code_row_box (path lip, tree line, string text, int a, int b, font fn,
              pencil pen, code_lexer lx, hashmap<string,color> cols,
              SI width) {
  return tm_new<code_box_rep> (lip, line, text, a, b, fn, pen, lx, cols,
                               width);
}
