
/******************************************************************************
* MODULE     : code_boxes.cpp
* DESCRIPTION: boxes for the lines of source code (code-lines)
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
#include "code_boxes.hpp"
#include "Boxes/change.hpp"
#include "code_lines.hpp"

extern int redraw_uninterrupted; // (boxes.cpp)

bool
code_line_hints (tree t, SI cell, array<int>& hp, array<SI>& hw,
                 array<string>& ht) {
  if (!code_line_has_hints (t)) return false;
  code_line_layers l;
  if (!code_line_layers_of (t, l) || N(l.hint_pos) == 0) return false;
  for (int k=0; k<N(l.hint_pos); k++) {
    string h= l.hint_text[k];
    int chars= 0, i= 0;
    while (i < N(h)) { tm_char_forwards (h, i); chars++; }
    hp << l.hint_pos[k];
    hw << (SI) chars * cell;
    ht << h;
  }
  return true;
}

// The runs of colours of a line with the colours of its layers over them
// (the semantic tokens of a language server, code_lines.hpp): a piece of a
// layer has its colour, the rest that of the syntax, as in
// typeset_layered_string. The pieces are in order and do not overlap
static void
code_overlay_layers (code_runs_entry& c, code_line_layers& l,
                     code_runs_entry& out) {
  out.line= -1; out.text= c.text; out.state= c.state;
  int k= 0;
  for (int r=0; r<N(c.starts); r++) {
    int x= c.starts[r], b= c.ends[r];
    while (x < b) {
      while (k < N(l.col_start) && l.col_end[k] <= x) k++;
      int y= b;
      string col= c.cols[r];
      if (k < N(l.col_start) && l.col_start[k] <= x) {
        col= l.col_color[k];
        y= min (y, l.col_end[k]);
      }
      else if (k < N(l.col_start)) y= min (y, l.col_start[k]);
      out.starts << x; out.ends << y; out.cols << col;
      x= y;
    }
  }
}

/******************************************************************************
* A code box shows the lines i0, i0+1... of a document of strings (typeset as
* lines of code, see bridge_code.cpp) as one box, without a box per line or per
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
* asked to the lexer of the block when a line is drawn (see code_boxes.hpp):
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

int
code_control (string s, int i, int& j) {
  int n= N(s);
  if (i + 3 >= n || s[i] != '<' || s[i+1] != '#') return -1;
  int k= i + 2;
  while (k < n && s[k] != '>') k++;
  if (k >= n || k == i + 2) return -1;
  int c= from_hexadecimal (s (i+2, k));
  if (c >= 32 && c != 127) return -1;
  j= k + 1;
  return c;
}

// the characters of s from a to b for the heights of the selections and of
// the cursor, without the spaces and the control characters (a tab, <#x>):
// the heights are then those of the same text in a paragraph in prog mode,
// where a space is a box of its own, and a tab a box (typeset_prog_tab)
// which does not count in them; the space of a monospaced font, and the
// box of a control character, may be glyphs which go below the baseline or
// above the line
static string
code_ink (string s, int a, int b) {
  string m;
  int i= a;
  while (i < b) {
    int j;
    int c= code_control (s, i, j);
    if (c >= 0) { i= j; continue; }
    if (s[i] != ' ') m << s[i];
    i++;
  }
  return m;
}

void
code_xpos (string s, int upto, font fn, SI cell, int tabw, array<SI>& xpos,
           array<int> hp, array<SI> hw) {
  int n= max (0, min (N(s), upto));
  string t= s (0, n);
  array<SI> raw (n + 1);
  raw[0]= 0;
  if (n > 0) fn->get_xpositions (t, A(raw));
  xpos= array<SI> (n + 1);
  xpos[0]= raw[0];
  SI shift= 0;
  int i= 0, h= 0;
  // (the hints before the character at i: the position i is before them,
  // the character after them, and those after it further; pend: their
  // width, where the character starts)
  SI pend= 0;
  auto hints_at= [&] (int i) {
    pend= 0;
    while (h < N(hp) && hp[h] <= i) {
      if (hp[h] == i) { shift += hw[h]; pend += hw[h]; }
      h++;
    }
  };
  while (i < n) {
    hints_at (i);
    int j= i;
    int c= code_control (t, i, j);
    if (c < 0) {
      j= i;
      tm_char_forwards (t, j);
      if (j > n) j= n;
      for (int k=i+1; k<=j; k++) xpos[k]= raw[k] + shift;
      i= j;
      continue;
    }
    // a tab, to the next stop; another control character, a cell
    SI x= xpos[i] + pend, end= x + cell;
    if (c == 9 && tabw > 0 && cell > 0) {
      SI stop= (SI) tabw * cell;
      end= (x / stop + 1) * stop;
    }
    for (int k=i+1; k<j; k++) xpos[k]= x;
    xpos[j]= end;
    shift= end - raw[j];
    i= j;
  }
}

SI
code_hint_before (int p, array<int>& hp, array<SI>& hw) {
  SI w= 0;
  for (int h=0; h<N(hp); h++) if (hp[h] == p) w += hw[h];
  return w;
}

array<int>
code_breaks (string s, font fn, SI width, SI cell, int tabw,
             array<int> hp, array<SI> hw) {
  // the starts of the rows of s, at most width wide
  array<int> r;
  r << 0;
  int n= N(s);
  if (n == 0 || width <= 0) return r;
  array<SI> xpos;
  code_xpos (s, n, fn, cell, tabw, xpos, hp, hw);
  // (the hints at the end of the line take room in its last row)
  SI tail= code_hint_before (n, hp, hw);
  if (xpos[n] + tail <= width) return r;
  int a= 0, i= 0, after_space= -1;
  while (i < n) {
    int j= i;
    if (code_control (s, i, j) < 0) { j= i; tm_char_forwards (s, j); }
    if (j > n) j= n;
    if (xpos[j] + (j == n? tail: 0) - xpos[a] > width && i > a) {
      int b= (after_space > a? after_space: i);
      r << b;
      a= b;
      after_space= -1;
      continue; // the character at i on the new row
    }
    if (s[i] == ' ') after_space= j;
    i= j;
  }
  return r;
}

/******************************************************************************
* The colours of the syntax (see code_boxes.hpp)
******************************************************************************/

// the colours kept: the lines, and their runs in all
#define CODE_RUNS_LINES 512
#define CODE_RUNS_BUDGET 1000000
// how far the states are made again beyond the line asked, after an edit
#define CODE_STATES_AHEAD 1000


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
  // (the new lines start in the state of the line which was at pos: the
  // state after the line before, which did not change; the fix below makes
  // the states after them)
  int start= (pos < n? states[pos]: 0);
  for (int i=0; i<nr; i++) r[pos+i]= start;
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
  // (the line before is made again: the one after it gets its state; when
  // the first lines go, the new first line starts in the state 0, and the
  // ones after it are made again from there)
  if (pos > 0 && pos - 1 < valid) {
    fix_from= (fix_from < 0? pos - 1: min (fix_from, pos - 1));
    fix_to  = max (fix_to, pos - 1);
  }
  else if (pos == 0 && N(states) > 0 && valid > 0) {
    states[0]= 0;
    fix_from= 0;
    fix_to  = max (fix_to, 0);
  }
  for (size_t k=0; k<kept.size (); k++)
    if (kept[k].line >= pos + nr) kept[k].line -= nr;
    else if (kept[k].line >= pos) kept[k].line= -1;
}

void
code_lexer_rep::changed (int i) {
  // (the colours kept for the line go: they may change with the same text,
  // when they come from elsewhere, as those of tree-sitter)
  for (size_t k=0; k<kept.size (); k++)
    if (kept[k].line == i) kept[k].line= -1;
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
  if (!stateful) {
    // the colours of a language without states of lines are asked each
    // time: they may come from elsewhere (tree-sitter: the highlight
    // observers of the lines, which change after the parse of the region,
    // treesit-schedule-updates), and are cheap to read
    static code_runs_entry once;
    once.line= -1; once.text= text; once.state= 0;
    once.starts= array<int> (); once.ends= array<int> ();
    once.cols= array<string> ();
    (void) lan->colour_line (t, 0, once.starts, once.ends, once.cols);
    return once;
  }
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
  SI       cell;   // the width of a cell (a character)
  int      tabw;   // the tabs stop every tabw cells
  int      invis;  // the spaces, tabs, ends of lines... are shown (2: not
                  // the end of the line: the last line of the block)
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
  // the folds (System/Language/code_lines.hpp): 1, the line is hidden (it
  // takes no room and is not drawn: the lines after it are a line higher);
  // 2, hidden lines follow it (it ends with a mark)
  array<int>         tall_fold;
  array<SI>          tall_pre, tall_post;
  array<long long>   tall_cum;
  bool     single; // one row [ra, rb) of the line doc (and ip is its path),
  int      ra, rb; // whose text was one_text
  string   one_text;

  code_box_rep (path ip, tree doc, int i0, int n,
                font fn, pencil pen, code_lexer lx,
                hashmap<string,color> cols, SI pitch, SI rpitch,
                SI cell, int tabw, int invis,
                array<int> tall_k, array<string> tall_txt,
                array<array<int> > tall_st, array<box> tall_box,
                array<int> tall_fold, SI width);
  code_box_rep (path lip, tree one, string text, int ra, int rb, font fn,
                pencil pen, code_lexer lx, hashmap<string,color> cols,
                SI cell, int tabw, int invis, SI width);
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
  int    fold (int k) {
    if (single) return 0;
    int j= tall_index (k);
    return j >= 0? tall_fold[j]: 0; }
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
    return single? ip: descend (ip, i0+k); }
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
  // the positions of the characters of the line k up to upto (see
  // code_xpos), the position in the row i of the line k nearest to x, and
  // the extents of the text between two positions (as a text box)
  void   xpositions (int k, int upto, array<SI>& xp) {
    // (with the inlay hints of the line: see code_line_hints)
    array<int> hp; array<SI> hw; array<string> ht;
    hints (k, hp, hw, ht);
    code_xpos (line (k), upto, fn, cell, tabw, xp, hp, hw); }
  bool   hints (int k, array<int>& hp, array<SI>& hw, array<string>& ht) {
    tree t= line_tree (k);
    return is_atomic (t) && same_text (t->label, line (k)) &&
           code_line_hints (t, cell, hp, hw, ht); }
  int    pos_at (int k, int i, SI x, SI delta);
  // (the last line of the block does not end with a newline)
  bool   has_end (int k) {
    return invis == 1 && (single || i0 + k < N(doc) - 1); }
  void   draw_invisibles (renderer ren, string s, array<SI>& xp, int a,
                          int b, bool last, SI y,
                          array<int> hp= array<int> (),
                          array<SI> hw= array<SI> ());
  void   draw_fold_mark (renderer ren, int k);
  void   draw_hint (renderer ren, string txt, SI x, SI y);
  int    get_type () { return CODE_BOX; }
  void   end_of_lines (SI& x, SI& y);
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

// (tm_new takes at most 15 arguments)
template<typename... A> static inline code_box_rep*
code_new (const A&... a) {
  void* ptr= fast_new (sizeof (code_box_rep));
  (void) new (ptr) code_box_rep (a...);
  return (code_box_rep*) ptr;
}

code_box_rep::code_box_rep (path ip2, tree doc2, int i02, int n2, font fn2,
                            pencil pen2, code_lexer lx2,
                            hashmap<string,color> cols2, SI pitch2,
                            SI rpitch2, SI cell2, int tabw2, int invis2,
                            array<int> tall_k2,
                            array<string> tall_txt2,
                            array<array<int> > tall_st2,
                            array<box> tall_box2, array<int> tall_fold2,
                            SI width):
  box_rep (ip2), doc (doc2), i0 (i02), n (max (n2, 1)),
  fn (fn2), pen (pen2), lx (lx2), cols (cols2), pitch (pitch2),
  rpitch (rpitch2), cell (cell2), tabw (tabw2), invis (invis2),
  tall_k (tall_k2), tall_txt (tall_txt2),
  tall_st (tall_st2), tall_box (tall_box2), tall_fold (tall_fold2),
  single (false), ra (0), rb (0)
{
  int t= N(tall_k);
  tall_pre = array<SI> (t);
  tall_post= array<SI> (t);
  for (int j=0; j<t; j++) {
    box b= (tall_fold[j] == 1? box (): tall_box[j]);
    tall_pre[j] = (is_nil (b)? 0: max ((SI) 0, b->y2 - fn->y2));
    tall_post[j]= (is_nil (b)? 0: max ((SI) 0, fn->y1 - b->y1));
  }
  if (t > 0) {
    tall_cum= array<long long> (t + 1);
    tall_cum[0]= 0;
    for (int j=0; j<t; j++)
      tall_cum[j+1]= tall_cum[j] +
        (tall_fold[j] == 1? - (long long) pitch:
         tall_pre[j] + tall_post[j] +
         (long long) (N(tall_st[j]) - 1) * rpitch);
  }
  // the lines with markup at their places
  for (int j=0; j<t; j++)
    if (!is_nil (tall_box[j])) {
      tall_box[j]->x0= 0;
      tall_box[j]->y0= base (tall_k[j], 0);
    }
  // (the bottom of the last line which is shown: hidden lines at the end,
  // a fold of the last lines, take no row)
  int kl= n-1;
  while (kl > 0 && tall_index (kl) >= 0 && tall_fold[tall_index (kl)] == 1)
    kl--;
  int jl= tall_index (kl);
  x1= x3= 0;
  x2= width;
  // (the marks drawn after the texts, the end of a line and the "..." of a
  // fold, may go beyond the width of a full row)
  x4= width + 4 * cell;
  y1= y3= base (kl, nr_rows (kl) - 1) + fn->y1 -
          (jl >= 0? tall_post[jl]: 0);
  // (the top of the first line: depth (0, 0) already counts its height)
  y2= y4= fn->y2;
  for (int j=0; j<t; j++)
    if (!is_nil (tall_box[j]) && tall_fold[j] != 1) {
      box b= tall_box[j];
      x1= min (x1, b->x1); x2= max (x2, b->x2);
      x3= min (x3, b->x0 + b->x3); x4= max (x4, b->x0 + b->x4);
      y3= min (y3, b->y0 + b->y3); y4= max (y4, b->y0 + b->y4);
    }
}

code_box_rep::code_box_rep (path lip, tree one, string text, int ra2,
                            int rb2, font fn2, pencil pen2, code_lexer lx2,
                            hashmap<string,color> cols2, SI cell2, int tabw2,
                            int invis2, SI width):
  box_rep (lip), doc (one), i0 (0), n (1),
  fn (fn2), pen (pen2), lx (lx2), cols (cols2), pitch (0), rpitch (0),
  cell (cell2), tabw (tabw2), invis (invis2),
  single (true), ra (ra2), rb (rb2), one_text (text)
{
  x1= x3= 0;
  x2= width;
  x4= width + 4 * cell; // (the mark of the end of the line, see above)
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
  array<int> tf;
  if (nr_rows (k) > 1) {
    tk << 0; tt << line (k); ts << starts (k); tb << box (); tf << 0; }
  box b= code_new (ip, doc, i0+k, 1, fn, pen, lx, cols,
                               pitch, rpitch, cell, tabw, invis,
                               tk, tt, ts, tb, tf, x2);
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
    int fk= fold (k);
    if (fk == 1) continue;
    if (fk == 2) draw_fold_mark (ren, k);
    if (rich (k)) {
      // a line with markup: its box, which draws its children
      // (drawn whole: this box reports its area as painted)
      rectangles rs;
      redraw_uninterrupted++;
      rich_box (k)->redraw (ren, path (), rs);
      redraw_uninterrupted--;
      continue;
    }
    tree t= line_tree (k);
    string s= line (k);
    if (!is_atomic (t)) continue;
    if (N(s) == 0) {
      // (the hints of an empty line, at its start)
      array<int> hp; array<SI> hw; array<string> ht;
      bool hinted= hints (k, hp, hw, ht);
      if (hinted)
        for (int h=0; h<N(hp); h++)
          if (hp[h] == 0) draw_hint (ren, ht[h], 0, base (k, 0));
      if (invis && (!single || rb == 0)) {
        array<SI> xp (1);
        xp[0]= 0;
        draw_invisibles (ren, s, xp, 0, 0, has_end (k), base (k, 0), hp, hw);
      }
      continue;
    }
    // (the number of the line in the document: for one row, the path of
    // the line has it)
    int li= (single? (is_nil (ip)? 0: ip->item): i0 + k);
    code_runs_entry& c0= lx->runs (li, s);
    // (the colours of the layers of a server over those of the syntax)
    code_line_layers layers;
    code_runs_entry over;
    bool layered= code_line_layers_of (t, layers) &&
                  N(layers.col_start) > 0 && same_text (t->label, s);
    if (layered) code_overlay_layers (c0, layers, over);
    code_runs_entry& c= (layered? over: c0);
    array<SI> xpos;
    xpositions (k, N(s), xpos);
    // (the inlay hints of a server: the position of a hint is before it,
    // its character after it, see code_xpos)
    array<int> hp; array<SI> hw; array<string> ht;
    bool hinted= hints (k, hp, hw, ht);
    auto hint_at= [&] (int p) {
      for (int h=0; h<N(hp); h++) if (hp[h] == p) return true;
      return false; };
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
      // (the hints of the row, drawn in the order of the text, before the
      // pieces after them: the text of a printed file is read in order)
      int hn= 0;
      auto hints_upto= [&] (int u) {
        bool some= false;
        while (hinted && hn < N(hp) && hp[hn] <= u) {
          int p= hp[hn];
          if (p >= a && (p < b || (p == b && b == N(s)))) {
            draw_hint (ren, ht[hn], xpos[p] - xpos[a], y);
            some= true;
          }
          hn++;
        }
        return some;
      };
      for (int r=r0; r<N(c.starts) && c.starts[r] < b; r++) {
        int s1= max (c.starts[r], a), s2= min (c.ends[r], b);
        if (s1 >= s2) continue;
        string col= c.cols[r];
        color cl= (col == ""? fg:
                   cols->contains (col)? cols[col]: named_color (col));
        ren->set_pencil (pencil (cl));
        // (a space is not drawn: the font has a visible space there; nor a
        // tab or another control character: see draw_invisibles)
        int u= s1, j;
        while (u < s2) {
          if (s[u] == ' ') { u++; continue; }
          if (code_control (s, u, j) >= 0) { u= j; continue; }
          // (a piece is cut at a hint, which is drawn before its character)
          int v= u;
          while (v < s2 && s[v] != ' ' && code_control (s, v, j) < 0 &&
                 (v == u || !hinted || !hint_at (v)))
            tm_char_forwards (s, v);
          if (v > s2) v= s2;
          SI before= (hinted? code_hint_before (u, hp, hw): 0);
          if (hints_upto (u)) ren->set_pencil (pencil (cl));
          fn->draw (ren, s (u, v), xpos[u] + before - xpos[a], y);
          u= v;
        }
      }
      // the hints after the last piece (before spaces, control
      // characters, or at the end of the line)
      hints_upto (b);
      if (invis || search_forwards ("<#9>", a, s) >= 0)
        draw_invisibles (ren, s, xpos, a, b,
                         i == N(st) - 1 && (!single || rb == N(s)) &&
                         has_end (k), y, hp, hw);
    }
  }
}

// The spaces (a dot), the tabs (an arrow to their stop), the other control
// characters (a box) of the row [a, b) of the line s at the baseline y, and
// its end (an arrow back) when it is the last row of the line
void
code_box_rep::draw_invisibles (renderer ren, string s, array<SI>& xp,
                               int a, int b, bool last, SI y,
                               array<int> hp, array<SI> hw) {
  ren->set_pencil (pencil (named_color ("#a0a0a0"), max (fn->wline, PIXEL)));
  SI x0= xp[a], ym= y + fn->yx / 2, d= max (cell / 12, PIXEL), h= cell / 4;
  int i= a;
  while (i < b) {
    int j= i;
    int c= code_control (s, i, j);
    if (c < 0) { j= i; tm_char_forwards (s, j); }
    // (the character starts after the hints before it: see code_xpos)
    SI xa= xp[i] + code_hint_before (i, hp, hw) - x0;
    SI xb= xp[min (j, N(xp) - 1)] - x0;
    // (the tabs are always shown, as by typeset_prog_tab; the others, and
    // the tabs as arrows to their stops, with code-invisibles)
    if (c < 0 && s[i] == ' ') {
      if (!invis) { i= j; continue; }
      SI cx= (xa + xb) / 2;
      ren->fill (cx - d, ym - d, cx + d, ym + d);
    }
    else if (c == 9 && !invis) {
      // (a small arrow at its start, as typeset_prog_tab draws it in a
      // paragraph, also on paper)
      ren->set_pencil (pencil (named_color ("#c8c8c8"), pen->get_width ()));
      fn->draw (ren, "<rightarrow>", xa, y);
      ren->set_pencil (pencil (named_color ("#a0a0a0"),
                               max (fn->wline, PIXEL)));
    }
    else if (c == 9) {
      // (with the invisible characters: an arrow up to its stop)
      SI l= xa + cell / 6, r= max (xb - cell / 6, l + h);
      ren->line (l, ym, r, ym);
      ren->line (r, ym, r - h, ym + h);
      ren->line (r, ym, r - h, ym - h);
    }
    else if (c >= 0 && invis) {
      SI l= xa + cell / 6, r= xb - cell / 6, bt= y, tp= y + fn->yx;
      ren->line (l, bt, r, bt); ren->line (r, bt, r, tp);
      ren->line (r, tp, l, tp); ren->line (l, tp, l, bt);
    }
    i= j;
  }
  if (last) {
    // the end of the line (after the hints at its end)
    SI x= xp[min (b, N(xp) - 1)] + code_hint_before (b, hp, hw) - x0 + cell / 6;
    SI r= x + (cell * 2) / 3, tp= y + fn->yx, md= y + fn->yx / 3;
    ren->line (r, tp, r, md);
    ren->line (r, md, x, md);
    ren->line (x, md, x + h, md + h);
    ren->line (x, md, x + h, md - h);
  }
}

void
code_box_rep::end_of_lines (SI& x, SI& y) {
  // the end of the last row of the last line (after its hints) and its
  // baseline
  int k= nr_lines () - 1;
  if (rich (k)) {
    box b= rich_box (k);
    x= b->x0 + b->x2; y= b->y0;
    return;
  }
  int i= nr_rows (k) - 1, a= starts (k) [i], e= row_end (k, i);
  array<SI> xp;
  xpositions (k, e, xp);
  x= xp[e] - xp[a];
  array<int> hp; array<SI> hw; array<string> ht;
  if (e == N (line (k)) && hints (k, hp, hw, ht))
    x += code_hint_before (e, hp, hw);
  y= base (k, i);
}

bool
code_box_end (box b, SI& x, SI& y) {
  if (is_nil (b) || b->get_type () != CODE_BOX) return false;
  ((code_box_rep*) b.operator-> ())->end_of_lines (x, y);
  return true;
}

void
code_box_rep::draw_hint (renderer ren, string txt, SI x, SI y) {
  // an inlay hint of a server, in grey, a cell for each character from x
  // (a space is not drawn: see typeset_layer_text)
  ren->set_pencil (pencil (named_color ("#909090")));
  int u= 0, cnt= 0;
  while (u < N(txt)) {
    int v= u;
    tm_char_forwards (txt, v);
    if (txt[u] != ' ') fn->draw (ren, txt (u, v), x + cnt * cell, y);
    cnt++;
    u= v;
  }
}

void
code_box_rep::draw_fold_mark (renderer ren, int k) {
  // after the last row of the line k: a space, then "..." (as the mark of
  // bridge_rep::fold_line)
  int i= nr_rows (k) - 1;
  SI  x;
  if (rich (k)) { box b= rich_box (k); x= b->x0 + b->x2; }
  else {
    array<SI> xp;
    xpositions (k, row_end (k, i), xp);
    x= xp[row_end (k, i)] - xp[starts (k) [i]];
    // (after the hints at the end of the line)
    array<int> hp; array<SI> hw; array<string> ht;
    if (hints (k, hp, hw, ht)) x += code_hint_before (row_end (k, i), hp, hw);
  }
  x += fn->spc->def;
  ren->set_pencil (pencil (named_color ("dark grey"), pen->get_width ()));
  fn->draw (ren, "...", x, base (k, i));
}

int
code_box_rep::pos_at (int k, int i, SI x, SI delta) {
  // as text_box_rep::find_box_path: the position before the character
  // whose middle is after x
  string s= line (k);
  int a= starts (k) [i], b= row_end (k, i), j;
  array<SI> xp;
  xpositions (k, b, xp);
  array<int> hp; array<SI> hw; array<string> ht;
  bool hinted= hints (k, hp, hw, ht);
  SI x0= xp[a];
  int p= a, last= a;
  while (p < b) {
    int q= p;
    if (code_control (s, p, j) >= 0) q= j; else tm_char_forwards (s, q);
    if (q > b) q= b;
    // (the middle of the character, after the hints before it: see
    // code_xpos)
    SI from= xp[p] + (hinted? code_hint_before (p, hp, hw): 0);
    SI m= ((from - x0) + (xp[q] - x0)) >> 1;
    if (x < m || (x == m && delta < 0)) return p;
    last= p;
    p= q;
  }
  // (beyond a row which is not the last of its line: before its last
  // character, since b is the start of the next row, where the cursor of b
  // is drawn; in paper mode too, where a row is a box of its own)
  if (b < N(s) && b > a) return last;
  return b;
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
  (void) force;
  found= true;
  return path (k, pos_at (k, i, x, delta));
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
  // p: a path in the edit tree; the lines are at reverse (ip) * (i, ...)
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
  if (N(p) >= N(me) + 2 && head (p, N(me)) == me) {
    int i= p[N(me)];
    if (i >= i0 && i < i0 + nr_lines ()) {
      int k= i - i0;
      if (rich (k)) return path (k, rich_box (k)->find_box_path (p, found));
      if (N(p) == N(me) + 2) {
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
  // as text_box_rep::find_cursor, with the positions of code_xpos
  int pos= is_nil (bp->next)? 0: bp->next->item;
  int i= row_of (k, pos);
  int a= starts (k) [i];
  string s= line (k);
  pos= max (a, min (pos, row_end (k, i)));
  array<SI> xp;
  xpositions (k, pos, xp);
  metric ex;
  ex->y1= ex->y2= 0;
  if (pos > a) {
    int q= pos;
    tm_char_backwards (s, q);
    // (the character before the cursor, unless it is a space or a tab)
    string m= code_ink (s, q, pos);
    if (N(m) > 0) fn->get_extents (m, ex);
  }
  cursor cu (xp[pos] - xp[a], base (k, i));
  cu->y1= min (ex->y1, (SI) 0);
  cu->y2= max (ex->y2, fn->yx);
  cu->slope= fn->get_right_slope (s (a, row_end (k, i)));
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
    // as text_box_rep::find_selection, with the positions of code_xpos
    int r= row_of (k1, p1), a= starts (k1) [r], e= row_end (k1, r);
    p1= max (a, min (p1, e)); p2= max (a, min (p2, e));
    string s= line (k1);
    array<SI> xp;
    xpositions (k1, p2, xp);
    // (the height of the characters without the spaces and the tabs: see
    // code_ink)
    string m= code_ink (s, p1, p2);
    SI ey1= 0, ey2= fn->yx;
    if (N(m) > 0) {
      metric ex;
      fn->get_extents (m, ex);
      ey1= ex->y1; ey2= ex->y2;
    }
    rectangles rs (rectangle (xp[p1] - xp[a], ey1 + base (k1, r),
                              xp[p2] - xp[a], ey2 + base (k1, r)));
    return selection (rs, find_tree_path (lbp), find_tree_path (rbp));
  }
  // from the first cursor to the right, the rows between, from the left to
  // the second cursor
  cursor c1= find_cursor (lbp), c2= find_cursor (rbp);
  SI top= fn->y2, bot= fn->y1;
  // (the first and the last row as in a paragraph: up to the end of the
  // text of the first, from the start of the last, as high as their
  // characters, see code_ink; the rows between as one band)
  SI y1a= c1->oy + bot, y1b= c1->oy + top, x1b= x2;
  SI y2a= c2->oy + bot, y2b= c2->oy + top;
  if (!rich (k1) && !rich (k2)) {
    int r1= row_of (k1, p1), e1= row_end (k1, r1), a1= starts (k1) [r1];
    int r2= row_of (k2, p2), a2= starts (k2) [r2];
    string s1= line (k1), s2= line (k2);
    array<SI> xp1;
    xpositions (k1, e1, xp1);
    x1b= max (c1->ox, xp1[e1] - xp1[a1]);
    string m1= code_ink (s1, max (a1, min (p1, e1)), e1);
    string m2= code_ink (s2, a2, max (a2, min (p2, row_end (k2, r2))));
    SI ey1= 0, ey2= fn->yx;
    if (N(m1) > 0) { metric ex; fn->get_extents (m1, ex); ey1= ex->y1; ey2= ex->y2; }
    y1a= c1->oy + ey1; y1b= c1->oy + ey2;
    ey1= 0; ey2= fn->yx;
    if (N(m2) > 0) { metric ex; fn->get_extents (m2, ex); ey1= ex->y1; ey2= ex->y2; }
    y2a= c2->oy + ey1; y2b= c2->oy + ey2;
  }
  rectangles rs (rectangle (x1, y2a, c2->ox, y2b));
  if (c2->oy + top < c1->oy + bot)
    rs= rectangles (rectangle (x1, c2->oy + top, x2, c1->oy + bot), rs);
  rs= rectangles (rectangle (c1->ox, y1a, x1b, y1b), rs);
  return selection (rs, find_tree_path (lbp), find_tree_path (rbp));
}

/******************************************************************************
* Interface
******************************************************************************/

box
code_box (path ip, tree doc, int i0, int n, font fn, pencil pen,
          code_lexer lx, hashmap<string,color> cols, SI pitch, SI rpitch,
          SI cell, int tabw, int invis,
          array<int> tall_k, array<string> tall_txt,
          array<array<int> > tall_st, array<box> tall_box,
          array<int> tall_fold, SI width) {
  return code_new (ip, doc, i0, n, fn, pen, lx, cols,
                               pitch, rpitch, cell, tabw, invis,
                               tall_k, tall_txt, tall_st, tall_box,
                               tall_fold, width);
}

box
code_row_box (path lip, tree line, string text, int a, int b, font fn,
              pencil pen, code_lexer lx, hashmap<string,color> cols,
              SI cell, int tabw, int invis, SI width) {
  return code_new (lip, line, text, a, b, fn, pen, lx, cols,
                               cell, tabw, invis, width);
}

/******************************************************************************
* A box scaled down to the height of a row (code-tall-objects: shrink)
*
* As transformed_box_rep, but the clicks, the cursor and the selections in
* the box are scaled too: the box is edited as it is drawn.
******************************************************************************/

struct code_fit_box_rep: public change_box_rep {
  double s; // the factor,
  SI     dy; // then the shift
  frame  fr;
  code_fit_box_rep (box b, double s, SI dy);
  operator tree () { return tree (TUPLE, "fit", (tree) bs[0]); }
  SI   sc (SI v) { return (SI) (v * s); }
  void pre_display (renderer &ren) { ren->set_transformation (fr); }
  void post_display (renderer &ren) { ren->reset_transformation (); }
  path find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
    return path (0, bs[0]->find_box_path ((SI) (x / s), (SI) ((y - dy) / s),
                                         delta, force, found)); }
  cursor find_cursor (path bp) {
    if (is_atom (bp)) return box_rep::find_cursor (bp);
    cursor cu= bs[0]->find_cursor (bp->next);
    cu->ox= sc (cu->ox); cu->oy= sc (cu->oy) + dy;
    cu->y1= sc (cu->y1); cu->y2= sc (cu->y2);
    return cu; }
  selection find_selection (path lbp, path rbp) {
    if (is_atom (lbp) || is_atom (rbp))
      return box_rep::find_selection (lbp, rbp);
    selection sel= bs[0]->find_selection (lbp->next, rbp->next);
    rectangles rs;
    for (rectangles r= sel->rs; !is_nil (r); r= r->next)
      rs= rectangles (fr (r->item), rs);
    return selection (rs, sel->start, sel->end); }
  void get_bracket_extents (SI& lo, SI& hi) { lo= y1; hi= y2; }
};

code_fit_box_rep::code_fit_box_rep (box b, double s2, SI dy2):
  change_box_rep (b->ip, true), s (s2), dy (dy2),
  fr (scaling (s2, point (0.0, (double) dy2)))
{
  insert (b, 0, 0);
  position ();
  finalize ();
  rectangle r1= fr (rectangle (x1, y1, x2, y2));
  rectangle r2= fr (rectangle (x3, y3, x4, y4));
  x1= r1->x1; y1= r1->y1; x2= r1->x2; y2= r1->y2;
  x3= r2->x1; y3= r2->y1; x4= r2->x2; y4= r2->y2;
}

box
code_fit_box (box b, SI bot, SI top) {
  SI h= b->y2 - b->y1;
  if (h <= top - bot || h <= 0) return b;
  double s= ((double) (top - bot)) / ((double) h);
  // (then within [bot, top])
  SI lo= (SI) (b->y1 * s), hi= (SI) (b->y2 * s), dy= 0;
  if (hi > top) dy= top - hi;
  else if (lo < bot) dy= bot - lo;
  return tm_new<code_fit_box_rep> (b, s, dy);
}
