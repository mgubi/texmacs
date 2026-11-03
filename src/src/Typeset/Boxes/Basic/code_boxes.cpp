
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
* asked to the language when a line is drawn, from the start of the line,
* and kept for the last lines drawn (code_runs: a very long line, as in a
* minified file, is drawn row after row).
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
* The colours of the lines which were drawn last
******************************************************************************/

int code_runs_generation= 0; // made larger when code-lines are typeset

// The colours of a line are not asked beyond this length: the languages
// look at the line from its start for every token (in_comment: quadratic,
// days for a minified file of a line of 1.5 MB), and editors do not colour
// such lines either
#define CODE_COLOR_MAX 20000

static inline bool
same_text (string a, string b) {
  // (the string of a line which did not change is the same string)
  return a.operator-> () == b.operator-> () || a == b;
}

struct code_runs {
  void*        key;   // the line
  string       text;  // its text
  void*        lan;
  int          gen;
  array<int>   start, end;
  array<string> col;  // the colours, as the language names them
  array<SI>    xpos;  // the positions of the characters
};

#define CODE_RUNS_KEPT 16
static code_runs code_runs_kept[CODE_RUNS_KEPT];
static int code_runs_next= 0;

static code_runs&
get_code_runs (tree t, language lan, font fn) {
  void* key= (void*) t.operator-> ();
  for (int i=0; i<CODE_RUNS_KEPT; i++) {
    code_runs& c= code_runs_kept[i];
    if (c.key == key && c.lan == (void*) lan.operator-> () &&
        c.gen == code_runs_generation && same_text (c.text, t->label))
      return c;
  }
  code_runs& c= code_runs_kept[code_runs_next];
  code_runs_next= (code_runs_next + 1) % CODE_RUNS_KEPT;
  c.key= key; c.text= t->label; c.lan= (void*) lan.operator-> ();
  c.gen= code_runs_generation;
  c.start= array<int> (); c.end= array<int> (); c.col= array<string> ();
  string s= t->label;
  int n= N(s);
  c.xpos= array<SI> (n+1);
  if (n > 0) fn->get_xpositions (s, A(c.xpos));
  else c.xpos[0]= 0;
  if (n > CODE_COLOR_MAX) {
    c.start << 0; c.end << n; c.col << string ("");
    return c;
  }
  int pos= 0;
  while (pos < n) {
    int start= pos;
    (void) lan->advance (t, pos);
    if (pos > n) pos= n;
    if (pos <= start) pos= start + 1;
    if (pos - start == 1 && s[start] == ' ') continue;
    c.start << start; c.end << pos; c.col << lan->get_color (t, start, pos);
  }
  return c;
}

static code_runs&
plain_code_runs (string s, font fn) {
  static code_runs c;
  c.key= NULL; c.text= s; c.lan= NULL; c.gen= -1;
  int n= N(s);
  c.xpos= array<SI> (n+1);
  if (n > 0) fn->get_xpositions (s, A(c.xpos));
  else c.xpos[0]= 0;
  c.start= array<int> (); c.end= array<int> (); c.col= array<string> ();
  c.start << 0; c.end << n; c.col << string ("");
  return c;
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
  language lan;
  hashmap<string,color> cols; // the colours given as variables
  SI       pitch;  // the distance between two lines,
  SI       rpitch; // and between two rows of a line
  // the lines which have several rows (by their number in the box, in
  // order), their texts and the starts of their rows when the box was made,
  // and the depth which the rows add before each of them (and in all:
  // tall_cum[N(tall_k)])
  array<int>         tall_k;
  array<string>      tall_txt;
  array<array<int> > tall_st;
  array<long long>   tall_cum;
  bool     single; // one row [ra, rb) of the line doc (and ip is its path),
  int      ra, rb; // whose text was one_text
  string   one_text;

  code_box_rep (path ip, tree doc, int i0, int n,
                font fn, pencil pen, language lan,
                hashmap<string,color> cols, SI pitch, SI rpitch,
                array<int> tall_k, array<string> tall_txt,
                array<array<int> > tall_st, SI width);
  code_box_rep (path lip, tree one, string text, int ra, int rb, font fn,
                pencil pen, language lan, hashmap<string,color> cols,
                SI width);
  operator tree () { return "code lines"; }

  // lines and rows. The tree changes before the next typesetting: the box
  // draws the wrapped lines as they were (their rows), and the others as
  // they are, as long as they are there
  int    nr_lines () { return n; }
  tree   line_tree (int k) {
    if (single) return doc;
    int i= i0 + k;
    return (i < N(doc)? doc[i]: tree ("")); }
  string line (int k) {
    if (single) return one_text;
    int j= tall_lower (k);
    if (j < N(tall_k) && tall_k[j] == k) return tall_txt[j];
    tree t= line_tree (k);
    return is_atomic (t)? t->label: string (""); }
  path   line_ip (int k) {
    return single? ip: descend (descend (ip, 0), i0+k); }
  int    tall_lower (int k) {
    // the number of the lines with several rows before the line k
    int lo= 0, hi= N(tall_k);
    while (lo < hi) {
      int mid= (lo + hi) / 2;
      if (tall_k[mid] < k) lo= mid + 1; else hi= mid;
    }
    return lo; }
  array<int> starts (int k) {
    if (single) { array<int> r; r << ra; return r; }
    int j= tall_lower (k);
    if (j < N(tall_k) && tall_k[j] == k) return tall_st[j];
    return code_one_row (); }
  int    nr_rows (int k) {
    if (single) return 1;
    int j= tall_lower (k);
    return (j < N(tall_k) && tall_k[j] == k)? N(tall_st[j]): 1; }
  int    row_end (int k, int i) {
    if (single) return rb;
    array<int> st= starts (k);
    return i+1 < N(st)? st[i+1]: N (line (k)); }
  int    row_of (int k, int pos) {
    array<int> st= starts (k);
    int i= N(st) - 1;
    while (i > 0 && st[i] > pos) i--;
    return i; }
  // the depth of the baseline of the row i of the line k below the first
  long long depth (int k, int i) {
    long long extra= (N(tall_cum) == 0? 0: tall_cum[tall_lower (k)]);
    return (long long) k * pitch + extra + (long long) i * rpitch; }
  SI     base (int k, int i) { return (SI) -depth (k, i); }
  void   row_at (SI y, int& k, int& i);
  box    row_box (int k, int i) {
    int a= starts (k) [i], b= row_end (k, i);
    return text_box (line_ip (k), a, line (k) (a, b), fn, pen); }

  // The lines are not children for what goes through all the children
  // (subnr is 0: drawing, positioning), but a box path (k, pos) leads to a
  // box of the line k, made when it is asked for, at its place
  box    subbox (int k);

  void      display (renderer ren);
  path      find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path      find_lip ();
  path      find_rip ();
  path      find_left_box_path () { return path (0, single? ra: 0); }
  path      find_right_box_path () {
    return path (nr_lines () - 1, single? rb: N (line (nr_lines () - 1))); }
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
};

code_box_rep::code_box_rep (path ip2, tree doc2, int i02, int n2, font fn2,
                            pencil pen2, language lan2,
                            hashmap<string,color> cols2, SI pitch2,
                            SI rpitch2, array<int> tall_k2,
                            array<string> tall_txt2,
                            array<array<int> > tall_st2, SI width):
  box_rep (ip2), doc (doc2), i0 (i02), n (max (n2, 1)),
  fn (fn2), pen (pen2), lan (lan2), cols (cols2), pitch (pitch2),
  rpitch (rpitch2), tall_k (tall_k2), tall_txt (tall_txt2),
  tall_st (tall_st2), single (false), ra (0), rb (0)
{
  if (N(tall_k) > 0) {
    tall_cum= array<long long> (N(tall_k) + 1);
    tall_cum[0]= 0;
    for (int j=0; j<N(tall_k); j++)
      tall_cum[j+1]= tall_cum[j] + (long long) (N(tall_st[j]) - 1) * rpitch;
  }
  x1= x3= 0;
  x2= x4= width;
  y1= y3= base (n-1, nr_rows (n-1) - 1) + fn->y1;
  y2= y4= fn->y2;
}

code_box_rep::code_box_rep (path lip, tree one, string text, int ra2,
                            int rb2, font fn2, pencil pen2, language lan2,
                            hashmap<string,color> cols2, SI width):
  box_rep (lip), doc (one), i0 (0), n (1),
  fn (fn2), pen (pen2), lan (lan2), cols (cols2), pitch (0), rpitch (0),
  single (true), ra (ra2), rb (rb2), one_text (text)
{
  x1= x3= 0;
  x2= x4= width;
  y1= y3= fn->y1;
  y2= y4= fn->y2;
}

void
code_box_rep::row_at (SI y, int& k, int& i) {
  // the row whose middle is the nearest to y
  int n= nr_lines ();
  if (single) { k= 0; i= 0; return; }
  long long q= (long long) (fn->y1 + fn->y2) / 2 - (long long) y;
  // the last line which starts above q (a binary search on the depths)
  int lo= 0, hi= n-1;
  while (lo < hi) {
    int mid= (lo + hi + 1) / 2;
    if (depth (mid, 0) <= q) lo= mid; else hi= mid - 1;
  }
  k= lo;
  int r= nr_rows (k);
  long long d= q - depth (k, 0);
  long long ri= (d + rpitch/2) / (rpitch > 0? rpitch: 1);
  if (ri < 0) ri= 0;
  if (ri > r-1) ri= r-1;
  i= (int) ri;
  if (k+1 < n) {
    long long d1= q - depth (k, i), d2= depth (k+1, 0) - q;
    if (d1 > 0 && d2 < d1) { k= k+1; i= 0; }
  }
}

box
code_box_rep::subbox (int k) {
  k= max (0, min (k, nr_lines () - 1));
  if (single) {
    box b= row_box (0, 0);
    b->x0= 0; b->y0= 0;
    return b;
  }
  array<int> tk;
  array<string> tt;
  array<array<int> > ts;
  if (nr_rows (k) > 1) { tk << 0; tt << line (k); ts << starts (k); }
  box b= tm_new<code_box_rep> (ip, doc, i0+k, 1, fn, pen, lan, cols,
                               pitch, rpitch, tk, tt, ts, x2);
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
    tree t= line_tree (k);
    string s= line (k);
    if (!is_atomic (t) || N(s) == 0) continue;
    // (the line changed since the box was made: drawn as it was, without
    // colours, until it is typeset again)
    code_runs& c= (same_text (t->label, s)? get_code_runs (t, lan, fn):
                                            plain_code_runs (s, fn));
    array<int> st= starts (k);
    int lo= (k == ka? ia: 0), hi= (k == kb? ib: N(st) - 1);
    for (int i= lo; i <= hi && i < N(st); i++) {
      int a= st[i], b= row_end (k, i);
      SI y= base (k, i);
      for (int r=0; r<N(c.start); r++) {
        int s1= max (c.start[r], a), s2= min (c.end[r], b);
        if (s1 >= s2) continue;
        string col= c.col[r];
        color cl= (col == ""? fg:
                   cols->contains (col)? cols[col]: named_color (col));
        ren->set_pencil (pencil (cl));
        // (a space is not drawn: the font has a visible space there)
        int u= s1;
        while (u < s2) {
          while (u < s2 && s[u] == ' ') u++;
          int v= u;
          while (v < s2 && s[v] != ' ') v++;
          if (v > u) fn->draw (ren, s (u, v), c.xpos[u] - c.xpos[a], y);
          u= v;
        }
      }
    }
  }
}

/******************************************************************************
* Cursor and selection, by the row and then by a text box of the row
******************************************************************************/

path
code_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  int k, i;
  row_at (y, k, i);
  int a= starts (k) [i];
  box rb= row_box (k, i);
  path bp= rb->find_box_path (x, y - base (k, i), delta, force, found);
  found= true;
  return path (k, a + (is_nil (bp)? 0: bp->item));
}

path
code_box_rep::find_lip () {
  return descend (line_ip (0), single? ra: 0);
}

path
code_box_rep::find_rip () {
  int k= nr_lines () - 1;
  return descend (line_ip (k), single? rb: N (line (k)));
}

path
code_box_rep::find_box_path (path p, bool& found) {
  // p: a path in the edit tree; the lines are at reverse (ip) * (0, i, pos)
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
  if (N(p) == N(me) + 3 && path_up (p, 3) == me && p[N(me)] == 0) {
    int i= p[N(me)+1], pos= last_item (p);
    if (i >= i0 && i < i0 + nr_lines ()) {
      found= true;
      return path (i - i0, max (0, min (pos, N (line (i - i0)))));
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
  int pos= is_nil (bp->next)? 0: bp->next->item;
  int lo= (single? ra: 0), hi= (single? rb: N (line (k)));
  return reverse (descend (line_ip (k), max (lo, min (pos, hi))));
}

cursor
code_box_rep::find_cursor (path bp) {
  if (is_atom (bp))
    bp= (bp->item == 0? find_left_box_path (): find_right_box_path ());
  int k= max (0, min (bp->item, nr_lines () - 1));
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
  int p1= is_nil (lbp->next)? 0: lbp->next->item;
  int p2= is_nil (rbp->next)? 0: rbp->next->item;
  if (k1 > k2 || (k1 == k2 && p1 > p2)) return find_selection (rbp, lbp);
  int r1= row_of (k1, p1), r2= row_of (k2, p2);
  if (k1 == k2 && r1 == r2) {
    int a= starts (k1) [r1];
    selection sel= row_box (k1, r1)->find_selection (path (p1 - a),
                                                      path (p2 - a));
    return selection (translate (sel->rs, 0, base (k1, r1)),
                      find_tree_path (lbp), find_tree_path (rbp));
  }
  cursor c1= find_cursor (lbp), c2= find_cursor (rbp);
  SI top = fn->y2, bot= fn->y1;
  // the rest of the first row, the rows between, the start of the last
  int k1n= k1, r1n= r1 + 1;
  if (r1n >= nr_rows (k1)) { k1n= k1 + 1; r1n= 0; }
  int k2p= k2, r2p= r2 - 1;
  if (r2p < 0) { k2p= k2 - 1; r2p= nr_rows (k2p) - 1; }
  rectangles rs (rectangle (x1, base (k2, r2) + bot, c2->ox,
                            base (k2, r2) + top));
  if (k1n < k2 || (k1n == k2 && r1n < r2))
    rs= rectangles (rectangle (x1, base (k2p, r2p) + bot, x2,
                               base (k1n, r1n) + top), rs);
  rs= rectangles (rectangle (c1->ox, base (k1, r1) + bot, x2,
                             base (k1, r1) + top), rs);
  return selection (rs, find_tree_path (lbp), find_tree_path (rbp));
}

/******************************************************************************
* Interface
******************************************************************************/

box
code_box (path ip, tree doc, int i0, int n, font fn, pencil pen,
          language lan, hashmap<string,color> cols, SI pitch, SI rpitch,
          array<int> tall_k, array<string> tall_txt,
          array<array<int> > tall_st, SI width) {
  return tm_new<code_box_rep> (ip, doc, i0, n, fn, pen, lan, cols,
                               pitch, rpitch, tall_k, tall_txt, tall_st,
                               width);
}

box
code_row_box (path lip, tree line, string text, int a, int b, font fn,
              pencil pen, language lan, hashmap<string,color> cols,
              SI width) {
  return tm_new<code_box_rep> (lip, line, text, a, b, fn, pen, lan, cols,
                               width);
}
