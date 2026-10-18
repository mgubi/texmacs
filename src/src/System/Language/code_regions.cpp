/******************************************************************************
* MODULE     : code_regions.cpp
* DESCRIPTION: the changes of the lines of program code
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "code_regions.hpp"
#include "code_lines.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "convert.hpp"
#include "hashmap.hpp"

static inline bool
same_line (string& x, string& y) {
  // (the strings of the lines which did not change are those of the
  // document: compared by their address first)
  return x.operator-> () == y.operator-> () || x == y;
}

bool
code_lines_common (array<string> old, array<string> cur, int& a, int& b) {
  int n0= N(old), n1= N(cur);
  a= 0;
  while (a < n0 && a < n1 && same_line (old[a], cur[a])) a++;
  b= 0;
  if (a == n0 && a == n1) return false;
  while (b < n0 - a && b < n1 - a && same_line (old[n0-1-b], cur[n1-1-b])) b++;
  return true;
}

/******************************************************************************
* Snapshots
******************************************************************************/

// The lines are kept as they are in the document (the strings of Cork are
// shared: a snapshot costs little, and the lines compare quickly), a line
// with markup as its text; they are converted to the text of the file when
// they are given (the changed ones only).

struct code_snapshot {
  array<string> lines;   // a string of Cork, or the text of a line of markup
  array<bool>   markup;
};

static hashmap<int,pointer> snapshots (NULL);
static int snapshot_counter= 0;

static string
markup_line_text (tree t) {
  // (the text of the line as a file has it, on one line)
  return replace (tree_to_verbatim (t, false, "SourceCode"), "\n", " ");
}

static void
snapshot_lines (tree root, array<string>& lines, array<bool>& markup) {
  lines= array<string> ();
  markup= array<bool> ();
  if (is_atomic (root)) {
    array<string> a= tokenize (root->label, "\n");
    for (int i=0; i<N(a); i++) { lines << a[i]; markup << false; }
  }
  else if (is_document (root))
    for (int i=0; i<N(root); i++) {
      if (is_atomic (root[i])) { lines << root[i]->label; markup << false; }
      else { lines << markup_line_text (root[i]); markup << true; }
    }
  else { lines << markup_line_text (root); markup << true; }
}

static string
line_text (code_snapshot* s, int i) {
  return s->markup[i]? s->lines[i]: cork_to_sourcecode (s->lines[i]);
}

static int
utf16_length (string u) {
  int r= 0;
  for (int i=0; i<N(u); i++) {
    unsigned char c= (unsigned char) u[i];
    if (c < 0x80 || (c >= 0xC0 && c < 0xF0)) r++;
    else if (c >= 0xF0) r += 2;
  }
  return r;
}

int
code_snapshot_new (tree root) {
  code_snapshot* s= tm_new<code_snapshot> ();
  snapshot_lines (root, s->lines, s->markup);
  int id= ++snapshot_counter;
  snapshots (id)= (pointer) s;
  return id;
}

void
code_snapshot_delete (int id) {
  if (!snapshots->contains (id)) return;
  tm_delete<code_snapshot> ((code_snapshot*) snapshots[id]);
  snapshots->reset (id);
}

bool
code_snapshot_changed (int id, tree root) {
  if (!snapshots->contains (id)) return true;
  code_snapshot* s= (code_snapshot*) snapshots[id];
  array<string> lines; array<bool> markup;
  snapshot_lines (root, lines, markup);
  int a, b;
  return code_lines_common (s->lines, lines, a, b);
}

string
code_snapshot_text (int id) {
  if (!snapshots->contains (id)) return "";
  code_snapshot* s= (code_snapshot*) snapshots[id];
  string r;
  for (int i=0; i<N(s->lines); i++) {
    if (i > 0) r << "\n";
    r << line_text (s, i);
  }
  return r;
}

static tree
position_change (int sl, int sc, int el, int ec, string text) {
  return tree (TUPLE, as_string (sl), as_string (sc), as_string (el),
               as_string (ec), text);
}

tree
code_snapshot_change (int id, tree root) {
  if (!snapshots->contains (id)) return tree (TUPLE);
  code_snapshot* s= (code_snapshot*) snapshots[id];
  code_snapshot cur;
  snapshot_lines (root, cur.lines, cur.markup);
  int a, b;
  if (!code_lines_common (s->lines, cur.lines, a, b)) return tree (TUPLE);
  int n0= N(s->lines), n1= N(cur.lines);
  tree r;
  if (n0 - b - a == 1 && n1 - b - a == 1 && !s->markup[a] && !cur.markup[a]) {
    // one line changed (as by typing): the characters between its common
    // start and its common end
    string o= s->lines[a], c= cur.lines[a];
    int p= 0, q= 0;
    while (p < N(o) && p < N(c)) {
      int po= p, pc= p;
      tm_char_forwards (o, po);
      tm_char_forwards (c, pc);
      if (po != pc || o (p, po) != c (p, pc)) break;
      p= po;
    }
    while (q < N(o) - p && q < N(c) - p) {
      int qo= N(o) - q, qc= N(c) - q;
      tm_char_backwards (o, qo);
      tm_char_backwards (c, qc);
      if (N(o) - qo != N(c) - qc || qo < p || qc < p ||
          o (qo, N(o) - q) != c (qc, N(c) - q)) break;
      q= N(o) - qo;
    }
    r= position_change (a, code_utf16_column (o, p),
                        a, code_utf16_column (o, N(o) - q),
                        cork_to_sourcecode (c (p, N(c) - q)));
  }
  else if (b > 0) {
    // the lines a.. before the common last lines: from the start of the
    // line a to the start of the first common last line
    string text;
    for (int i=a; i<n1-b; i++) text << line_text (&cur, i) << "\n";
    r= position_change (a, 0, n0 - b, 0, text);
  }
  else {
    // up to the end of the text, which ends by no line end
    string text;
    for (int i=a; i<n1; i++) {
      if (i > 0) text << "\n";
      text << line_text (&cur, i);
    }
    string last= (n0 > 0? line_text (s, n0 - 1): string (""));
    if (n0 == 0) r= position_change (0, 0, 0, 0, text);
    else if (a > 0) {
      string prev= line_text (s, a - 1);
      r= position_change (a - 1, utf16_length (prev),
                          n0 - 1, utf16_length (last), text);
    }
    else r= position_change (0, 0, n0 - 1, utf16_length (last), text);
  }
  s->lines = cur.lines;
  s->markup= cur.markup;
  return r;
}
