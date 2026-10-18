/******************************************************************************
* MODULE     : code_lines.cpp
* DESCRIPTION: the state of lines of program code which is not in the document
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "code_lines.hpp"
#include "observer.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "hashmap.hpp"

/******************************************************************************
* Folds
******************************************************************************/

// The state of a line is the number of the folds which hide it (folds nest),
// kept as a highlight of a code of its own for each number (which no
// language uses)
#define CODE_FOLD_CODES 999990   // CODE_FOLD_CODES + 1 ... + CODE_FOLD_MAX
#define CODE_FOLD_MAX 9

void remove_highlight (observer& obs, int lan);

int
code_line_fold_state (tree t) {
  if (!is_atomic (t) || is_nil (t->obs)) return 0;
  for (int d= 1; d <= CODE_FOLD_MAX; d++)
    if (has_highlight (t, CODE_FOLD_CODES + d)) return d;
  return 0;
}

void
code_set_line_fold_state (tree t, int state) {
  if (!is_atomic (t)) return;
  if (!is_nil (t->obs))
    for (int d= 1; d <= CODE_FOLD_MAX; d++)
      remove_highlight (t->obs, CODE_FOLD_CODES + d);
  if (state > 0)
    attach_highlight (t, CODE_FOLD_CODES + min (state, CODE_FOLD_MAX), 0, 0, 0);
}

/******************************************************************************
* Layers
******************************************************************************/

// (the colours of a line: for each character the number of its colour plus
// one, 0 for none; its hints: for each position, the end of the line too,
// the number of its text plus one. The colours and the texts are numbered
// once: they are few, and repeated)
#define CODE_LAYER_COLORS 999981
#define CODE_LAYER_HINTS  999980

static array<string> layer_names;
static hashmap<string,int> layer_numbers (-1);

static int
layer_number (string s) {
  int k= layer_numbers[s];
  if (k < 0) {
    k= N(layer_names);
    layer_names << s;
    layer_numbers (s)= k;
  }
  return k;
}

void
code_clear_line_layers (tree t) {
  if (!is_atomic (t) || is_nil (t->obs)) return;
  remove_highlight (t->obs, CODE_LAYER_COLORS);
  remove_highlight (t->obs, CODE_LAYER_HINTS);
}

void
code_set_line_layers (tree t, tree colors, tree hints) {
  code_clear_line_layers (t);
  if (!is_atomic (t)) return;
  int n= N(t->label);
  array<int> cols (n), hs (n + 1);
  bool some_col= false, some_hint= false;
  for (int i=0; i<n; i++) cols[i]= 0;
  for (int i=0; i<=n; i++) hs[i]= 0;
  for (int i=0; i<N(colors); i++)
    if (is_tuple (colors[i]) && N(colors[i]) == 3 &&
        is_int (colors[i][0]) && is_int (colors[i][1]) &&
        is_atomic (colors[i][2])) {
      int a= max (0, as_int (colors[i][0])), b= min (n, as_int (colors[i][1]));
      int k= layer_number (colors[i][2]->label) + 1;
      for (int j=a; j<b; j++) { cols[j]= k; some_col= true; }
    }
  for (int i=0; i<N(hints); i++)
    if (is_tuple (hints[i]) && N(hints[i]) == 2 &&
        is_int (hints[i][0]) && is_atomic (hints[i][1])) {
      int p= as_int (hints[i][0]);
      if (p >= 0 && p <= n) {
        hs[p]= layer_number (hints[i][1]->label) + 1;
        some_hint= true;
      }
    }
  if (some_col) attach_observer (t, highlight_observer (CODE_LAYER_COLORS, cols));
  if (some_hint) attach_observer (t, highlight_observer (CODE_LAYER_HINTS, hs));
}

bool
code_line_layers_of (tree t, code_line_layers& l) {
  if (!is_atomic (t) || is_nil (t->obs)) return false;
  array<int> cols= obtain_highlight (t, CODE_LAYER_COLORS);
  array<int> hs  = obtain_highlight (t, CODE_LAYER_HINTS);
  if (N(cols) == 0 && N(hs) == 0) return false;
  int i= 0;
  while (i < N(cols)) {
    if (cols[i] == 0) { i++; continue; }
    int j= i;
    while (j < N(cols) && cols[j] == cols[i]) j++;
    l.col_start << i;
    l.col_end   << j;
    l.col_color << layer_names[cols[i] - 1];
    i= j;
  }
  for (int p=0; p<N(hs); p++)
    if (hs[p] != 0) {
      l.hint_pos  << p;
      l.hint_text << layer_names[hs[p] - 1];
    }
  return true;
}

tree
code_line_layers_tree (tree t) {
  code_line_layers l;
  tree cols (TUPLE), hs (TUPLE);
  if (code_line_layers_of (t, l)) {
    for (int i=0; i<N(l.col_start); i++)
      cols << tree (TUPLE, as_string (l.col_start[i]), as_string (l.col_end[i]),
                    l.col_color[i]);
    for (int i=0; i<N(l.hint_pos); i++)
      hs << tree (TUPLE, as_string (l.hint_pos[i]), l.hint_text[i]);
  }
  return tree (TUPLE, cols, hs);
}

/******************************************************************************
* Positions
******************************************************************************/

int
code_column (string s, int offset, int tw) {
  if (tw <= 0) tw= 8;
  int i= 0, col= 0, n= min (offset, N(s));
  while (i < n) {
    if (is_source_tab (s, i)) { col += tw - col % tw; i += 4; }
    else { tm_char_forwards (s, i); col++; }
  }
  return col;
}

static int
utf16_units (string u) {
  // the UTF-16 units of the utf8 string @u
  int r= 0;
  for (int i=0; i<N(u); i++) {
    unsigned char c= (unsigned char) u[i];
    if (c < 0x80 || (c >= 0xC0 && c < 0xF0)) r++;
    else if (c >= 0xF0) r += 2;
  }
  return r;
}

static bool
plain_line (string s) {
  // one byte for each character, of one UTF-16 unit
  for (int i=0; i<N(s); i++)
    if (s[i] == '<' || ((unsigned char) s[i]) >= 128) return false;
  return true;
}

int
code_utf16_column (string s, int offset) {
  int n= max (0, min (offset, N(s)));
  if (plain_line (s)) return n;
  int i= 0, r= 0;
  while (i < n) {
    int j= i;
    tm_char_forwards (s, j);
    r += utf16_units (cork_to_sourcecode (s (i, j)));
    i= j;
  }
  return r;
}

int
code_utf16_offset (string s, int units) {
  // (a position inside a character is at its start, one beyond the end of
  // the line at its end)
  if (plain_line (s)) return max (0, min (units, N(s)));
  int i= 0, r= 0;
  while (i < N(s) && r < units) {
    int j= i;
    tm_char_forwards (s, j);
    int m= r + utf16_units (cork_to_sourcecode (s (i, j)));
    if (m > units) break;
    r= m;
    i= j;
  }
  return i;
}

int
code_indent_end (string s) {
  int i= 0;
  while (i < N(s)) {
    if (s[i] == ' ' || s[i] == '\t') i++;
    else if (is_source_tab (s, i)) i += 4;
    else break;
  }
  return i;
}
