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

/******************************************************************************
* Folds
******************************************************************************/

// (a highlight of a code of its own, which no language uses)
#define CODE_FOLD_HIDDEN 999998

void remove_highlight (observer& obs, int lan);

int
code_line_fold_state (tree t) {
  if (!is_atomic (t)) return 0;
  return has_highlight (t, CODE_FOLD_HIDDEN)? 1: 0;
}

void
code_set_line_fold_state (tree t, int state) {
  if (!is_atomic (t)) return;
  if (!is_nil (t->obs)) remove_highlight (t->obs, CODE_FOLD_HIDDEN);
  if (state == 1) attach_highlight (t, CODE_FOLD_HIDDEN, 0, 0, 0);
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
