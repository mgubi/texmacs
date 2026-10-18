/******************************************************************************
* MODULE     : lsp_decorations.cpp
* DESCRIPTION: Colours and hints of language servers in the lines of code
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// The decorations are kept for the representation of the tree of a line
// (its address, which is only compared). The Scheme programs keep the trees
// of the lines which they decorate, until they undecorate them
// (lsp-decorate.scm): a line which is removed is not freed
// meanwhile, and its address cannot be taken by a line made later; the
// text of the line is checked too (a line which is edited keeps its tree).
// The lines which have none cost a lookup in an empty table: the
// typesetting of the other documents does not change.

#include "lsp_decorations.hpp"
#include "hashmap.hpp"

static hashmap<pointer,pointer> lsp_decorated (NULL);

static lsp_line_decorations*
find_decorations (tree line) {
  pointer key= (pointer) inside (line);
  if (!lsp_decorated->contains (key)) return NULL;
  return (lsp_line_decorations*) lsp_decorated [key];
}

void
lsp_undecorate_line (tree line) {
  pointer key= (pointer) inside (line);
  lsp_line_decorations* d= find_decorations (line);
  if (d == NULL) return;
  lsp_decorated->reset (key);
  tm_delete<lsp_line_decorations> (d);
}

void
lsp_decorate_line (tree line, tree colors, tree hints) {
  lsp_undecorate_line (line);
  if (!is_atomic (line)) return;
  if (N(colors) == 0 && N(hints) == 0) return;
  lsp_line_decorations* d= tm_new<lsp_line_decorations> ();
  d->label= line->label;
  for (int i=0; i<N(colors); i++)
    if (is_tuple (colors[i]) && N(colors[i]) == 3 &&
        is_int (colors[i][0]) && is_int (colors[i][1]) &&
        is_atomic (colors[i][2])) {
      d->col_start << as_int (colors[i][0]);
      d->col_end   << as_int (colors[i][1]);
      d->col_color << colors[i][2]->label;
    }
  for (int i=0; i<N(hints); i++)
    if (is_tuple (hints[i]) && N(hints[i]) == 2 &&
        is_int (hints[i][0]) && is_atomic (hints[i][1])) {
      d->hint_pos  << as_int (hints[i][0]);
      d->hint_text << hints[i][1]->label;
    }
  lsp_decorated ((pointer) inside (line))= (pointer) d;
}

lsp_line_decorations*
lsp_line_decorations_of (tree line) {
  if (N(lsp_decorated) == 0 || !is_atomic (line)) return NULL;
  lsp_line_decorations* d= find_decorations (line);
  if (d == NULL || d->label != line->label) return NULL;
  return d;
}
