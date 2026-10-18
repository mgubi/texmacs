/******************************************************************************
* MODULE     : lsp_decorations.hpp
* DESCRIPTION: Colours and hints of language servers in the lines of code
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LSP_DECORATIONS_H
#define LSP_DECORATIONS_H
#include "tree.hpp"

// The decorations of a line of a source file (a string of its document):
// colours of pieces of it (semantic tokens of a language server) and hints
// shown before some of its characters (inlay hints), which are not part of
// the text. They are kept for the tree of the line and its text then: a
// line which has changed since is typeset without them.
struct lsp_line_decorations {
  string        label;       // the text of the line when they were given
  array<int>    col_start;   // the pieces which have a colour of their own
  array<int>    col_end;
  array<string> col_color;
  array<int>    hint_pos;    // the hints, before the character hint_pos
  array<string> hint_text;
};

// colours: (tuple (tuple start end color) ...), hints: (tuple (tuple pos
// text) ...), both in the order of the line
void lsp_decorate_line (tree line, tree colors, tree hints);
void lsp_undecorate_line (tree line);
// a folded line: kept in the document, but not typeset
void lsp_fold_line (tree line, bool folded);
bool lsp_line_folded (tree line);
// the marker at the end of the first line of a fold ("" for none), and the
// marker of @line (or "": none, or not for its present text)
void   lsp_fold_marker (tree line, string text);
string lsp_fold_marker_of (tree line);
// the decorations of @line, or NULL (none, or not for its present text)
lsp_line_decorations* lsp_line_decorations_of (tree line);

#endif // LSP_DECORATIONS_H
