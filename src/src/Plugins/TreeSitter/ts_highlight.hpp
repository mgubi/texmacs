/******************************************************************************
* MODULE     : ts_highlight.hpp
* DESCRIPTION: syntax highlighting of programs by tree-sitter
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TS_HIGHLIGHT_H
#define TS_HIGHLIGHT_H
#include "language.hpp"
#include "scheme.hpp"

bool tree_sitter_wraps (string lan);
language_rep* tree_sitter_language (string lan, language_rep* base);
void tree_sitter_set_highlighter (string lan, int query, array<string> classes);
array<tree> tree_sitter_pending_updates ();
void tree_sitter_set_highlighting (bool on);
object tree_sitter_line_colors (string lan, tree t);

#endif // TS_HIGHLIGHT_H
