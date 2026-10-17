/******************************************************************************
* MODULE     : syntax_region.hpp
* DESCRIPTION: the tree-sitter syntax trees of the code regions of documents
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SYNTAX_REGION_H
#define SYNTAX_REGION_H
#include "tree.hpp"
#include "scheme.hpp"

/* A region is a document whose children are the lines of a program, or a
   single string. Its syntax tree is kept between requests and brought up to
   date by comparing the lines with those of the previous request (one edit
   of tree-sitter, then an incremental parse). Rows are lines, columns are
   positions in the TeXmacs strings. */

string syntax_region_sexp (tree region, int language);
object syntax_region_node_at (tree region, int language, int row, int column);
object syntax_region_errors (tree region, int language);
array<int> syntax_region_statistics (tree region, int language);
void syntax_region_forget (tree region);
array<array<int> > syntax_region_highlight (tree region, int language,
                                            int query, array<int> colors);

#endif // SYNTAX_REGION_H
