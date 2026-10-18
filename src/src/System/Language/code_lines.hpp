/******************************************************************************
* MODULE     : code_lines.hpp
* DESCRIPTION: the state of lines of program code which is not in the document
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CODE_LINES_H
#define CODE_LINES_H
#include "tree.hpp"

// The state of a line of program code which is a state of the view, not of
// the document (see doc/code-features.md): it is kept on the highlight
// observer of the line, which a modification of the line detaches, so that
// an edited line forgets it. The features of programs (tree-sitter, the
// language servers) set it; the typesetter and the editor read it.

// a fold: a hidden line (1) takes no room and is not drawn (its boxes stay,
// for the cursor, see bridge_rep::fold_line); the line before hidden lines
// ends with a mark. The lines are then to be typeset again (update-path)
int  code_line_fold_state (tree line);
void code_set_line_fold_state (tree line, int state);

// positions in a line of program code (a Cork string, see doc/code-features.md):
// offsets are those of the string; a character of Cork (a byte, or <...>)
// is one column, a tab (<#9>, see source_document_to_tree) goes to the next
// stop of every tw columns; UTF-16 units are those of the text of the file
// (as language servers count them, cork_to_sourcecode)
int code_column (string line, int offset, int tw);
int code_utf16_column (string line, int offset);
int code_utf16_offset (string line, int units);
// the offset after the blanks at the start of @line (spaces and tabs)
int code_indent_end (string line);

#endif // CODE_LINES_H
