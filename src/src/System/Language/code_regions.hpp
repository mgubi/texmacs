/******************************************************************************
* MODULE     : code_regions.hpp
* DESCRIPTION: the changes of the lines of program code
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CODE_REGIONS_H
#define CODE_REGIONS_H
#include "tree.hpp"

// A region of program code is a document of lines (a source file, a code
// block, a session input). The features of program code follow its changes
// (see doc/code-features.md): each keeps a snapshot of the lines, at its
// own pace, and asks for the change since (tree-sitter at each highlight,
// for ts_tree_edit; a language server after a pause, for didChange).

// the lines a..n0-b of @old became the lines a..n1-b of @cur: the number a
// of their common first lines and b of their common last ones (a + b is at
// most the smallest length); false if they are the same
bool code_lines_common (array<string> old, array<string> cur, int& a, int& b);

// snapshots of the text of a region as a file has it (UTF-8, the encoding
// SourceCode, a line with markup as its verbatim text on one line)
int    code_snapshot_new (tree root);
void   code_snapshot_delete (int id);
// the change since the snapshot, which is then the present text: (tuple)
// for none, else (tuple sl sc el ec text): the text from the line sl and
// the UTF-16 unit sc to the line el and the unit el became text (UTF-8)
tree   code_snapshot_change (int id, tree root);
bool   code_snapshot_changed (int id, tree root);
string code_snapshot_text (int id);

#endif // CODE_REGIONS_H
