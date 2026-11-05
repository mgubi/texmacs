
/******************************************************************************
* MODULE     : code_boxes.hpp
* DESCRIPTION: the lines of source code (code-lines): boxes, soft
*              line breaks and colours of the syntax
* COPYRIGHT  : (C) 2026  the TeXmacs contributors
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CODE_BOXES_H
#define CODE_BOXES_H
#include "boxes.hpp"
#include "language.hpp"
#include "colors.hpp"
#include <vector>

/******************************************************************************
* The colours of the syntax of the lines of a block (code_boxes.cpp)
*
* The lexer of a block has the state of the language at the start of each
* line (language_rep::colour_line: in a comment of several lines...), made
* when it is asked for, from the first line whose state is not known. An
* edit changes the states from the line which changed, up to the first line
* whose state is the same as before (they are made again when asked). The
* colours of the lines which were drawn last are kept (runs), within a
* budget. The bridge of the block tells the lexer where lines are inserted,
* removed or changed; its document is the one of the editor.
******************************************************************************/

// the same text (the string of a line which did not change is the same
// string: then it is not compared)
inline bool
same_text (string a, string b) {
  return a.operator-> () == b.operator-> () || a == b;
}

// The colours of a line are not asked beyond this length (a minified file:
// a line is tokenized at each change of its text), as in other editors; the
// state after such a line is the one before it
#define CODE_COLOR_MAX 100000

struct code_runs_entry {
  int           line;   // the line,
  string        text;   // its text,
  int           state;  // and its state at its start
  array<int>    starts, ends;
  array<string> cols;   // the colours, as the language names them
};

class code_lexer;
class code_lexer_rep: public concrete_struct {
public:
  language   lan;
  bool       stateful; // the language has line states
  tree       doc;      // the document of the lines
  array<int> states;   // the states at the start of the lines i < valid
  int        valid;
  int        fix_from, fix_to; // the lines changed since (-1: none)
  std::vector<code_runs_entry> kept; // the colours of the last lines drawn
  int        kept_next, kept_size;

  code_lexer_rep (language lan);
  void reset ();
  void inserted (int pos, int nr);
  void removed (int pos, int nr);
  void changed (int i);
  int  state_at (int i);
  // the colours of the line i, whose text is text (the one of the box: when
  // it is not the one of the document any more, the line has no colours)
  code_runs_entry& runs (int i, string text);
private:
  int  line_out (int j);
};

class code_lexer {
  CONCRETE_NULL(code_lexer);
  code_lexer (language lan);
};
CONCRETE_NULL_CODE(code_lexer);

/******************************************************************************
* Boxes and soft line breaks (code_boxes.cpp)
******************************************************************************/

// The control character at i of s, <#..> (9: a tab), or -1 (j: after it)
int code_control (string s, int i, int& j);
// the positions of the characters of s up to upto, from the start of the
// line: as the font gives them, but a tab goes to the next stop (every tabw
// cells of width cell) and another control character is a cell wide
// (hp, hw: the inlay hints of a language server, before the characters hp,
// hw wide: the characters from hp on are further; see code_line_hints)
void code_xpos (string s, int upto, font fn, SI cell, int tabw,
                array<SI>& xpos, array<int> hp= array<int> (),
                array<SI> hw= array<SI> ());
// the starts of the rows of the text s, at most width wide
array<int> code_breaks (string s, font fn, SI width, SI cell, int tabw,
                        array<int> hp= array<int> (),
                        array<SI> hw= array<SI> ());
// the inlay hints of the line t (its layers, code_lines.hpp): their
// positions, their widths (a cell for each character, as the other
// characters of the grid) and their texts; false when it has none
bool code_line_hints (tree t, SI cell, array<int>& hp, array<SI>& hw,
                      array<string>& ht);

// the lines i0, ..., i0+n-1 of doc (the special lines tall_k of the box
// have the texts tall_txt, broken into rows at tall_st, or, for the lines
// with markup, the boxes tall_box; tall_fold: 1 the line is hidden by a
// fold, 2 hidden lines follow it, see System/Language/code_lines.hpp)
// (cell: the width of a character; tabw: the tabs stop every tabw cells;
// invis: the spaces, the tabs, the ends of the lines... are shown; 2: not
// the end of the line, which is the last one)
box code_box (path ip, tree doc, int i0, int n, font fn, pencil pen,
              code_lexer lx, hashmap<string,color> cols, SI pitch,
              SI rpitch, SI cell, int tabw, int invis,
              array<int> tall_k, array<string> tall_txt,
              array<array<int> > tall_st, array<box> tall_box,
              array<int> tall_fold, SI width);

// the row [a, b) of the line with the text text, whose path is lip
box code_row_box (path lip, tree line, string text, int a, int b, font fn,
                  pencil pen, code_lexer lx, hashmap<string,color> cols,
                  SI cell, int tabw, int invis, SI width);

// b, scaled down (and shifted) to be within [bot, top] when it is taller:
// the clicks, the cursor and the selections in it are scaled too
box code_fit_box (box b, SI bot, SI top);

#endif // defined CODE_BOXES_H
