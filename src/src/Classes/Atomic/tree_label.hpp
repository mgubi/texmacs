
/******************************************************************************
* MODULE     : tree_label.hpp
* DESCRIPTION: labels of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TREE_LABEL_H
#define TREE_LABEL_H
#include "string.hpp"

/******************************************************************************
* Standard tree labels
******************************************************************************/

enum tree_label {
  STRING, UNKNOWN, UNINIT, ERROR, RAW_DATA,

  // basic formatting tags
  DOCUMENT, PARA, SURROUND, CONCAT, GROUP,
  HSPACE, VAR_VSPACE, VSPACE, SPACE,
  HTAB, MOVE, RESIZE, REPEAT, _FLOAT,
  DATOMS, DLINES, DPAGES, DBOX,

  // zero-ary formatting directives
  // modify is_formatting predicate when inserting new tags
  WITH_LIMITS, LINE_BREAK, NEW_LINE, NEXT_LINE,
  NO_BREAK, YES_INDENT, NO_INDENT, VAR_YES_INDENT,
  VAR_NO_INDENT, VAR_PAGE_BREAK, PAGE_BREAK,
  VAR_NO_PAGE_BREAK, NO_PAGE_BREAK, VAR_NEW_PAGE,
  NEW_PAGE, VAR_NEW_DPAGE, NEW_DPAGE,

  // mathematics
  LEFT, MID, RIGHT, BIG,
  LPRIME, RPRIME, BELOW, ABOVE,
  LSUB, LSUP, RSUB, RSUP,
  FRAC, SQRT, WIDE, VAR_WIDE, NEG, TREE,

  // tabular material
  TFORMAT, TWITH, CWITH, TMARKER,
  TABLE, ROW, CELL, SUBTABLE,

  // macro language
  ASSIGN, WITH, PROVIDES, VALUE, QUOTE_VALUE,
  MACRO, DRD_PROPS, ARG, QUOTE_ARG, COMPOUND,
  XMACRO, GET_LABEL, GET_ARITY, MAP_ARGS, EVAL_ARGS, MARK,
  EVAL, QUOTE, QUASI, QUASIQUOTE, UNQUOTE, VAR_UNQUOTE,
  IF, VAR_IF, CASE, WHILE, FOR_EACH,
  EXTERN, INCLUDE, USE_PACKAGE,
  
  // computational markup
  OR, XOR, AND, NOT,
  PLUS, MINUS, TIMES, OVER, DIV, MOD,
  MERGE, LENGTH, RANGE, NUMBER, _DATE, TRANSLATE, FIND_FILE,
  IS_TUPLE, LOOK_UP,
  EQUAL, UNEQUAL, LESS, LESSEQ, GREATER, GREATEREQ,

  // tags for source tree editing
  STYLE_WITH, VAR_STYLE_WITH, STYLE_ONLY, VAR_STYLE_ONLY,
  ACTIVE, VAR_ACTIVE, INACTIVE, VAR_INACTIVE,
  REWRITE_INACTIVE, INLINE_TAG, OPEN_TAG, MIDDLE_TAG, CLOSE_TAG,
  SYMBOL, LATEX, HYBRID,

  // other tags
  TUPLE, ATTR, COLLECTION, ASSOCIATE, BACKUP,
  LABEL, REFERENCE, PAGEREF, WRITE,
  SPECIFIC, HLINK, ACTION,
  TAG, MEANING, FLAG,

  // graphical tags
  GRAPHICS, SUPERPOSE, TEXT_AT, _POINT,
  LINE, CLINE, ARC, SPLINE, VAR_SPLINE, CSPLINE,
  FILL, POSTSCRIPT,

  // obsolete tags
  FORMAT, LINE_SEP, SPLIT, DELAY, HOLD, RELEASE,
  OLD_MATRIX, OLD_TABLE, OLD_MOSAIC, OLD_MOSAIC_ITEM,
  SET, RESET, EXPAND, VAR_EXPAND, HIDE_EXPAND,
  APPLY, BEGIN, END, FUNC, ENV,
  AUTHORIZE,

  // user extensions
  START_EXTENSIONS
};

inline tree_label SUB (bool right) { return right? RSUB: LSUB; }
inline tree_label SUP (bool right) { return right? RSUP: LSUP; }

/******************************************************************************
* Conversions from tree_labels to strings and vice versa
******************************************************************************/

void make_tree_label (tree_label l, string s);
tree_label make_tree_label (string s); // for extensions
string as_string (tree_label l);
tree_label as_tree_label (string s);
bool existing_tree_label (string s);

#ifdef OS_WIN32
inline bool operator == (tree_label first, tree_label second) {
  return (int) first == (int) second; }
inline bool operator != (tree_label first, tree_label second) {
  return (int) first != (int) second; }
#endif

#endif // defined TREE_LABEL_H
