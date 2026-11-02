
/******************************************************************************
* MODULE     : bridge_code.cpp
* DESCRIPTION: Bridge for the lines of source code (the tag code-lines)
* COPYRIGHT  : (C) 2026  the TeXmacs contributors
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "Boxes/construct.hpp"
#include "Line/lazy_paragraph.hpp"
#include "analyze.hpp"
#include "colors.hpp"

box code_box (path ip, tree doc, int i0, int i1, font fn, pencil pen,
              language lan, hashmap<string,color> cols, SI pitch, SI width);

// The colours which the languages give as variables of the environment
// (the concater reads them when it typesets a token: typeset_colored_substring)
static const char* code_color_vars[]= {
  "comment-color", "constant-color", "declaration-color", "defined-color",
  "keyword-color", "macro-color", "modifier-color", "number-color",
  "operator-color", "preprocessor-color", "string-color", "type-color",
  NULL };

/******************************************************************************
* <code-lines|<document|line|...>> holds lines of source code, as strings.
* They are typeset by one bridge, without a bridge, a paragraph or a box per
* line (see code_boxes.cpp): outside paper mode as one box, in paper mode as
* one box per line (the pager breaks the pages between them). The lines do
* not change the environment, which is the same everywhere inside.
******************************************************************************/

class bridge_code_rep: public bridge_rep {
public:
  bridge_code_rep (typesetter ttt, tree st, path ip);
  void notify_assign (path p, tree u);
  bool notify_macro (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  void my_typeset (int desired_status);
};

bridge_code_rep::bridge_code_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

bridge
bridge_code (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_code_rep> (ttt, st, ip);
}

void
bridge_code_rep::notify_assign (path p, tree u) {
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_code_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, var, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_code_rep::notify_change () {
  status= CORRUPTED;
}

void
bridge_code_rep::my_exec_until (path p) {
  // the lines do not change the environment
  (void) p;
}

void
bridge_code_rep::my_typeset (int desired_status) {
  (void) desired_status;
  tree doc= (N(st) == 1? st[0]: tree (DOCUMENT, ""));
  if (!is_func (doc, DOCUMENT)) doc= tree (DOCUMENT, doc);
  if (N(doc) == 0) doc= tree (DOCUMENT, "");
  int n= N(doc);

  // the lines as in a document of one line paragraphs: one font height and
  // the separation of the lines, and the space between paragraphs
  SI height= env->as_length (string ("1fn")) + env->get_length (PAR_SEP);
  SI pitch = height + env->get_vspace (PAR_PAR_SEP)->def;
  SI width = env->get_length (PAR_WIDTH);
  SI cw    = env->fn->wfn / 2;
  {
    metric ex;
    env->fn->get_extents ("m", ex);
    if (ex->x2 > 0) cw= ex->x2;
  }
  int longest= 0;
  for (int i=0; i<N(doc); i++)
    if (is_atomic (doc[i])) longest= max (longest, N(doc[i]->label));
  width= max (width, (SI) longest * cw);

  // the colours of the syntax, which are drawn when the lines are
  hashmap<string,color> cols;
  for (int i=0; code_color_vars[i] != NULL; i++) {
    string var= code_color_vars[i];
    if (env->provides (var)) {
      tree t= env->read (var);
      cols (var)= (t == ""? env->pen->get_color ():
                            named_color (as_string (t), env->alpha));
    }
  }

  array<box> bs;
  if (ttt->paper)
    for (int i=0; i<n; i++)
      bs << code_box (ip, doc, i, i+1, env->fn, env->pen, env->lan,
                      cols, pitch, width);
  else bs << code_box (ip, doc, 0, n, env->fn, env->pen, env->lan,
                       cols, pitch, width);

  // each box as a paragraph (in paper mode, one per line: they are then
  // stacked, and the pages broken, as the paragraphs of a document), with
  // the line items of the bridges around before the first and after the last
  for (int i=0; i<N(bs); i++) {
    lazy_paragraph par (env, ip);
    if (i == 0) par->a= ttt->a;
    par->a << line_item (STD_ITEM, OP_SKIP, bs[i], HYPH_INVALID);
    if (i == N(bs) - 1) par->a << ttt->b;
    par->format_paragraph ();
    ttt->insert_stack (par->sss->l, par->sss->sb);
  }
}
