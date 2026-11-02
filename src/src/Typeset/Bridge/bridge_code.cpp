
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

// Is the line of a paragraph of one code box the box in a phrase box, which
// can be made for another box in the same way? (Then 'it' is the line and x
// where the box is in it; not when the paragraph made more, as decorations)
static bool
code_template (array<page_item> l, box cb, page_item& it, SI& x) {
  int k= -1;
  for (int i=0; i<N(l); i++)
    if (l[i]->type == PAGE_LINE_ITEM) {
      if (k >= 0) return false;
      k= i;
    }
  if (k < 0 || N(l) != 1) return false;
  box b= l[k]->b;
  if (N(l[k]->fl) != 0 || b->subnr () != 1) return false;
  if (b->subbox (0).operator-> () != cb.operator-> ()) return false;
  x= b->sx (0);
  array<box> bs (1); bs[0]= cb;
  array<SI> sp (1); sp[0]= x;
  box r= phrase_box (b->ip, bs, sp);
  if (r->x1 != b->x1 || r->y1 != b->y1 || r->x2 != b->x2 || r->y2 != b->y2 ||
      r->x3 != b->x3 || r->y3 != b->y3 || r->x4 != b->x4 || r->y4 != b->y4 ||
      r->sx (0) != x || r->sy (0) != b->sy (0))
    return false;
  it= l[k];
  return true;
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
  // the line items of the bridges around before the first and after the last.
  // The paragraphs of the lines between the first and the last are all
  // alike: one of them is typeset, and the others are made as it was (the
  // making of a paragraph reads the environment, some 30 us a line)
  int N1= N(bs) - 1;
  page_item    tpl;  // the line of the paragraph of the line 1,
  SI           tpl_x= 0; // where the box of the line is in it,
  stack_border tpl_sb; // and its border
  bool         tpl_ok= false;
  for (int i=0; i<N(bs); i++) {
    if (tpl_ok && i < N1) {
      array<box> one (1); one[0]= bs[i];
      array<SI> sp (1); sp[0]= tpl_x;
      box b= phrase_box (tpl->b->ip, one, sp);
      array<page_item> l2 (1);
      l2[0]= page_item (tpl->type, b, tpl->spc, tpl->penalty,
                        tpl->fl, tpl->nr_cols, tpl->t);
      ttt->insert_stack (l2, tpl_sb);
      continue;
    }
    lazy_paragraph par (env, ip);
    if (i == 0) par->a= ttt->a;
    par->a << line_item (STD_ITEM, OP_SKIP, bs[i], HYPH_INVALID);
    if (i == N1) par->a << ttt->b;
    par->format_paragraph ();
    if (i == 1 && i < N1) tpl_ok= code_template (par->sss->l, bs[i],
                                                 tpl, tpl_x);
    if (i == 1 && tpl_ok) tpl_sb= par->sss->sb; // (merge_stack only reads it)
    ttt->insert_stack (par->sss->l, par->sss->sb);
  }
}
