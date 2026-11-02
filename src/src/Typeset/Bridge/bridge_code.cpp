
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
#include <unordered_map>

box code_box (path ip, tree doc, int i0, int i1, font fn, pencil pen,
              language lan, hashmap<string,color> cols, SI pitch, SI width);
box code_line_box (path lip, tree line, font fn, pencil pen, language lan,
                   hashmap<string,color> cols, SI pitch, SI width);
void merge_stack (array<page_item>& l, stack_border& sb,
                  array<page_item> l2, stack_border sb2);

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
*
* In paper mode, the line of a line which did not change is used again from
* one pass to the next (lines): the same box and the same page item, so that
* the pager keeps the pages which did not change and the change log has only
* the lines which did. Its box has the path which the line keeps, renumbered
* in place when lines are inserted or removed (see code_boxes.cpp). What all
* the lines share (the font, the widths, the paragraph of the line 1 which
* they are made as) is in a signature: when it changes, nothing is used
* again. TEXMACS_TYPESET_CACHES=off makes everything again, "check" also
* stacks all the lines as paragraphs and compares.
******************************************************************************/

struct code_line {
  tree      line;  // the line (it keeps the tree alive: the key is its address)
  string    text;  // its text, when the box was made
  box       b;     // the line of its paragraph (a phrase box)
  page_item it;    // as it is stacked between two lines
};

// the paragraph of the first or of the last line, used again when the
// line did not change and there are no line items from around
struct code_end {
  box              b;  // the box of the line (none: not kept)
  array<page_item> l;
  stack_border     sb;
};

class bridge_code_rep: public bridge_rep {
public:
  std::unordered_map<void*,code_line> lines; // see above
  unsigned long long lines_sig;              // what they share
  code_end first, last;                      // their paragraphs
  void end_paragraph (code_end& e, box b, array<line_item> before,
                      array<line_item> after,
                      array<page_item>& l, stack_border& sb);

  void typeset_paper (tree doc, font fn, pencil pen, language lan,
                      hashmap<string,color> cols, SI pitch, SI width);
  void typeset_paragraphs (array<box> bs, bool alike= true);

public:
  bridge_code_rep (typesetter ttt, tree st, path ip);
  void notify_assign (path p, tree u);
  bool notify_macro (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_code_rep::bridge_code_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), lines_sig (0) {}

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

bool
bridge_code_rep::my_typeset_will_be_complete () {
  // Typeset again, the block makes again the lines which changed only (in
  // paper mode), and it has no labels nor entries of the auxiliary tables:
  // an edit is not a complete pass (which would also make the headers and
  // the footers of all the pages again), unless it is the first typesetting
  return (status & VALID_MASK) == CORRUPTED && version == 0;
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

  if (ttt->paper && n >= 4 && typeset_caches_mode () != 0) {
    typeset_paper (doc, env->fn, env->pen, env->lan, cols, pitch, width);
    return;
  }
  lines.clear ();
  array<box> bs;
  if (ttt->paper)
    for (int i=0; i<n; i++)
      bs << code_box (ip, doc, i, i+1, env->fn, env->pen, env->lan,
                      cols, pitch, width);
  else bs << code_box (ip, doc, 0, n, env->fn, env->pen, env->lan,
                       cols, pitch, width);
  typeset_paragraphs (bs);
}

/******************************************************************************
* The boxes as paragraphs
******************************************************************************/

void
bridge_code_rep::typeset_paragraphs (array<box> bs, bool alike) {
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
    if (i == 0) par->a= copy (ttt->a); // (<< below would append to ttt->a)
    par->a << line_item (STD_ITEM, OP_SKIP, bs[i], HYPH_INVALID);
    if (i == N1) par->a << ttt->b;
    par->format_paragraph ();
    if (alike && i == 1 && i < N1)
      tpl_ok= code_template (par->sss->l, bs[i], tpl, tpl_x);
    if (i == 1 && tpl_ok) tpl_sb= par->sss->sb; // (merge_stack only reads it)
    ttt->insert_stack (par->sss->l, par->sss->sb);
  }
}

/******************************************************************************
* Paper mode: the lines which did not change are used again
******************************************************************************/

static inline unsigned long long
sig_mix (unsigned long long h, unsigned long long x) {
  h= (h ^ x) * 0x9E3779B97F4A7C15ULL;
  return h ^ (h >> 29);
}

static inline unsigned long long
sig_space (unsigned long long h, space spc) {
  h= sig_mix (h, (unsigned long long) spc->min);
  h= sig_mix (h, (unsigned long long) spc->def);
  return sig_mix (h, (unsigned long long) spc->max);
}

static stack_border
copy_border (stack_border sb) {
  stack_border r;
  r->height= sb->height; r->sep= sb->sep;
  r->hor_sep= sb->hor_sep; r->ver_sep= sb->ver_sep;
  r->bot= sb->bot; r->top= sb->top;
  r->height_before= sb->height_before; r->sep_before= sb->sep_before;
  r->hor_sep_before= sb->hor_sep_before; r->ver_sep_before= sb->ver_sep_before;
  r->vspc_before= sb->vspc_before; r->vspc_after= sb->vspc_after;
  r->nobr_before= sb->nobr_before; r->nobr_after= sb->nobr_after;
  return r;
}

static void
code_paragraph (edit_env env, path ip, box b,
                array<line_item> before, array<line_item> after,
                array<page_item>& l, stack_border& sb) {
  lazy_paragraph par (env, ip);
  par->a= copy (before); // (<< below would append to 'before')
  par->a << line_item (STD_ITEM, OP_SKIP, b, HYPH_INVALID);
  par->a << after;
  par->format_paragraph ();
  l= par->sss->l;
  sb= par->sss->sb;
}

// do two stacks of lines make the same lines? (for the check mode)
static bool
same_code_stack (array<page_item> l1, array<page_item> l2) {
  if (N(l1) != N(l2)) return false;
  for (int i=0; i<N(l1); i++) {
    page_item a= l1[i], b= l2[i];
    if (a->type != b->type || a->penalty != b->penalty ||
        a->nr_cols != b->nr_cols || a->spc->min != b->spc->min ||
        a->spc->def != b->spc->def || a->spc->max != b->spc->max ||
        a->b->x1 != b->b->x1 || a->b->y1 != b->b->y1 ||
        a->b->x2 != b->b->x2 || a->b->y2 != b->b->y2 ||
        a->b->x3 != b->b->x3 || a->b->y3 != b->b->y3 ||
        a->b->x4 != b->b->x4 || a->b->y4 != b->b->y4 ||
        a->b->find_lip () != b->b->find_lip () ||
        a->b->find_rip () != b->b->find_rip ())
      return false;
  }
  return true;
}

void
bridge_code_rep::end_paragraph (code_end& e, box b, array<line_item> before,
                                array<line_item> after,
                                array<page_item>& l, stack_border& sb) {
  // (merge_stack copies the lines which it changes, and only reads sb)
  bool alone= N(before) == 0 && N(after) == 0;
  if (alone && !is_nil (e.b) && e.b.operator-> () == b.operator-> ()) {
    l= e.l; sb= e.sb;
    return;
  }
  code_paragraph (env, ip, b, before, after, l, sb);
  if (alone) { e.b= b; e.l= l; e.sb= sb; }
  else e.b= box ();
}

void
bridge_code_rep::typeset_paper (tree doc, font fn, pencil pen, language lan,
                                hashmap<string,color> cols,
                                SI pitch, SI width) {
  int n= N(doc);
  path dip= descend (ip, 0);
  auto line_path= [&] (int i) -> path {
    // the path which the line keeps, renumbered in place by the editor
    path lip= obtain_ip (doc[i]);
    if (!ip_attached (lip) || is_nil (lip) || lip->item != i)
      lip= descend (dip, i);
    return lip;
  };

  // the line 1 as a paragraph: the lines between the first and the last
  // are made as it was, and stacked as it is between two lines
  box cb1= code_line_box (line_path (1), doc[1], fn, pen, lan, cols,
                          pitch, width);
  array<page_item> l1;
  stack_border sb1;
  code_paragraph (env, ip, cb1, array<line_item> (), array<line_item> (),
                  l1, sb1);
  page_item tpl;
  SI tpl_x= 0;
  if (!code_template (l1, cb1, tpl, tpl_x)) {
    lines.clear ();
    array<box> bs;
    for (int i=0; i<n; i++)
      bs << code_line_box (line_path (i), doc[i], fn, pen, lan, cols,
                           pitch, width);
    typeset_paragraphs (bs);
    return;
  }
  array<page_item> ls (1);
  ls[0]= tpl;
  stack_border sbs= copy_border (sb1);
  merge_stack (ls, sbs, l1, sb1);
  space mid_spc= ls[0]->spc;
  int   mid_pen= ls[0]->penalty;

  // what the lines share
  unsigned long long sig= 0x84222325CBF29CE4ULL;
  sig= sig_mix (sig, (unsigned long long) pitch);
  sig= sig_mix (sig, (unsigned long long) width);
  sig= sig_mix (sig, (unsigned long long) (void*) fn.operator-> ());
  sig= sig_mix (sig, (unsigned long long) pen->get_color ());
  sig= sig_mix (sig, (unsigned long long) pen->get_width ());
  sig= sig_mix (sig, (unsigned long long) (void*) lan.operator-> ());
  for (int i=0; code_color_vars[i] != NULL; i++) {
    string var= code_color_vars[i];
    sig= sig_mix (sig, cols->contains (var)? 1 + (unsigned long long) cols[var]: 0);
  }
  for (path p= ip; !is_nil (p); p= p->next)
    sig= sig_mix (sig, (unsigned long long) (long long) p->item);
  sig= sig_mix (sig, (unsigned long long) tpl_x);
  sig= sig_mix (sig, (unsigned long long) tpl->type);
  sig= sig_mix (sig, (unsigned long long) tpl->nr_cols);
  sig= sig_mix (sig, (unsigned long long) (long long) tpl->penalty);
  sig= sig_space (sig, tpl->spc);
  sig= sig_space (sig, mid_spc);
  sig= sig_mix (sig, (unsigned long long) (long long) mid_pen);
  if (sig != lines_sig) {
    lines.clear ();
    first.b= box ();
    last.b= box ();
  }
  lines_sig= sig;

  // the lines, made again when they changed
  std::unordered_map<void*,code_line> next;
  next.reserve (n);
  code_line scratch; // (a line with markup, which is not kept)
  auto get= [&] (int i) -> code_line& {
    tree& t= doc[i];
    void* key= (void*) t.operator-> ();
    path lip= line_path (i);
    if (is_atomic (t)) {
      auto f= lines.find (key);
      if (f != lines.end () && f->second.text == t->label &&
          f->second.b[0]->ip == lip) {
        code_line& r= next[key];
        r= f->second;
        return r;
      }
    }
    box cb= code_line_box (lip, t, fn, pen, lan, cols, pitch, width);
    array<box> one (1); one[0]= cb;
    array<SI> sp (1); sp[0]= tpl_x;
    box b= phrase_box (tpl->b->ip, one, sp);
    code_line c;
    c.line= t;
    c.text= (is_atomic (t)? t->label: string ("<markup>"));
    c.b   = b;
    c.it  = page_item (tpl->type, b, mid_spc, mid_pen,
                       tpl->fl, tpl->nr_cols, tpl->t);
    if (!is_atomic (t)) { scratch= c; return scratch; }
    code_line& r= next[key];
    r= c;
    return r;
  };

  // the first line, with the line items before, and the line 1
  array<page_item> l0;
  stack_border sb0;
  end_paragraph (first, get (0).b[0], ttt->a, array<line_item> (), l0, sb0);
  ttt->insert_stack (l0, sb0);
  array<page_item> one (1);
  one[0]= get (1).it;
  ttt->insert_stack (one, sb1);
  // the lines between, already stacked (merge_stack would only shove them
  // as the line 1 was: their borders and their heights are the same)
  for (int i=2; i<n-2; i++) ttt->l << get (i).it;
  // the line before the last, as a line alone: the last paragraph shoves it
  if (n-2 >= 2) {
    code_line& c= get (n-2);
    ttt->l << page_item (tpl->type, c.b, tpl->spc, tpl->penalty,
                         tpl->fl, tpl->nr_cols, tpl->t);
  }
  // the last line, with the line items after
  array<page_item> ln;
  stack_border sbn;
  end_paragraph (last, get (n-1).b[0], array<line_item> (), ttt->b, ln, sbn);
  ttt->insert_stack (ln, sbn);
  lines.swap (next);

  if (typeset_caches_mode () == 2) {
    // check: every line as a paragraph, stacked one after the other
    array<page_item> fast_l= ttt->l;
    stack_border     fast_sb= ttt->sb;
    ttt->l = array<page_item> ();
    ttt->sb= stack_border ();
    array<box> bs;
    for (int i=0; i<n; i++)
      bs << code_line_box (line_path (i), doc[i], fn, pen, lan, cols,
                           pitch, width);
    typeset_paragraphs (bs, false);
    if (!same_code_stack (fast_l, ttt->l))
      typeset_cache_mismatch ("lines of code used again");
    ttt->l = fast_l;
    ttt->sb= fast_sb;
  }
}
