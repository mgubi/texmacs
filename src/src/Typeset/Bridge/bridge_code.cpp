
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
              language lan, hashmap<string,color> cols, SI pitch, SI rpitch,
              hashmap<int,array<int> > wraps, SI width);
box code_row_box (path lip, tree line, int a, int b, font fn, pencil pen,
                  language lan, hashmap<string,color> cols, SI width);
array<int> code_breaks (string s, font fn, SI width);
extern int code_runs_generation;
static int code_rows_made= 0;
int get_code_rows_made () { return code_rows_made; }
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
* one box per row (the pager breaks the pages between them). A line wider
* than the paragraph is broken into rows (soft line breaks: code_breaks):
* the rows of a line are the lines of a paragraph, the lines of the block
* the paragraphs of a document. The lines do not change the environment,
* which is the same everywhere inside.
*
* In paper mode, the rows of a line which did not change are used again from
* one pass to the next (lines): the same boxes and the same page items, so
* that the pager keeps the pages which did not change and the change log has
* only the lines which did. Their boxes have the path which the line keeps,
* renumbered in place when lines are inserted or removed (see
* code_boxes.cpp). The rows are made as the paragraphs of one and of two
* rows of the line 1 were (the making of a paragraph reads the environment,
* some 30 us a line), and are stacked as they are there. What all the lines
* share (the font, the widths, these paragraphs) is in a signature: when it
* changes, nothing is used again. TEXMACS_TYPESET_CACHES=off makes everything
* again, "check" also stacks all the lines as paragraphs and compares.
******************************************************************************/

struct code_line {
  tree             line;  // the line (it keeps the tree alive: the key is
                          // its address)
  string           text;  // its text, when the boxes were made
  array<box>       rows;  // the boxes of its rows
  array<box>       bs;    // the lines of the page which they are in (phrase
                          // boxes)
  array<page_item> its;   // as they are stacked before another line
};

// the paragraph of the first or of the last line, used again when the
// line did not change and there are no line items from around
struct code_end {
  box              b;  // the box of its first row (none: not kept)
  array<page_item> l;
  stack_border     sb;
};

// the soft line breaks of a line, kept while its text is the same
struct code_wrap {
  string     text;
  array<int> starts;
};

class bridge_code_rep: public bridge_rep {
public:
  std::unordered_map<void*,code_line> lines; // see above
  unsigned long long lines_sig;              // what they share
  code_end first, last;                      // their paragraphs
  std::unordered_map<void*,code_wrap> wraps; // the lines with several rows
  unsigned long long wraps_sig;              // the font and the width

  array<int> row_starts (tree t, font fn, SI width, SI cw,
                         std::unordered_map<void*,code_wrap>& next);
  void end_paragraph (code_end& e, array<box> rows, array<line_item> before,
                      array<line_item> after,
                      array<page_item>& l, stack_border& sb);
  void typeset_paper (tree doc, font fn, pencil pen, language lan,
                      hashmap<string,color> cols, SI width, SI cw);
  void typeset_paragraphs (array<array<box> > pars);

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
  bridge_rep (ttt, st, ip), lines_sig (0), wraps_sig (0) {}

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

/******************************************************************************
* Subroutines
******************************************************************************/

static inline bool
same_text (string a, string b) {
  // (the string of a line which did not change is the same string)
  return a.operator-> () == b.operator-> () || a == b;
}

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

// the paragraph of the rows of a line (a line each: next-line between them)
static void
code_paragraph (edit_env env, path ip, array<box> rows,
                array<line_item> before, array<line_item> after,
                array<page_item>& l, stack_border& sb) {
  lazy_paragraph par (env, ip);
  par->a= copy (before); // (<< below would append to 'before')
  for (int i=0; i<N(rows); i++) {
    if (i > 0)
      par->a << line_item (CONTROL_ITEM, OP_SKIP,
                           empty_box (decorate_middle (ip)),
                           HYPH_INVALID, tree (NEXT_LINE));
    par->a << line_item (STD_ITEM, OP_SKIP, rows[i], HYPH_INVALID);
  }
  par->a << after;
  par->format_paragraph ();
  l= par->sss->l;
  sb= par->sss->sb;
}

// The line of the page of a row: a phrase box, with the box of the row
// last, and before it empty boxes (the next-line between two rows)
struct code_phrase {
  path       ip;    // the one of the phrase box
  array<box> lead;  // the empty boxes (their extents)
  array<SI>  spc;   // the spaces before the boxes
};

static bool
same_phrase (code_phrase& a, code_phrase& b) {
  if (N(a.lead) != N(b.lead) || a.spc != b.spc) return false;
  for (int i=0; i<N(a.lead); i++)
    if (a.lead[i]->x1 != b.lead[i]->x1 || a.lead[i]->y1 != b.lead[i]->y1 ||
        a.lead[i]->x2 != b.lead[i]->x2 || a.lead[i]->y2 != b.lead[i]->y2)
      return false;
  return true;
}

static box
make_phrase (code_phrase& ph, box b, path eip) {
  array<box> bs;
  for (int i=0; i<N(ph.lead); i++) {
    box e= ph.lead[i];
    bs << empty_box (eip, e->x1, e->y1, e->x2, e->y2);
  }
  bs << b;
  return phrase_box (ph.ip, bs, ph.spc);
}

// Is the page item a phrase box which can be made for another row in the
// same way? (Not when the paragraph made more, as decorations)
static bool
get_phrase (page_item it, box b, path eip, code_phrase& ph) {
  if (it->type != PAGE_LINE_ITEM || N(it->fl) != 0) return false;
  box p= it->b;
  int n= p->subnr ();
  if (n < 1 || p->subbox (n-1).operator-> () != b.operator-> ())
    return false;
  ph.ip  = p->ip;
  ph.lead= array<box> ();
  ph.spc = array<SI> ();
  SI x= 0;
  for (int i=0; i<n; i++) {
    box c= p->subbox (i);
    if (i < n-1 && (c->get_type () != STD_BOX || c->w () != 0 ||
                    c->subnr () != 0 || is_accessible (c->ip)))
      return false;
    if (i < n-1) ph.lead << c;
    ph.spc << (p->sx (i) - x);
    x= p->sx (i) + c->x2;
  }
  box r= make_phrase (ph, b, eip);
  if (r->x1 != p->x1 || r->y1 != p->y1 || r->x2 != p->x2 || r->y2 != p->y2 ||
      r->x3 != p->x3 || r->y3 != p->y3 || r->x4 != p->x4 || r->y4 != p->y4)
    return false;
  for (int i=0; i<n; i++)
    if (r->sx (i) != p->sx (i) || r->sy (i) != p->sy (i)) return false;
  return true;
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

array<int>
bridge_code_rep::row_starts (tree t, font fn, SI width, SI cw,
                             std::unordered_map<void*,code_wrap>& next) {
  // the starts of the rows of a line (only lines which may be too wide are
  // measured: a character is at most cw wide, also <...>)
  static array<int> one_row;
  if (N(one_row) == 0) one_row << 0;
  if (!is_atomic (t) || (SI) N(t->label) * cw <= width) return one_row;
  void* key= (void*) t.operator-> ();
  auto f= wraps.find (key);
  if (f != wraps.end () && same_text (f->second.text, t->label)) {
    next[key]= f->second;
    return f->second.starts;
  }
  code_wrap w;
  w.text  = t->label;
  w.starts= code_breaks (t->label, fn, width);
  next[key]= w;
  return w.starts;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_code_rep::my_typeset (int desired_status) {
  (void) desired_status;
  code_runs_generation++; // (the colours of a line may depend on others)
  tree doc= (N(st) == 1? st[0]: tree (DOCUMENT, ""));
  if (!is_func (doc, DOCUMENT)) doc= tree (DOCUMENT, doc);
  if (N(doc) == 0) doc= tree (DOCUMENT, "");
  int n= N(doc);
  font fn= env->fn;

  // the width of the rows: the one of the first line of a paragraph
  SI width, d1, d2, d3, d4, d5, d6, d7;
  env->get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
  width -= env->get_length (PAR_LEFT) + env->get_length (PAR_RIGHT) +
           env->as_length (env->read (PAR_FIRST));
  SI cw= 0;
  {
    const char* wide[]= { "M", "W", "m", "@", "_", NULL };
    for (int i=0; wide[i] != NULL; i++) {
      metric ex;
      fn->get_extents (wide[i], ex);
      cw= max (cw, ex->x2);
    }
    if (cw <= 0) cw= fn->wfn;
  }
  width= max (width, 8 * cw);

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

  // the soft line breaks are kept for the same font and width
  unsigned long long wsig= sig_mix (sig_mix (0x2545F4914F6CDD1DULL,
                                             (unsigned long long) width),
                                    (unsigned long long)
                                    (void*) fn.operator-> ());
  if (wsig != wraps_sig) wraps.clear ();
  wraps_sig= wsig;

  if (ttt->paper && n >= 4 && typeset_caches_mode () != 0) {
    typeset_paper (doc, fn, env->pen, env->lan, cols, width, cw);
    return;
  }
  lines.clear ();
  first.b= box ();
  last.b= box ();
  std::unordered_map<void*,code_wrap> next;
  array<array<box> > pars;
  if (ttt->paper)
    for (int i=0; i<n; i++) {
      array<int> st= row_starts (doc[i], fn, width, cw, next);
      path lip= descend (descend (ip, 0), i);
      int len= (is_atomic (doc[i])? N(doc[i]->label): 0);
      array<box> rows;
      for (int r=0; r<N(st); r++)
        rows << code_row_box (lip, doc[i], st[r],
                              r+1 < N(st)? st[r+1]: len,
                              fn, env->pen, env->lan, cols, width);
      pars << rows;
    }
  else {
    // the rows of a line as the lines of a paragraph, the lines as the
    // paragraphs of a document
    SI height= env->as_length (string ("1fn")) + env->get_length (PAR_SEP);
    SI pitch = height + env->get_vspace (PAR_PAR_SEP)->def;
    SI rpitch= height + env->get_vspace (PAR_LINE_SEP)->def;
    hashmap<int,array<int> > ws;
    for (int i=0; i<n; i++) {
      array<int> st= row_starts (doc[i], fn, width, cw, next);
      if (N(st) > 1) ws (i)= st;
    }
    array<box> one;
    one << code_box (ip, doc, 0, n, fn, env->pen, env->lan, cols,
                     pitch, rpitch, ws, width);
    pars << one;
  }
  wraps.swap (next);
  typeset_paragraphs (pars);
}

void
bridge_code_rep::typeset_paragraphs (array<array<box> > pars) {
  // each line as a paragraph of its rows, with the line items of the bridges
  // around before the first and after the last
  int N1= N(pars) - 1;
  for (int i=0; i<N(pars); i++) {
    array<page_item> l;
    stack_border sb;
    code_paragraph (env, ip, pars[i],
                    i == 0? ttt->a: array<line_item> (),
                    i == N1? ttt->b: array<line_item> (), l, sb);
    ttt->insert_stack (l, sb);
  }
}

/******************************************************************************
* Paper mode: the lines which did not change are used again
******************************************************************************/

void
bridge_code_rep::end_paragraph (code_end& e, array<box> rows,
                                array<line_item> before,
                                array<line_item> after,
                                array<page_item>& l, stack_border& sb) {
  // (merge_stack copies the lines which it changes, and only reads sb)
  bool alone= N(before) == 0 && N(after) == 0;
  if (alone && !is_nil (e.b) && e.b.operator-> () == rows[0].operator-> ()) {
    l= e.l; sb= e.sb;
    return;
  }
  code_paragraph (env, ip, rows, before, after, l, sb);
  if (alone) { e.b= rows[0]; e.l= l; e.sb= sb; }
  else e.b= box ();
}

void
bridge_code_rep::typeset_paper (tree doc, font fn, pencil pen, language lan,
                                hashmap<string,color> cols, SI width, SI cw) {
  int n= N(doc);
  path dip= descend (ip, 0);
  std::unordered_map<void*,code_wrap> next_wraps;
  auto line_path= [&] (int i) -> path {
    // the path which the line keeps, renumbered in place by the editor
    path lip= obtain_ip (doc[i]);
    if (!ip_attached (lip) || is_nil (lip) || lip->item != i)
      lip= descend (dip, i);
    return lip;
  };
  auto make_rows= [&] (int i, path lip) -> array<box> {
    array<int> st= row_starts (doc[i], fn, width, cw, next_wraps);
    int len= (is_atomic (doc[i])? N(doc[i]->label): 0);
    array<box> rows;
    for (int r=0; r<N(st); r++)
      rows << code_row_box (lip, doc[i], st[r], r+1 < N(st)? st[r+1]: len,
                            fn, pen, lan, cols, width);
    code_rows_made += N(rows);
    return rows;
  };
  auto all_paragraphs= [&] () {
    array<array<box> > pars;
    for (int i=0; i<n; i++) pars << make_rows (i, line_path (i));
    typeset_paragraphs (pars);
  };

  // Paragraphs of 1 to CODE_TPL rows (of the line 1): the rows are made as
  // these were, and stacked as they are, before a row of the same line,
  // and, for the last one, before another line (mid: a row stacked before a
  // paragraph). The breaks of pages after the first and the last two rows
  // of a paragraph have larger penalties (stacker_rep::new_paragraph): a
  // line of more rows has its first and last two as these of CODE_TPL rows,
  // and the others as the middle one
  #define CODE_TPL 5
  path lip1= line_path (1);
  int  len1= (is_atomic (doc[1])? N(doc[1]->label): 0);
  path eip = decorate_middle (ip);
  array<array<page_item> > T (CODE_TPL + 1);
  array<stack_border> TS (CODE_TPL + 1);
  code_phrase ph_first, ph_rest;
  bool ok= true;
  for (int R=1; R<=CODE_TPL && ok; R++) {
    // (a box is placed in one phrase box only: its place is in the box)
    array<box> rs;
    for (int r=0; r<R; r++)
      rs << code_row_box (lip1, doc[1], 0, len1, fn, pen, lan, cols, width);
    code_paragraph (env, ip, rs, array<line_item> (), array<line_item> (),
                    T[R], TS[R]);
    ok= (N(T[R]) == R);
    for (int r=0; ok && r<R; r++) {
      code_phrase ph;
      ok= get_phrase (T[R][r], rs[r], eip, ph);
      if (!ok) break;
      if (R == 1) ph_first= ph;
      else if (R == 2 && r == 1) ph_rest= ph;
      else ok= same_phrase (ph, r == 0? ph_first: ph_rest);
    }
  }
  if (!ok) {
    lines.clear ();
    all_paragraphs ();
    wraps.swap (next_wraps);
    return;
  }
  page_item tpl= T[1][0];
  stack_border sb1= TS[1];
  array<page_item> mid (CODE_TPL + 1);
  for (int R=1; R<=CODE_TPL; R++) {
    array<page_item> ls (1);
    ls[0]= T[R][R-1];
    stack_border sbs= copy_border (TS[R]);
    merge_stack (ls, sbs, T[1], sb1);
    mid[R]= ls[0];
  }
  // the space and the penalty after the row r of a line of R rows (last:
  // the last row before another line; alone: the last row, not stacked)
  auto row_item= [&] (int R, int r, bool alone) -> page_item {
    int Rt= min (R, CODE_TPL), k= r;
    if (R > CODE_TPL)
      k= (r < 2? r: (r > R-3? r - (R - CODE_TPL): 2));
    if (r < R-1 || alone) return T[Rt][k];
    return mid[Rt];
  };

  // what the lines share
  unsigned long long sig= 0x84222325CBF29CE4ULL;
  sig= sig_mix (sig, wraps_sig);
  sig= sig_mix (sig, (unsigned long long) pen->get_color ());
  sig= sig_mix (sig, (unsigned long long) pen->get_width ());
  sig= sig_mix (sig, (unsigned long long) (void*) lan.operator-> ());
  for (int i=0; code_color_vars[i] != NULL; i++) {
    string var= code_color_vars[i];
    sig= sig_mix (sig, cols->contains (var)? 1 + (unsigned long long) cols[var]: 0);
  }
  for (path p= ip; !is_nil (p); p= p->next)
    sig= sig_mix (sig, (unsigned long long) (long long) p->item);
  for (int k=0; k<2; k++) {
    code_phrase& ph= (k == 0? ph_first: ph_rest);
    sig= sig_mix (sig, (unsigned long long) N(ph.lead));
    for (int i=0; i<N(ph.lead); i++) {
      sig= sig_mix (sig, (unsigned long long) ph.lead[i]->y1);
      sig= sig_mix (sig, (unsigned long long) ph.lead[i]->y2);
    }
    for (int i=0; i<N(ph.spc); i++)
      sig= sig_mix (sig, (unsigned long long) ph.spc[i]);
  }
  sig= sig_mix (sig, (unsigned long long) tpl->type);
  sig= sig_mix (sig, (unsigned long long) tpl->nr_cols);
  sig= sig_mix (sig, (unsigned long long) (long long) tpl->penalty);
  sig= sig_space (sig, tpl->spc);
  for (int R=1; R<=CODE_TPL; R++) {
    for (int r=0; r<R; r++) {
      sig= sig_space (sig, T[R][r]->spc);
      sig= sig_mix (sig, (unsigned long long) (long long) T[R][r]->penalty);
    }
    sig= sig_space (sig, mid[R]->spc);
    sig= sig_mix (sig, (unsigned long long) (long long) mid[R]->penalty);
  }
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
      if (f != lines.end () && same_text (f->second.text, t->label) &&
          f->second.rows[0]->ip == lip) {
        code_line& r= next[key];
        r= f->second;
        // (the soft line breaks of the line are kept with it)
        (void) row_starts (t, fn, width, cw, next_wraps);
        return r;
      }
    }
    code_line c;
    c.line= t;
    c.text= (is_atomic (t)? t->label: string ("<markup>"));
    c.rows= make_rows (i, lip);
    int R= N(c.rows);
    for (int r=0; r<R; r++) {
      box b= make_phrase (r == 0? ph_first: ph_rest, c.rows[r], eip);
      c.bs  << b;
      page_item ri= row_item (R, r, false);
      c.its << page_item (tpl->type, b, ri->spc, ri->penalty,
                          tpl->fl, tpl->nr_cols, tpl->t);
    }
    if (!is_atomic (t)) { scratch= c; return scratch; }
    code_line& r= next[key];
    r= c;
    return r;
  };

  // the first line, with the line items before, and the line 1
  array<page_item> l0;
  stack_border sb0;
  end_paragraph (first, get (0).rows, ttt->a, array<line_item> (), l0, sb0);
  ttt->insert_stack (l0, sb0);
  ttt->insert_stack (get (1).its, sb1);
  // the lines between, already stacked (merge_stack would only shove them
  // as the line 1 was: their borders and their heights are the same)
  for (int i=2; i<n-2; i++) ttt->l << get (i).its;
  // the line before the last: its last row as a line alone, which the last
  // paragraph shoves
  if (n-2 >= 2) {
    code_line& c= get (n-2);
    int R= N(c.its);
    for (int r=0; r+1<R; r++) ttt->l << c.its[r];
    page_item ri= row_item (R, R-1, true);
    ttt->l << page_item (tpl->type, c.bs[R-1], ri->spc, ri->penalty,
                         tpl->fl, tpl->nr_cols, tpl->t);
  }
  // the last line, with the line items after
  array<page_item> ln;
  stack_border sbn;
  end_paragraph (last, get (n-1).rows, array<line_item> (), ttt->b, ln, sbn);
  ttt->insert_stack (ln, sbn);
  lines.swap (next);
  wraps.swap (next_wraps);

  if (typeset_caches_mode () == 2) {
    // check: every line as a paragraph, stacked one after the other
    array<page_item> fast_l= ttt->l;
    stack_border     fast_sb= ttt->sb;
    ttt->l = array<page_item> ();
    ttt->sb= stack_border ();
    std::unordered_map<void*,code_wrap> saved= wraps;
    int made= code_rows_made;
    all_paragraphs ();
    code_rows_made= made; // (the count is the one without the check)
    wraps= saved;
    if (!same_code_stack (fast_l, ttt->l))
      typeset_cache_mismatch ("lines of code used again");
    ttt->l = fast_l;
    ttt->sb= fast_sb;
  }
}
