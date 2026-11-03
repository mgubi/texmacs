
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
#include "Boxes/Basic/code_boxes.hpp"
#include "code_lines.hpp" // (System/Language: the state of the lines)

static int code_rows_made= 0;
int get_code_rows_made () { return code_rows_made; }
static int code_records_made= 0;
int get_code_records_made () { return code_records_made; }

// The state of a line which the features of programs give (tree-sitter,
// the language servers: System/Language/code_lines.hpp, and
// src/docs/code-features.md). A line with layers (the colours of a server,
// inlay hints) is typeset by the concater (typeset_layered_string), as a
// line with markup; the folds hide lines, as bridge_rep::fold_line does

extern bool typeset_folds; // (not when printing, see bridge.cpp)

static inline bool
rich_line (tree t) {
  // a line typeset by the concater: with markup, or with layers
  if (!is_atomic (t)) return true;
  return code_line_has_layers (t);
}

static inline int
fold_kind (tree doc, int i) {
  // 1: the line i is hidden; 2: hidden lines follow it (a mark); else 0
  if (!typeset_folds || !code_folds_possible ()) return 0;
  if (code_line_fold_state (doc[i]) > 0) return 1;
  if (i + 1 < N(doc) && code_line_fold_state (doc[i+1]) > 0) return 2;
  return 0;
}

static void
fold_items (array<page_item>& items, int kind, path lip, edit_env env) {
  // the page items of a line with a fold kind (as bridge_rep::fold_line)
  if (kind == 1) {
    for (int i=0; i<N(items); i++)
      if (items[i]->type != PAGE_CONTROL_ITEM) {
        box b= items[i]->b;
        items[i]->type= PAGE_HIDDEN_ITEM;
        items[i]->b   = clip_box (lip, b, b->x1, 0, b->x1, 0);
        items[i]->spc = space (0, 0, 0);
      }
  }
  else if (kind == 2) {
    for (int i=N(items)-1; i>=0; i--)
      if (items[i]->type == PAGE_LINE_ITEM) {
        pencil pen (named_color ("dark grey"), env->pen->get_width ());
        box gap = empty_box (decorate_right (lip), 0, 0, env->fn->spc->def, 0);
        box mark= text_box (decorate_right (lip), 0, "...", env->fn, pen);
        array<box> bs;
        bs << items[i]->b << gap << mark;
        items[i]->b= concat_box (items[i]->b->ip, bs);
        break;
      }
  }
}
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
* the paragraphs of a document. A line with markup is typeset by the
* concater, and its items are put on the grid (see rich_paragraph). The
* lines do not change the environment, which is the same everywhere inside.
*
* The bridge keeps a record of each line which needs one (recs): the soft
* line breaks of a line which may be too wide, a line with markup, and, in
* paper mode, every line, with its rows, its boxes and its page items. A
* record holds while its line is the same tree with the same text (the same
* string: an edit gives the line another one) and while the font and the
* width are the same (wsig). The edits come as notifications with the place
* of the line: the records are inserted, removed or dropped there, without
* going through the other lines; a short line without markup has none. The
* tree st is the one of the document: it is changed in place by the edit,
* after the notification (see apply in observer.cpp), and nothing is made
* from it here.
*
* In paper mode, the rows of a line which did not change are used again from
* one pass to the next: the same boxes and the same page items, so that the
* pager keeps the pages which did not change and the change log has only the
* lines which did. Their boxes have the path which the line keeps,
* renumbered in place when lines are inserted or removed (see
* code_boxes.cpp). The rows are made as the paragraphs of one to five rows
* of the line 1 were (the making of a paragraph reads the environment, some
* 30 us a line), and are stacked as they are there. What all the lines share
* (the font, the widths, these paragraphs) is in a signature (psig): when it
* changes, the rows are made again. TEXMACS_TYPESET_CACHES=off makes
* everything again, "check" also makes the records again and stacks all the
* lines as paragraphs, and compares.
******************************************************************************/

struct code_rec_rep: concrete_struct {
  tree               line;   // the line,
  string             text;   // its text (when it is a string),
  unsigned long long wsig;   // and the font and the width of the rows
  array<int>         starts; // the starts of its rows
  // in paper mode
  unsigned long long psig;   // what the rows were made as
  path               lip;    // the path of the line which they have
  array<box>         rows;   // the boxes of the rows
  array<box>         bs;     // the lines of the page which they are in
  array<page_item>   its;    // as they are stacked before another line
  // (a line with markup: its rows are typeset as a paragraph, with this
  // border, and its items are as they are in the paragraph)
  bool               rich;
  stack_border       psb;
  int                pass;   // (the pass in which it was typeset)
  code_rec_rep (): wsig (0), psig (0), rich (false), pass (-1) {}
};

class code_rec {
  CONCRETE_NULL(code_rec);
  code_rec (bool make): rep (make? tm_new<code_rec_rep> (): NULL) {}
};
CONCRETE_NULL_CODE(code_rec);

// the paragraph of the first or of the last line, used again when the
// line did not change and there are no line items from around
struct code_end {
  box              b;  // the box of its first row (none: not kept)
  array<page_item> l;
  stack_border     sb;
};

class bridge_code_rep: public bridge_rep {
public:
  array<code_rec> recs;    // the records of the lines (see above)
  code_lexer      lexer;   // the colours of the syntax
  bool            recs_ok; // they are at the places of the lines
  code_end first, last;    // the paragraphs of the first and the last line
  int      passes;         // the passes of typeset_paper
  SI       gcell;          // the width of a cell of the grid,
  int      tabw;           // the tabs stop every tabw cells,
  bool     invis;          // the spaces, tabs... are shown (code-invisibles)
  bool     shrink;         // the objects taller than a row are scaled down
  SI       row_lo, row_hi; // to these (code-tall-objects: shrink)

  void lines_inserted (int pos, int nr);
  void lines_removed (int pos, int nr);
  void line_changed (int i);
  void lines_unknown () {
    recs_ok= false;
    if (!is_nil (lexer)) lexer->reset (); }
  code_rec line_rec (tree& t, font fn, SI width, SI cw,
                     unsigned long long wsig, bool always);
  void end_paragraph (code_end& e, array<box> rows, array<line_item> before,
                      array<line_item> after,
                      array<page_item>& l, stack_border& sb);
  void typeset_paper (tree doc, font fn, pencil pen, code_lexer lx,
                      hashmap<string,color> cols, SI width, SI cw,
                      unsigned long long wsig, SI cell, SI rpitch);
  void typeset_paragraphs (tree doc, array<array<box> > pars, SI width,
                           SI cell);
  void rich_paragraph (tree& t, path lip, SI width, SI cell,
                       array<line_item> before, array<line_item> after,
                       array<page_item>& l, stack_border& sb);
  box  rich_box (tree& t, path lip, SI width, SI cell);

public:
  bridge_code_rep (typesetter ttt, tree st, path ip);
  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  void notify_split  (path p);
  void notify_join   (path p);
  bool notify_macro (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_code_rep::bridge_code_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), recs_ok (false), passes (0), gcell (0),
  tabw (8), invis (false), shrink (false), row_lo (0), row_hi (0) {}

bridge
bridge_code (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_code_rep> (ttt, st, ip);
}

/******************************************************************************
* Notifications: the records follow the lines
******************************************************************************/

void
bridge_code_rep::lines_inserted (int pos, int nr) {
  if (!is_nil (lexer)) lexer->inserted (pos, nr);
  int n= N(recs);
  if (!recs_ok || pos < 0 || pos > n || nr < 0) { lines_unknown (); return; }
  array<code_rec> r (n + nr);
  for (int i=0; i<pos; i++) r[i]= recs[i];
  for (int i=pos; i<n; i++) r[i+nr]= recs[i];
  recs= r;
}

void
bridge_code_rep::lines_removed (int pos, int nr) {
  if (!is_nil (lexer)) lexer->removed (pos, nr);
  int n= N(recs);
  if (!recs_ok || pos < 0 || nr < 0 || pos + nr > n) {
    lines_unknown (); return; }
  array<code_rec> r (n - nr);
  for (int i=0; i<pos; i++) r[i]= recs[i];
  for (int i=pos+nr; i<n; i++) r[i-nr]= recs[i];
  recs= r;
}

void
bridge_code_rep::line_changed (int i) {
  if (!is_nil (lexer)) lexer->changed (i);
  if (!recs_ok || i < 0 || i >= N(recs)) { lines_unknown (); return; }
  recs[i]= code_rec ();
}

// The paths are relative to st= <code-lines|<document|line|...>>: (0, i) is
// the line i, (0, i, ...) is inside it. st is not changed here (see above),
// except when it is assigned as a whole
void
bridge_code_rep::notify_assign (path p, tree u) {
  status= CORRUPTED;
  if (N(p) >= 2 && p->item == 0) line_changed (p->next->item);
  else {
    st= substitute (st, p, u);
    lines_unknown ();
  }
}

void
bridge_code_rep::notify_insert (path p, tree u) {
  status= CORRUPTED;
  if (N(p) == 2 && p->item == 0 && is_compound (u))
    lines_inserted (p->next->item, N(u));
  else if (N(p) >= 3 && p->item == 0) line_changed (p->next->item);
  else lines_unknown ();
}

void
bridge_code_rep::notify_remove (path p, int nr) {
  status= CORRUPTED;
  if (N(p) == 2 && p->item == 0) lines_removed (p->next->item, nr);
  else if (N(p) >= 3 && p->item == 0) line_changed (p->next->item);
  else lines_unknown ();
}

void
bridge_code_rep::notify_split (path p) {
  // p= q * pos * at: the child pos of q split at at
  status= CORRUPTED;
  if (N(p) == 3 && p->item == 0) {
    int i= p->next->item;
    line_changed (i);
    lines_inserted (i+1, 1);
  }
  else if (N(p) > 3 && p->item == 0) line_changed (p->next->item);
  else lines_unknown ();
}

void
bridge_code_rep::notify_join (path p) {
  // p= q * pos: the children pos and pos+1 of q joined
  status= CORRUPTED;
  if (N(p) == 2 && p->item == 0) {
    int i= p->next->item;
    line_changed (i);
    lines_removed (i+1, 1);
  }
  else if (N(p) > 2 && p->item == 0) line_changed (p->next->item);
  else lines_unknown ();
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
  // Typeset again, the block makes again the lines which changed only, and
  // it has no labels nor entries of the auxiliary tables: an edit is not a
  // complete pass (which would also make the headers and the footers of all
  // the pages again), unless it is the first typesetting
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

code_rec
bridge_code_rep::line_rec (tree& t, font fn, SI width, SI cw,
                           unsigned long long wsig, bool always) {
  // a record of the line, made when it is needed (only for lines which may
  // be too wide, or with markup, unless always: a character is at most cw
  // wide, also <...>)
  bool plain= !rich_line (t);
  // (a tab, <#9>, is at most tabw cells wide)
  bool tabs= plain && search_forwards ("<#9>", t->label) >= 0;
  if (!always && plain && !tabs && (SI) N(t->label) * cw <= width)
    return code_rec ();
  code_rec r (true);
  code_records_made++;
  r->line= t;
  r->text= (is_atomic (t)? t->label: string (""));
  r->wsig= wsig;
  if (plain && (tabs || (SI) N(t->label) * cw > width))
    r->starts= code_breaks (t->label, fn, width, gcell, tabw);
  else r->starts << 0;
  return r;
}

static inline bool
same_rec (code_rec r, tree& t, unsigned long long wsig) {
  // (the line is the same tree, with the same string)
  return !is_nil (r) && r->wsig == wsig &&
         r->line.operator-> () == t.operator-> () &&
         (!is_atomic (t) ||
          r->text.operator-> () == t->label.operator-> ());
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_code_rep::my_typeset (int desired_status) {
  (void) desired_status;
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
  // the width of a cell of the grid (a character of the font) and the
  // distances between the lines and between the rows of a line (the lines
  // and the paragraphs of a document)
  SI cell= 0;
  {
    SI xp[5];
    fn->get_xpositions ("mmmm", xp);
    cell= xp[4] / 4;
    if (cell <= 0) cell= cw;
  }
  gcell= cell;
  // (the stops of the tabs of programs, see concater_rep::typeset_prog_tab)
  tabw= 8;
  if (env->provides (PROG_TAB_WIDTH)) {
    string tw= as_string (env->read (PROG_TAB_WIDTH));
    if (is_int (tw)) tabw= max (1, min (64, as_int (tw)));
  }
  invis= (env->provides ("code-invisibles") &&
          as_string (env->read ("code-invisibles")) == "true");
  shrink= (env->provides ("code-tall-objects") &&
           as_string (env->read ("code-tall-objects")) == "shrink");
  row_lo= fn->y1;
  row_hi= fn->y2;
  SI height= env->as_length (string ("1fn")) + env->get_length (PAR_SEP);
  SI pitch = height + env->get_vspace (PAR_PAR_SEP)->def;
  SI rpitch= height + env->get_vspace (PAR_LINE_SEP)->def;

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

  // the records of the lines, where the notifications left them: made again
  // for the lines which changed, or all when their places are not known
  unsigned long long wsig= sig_mix (sig_mix (0x2545F4914F6CDD1DULL,
                                             (unsigned long long) width),
                                    (unsigned long long)
                                    (void*) fn.operator-> ());
  wsig= sig_mix (wsig, (unsigned long long)
                       (tabw * 4 + (invis? 1: 0) + (shrink? 2: 0)));
  if (!recs_ok || N(recs) != n || typeset_caches_mode () == 0) {
    recs= array<code_rec> (n);
    recs_ok= true;
    if (!is_nil (lexer)) lexer->reset ();
  }
  // the lexer of the colours, for the language (see code_boxes.hpp)
  if (is_nil (lexer) || lexer->lan.operator-> () != env->lan.operator-> ())
    lexer= code_lexer (env->lan);
  lexer->doc= doc;
  if (typeset_caches_mode () == 0) lexer->reset ();
  if (typeset_caches_mode () == 2 && lexer->stateful) {
    // check: the states of the lines kept by the lexer, as they are made
    // from the first line
    int state= 0;
    for (int i=0; i<n; i++) {
      if (lexer->state_at (i) != state) {
        typeset_cache_mismatch ("states of the lines of code");
        lexer->reset ();
        break;
      }
      if (is_atomic (doc[i]) && N(doc[i]->label) > CODE_COLOR_MAX) continue;
      array<int> a, b;
      array<string> c;
      state= env->lan->colour_line (doc[i], state, a, b, c);
    }
  }
  bool paper= ttt->paper && n >= 4 && typeset_caches_mode () != 0;
  for (int i=0; i<n; i++) {
    tree& t= doc[i];
    if (same_rec (recs[i], t, wsig)) continue;
    recs[i]= line_rec (t, fn, width, cw, wsig, paper);
  }
  if (typeset_caches_mode () == 2) {
    // check: the records as they are made from the lines
    int made= code_records_made;
    for (int i=0; i<n; i++) {
      code_rec r= line_rec (doc[i], fn, width, cw, wsig, paper);
      bool same= (is_nil (r) == is_nil (recs[i]));
      if (same && !is_nil (r))
        same= recs[i]->line.operator-> () == doc[i].operator-> () &&
              recs[i]->starts == r->starts;
      if (!same) {
        typeset_cache_mismatch ("records of the lines of code");
        recs[i]= r;
      }
    }
    code_records_made= made; // (the count is the one without the check)
  }

  if (paper) {
    typeset_paper (doc, fn, env->pen, lexer, cols, width, cw, wsig,
                   cell, rpitch);
    return;
  }
  first.b= box ();
  last.b= box ();
  array<array<int> > one_row (1);
  one_row[0] << 0;
  array<array<box> > pars;
  if (ttt->paper)
    for (int i=0; i<n; i++) {
      if (rich_line (doc[i])) {
        pars << array<box> ();
        continue;
      }
      array<int> st= (is_nil (recs[i])? one_row[0]: recs[i]->starts);
      path lip= descend (descend (ip, 0), i);
      string text= (is_atomic (doc[i])? doc[i]->label: string (""));
      array<box> rows;
      for (int r=0; r<N(st); r++)
        rows << code_row_box (lip, doc[i], text, st[r],
                              r+1 < N(st)? st[r+1]: N(text),
                              fn, env->pen, lexer, cols, gcell, tabw,
                              invis? (i == n-1? 2: 1): 0, width);
      pars << rows;
    }
  else {
    // the rows of a line as the lines of a paragraph, the lines as the
    // paragraphs of a document; a line with markup is a box of its items
    array<int> tall_k;
    array<string> tall_txt;
    array<array<int> > tall_st;
    array<box> tall_box;
    array<int> tall_fold;
    for (int i=0; i<n; i++) {
      int fk= fold_kind (doc, i);
      if (fk == 1) {
        // (hidden: its text for the cursor, which may go there)
        tall_k << i;
        tall_txt << (is_atomic (doc[i])? doc[i]->label: string (""));
        tall_st << one_row[0];
        tall_box << box ();
        tall_fold << 1;
      }
      else if (rich_line (doc[i])) {
        tall_k << i;
        tall_txt << string ("");
        tall_st << one_row[0];
        tall_box << rich_box (doc[i], descend (descend (ip, 0), i), width,
                              cell);
        tall_fold << fk;
      }
      else if ((!is_nil (recs[i]) && N(recs[i]->starts) > 1) || fk == 2) {
        tall_k << i;
        tall_txt << doc[i]->label;
        tall_st << (is_nil (recs[i])? one_row[0]: recs[i]->starts);
        tall_box << box ();
        tall_fold << fk;
      }
    }
    array<box> one;
    one << code_box (ip, doc, 0, n, fn, env->pen, lexer, cols,
                     pitch, rpitch, gcell, tabw, invis,
                     tall_k, tall_txt, tall_st, tall_box, tall_fold, width);
    pars << one;
  }
  typeset_paragraphs (doc, pars, width, cell);
}

void
bridge_code_rep::typeset_paragraphs (tree doc, array<array<box> > pars,
                                     SI width, SI cell) {
  // each line as a paragraph of its rows (no rows: a line with markup, as
  // the paragraph of its items), with the line items of the bridges around
  // before the first and after the last
  int N1= N(pars) - 1;
  for (int i=0; i<N(pars); i++) {
    array<page_item> l;
    stack_border sb;
    array<line_item> before= (i == 0? ttt->a: array<line_item> ());
    array<line_item> after = (i == N1? ttt->b: array<line_item> ());
    if (N(pars[i]) == 0 && N(doc) > 1 && rich_line (doc[i]))
      rich_paragraph (doc[i], descend (descend (ip, 0), i), width, cell,
                      before, after, l, sb);
    else code_paragraph (env, ip, pars[i], before, after, l, sb);
    // (a line of its own: the folds, in paper mode)
    if (N(pars) == N(doc))
      fold_items (l, fold_kind (doc, i), descend (descend (ip, 0), i), env);
    ttt->insert_stack (l, sb);
  }
}

/******************************************************************************
* Paper mode: the lines which did not change are used again
******************************************************************************/

/******************************************************************************
* Lines with markup: the line items on the grid
*
* A line which is not a string is typeset by the concater (typeset_concat),
* and its line items are put on the grid of the cells, in order, and made a
* paragraph (lazy_paragraph: the decorations of the items, as an underline,
* are those of a paragraph). A string whose characters are a cell wide
* (strong, em, a colour, a macro which only changes them...) takes a cell for
* each character; a string in another font (a bold font which is wider) is
* split into its characters, each in its cell; any other box (math, an
* image...) takes the cells which it needs; the spaces after an item take
* theirs. An item is put at its cell by the space after the item before it
* (which cannot stretch), and the line is broken (next-line) before an item
* which does not fit. The items are the boxes of the concater, with their
* paths: the cursor and the edits go into them. The paragraph is the one of
* the line in paper mode, and its lines are the box of the line otherwise.
******************************************************************************/

void
bridge_code_rep::rich_paragraph (tree& t, path lip, SI width, SI cell,
                                 array<line_item> before,
                                 array<line_item> after,
                                 array<page_item>& l, stack_border& sb) {
  array<line_item> items= typeset_concat (env, t, lip);
  path eip= decorate_middle (lip);
  int cols= max (1, (int) (width / cell));
  array<line_item> a;
  SI x= 0;  // where the paragraph puts the next item
  int col= 0;
  auto put= [&] (line_item it, SI want) {
    if (N(a) > 0) a[N(a)-1]->spc= space (want - x);
    it->spc= space (0);
    if (it->type != CONTROL_ITEM) it->penalty= HYPH_INVALID;
    a << it;
    x= want + it->b->x2;
  };
  auto new_row= [&] () {
    put (line_item (CONTROL_ITEM, OP_SKIP, empty_box (eip), HYPH_INVALID,
                    tree (NEXT_LINE)), x);
    x= 0;
    col= 0;
  };
  for (int i=0; i<N(items); i++) {
    line_item it= items[i];
    SI sp= it->spc->def;
    box ib= it->b;
    // (code-tall-objects: shrink)
    auto fit= [&] (box b) -> box {
      return shrink? code_fit_box (b, row_lo, row_hi): b; };
    if (it->type == CONTROL_ITEM || it->type == FLOAT_ITEM ||
        it->type == NOTE_LINE_ITEM || it->type == NOTE_PAGE_ITEM ||
        it->type == MARKER_ITEM) {
      put (it, x);
    }
    else if (ib->get_type () == TEXT_BOX &&
             N(ib->get_leaf_string ()) > 0) {
      string str= ib->get_leaf_string ();
      int g= tm_string_length (str);
      SI d= ib->x2 - (SI) g * cell;
      if (col > 0 && col + g > cols) new_row ();
      if (d <= cell / 8 && d >= -cell / 8) put (it, (SI) col * cell);
      else {
        // its characters, each in its cell
        int pos= ib->get_leaf_left_pos (), k= 0;
        font f= ib->get_leaf_font ();
        pencil pn= ib->get_leaf_pencil ();
        for (int u=0; u<N(str); k++) {
          int v= u;
          tm_char_forwards (str, v);
          box cb= fit (text_box (ib->ip, pos + u, str (u, v), f, pn));
          put (line_item (STRING_ITEM, OP_TEXT, cb, HYPH_INVALID, it->lan),
               (SI) (col + k) * cell + (cell - cb->x2) / 2);
          u= v;
        }
      }
      col += g;
    }
    else {
      ib= it->b= fit (ib);
      SI w= ib->x2 - min (ib->x1, (SI) 0);
      int cells= (w <= 0? 0: (int) ((w + cell - 1) / cell));
      if (cells > 0 && col > 0 && col + cells > cols) new_row ();
      put (it, (SI) col * cell - min (ib->x1, (SI) 0));
      col += cells;
    }
    if (sp > 0) col += max (1, (int) ((sp + cell / 2) / cell));
  }
  lazy_paragraph par (env, ip);
  par->a= copy (before); // (<< below would append to 'before')
  par->a << a;
  par->a << after;
  par->format_paragraph ();
  l= par->sss->l;
  sb= par->sss->sb;
  code_rows_made += N(l);
}

box
bridge_code_rep::rich_box (tree& t, path lip, SI width, SI cell) {
  // the lines of the paragraph of the line, the first baseline at 0
  array<page_item> l;
  stack_border sb;
  rich_paragraph (t, lip, width, cell, array<line_item> (),
                  array<line_item> (), l, sb);
  array<box> bs;
  array<SI> xs, ys;
  SI y= 0;
  box prev;
  space prev_spc;
  for (int i=0; i<N(l); i++) {
    if (l[i]->type != PAGE_LINE_ITEM) continue;
    box b= l[i]->b;
    if (!is_nil (prev)) y -= -prev->y1 + prev_spc->def + b->y2;
    bs << b; xs << (SI) 0; ys << y;
    prev= b;
    prev_spc= l[i]->spc;
  }
  if (N(bs) == 0) {
    bs << empty_box (decorate_middle (lip), 0, env->fn->y1, 0, env->fn->y2);
    xs << (SI) 0; ys << (SI) 0;
  }
  return composite_box (decorate_middle (lip), bs, xs, ys);
}

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
bridge_code_rep::typeset_paper (tree doc, font fn, pencil pen, code_lexer lx,
                                hashmap<string,color> cols, SI width, SI cw,
                                unsigned long long wsig, SI cell,
                                SI rpitch) {
  int n= N(doc);
  path dip= descend (ip, 0);
  auto line_path= [&] (int i) -> path {
    // the path which the line keeps, renumbered in place by the editor
    path lip= obtain_ip (doc[i]);
    if (!ip_attached (lip) || is_nil (lip) || lip->item != i)
      lip= descend (dip, i);
    return lip;
  };
  auto make_rows= [&] (int i, path lip) -> array<box> {
    // the boxes of the rows of the line i (it has a record: see my_typeset)
    code_rec r= recs[i];
    if (rich_line (doc[i])) return array<box> (); // (see typeset_paragraphs)
    array<int> st= r->starts;
    int len= N(r->text);
    array<box> rows;
    for (int k=0; k<N(st); k++)
      rows << code_row_box (lip, doc[i], r->text, st[k],
                            k+1 < N(st)? st[k+1]: len,
                            fn, pen, lx, cols, gcell, tabw,
                            invis? (i == n-1? 2: 1): 0, width);
    code_rows_made += N(rows);
    return rows;
  };
  auto all_paragraphs= [&] () {
    array<array<box> > pars;
    for (int i=0; i<n; i++) pars << make_rows (i, line_path (i));
    typeset_paragraphs (doc, pars, width, cell);
  };
  // (folds: the lines as paragraphs, whose items are hidden or marked; the
  // folds are few and short-lived, the rows of the lines are still those
  // of the records)
  if (typeset_folds && code_folds_possible ())
    for (int i=0; i<n; i++)
      if (code_line_fold_state (doc[i]) > 0) {
        first.b= box ();
        last.b= box ();
        all_paragraphs ();
        return;
      }

  // Paragraphs of 1 to CODE_TPL rows (of the line 1): the rows are made as
  // these were, and stacked as they are, before a row of the same line,
  // and, for the last one, before another line (mid: a row stacked before a
  // paragraph). The breaks of pages after the first and the last two rows
  // of a paragraph have larger penalties (stacker_rep::new_paragraph): a
  // line of more rows has its first and last two as these of CODE_TPL rows,
  // and the others as the middle one
  #define CODE_TPL 5
  path lip1= line_path (1);
  string text1= recs[1]->text;
  path eip = decorate_middle (ip);
  array<array<page_item> > T (CODE_TPL + 1);
  array<stack_border> TS (CODE_TPL + 1);
  code_phrase ph_first, ph_rest;
  bool ok= true;
  for (int R=1; R<=CODE_TPL && ok; R++) {
    // (a box is placed in one phrase box only: its place is in the box)
    array<box> rs;
    for (int r=0; r<R; r++)
      rs << code_row_box (lip1, doc[1], text1, 0, N(text1), fn, pen, lx,
                          cols, gcell, tabw, invis, width);
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
    all_paragraphs ();
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
  sig= sig_mix (sig, wsig);
  sig= sig_mix (sig, (unsigned long long) pen->get_color ());
  sig= sig_mix (sig, (unsigned long long) pen->get_width ());
  sig= sig_mix (sig, (unsigned long long) (void*) lx.operator-> ());
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
  for (int R=1; R<=CODE_TPL; R++) {
    for (int r=0; r<R; r++) {
      sig= sig_space (sig, T[R][r]->spc);
      sig= sig_mix (sig, (unsigned long long) (long long) T[R][r]->penalty);
    }
    sig= sig_space (sig, mid[R]->spc);
    sig= sig_mix (sig, (unsigned long long) (long long) mid[R]->penalty);
  }
  if (sig == 0) sig= 1; // (0: the rows of a record were not made)

  passes++;
  // the rows of the line i, made again when the line or what they share
  // changed (or the path of the line: then it is not the one of the editor)
  auto get= [&] (int i) -> code_rec {
    code_rec r= recs[i];
    path lip= line_path (i);
    if (rich_line (doc[i])) {
      // a line with markup: typeset as a paragraph of its rows, at each
      // pass (its items depend on the environment and the macros)
      if (r->rich && r->pass == passes) return r;
      r->pass= passes;
      r->rich= true;
      r->psig= 0;
      r->lip = lip;
      r->rows= array<box> ();
      r->bs  = array<box> ();
      rich_paragraph (doc[i], lip, width, cell, array<line_item> (),
                      array<line_item> (), r->its, r->psb);
      return r;
    }
    if (r->psig == sig && N(r->rows) > 0 && r->lip == lip) return r;
    r->rich= false;
    r->psig= sig;
    r->lip = lip;
    r->rows= make_rows (i, lip);
    r->bs  = array<box> ();
    r->its = array<page_item> ();
    int R= N(r->rows);
    for (int k=0; k<R; k++) {
      box b= make_phrase (k == 0? ph_first: ph_rest, r->rows[k], eip);
      r->bs << b;
      page_item ri= row_item (R, k, false);
      r->its << page_item (tpl->type, b, ri->spc, ri->penalty,
                           tpl->fl, tpl->nr_cols, tpl->t);
    }
    return r;
  };

  // the first line, with the line items before, and the line 1
  array<page_item> l0;
  stack_border sb0;
  if (get (0)->rich) {
    first.b= box ();
    rich_paragraph (doc[0], line_path (0), width, cell, ttt->a,
                    array<line_item> (), l0, sb0);
  }
  else end_paragraph (first, get (0)->rows, ttt->a, array<line_item> (),
                      l0, sb0);
  ttt->insert_stack (l0, sb0);
  // The lines between: already stacked (merge_stack would only shove them
  // as the line 1 was: their borders and their heights are the same), but
  // for a line with markup and the line after it, which are stacked by
  // merge_stack, and the line before it and before the last, whose last
  // row is a line alone (merge_stack shoves it)
  bool merge= true; // (the line 1 is stacked after the paragraph of the first)
  for (int i=1; i<=n-2; i++) {
    code_rec r= get (i);
    if (r->rich) {
      ttt->insert_stack (r->its, r->psb);
      merge= true;
      continue;
    }
    bool alone= (i == n-2) || get (i+1)->rich;
    array<page_item> its= r->its;
    if (alone) {
      int R= N(its);
      its= copy (its);
      page_item ri= row_item (R, R-1, true);
      its[R-1]= page_item (tpl->type, r->bs[R-1], ri->spc, ri->penalty,
                           tpl->fl, tpl->nr_cols, tpl->t);
    }
    if (merge) ttt->insert_stack (its, sb1);
    else ttt->l << its;
    merge= false;
  }
  // the last line, with the line items after
  array<page_item> ln;
  stack_border sbn;
  if (get (n-1)->rich) {
    last.b= box ();
    rich_paragraph (doc[n-1], line_path (n-1), width, cell,
                    array<line_item> (), ttt->b, ln, sbn);
  }
  else end_paragraph (last, get (n-1)->rows, array<line_item> (), ttt->b,
                      ln, sbn);
  ttt->insert_stack (ln, sbn);

  if (typeset_caches_mode () == 2) {
    // check: every line as a paragraph, stacked one after the other
    array<page_item> fast_l= ttt->l;
    stack_border     fast_sb= ttt->sb;
    ttt->l = array<page_item> ();
    ttt->sb= stack_border ();
    int made= code_rows_made;
    all_paragraphs ();
    code_rows_made= made; // (the count is the one without the check)
    if (!same_code_stack (fast_l, ttt->l))
      typeset_cache_mismatch ("lines of code used again");
    ttt->l = fast_l;
    ttt->sb= fast_sb;
  }
}
