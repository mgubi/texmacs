
/******************************************************************************
* MODULE     : make_pages.cpp
* DESCRIPTION: Control routines for typesetting paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "page_type.hpp"
#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"
#include "pager.hpp"
#include "sys_utils.hpp" // get_env
#include "iterator.hpp"
box format_stack (path ip, array<box> bx, array<space> ht, SI height,
		  bool may_stretch);
#include "Boxes/construct.hpp"
array<page_item> sub (array<page_item> l, path p, path q);
page_item access (array<page_item> l, path p);
space as_space (tree t);
skeleton break_pages (array<page_item> l, space ph, int qual,
		      space fn_sep, space fnote_sep, space float_sep,
                      font fn, int first_page);
box page_box (path ip, box b, tree page, int page_nr, brush bgc,
              SI width, SI height, SI left, SI top,
	      SI bot, box header, box footer, SI head_sep, SI foot_sep);

box
pager_rep::pages_format (array<page_item> l, SI ht, SI tcor, SI bcor) {
  // cout << "Formatting insertion of height " << ht << LF;
  ht -= (tcor + bcor);
  int i, n= N(l);
  array<box>   bs;
  array<space> spc;
  for (i=0; i<n; i++) {
    page_item item= l[i];
    if (item->type == PAGE_CONTROL_ITEM) {
      if (is_tuple (item->t, "env_page")) {
	if (((item->t[1] == PAGE_THIS_HEADER) ||
	     (item->t[1] == PAGE_THIS_FOOTER)) &&
	    (item->t[2] == "")) style (item->t[1]->label)= " ";
	else if (item->t[1] == PAGE_NR)
	  page_offset= as_int (item->t[2]->label)- N(pages)- 1;
	else style (item->t[1]->label)= copy (item->t[2]);
      }
    }
    else {
      bs  << item->b;
      spc << item->spc;
    }
  }
  if (N(bs) == 0) {
    box b= empty_box (decorate_middle (ip), 0, -ht, 0, 0);
    return vcorrect_box (b->ip, b, tcor, bcor);
  }
  else {
    box b= format_stack (ip, bs, spc, ht, true);
    return vcorrect_box (b->ip, b, tcor, bcor);
  }
}

box
pager_rep::pages_format (insertion ins) {
  if (is_tuple (ins->type, "multi-column")) {
    // cout << "Format multicolumn " << ins->sk << LF << LF << INDENT;
    int col, nr_cols= N (ins->sk), real_nr_cols= as_int (ins->type[1]);
    SI w= (text_width + col_sep) / real_nr_cols;
    array<box> bs (nr_cols);
    array<SI>  x  (nr_cols);
    array<SI>  y  (nr_cols);
    for (col=0; col<nr_cols; col++) {
      bs[col]= pages_format (ins->sk[col]);
      x [col]= col * w;
      y [col]= 0;
    }
    // cout << UNINDENT << "Formatted multicolumn" << LF;
    return scatter_box (ip, bs, x, y);
  }
  else {
    array<page_item> sub_l= sub (l, ins->begin, ins->end);
    SI ht= stretch_space (ins->ht, ins->stretch);
    SI xh= stretch_space (ins->xh, ins->stretch);
    return pages_format (sub_l, ht, ins->top_cor, ins->bot_cor + xh);
  }
}

box
pager_rep::pages_format (pagelet pg) {
  // cout << "Formatting pagelet " << (N(pages)+1)
  //      << " stretch " << pg->stretch
  //      << " height " << stretch_space (pg->ht, pg->stretch) << LF << INDENT;
  if (N (pg->ins) == 0) {
    if (N(pages) == 0) return dummy_box (decorate_middle (ip));
    return dummy_box (decorate_middle (pages [N(pages)-1] -> find_rip ()));
  }
  else if (N (pg->ins) == 1) {
    insertion ins= pg->ins[0];
    // cout << ins << " stretch " << ins->stretch
    //      << " height " << stretch_space (ins->ht, ins->stretch) << LF;
    //      << UNINDENT << "Formatted pagelet " << (N(pages)+1) << LF << LF;
    return pages_format (ins);
  }
  else {
    int i, n= N(pg->ins);
    array<box> bs (n);
    array<SI>  bx (n);
    array<SI>  by (n);
    SI y= 0, fnote_y= MAX_SI;
    for (i=0; i<n; i++) {
      insertion ins= pg->ins[i];
      // cout << ins << " at " << y << " stretch " << ins->stretch
      //      << " height " << stretch_space (ins->ht, ins->stretch) << LF;
      bs[i]= pages_format (ins);
      bx[i]= 0;
      by[i]= y;
      y -= bs[i]->h();
      if (i < n-1) {
	insertion next= pg->ins[i+1];
	if (ins->type != next->type) {
	  if (is_tuple (next->type, "footnote")) {
	    // cout << "sep " << stretch_space (fnote_sep, ins->stretch) << LF;
	    y -= stretch_space (fnote_sep, ins->stretch);
	    fnote_y= y;
	  }
	  else if (is_tuple (ins->type, "float")) {
	    if (!is_tuple (next->type, "float")) {
	      // cout << "sep " << stretch_space(float_sep, ins->stretch) << LF;
	      y -= stretch_space (float_sep, ins->stretch);
	    }
	  }
	  else if (is_tuple (ins->type, "multi-column") ||
		   is_tuple (next->type, "multi-column")) {
	    page_item item= access (l, path_dec (ins->end));
	    // cout << "sep " << stretch_space (item->spc, ins->stretch) << LF;
	    y -= stretch_space (item->spc, ins->stretch);
	  }
	}
	if (is_tuple (ins->type, "footnote"))
	  if (is_tuple (next->type, "footnote")) {
	    page_item item= access (l, path_dec (ins->end));
	    SI sep= stretch_space (item->spc + fn_sep, ins->stretch);
	    // cout << "sep " << sep << LF;
	    y -= sep;
	  }
	if (is_tuple (next->type, "float")) {
	  // cout << "sep " << stretch_space (float_sep, ins->stretch) << LF;
	  y -= stretch_space (float_sep, ins->stretch);
	}
	if (is_tuple (ins->type, "if-page-break")) {
          // cout << "sep "
          // << stretch_space (as_space (ins->type[2]), ins->stretch) << LF;
          y -= stretch_space (as_space (ins->type[2]), ins->stretch);
	}
      }
    }
    if (fnote_y != MAX_SI) {
      pencil pen= env->pen->set_width (env->fn->wline);
      bs << line_box (decorate(), 0, 0, fnote_bl, 0, pen);
      bx << ((SI) 0);
      by << (fnote_y + env->fn->sep);
    }
    // cout << UNINDENT << "Formatted pagelet " << (N(pages)+1) << LF << LF;
    return scatter_box (ip, bs, bx, by);
  }
}

/******************************************************************************
* The pages of the last pass
*
* The pager puts all the pages together at each pass: the lines of each
* page are stacked and stretched to its height, and its header and footer
* are typeset. After an edit nearly all the pages are made of the same
* lines as before, and have the same header and footer: what was made in
* the last pass is kept by the typesetter (page_memo in pager.hpp) and
* used again, page by page.
*
* The text of a page is used again when it is made of the same boxes (the
* very same ones: a bridge which is used again gives the boxes it gave)
* with the same spaces, in insertions of the same kinds, heights and
* stretches (text_parts lists all of this, as pages_format uses it). A
* header or footer is used again when the tree to typeset is the same, the
* variables which its typesetting read have the values they had, and so
* have the references it looked up; one which reads something else (a
* Scheme routine, the date) is typeset again, and so are all of them in a
* complete pass, which collects the references which are used. Nothing is
* kept when a parameter of the pager changed, or a variable from which the
* environment derives a state which is not read as a variable (the font,
* the color...).
*
* The environment variable TEXMACS_PAGE_REUSE may be "off" (everything is
* made again) or "check" (made again too, and compared with what is kept).
******************************************************************************/

#define PART_READS_MAX   256
#define PART_LOOKUPS_MAX 64

// 0: nothing is used again, 1: used again, 2: made again and compared
static int
page_reuse_mode () {
  static int mode= -1;
  if (mode < 0) {
    string s= get_env ("TEXMACS_PAGE_REUSE");
    mode= (s == "off"? 0: (s == "check"? 2: 1));
  }
  return mode;
}

static bool
same_tree (tree t, tree u) {
  return strong_equal (t, u) || t == u;
}

static bool
same_box (box b1, box b2) {
  return b1->w () == b2->w () && b1->h () == b2->h () &&
         ((tree) b1) == ((tree) b2);
}

static bool
same_text (page_text& t1, page_text& t2) {
  if (N(t1.bs) != N(t2.bs) || N(t1.nums) != N(t2.nums) ||
      N(t1.ts) != N(t2.ts)) return false;
  for (int i=0; i<N(t1.bs); i++)
    if (t1.bs[i].operator -> () != t2.bs[i].operator -> ()) return false;
  for (int i=0; i<N(t1.nums); i++)
    if (t1.nums[i] != t2.nums[i]) return false;
  for (int i=0; i<N(t1.ts); i++)
    if (!same_tree (t1.ts[i], t2.ts[i])) return false;
  return true;
}

void
pager_rep::memo_start () {
  if (memo == NULL) return;
  if (page_reuse_mode () == 0) {
    // as before: nothing is kept, nor looked at
    *memo= page_memo ();
    memo= NULL;
    return;
  }
  array<double> pars;
  pars << (double) text_width << (double) text_height << (double) width
       << (double) height << (double) odd << (double) even << (double) top
       << (double) bot << (double) may_extend << (double) may_shrink
       << (double) head_sep << (double) foot_sep << (double) col_sep
       << (double) fn_sep->min << (double) fn_sep->def << (double) fn_sep->max
       << (double) fnote_sep->min << (double) fnote_sep->def
       << (double) fnote_sep->max << (double) fnote_bl
       << (double) float_sep->min << (double) float_sep->def
       << (double) float_sep->max << (double) mnote_sep
       << (double) (show_hf? 1: 0) << (double) (paper? 1: 0)
       << (double) page_offset << (double) env->dpi;
  // (not the zoom: a header which depends on it reads it, and the borders
  // of the pages, which are in pixels, are made outside: make_pages)
  unsigned long long state= env->derived_state ();
  bool same= N(pars) == N(memo->pars) && state == memo->state;
  for (int i=0; same && i<N(pars); i++) same= pars[i] == memo->pars[i];
  if (!same) {
    memo->pars   = pars;
    memo->state  = state;
    memo->headers= array<page_part> ();
    memo->footers= array<page_part> ();
    memo->texts  = array<page_text> ();
  }
}

// a header or a footer of the page which is being made
box
pager_rep::make_part (array<page_part>* parts, tree what) {
  if (parts == NULL)
    return typeset_as_concat (env, attach_here (tree (PARA, what),
                                                decorate ()));
  int nr= N(pages);
  while (N(*parts) <= nr) (*parts) << page_part ();
  page_part& old= (*parts) [nr];
  bool same= old.ok && page_reuse_mode () != 0 && !env->complete &&
             same_tree (old.what, what);
  for (int i=0; same && i<N(old.vars); i++)
    same= same_tree (env->read (old.vars[i]), old.values[i]);
  for (int i=0; same && i<N(old.keys); i++)
    same= env->lookup_value (old.keys[i]) == old.looked[i];
  if (same && page_reuse_mode () == 1) {
    if (edit_profile.on) edit_profile.parts_kept++;
    env->write_bindings (old.bindings);
    return old.b;
  }
  // typeset, with a record of what is read
  env_id_set reads;
  page_part now;
  hashmap<string,tree> before= env->all_values ();
  edit_env_rep::read_record record (env.operator-> (), &reads,
                                    &now.keys, &now.looked);
  edit_env_rep::binding_record bindings (env.operator-> (), &now.bindings);
  box b= typeset_as_concat (env, attach_here (tree (PARA, what),
                                               decorate ()));
  bindings.stop ();
  now.ok= record.complete () &&
          reads.size () <= PART_READS_MAX && N(now.keys) <= PART_LOOKUPS_MAX;
  record.stop ();
  bool differs= same && (!same_box (b, old.b) || now.bindings != old.bindings);
  if (differs) typeset_check_mismatches++;
  if (differs)
    cout << "TeXmacs] page " << (nr + 1)
         << ": the header or footer kept differs from the one made" << LF;
  if (now.ok) {
    for (int i=0; i<reads.size (); i++) {
      string var= env_var_name (reads.ids[i]);
      tree val= env->read (var);
      // a part which changes a variable it reads would be compared, the
      // next time, with a value which is not the one it started from
      if (!same_tree (val, before [var])) now.ok= false;
      now.vars << var;
      now.values << val;
    }
  }
  now.what= what;
  now.b= b;
  old= now;
  return b;
}

// what the text of a page is made of, in the order of pages_format; the
// control items are obeyed as they are there (once more does not matter)
void
pager_rep::text_parts (insertion ins, page_text& pt) {
  pt.ts << ins->type;
  pt.nums << (double) ins->ht->min << (double) ins->ht->def
          << (double) ins->ht->max << (double) ins->xh->min
          << (double) ins->xh->def << (double) ins->xh->max
          << ins->stretch << (double) ins->top_cor << (double) ins->bot_cor;
  if (is_tuple (ins->type, "multi-column")) {
    pt.nums << (double) N(ins->sk);
    for (int col=0; col<N(ins->sk); col++)
      text_parts (ins->sk[col], pt);
  }
  else {
    array<page_item> sub_l= sub (l, ins->begin, ins->end);
    pt.nums << (double) N(sub_l);
    for (int i=0; i<N(sub_l); i++) {
      page_item item= sub_l[i];
      if (item->type == PAGE_CONTROL_ITEM) {
        pt.ts << item->t;
        if (is_tuple (item->t, "env_page")) {
          if (((item->t[1] == PAGE_THIS_HEADER) ||
               (item->t[1] == PAGE_THIS_FOOTER)) &&
              (item->t[2] == "")) style (item->t[1]->label)= " ";
          else if (item->t[1] == PAGE_NR)
            page_offset= as_int (item->t[2]->label)- N(pages)- 1;
          else style (item->t[1]->label)= copy (item->t[2]);
        }
      }
      else {
        pt.bs << item->b;
        pt.nums << (double) item->spc->min << (double) item->spc->def
                << (double) item->spc->max;
      }
    }
  }
}

void
pager_rep::text_parts (pagelet pg, page_text& pt) {
  int n= N(pg->ins);
  pt.nums << (double) n << pg->stretch;
  // a pagelet without insertions takes the place of the last page
  if (n == 0) pt.ok= false;
  for (int i=0; i<n; i++) {
    insertion ins= pg->ins[i];
    text_parts (ins, pt);
    if (i < n-1) {
      // the space after the insertion, in the cases in which pages_format
      // takes it (the end of an empty insertion is no item)
      insertion next= pg->ins[i+1];
      bool fnote= is_tuple (ins->type, "footnote");
      bool nfnote= is_tuple (next->type, "footnote");
      bool mcol= ins->type != next->type && !nfnote &&
                 !is_tuple (ins->type, "float") &&
                 (is_tuple (ins->type, "multi-column") ||
                  is_tuple (next->type, "multi-column"));
      if (mcol || (fnote && nfnote)) {
        page_item item= access (l, path_dec (ins->end));
        pt.nums << (double) item->spc->min << (double) item->spc->def
                << (double) item->spc->max;
      }
    }
  }
}

box
pager_rep::pages_make_page (pagelet pg) {
  double old= env->magn_len;
  env->magn_len= 1.0;
  box sb;
  int nr_page= N(pages);
  page_text now;
  now.ok= true;
  if (memo != NULL) text_parts (pg, now);
  if (memo == NULL || !now.ok) {
    sb= pages_format (pg);
    if (memo != NULL && nr_page < N(memo->texts))
      memo->texts [nr_page]= page_text ();
  }
  else {
    // the text of the last pass, when it was made of the same boxes
    while (N(memo->texts) <= nr_page) memo->texts << page_text ();
    page_text& old_text= memo->texts [nr_page];
    bool same= old_text.ok && page_reuse_mode () != 0 &&
               same_text (old_text, now);
    if (same && page_reuse_mode () == 1) {
      sb= old_text.b;
      if (edit_profile.on) edit_profile.texts_kept++;
    }
    else {
      sb= pages_format (pg);
      if (same && !same_box (sb, old_text.b)) typeset_check_mismatches++;
      if (same && !same_box (sb, old_text.b))
        cout << "TeXmacs] page " << (nr_page + 1)
             << ": the text kept differs from the one made" << LF;
      now.b= sb;
      old_text= now;
    }
  }
  if (edit_profile.on) edit_profile.pages_made++;
  box lb= move_box (ip, sb, 0, 0);
  int nr= N(pages) + 1 + page_offset;
  SI  left= (nr&1)==0? even: odd;
  env->write (PAGE_NR, as_string (nr));
  env->write (PAGE_THE_PAGE, style[PAGE_THE_PAGE]);
  tree page_t= env->exec (compound (PAGE_THE_PAGE));
  bool empty= N (pg->ins) == 0;
  box header= make_header (empty);
  box footer= make_footer (empty);
  brush bgc = make_background (empty);
  adjust_margins (empty);
  box page= page_box (ip, lb, page_t, nr, bgc, width, height,
                      left, top + dtop, top + dtop + text_height,
                      header, footer, head_sep, foot_sep);
  if (env->get_string (PAGE_CROP_MARKS) == "") return page;
  bool ls= env->page_landscape;
  string sz= env->get_string (PAGE_CROP_MARKS);
  SI w= env->as_length (page_get_feature (sz, PAGE_WIDTH, ls));
  SI h= env->as_length (page_get_feature (sz, PAGE_HEIGHT, ls));
  SI lw= env->as_length ("0.2ln");
  SI ll= env->as_length ("1cm");
  env->magn_len= old;
  return crop_marks_box (ip, page, w, h, lw, ll);
}

void
pager_rep::pages_make () {
  space ht (text_height- may_shrink, text_height, text_height+ may_extend);
  skeleton sk=
    break_pages (l, ht, quality, fn_sep, fnote_sep, float_sep,
                 env->fn, env->first_page);
  // a document whose items are all hidden (or which has none) has no
  // pagelet: it is typeset as one empty page (in a skeleton of its own:
  // the one found may be shared with the cache of the page breaks)
  if (N(sk) == 0) {
    skeleton one;
    one << pagelet (ht);
    sk= one;
  }
  int i, n= N(sk);
  for (i=0; i<n; i++)
    pages << pages_make_page (sk[i]);
  if (memo != NULL) {
    // the pages which are no longer there are not kept
    if (N(memo->texts)   > n) memo->texts  = range (memo->texts, 0, n);
    if (N(memo->headers) > n) memo->headers= range (memo->headers, 0, n);
    if (N(memo->footers) > n) memo->footers= range (memo->footers, 0, n);
  }
}

void
pager_rep::papyrus_make () {
  space ht (MAX_SI >> 1);
  skeleton sk=
    break_pages (l, ht, quality, fn_sep, fnote_sep, float_sep,
                 env->fn, env->first_page);
  if (N(sk) == 0) {
    skeleton one;
    one << pagelet (ht);
    sk= one;
  }
  if (N(sk) != 1) {
    failed_error << "Number of pages: " << N(sk) << "\n";
    FAILED ("unexpected situation");
  }

  box sb= pages_format (sk[0]);
  box b = move_box (ip, sb, 0, 0);
  brush bgc = make_background (false);
  adjust_margins (false);
  SI ph= b->h();
  SI left  = (odd+even) >> 1;
  SI height= top + dtop + bot + dbot + ph;
  if (env->get_string (PAGE_MEDIUM) == "beamer")
    height= max (height, env->page_user_height + top + bot);
  array<box> bs   (1); bs   [0]= b;
  array<SI>  bs_x (1); bs_x [0]= left;
  array<SI>  bs_y (1); bs_y [0]= -top - dtop;
  box pb= page_box (ip, "?", 0, bgc, width, height,
		    bs, bs_x, bs_y, 0, 0, 0);
  pages << pb;
}
