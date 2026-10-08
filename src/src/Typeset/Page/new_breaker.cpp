
/******************************************************************************
* MODULE     : new_breaker.cpp
* DESCRIPTION: Page breaking
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "typesetter.hpp" // edit_profile
#include "sys_utils.hpp"  // get_env
#include "new_breaker.hpp"

/******************************************************************************
* Float placement subroutines
******************************************************************************/

bool
float_has (tree t, char c) {
  if (N(t) < 2 || t[0] != "float") return false;
  string s= as_string (t[1]);
  for (int i=0; i<N(s); i++)
    if (s[i] == c) return true;
  return false;
}

bool
float_here (tree t) {
  return !float_has (t, 't') && !float_has (t, 'b');
}

space
as_space (tree t) {
  if (is_func (t, TMLEN, 1))
    return space ((SI) (as_double (t[0]->label)));
  else if (is_func (t, TMLEN, 3)) {
    SI _min= (SI) as_double (t[0]->label);
    SI _def= (SI) as_double (t[1]->label);
    SI _max= (SI) as_double (t[2]->label);
    return space (_min, _def, _max);
  }
  else return space (0);
}

/******************************************************************************
* Constructor
******************************************************************************/

// TEXMACS_PAGE_BREAK_FAST: 0 for the search as it was, 1 to 4 for the
// changes of the faster search (the first three by default: the fourth
// gains little, see below), "check" to run both and report a difference
static int
breaker_fast_level () {
  static int level= -1;
  if (level < 0) {
    string s= get_env ("TEXMACS_PAGE_BREAK_FAST");
    level= (is_int (s) && as_int (s) >= 0 && as_int (s) <= 4) ? as_int (s) : 3;
  }
  return level;
}

new_breaker_rep::new_breaker_rep (
  array<page_item> l2, space ph, int quality2,
  space fn_sep2, space fnote_sep2, space float_sep2,
  font fn2, int fp2):
    l (l2), papyrus_mode (ph == (MAX_SI >> 1)), height (ph),
    fn_sep (fn_sep2), fnote_sep (fnote_sep2), float_sep (float_sep2),
    fn (fn2), first_page (fp2), quality (quality2), last_page_flag (true),
    body_ht (), body_cor (), foot_ht (), foot_tot (),
    float_ht (), float_tot (), ins_list (),
    best_prev (path (-1)), best_pens (vpenalty (MAX_INT)),
    todo_list (false), done_list (false),
    cache_uniform (array<path> ()),
    cache_colbreaks (array<path> ()),
    fast_level (quality2 > 1 ? breaker_fast_level () : 0),
    old (NULL), old_prefix (0), old_suffix (0)
{
  // HACK: migrate double column footnotes in single column text
  for (int i=0; i+1<N(l); i++)
    if (l[i]->nr_cols == 1 && N(l[i]->fl)>0) {
      array<lazy> ofl, nfl;
      for (int j=0; j<N(l[i]->fl); j++) {
        lazy_vstream lvs= (lazy_vstream) l[i]->fl[j];
        if (is_tuple (lvs->channel, "footnote") &&
            N(lvs->l) > 0 && lvs->l[0]->nr_cols == 2)
          nfl << l[i]->fl[j];
        else ofl << l[i]->fl[j];
      }
      if (N(nfl) != 0)
        for (int j=i+1; j<N(l); j++)
          if (l[j]->nr_cols == 2) {
            nfl << l[j]->fl;
            l[i]= copy (l[i]);
            l[j]= copy (l[j]);
            l[i]->fl= ofl;
            l[j]->fl= nfl;
            break;
          }
    }
  // END HACK

  int same= 0;
  for (int i=0; i<N(l); i++) {
    SI   bot_cor= max (l[i]->b->y1- fn->y1, 0);
    SI   bod_cor= l[i]->b->h ();
    SI   top_cor= max (fn->y2- l[i]->b->y2, 0);
    if (l[i]->type != PAGE_LINE_ITEM) bot_cor= bod_cor= top_cor= 0;
    body_ht  << (space (l[i]->b->h()) + l[i]->spc);
    body_cor << space (bot_cor, bod_cor, top_cor);
    body_tot << (i==0? space(0): body_tot[i-1]) + body_ht[i];
    if ((i==N(l)-1) || (l[i]->nr_cols!=l[i+1]->nr_cols)) l[i]->penalty=0;

    int k= N (l[i]->fl);
    space foot_spc (0);
    space float_spc (0);
    space wide_spc (0);
    space break_spc (0);
    array<insertion> ins_here;
    for (int j=0; j<k; j++) {
      lazy_vstream lvs= (lazy_vstream) l[i]->fl[j];
      insertion ins= make_insertion (lvs, path (i, j));
      ins_here << ins;
      //cout << i << ", " << j << ", " << ins->type
      //     << "; " << ins->nr_cols << ", " << l[i]->nr_cols << LF;
      if (ins->ht->def <= 0) continue;
      if (ins->nr_cols == 1 && l[i]->nr_cols > 1) {
        if (is_tuple (lvs->channel, "footnote"))
          wide_spc += ins->ht + fn_sep;
        else if (is_tuple (lvs->channel, "float"))
          wide_spc += ins->ht + float_sep;
        else if (is_tuple (lvs->channel, "if-page-break"))
          break_spc += ins->ht + as_space (lvs->channel[2]);
      }
      else {
        if (is_tuple (lvs->channel, "footnote"))
          foot_spc += ins->ht + fn_sep;
        else if (is_tuple (lvs->channel, "float")) {
          if (float_here (lvs->channel)) float_spc += ins->ht + 2*float_sep;
          else float_spc += ins->ht + float_sep;
        }
        else if (is_tuple (lvs->channel, "if-page-break"))
          break_spc += ins->ht + as_space (lvs->channel[2]);
      }
    }
    ins_list  << ins_here;
    foot_ht   << foot_spc;
    foot_tot  << (i==0? space(0): foot_tot[i-1]) + foot_ht[i];
    float_ht  << float_spc;
    float_tot << (i==0? space(0): float_tot[i-1]) + float_ht[i];
    wide_ht   << wide_spc;
    wide_tot  << (i==0? space(0): wide_tot[i-1]) + wide_ht[i];
    break_ht  << break_spc;

    if (i>0 && l[i]->nr_cols != l[i-1]->nr_cols) same= i;
    col_number << l[i]->nr_cols;
    col_same   << same;

    bool np= false, pb= false;
    if (!papyrus_mode && l[i]->type == PAGE_CONTROL_ITEM) {
      if (l[i]->t == PAGE_BREAK) pb= true;
      if (l[i]->t == NEW_PAGE || l[i]->t == NEW_DPAGE) np= pb= true;
    }
    must_new   << np;
    must_break << pb;
  }
  col_same   << same;
  must_new   << false;
  must_break << false;

  best_prev (path (0))= path (-2); 
  best_pens (path (0))= 0;
  if (fast_level > 0) {
    int m= N(l) + 1;
    has_a= array<bool> (m); done_a= array<bool> (m);
    pen_a= array<int> (m); exc_a= array<int> (m);
    prev_a= array<path> (m);
    for (int i=0; i<m; i++) {
      has_a[i]= done_a[i]= false; pen_a[i]= MAX_INT; exc_a[i]= 0; }
    has_a[0]= true; pen_a[0]= 0; prev_a[0]= path (-2);
    cand_off.assign (m, 0);
    cand_cnt.assign (m, 0);
  }
  //cout << HRULE;
}

/******************************************************************************
* Trivial layout of footnote and floating page insertions
******************************************************************************/

insertion
new_breaker_rep::make_insertion (lazy_vstream lvs, path p) {
  // FIXME: Very long insertions should themselves be page-broken
  
  path p1= p * 0;
  path p2= p * N(lvs->l);
  insertion ins (copy (lvs->channel), p1, p2);
  
  array<page_item> l= lvs->l;
  if (N(l) == 0) {
    ins->ht     = space (0);
    ins->top_cor= 0;
    ins->bot_cor= 0;
    ins->pen    = 0;
    ins->nr_cols= 1;
    return ins;
  }
  
  array<space> ins_ht;
  array<space> ins_cor;
  array<space> ins_tot;
  for (int i=0; i<N(l); i++) {
    SI   bot_cor= max (l[i]->b->y1- fn->y1, 0);
    SI   bod_cor= l[i]->b->h ();
    SI   top_cor= max (fn->y2- l[i]->b->y2, 0);
    if (l[i]->type != PAGE_LINE_ITEM) bot_cor= bod_cor= top_cor= 0;
    ins_ht  << (space (l[i]->b->h()) + l[i]->spc);
    ins_cor << space (bot_cor, bod_cor, top_cor);
    ins_tot << (i==0? space(0): ins_tot[i-1]) + ins_ht[i];
  }
  
  space spc;
  if (N(l) > 1) spc= copy (ins_tot[N(l)-2]);
  SI top_cor= ins_cor[0]->max;
  SI bot_cor= ins_cor[N(l)-1]->min;
  spc += space (top_cor + ins_cor[N(l)-1]->def + bot_cor);
  ins->ht     = spc;
  ins->top_cor= top_cor;
  ins->bot_cor= bot_cor;
  ins->pen    = 0;
  ins->nr_cols= l[0]->nr_cols;
  return ins;
}

/******************************************************************************
* Determination of the min and max spaces master routine
******************************************************************************/

space
new_breaker_rep::compute_space (path b1, path b2, bool wide_part) {
  //cout << "    Compute space " << b1 << ", " << b2 << ", " << wide_part << LF;
  int i1= b1->item, i2= b2->item;
  if (!is_nil (b1->next)) {
    if (b2 == b1) return space (0);
    path nx= b1->next;
    path q1= path (b1->item, nx->next->next);
    int  i0= nx->item;
    int  j0= nx->next->item;
    insertion fl= ins_list[i0][j0];
    space sep= float_sep;
    if (fl->ht->def <= 0) return compute_space (q1, b2, wide_part);
    if (float_here (fl->type)) sep= 2*sep;
    return fl->ht + sep + compute_space (q1, b2, wide_part);
  }
  if (!is_nil (b2->next))
    return compute_space (b1, path (b2->item), wide_part) -
           compute_space (b2, path (b2->item), wide_part);
  if (i1 == i2) return space (0);
  
  space spc;
  if (i1 == 0) { if (i2 > 1) spc= copy (body_tot[i2-2]); }
  else spc= body_tot[i2-2] - body_tot[i1-1];
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  spc += break_ht[i1];
  spc += space (top_cor + body_cor[i2-1]->def + bot_cor);

  if (!wide_part && foot_tot[i2-1]->def > (i1==0? 0: foot_tot[i1-1]->def)) {
    space foot_spc= foot_tot[i2-1] - (i1==0? space(0): foot_tot[i1-1]);
    foot_spc += fnote_sep - fn_sep;
    spc += foot_spc;
  }

  if (float_tot[i2-1]->def > (i1==0? 0: float_tot[i1-1]->def))
    spc += (float_tot[i2-1] - (i1==0? space(0): float_tot[i1-1]));

  //cout << "    Computed space " << b1 << ", " << b2 << " ~> " << spc << LF;
  return spc;
}

/******************************************************************************
* Find page breaks for a given start
******************************************************************************/

bool
new_breaker_rep::last_break (path b) {
  return last_page_flag && (b == path (N(l)) ||
                            (must_new[b->item] && is_nil (b->next)));
}

/******************************************************************************
* A faster search
*
* The search below is what it was, in the same order and with the same
* arithmetic, so that it finds the same breaks; four changes make it
* faster for the plain positions (a number of items, with no pending
* float), which are all of them in a document without floats:
*   1. their best previous break and penalty are in arrays (has_a, pen_a,
*      exc_a, prev_a, done_a) instead of tables indexed by paths;
*   2. no path is made for each candidate end of a page;
*   3. the height of a candidate page and its penalty are computed with
*      integers, instead of spaces and penalties allocated one by one;
*   4. the candidates of a start (what each adds to its penalty) depend
*      on the items from the start to the candidate only: those of the
*      previous search are used again for the starts whose items did not
*      change (breaker_history). Not in use by default.
* Measured on a document of 140 pages (6200 starts, 345000 candidates):
* the search takes 43 ms as it was, 30 to 37 ms with the first change,
* 17 to 25 ms with the second, 7 ms with the third and 5 ms with the
* fourth; with footnotes, floats and forced breaks (70 pages), 21 ms as
* it was, 3 ms with the third change and the same with the fourth.
* todo_list and done_list stay tables: the order in which the starts are
* tried is the order of their iteration, and the result may depend on it.
* The positions with pending floats, the pages with several columns and
* the lower qualities of page breaking use the search as it was.
******************************************************************************/

struct breaker_history {
  bool        used;
  array<SI>   nums;        // the signature of the items (breaker_signature)
  array<tree> trees;
  std::vector<int> num_off, tree_off;  // where each item starts in them
  int         n;           // number of items
  std::vector<breaker_cand> cands;
  std::vector<int>          cand_off, cand_cnt;
  breaker_history (): used (false), n (0) {}
};

bool
new_breaker_rep::has_best (path b) {
  if (is_plain (b)) return has_a[b->item];
  return best_pens->contains (b);
}

bool
new_breaker_rep::is_done (path b) {
  if (is_plain (b)) return done_a[b->item];
  return done_list->contains (b);
}

vpenalty
new_breaker_rep::get_pen (path b) {
  if (is_plain (b)) {
    int i= b->item;
    return has_a[i] ? vpenalty (pen_a[i], exc_a[i]) : vpenalty (MAX_INT);
  }
  return best_pens [b];
}

void
new_breaker_rep::set_best (path b, path prev, vpenalty pen) {
  if (is_plain (b)) {
    int i= b->item;
    has_a[i]= true; pen_a[i]= pen->pen; exc_a[i]= pen->exc; prev_a[i]= prev;
  }
  else {
    best_prev (b)= prev;
    best_pens (b)= pen;
  }
}

// the arrays into the tables, which the assembly of the skeleton reads
void
new_breaker_rep::export_tables () {
  if (fast_level == 0) return;
  for (int i=0; i<N(has_a); i++)
    if (has_a[i]) {
      best_prev (path (i))= prev_a[i];
      best_pens (path (i))= vpenalty (pen_a[i], exc_a[i]);
    }
}

// compute_space for two plain positions, with integers
void
new_breaker_rep::plain_space (int i1, int i2, SI& smin, SI& sdef, SI& smax) {
  smin= sdef= smax= 0;
  if (i1 == i2) return;
  if (i1 == 0) {
    if (i2 > 1) {
      smin= body_tot[i2-2]->min; sdef= body_tot[i2-2]->def; smax= body_tot[i2-2]->max; }
  }
  else {
    smin= body_tot[i2-2]->min - body_tot[i1-1]->min;
    sdef= body_tot[i2-2]->def - body_tot[i1-1]->def;
    smax= body_tot[i2-2]->max - body_tot[i1-1]->max;
  }
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  smin += break_ht[i1]->min; sdef += break_ht[i1]->def; smax += break_ht[i1]->max;
  SI cor= top_cor + body_cor[i2-1]->def + bot_cor;
  smin += cor; sdef += cor; smax += cor;
  if (foot_tot[i2-1]->def > (i1==0? 0: foot_tot[i1-1]->def)) {
    smin += foot_tot[i2-1]->min - (i1==0? 0: foot_tot[i1-1]->min) + fnote_sep->min - fn_sep->min;
    sdef += foot_tot[i2-1]->def - (i1==0? 0: foot_tot[i1-1]->def) + fnote_sep->def - fn_sep->def;
    smax += foot_tot[i2-1]->max - (i1==0? 0: foot_tot[i1-1]->max) + fnote_sep->max - fn_sep->max;
  }
  if (float_tot[i2-1]->def > (i1==0? 0: float_tot[i1-1]->def)) {
    smin += float_tot[i2-1]->min - (i1==0? 0: float_tot[i1-1]->min);
    sdef += float_tot[i2-1]->def - (i1==0? 0: float_tot[i1-1]->def);
    smax += float_tot[i2-1]->max - (i1==0? 0: float_tot[i1-1]->max);
  }
}

// as_vpenalty, its excentricity only (the main penalty is 0)
static inline int
excentricity (SI diff) {
  if (diff < 0) diff= -diff;
  if (diff < 0x1000) return (int) ((diff*diff) >> 16);
  else if (diff < 0x100000) return (int) ((diff >> 8) * (diff >> 8));
  else return 0x1000000;
}

// The candidates from the plain start b1, as long as no float comes; then
// the search goes on as it was (find_page_breaks_from)
void
new_breaker_rep::find_page_breaks_plain (path b1, path b1x, vpenalty prev_pen) {
  int n= N(l), s= b1->item, i1= b1x->item;
  bool ok= false, found_one= false;
  int ppen= prev_pen->pen, pexc= prev_pen->exc;
  SI hmin= height->min, hdef= height->def, hmax= height->max;

  // the candidates of this start in the previous search, when all the
  // items they depend on (from the start to the candidate) are the same
  const breaker_cand* memo= NULL;
  int memo_n= 0;
  bool keep= (fast_level >= 4);
  if (keep && old != NULL) {
    int t= s - (n - old->n);
    if (s <= old->n && s + old->cand_cnt[s] <= old_prefix - 1) {
      memo_n= old->cand_cnt[s];
      if (memo_n > 0) memo= &old->cands[old->cand_off[s]];
    }
    else if (t >= 0 && t <= old->n && s >= n - old_suffix) {
      memo_n= old->cand_cnt[t];
      if (memo_n > 0) memo= &old->cands[old->cand_off[t]];
    }
  }
  int first= (int) cands.size ();
  if (keep) cand_off[s]= first;
  if (memo_n > 0 && edit_profile.on) edit_profile.starts_kept++;

  int k= 0;
  for (int i= s; ; i++, k++) {
    int j= i + 1;
    breaker_cand c;
    if (k < memo_n) c= memo[k];
    else {
      if (i >= n) break;
      if (N(ins_list[i]) != 0 &&
          float_tot[i]->def > (i==0? 0: float_tot[i-1]->def)) {
        // a float: as before from here (the candidates so far are kept)
        if (keep) cand_cnt[s]= k;
        find_page_breaks_from (b1, b1x, prev_pen, path (i), ok, found_one);
        return;
      }
      bool break_page= must_break[j];
      int bpen= l[j-1]->penalty;
      if (j == n) bpen= 0;
      if (j == s) bpen= 0;
      if (break_page) bpen= 0;
      c.dpen= 0; c.dexc= 0; c.flags= break_page ? 4 : 0;
      if (bpen < HYPH_INVALID) {
        c.flags |= 1;
        c.dpen= bpen;
        SI smin, sdef, smax;
        bool single= (i1 >= j) || (col_same[j-1] <= i1 && col_number[i1] == 1);
        if (single && fast_level >= 3) plain_space (i1, j, smin, sdef, smax);
        else {
          // (several columns, or the changes 2 and 3 not in use)
          path b2 (j);
          space spc;
          if (has_columns (b1x, b2, 1)) spc= compute_space (b1x, b2);
          else {
            vpenalty mcpen;
            spc= compute_space (b1x, b2, mcpen);
            c.dpen += mcpen->pen; c.dexc += mcpen->exc;
            keep= false; // not known to depend on these items only
          }
          smin= spc->min; sdef= spc->def; smax= spc->max;
        }
        bool last= last_page_flag && (j == n || must_new[j]);
        if (!last) c.dexc += excentricity (sdef - hdef);
        if (!last && !break_page && smax < hdef) {
          if (smax >= hmin) c.dpen += EXTEND_PAGE_PENALTY;
          else {
            double factor=
              ((double) max (sdef, 1))/((double) max (hdef, 1));
            if (factor < 0.0 ) factor= 0.0;
            if (factor > 0.99) factor= 0.99;
            c.dpen += (int) ((1.0 - factor) * TOO_SHORT_PENALTY);
          }
        }
        else if (smin > hdef) {
          if (smin <= hmax) c.dpen += REDUCE_PAGE_PENALTY;
          else {
            double factor=
              ((double) max (sdef, 1))/((double) max (hdef, 1));
            if (factor < 1.0  ) factor= 1.0;
            if (factor > 100.0) factor= 100.0;
            c.dpen += (int) (factor * TOO_LONG_PENALTY);
          }
        }
        if (smin > hmax) c.flags |= 2;
      }
    }
    if (keep) cands.push_back (c);

    if (c.flags & 1) {
      ok= true;
      int pen= ppen + c.dpen, exc= pexc + c.dexc;
      if (!has_a[j] && !done_a[j]) todo_list (path (j))= true;
      int bpen= has_a[j] ? pen_a[j] : MAX_INT;
      int bexc= has_a[j] ? exc_a[j] : 0;
      if (pen < bpen || (pen == bpen && exc < bexc)) {
        has_a[j]= true; pen_a[j]= pen; exc_a[j]= exc; prev_a[j]= b1; }
      if (has_a[j]) found_one= true;
    }
    if (!found_one) continue;
    if (ok && (c.flags & 2)) { k++; break; }
    if (c.flags & 4) { k++; break; }
  }
  if (keep) cand_cnt[s]= k;
  else if (fast_level >= 4) {
    cands.resize (first);
    cand_cnt[s]= 0;
  }
}

void
new_breaker_rep::find_page_breaks (path b1) {
  path b1x= b1;
  if (must_break[b1x->item] && b1x->item < N(l))
    b1x= path (b1x->item + 1, b1x->next);
  //cout << "Find page breaks " << b1 << LF;
  vpenalty prev_pen= get_pen (b1);
  if (fast_level >= 2 && is_nil (b1->next) && height->def < (1 << 28))
    find_page_breaks_plain (b1, b1x, prev_pen);
  else find_page_breaks_from (b1, b1x, prev_pen, b1, false, false);
}

void
new_breaker_rep::find_page_breaks_from (path b1, path b1x, vpenalty prev_pen,
                                        path b2, bool ok, bool found_one) {
  int n= N(l);
  int float_status= 0;
  path floats;
  while (true) {
    if (height->def >= (1 << 28) && b2->item < n)
      b2= path (n);
    else if (!is_nil (b2->next))
      b2= path (b2->item, b2->next->next->next);
    else if (b2->item >= n)
      break;
    else {
      int i= b2->item;
      if (N(ins_list[i]) != 0 &&
          float_tot[i]->def > (i==0? 0: float_tot[i-1]->def)) {
        for (int j=0; j<N(ins_list[i]); j++) {
          insertion ins= ins_list[i][j];
          if (is_tuple (ins->type, "float")) {
            if (ins->nr_cols == 1 && l[i]->nr_cols > 1);
            else if (float_has (ins->type, 'f')) {
              while (!is_nil (floats)) {
                int i2= floats->item;
                int j2= floats->next->item;
                floats= floats->next->next;
                insertion ins2= ins_list[i2][j2];
                if (float_status == 0 && !float_has (ins2->type, 't'))
                  float_status= 1;
                if (float_status == 1 && !float_has (ins2->type, 'h'))
                  float_status= 2;
                if (float_status == 2 && !float_has (ins2->type, 'b'))
                  float_status= 3;
              }
            }
            else floats= floats * path (i, j);
          }
        }
      }
      b2= path (i+1, floats);
    }
    if (b2->item > n) break;
    bool break_page= must_break[b2->item];
    if (must_new[b2->item]) b2= path (b2->item);
    
    space spc;
    int bpen= l[b2->item - 1]->penalty;
    if (b2->item == n && is_nil (b2->next)) bpen= 0;
    if (b2->item == b1->item) bpen= 0;
    if (break_page) bpen= 0;
    if (float_status == 3) {
      if (ok) break;
      else bpen += BAD_FLOATS_PENALTY;
    }
    if (bpen < HYPH_INVALID) {
      ok= true;
      vpenalty pen= prev_pen + vpenalty (bpen);
      if (has_columns (b1x, b2, 1))
        spc= compute_space (b1x, b2);
      else {
        vpenalty mcpen;
        spc= compute_space (b1x, b2, mcpen);
        //cout << "Space " << b1x << ", " << b2
        //     << " ~> " << spc << ", " << mcpen << LF;
        pen += mcpen;
      }
      if (!last_break (b2))
	pen += as_vpenalty (spc->def - height->def);
      if (!last_break (b2) && !break_page && spc->max < height->def) {
	if (spc->max >= height->min) pen += EXTEND_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 0.0 ) factor= 0.0;
	  if (factor > 0.99) factor= 0.99;
	  pen += vpenalty ((int) ((1.0 - factor) * TOO_SHORT_PENALTY));
	}
      }
      else if (spc->min > height->def) {
	if (spc->min <= height->max) pen += REDUCE_PAGE_PENALTY;
	else {
	  double factor=
	    ((double) max (spc->def, 1))/((double) max (height->def, 1));
	  if (factor < 1.0  ) factor= 1.0;
	  if (factor > 100.0) factor= 100.0;
	  pen += vpenalty ((int) (factor * TOO_LONG_PENALTY));
	}
      }
      if (!has_best (b2) && !is_done (b2))
        todo_list (b2)= true;
      if (pen < get_pen (b2)) {
        //cout << b1 << ", " << b2 << " ~> " << pen << "\n";
        set_best (b2, b1, pen);
      }
      if (has_best (b2)) found_one= true;
    }
    if (!found_one) continue;
    if (ok && spc->min > height->max && is_nil (b2->next)) break;
    if (break_page && is_nil (b2->next)) break;
  }
}

/******************************************************************************
* Master routine for finding all page breaks
******************************************************************************/

void
new_breaker_rep::find_page_breaks () {
  //cout << "Find page breaks" << LF;
  //for (int i=0; i<N(l); i++)
  //  cout << "  " << i << ": \t" << l[i]
  //       << ", " << body_ht[i]
  //       << ", " << body_cor[i] << ", " << body_tot[i] << LF;
  todo_list (path (0))= true;  
  while (N(todo_list) != 0) {
    hashmap<path,bool> temp_list= todo_list;
    todo_list= hashmap<path,bool> (false);
    done_list->join (temp_list);
    if (fast_level > 0)
      for (iterator<path> it= iterate (temp_list); it->busy (); ) {
        path p= it->next ();
        if (is_nil (p->next)) done_a[p->item]= true;
      }
    if (quality>1) {
      for (iterator<path> it= iterate (temp_list); it->busy (); )
        find_page_breaks (it->next ());
    }
    else {
      path best_start;
      vpenalty best_pen= HYPH_INVALID;
      for (iterator<path> it= iterate (temp_list); it->busy (); ) {
        path here= it->next ();
        if (is_nil (best_start) || best_pens[here] < best_pen) {
          best_start= here;
          best_pen= best_pens[here];
        }
      }
      if (best_start == path (N(l))) break;
      find_page_breaks (best_start);
      while (N(todo_list) == 0 && !best_prev->contains (N(l))) {
        // Fix for bug #62844
        path best (0);
        for (iterator<path> it= iterate (best_prev); it->busy (); ) {
          path next= it->next ();
          if (path_inf (best, next))
            if (!done_list->contains (next) ||
                (temp_list->contains (next) && next != best_start))
              best= next;
        }
        find_page_breaks (best);
      }
    }
  }
  //cout << "Found page breaks" << LF;
}

/******************************************************************************
* Formatting pagelets
******************************************************************************/

vpenalty
new_breaker_rep::format_insertion (insertion& ins, double stretch) {
  // cout << "Stretch " << ins << ": " << stretch << LF;
  ins->stretch= stretch;
  skeleton sk = ins->sk;
  if (N(sk) == 0) return vpenalty ();

  int i, k=N(sk);
  vpenalty pen;
  SI ht= stretch_space (ins->ht, stretch);
  // cout << "Formatting multicolumn " << ins->ht
  //      << " stretch " << stretch
  //      << " -> height " << ht << LF << INDENT;
  for (i=0; i<k; i++) {
    pagelet& pg= sk[i];
    // cout << i << ": " << pg->ht;
    double pg_stretch= 0.0;
    if (ht > pg->ht->max) pg_stretch= 1.0;
    else if (ht < pg->ht->min) pg_stretch= -1.0;
    else if ((ht > pg->ht->def) && (pg->ht->max > pg->ht->def))
      pg_stretch=
	((double) (ht - pg->ht->def)) /
	((double) (pg->ht->max - pg->ht->def));
    else if ((ht < pg->ht->def) && (pg->ht->def > pg->ht->min))
      pg_stretch=
	((double) (ht - pg->ht->def)) /
	((double) (pg->ht->def - pg->ht->min));
    // cout << " -> " << pg_stretch << LF;
    pen += format_pagelet (pg, pg_stretch);
    pen += pg->pen + as_vpenalty (pg->ht->def - ht);
  }
  // cout << UNINDENT << "Formatted multicolumn, penalty= " << pen << LF;
  return pen;
}

vpenalty
new_breaker_rep::format_pagelet (pagelet& pg, double stretch) {
  // cout << "Stretch " << pg << ": " << stretch << LF;
  int i;
  vpenalty pen;
  pg->stretch= stretch;
  for (i=0; i<N(pg->ins); i++)
    pen += format_insertion (pg->ins[i], stretch);
  return pen;
}

vpenalty
new_breaker_rep::format_pagelet (pagelet& pg, space ht, bool last_page) {
  // cout << "Formatting " << pg << ", " << ht << LF << INDENT;
  float stretch= 0.0;
  vpenalty pen;

  if (last_page && (pg->ht->def <= ht->def)) {
    // cout << "Eject last page" << LF;
    stretch= 0.0;
  }
  else if ((ht->def >= pg->ht->min) && (ht->def <= pg->ht->max)) {
    if (ht->def > pg->ht->def) {
      // cout << "Stretch" << LF;
      stretch=
	((double) (ht->def - pg->ht->def)) /
	((double) (pg->ht->max - pg->ht->def));
    }
    else if (ht->def < pg->ht->def) {
      // cout << "Shrink" << LF;
      stretch=
	((double) (ht->def - pg->ht->def)) /
	((double) (pg->ht->def - pg->ht->min));
    }
    pen= as_vpenalty (ht->def- pg->ht->def);
  }
  else if ((ht->def < pg->ht->min) && (ht->max >= pg->ht->min)) {
    // cout << "Extend page" << LF;
    stretch= -1.0;
    pen= vpenalty (EXTEND_PAGE_PENALTY) + as_vpenalty (ht->def- pg->ht->def);
  }
  else if ((ht->def > pg->ht->max) && (ht->min <= pg->ht->max)) {
    // cout << "Reduce page" << LF;
    stretch= 1.0;
    pen= vpenalty (REDUCE_PAGE_PENALTY) + as_vpenalty (ht->def- pg->ht->def);
  }
  else if (ht->max < pg->ht->min) {
    // cout << "Overfull page" << LF;
    stretch= -1.0;
    double factor= ((double) max (pg->ht->def, 1))/((double) max (ht->def, 1));
    if (factor < 1.0  ) factor= 1.0;
    if (factor > 100.0) factor= 100.0;
    pen= vpenalty ((int) (factor * TOO_LONG_PENALTY));
  }
  else {
    // cout << "Underfull page" << LF;
    stretch= 1.0;
    double factor= ((double) max (pg->ht->def, 1))/((double) max (ht->def, 1));
    if (factor < 0.0 ) factor= 0.0;
    if (factor > 0.99) factor= 0.99;
    pen= vpenalty ((int) ((1.0 - factor) * TOO_SHORT_PENALTY));
  }
  pen += format_pagelet (pg, stretch);
  // cout << UNINDENT << "Formatted [ stretch= " << stretch
  //      << ", penalty= " << (pg->pen + pen) << " ]" << LF << LF;
  return pg->pen + pen;
}

/******************************************************************************
* Assembling the skeleton
******************************************************************************/

insertion
new_breaker_rep::make_insertion (int i1, int i2) {
  //cout << "Make insertion " << i1 << ", " << i2 << LF;
  path p1= i1;
  path p2= i2;
  insertion ins ("", p1, p2);  
  space spc;
  if (i1 == 0) { if (i2 >= 2) spc= copy (body_tot[i2-2]); }
  else { spc= body_tot[i2-2] - body_tot[i1-1]; }
  SI top_cor= body_cor[i1]->max;
  SI bot_cor= body_cor[i2-1]->min;
  spc += space (top_cor + body_cor[i2-1]->def + bot_cor);
  ins->ht     = spc;
  ins->top_cor= top_cor;
  ins->bot_cor= bot_cor;
  ins->pen    = best_pens [path (i2)];
  return ins;
}

bool
new_breaker_rep::here_floats (path p) {
  bool here_flag= true;
  while (!is_nil (p)) {
    int i= p->item, j= p->next->item;
    p= p->next->next;
    if (!float_has (ins_list[i][j]->type, 'h')) here_flag= false;
  }
  return here_flag;
}

static path
filter_subsequent (path floats, path avoid) {
  if (is_nil (floats) || is_nil (avoid)) return floats;
  if (floats->item == avoid->item && floats->next->item == avoid->next->item)
    return filter_subsequent (floats->next->next, avoid->next->next);
  return path (floats->item, floats->next->item) *
         filter_subsequent (floats->next->next, avoid);
}

void
new_breaker_rep::lengthen_previous (pagelet& pg, int pos, space done) {
  if (pos == 0 || N(pg->ins) == 0) return;
  space spc= body_ht[pos-1] - l[pos-1]->b->h();
  spc= max (spc - done, space (0));
  pg->ins[N(pg->ins)-1]->xh += spc;
  pg->ht += spc;
}

pagelet
new_breaker_rep::assemble (path start, path end, bool wide_part) {
  //cout << "Assemble " << start << ", " << end << INDENT << LF;
  // Position the floats
  path floats, avoid= end->next;
  for (int i=start->item; i<end->item; i++)
    for (int j=0; j<N(ins_list[i]); j++)
      if (is_tuple (ins_list[i][j]->type, "float")) {
        if (ins_list[i][j]->nr_cols == 1 && l[i]->nr_cols > 1);
        else if (!is_nil (avoid) && avoid->item == i && avoid->next->item == j)
          avoid= avoid->next->next;
        else floats= floats * path (i, j);
      }
  path top= filter_subsequent (start->next, avoid), here, bottom;
  while (!is_nil (floats)) {
    bool ok= false;
    int i= floats->item, j= floats->next->item;
    if (float_has (ins_list[i][j]->type, 't')) {
      top= top * path (i, j);
      floats= floats->next->next;
      if (is_nil (floats)) break;
      ok= true;
    }
    if (N(top) + N(bottom) >= (2 * N(floats)) && here_floats (floats)) {
      here= floats;
      break;
    }
    int num= N(floats);
    i= floats[num-2]; j= floats[num-1];
    if (float_has (ins_list[i][j]->type, 'b')) {
      bottom= path (i, j) * bottom;
      floats= path_up (floats, 2);
      if (is_nil (floats)) break;
      ok= true;
    }
    if (N(top) + N(bottom) >= (2 * N(floats)) && here_floats (floats)) {
      here= floats;
      break;
    }
    if (!ok) {
      here= floats;
      break;
    }
  }

  // Add the page
  pagelet pg (0);
  while (!is_nil (top)) {
    int i= top->item, j= top->next->item;
    top= top->next->next;
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
    }
  }
  int pos= start->item;
  if (pos < N(ins_list))
    for (int j=0; j<N(ins_list[pos]); j++)
      if (is_tuple (ins_list[pos][j]->type, "if-page-break")) {
        pg << ins_list[pos][j];
        pg << as_space (ins_list[pos][j]->type[2]);
      }
  bool several= false;
  space done= space (0);
  while (!is_nil (here)) {
    int i= here->item, j= here->next->item;
    here= here->next->next;
    if (i >= end->item && i > pos) {
      if (several) lengthen_previous (pg, pos, done);
      insertion ins= make_insertion (pos, i);
      pg << ins;
      if (ins_list[i][j]->ht->def > 0) pg << float_sep;
      pos= i;
      several= true;
    }
    else if (i+1 > pos) {
      if (several) lengthen_previous (pg, pos, done);
      insertion ins= make_insertion (pos, i+1);
      pg << ins;
      if (ins_list[i][j]->ht->def > 0) pg << float_sep;
      pos= i+1;
      several= true;
    }
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
      done= float_sep;
    }
    else done= space (0);
  }
  if (end->item > pos) {
    if (several) lengthen_previous (pg, pos, done);
    insertion ins= make_insertion (pos, end->item);
    pg << ins;
  }
  while (!is_nil (bottom)) {
    int i= bottom->item, j= bottom->next->item;
    bottom= bottom->next->next;
    if (ins_list[i][j]->ht->def > 0) {
      pg << ins_list[i][j];
      pg << float_sep;
    }
  }
  bool has_footnotes= false;
  for (int i=start->item; i<end->item; i++)
    for (int j=0; j<N(ins_list[i]); j++)
      if (is_tuple (ins_list[i][j]->type, "footnote"))
        if (ins_list[i][j]->nr_cols != 1 || (!wide_part && l[i]->nr_cols == 1)) {
          if (ins_list[i][j]->ht->def > 0) {
            pg << ins_list[i][j];
            if (has_footnotes) pg << fn_sep;
            else pg << fnote_sep;
            has_footnotes= true;
          }
        }
  //cout << UNINDENT << "Assembled " << start << ", " << end << LF;
  return pg;
}

void
new_breaker_rep::assemble_skeleton (skeleton& sk, path end, int& offset) {
  path start= best_prev [end];
  //cout << "Assemble skeleton " << start << " -- " << end << LF;
  if (start->item < 0) return;
  assemble_skeleton (sk, start, offset);
  path begin= start;
  if (must_break[begin->item] && begin->item < end->item) {
    if (l[begin->item]->t == NEW_DPAGE)
      if (((N(sk) + offset) & 1) == 1)
        sk << pagelet (space (0));
    begin= path (begin->item + 1, begin->next);
  }
  //cout << "Assemble page " << begin << ", " << end << "; " << offset
  //     << INDENT << LF;
  if (has_columns (begin, end, 1)) {
    pagelet pg= assemble (begin, end);
    bool last_page= last_break (end);
    format_pagelet (pg, height, last_page);
    sk << pg;
  }
  else {
    pagelet pg= assemble_multi_columns (begin, end);
    bool last_page= last_break (end);
    format_pagelet (pg, height, last_page);
    sk << pg;
  }
  for (int i=begin->item; i<end->item; i++)
    if (is_tuple (l[i]->t, "env_page") && l[i]->t[1] == PAGE_NR)
      offset= as_int (l[i]->t[2]->label) - N(sk);
  //cout << UNINDENT << "Assembled page " << begin << ", " << end << LF;
}

/******************************************************************************
* The exported page breaking routine
******************************************************************************/

/******************************************************************************
* The page breaks of the last documents are kept
*
* The search of the page breaks (find_page_breaks) tries every line as the
* start of a page: 40 ms for 140 pages, at every typesetting pass, that is
* at every keystroke. Its result is a function of what it reads: for every
* page item the height and the vertical extents of its box, its space, its
* penalty, its type, its number of columns, its control tree and its
* floating objects (their channel and their items, in the same way), and
* the parameters of the breaker. These numbers and trees are the signature
* of a call; most edits do not change them (a character typed in a line
* makes new boxes of the same heights). The skeletons made for the last
* few signatures are kept and returned for an equal signature: a skeleton
* holds no box, only the positions of the items of each page with their
* heights and stretch, and the pager, which fills the pages with the
* current items, only reads it.
*
* The environment variable TEXMACS_PAGE_BREAK_CACHE may be "off" (always
* search) or "check" (search too, and report a difference).
******************************************************************************/

static void
breaker_signature (array<page_item> l, array<SI>& nums, array<tree>& trees,
                   std::vector<int>* num_off= NULL,
                   std::vector<int>* tree_off= NULL) {
  // num_off, tree_off: where each item starts (and where the last one ends)
  nums << (SI) N(l);
  for (int i=0; i<N(l); i++) {
    page_item_rep* it= l[i].operator -> ();
    if (num_off != NULL) {
      num_off->push_back (N(nums)); tree_off->push_back (N(trees)); }
    nums << (SI) it->type << (SI) it->b->h () << (SI) it->b->y1 << (SI) it->b->y2
         << (SI) it->spc->min << (SI) it->spc->def << (SI) it->spc->max
         << (SI) it->penalty << (SI) it->nr_cols << (SI) N(it->fl);
    trees << it->t;
    for (int j=0; j<N(it->fl); j++) {
      lazy_vstream lvs= (lazy_vstream) it->fl[j];
      trees << lvs->channel;
      breaker_signature (lvs->l, nums, trees);
    }
  }
  if (num_off != NULL) {
    num_off->push_back (N(nums)); tree_off->push_back (N(trees)); }
}

// the previous search (one is kept), and whether the item i of a new
// signature is the item j of it
static breaker_history breaker_last;
#define BREAKER_PARAMS 16 // the parameters of the breaker, before the items

static bool
same_item (breaker_history& h, int j, array<SI>& nums, array<tree>& trees,
           std::vector<int>& num_off, std::vector<int>& tree_off, int i) {
  int a1= h.num_off[j], a2= h.num_off[j+1], b1= num_off[i], b2= num_off[i+1];
  if (a2 - a1 != b2 - b1) return false;
  for (int k=0; k<a2-a1; k++)
    if (h.nums[a1+k] != nums[b1+k]) return false;
  int c1= h.tree_off[j], c2= h.tree_off[j+1], d1= tree_off[i], d2= tree_off[i+1];
  if (c2 - c1 != d2 - d1) return false;
  for (int k=0; k<c2-c1; k++)
    if (inside (h.trees[c1+k]) != inside (trees[d1+k]) &&
        h.trees[c1+k] != trees[d1+k]) return false;
  return true;
}

static bool
same_signature (array<SI> n1, array<tree> t1, array<SI> n2, array<tree> t2) {
  if (N(n1) != N(n2) || N(t1) != N(t2)) return false;
  for (int i=0; i<N(n1); i++)
    if (n1[i] != n2[i]) return false;
  for (int i=0; i<N(t1); i++)
    if (inside (t1[i]) != inside (t2[i]) && t1[i] != t2[i]) return false;
  return true;
}

struct breaker_memo {
  bool        used;
  array<SI>   nums;
  array<tree> trees;
  skeleton    sk;
  breaker_memo (): used (false) {}
};

#define BREAKER_MEMOS 4
static breaker_memo breaker_memos[BREAKER_MEMOS];
static int breaker_memo_next= 0;

static skeleton
search_page_breaks (array<page_item> l, space ph, int qual,
                    space fn_sep, space fnote_sep, space float_sep,
                    font fn, int first_page, int level,
                    array<SI> nums, array<tree> trees,
                    std::vector<int>& num_off, std::vector<int>& tree_off)
{
  new_breaker_rep* H=
    tm_new<new_breaker_rep> (l, ph, qual, fn_sep, fnote_sep, float_sep,
                             fn, first_page);
  if (level < H->fast_level) H->fast_level= level;
  int n= N(l);
  if (H->fast_level >= 4 && breaker_last.used && N(nums) > BREAKER_PARAMS) {
    // the items in common with the previous search, at both ends, when
    // the parameters of the breaker are the same
    breaker_history& h= breaker_last;
    bool same= true;
    for (int i=0; i<BREAKER_PARAMS && same; i++) same= (h.nums[i] == nums[i]);
    if (same) {
      int m= min (n, h.n), p= 0, q= 0;
      while (p < m && same_item (h, p, nums, trees, num_off, tree_off, p)) p++;
      while (q < m - p &&
             same_item (h, h.n-1-q, nums, trees, num_off, tree_off, n-1-q)) q++;
      H->old= &h; H->old_prefix= p; H->old_suffix= q;
      if (edit_profile.on) edit_profile.breaks_common= p + q;
    }
  }
  //cout << HRULE << LF;
  double t0= edit_profile.on ? edit_profile_now () : 0;
  H->find_page_breaks ();
  H->export_tables ();
  if (edit_profile.on) {
    edit_profile.search += edit_profile_now () - t0;
    edit_profile.starts += N(H->done_list);
  }
  //cout << HRULE << LF;
  skeleton sk;
  int offset= first_page - 1;
  H->assemble_skeleton (sk, path (n), offset);
  //cout << HRULE << LF;
  if (H->fast_level >= 4 && N(nums) > BREAKER_PARAMS) {
    // (after the search: it reads the previous one)
    breaker_history& h= breaker_last;
    h.used= true; h.n= n;
    h.nums= nums; h.trees= trees;
    h.num_off= num_off; h.tree_off= tree_off;
    h.cands.swap (H->cands);
    h.cand_off.swap (H->cand_off);
    h.cand_cnt.swap (H->cand_cnt);
  }
  tm_delete (H);
  return sk;
}

skeleton
new_break_pages (array<page_item> l, space ph, int qual,
                 space fn_sep, space fnote_sep, space float_sep,
                 font fn, int first_page)
{
  static string mode= get_env ("TEXMACS_PAGE_BREAK_CACHE");
  static string fast= get_env ("TEXMACS_PAGE_BREAK_FAST");
  array<SI> nums;
  array<tree> trees;
  std::vector<int> num_off, tree_off;
  breaker_memo* memo= NULL;
  nums << (SI) ph->min << (SI) ph->def << (SI) ph->max << (SI) qual
       << (SI) fn_sep->min << (SI) fn_sep->def << (SI) fn_sep->max
       << (SI) fnote_sep->min << (SI) fnote_sep->def << (SI) fnote_sep->max
       << (SI) float_sep->min << (SI) float_sep->def << (SI) float_sep->max
       << (SI) fn->y1 << (SI) fn->y2 << (SI) first_page;
  breaker_signature (l, nums, trees, &num_off, &tree_off);
  if (mode != "off")
    for (int i=0; i<BREAKER_MEMOS && memo == NULL; i++)
      if (breaker_memos[i].used &&
          same_signature (breaker_memos[i].nums, breaker_memos[i].trees,
                          nums, trees))
        memo= &breaker_memos[i];

  if (memo != NULL && mode != "check") {
    if (edit_profile.on) edit_profile.breaks_reused++;
    return memo->sk;
  }

  skeleton sk=
    search_page_breaks (l, ph, qual, fn_sep, fnote_sep, float_sep, fn,
                        first_page, 4, nums, trees, num_off, tree_off);
  if (fast == "check") {
    // the search as it was against the faster one
    skeleton sk0=
      search_page_breaks (l, ph, qual, fn_sep, fnote_sep, float_sep, fn,
                          first_page, 0, nums, trees, num_off, tree_off);
    if (sk0 != sk)
      failed_error << "The faster search of the page breaks differs "
                   << "from the search as it was (" << N(l) << " items)" << LF;
  }

  if (memo != NULL) {
    // "check": what was kept against what is found
    if (memo->sk != sk)
      failed_error << "The skeleton kept for this signature differs "
                   << "from the one found (" << N(l) << " items)" << LF;
    else if (edit_profile.on) edit_profile.breaks_reused++;
  }
  else if (mode != "off") {
    memo= &breaker_memos[breaker_memo_next];
    breaker_memo_next= (breaker_memo_next + 1) % BREAKER_MEMOS;
    memo->used= true;
    memo->nums= nums;
    memo->trees= trees;
    memo->sk= sk;
  }
  return sk;
}
