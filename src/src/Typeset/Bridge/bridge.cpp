
/******************************************************************************
* MODULE     : bridge.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "iterator.hpp"
#include "sys_utils.hpp" // get_env
#include "merge_sort.hpp"
#include "Boxes/construct.hpp"

bridge bridge_document (typesetter, tree, path);
bridge bridge_surround (typesetter, tree, path);
bridge bridge_hidden (typesetter, tree, path);
bridge bridge_formatting (typesetter, tree, path, string);
bridge bridge_with (typesetter, tree, path);
bridge bridge_rewrite (typesetter, tree, path);
bridge bridge_argument (typesetter, tree, path);
bridge bridge_default (typesetter, tree, path);
bridge bridge_compound (typesetter, tree, path);
bridge bridge_mark (typesetter, tree, path);
bridge bridge_expand_as (typesetter, tree, path);
bridge bridge_eval (typesetter, tree, path);
bridge bridge_auto (typesetter, tree, path, tree, bool);
bridge bridge_locus (typesetter, tree, path);
bridge bridge_ornament (typesetter, tree, path);
bridge bridge_art_box (typesetter, tree, path);
bridge bridge_canvas (typesetter, tree, path);

bridge nil_bridge;

/******************************************************************************
* Constructors and basic operations
******************************************************************************/

bridge_rep::bridge_rep (typesetter ttt2, tree st2, path ip2):
  ttt (ttt2), env (ttt->env), st (st2), ip (ip2),
  status (CORRUPTED), changes (UNINIT), reads (),
  in_known (false), in_items (""), in_fp (0) {}

static tree inactive_auto
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x"), "recurse*"));
static tree error_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "error*"));
static tree inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "once*"));
static tree var_inactive_m
  (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x", "0"), "recurse*"));

bridge
make_inactive_bridge (typesetter ttt, tree st, path ip) {
  if (is_document (st))
    return bridge_document (ttt, st, ip);
  else return bridge_auto (ttt, st, ip, inactive_auto, false);
}

bridge
make_bridge (typesetter ttt, tree st, path ip) {
  // cout << "Make bridge " << st << ", " << ip << LF;
  // cout << "Preamble mode= " << ttt->env->preamble << LF;
  if (ttt->env->preamble)
    return make_inactive_bridge (ttt, st, ip);
  switch (L(st)) {
  case _ERROR:
    return bridge_auto (ttt, st, ip, error_m, true);
  case DOCUMENT:
    return bridge_document (ttt, st, ip);
  case SURROUND:
    return bridge_surround (ttt, st, ip);
  case HIDDEN:
    return bridge_hidden (ttt, st, ip);
  case DATOMS:
    return bridge_formatting (ttt, st, ip, ATOM_DECORATIONS);
  case DLINES:
    return bridge_formatting (ttt, st, ip, LINE_DECORATIONS);
  case DPAGES:
    return bridge_formatting (ttt, st, ip, PAGE_DECORATIONS);
  case TFORMAT:
    return bridge_formatting (ttt, st, ip, CELL_FORMAT);
  case WITH:
    return bridge_with (ttt, st, ip);
  case COMPOUND:
    return bridge_compound (ttt, st, ip);
  case ARG:
    return bridge_argument (ttt, st, ip);
  case MAP_ARGS:
    // FIXME: we might want to merge bridge_rewrite and bridge_eval
    // 'map_args' should really be implemented using bridge_rewrite,
    // but bridge_eval leads to better locality of updates for 'screens'
    return bridge_eval (ttt, st, ip);
  case MARK:
  case VAR_MARK:
    return bridge_mark (ttt, st, ip);
  case EXPAND_AS:
    return bridge_expand_as (ttt, st, ip);
  case EVAL:
  case QUASI:
    return bridge_eval (ttt, st, ip);
  case EXTERN:
  case VAR_INCLUDE:
  case WITH_PACKAGE:
    return bridge_rewrite (ttt, st, ip);
  case INCLUDE:
    return bridge_compound (ttt, st, ip);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return bridge_compound (ttt, st, ip);
  case INACTIVE:
    return bridge_auto (ttt, st, ip, inactive_m, true);
  case VAR_INACTIVE:
    return bridge_auto (ttt, st, ip, var_inactive_m, true);
  case REWRITE_INACTIVE:
    return bridge_rewrite (ttt, st, ip);
  case LOCUS:
    return bridge_locus (ttt, st, ip);
  case HLINK:
  case ACTION:
    return bridge_compound (ttt, st, ip);
  case ANIM_STATIC:
  case ANIM_DYNAMIC:
    return bridge_eval (ttt, st, ip);
  case CANVAS:
    return bridge_canvas (ttt, st, ip);
  case ORNAMENT:
    return bridge_ornament (ttt, st, ip);
  case ART_BOX:
    return bridge_art_box (ttt, st, ip);
  default:
    if (L(st) < START_EXTENSIONS) return bridge_default (ttt, st, ip);
    else return bridge_compound (ttt, st, ip);
  }
}

void
replace_bridge (bridge& br, tree st, path ip) {
  bridge new_br= make_bridge (br->ttt, st, ip);
  new_br->changes= br->changes;
  br= new_br;
}

void
replace_bridge (bridge& br, path p, tree oldt, tree newt, path ip) {
  if (oldt == newt) return;
  if (is_atomic (newt) || L(oldt) != L(newt) || N(oldt) != N(newt)) {
    if (is_nil (p)) replace_bridge (br, newt, ip);
    else br->notify_assign (p, newt);
  }
  else
    for (int i=0; i<N(newt); i++)
      replace_bridge (br, p * i, oldt[i], newt[i], ip);
}

bool
bridge::operator == (bridge item2) {
  return rep == item2.rep;
}

bool
bridge::operator != (bridge item2) {
  return rep != item2.rep;
}

tm_ostream&
operator << (tm_ostream& out, bridge br) {
  return out << "bridge [" << br->st << ", " << br->ip << "]";
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) {
    ASSERT (is_atomic (u), "two atoms expected");
    t= t->label (0, l) * u->label * t->label (l, N(t->label));
  }
  else t= (t (0, l) * u) * t (l, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_remove (path p, int nr) {
  // cout << "Insert " << p << ", " << nr << " in " << st << "\n";
  path q= path_up (p);
  int  l= last_item (p);
  tree t= subtree (st, q);
  if (is_atomic (t)) t= t->label (0, l) * t->label (l+nr, N(t->label));
  else t= t (0, l) * t (l+nr, N(t));
  notify_assign (q, t);
}

void
bridge_rep::notify_split (path p) {
  // cout << "Split " << p << " in " << st << "\n";
  path q  = path_up (p, 2);
  int  pos= last_item (path_up (p));
  int  l  = last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos])) {
    string s1= t[pos]->label (0, l), s2= t[pos]->label (l, N (t[pos]->label));
    notify_insert (q * pos, tree (L(t), s1));
    notify_assign (q * (pos+1), s2);
  }
  else {
    tree t1= t[pos] (0, l), t2= t[pos] (l, N(t[pos]));
    notify_insert (q * pos, tree (L(t), t1));
    notify_assign (q * (pos+1), t2);
  }
}

void
bridge_rep::notify_join (path p) {
  // cout << "Join " << p << " in " << st << "\n";
  path q  = path_up (p);
  int  pos= last_item (p);
  tree t  = subtree (st, q);

  if (is_atomic (t[pos]) && is_atomic (t[pos+1])) {
    string j= t[pos]->label * t[pos+1]->label;
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
  else {
    tree j= t[pos] * t[pos+1];
    notify_remove (q * pos, 1);
    notify_assign (q * pos, j);
  }
}

/******************************************************************************
* Getting environment variables and typesetting
******************************************************************************/

void
bridge_rep::my_clean_links () {
  link_env= link_repository (true);
}

void
bridge_rep::my_exec_until (path p) {
  env->exec_until (st, p);
}

bool
bridge_rep::my_typeset_will_be_complete () {
  return (status & VALID_MASK) == CORRUPTED;
}

void
bridge_rep::my_typeset (int desired_status) {
  if ((desired_status & WANTED_MASK) == WANTED_PARAGRAPH)
    ttt->insert_paragraph (st, ip);
  if ((desired_status & WANTED_MASK) == WANTED_PARUNIT)
    ttt->insert_parunit (st, ip);
}

void
bridge_rep::exec_until (path p, bool skip_flag) {
  // This virtual routine is redefined in bridge_auto in order to
  // treat cursor positions on the border in a special way depending
  // on skip_flag

  (void) skip_flag;
  // cout << "Exec until " << p << " in " << st << "\n";
  if ((status & VALID_MASK) != PROCESSED) {
    // cout << "  Re-execute until\n";
    env->exec_until (st, p);
  }
  else if (p == path (right_index (st))) {
    // cout << "  Patch env\n";
    env->patch_env (changes);
  }
  else if (p != path (0)) {
    // cout << "  My execute until\n";
    my_exec_until (p);
  }
  // cout << "  Done\n";
}

extern tree the_et;

#define READS_MAX 256

#define LOOKUPS_MAX 64

// the reads and the lookups of a bridge which was typeset or used again,
// for the record of the bridge above it
static void
add_reads (edit_env env, bool known, array<int> reads,
           array<string> keys, array<tree> values) {
  hashmap<int,bool>* rec= env->read_recorder ();
  if (rec == NULL || env->read_unknown) return;
  if (!known || N(*rec) + N(reads) > READS_MAX ||
      N(*env->rec_keys) + N(keys) > LOOKUPS_MAX) {
    env->read_unknown= true; return; }
  for (int i=0; i<N(reads); i++) rec->operator () (reads[i])= true;
  (*env->rec_keys) << keys;
  (*env->rec_values) << values;
}

// do the references and attachments looked up still have the values found?
static bool
same_lookups (edit_env env, array<string> keys, array<tree> values) {
  for (int i=0; i<N(keys); i++)
    if (env->lookup_value (keys[i]) != values[i]) return false;
  return true;
}

// what a bridge made, for the mode "check": the boxes as trees (with
// their text), and their sizes
static array<tree>
lines_contents (array<page_item> l) {
  array<tree> r;
  for (int i=0; i<N(l); i++) r << (tree) l[i]->b;
  return r;
}

static array<SI>
lines_signature (array<page_item> l) {
  array<SI> r;
  for (int i=0; i<N(l); i++)
    r << (SI) l[i]->type << (SI) l[i]->b->w () << (SI) l[i]->b->h ()
      << (SI) l[i]->b->x1 << (SI) l[i]->b->y1
      << (SI) l[i]->b->x3 << (SI) l[i]->b->y3
      << (SI) l[i]->b->x4 << (SI) l[i]->b->y4
      << (SI) l[i]->spc->min << (SI) l[i]->spc->def << (SI) l[i]->spc->max
      << (SI) l[i]->penalty << (SI) N(l[i]->fl);
  return r;
}

// and the border of the stack (vertical spaces, page breaks...)
static array<SI>
border_signature (stack_border sb) {
  array<SI> r;
  r << sb->height << sb->sep << sb->hor_sep << sb->ver_sep
    << sb->bot << sb->top << sb->height_before << sb->sep_before
    << sb->hor_sep_before << sb->ver_sep_before
    << sb->vspc_before->min << sb->vspc_before->def << sb->vspc_before->max
    << sb->vspc_after->min << sb->vspc_after->def << sb->vspc_after->max
    << (SI) sb->nobr_before << (SI) sb->nobr_after;
  return r;
}

/******************************************************************************
* What is typeset again
*
* What a bridge makes is a function of its subtree and of what it was
* typeset from: the variables which it read before it wrote them, with
* their values, the state of the environment derived from the variables
* (the font, the colour...: edit_env_rep::derived_state), the references
* and attachments which it looked up, and the line items which the bridges
* around it gave it (ttt->a and ttt->b). These are its inputs. A bridge
* whose subtree did not change (its status) is used again when all of its
* inputs are known (no Scheme routine, not too many) and have the values
* they had, or when the whole environment is as it was (a fingerprint of
* it, kept as it is written: env_table::fp), which is what a bridge whose
* inputs are not known needs: then it writes what it wrote (changes, exact
* for a variable which was not there: env_absent). Nothing about the bridges before it is
* needed: what changed before it, what was removed or bound above it, is
* seen in the values of its inputs. The inputs of the bridges below a
* bridge are among its own, unless it wrote them before.
*
* TEXMACS_TYPESET_READS may be "off" (the classic cache: a bridge is used
* again when the bookkeeping of the changes of the environment, old_patch,
* says that nothing changed before it) or "check" (a bridge which would be
* used again is typeset again, and what it makes is compared).
******************************************************************************/

#define INPUTS_MAX 256

static tree
items_signature (array<line_item> a, array<line_item> b) {
  if (N(a) == 0 && N(b) == 0) return "";
  tree r (TUPLE);
  for (int k=0; k<2; k++) {
    array<line_item> l= (k == 0? a: b);
    for (int i=0; i<N(l); i++)
      r << tree (TUPLE, as_string (l[i]->type), as_string (l[i]->op_type),
                 (tree) l[i]->b, as_string (l[i]->b->w ()),
                 as_string (l[i]->b->h ()), as_string (l[i]->penalty),
                 as_string (l[i]->spc->def))
        << l[i]->t;
    r << tree ("|");
  }
  return r;
}

static inline bool
same_value (tree before, tree now) {
  if (strong_equal (before, now)) return true;
  if (strong_equal (before, env_absent ()) ||
      strong_equal (now, env_absent ())) return false;
  return before == now;
}

bool
bridge_rep::same_inputs () {
  if (N(ttt->a) == 0 && N(ttt->b) == 0) {
    if (in_items != "") return false; }
  else if (in_items != items_signature (ttt->a, ttt->b)) return false;
  if (!same_lookups (env, seen_keys, seen_values)) return false;
  // the whole environment as it was (the fingerprint: also for a bridge
  // whose inputs are not known), or the inputs
  if (env->fingerprint () == in_fp) return true;
  if (!in_known) return false;
  for (int i=0; i<N(in_vars); i++)
    if (!same_value (in_vals[i], env->current (in_vars[i]))) return false;
  return env->derived_state () == in_state;
}

// the inputs of a bridge are inputs of the bridge which is recorded above
// it, when it did not write them before (parent_back) or read them
static void
add_inputs (edit_env env, hashmap<string,tree> parent_back, bool known,
            array<string> vars, array<tree> vals) {
  array<string>* pv= env->input_vars ();
  array<tree>* pw= env->input_vals ();
  hashmap<int,bool>* rec= env->read_recorder ();
  if (pv == NULL || rec == NULL || env->read_unknown) return;
  if (!known || N(*pv) + N(vars) > INPUTS_MAX) {
    env->read_unknown= true; return; }
  for (int i=0; i<N(vars); i++) {
    int id= env_var_id (vars[i]);
    if ((*rec)->contains (id) || parent_back->contains (vars[i])) continue;
    rec->operator () (id)= true;
    (*pv) << vars[i];
    (*pw) << vals[i];
  }
}

void
bridge_rep::typeset_by_inputs (int desired_status) {
  bool same= (status == desired_status) && same_inputs ();
  bool check= same && typeset_inputs_mode () == 2;
  array<SI> old_lines, old_border;
  array<tree> old_contents;
  hashmap<string,tree> old_changes (UNINIT);
  if (check) {
    old_lines= lines_signature (l);
    old_border= border_signature (sb);
    old_contents= lines_contents (l);
    old_changes= changes;
  }
  if (same && !check) {
    if (edit_profile.on) edit_profile.cached++;
    add_inputs (env, env->values_before (), true, in_vars, in_vals);
    add_reads (env, true, reads, seen_keys, seen_values);
    env->monitored_patch_env (changes);
    return;
  }
  if (edit_profile.on) edit_profile.redone++;
  array<tree> state= env->derived_state ();
  tree items= items_signature (ttt->a, ttt->b);
  unsigned long long fp= env->fingerprint ();
  hashmap<string,tree> prev_back (UNINIT);
  my_clean_links ();
  link_repository old_link_env= env->link_env;
  env->link_env= link_env;
  ttt->local_start (l, sb);
  env->local_start (prev_back);
  if (env->hl_lan != 0) env->lan->highlight (st);
  hashmap<int,bool> my_reads (false);
  array<string> my_keys, my_vars;
  array<tree> my_values, my_vals;
  edit_env_rep::read_record record (env.operator-> (), &my_reads,
                                    &my_keys, &my_values,
                                    &my_vars, &my_vals);
  my_typeset (desired_status);
  bool known= record.complete () && N(my_vars) <= INPUTS_MAX &&
              N(my_keys) <= LOOKUPS_MAX;
  reads= array<int> ();
  for (iterator<int> it= iterate (my_reads); it->busy (); )
    reads << it->next ();
  merge_sort (reads);
  record.stop ();
  in_known= known;
  in_vars= my_vars;
  in_vals= my_vals;
  in_state= state;
  in_items= items;
  in_fp= fp;
  seen_keys= my_keys;
  seen_values= my_values;
  add_inputs (env, prev_back, known, my_vars, my_vals);
  add_reads (env, known && N(reads) <= READS_MAX, reads, seen_keys,
             seen_values);
  env->local_changes (changes);
  env->local_end (prev_back);
  ttt->local_end (l, sb);
  if (check) {
    if (edit_profile.on) edit_profile.unread++;
    if (lines_signature (l) != old_lines || changes != old_changes ||
        border_signature (sb) != old_border ||
        lines_contents (l) != old_contents) {
      typeset_check_mismatches++;
      failed_error << "A bridge whose inputs were the same is typeset "
                   << "otherwise: " << st << LF;
    }
  }
  env->link_env= old_link_env;
  status= desired_status;
}

void
bridge_rep::typeset (int desired_status) {
  // FIXME: this dirty hack ensures a perfect coherence between
  // the bridge and the edit tree at the typesetting stage.
  // This should not be necessary, but we use because the ip_observers
  // may become wrong otherwise.
  if (is_accessible (ip))
    st= subtree (the_et, reverse (ip));
  if (!is_accessible (ip)) {
    path ip2= obtain_ip (st);
    if (ip2 != path (DETACHED))
      ip= ip2;
  }

  //cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
  if (typeset_inputs_mode () != 0) typeset_by_inputs (desired_status);
  else if ((status==desired_status) && (N(ttt->old_patch)==0)) {
    //cout << "cached" << LF;
    env->monitored_patch_env (changes);
    // cout << "changes       = " << changes << LF;
  }
  else {
    // cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
    //cout << "recomputing" << LF;
    hashmap<string,tree> prev_back (UNINIT);
    my_clean_links ();
    link_repository old_link_env= env->link_env;
    env->link_env= link_env;
    ttt->local_start (l, sb);
    env->local_start (prev_back);
    if (env->hl_lan != 0) env->lan->highlight (st);
    my_typeset (desired_status);
    env->local_update (ttt->old_patch, changes);
    env->local_end (prev_back);
    ttt->local_end (l, sb);
    env->link_env= old_link_env;
    status= desired_status;
    // cout << "old_patch     = " << ttt->old_patch << LF;
    // cout << "changes       = " << changes << LF;
    // cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;
  }
  //cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;

  // ttt->insert_stack (l, sb);
  //if (N(l) == 0); else
  if (ttt->paper || (N(l) <= 1)) ttt->insert_stack (l, sb);
  else {
    bool flag= false;
    int i, n= N(l);
    for (i=0; i<n; i++)
      flag= flag || (N (l[i]->fl) != 0) || (l[i]->nr_cols > 1);
    if (flag) ttt->insert_stack (l, sb);
    else {
      int first=-1, last=-1;
      array<box> bs;
      array<SI>  spc;
      array<page_item> special_l;
      for (i=0; i<n; i++)
	if (l[i]->type != PAGE_CONTROL_ITEM) {
	  if (first == -1 && l[i]->type == PAGE_LINE_ITEM) first= N(bs);
	  bs  << l[i]->b;
	  spc << l[i]->spc->def;
	  last= i;
	}
        else if (is_tuple (l[i]->t, "env_page") &&
                 (l[i]->t[1] == PAGE_THIS_TOP ||
                  l[i]->t[1] == PAGE_THIS_BOT ||
                  l[i]->t[1] == PAGE_THIS_BG_COLOR))
          special_l << l[i];
      box lb= stack_box (path (ip), bs, spc);
      if (first != -1) lb= move_box (path (ip), lb, 0, bs[first]->y2);
      array<page_item> new_l (1);
      new_l[0]= page_item (lb);
      new_l[0]->spc= l[last]->spc;
      new_l << special_l;
      ttt->insert_stack (new_l, sb);
    }
  }

  //cout << "l   = " << l << LF;
  //cout << "sb  = " << sb << LF;
  //cout << "l   = " << ttt->l << LF;
  //cout << "a   = " << ttt->a << LF;
  //cout << "b   = " << ttt->b << LF;
}
