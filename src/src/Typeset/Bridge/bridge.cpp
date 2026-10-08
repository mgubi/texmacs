
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
  status (CORRUPTED), changes (UNINIT), reads (), reads_known (false),
  removed (UNINIT) {}

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
  new_br->removed= br->removed;
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

/******************************************************************************
* What is typeset again when the environment changed
*
* ttt->old_patch holds the variables whose value, at this point of the
* document, is not the one of the previous pass. As long as it was not
* empty every bridge was typeset again: after a new section, every
* paragraph down to the end of the document, since the number of the
* section never comes back to what it was (2600 bridges and 120 ms in a
* document of 140 pages).
*
* What a bridge made depends on the variables which its typesetting reads
* or writes. They are recorded (env_table in env.hpp, 'reads'), and a
* bridge whose subtree did not change is not typeset again when none of
* the variables of old_patch is among them. This holds when:
*   - the bridge got no line items from the bridges around it (ttt->a and
*     ttt->b), which end up in what it makes: the paragraphs of a document
*     get none, the bridges inside an equation or a title do;
*   - the reads were all recorded. Those of a bridge are added to the
*     record of the bridge above it, unless they are not known or the
*     record would hold more than READS_MAX variables: a document does not
*     keep the reads of all its paragraphs (it is typeset again, which is
*     cheap: it visits them), an equation or a title keeps its own. The
*     table must not have been read as a whole, and nothing else must
*     have been looked at which may change (read_other: a reference, an
*     attachment, a Scheme routine): a paragraph with a reference is
*     typeset again as it was before, which is also what brought its
*     number up to date when it followed the change;
*   - the variables of old_patch are plain ones (plain_variable): the
*     others (the font, the mode, the language, the colors...) are turned
*     into a state of the environment when they are written, which is used
*     without reading them.
* The variables which the bridge writes then get the values it wrote in
* the previous pass ('changes', which did not depend on the others): they
* are as before from here on, and leave old_patch.
*
* A paragraph which is removed takes its changes of the environment with
* it, and nothing was typeset again which could have noticed: every bridge
* after it was marked for typesetting (bridge_document_rep::notify_remove).
* Instead, the bridge which follows keeps the changes of the removed ones
* ('removed'), which are the values the variables had at that point of
* the previous pass: the next pass compares them with the values they
* have now, and those which differ enter old_patch, as after a bridge
* which is typeset again.
*
* TEXMACS_TYPESET_READS may be "off" (typeset again as before) or "check"
* (typeset again all the same, and report a result which differs).
******************************************************************************/

bool
bridge_reads_on () {
  static int on= -1;
  if (on < 0) on= (get_env ("TEXMACS_TYPESET_READS") == "off") ? 0 : 1;
  return on == 1;
}

static int
reads_mode () {
  // 0: off, 1: on, 2: check
  static int mode= -1;
  if (mode < 0) {
    string s= get_env ("TEXMACS_TYPESET_READS");
    mode= (s == "off") ? 0 : (s == "check") ? 2 : 1;
  }
  return mode;
}

#define READS_MAX 256

static bool
has_read (array<int> reads, int id) {
  int a= 0, b= N(reads);
  while (a < b) {
    int m= (a + b) >> 1;
    if (reads[m] == id) return true;
    if (reads[m] < id) a= m + 1; else b= m;
  }
  return false;
}

static bool
patch_unread (hashmap<string,tree> patch, array<int> reads, edit_env env) {
  for (iterator<string> it= iterate (patch); it->busy (); ) {
    string var= it->next ();
    if (!env->plain_variable (var) || has_read (reads, env_var_id (var)))
      return false;
  }
  return true;
}

// the reads of a bridge which was typeset or used again, for the record of
// the bridge above it
static void
add_reads (edit_env env, bool known, array<int> reads) {
  hashmap<int,bool>* rec= env->read_recorder ();
  if (rec == NULL || env->read_unknown) return;
  if (!known || N(*rec) + N(reads) > READS_MAX) { env->read_unknown= true; return; }
  for (int i=0; i<N(reads); i++) rec->operator () (reads[i])= true;
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
  if (N(removed) != 0) {
    // (see above: the changes of the paragraphs removed before this one)
    env->compare_changes (ttt->old_patch, removed);
    removed= hashmap<string,tree> (UNINIT);
  }
  // (what a bridge makes also holds the line items which the bridges
  // around it left for its first and last lines, ttt->a and ttt->b: the
  // number of an equation ends up in the lines of its body)
  bool alone= (N(ttt->a) == 0) && (N(ttt->b) == 0);
  bool unread= (status==desired_status) && (N(ttt->old_patch)!=0) &&
               reads_known && alone && reads_mode () != 0 &&
               patch_unread (ttt->old_patch, reads, env);
  bool check= unread && reads_mode () == 2;
  array<SI> old_lines;
  array<tree> old_contents;
  hashmap<string,tree> old_changes (UNINIT);
  if (check) {
    old_lines= lines_signature (l);
    old_contents= lines_contents (l);
    old_changes= changes;
  }
  if ((status==desired_status) && (N(ttt->old_patch)==0)) {
    //cout << "cached" << LF;
    if (edit_profile.on) edit_profile.cached++;
    add_reads (env, reads_known, reads);
    env->monitored_patch_env (changes);
    // cout << "changes       = " << changes << LF;
  }
  else if (unread && !check) {
    // as above; what the bridge writes is as in the previous pass
    if (edit_profile.on) { edit_profile.cached++; edit_profile.unread++; }
    add_reads (env, reads_known, reads);
    env->monitored_patch_env (changes);
    env->compare_changes (ttt->old_patch, changes);
  }
  else {
    // cout << "Typesetting " << st << ", " << desired_status << LF << INDENT;
    //cout << "recomputing" << LF;
    if (edit_profile.on) edit_profile.redone++;
    hashmap<string,tree> prev_back (UNINIT);
    my_clean_links ();
    link_repository old_link_env= env->link_env;
    env->link_env= link_env;
    ttt->local_start (l, sb);
    env->local_start (prev_back);
    if (env->hl_lan != 0) env->lan->highlight (st);
    // the variables read by the typesetting of the subtree
    hashmap<int,bool>* outer_rec= env->read_recorder ();
    bool outer_all= env->read_all (), outer_unknown= env->read_unknown;
    hashmap<int,bool> my_reads (false);
    env->record_reads (&my_reads, false);
    env->read_unknown= false;
    my_typeset (desired_status);
    bool recorded= !env->read_unknown && !env->read_all () &&
                   N(my_reads) <= READS_MAX;
    reads_known= recorded && alone;
    reads= array<int> ();
    if (recorded) {
      for (iterator<int> it= iterate (my_reads); it->busy (); )
        reads << it->next ();
      merge_sort (reads);
    }
    env->record_reads (outer_rec, outer_all);
    env->read_unknown= outer_unknown;
    add_reads (env, recorded, reads);
    env->local_update (ttt->old_patch, changes);
    env->local_end (prev_back);
    ttt->local_end (l, sb);
    if (check) {
      if (edit_profile.on) edit_profile.unread++;
      if (lines_signature (l) != old_lines || changes != old_changes ||
          lines_contents (l) != old_contents)
        failed_error << "A bridge which read none of the variables which "
                     << "changed (" << ttt->old_patch << ") is typeset "
                     << "otherwise: " << st << LF;
    }
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
