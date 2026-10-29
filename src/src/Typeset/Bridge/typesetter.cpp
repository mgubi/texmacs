
/******************************************************************************
* MODULE     : typesetter.hpp
* DESCRIPTION: Implementation of the main TeXmacs typesetting routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Bridge/impl_typesetter.hpp"
#include "iterator.hpp"
#include "sys_utils.hpp" // get_env
#include <chrono>

extern tree the_et;

/******************************************************************************
* Constructor and destructor
******************************************************************************/

typesetter_rep::typesetter_rep (edit_env& env2, tree et, path ip):
  env (env2), old_patch (UNINIT)
{
  paper= (env->get_string (PAGE_MEDIUM) == "paper");
  br= make_bridge (this, et, ip);
  x1= y1= x2= y2=0;
  kept_pages= tm_new<page_memo> ();
  collect= false;
  dropped= false;
}

typesetter_rep::~typesetter_rep () {
  tm_delete (kept_pages);
}

typesetter
new_typesetter (edit_env& env, tree et, path ip) {
  return tm_new<typesetter_rep> (env, et, ip);
}

void
delete_typesetter (typesetter ttt) {
  tm_delete (ttt);
}

/******************************************************************************
* Output flux
******************************************************************************/

void
typesetter_rep::insert_stack (array<page_item> l2, stack_border sb2) {
  merge_stack (l, sb, l2, sb2);
}

void
typesetter_rep::insert_parunit (tree t, path ip) {
  insert_paragraph (t, ip);
}

void
typesetter_rep::insert_paragraph (tree t, path ip) {
  // cout << "Typesetting " << t << ", " << ip << "\n";
  stack_border     temp_sb;
  array<page_item> temp_l= typeset_stack (env, t, ip, a, b, temp_sb);
  insert_stack (temp_l, temp_sb);

  /*
  int i, n= N(temp_l);
  for (i=0; i<n; i++)
    cout << i << ", "
	 << temp_l[i]->b->find_lip () << ", "
	 << temp_l[i]->b->find_rip () << ",\t"
	 << temp_l[i]->b << "\n";
  */
}

void
typesetter_rep::insert_surround  (array<line_item> a2, array<line_item> b2) {
  a << a2;
  array<line_item> temp_b= b;
  b= copy (b2);
  b << temp_b;
}

void
typesetter_rep::insert_marker (tree st, path ip) {
  (void) st;
  // if (!is_multi_paragraph (st)) {
  array<line_item> a2= typeset_marker (env, descend (ip, 0));
  array<line_item> b2= typeset_marker (env, descend (ip, 1));
  insert_surround (a2, b2);
  // }
}

void
typesetter_rep::local_start (array<page_item>& prev_l, stack_border& prev_sb) {
  prev_l   = l;
  prev_sb  = sb;
  l        = array<page_item> ();
  sb       = stack_border ();
}

void
typesetter_rep::local_end (array<page_item>& prev_l, stack_border& prev_sb) {
  array<page_item> temp_l   = l;
  stack_border     temp_sb  = sb;
  l        = prev_l;
  sb       = prev_sb;
  prev_l   = temp_l;
  prev_sb  = temp_sb;
}

/******************************************************************************
* Main typesetting routines
******************************************************************************/

// see typesetter.hpp
int typeset_check_mismatches= 0;
int get_typeset_check_mismatches () { return typeset_check_mismatches; }
int typeset_passes_total= 0;
int typeset_redone_total= 0;
int get_typeset_passes () { return typeset_passes_total; }
int get_typeset_redone () { return typeset_redone_total; }
void
typeset_count_redone (int n) {
  typeset_redone_total += n;
  if (edit_profile.on) edit_profile.redone += n;
}

void
typeset_cache_mismatch (string what) {
  typeset_check_mismatches++;
  failed_error << "What the " << what << " kept differs from what is made "
               << "again" << LF;
}

int
typeset_caches_mode () {
  static int mode= -1;
  if (mode < 0) {
    string s= get_env ("TEXMACS_TYPESET_CACHES");
    mode= (s == "off"? 0: (s == "check"? 2: 1));
  }
  return mode;
}

static bool
edit_profile_wanted () {
  string s= get_env ("TEXMACS_EDIT_PROFILE");
  return s != "" && s != "off" && s != "0";
}

edit_profile_data edit_profile= { edit_profile_wanted () }; // (and zeros)

double
edit_profile_now () {
  using namespace std::chrono;
  return duration<double, std::milli> (steady_clock::now ().time_since_epoch ()).count ();
}

static rectangles
requires_update (rectangles log) {
  rectangles rs;
  while (!is_nil (log)) {
    rectangle r1= log->item;
    rectangle r2= log->next->item;
    if (r1 == rectangle (0, 0, 0, 0)) rs= rectangles (r2, rs);
    else if (r2 == rectangle (0, 0, 0, 0)) rs= rectangles (r1, rs);
    else if (r1 != r2) rs= rectangles (r1, rectangles (r2, rs));
    log= log->next->next;
  }
  return reverse (rs);
}

void
typesetter_rep::determine_page_references (box b) {
  hashmap<string,tree> h ("?");
  b->collect_page_numbers (h, "?");
  iterator<string> it= iterate (h);
  while (it->busy()) {
    string var= it->next ();
    tree   val= copy (h[var]);
    tree   old= env->local_ref [var];
    tree   now;
    if (is_func (old, TUPLE, 2)) now= tuple (old[0], val);
    else if (is_func (old, TUPLE, 3)) now= tuple (old[0], val, old[2]);
    else now= tuple (old, val);
    // (not written when it is there already: see labels_stamp)
    if (!env->local_ref->contains (var) || old != now)
      env->local_ref (var)= now;
    env->touched (var)= true;
  }
}

// A pass which is not complete does not clean the table of the references
// (clean_unused in edit_typeset.cpp): with the reuse by inputs it types
// again only the bridges which changed, but every bridge, typeset or used
// again, writes its bindings (write_bindings), and the root holds all of
// them (out_bindings). A key which the root wrote in the pass before and
// which neither it nor the pager wrote in this one is that of a label
// which an edit removed: its entry goes, and the pass which follows types
// again what looked it up (same_lookups), as with the classic cache or
// from scratch, where the references to it are undefined at once (#571).
bool
typesetter_rep::drop_bindings (array<tree> before, array<tree> pages) {
  array<tree> now= br->out_bindings;
  int i, n= N(before);
  if (n == N(now)) {
    for (i=0; i<n; i++)
      if (before[i].operator-> () != now[i].operator-> () &&
          before[i] != now[i]) break;
    if (i == n) return false;
  }
  hashmap<string,bool> written (false);
  for (int k=0; k<2; k++) {
    array<tree> l= (k == 0? now: pages);
    for (i=0; i<N(l); i++)
      if (is_tuple (l[i]) && N(l[i]) >= 1 && is_atomic (l[i][0]))
        written (l[i][0]->label)= true;
  }
  bool removed= false;
  for (i=0; i<n; i++) {
    tree t= before[i];
    if (!is_tuple (t) || N(t) < 1 || !is_atomic (t[0])) continue;
    string key= t[0]->label;
    if (written->contains (key) || !env->local_ref->contains (key)) continue;
    env->local_ref->reset (key);
    removed= true;
  }
  return removed;
}

box
typesetter_rep::typeset () {
  // (the tree of the buffer, for the automatic labels: see exec_auto_id)
  path rp= reverse (br->ip);
  tree buffer= "";
  if (!is_nil (rp) && rp->item >= 0 && has_subtree (the_et, path (rp->item)))
    buffer= the_et[rp->item];
  edit_env_rep::typesetting_pass pass (env.operator-> (), buffer);
  old_patch= hashmap<string,tree> (UNINIT);
  l        = array<page_item> ();
  sb       = stack_border ();
  a        = array<line_item> ();
  b        = array<line_item> ();
  paper    = (env->get_string (PAGE_MEDIUM) == "paper");

  // Test whether we are doing a complete typesetting (or a pass which
  // makes the tables again from the bridges, those used again writing
  // their bindings and the entries of the auxiliary tables again: collect)
  env->complete= collect || br->my_typeset_will_be_complete ();
  tree st= br->st;
  int i= 0, n= N(st);
  if (is_compound (st[0], "show-preamble")) { i++; env->complete= false; }
  if (is_compound (st[0], "hide-preamble")) i++;
  for (; i<n && env->complete; i++) {
    if (is_compound (st[i], "hide-part")) env->complete= false;
    if (!is_compound (st[i], "show-part")) break;
  }
  // (the bindings of the last pass, see drop_bindings: not when a preamble
  // or parts are hidden, whose labels stay)
  bool track= false;
  if (!env->complete && typeset_inputs_mode () != 0) {
    track= !is_compound (st[0], "show-preamble");
    for (int j= (is_compound (st[0], "hide-preamble")? 1: 0); j<n; j++) {
      if (is_compound (st[j], "hide-part")) track= false;
      if (!is_compound (st[j], "show-part")) break;
    }
  }
  array<tree> before;
  if (track) before= br->out_bindings;

  // Typeset
  if (env->complete) {
    env->local_aux= hashmap<string,tree> (UNINIT);
    env->missing  = hashmap<string,tree> (UNINIT);
    env->redefined= array<tree> ();
    env->touched  = hashmap<string,bool> (false);
  }
  typeset_passes_total++;
  double t0= edit_profile.on ? edit_profile_now () : 0;
  {
    shove_pass pass (shoves);
    bridge_record_next= true;
    bridge_macro_scope scope (false);
    br->typeset (PROCESSED+ WANTED_PARAGRAPH);
  }
  double t1= edit_profile.on ? edit_profile_now () : 0;
  pager ppp= tm_new<pager_rep> (br->ip, env, l);
  ppp->memo= kept_pages;
  array<tree> pages;
  box rb;
  if (track) {
    edit_env_rep::binding_record bindings (env.operator-> (), &pages);
    rb= ppp->make_pages ();
  }
  else rb= ppp->make_pages ();
  if (env->complete && paper) determine_page_references (rb);
  tm_delete (ppp);
  dropped= track && drop_bindings (before, pages);
  if (edit_profile.on) {
    edit_profile.bridges += t1 - t0;
    edit_profile.pages   += edit_profile_now () - t1;
    edit_profile.passes++;
  }
  // env->complete= false;  // moved to edit_typeset_rep::typeset
  return rb;
}

// the length of the change log, without a recursion as deep as it is long
// (N of a list: too deep for a browser after the first pass of a book)
static int
log_length (rectangles l) {
  int n= 0;
  for (; !is_nil (l); l= l->next) n++;
  return n;
}

box
typesetter_rep::typeset (SI& x1b, SI& y1b, SI& x2b, SI& y2b) {
  x1= x1b; y1= y1b; x2=x2b; y2= y2b;
  box b= typeset ();
  // cout << "-------------------------------------------------------------\n";
  double t0= edit_profile.on ? edit_profile_now () : 0;
  repaint.start_pass ();
  b->position_at (0, 0, repaint);
  repaint.end_pass ();
  rectangles change_log= repaint.rects;
  repaint.rects= rectangles ();
  if (edit_profile.on) edit_profile.lines += log_length (change_log) / 2;
  change_log= requires_update (change_log);
  if (edit_profile.on) edit_profile.rects += log_length (change_log);
  rectangle r (0, 0, 0, 0);
  if (!is_nil (change_log)) r= least_upper_bound (change_log);
  array<brush> new_bgs;
  array<rectangle> rs;
  b->collect_page_colors (new_bgs, rs);
  for (int i=0; i<min(N(old_bgs), N(new_bgs)); i++)
    if (new_bgs[i] != old_bgs[i])
      r= least_upper_bound (r, rs[i]);
  old_bgs= new_bgs;
  if (edit_profile.on) edit_profile.changes += edit_profile_now () - t0;
  if (typeset_caches_mode () == 2)
    check_repaint (b, r, placed_boxes, placed_rects);
  x1b= r->x1; y1b= r->y1; x2b= r->x2; y2b= r->y2;
  return b;
}

/******************************************************************************
* Event notification
******************************************************************************/

void
notify_assign (typesetter ttt, path p, tree u) {
  // cout << "Assign " << p << ", " << u << "\n";
  if (is_nil (p)) {
    ttt->br= make_bridge (ttt, u, ttt->br->ip);
    // everything is to be made again, for a reason which may be none
    // that the pager knows (fonts which arrived, in a browser): the
    // pages of the last pass are forgotten too
    *ttt->kept_pages= page_memo ();
  }
  else ttt->br->notify_assign (p, u);
}

void
notify_insert (typesetter ttt, path p, tree u) {
  // cout << "Insert " << p << ", " << u << "\n";
  ttt->br->notify_insert (p, u);
}

void
notify_remove (typesetter ttt, path p, int nr) {
  // cout << "Remove " << p << ", " << nr << "\n";
  ttt->br->notify_remove (p, nr);
}

void
notify_split (typesetter ttt, path p) {
  // cout << "Split " << p << "\n";
  ttt->br->notify_split (p);
}

void
notify_join (typesetter ttt, path p) {
  // cout << "Join " << p << "\n";
  ttt->br->notify_join (p);
}

void
notify_assign_node (typesetter ttt, path p, tree_label op) {
  // cout << "Assign node " << p << ", " << as_string (op) << "\n";
  tree t= subtree (ttt->br->st, p);
  int i, n= N(t);
  tree r (op, n);
  for (i=0; i<n; i++) r[i]= t[i];
  if (is_nil (p)) notify_assign (ttt, p, r);
  else ttt->br->notify_assign (p, r);
}

void
notify_insert_node (typesetter ttt, path p, tree t) {
  // cout << "Insert node " << p << ", " << t << "\n";
  int i, pos= last_item (p), n= N(t);
  tree r (t, n+1);
  for (i=0; i<pos; i++) r[i]= t[i];
  r[pos]= subtree (ttt->br->st, path_up (p));
  for (i=pos; i<n; i++) r[i+1]= t[i];
  if (is_nil (path_up (p))) notify_assign (ttt, path (), r);
  else ttt->br->notify_assign (path_up (p), r);
}

void
notify_remove_node (typesetter ttt, path p) {
  // cout << "Remove node " << p << "\n";
  tree t= subtree (ttt->br->st, p);
  if (is_nil (path_up (p))) notify_assign (ttt, path (), t);
  else ttt->br->notify_assign (path_up (p), t);
}

/******************************************************************************
* Getting environment variables and typesetting interface
******************************************************************************/

void
exec_until (typesetter ttt, path p) {
  ttt->br->exec_until (p);
}

box
typeset (typesetter ttt, SI& x1, SI& y1, SI& x2, SI& y2) {
  return ttt->typeset (x1, y1, x2, y2);
}

box
typeset_as_document (edit_env env, tree t, path ip) {
  env->style_init_env ();
  env->update ();
  typesetter ttt= new_typesetter (env, t, ip);
  box b= ttt->typeset ();
  delete_typesetter (ttt);
  return b;
}
