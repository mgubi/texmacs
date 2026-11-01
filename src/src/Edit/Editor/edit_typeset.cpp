
/******************************************************************************
* MODULE     : typeset.cpp
* DESCRIPTION: typeset the tree being edited
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <climits>
#include "edit_typeset.hpp"
#include "tm_buffer.hpp"
#include "tm_window.hpp" // tm_view_rep (the other views of the buffer)
#include "convert.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "tm_timer.hpp"
#include "Bridge/impl_typesetter.hpp"
#include "new_style.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Environment/std_environment.hpp"
#endif // EXPERIMENTAL

//box empty_box (path ip, int x1=0, int y1=0, int x2=0, int y2=0);
bool enable_fastenv= false;

/******************************************************************************
* Contructors, destructors and notification of modifications
******************************************************************************/

edit_typeset_rep::edit_typeset_rep ():
  editor_rep (), // NOTE: ignored by the compiler, but suppresses warning
  the_style (TUPLE),
  cur (hashmap<string,tree> (UNINIT)),
  stydef (UNINIT), pre (UNINIT), pre_version (0), prep_version (-1),
  prep_env (UNINIT), init (UNINIT), fin (UNINIT), grefs (UNINIT),
  env (drd, buf->buf->master,
       buf->data->ref, (buf->prj==NULL? grefs: buf->prj->data->ref),
       buf->data->aux, (buf->prj==NULL? buf->data->aux: buf->prj->data->aux),
       buf->data->att, (buf->prj==NULL? buf->data->att: buf->prj->data->att)),
  ttt (new_typesetter (env, subtree (et, rp), reverse (rp))) {
    init_update ();
}

edit_typeset_rep::~edit_typeset_rep () {
  // the boxes of the document go first; those which the typesetter keeps
  // (pages, shoves, the repaint log) go with it, in delete_typesetter
  eb= box ();
  delete_typesetter (ttt);
}

void
edit_typeset_rep::set_data (new_data data) {
  set_style (data->style);
  set_init  (data->init);
  set_fin   (data->fin);
  set_ref   (data->ref);
  set_aux   (data->aux);
  set_att   (data->att);
  notify_page_change ();
  add_init (data->init);
  notify_change (THE_DECORATIONS);
  typeset_invalidate_env ();
  iterator<string> it = iterate (data->att);
  while (it->busy()) {
    string key= it->next ();
    (void) call (string ("notify-set-attachment"),
                 buf->buf->name, key, data->att [key]);
  }
}

void
edit_typeset_rep::get_data (new_data& data) {
  // copies: the editor changes its own maps in place (init_env), and the
  // buffer's data must not follow the changes of one view (#528, #535)
  data->style= copy (get_style ());
  data->init = copy (get_init ());
  data->fin  = copy (get_fin ());
  data->ref  = get_ref ();
  data->aux  = get_aux ();
  data->att  = get_att ();
}

typesetter edit_typeset_rep::get_typesetter () { return ttt; }
tree edit_typeset_rep::get_style () { return the_style; }
void edit_typeset_rep::set_style (tree t) { the_style= copy (t); }
hashmap<string,tree> edit_typeset_rep::get_init () { return init; }
hashmap<string,tree> edit_typeset_rep::get_fin () { return fin; }
hashmap<string,tree> edit_typeset_rep::get_ref () { return buf->data->ref; }
hashmap<string,tree> edit_typeset_rep::get_aux () { return buf->data->aux; }
hashmap<string,tree> edit_typeset_rep::get_att () { return buf->data->att; }
void edit_typeset_rep::set_fin (hashmap<string,tree> H) { fin= H; }
void
edit_typeset_rep::set_ref (hashmap<string,tree> H) {
  buf->data->ref= H;
  // (loaded tables: see clean_unused)
  env->refs_settled= false;
  env->read_refs= hashmap<string,bool> (false);
}
void edit_typeset_rep::set_aux (hashmap<string,tree> H) { buf->data->aux= H; }
void edit_typeset_rep::set_att (hashmap<string,tree> H) { buf->data->att= H; }

tree
edit_typeset_rep::get_ref (string key) {
  return buf->data->ref[key];
}

void
edit_typeset_rep::set_ref (string key, tree im) {
  buf->data->ref (key)= im;
}

void
edit_typeset_rep::reset_ref (string key) {
  buf->data->ref->reset (key);
}

static string
concat_as_string (tree t) {
  if (is_atomic (t)) return t->label;
  else if (is_func (t, CONCAT)) {
    string r;
    for (int i=0; i<N(t); i++)
      r << concat_as_string (t[i]);
    return r;
  }
  else return "?";
}

array<string>
edit_typeset_rep::find_refs (string val, bool global) {
  tree a= (tree) buf->data->ref;
  if (global && buf->prj != NULL) a= buf->prj->data->ref;
  array<string> v;
  int i, n= N(a);
  for (i=0; i<n; i++)
    if (N(a[i]) >= 2 && N(a[i][1]) >= 1 &&
        concat_as_string (a[i][1][0]) == val)
      v << a[i][0]->label;
  return v;
}

array<string>
edit_typeset_rep::list_refs (bool global) {
  tree a= (tree) buf->data->ref;
  if (global && buf->prj != NULL) a= buf->prj->data->ref;
  array<string> v;
  int i, n= N(a);
  for (i=0; i<n; i++)
    v << a[i][0]->label;
  merge_sort (v);
  return v;
}

tree
edit_typeset_rep::get_aux (string key) {
  return buf->data->aux[key];
}

void
edit_typeset_rep::set_aux (string key, tree im) {
  buf->data->aux (key)= im;
}

void
edit_typeset_rep::reset_aux (string key) {
  buf->data->aux->reset (key);
}

array<string>
edit_typeset_rep::list_auxs (bool global) {
  tree a= (tree) buf->data->aux;
  if (global && buf->prj != NULL) a= buf->prj->data->aux;
  array<string> v;
  int i, n= N(a);
  for (i=0; i<n; i++)
    v << a[i][0]->label;
  merge_sort (v);
  return v;
}

tree
edit_typeset_rep::get_att (string key) {
  return buf->data->att[key];
}

void
edit_typeset_rep::set_att (string key, tree im) {
  buf->data->att (key)= im;
}

void
edit_typeset_rep::reset_att (string key) {
  buf->data->att->reset (key);
}

array<string>
edit_typeset_rep::list_atts (bool global) {
  tree a= (tree) buf->data->att;
  if (global && buf->prj != NULL) a= buf->prj->data->att;
  array<string> v;
  int i, n= N(a);
  for (i=0; i<n; i++)
    v << a[i][0]->label;
  merge_sort (v);
  return v;
}

void
edit_typeset_rep::set_init (hashmap<string,tree> H) {
  init= hashmap<string,tree> (UNINIT);
  add_init (H);
}

void
edit_typeset_rep::add_init (hashmap<string,tree> H) {
  init->join (H);
  ::notify_assign (ttt, path(), subtree (et, rp));
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::clear_local_info () {
  buf->data->ref= hashmap<string,tree> ();
  buf->data->aux= hashmap<string,tree> ();
}

/******************************************************************************
* Miscellaneous routines for lengths arithmetic (should be elsewhere)
******************************************************************************/

SI
edit_typeset_rep::as_length (string l) {
  return env->as_length (l); }

string
edit_typeset_rep::add_lengths (string l1, string l2) {
  return env->add_lengths (l1, l2); }

string
edit_typeset_rep::sub_lengths (string l1, string l2) {
  return env->sub_lengths (l1, l2); }

string
edit_typeset_rep::max_lengths (string l1, string l2) {
  return env->max_lengths (l1, l2); }

string
edit_typeset_rep::min_lengths (string l1, string l2) {
  return env->min_lengths (l1, l2); }

string
edit_typeset_rep::multiply_length (double x, string l) {
  return env->multiply_length (x, l); }

bool
edit_typeset_rep::is_length (string s) {
  return env->is_length (s); }

double
edit_typeset_rep::divide_lengths (string l1, string l2) {
  return env->divide_lengths (l1, l2); }

/******************************************************************************
* Processing preamble
******************************************************************************/

void
use_modules (tree t) {
  if (is_tuple (t))
    for (int i=0; i<N(t); i++) {
      // style caches written before the names were checked can hold
      // anything, so they are checked again here
      use_module (as_string (t[i]));
    }
}

void
edit_typeset_rep::typeset_style_use_cache (tree style) {
  style= preprocess_style (style, buf->buf->master);
  //cout << "Typesetting style using cache " << style << LF;
  bool ok;
  hashmap<string,tree> H;
  tree t;
  style_get_cache (style, H, t, ok);
  if (ok) {
    env->patch_env (H);
    ok= drd->set_locals (t);
    drd->set_environment (H);
  }
  if (!ok) {
    //cout << "Typeset without cache " << style << LF;
    if (!is_tuple (style)) FAILED ("tuple expected as style");
    H= get_style_env (style);
    drd= get_style_drd (style);
    style_set_cache (style, H, drd->get_locals ());
    env->patch_env (H);
    drd->set_environment (H);
  }
  use_modules (env->read (THE_MODULES));
}

// Presentation mode (toggle-full-screen-mode) hides the informative flags
// in the view which is full screen only: its typesetting uses info-flag
// none, while the document's own info-flag (in init, shared by the views
// of the buffer and saved) is not changed (#549). full-screen-mode is a
// variable of the view (is_view_init_variable).
static bool
hides_info_flags (hashmap<string,tree> init) {
  return init->contains ("full-screen-mode") &&
         init ["full-screen-mode"] == "true";
}

void
edit_typeset_rep::typeset_preamble () {
  env->write_default_env ();
  typeset_style_use_cache (the_style);
  env->update ();
  env->read_env (stydef);
  env->patch_env (init);
  if (hides_info_flags (init)) env->write (INFO_FLAG, "none");
  env->update ();
  hashmap<string,tree> h (UNINIT);
  env->read_env (h);
  set_pre (h);
  drd->heuristic_init (pre);
}

// pre is only changed by these, which tell typeset_prepare that what it
// kept is not the environment for it any more
void
edit_typeset_rep::set_pre (hashmap<string,tree> h) {
  pre= h;
  pre_version++;
}

void
edit_typeset_rep::set_pre (string var, tree val) {
  // (val uninit: reset; pre is not changed in place: see write_prepared_env)
  pre= copy (pre);
  if (val == tree (UNINIT)) pre->reset (var);
  else pre (var)= val;
  pre_version++;
}

int typeset_caches_mode (); // see typesetter.hpp
void typeset_cache_mismatch (string what);

void
edit_typeset_rep::typeset_bind_project () {
  // the tables of the project to which the buffer is attached now
  bool prj= buf->prj != NULL;
  env->global_ref= &(prj? buf->prj->data->ref: grefs);
  env->global_aux= &(prj? buf->prj->data->aux: buf->data->aux);
  env->global_att= &(prj? buf->prj->data->att: buf->data->att);
}

void
edit_typeset_rep::typeset_prepare () {
  if (env->base_file_name != buf->buf->master) {
    // the buffer was renamed: its trust is the one of its new name
    env->base_file_name= buf->buf->master;
    env->cur_file_name = buf->buf->master;
    env->secure= is_secure (env->cur_file_name);
  }
  typeset_bind_project ();
  env->read_only= buf->buf->read_only;
  // pre is the complete environment after the style and the initial
  // settings, so patching the default environment with it rewrites thousands
  // of variables; this happens at every typesetting pass and every get-env.
  // Keep the patched environment and only redo the updates of the variables
  // whose assignment has side effects on the edit_env state (which are not
  // made again by style_init_env and update: active_vars). The table is
  // made the kept one again by writing back the variables written since
  // (edit_env_rep::write_prepared_env).
  // TEXMACS_TYPESET_CACHES "off" patches it all the same, and "check"
  // does both and compares.
  int mode= typeset_caches_mode ();
  if (prep_version != pre_version || mode == 0) {
    env->write_prepared_env (pre);
    env->read_env (prep_env);
    prep_active= array<string> ();
    env->active_vars (pre, prep_active);
    prep_version= pre_version;
  }
  else {
    env->write_prepared_env (pre, prep_env);
    for (int i=0; i<N(prep_active); i++) env->update (prep_active[i]);
    if (mode == 2) {
      // (and the table written back is used again afterwards, so that it
      // is what is checked at the next pass)
      hashmap<string,tree> table= env->prepared_table ();
      hashmap<string,tree> kept (UNINIT), made (UNINIT);
      env->read_env (kept);
      env->write_default_env ();
      env->patch_env (pre);
      env->read_env (made);
      if (kept != made) typeset_cache_mismatch ("initial environment");
      env->put_back_table (table);
    }
  }
  env->style_init_env ();
  env->update ();
}

void
edit_typeset_rep::init_update () {
  typeset_bind_project ();
  if (buf->prj != NULL) {
    string id= as_string (delta (buf->prj->buf->name, buf->buf->name));
    string lab= "part:" * id;
    hashmap<string,tree> aux= *env->global_aux;
    hashmap<string,tree> ref= *env->global_ref;
    if (aux->contains ("parts")) {
      tree parts= aux ["parts"];
      if (is_func (parts, DOCUMENT))
        for (int i=0; i<N(parts); i++)
          if (is_tuple (parts[i]) && N(parts[i]) >= 1 &&
              is_atomic (parts[i][0]))
            if (parts[i][0] == id ||
                relative (buf->prj->buf->name,
                          url_unix (parts[i][0]->label)) == buf->buf->name) {
              lab= "part:" * parts[i][0]->label;
              for (int j=1; j+1 < N(parts[i]); j+=2)
                if (is_atomic (parts[i][j])) {
                  buf->data->init (parts[i][j]->label)= copy (parts[i][j+1]);
                  init (parts[i][j]->label)= copy (parts[i][j+1]);
                }
            }
    }
    if (ref->contains (lab)) {
      tree val= ref [lab];
      if (is_tuple (val) && N(val) >= 2 && val[1] != tree (UNINIT)) {
        buf->data->init (PAGE_FIRST)= copy (val[1]);
        init (PAGE_FIRST)= copy (val[1]);
      }
    }
  }
  else if (buf->data->init ["part-flag"] == "true")
    grefs= copy (buf->data->ref);
}

void
edit_typeset_rep::drd_update () {
  typeset_exec_until (tp);
  drd->heuristic_init (cur[tp]);
}

#ifdef EXPERIMENTAL
void
edit_typeset_rep::environment_update () {
  hashmap<string,tree> h;
  typeset_prepare ();
  env->assign ("base-file-name", as_string (env->base_file_name));
  env->assign ("cur-file-name", as_string (env->cur_file_name));
  env->assign ("secure", bool_as_tree (env->secure));
  env->read_env (h);
  ::primitive (ste, h);
}
#endif

/******************************************************************************
* Routines for getting information
******************************************************************************/

void
edit_typeset_rep::typeset_invalidate_env () {
  cur= hashmap<path,hashmap<string,tree> > (hashmap<string,tree> (UNINIT));
}

static void
restricted_exec (edit_env env, tree t, int end) {
  if (is_func (t, ASSIGN, 2) && end == 2)
    env->exec (t);
  else if (is_document (t) || is_concat (t))
    for (int i=0; i < min (end, 10); i++)
      restricted_exec (env, t[i], arity (t[i]));
  else if (is_compound (t, "hide-preamble", 1) ||
           is_compound (t, "show-preamble", 1))
    env->exec (t[0]);
  else if (is_compound (t, "script-input", 4) && end == 2) {
    if (env->read (MODE) == "text") {
      env->write (MODE, "prog");
      env->write (PROG_LANGUAGE, t[0]);
    }
  }
}

static tree
filter_format (tree fm, int i, int n) {
  array<tree> r;
  for (int k=0; k<N(fm); k++)
    if (is_func (fm[k], CWITH) && N(fm[k]) >= 4 &&
        is_int (fm[k][0]) && is_int (fm[k][1])) {
      int j1= as_int (fm[k][0]->label);
      int j2= as_int (fm[k][1]->label);
      if (j1 > 0) j1--; else j1 += n;
      if (j2 > 0) j2--; else j2 += n;
      if (i >= j1 && i <= j2) r << fm[k] (2, N(fm[k]));
    }
  return tree (TFORMAT, r);
}

static void
table_descend (tree& t, path& p, tree& fm) {
  while (!is_nil (p)) {
    if (L(t) == TFORMAT && p->item == N(t) - 1) {
      array<tree> r;
      for (int k=0; k<N(t)-1; k++)
        if (is_func (t[k], CWITH, 6) &&
            is_atomic (t[k][4]) &&
            !starts (t[k][4]->label, "cell-"))
          r << t[k];
      fm= tree (TFORMAT, r);
      t= t[N(t)-1];
      p= p->next;
    }
    else if ((L(t) == TABLE || L(t) == ROW) &&
             p->item >= 0 && p->item < N(t)) {
      fm= filter_format (fm, p->item, N(t));
      t= t[p->item];
      p= p->next;
    }
    else break;
  }
}

static void
define_style_macros (edit_env& env, tree t) {
  if (is_document (t) || is_concat (t)) {
    int i, n=N(t);
    for (i=0; i<n; i++)
      define_style_macros (env, t[i]);
  }
  else if (is_func (t, ASSIGN, 2) && is_atomic (t[0]))
    env->write (t[0]->label, t[1]);
}

void
edit_typeset_rep::typeset_exec_until (path p) {
  // FIXME: we should ensure that p is inside the document
  // if (!(rp <= p)) p= correct_cursor (et, rp * 0);

  //time_t t1= texmacs_time ();
  if (has_changed (THE_TREE + THE_ENVIRONMENT))
    if (p != correct_cursor (et, rp * 0)) {
      if (DEBUG_STD) std_warning << "resynchronizing for path " << p << "\n";
      // apply_changes ();
    }
  if (p == tp && inside_graphics (true) && p != closest_inside (et, p)) {
    //cout << "TeXmacs] Warning: corrected cursor\n";
    tp= closest_inside (et, tp);
    p = tp;
  }

  //cout << "Exec until " << p << LF;
  if (N(cur[p])!=0) return;
  if (N(cur)>=25) // avoids out of memory in weird cases
    typeset_invalidate_env ();
  typeset_prepare ();
  if (enable_fastenv) {
    if (!(rp < p)) {
      failed_error << "Erroneous path " << p << "\n";
      FAILED ("invalid typesetting path");
    }
    tree t= subtree (et, rp);
    path q= path_up (p / rp);
    while (!is_nil (q)) {
      int i= q->item;
      restricted_exec (env, t, i);
      if (L(t) == TFORMAT && i == N(t) - 1) {
        tree fm= tree (TFORMAT);
        table_descend (t, q, fm);
        if (!is_nil (q))
          for (int k=0; k<N(fm); k++)
            if (is_func (fm[k], CWITH, 2))
              env->write (fm[k][0]->label, fm[k][1]);
      }
      else {
        tree w= drd->get_env_child (t, i, tree (ATTR));
        if (w == "") break;
        //cout << "t= " << t << "\n";
        //cout << "i= " << i << "\n";
        //cout << "w= " << w << "\n";
        tree ww (w, N(w));
        for (int j=0; j<N(w); j+=2) {
          //cout << w[j] << " := " << env->exec (w[j+1]) << "\n";
          ww[j+1]= env->exec (w[j+1]);
        }
        for (int j=0; j<N(w); j+=2)
          env->write (w[j]->label, ww[j+1]);
        t= t[i];
        q= q->next;
      }
    }
    if (env->read (PREAMBLE) == "true")
      env->write (MODE, "src");
  }
  else exec_until (ttt, p / rp);
  if (env->read (MODE) == "src" && env->read (PREAMBLE) != "true")
    define_style_macros (env, subtree (et, rp));
  env->read_env (cur (p));
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset_exec_until took " << t2-t1 << "ms\n";
}

tree
edit_typeset_rep::get_full_env () {
  typeset_exec_until (tp);
  return (tree) cur[tp];
}

bool
edit_typeset_rep::defined_at_cursor (string var) {
  typeset_exec_until (tp);
  return cur[tp]->contains (var);
}

tree
edit_typeset_rep::get_env_value (string var, path p) {
  typeset_exec_until (p);
  tree t= cur[p][var];
  return is_func (t, BACKUP, 2)? t[0]: t;
}

tree
edit_typeset_rep::get_env_value (string var) {
 /* FIXME: tp is wrong (and consequently, crashes TeXmacs)
  *   when we call this routine from inside the code which
  *   is triggered by a button, for example.
  *
  * Test: fire TeXmacs, then open a new Graphics, then click
  *   on the icon for going in spline mode. Then it crashes,
  *   because we call (get-env-tree) from inside the Scheme.
  *   If we call (get-env-tree-at ... (cDr (cursor-path))),
  *   then it works.
  */
  return get_env_value (var, tp);
}

bool
edit_typeset_rep::defined_at_init (string var) {
  if (init->contains (var)) return true;
  if (N(pre)==0) typeset_preamble ();
  return pre->contains (var);
}

bool
edit_typeset_rep::defined_in_init (string var) {
  return init->contains (var);
}

tree
edit_typeset_rep::get_init_value (string var) {
  if (init->contains (var)) {
    tree t= init [var];
    if (var == BG_COLOR && is_func (t, _PATTERN)) t= env->exec (t);
    return is_func (t, BACKUP, 2)? t[0]: t;
  }
  if (N(pre)==0) typeset_preamble ();
  tree t= pre [var];
  // (the document's value, not the one of presentation mode)
  if (var == INFO_FLAG && hides_info_flags (init)) t= stydef [var];
  if (var == BG_COLOR && is_func (t, _PATTERN)) t= env->exec (t);
  return is_func (t, BACKUP, 2)? t[0]: t;
}

string
edit_typeset_rep::get_env_string (string var) {
  return as_string (get_env_value (var));
}

string
edit_typeset_rep::get_init_string (string var) {
  return as_string (get_init_value (var));
}

int
edit_typeset_rep::get_env_int (string var) {
  return as_int (get_env_value (var));
}

int
edit_typeset_rep::get_init_int (string var) {
  return as_int (get_init_value (var));
}

double
edit_typeset_rep::get_env_double (string var) {
  return as_double (get_env_value (var));
}

double
edit_typeset_rep::get_init_double (string var) {
  return as_double (get_init_value (var));
}

color
edit_typeset_rep::get_env_color (string var) {
  return named_color (as_string (get_env_value (var)));
}

color
edit_typeset_rep::get_init_color (string var) {
  return named_color (as_string (get_init_value (var)));
}

language
edit_typeset_rep::get_env_language () {
  string mode= get_env_string (MODE);
  if (mode == "text")
    return text_language (get_env_string (LANGUAGE));
  else if (mode == "math")
    return math_language (get_env_string (MATH_LANGUAGE));
  else return prog_language (get_env_string (PROG_LANGUAGE));
}

int
edit_typeset_rep::get_page_count () {
  return N (eb[0]);
}

SI
edit_typeset_rep::get_page_width (bool deco) {
  (void) get_env_string (PAGE_WIDTH);
  return (env->get_page_width (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_pages_width (bool deco) {
  (void) get_env_string (PAGE_WIDTH);
  return (env->get_pages_width (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_page_height (bool deco) {
  (void) get_env_string (PAGE_HEIGHT);
  return (env->get_page_height (deco) + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_total_width (bool deco) {
  SI w= eb->w();
  if (!deco) {
    SI w1= env->get_pages_width (false);
    SI w2= env->get_pages_width (true);
    w -= (w2 - w1);
  }
  return (w + std_shrinkf - 1) / std_shrinkf;
}

SI
edit_typeset_rep::get_total_height (bool deco) {
  SI h= eb->h();
  if (!deco) {
    SI h1= env->get_page_height (false);
    SI h2= env->get_page_height (true);
    int nr= get_page_count ();
    int nx= env->page_packet;
    int ny= ((nr + env->page_offset) + nx - 1) / nx;
    h -= ny * (h2 - h1);
  }
  return (h + std_shrinkf - 1) / std_shrinkf;
}

/******************************************************************************
* Execution without typesetting
******************************************************************************/

static tree
simplify_execed (tree t) {
  if (is_atomic (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= simplify_execed (t[i]);
  if (is_func (r, QUOTE, 1) && is_atomic (r[0]))
    return r[0];
  else return r;
}

// the conversion to the office formats keeps the references to pages, with
// their page, since the office programs compute these pages again
static bool keep_pagerefs= false;

static tree
expand_references (tree t, hashmap<string,tree> h, hashmap<string,tree> g) {
  // h: the references of the buffer, g: those of its project
  if (is_atomic (t)) return t;
  if (keep_pagerefs && is_func (t, PAGEREF) && N(t) >= 1) {
    string ref= as_string (simplify_execed (t[0]));
    if (h->contains (ref)) return tree (PAGEREF, ref, copy (h[ref][1]));
    if (g->contains (ref)) return tree (PAGEREF, ref, copy (g[ref][1]));
    return tree (PAGEREF, ref);
  }
  if (is_func (t, REFERENCE, 1) || is_func (t, PAGEREF)) {
    string ref= as_string (simplify_execed (t[0]));
    int which= is_func (t, REFERENCE, 1)? 0: 1;
    if (h->contains (ref))
      return tree (HLINK, copy (h[ref][which]), "#" * ref);
    if (g->contains (ref))
      return tree (HLINK, copy (g[ref][which]), "#" * ref);
    return tree (HLINK, "?", "#" * ref);
  }
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    r[i]= expand_references (t[i], h, g);
  return r;  
}

static void
prefix_specific (hashmap<string,tree>& H, string prefix) {
  hashmap<string,tree> R;
  iterator<string> it = iterate (H);
  while (it->busy()) {
    string key= it->next ();
    string var= prefix * key;
    if (H->contains (var)) R(key)= H[var];
    else R(key)= H[key];
  }
  H= R;
}

tree
edit_typeset_rep::exec (tree t, hashmap<string,tree> H, bool expand_refs) {
  hashmap<string,tree> H2;
  env->read_env (H2);
  env->write_env (H);
  t= env->exec (t);
  if (expand_refs)
    t= expand_references (t, buf->data->ref, *env->global_ref);
  t= simplify_execed (t);
  t= simplify_correct (t);
  env->write_env (H2);
  return t;
}

tree
edit_typeset_rep::exec_texmacs (tree t, path p) {
  typeset_exec_until (p);
  return exec (t, cur[p]);
}

tree
edit_typeset_rep::exec_texmacs (tree t) {
  return exec_texmacs (t, rp * 0);
}

tree
edit_typeset_rep::exec_verbatim (tree t, path p) {
  t= convert_OTS1_symbols_to_universal_encoding (t);
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  H ("TeXmacs")= tree (MACRO, "TeXmacs");
  H ("LaTeX")= tree (MACRO, "LaTeX");
  H ("TeX")= tree (MACRO, "TeX");
  return exec (t, H);
}

tree
edit_typeset_rep::exec_verbatim (tree t) {
  return exec_verbatim (t, rp * 0);
}

static tree
search_doc_title (tree t) {
  if (is_atomic (t)) return "";
  else if (is_compound (t, "doc-title", 1)) return t[0];
  else {
    for (int i=0; i<N(t); i++) {
      tree r= search_doc_title (t[i]);
      if (r != "") return r;
    }
    return "";
  }
}

static tree
expand_includes (tree t) {
  // replace include by include* so that the included files are
  // expanded in the environment of the main document.
  // Only do this for files in the directory of the main document:
  // the relative urls inside other included files would be resolved
  // against the wrong directory; tmhtml-include handles those.
  if (is_atomic (t)) return t;
  else if (is_func (t, INCLUDE, 1) && is_atomic (t[0]) &&
           is_atomic (url_unix (t[0]->label)))
    return tree (VAR_INCLUDE, t[0]);
  else {
    // only copy the tree if some include was actually rewritten
    int i, n= N(t);
    for (i=0; i<n; i++) {
      tree c= expand_includes (t[i]);
      if (!strong_equal (c, t[i])) {
        tree r (t, n);
        for (int j=0; j<i; j++) r[j]= t[j];
        r[i]= c;
        for (i++; i<n; i++) r[i]= expand_includes (t[i]);
        return r;
      }
    }
    return t;
  }
}

tree
edit_typeset_rep::exec_html (tree t, path p) {
  t= convert_OTS1_symbols_to_universal_encoding (t);
  t= expand_includes (t);
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(stree->tree (tmhtml-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  prefix_specific (H, "tmhtml-");
  tree w (WITH);
  tree doc_title= search_doc_title (t);
  if (doc_title != "")
    w << string ("html-doc-title") << doc_title;
  if (H->contains ("html-title"))
    w << string ("html-title") << H["html-title"];
  if (H->contains ("html-css"))
    w << string ("html-css") << H["html-css"];
  if (H->contains ("html-head-javascript"))
    w << string ("html-head-javascript") << H["html-head-javascript"];
  if (H->contains ("html-head-javascript-src"))
    w << string ("html-head-javascript-src") << H["html-head-javascript-src"];
  if (H->contains ("html-head-favicon"))
    w << string ("html-head-favicon") << H["html-head-favicon"];
  if (H->contains ("html-extra-css"))
    w << string ("html-extra-css") << H["html-extra-css"];
  if (H->contains ("html-extra-javascript-src"))
    w << string ("html-extra-javascript-src")
      << H["html-extra-javascript-src"];
  if (H->contains ("html-extra-javascript"))
    w << string ("html-extra-javascript") << H["html-extra-javascript"];
  if (H->contains ("html-site-version"))
    w << string ("html-site-version") << H["html-site-version"];
  if (N(w) == 0) return exec (t, H);
  else {
    w << t;
    return exec (w, H);
  }
  //tree r= exec (t, H);
  //cout << "In: " << t << "\n";
  //cout << "Out: " << r << "\n";
  //return r;
}

tree
edit_typeset_rep::exec_html (tree t) {
  return exec_html (t, rp * 0);
}

// The expansion of a document for its conversion to Markdown: as for Html,
// the macros of its styles are expanded, except those of the list
// tmmarkdown-env-patch (progs/convert/markdown/tmmarkdown-expand.scm), which
// the converter understands: the structure of the document rather than its
// layout. A style may define a macro tmmarkdown-foo, which is then foo for
// this conversion.

tree
edit_typeset_rep::exec_markdown (tree t, path p) {
  t= convert_OTS1_symbols_to_universal_encoding (t);
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(stree->tree (tmmarkdown-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  prefix_specific (H, "tmmarkdown-");
  return exec (t, H);
}

tree
edit_typeset_rep::exec_markdown (tree t) {
  return exec_markdown (t, rp * 0);
}

/******************************************************************************
* The same for the office formats (.docx, .odt): the macros which their
* converter knows are those of (tmoffice-env-patch).
******************************************************************************/

tree
edit_typeset_rep::exec_office (tree t, path p) {
  t= convert_OTS1_symbols_to_universal_encoding (t);
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  tree patch= as_tree (eval ("(stree->tree (tmoffice-env-patch))"));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  prefix_specific (H, "tmoffice-");
  keep_pagerefs= true;
  tree r= exec (t, H);
  keep_pagerefs= false;
  return r;
}

tree
edit_typeset_rep::exec_office (tree t) {
  return exec_office (t, rp * 0);
}

static tree
value_to_compound (tree t, hashmap<string,tree> h) {
  if (is_atomic (t)) return t;
  else if (is_func (t, VALUE, 1) &&
	   is_atomic (t[0]) &&
	   h->contains (t[0]->label))
    return compound (t[0]->label);
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= value_to_compound (t[i], h);
    return r;
  }
}

tree
edit_typeset_rep::exec_latex (tree t, path p) {
  t= convert_OTS1_symbols_to_universal_encoding (t);
  bool expand_unknown_macros= "on" == as_string (
      call ("get-preference", "texmacs->latex:expand-macros"));
  bool expand_user_macro= "on" == as_string (
      call ("get-preference", "texmacs->latex:expand-user-macros"));
  if (!expand_unknown_macros && !expand_user_macro)
    return t;
  if (p == (rp * 0)) typeset_preamble ();
  typeset_exec_until (p);
  hashmap<string,tree> H= copy (cur[p]);
  object l= null_object ();
  iterator<string> it= iterate (H);
  while (it->busy ()) l= cons (object (it->next ()), l);
  tree patch= as_tree (call ("stree->tree", call ("tmtex-env-patch", t, l)));
  hashmap<string,tree> P (UNINIT, patch);
  H->join (P);
  prefix_specific (H, "tmlatex-");

  if (!expand_user_macro &&
      is_document (t) && is_compound (t[0], "hide-preamble")) {
    tree r= copy (t);
    r[0]= "";
    r= exec (value_to_compound (r, P), H, false);
    r[0]= exec (t[0], H, false);
    return r;
  }
  else {
    tree r= exec (value_to_compound (t, P), H, false);
    return r;
  }
}

tree
edit_typeset_rep::exec_latex (tree t) {
  return exec_latex (t, rp * 0);
}

tree
edit_typeset_rep::texmacs_exec (tree t) {
  return ::texmacs_exec (env, t);
}

tree
edit_typeset_rep::var_texmacs_exec (tree t) {
  typeset_exec_until (tp);
  env->write_env (cur[tp]);
  env->update_frame ();
  return texmacs_exec (t);
}

/******************************************************************************
* Wrappers for editing animations
******************************************************************************/

tree
edit_typeset_rep::checkout_animation (tree t) {
  path p= search_upwards (ANIM_STATIC);
  if (is_nil (p)) p= search_upwards (ANIM_DYNAMIC);
  if (is_nil (p)) p= search_upwards ("anim-edit");
  if (!is_nil (p)) {
    typeset_exec_until (p);
    env->write_env (cur[p]);
  }
  return env->checkout_animation (t);
}

tree
edit_typeset_rep::commit_animation (tree t) {
  path p= search_upwards ("anim-edit");
  if (is_nil (p)) p= search_upwards (ANIM_STATIC);
  if (is_nil (p)) p= search_upwards (ANIM_DYNAMIC);
  if (!is_nil (p)) {
    typeset_exec_until (p);
    env->write_env (cur[p]);
  }
  return env->commit_animation (t);
}

/******************************************************************************
* Initialization
******************************************************************************/

// The style is a property of the buffer, as its initial environment: a
// change made in one view goes to the buffer's data, from which new views
// start, and to the other views of the buffer, which are marked modified
// too (#535).

static void
style_buffer_views (tm_buffer buf, tree style) {
  // the buffer's data and all its views get the style @style; the view
  // which made the change has it already. An editor whose view was deleted
  // (delete_view) has no buffer.
  if (buf == NULL) return;
  buf->data->style= copy (style);
  for (int i=0; i<N(buf->vws); i++)
    buf->vws[i]->ed->change_style_local (style);
}

void
edit_typeset_rep::change_style (tree t) {
  if (the_style == t) return;
  change_style_local (t);
  style_buffer_views (buf, t);
}

void
edit_typeset_rep::change_style_local (tree t) {
  // the change of change_style in one view of the buffer
  if (the_style == t) return;
  the_style= copy (t);
  require_save ();
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_style () {
  notify_change (THE_ENVIRONMENT);
}

void
edit_typeset_rep::init_style (string name) {
  tree st;
  if ((name == "none") || (name == "") || (name == "style")) st= TUPLE;
  else if (arity (the_style) == 0) st= tree (TUPLE, name);
  else st= tree (TUPLE, name) * the_style (1, N(the_style));
  change_style (st);
  require_save ();
  notify_change (THE_ENVIRONMENT);
}

tree
edit_typeset_rep::get_init_all () {
  return (tree) init;
}

// The initial environment is a property of the buffer: a change made in
// one view goes to the buffer's data, from which new views start, and to
// the other views of the buffer, which are marked modified too (#528).
// The variables of the window (is_view_init_variable: its size, zoom and
// full screen mode) stay those of the view, and do not modify the buffer.

static void
init_buffer_views (tm_buffer buf, string var, tree by, bool reset) {
  // the buffer's data and all its views get the value @by of @var, or lose
  // @var when @reset; the view which made the change has it already, so
  // that init_env_local and init_default_local do nothing in it. An editor
  // whose view was deleted (delete_view) has no buffer.
  if (buf == NULL) return;
  if (reset) buf->data->init->reset (var);
  else buf->data->init (var)= copy (by);
  for (int i=0; i<N(buf->vws); i++)
    if (reset) buf->vws[i]->ed->init_default_local (var);
    else buf->vws[i]->ed->init_env_local (var, copy (by));
}

void
edit_typeset_rep::init_env (string var, tree by) {
  if (init (var) == by) return;
  init (var)= by;
  if (is_view_init_variable (var)) {
    notify_change (THE_ENVIRONMENT);
    return;
  }
  require_save ();
  notify_change (THE_ENVIRONMENT);
  init_buffer_views (buf, var, by, false);
}

void
edit_typeset_rep::init_env_local (string var, tree by) {
  // the change of init_env in one view of the buffer (init_buffer_views)
  if (init (var) == by) return;
  init (var)= by;
  require_save ();
  notify_change (THE_ENVIRONMENT);
}

// A change of the zoom: only the size of a pixel depends on it (get_pixel
// in edit_env_rep), so with the inputs of the bridges the document is not
// typeset from scratch, and what does not read it is used again. The
// classic cache (TEXMACS_TYPESET_READS=off) typesets it all again.
void
edit_typeset_rep::set_zoom (tree z) {
  if (init (ZOOM_FACTOR) == z) return;
  if (typeset_inputs_mode () == 0) {
    init_env (ZOOM_FACTOR, z);
    return;
  }
  init (ZOOM_FACTOR)= z;
  set_pre (ZOOM_FACTOR, z);
  notify_change (THE_TREE);
}

void
edit_typeset_rep::init_default (string var) {
  if (!init->contains (var)) return;
  init_default_local (var);
  if (is_view_init_variable (var)) return;
  init_buffer_views (buf, var, tree (UNINIT), true);
}

void
edit_typeset_rep::init_default_local (string var) {
  // the change of init_default in one view of the buffer
  if (!init->contains (var)) return;
  init->reset (var);
  set_pre (var, stydef->contains (var)? stydef[var]: tree (UNINIT));
  if (!is_view_init_variable (var)) require_save ();
  notify_change (THE_ENVIRONMENT);
}

/******************************************************************************
* Actual typesetting
******************************************************************************/

void
edit_typeset_rep::typeset_sub (SI& x1, SI& y1, SI& x2, SI& y2) {
  //time_t t1= texmacs_time ();
  typeset_prepare ();
  eb= empty_box (reverse (rp));
  // a document without any paragraph has no position for the cursor (a
  // key pressed in it loops, #704): it holds an empty paragraph, as the
  // recovery of a typesetting failure used to leave it
  if (is_func (subtree (et, rp), DOCUMENT, 0)) {
    assign (rp, tree (DOCUMENT, ""));
    ::notify_assign (ttt, path(), subtree (et, rp));
  }
  // saves memory
  bench_start ("typeset");
#ifdef USE_EXCEPTIONS
  try {
#endif
    eb= ::typeset (ttt, x1, y1, x2, y2);
#ifdef USE_EXCEPTIONS
  }
  catch (string msg) {
    the_exception= msg;
    std_error << "Typesetting failure, resetting to empty document\n";
    // the text of the document is not lost silently: a copy of it is saved
    // and the user is told (see typeset-failure-recover in tm-files.scm)
    (void) call ("typeset-failure-recover", buf->buf->name, msg);
    assign (rp, tree (DOCUMENT, ""));
    ::notify_assign (ttt, path(), subtree (et, rp));
    eb= ::typeset (ttt, x1, y1, x2, y2);    
  }
  handle_exceptions ();
#endif
  bench_end ("typeset");
  ttt->collect= false;
  //time_t t2= texmacs_time ();
  //if (t2 - t1 >= 10) cout << "typeset took " << t2-t1 << "ms\n";
  picture_cache_clean ();
}

static void
report_missing (hashmap<string,tree> missing) {
  array<string> a;
  for (iterator<string> it= iterate (missing); it->busy(); a << it->next ()) {}
  merge_sort (a);
  for (int i=0; i<N(a); i++)
    if (!starts (a[i], "bib-"))
      typeset_warning << "Undefined reference " << a[i] << LF;
}

static void
report_redefined (array<tree> redefined) {
  for (int i=0; i<N(redefined); i++) {
    tree t= redefined[i];
    if (t[1]->label == "")
      typeset_warning << "Redefined " << t[0]->label << LF;
    else
      typeset_warning << "Redefined " << t[0]->label
                      << " as " << t[1]->label << LF;
  }
}

// Removes the references which the last complete pass did not define, and
// tells how many of them make one more pass necessary. A reference to one
// of them was typeset with its old value during that pass (lookup_value
// reads local_ref first), and was not reported as missing: one more pass
// gives it "?" (#417).
// The first time, the table is the one which was loaded with the file, and
// holds entries which no label of the document defines any more (old
// names, labels of other versions): they matter only if a reference to
// them was typeset since (read_refs). Afterwards, an entry is there because
// a pass defined it, and a bridge which was kept may have read it (#408).
static int
clean_unused (edit_env env) {
  hashmap<string,tree>& refs= env->local_ref;
  bool settled= env->refs_settled;
  array<string> a;
  int matter= 0;
  for (iterator<string> it= iterate (refs); it->busy(); ) {
    string key= it->next ();
    if (!env->touched [key]) {
      a << key;
      if (settled || env->read_refs->contains (key)) matter++;
    }
  }
  for (int i=0; i<N(a); i++)
    refs->reset (a[i]);
  env->refs_settled= true;
  env->read_refs= hashmap<string,bool> (false);
  return matter;
}

/******************************************************************************
* Passes
*
* edit_typeset_rep::typeset makes passes until the references are settled.
* A pass is of one of three kinds:
* - complete, from scratch: every bridge is made again (::notify_assign of
*   the root, or a new root: the first typesetting, a new view, a revert);
*   it makes the tables of the references and the auxiliary tables, and
*   finds the pages of the labels;
* - complete with the bridges used again (ttt->collect, #427, only with the
*   inputs of the bridges): those used again write their bindings and their
*   entries of the auxiliary tables again, those which looked up a
*   reference whose entry changed are typeset again;
* - incomplete: only what changed is typeset again (each edit), or a
*   preamble is shown or parts are hidden.
* Who cleans the table of the references (local_ref): clean_unused after a
* complete pass, which removes what that pass did not define (#417), and
* typesetter_rep::drop_bindings after an incomplete pass with the inputs of
* the bridges, which removes what no bridge wrote any more (#571).
* Who notices that a binding got another value, so that what read the old
* one is typeset again: env->redefined after a complete pass, or a label
* removed by clean_unused (one more complete pass: with the inputs of the
* bridges a collect pass for a redefined label, from scratch for a removed
* one); env->rebound after an incomplete pass of the classic cache (one
* pass from scratch, #576, complete unless a preamble is shown or parts
* are hidden); ttt->bindings_changed after an
* incomplete pass with the inputs of the bridges (one more incomplete pass,
* which typesets again what looked the binding up, #571, also for a binding
* which was added or removed). The passes for changed bindings share one
* limit, REBINDING_PASSES_MAX: one pass is enough, unless what it typesets
* again changes bindings in turn (a label made only when another one has
* some value; a binding which feeds on itself would go on forever).
******************************************************************************/

#define REBINDING_PASSES_MAX 4

// After a pass which is not complete: whether it changed bindings which
// were read before (see "Passes" above), and then the next pass is
// prepared; nr counts these passes, complete tells that the classic cache
// makes a complete one
static bool
another_pass_for_bindings (edit_env env, typesetter ttt,
                           int& nr, bool& complete) {
  complete= false;
  if (nr >= REBINDING_PASSES_MAX) return false;
  if (typeset_inputs_mode () != 0) {
    // the incomplete pass which follows typesets again what looked up a
    // binding which is not the same (the stamp of their lookups)
    if (!ttt->bindings_changed) return false;
    nr++;
    return true;
  }
  // The classic cache (TEXMACS_TYPESET_READS=off) does not know what read
  // a binding: when a pass which typeset only what changed gave another
  // value to a binding (set-binding, a label renumbered), what read the
  // old one (a reference, the part of a surround, #576) is typeset again
  // by a complete pass, as for a redefined label after a complete pass,
  // and only once (the whole limit)
  if (!env->rebound) return false;
  nr= REBINDING_PASSES_MAX;
  complete= true;
  ::notify_assign (ttt, path(), ttt->br->st);
  return true;
}

void
edit_typeset_rep::typeset (SI& x1, SI& y1, SI& x2, SI& y2) {
  int missing_nr= INT_MAX;
  int redefined_nr= INT_MAX;
  int rebinding_nr= 0;
  bool rebound_pass= false;
  x1= MAX_SI; y1= MAX_SI; x2= MIN_SI; y2= MIN_SI;
  while (true) {
    SI sx1, sy1, sx2, sy2;
    typeset_sub (sx1, sy1, sx2, sy2);
    x1= min (x1, sx1); y1= min (y1, sy1);
    x2= max (x2, sx2); y2= max (y2, sy2);
    if (!env->complete) {
      bool complete;
      if (another_pass_for_bindings (env, ttt, rebinding_nr, complete)) {
        if (complete) rebound_pass= true;
        continue;
      }
      break;
    }
    env->complete= false;
    // a removed label is treated like a redefined one: one more pass
    // (still bounded by the test below, which stops when the counts of
    // missing and redefined references no longer decrease)
    int removed= clean_unused (env);
    if (N(env->missing) == 0 && N(env->redefined) == 0 && removed == 0) break;
    // (the complete pass made for a binding given another value: the
    // missing references were missing before it, and are not a reason for
    // one more pass, and they were not reported, as after any pass which
    // is not complete)
    if (rebound_pass && N(env->redefined) == 0 && removed == 0) break;
    rebound_pass= false;
    if ((N(env->missing) == missing_nr && N(env->redefined) == redefined_nr) ||
        (N(env->missing) > missing_nr || N(env->redefined) > redefined_nr)) {
      report_missing (env->missing);
      report_redefined (env->redefined);
      break;
    }
    missing_nr= N(env->missing);
    redefined_nr= N(env->redefined);
    // (a complete pass again: the next one must collect the references,
    // the page numbers and the auxiliary tables, which a pass that uses
    // bridges again does not.) With the inputs of the bridges and no
    // removed label, it uses the bridges again (collect): those which
    // looked up a reference whose table entry changed since are typeset
    // again (the stamp of their lookups), the others are used again and
    // tell the references they found missing (replay_missing)
    if (typeset_inputs_mode () != 0 && removed == 0) ttt->collect= true;
    else ::notify_assign (ttt, path(), ttt->br->st);
  }
}

void
edit_typeset_rep::typeset_forced () {
  //cout << "Typeset forced\n";
  SI x1, y1, x2, y2;
  typeset (x1, y1, x2, y2);
  // the changes are typeset here, so the next typesetting (apply_changes)
  // finds none and repaints nothing: their rectangle is repainted now
  SI px= get_pixel_size ();
  invalidate (x1- 2*px, y1- 2*px, x2+ 2*px, y2+ 2*px);
}

void
edit_typeset_rep::typeset_invalidate (path p) {
  if (rp <= p) {
    //cout << "Invalidate " << p << "\n";
    notify_change (THE_TREE);
    ::notify_assign (ttt, p / rp, subtree (et, p));
  }
}

void
edit_typeset_rep::typeset_invalidate_all () {
  //cout << "Invalidate all\n";
  notify_change (THE_ENVIRONMENT);
  typeset_preamble ();
  ::notify_assign (ttt, path(), subtree (et, rp));
}

// The labels are to be numbered again. With the inputs of the bridges,
// only the bridges which make labels are typeset again (and those whose
// environment changes in consequence): they read auto-epoch (exec_auto_id
// in env_exec.cpp), which is changed. The classic cache typesets it all
// again.
void
edit_typeset_rep::typeset_invalidate_labels () {
  static int epoch= 0;
  if (typeset_inputs_mode () == 0) {
    typeset_invalidate_all ();
    return;
  }
  set_pre ("auto-epoch", as_string (++epoch));
  notify_change (THE_TREE);
}

// The next typesetting is a complete pass which uses the bridges again
// (the auxiliary tables, the page numbers of the references and the
// missing references are those of a typesetting from scratch, see
// typesetter_rep::collect). For the end of Update All, which comes after
// the layout was made from scratch (inclusions-gc) and the tables were
// regenerated in the document: only what the edits changed is typeset
// again. The classic cache typesets it all again.
void
edit_typeset_rep::typeset_invalidate_keep () {
  if (typeset_inputs_mode () == 0) {
    typeset_invalidate_all ();
    return;
  }
  ttt->collect= true;
  notify_change (THE_TREE);
}

// The files of some included documents changed (Update All, Tools > Update >
// Inclusions: the cache of the inclusions holds other trees for them, see
// refresh_inclusions). The include is an input of the bridge of the include
// (rewrite_include in env_exec.cpp), so that the next typesetting types
// again the includes of these files, and uses the layout of the rest. The
// classic cache types it all again.
void
edit_typeset_rep::typeset_invalidate_inclusions () {
  if (typeset_inputs_mode () == 0) {
    typeset_invalidate_all ();
    return;
  }
  notify_change (THE_TREE);
}

// The pictures were loaded again (Tools > Update > Pictures): what is on the
// screen is drawn again from the files, and the images whose size changed
// are typeset again, the others being used again (the size of an image is
// an input of the bridge of the image, see typeset_image in
// Concat/concat_active.cpp). The classic cache types it all again.
void
edit_typeset_rep::typeset_invalidate_pictures () {
  if (typeset_inputs_mode () == 0) {
    typeset_invalidate_all ();
    return;
  }
  if (typeset_pictures_used ()) notify_change (THE_TREE);
  invalidate_all ();
}

// The automatic labels are numbered again, in the order of the document
// (exec_auto_id in env_exec.cpp), which is to be done before the tables
// of contents and the indexes are made again: these hold the names of
// the labels. All the views of the buffer have the same labels. Tells
// whether a name changed.
bool
edit_typeset_rep::typeset_renumber_labels () {
  // (the tree which holds the numbers to come: see exec_auto_id)
  tree root= is_nil (rp) ? et : et[rp->item];
  array<int> before= auto_labels_numbers (root);
  if (!auto_labels_renumber (root)) return false;
  // (a change of the environment which is waiting, such as the one of
  // inclusions-gc, makes apply_changes typeset all again: it is done here,
  // by the pass below, which is then not to be done twice)
  bool pending= (typeset_inputs_mode () != 0 && has_changed (THE_ENVIRONMENT));
  if (pending) typeset_invalidate_all ();
  for (int i=0; i<N(buf->vws); i++)
    ((tm_view) buf->vws[i])->ed->typeset_invalidate_labels ();
  // one pass, for the names: the references are not yet those of the
  // tables to come, and the document is typeset again after them. It is a
  // complete pass, which makes the auxiliary tables (generate_aux uses
  // them), also when the bridges which make no labels are used again
  SI x1, y1, x2, y2;
  ttt->collect= (typeset_inputs_mode () != 0);
  typeset_sub (x1, y1, x2, y2);
  ttt->collect= false;
  if (env->complete) {
    env->complete= false;
    // the references to a removed label are typeset again by the
    // typesetting which follows (see typeset above)
    if (clean_unused (env) != 0)
      ::notify_assign (ttt, path(), ttt->br->st);
  }
  if (pending) {
    // what the pass made is on screen only after a repaint
    notify_change (THE_TREE, THE_ENVIRONMENT);
    SI px= get_pixel_size ();
    invalidate (x1- 2*px, y1- 2*px, x2+ 2*px, y2+ 2*px);
    invalidate_all ();
  }
  return auto_labels_numbers (root) != before;
}

void
edit_typeset_rep::typeset_invalidate_players (path p, bool reattach) {
  if (rp <= p) {
    tree t= subtree (et, p);
    blackbox bb;
    bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
    if (ok) {
      if (reattach) tree_addendum_delete (t, ADDENDUM_PLAYER);
      typeset_invalidate (p);
    }
    if (is_compound (t)) {
      int i, n= N(t);
      for (i=0; i<n; i++)
        typeset_invalidate_players (p * i, reattach);
    }
  }
}
