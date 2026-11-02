
/******************************************************************************
* MODULE     : bridge_surround.cpp
* DESCRIPTION: Bridge between logical and physical paragraph surroundings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

class bridge_surround_rep: public bridge_rep {
protected:
  bridge               body;
  hashmap<string,tree> changes_before;

  bool                 corrupted;
  array<line_item>     a;
  array<line_item>     b;
  // when the inputs are used: the parts are typeset again when the
  // environment (its fingerprint) or what they looked up is otherwise, and
  // kept otherwise (the bridges below compare them as boxes)
  bool                 parts_ok;
  unsigned long long   parts_fp;
  bool                 parts_zoom;   // whether they read the zoom factor,
  tree                 parts_zoom_at; // at this zoom (see env_table::zoom)
  array<string>        parts_keys;
  array<tree>          parts_values;
  unsigned long long   parts_stamp;  // (see same_lookups)
  bool                 parts_known;  // and what they read, as the inputs of
  array<int>           parts_vars;   // a bridge (they are typeset first in
  unsigned long long   parts_vals;   // the record of the surround)
  array<tree>          parts_bindings; // the bindings they wrote

public:
  bridge_surround_rep (typesetter ttt, tree st, path ip);
  void initialize ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int level, path p, tree u);
  void notify_change ();

  void my_clean_links ();
  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
  bool parts_again ();
  // (while the parts are typeset under a record of their own)
  array<string> nested_keys; array<tree> nested_values;
  array<int> nested_vars; unsigned long long nested_vals;
  bool nested_known, nested_zoom;
};

bridge_surround_rep::bridge_surround_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), changes_before (UNINIT),
  parts_ok (false), parts_fp (0), parts_zoom (false), parts_zoom_at (""),
  parts_stamp (0), parts_known (false), nested_vals (0), nested_known (false),
  nested_zoom (false)
{
  initialize ();
}

void
bridge_surround_rep::initialize () {
  while (N(st)<3) // hack for temporarily incorrect situations (A-backspace)
    st= tree (SURROUND, "") * st;
  if (is_nil (body)) body= make_bridge (ttt, st[2], descend (ip, 2));
  else replace_bridge (body, st[2], descend (ip, 2));
  changes_before= hashmap<string,tree> (UNINIT);
  corrupted= true;
}

bridge
bridge_surround (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_surround_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_surround_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, SURROUND), "nil path");
  if (is_nil (p) || (p->item != 2)) {
    st= substitute (st, p, u);
    initialize ();
  }
  else {
    // bool mp_flag= is_multi_paragraph (st);
    if (is_atom (p)) {
      body= make_bridge (ttt, u, descend (ip, 2));
      st= substitute (st, 2, body->st);
    }
    else {
      body->notify_assign (p->next, u);
      st= substitute (st, p->item, body->st);
    }
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_surround_rep::notify_insert (path p, tree u) {
  // cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 2)) bridge_rep::notify_insert (p, u);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_insert (p->next, u);
    st= substitute (st, 2, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

void
bridge_surround_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p) || (p->item != 2)) bridge_rep::notify_remove (p, nr);
  else {
    // bool mp_flag= is_multi_paragraph (st);
    body->notify_remove (p->next, nr);
    st= substitute (st, 2, body->st);
    // if (mp_flag != is_multi_paragraph (st)) initialize ();
  }
  status= CORRUPTED;
}

bool
bridge_surround_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  /*
  cout << "Macro argument " << var << " [action=" << tp
       << ", level=" << l << "] " << p << ", " << u << " in " << st << "\n";
  */

  bool flag= env->depends (st[0], var, l) || env->depends (st[1], var, l);
  if (flag) initialize ();
  flag= body->notify_macro (tp, var, l, p, u) || flag;
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_surround_rep::notify_change () {
  status= CORRUPTED;
  corrupted= true;
  body->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_surround_rep::my_clean_links () {
  // the loci and links of the parts: kept with the parts (nothing writes
  // the environment before my_typeset, which asks the same question)
  if (parts_again ()) link_env= link_repository (true);
}

bool
bridge_surround_rep::parts_again () {
  if (typeset_inputs_mode () == 0)
    return corrupted || (N(ttt->old_patch) != 0);
  return corrupted || !parts_ok || env->fingerprint () != parts_fp ||
         ((parts_zoom || env_zoom_derived ()) &&
          env->zoom_value () != parts_zoom_at) ||
         !same_lookups (env, parts_keys, parts_values, parts_stamp);
}

void
bridge_surround_rep::my_exec_until (path p) {
  if (p->item == 2) {
    env->patch_env (changes_before);
    body->exec_until (p->next);
  }
  else env->exec_until (st, p);
}

bool
bridge_surround_rep::my_typeset_will_be_complete () {
  if (status != CORRUPTED) return false;
  return body->my_typeset_will_be_complete ();
}

void
bridge_surround_rep::my_typeset (int desired_status) {
  if (parts_again ()) {
    bool inputs= (typeset_inputs_mode () != 0);
    unsigned long long fp= (inputs? env->fingerprint (): 0);
    tree zoom_at= env->zoom_value ();
    array<string>* keys= env->rec_keys;
    int k0= (keys == NULL? 0: N(*keys));
    array<int>* vars= env->input_vars ();
    int v0= (vars == NULL? 0: N(*vars));
    unsigned long long* vh= env->input_hash ();
    unsigned long long h0= (vh == NULL? 0: *vh);
    bool all0= env->read_all (), unknown0= env->read_unknown;
    bool zoom0= env->zoom_read ();
    env->zoom_read ()= false;
    array<tree>* bs= env->rec_bindings;
    int b0= (bs == NULL? 0: N(*bs));
    hashmap<string,tree> prev_back (UNINIT);
    env->local_start (prev_back);
    /*
    cout << st[0] << "\n";
    cout << st[1] << "\n";
    cout << "-------------------------------------------------------------\n";
    */
    // A surround which does not record (it is below a paragraph) types its
    // parts under a record of its own, which it then gives to the record
    // around: the slice of that record which they added would miss what
    // the paragraph had read before.
    bool nested= (inputs && in_unrecorded && keys != NULL);
    if (nested) {
      env_id_set nreads;
      array<string> nkeys;
      array<tree> nvalues;
      array<int> nvars;
      unsigned long long nvals= 0;
      {
        edit_env_rep::read_record record (env.operator-> (), &nreads,
                                          &nkeys, &nvalues, &nvars, &nvals);
        a= typeset_concat (env, st[0], descend (ip, 0));
        b= typeset_concat (env, st[1], descend (ip, 1));
        nested_known= record.complete () && N(nvars) <= 256 && N(nkeys) <= 64;
        record.stop ();
        nested_zoom= record.zoom;
      }
      nested_keys= nkeys; nested_values= nvalues;
      nested_vars= nvars; nested_vals= nvals;
      // (the inputs first, as give_record: the reads put the variables
      // in the record, and a variable in it is not an input)
      add_inputs (env, nested_known, nvars, nvals);
      add_reads (env, nested_known && nreads.size () <= 256, nreads.ids,
                 nkeys, nvalues);
    }
    else {
      a= typeset_concat (env, st[0], descend (ip, 0));
      b= typeset_concat (env, st[1], descend (ip, 1));
    }
    env->local_update (ttt->old_patch, changes_before);
    env->local_end (prev_back);
    corrupted= false;
    bool zoom= (nested? nested_zoom: env->zoom_read ());
    env->zoom_read ()= zoom0 || zoom;
    if (nested) {
      parts_zoom= zoom;
      parts_zoom_at= zoom_at;
      parts_ok= true;
      parts_fp= fp;
      parts_keys= nested_keys;
      parts_values= nested_values;
      parts_stamp= 0;
      parts_known= nested_known;
      parts_vars= nested_vars;
      parts_vals= nested_vals;
      parts_bindings= (bs == NULL? array<tree> (): range (*bs, b0, N(*bs)));
    }
    else if (inputs) {
      parts_zoom= zoom;
      parts_zoom_at= zoom_at;
      // (what the parts looked up: added to the record of the bridge)
      parts_ok= (keys != NULL);
      parts_fp= fp;
      parts_keys= (keys == NULL? array<string> (): range (*keys, k0, N(*keys)));
      parts_values= (keys == NULL? array<tree> ():
                     range (*env->rec_values, k0, N(*keys)));
      parts_stamp= 0;
      parts_known= vars != NULL && (all0 || !env->read_all ()) &&
                   (unknown0 || !env->read_unknown);
      parts_vars= (vars == NULL? array<int> (): range (*vars, v0, N(*vars)));
      parts_vals= (vh == NULL? 0: *vh ^ h0);
      parts_bindings= (bs == NULL? array<tree> (): range (*bs, b0, N(*bs)));
    }
  }
  else {
    if (typeset_inputs_mode () != 0) {
      // the parts kept: what they read and looked up when they were made
      // goes to the record of the bridge, as for a bridge used again (and
      // before what they wrote, which they read first: a counter)
      for (int i=0; i<N(parts_keys); i++)
        env->record_lookup (parts_keys[i], parts_values[i]);
      add_inputs (env, parts_known, parts_vars, parts_vals);
      if (parts_zoom) env->zoom_read ()= true;
      env->write_bindings (parts_bindings);
      env->replay_missing (parts_keys, parts_values);
    }
    env->monitored_patch_env (changes_before);
  }

  ttt->insert_marker (st, ip);
  ttt->insert_surround (a, b);
  body->typeset (desired_status);
}
