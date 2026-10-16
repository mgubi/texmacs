
/******************************************************************************
* MODULE     : env.cpp
* DESCRIPTION: the environment of the math_editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "sys_utils.hpp" // get_env
#include <unordered_map>
extern hashmap<string,int> default_var_type;
void initialize_default_var_type ();
extern hashmap<string,tree> default_env;
void initialize_default_env ();
#include "page_type.hpp"
extern int typeset_check_mismatches;

/******************************************************************************
* Initialization
******************************************************************************/

edit_env_rep::edit_env_rep (drd_info& drd2,
			    url base_file_name2,
			    hashmap<string,tree>& local_ref2,
			    hashmap<string,tree>& global_ref2,
			    hashmap<string,tree>& local_aux2,
			    hashmap<string,tree>& global_aux2,
			    hashmap<string,tree>& local_att2,
			    hashmap<string,tree>& global_att2):
  drd (drd2),
  env (UNINIT), back (UNINIT), src (path (DECORATION)),
  var_type (default_var_type),
  base_file_name (base_file_name2),
  cur_file_name (base_file_name2),
  secure (is_secure (base_file_name2)),
  local_ref (local_ref2), global_ref (global_ref2),
  local_aux (local_aux2), global_aux (global_aux2),
  local_att (local_att2), global_att (global_att2),
  missing (UNINIT), redefined (), touched (false),
  read_unknown (false), rec_keys (NULL), rec_values (NULL),
  rec_key_set (NULL), rec_bindings (NULL), rec_merged (NULL), rec_held (NULL),
  auto_buffer (""), typesetting (0), files_base (""), files_cur (""),
  files_h (0), prep_pre (UNINIT), prep_fp (0), prep_sfp (0),
  prep_sfp_version (-1)
{
  env.back_log= &back;
  initialize_default_env ();
  initialize_default_var_type ();
  env= copy (default_env);
  style_init_env ();
  update ();
  complete= false;
  recover_env= tuple ();
  anim_start= anim_end= anim_portion= 0.0;
}

edit_env::edit_env (drd_info& drd,
		    url base_file_name,
		    hashmap<string,tree>& local_ref,
		    hashmap<string,tree>& global_ref,
		    hashmap<string,tree>& local_aux,
		    hashmap<string,tree>& global_aux,
		    hashmap<string,tree>& local_att,
		    hashmap<string,tree>& global_att):
  rep (tm_new<edit_env_rep> (drd, base_file_name,
                             local_ref, global_ref,
                             local_aux, global_aux,
                             local_att, global_att)) {}

void
edit_env_rep::style_init_env () {
  dpi = get_int (DPI);
  inch= ((double) dpi*PIXEL);
  flexibility= get_double (PAGE_FLEXIBILITY);
  first_page= get_double (PAGE_FIRST);
  back= hashmap<string,tree> (UNINIT);
  update_page_pars ();
}

/******************************************************************************
* Modification of environment variables
******************************************************************************/

/*
void
edit_env_rep::set (string s, tree t) {
  // cout << "Set " << s << " := " << t << "\n";
  tree r= exec (t); // Replacing r by exec(t) below yields bug when optimizing
  // cout << "Do " << s << " := " << r << "\n";
  monitored_write (s, tree (BACKUP, r, read(s)));
  update (s);
}

void
edit_env_rep::reset (string s) {
  // cout << "Reset " << s << "\n";
  if (is_func (read(s), BACKUP, 2)) monitored_write (s, read(s)[1]);
  update (s);
}

void
edit_env_rep::assign (string s, tree t) {
  // cout << "Assign " << s << " := " << t << "\n";
  t= exec (t);
  // cout << "Do     " << s << " := " << t << "\n";
  if (is_func (read(s), BACKUP, 2)) {
    if (read(s)[0] != t) {
      monitored_write (s, tree (BACKUP, t, read(s)[1]));
      update (s);
    }
  }
  else {
    if (read(s) != t) {
      monitored_write (s, t);
      update (s);
    }
  }
}
*/

tree
edit_env_rep::local_begin_extents (box b) {
  tree old= tree (TUPLE,
		  env ["w-length"], env ["h-length"],
		  env ["l-length"], env ["b-length"],
		  env ["r-length"], env ["t-length"]);
  env ("w-length")= as_string (b->w ()) * "tmpt";
  env ("h-length")= as_string (b->h ()) * "tmpt";
  env ("l-length")= as_string (b->x1) * "tmpt";
  env ("b-length")= as_string (b->y1) * "tmpt";
  env ("r-length")= as_string (b->x2) * "tmpt";
  env ("t-length")= as_string (b->y2) * "tmpt";
  return old;
}

void
edit_env_rep::local_end_extents (tree t) {
  env ("w-length")= t[0];
  env ("h-length")= t[1];
  env ("l-length")= t[2];
  env ("b-length")= t[3];
  env ("r-length")= t[4];
  env ("t-length")= t[5];
}

/******************************************************************************
* Global manipulations of the environment
******************************************************************************/

// The value of a binding or of an attachment (see rec_keys in env.hpp),
// and the same with a record of it
tree
edit_env_rep::lookup_value (string name) {
  if (N(name) == 0) return tree (UNINIT);
  string key= name (1, N(name));
  if (name[0] == 'a')
    return local_att->contains (key)? local_att [key]: global_att [key];
  int type= (name[0] == '1') ? 1 : 0;
  tree value= local_ref->contains (key)? local_ref [key]: global_ref [key];
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= tree (UNINIT);
  return value;
}

tree
edit_env_rep::lookup (string name) {
  tree value= lookup_value (name);
  record_lookup (name, value);
  return value;
}

// (a fingerprint rather than the values: a bridge keeps it, and there are
// some 250 such variables)
unsigned long long
edit_env_rep::derived_state () {
  static int known= -1, known_derived= -1;
  if (known != N(var_type) || known_derived != N(env_derived_inputs ())) {
    // the variables which are not plain, and the plain variables from
    // which a state was derived
    hashmap<string,bool> vars (false);
    iterator<string> it= iterate (var_type);
    while (it->busy ()) {
      string var= it->next ();
      if (!plain_variable (var)) vars (var)= true;
    }
    for (iterator<int> it2= iterate (env_derived_inputs ()); it2->busy (); )
      vars (env_var_name (it2->next ()))= true;
    env_state_vars ()= vars;
    env_state_version++;
    known= N(var_type);
    known_derived= N(env_derived_inputs ());
  }
  return env.state_fingerprint () ^ files_hash ();
}

// the files, which are not variables: the bindings record the file in
// which they are made, relative to the one of the buffer (a buffer saved
// under another name), and images or links are found from them
unsigned long long
edit_env_rep::files_hash () {
  tree b= base_file_name->t, c= cur_file_name->t;
  if (b.operator-> () != files_base.operator-> () ||
      c.operator-> () != files_cur.operator-> ()) {
    files_base= b; files_cur= c;
    files_h= env_entry_hash ("\1base-file-name", base_file_name->t) ^
             env_entry_hash ("\1cur-file-name", cur_file_name->t);
  }
  return files_h;
}

hashmap<string,bool>&
env_state_vars () {
  static hashmap<string,bool> vars (false);
  return vars;
}

int env_state_version= 0;

int env_deriving= 0;

int env_id_depth= 0;
unsigned long long env_id_count= 0;

env_id_level*
env_id_level_at (int depth) {
  static array<env_id_level*> levels;
  while (N(levels) <= depth) {
    env_id_level* lv= tm_new<env_id_level> ();
    lv->marks= NULL; lv->n= 0;
    levels << lv;
  }
  return levels[depth];
}

void
env_id_grow (env_id_level* lv, int id) {
  int n= (lv->n == 0? 256: lv->n);
  while (n <= id) n <<= 1;
  unsigned long long* m= tm_new_array<unsigned long long> (n);
  for (int i=0; i<n; i++) m[i]= (i < lv->n? lv->marks[i]: 0);
  if (lv->marks != NULL) tm_delete_array (lv->marks);
  lv->marks= m; lv->n= n;
}

hashmap<int,bool>&
env_derived_inputs () {
  static hashmap<int,bool> ids (false);
  return ids;
}

// a number for each name of variable (see env_table), and back
static hashmap<string,int>&
var_ids () {
  static hashmap<string,int> ids (-1);
  return ids;
}

static array<string>&
var_names () {
  static array<string> names;
  return names;
}

int
env_var_id (string var) {
  int id= var_ids () [var];
  if (id < 0) {
    id= N(var_names ());
    var_ids () (var)= id;
    var_names () << var;
  }
  return id;
}

string
env_var_name (int id) {
  return var_names () [id];
}

void
edit_env_rep::write_default_env () {
  env= copy (default_env);
}

void
edit_env_rep::write_prepared_env (hashmap<string,tree> pre) {
  // The default table and the table of the style, at the start of each
  // pass: the same from one pass to the next while 'pre' is the same
  // table (it is not changed in place: edit_typeset_rep::init_default),
  // so that the fingerprints of the result are kept, and the table is not
  // hashed again (in the check mode, it is, and they are compared)
  write_default_env ();
  patch_env (pre);
  if (typeset_inputs_mode () == 0) return;
  bool known= prep_pre.operator-> () == pre.operator-> ();
  if (known && typeset_inputs_mode () != 2) {
    env.adopt (prep_fp, prep_sfp, prep_sfp_version);
    return;
  }
  env.state_fingerprint ();
  if (known && (env.fp != prep_fp || (env.sfp_version == prep_sfp_version &&
                                      env.sfp != prep_sfp))) {
    typeset_check_mismatches++;
    failed_error << "The table of the style has another fingerprint "
                 << "than before" << LF;
  }
  prep_pre= pre;
  prep_fp= env.fp;
  prep_sfp= env.sfp;
  prep_sfp_version= env.sfp_version;
}

void
edit_env_rep::write_env (hashmap<string,tree> user_env) {
  env= copy (user_env);
}

// (env_absent () in a patch: the variable goes)
void
edit_env_rep::monitored_patch_env (hashmap<string,tree> patch) {
  if (patch->size == 0) return;
  int i=0, n=patch->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=patch->a[i];
    for (; !is_nil(l); l=l->next)
      if (strong_equal (l->item.im, env_absent ())) {
        env.log_back (l->item.key);
        env.remove (l->item.key);
        update (l->item.key);
      }
      else monitored_write_update (l->item.key, l->item.im);
  }
}

void
edit_env_rep::patch_env (hashmap<string,tree> patch) {
  if (patch->size == 0) return;
  int i=0, n=patch->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=patch->a[i];
    for (; !is_nil(l); l=l->next)
      if (strong_equal (l->item.im, env_absent ())) {
        env.remove (l->item.key);
        update (l->item.key);
      }
      else write_update (l->item.key, l->item.im);
  }
}

/******************************************************************************
* The fingerprint of the environment (see env_table::fp)
******************************************************************************/

static inline unsigned long long
mix64 (unsigned long long h) {
  h= (h ^ (h >> 30)) * 0xBF58476D1CE4E5B9ULL;
  h= (h ^ (h >> 27)) * 0x94D049BB133111EBULL;
  return h ^ (h >> 31);
}

static inline unsigned long long
string_hash64 (string s) {
  unsigned long long h= 0xCBF29CE484222325ULL;
  for (int i=0; i<N(s); i++) h= (h ^ (unsigned char) s[i]) * 0x100000001B3ULL;
  return mix64 (h);
}

// the hashes of the compound values, by tree (which the memo holds, so
// that another tree does not come at the same place). Values are made
// again at each pass (tuples, copies of macros...): the memo has two
// generations, the recent entries and the older ones, and the older ones
// which are not used again go when the recent ones are many. The values
// used at every pass, as those of the style, stay.
struct value_hash_entry {
  tree t; unsigned long long h;
  value_hash_entry (): t (), h (0) {}
  value_hash_entry (tree t2, unsigned long long h2): t (t2), h (h2) {}
};
typedef std::unordered_map<void*,value_hash_entry> value_hash_map;
static value_hash_map value_hashes, value_hashes_old;
#define VALUE_HASHES_MAX 65536

static unsigned long long
value_hash64 (tree t) {
  if (is_atomic (t)) return string_hash64 (t->label);
  void* key= (void*) t.operator-> ();
  auto it= value_hashes.find (key);
  if (it != value_hashes.end ()) return it->second.h;
  unsigned long long h;
  auto old= value_hashes_old.find (key);
  if (old != value_hashes_old.end ()) h= old->second.h;
  else {
    h= mix64 (0x9E3779B97F4A7C15ULL + (int) L(t));
    for (int i=0; i<N(t); i++) h= mix64 (h * 31 + value_hash64 (t[i]));
  }
  if (value_hashes.size () >= VALUE_HASHES_MAX) {
    value_hashes_old.swap (value_hashes);
    value_hashes.clear ();
  }
  value_hashes[key]= value_hash_entry (t, h);
  return h;
}

unsigned long long
edit_env_rep::labels_stamp () {
  return mix64 (local_ref->stamp ()) ^
         mix64 (global_ref->stamp () * 0x9E3779B97F4A7C15ULL + 1) ^
         mix64 (local_att->stamp () * 0x9E3779B97F4A7C15ULL + 2) ^
         mix64 (global_att->stamp () * 0x9E3779B97F4A7C15ULL + 3);
}

unsigned long long
env_entry_hash (string var, tree val) {
  return mix64 (string_hash64 (var) * 0x9E3779B97F4A7C15ULL ^
                value_hash64 (val));
}

unsigned long long
env_input_hash (int id, tree val) {
  return mix64 (((unsigned long long) id + 1) * 0x9E3779B97F4A7C15ULL ^
                value_hash64 (val));
}

unsigned long long
edit_env_rep::start_hash (int id) {
  string var= env_var_name (id);
  tree* before= back->find (var);
  return env_input_hash (id, before == NULL? env.value (var): *before);
}

unsigned long long
edit_env_rep::inputs_hash (array<int> vars, bool at_start) {
  unsigned long long h= 0;
  for (int i=0; i<N(vars); i++)
    h ^= (at_start? start_hash (vars[i]):
          env_input_hash (vars[i], env.value (env_var_name (vars[i]))));
  return h;
}

unsigned long long
env_table::fingerprint () {
  if (stale) {
    stale= false;
    fp= 0;
    sfp_version= -1;  // (sfp is made again by state_fingerprint)
    dirty= array<string> ();
    dirty_set= hashmap<string,bool> (false);
    for (iterator<string> it= iterate (h); it->busy (); ) {
      string var= it->next ();
      fp ^= env_entry_hash (var, h [var]);
    }
    return fp;
  }
  hashmap<string,bool>& state= env_state_vars ();
  for (int i=0; i<N(dirty); i++)
    if (h->contains (dirty[i])) {
      unsigned long long e= env_entry_hash (dirty[i], h [dirty[i]]);
      fp ^= e;
      if (state->contains (dirty[i])) sfp ^= e;
    }
  if (N(dirty) != 0) {
    dirty= array<string> ();
    dirty_set= hashmap<string,bool> (false);
  }
  return fp;
}

unsigned long long
env_table::state_fingerprint () {
  if (typeset_inputs_mode () == 0) {
    // (the fingerprints are not kept in the classic mode: from the values)
    unsigned long long r= 0;
    for (iterator<string> it= iterate (env_state_vars ()); it->busy (); ) {
      string var= it->next ();
      if (h->contains (var)) r ^= env_entry_hash (var, h [var]);
    }
    return r ^ mix64 (0x9E3779B97F4A7C15ULL + env_state_version);
  }
  fingerprint ();
  if (sfp_version != env_state_version) {
    // the variables of the state changed: sfp is made again (also when
    // writes in between went in and out under different sets)
    sfp= 0;
    for (iterator<string> it= iterate (env_state_vars ()); it->busy (); ) {
      string var= it->next ();
      if (h->contains (var)) sfp ^= env_entry_hash (var, h [var]);
    }
    sfp_version= env_state_version;
  }
  return sfp ^ mix64 (0x9E3779B97F4A7C15ULL + env_state_version);
}

void
env_table::refingerprint () {
  // (made again when it is asked for: see 'stale')
  stale= true;
  fp= 0;
  sfp= 0;
  sfp_version= -1;
  if (N(dirty) != 0) {
    dirty= array<string> ();
    dirty_set= hashmap<string,bool> (false);
  }
}

int
typeset_inputs_mode () {
  static int mode= -1;
  if (mode < 0) {
    string s= get_env ("TEXMACS_TYPESET_READS");
    mode= (s == "off"? 0: (s == "check"? 2: 1));
  }
  return mode;
}

tree
env_absent () {
  static tree marker (TUPLE, "absent-variable");
  return marker;
}

void
edit_env_rep::read_env (hashmap<string,tree>& ret) {
  ret= copy ((hashmap<string,tree>) env); // (all of it: see env_table)
}

void
edit_env_rep::local_start (hashmap<string,tree>& prev_back) {
  prev_back= back;
  back= hashmap<string,tree> (UNINIT);  
}

void
edit_env_rep::local_changes (hashmap<string,tree>& change) {
  // the variables which have another value than before the bridge, with
  // the value now (env_absent () for a variable which is not there now,
  // when the inputs are used: a variable which was not there is not one
  // with the value UNINIT)
  if (typeset_inputs_mode () == 0) { change= invert (back, env.h); return; }
  change= hashmap<string,tree> (UNINIT);
  int i=0, n=back->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=back->a[i];
    for (; !is_nil(l); l=l->next) {
      tree before= l->item.im, now= env.value (l->item.key);
      bool same= strong_equal (before, now) ||
                 (!strong_equal (before, env_absent ()) &&
                  !strong_equal (now, env_absent ()) && before == now);
      if (!same) change (l->item.key)= now;
    }
  }
}

void
edit_env_rep::local_update (hashmap<string,tree>& old_patch,
			    hashmap<string,tree>& change)
{
  if (typeset_inputs_mode () != 0) {
    // (old_patch is not used: see "What is typeset again" in bridge.cpp)
    local_changes (change);
    return;
  }
  old_patch->pre_patch (back, env.h);
  old_patch->post_patch (change, env.h);
  change= invert (back, env.h);
}

void
edit_env_rep::local_end (hashmap<string,tree>& prev_back) {
  int i=0, n=back->n;
  for (; i<n; i++) {
    list<hashentry<string,tree> > l=back->a[i];
    for (; !is_nil(l); l=l->next)
      prev_back->write_back (l->item.key, back);
  }
  back= prev_back;
}

tm_ostream&
operator << (tm_ostream& out, edit_env env) {
  return out << env->env.h;
}
