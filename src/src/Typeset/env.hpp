
/******************************************************************************
* MODULE     : env.hpp
* DESCRIPTION: edit environment for typesetting
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ENV_H
#define ENV_H
#include "vars.hpp"
#include "drd_info.hpp"
#include "gui.hpp"
#include "font.hpp"
#include "language.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "boxes.hpp"
#include "url.hpp"
#include "frame.hpp"
#include "link.hpp"
#include "player.hpp"

#define DECORATION (-1)

/******************************************************************************
* The different types of system environment variables
******************************************************************************/

#define Env_User               0
#define Env_Fixed              1
#define Env_Zoom               2
#define Env_Magnification      3
#define Env_Magnify            4
#define Env_Language           5
#define Env_Mode               6
#define Env_Info_Level         7
#define Env_Font               8
#define Env_Font_Size          9
#define Env_Font_Sizes        10
#define Env_Index_Level       11
#define Env_Display_Style     12
#define Env_Math_Condensed    13
#define Env_Vertical_Pos      14
#define Env_Math_Nesting      15
#define Env_Math_Width        16
#define Env_Color             17
#define Env_Pattern_Mode      18
#define Env_Spacing           19
#define Env_Paragraph         20
#define Env_Page              21
#define Env_Page_Extents      22
#define Env_Preamble          23
#define Env_Geometry          24
#define Env_Frame             25
#define Env_Line_Width        26
#define Env_Grid              27
#define Env_Grid_Aspect       28
#define Env_Src_Style         29
#define Env_Src_Special       30
#define Env_Src_Compact       31
#define Env_Src_Close         32
#define Env_Src_Color         33
#define Env_Point_Style       34
#define Env_Point_Size        35
#define Env_Dash_Style        36
#define Env_Dash_Style_Unit   37
#define Env_Fill_Color        38
#define Env_Line_Arrows       39
#define Env_Line_Portion      40
#define Env_Text_At_Halign    41
#define Env_Text_At_Valign    42
#define Env_Text_At_Repulse   43
#define Env_Doc_At_Valign     44

/******************************************************************************
* For style file editing
******************************************************************************/

#define STYLE_ANGULAR         0
#define STYLE_SCHEME          1
#define STYLE_LATEX           2
#define STYLE_FUNCTIONAL      3

#define SPECIAL_RAW           0
#define SPECIAL_FORMAT        1
#define SPECIAL_NORMAL        2
#define SPECIAL_MAXIMAL       3

#define COMPACT_ALL           0
#define COMPACT_INLINE_ARGS   1
#define COMPACT_INLINE_START  2
#define COMPACT_INLINE        3
#define COMPACT_NONE          4

#define CLOSE_MINIMAL         0
#define CLOSE_COMPACT         1
#define CLOSE_LONG            2
#define CLOSE_REPEAT          3

#define INACTIVE_INLINE_RECURSE  0
#define INACTIVE_INLINE_ONCE     1
#define INACTIVE_INLINE_ERROR    2
#define INACTIVE_BLOCK_RECURSE   3
#define INACTIVE_BLOCK_ONCE      4
#define INACTIVE_BLOCK_ERROR     5

/******************************************************************************
* Other enumerated values
******************************************************************************/

#define INFO_NONE          0
#define INFO_MINIMAL       1
#define INFO_SHORT         2
#define INFO_DETAILED      3
#define INFO_PAPER         4
#define INFO_SHORT_PAPER   5

/******************************************************************************
* The edit environment
******************************************************************************/

// The variables of an environment, as a table which can record what is
// read and written of it. A bridge records the variables which the
// typesetting of its subtree reads or writes (bridge_rep::typeset): when
// the environment it gets differs from the one of the previous pass in
// other variables only, what it made then is still right. The variables
// it writes count as well: the changes it keeps are the variables whose
// value it changed, and one which it wrote with the value it had already
// is not among them (an exercise which sets the current label to its
// number, after a theorem with the same number). A read of the whole
// table cannot be recorded variable by variable: it sets 'all'. The
// variables are recorded by number (env_var_id): the sets of the bridges
// are arrays of integers.
int env_var_id (string var);
// The variables which the environment read while it derived a state from
// its variables (edit_env_rep::update): a length in a unit made by a user
// macro, the colour of a pattern... Such a variable is not "plain": when
// it changes, what was derived from it may change too (plain_variable)
extern int env_deriving;
hashmap<int,bool>& env_derived_inputs ();
inline void env_note_read (int id) {
  if (env_deriving > 0) env_derived_inputs () (id)= true; }
string env_var_name (int id);
int env_zoom_id (); // (the id of ZOOM_FACTOR)
// whether a state was derived from the zoom factor (a length of the state in
// px, as math-frac-limit: env_derived_inputs). Its value is then in the
// derived state, and every bridge is used again only at the same zoom
bool env_zoom_derived ();
// TEXMACS_TYPESET_READS: whether a bridge is used again when its inputs
// are as they were (see "What is typeset again" in bridge.cpp): 1 (the
// default), 0 ("off": the classic cache), 2 ("check")
int typeset_inputs_mode ();
// in the values before writes and in the changes of a bridge (when the
// inputs are used), a variable which was not there
tree env_absent ();
// a hash of a variable and its value, of 64 bits (see env_table::fp)
unsigned long long env_entry_hash (string var, tree val);
// the same for an input of a bridge (by env_var_id: see inputs_hash)
unsigned long long env_input_hash (int id, tree val);
// the variables of the derived state (edit_env_rep::derived_state), and
// a number which changes with them
hashmap<string,bool>& env_state_vars ();
extern int env_state_version;

// A set of variables (by env_var_id): the record of what a bridge reads.
// The variables are kept in the order in which they came, and whether one
// is there is a look in an array of marks: the sets are made and go as a
// stack (those of the bridges, whose typesetting is within the one of the
// bridge above), each with the marks of its depth, in which its own number
// marks its variables (they were hashmaps, which were about a fifth of the
// time of a typesetting of the whole document)
struct env_id_level { unsigned long long* marks; int n; };
env_id_level* env_id_level_at (int depth);
void env_id_grow (env_id_level* lv, int id);
extern int env_id_depth;
extern unsigned long long env_id_count;

class env_id_set {
  env_id_level* lv;
  unsigned long long nr;
  env_id_set (const env_id_set&);
  env_id_set& operator = (const env_id_set&);
public:
  array<int> ids;
  inline env_id_set ():
    lv (env_id_level_at (env_id_depth++)), nr (++env_id_count) {}
  inline ~env_id_set () { env_id_depth--; }
  inline bool contains (int id) { return id < lv->n && lv->marks[id] == nr; }
  inline bool insert (int id) {
    // (whether it is a new one)
    if (id >= lv->n) env_id_grow (lv, id);
    if (lv->marks[id] == nr) return false;
    lv->marks[id]= nr; ids << id; return true; }
  inline int size () { return N(ids); }
};

class env_table {
public:
  hashmap<string,tree>  h;
  env_id_set*           rec;  // the variables read or written, if recording
  bool                  all;  // the whole table was read, while recording
                              // (or something else: read_other)
  // The zoom factor was read while recording, or the whole table: the zoom
  // is not in the fingerprint (env_entry_hash), and a bridge used again
  // through the fingerprint which read it is used again only at the same
  // zoom (bridge_rep::same_inputs)
  bool                  zoom;
  // The values which the variables had before they were written, from
  // which the changes made by a bridge are found (edit_env_rep::back).
  // Every write, while a bridge is recorded, goes there: also those of a
  // binding (a with, a cell...), which puts the value back afterwards, but
  // a variable which is assigned below a binding would otherwise have the
  // bound value as its value before, and be missed when it ends with it
  hashmap<string,tree>* back_log;
  // The inputs of the bridge which is recorded: the variables which it
  // read before it wrote them, and a hash of the values which they had
  // (the exclusive or of their env_input_hash: edit_env_rep::inputs_hash)
  array<int>*           in_vars;  // (by env_var_id)
  unsigned long long*   in_hash;
  // When the inputs are used: a fingerprint of the whole table (the
  // exclusive or of env_entry_hash of its variables), kept as the table is
  // written. The variables which were written since it was brought up to
  // date ('dirty') are not in it, and are put back in by fingerprint ().
  // When the whole table was replaced ('stale': at the start of each pass,
  // whose preparation then writes all the variables of the style), the
  // writes are not followed and fingerprint () makes it again, once.
  bool                  stale;
  unsigned long long    fp;
  array<string>         dirty;
  hashmap<string,bool>  dirty_set;
  // The same for the variables of the derived state only (env_state_vars,
  // as they were for env_state_version sfp_version)
  unsigned long long    sfp;
  int                   sfp_version;
  // The variables written since the table was the prepared one, while
  // they are followed: the table is made the prepared one again by writing
  // them back (edit_env_rep::write_prepared_env), and not by a copy of it
  bool                  follow;
  array<string>         touched;
  hashmap<string,bool>  touched_set;
  inline env_table (tree init):
    h (init), rec (NULL), all (false), zoom (false), back_log (NULL),
    in_vars (NULL), in_hash (NULL), stale (true), fp (0), dirty_set (false),
    sfp (0), sfp_version (-1), follow (false), touched_set (false) {}
  inline void touch (string s) {
    if (!follow || touched_set->contains (s)) return;
    touched_set (s)= true;
    touched << s; }
  inline void will_write (string s) {
    if (stale || typeset_inputs_mode () == 0 || dirty_set->contains (s))
      return;
    if (h->contains (s)) {
      unsigned long long e= env_entry_hash (s, h [s]);
      fp ^= e;
      if (env_state_vars ()->contains (s)) sfp ^= e; }
    dirty_set (s)= true;
    dirty << s; }
  unsigned long long fingerprint ();
  unsigned long long state_fingerprint ();
  void refingerprint ();
  // (the fingerprints of a table which is known to hold the same as one
  // whose fingerprints were made: see edit_env_rep::write_prepared_env)
  inline void adopt (unsigned long long fp2, unsigned long long sfp2,
                     int sfp_version2) {
    stale= false; fp= fp2; sfp= sfp2; sfp_version= sfp_version2;
    if (N(dirty) != 0) {
      dirty= array<string> ();
      dirty_set= hashmap<string,bool> (false); } }
  inline tree value (string s) {
    // (what a variable holds, or env_absent (), without recording)
    tree* v= h->find (s);
    return v == NULL? env_absent (): *v; }
  inline void log_back (string s) {
    // the value before the first write of s
    if (back_log == NULL || (*back_log)->contains (s)) return;
    if (typeset_inputs_mode () != 0) (*back_log) (s)= value (s);
    else (*back_log)->write_back (s, h); }
  inline void note (string s, bool write, bool read= true) {
    if (write) { will_write (s); touch (s); }
    if (rec == NULL && env_deriving <= 0) return;
    int id= env_var_id (s);
    if (rec != NULL) {
      bool first= rec->insert (id);
      if (first && id == env_zoom_id ()) zoom= true;
      if (read && first && in_vars != NULL && back_log != NULL &&
          !(*back_log)->contains (s)) {
        (*in_vars) << id;
        if (in_hash != NULL) *in_hash ^= env_input_hash (id, value (s)); }
      if (write) log_back (s); }
    if (env_deriving > 0) env_note_read (id); }
  inline tree operator [] (string s) { note (s, false); return h [s]; }
  inline tree& operator () (string s) { note (s, true); return h (s); }
  // (a write which does not read the value before)
  inline tree& w (string s) { note (s, true, false); return h (s); }
  inline bool contains (string s) { note (s, false); return h->contains (s); }
  inline void remove (string s) { note (s, true); h->reset (s); }
  inline env_table& operator = (hashmap<string,tree> h2) {
    h= h2; follow= false; refingerprint (); return *this; }
  inline operator hashmap<string,tree> () {
    if (rec != NULL) { all= true; zoom= true; }
    return h; }
};

// The automatic labels of a document (exec_auto_id in env_exec.cpp): the
// numbers of the labels of 'root', in the order of the document, and a
// new start for them, to be followed by a complete typesetting, which
// gives the numbers again in the order in which the labels are made
array<int> auto_labels_numbers (tree root);
bool auto_labels_renumber (tree root);

class edit_env;
class ornament_parameters;
class art_box_parameters;
class edit_env_rep: public concrete_struct {
public:
  drd_info&                    drd;
private:
  env_table                    env;
  hashmap<string,tree>         back;
public:
  hashmap<string,path>         src;
  list<hashmap<string,tree> >  macro_arg;
  list<hashmap<string,path> >  macro_src;
  array<box>                   decorated_boxes;

  hashmap<string,int>&         var_type;
  url                          base_file_name;
  url                          cur_file_name;
  bool                         secure;
  hashmap<string,tree>&        local_ref;
  hashmap<string,tree>*        global_ref;  // the tables of the project,
  hashmap<string,tree>&        local_aux;
  hashmap<string,tree>*        global_aux;  // which may change
  hashmap<string,tree>&        local_att;
  hashmap<string,tree>*        global_att;
  bool                         complete;    // typeset complete document ?
  bool                         read_only;   // write-protected ?
  hashmap<string,tree>         missing;     // missing refs
  array<tree>                  redefined;   // redefined labels
  hashmap<string,bool>         touched;     // touched refs
  hashmap<string,bool>         read_refs;   // refs read since the tables
                                            // were loaded (see clean_unused)
  bool                         refs_settled;// tables cleaned by a pass since
  link_repository              link_env;    // current links
  array<array<int> >           size_cache;  // math font size cache
  array<rectangle>             white_zones; // text exclusion zones for curves

  int          dpi;
  double       inch;
  double       zoomf;
  SI           zoom_pixel;  // (read with get_pixel, but where it only
                            // names a cache: typeset_image)
  double       magn;
  double       magn_len;
  double       mgfy;
  double       flexibility;
  int          first_page;
  int          mode;
  int          mode_op;
  language     lan;
  int          hl_lan;
  font         fn;
  int          fn_size;
  int          index_level;
  bool         display_style;
  bool         math_condensed;
  int          vert_pos;
  SI           frac_max;
  SI           table_max;
  pencil       flatten_pen;
  int          alpha;
  pencil       pen;
  bool         no_patterns;
  bool         preamble;
  int          spacing_policy;
  tree         math_font_sizes;
  int          nesting_level;

  int          info_level;
  int          src_style;
  int          src_special;
  int          src_compact;
  int          src_close;
  string       src_tag_color;
  color        src_tag_col;
  int          inactive_mode;
  tree         recover_env;

  double       anim_start;
  double       anim_end;
  double       anim_portion;

  SI           gw;
  SI           gh;
  string       gvalign;
  frame        fr;
  point        clip_lim1;
  point        clip_lim2;
  string       point_style;
  SI           point_size;
  SI           point_border;
  array<bool>  dash_style;
  array<point> dash_motif;
  SI           dash_style_unit;
  double       dash_style_ratio;
  brush        fill_brush;
  array<tree>  line_arrows;
  double       line_portion;
  string       text_at_halign;
  string       text_at_valign;
  SI           text_at_repulse;
  string       doc_at_valign;

  string       page_type;
  string       page_real_type;
  bool         page_landscape;
  bool         page_automatic;
  bool         page_single;
  bool         page_floats;
  int          page_packet;
  int          page_offset;
  tree         page_border;
  int          page_margin_mode;
  SI           page_width;
  SI           page_height;
  SI           page_real_width;
  SI           page_real_height;
  SI           page_user_width;
  SI           page_user_height;
  SI           page_odd_margin;
  SI           page_even_margin;
  SI           page_right_margin;
  SI           page_top_margin;
  SI           page_bottom_margin;

private:
  tree exec_formatting (tree t, string v);
  void exec_until_formatting (tree t, path p, string v);
  bool exec_until_formatting (tree t, path p, string var, int l, string v);
  tree exec_table (tree t);
  void exec_until_table (tree t, path p);
  bool exec_until_table (tree t, path p, string var, int level);
  tree exec_assign (tree t);
  tree exec_provide (tree t);
  tree exec_with (tree t);
  void exec_until_with (tree t, path p);
  bool exec_until_with (tree t, path p, string var, int level);
  tree exec_drd_props (tree t);
  tree exec_compound (tree t);
  void exec_until_compound (tree t, path p);
  bool exec_until_compound (tree t, path p, string var, int level);
  tree exec_provides (tree t);
  tree exec_value (tree t);
  tree exec_quote_value (tree t);
  tree exec_or_value (tree t);
  tree exec_arg (tree t);
  bool exec_until_arg (tree t, path p, string var, int level);
  tree exec_quote_arg (tree t);
  tree exec_get_label (tree t);
  tree exec_get_arity (tree t);
  tree exec_new_theme (tree t);
  tree exec_copy_theme (tree t);
  tree exec_apply_theme_sub (string var);
  tree exec_apply_theme (tree t);
  tree exec_select_theme_sub (string theme, string from);
  tree exec_select_theme (tree t);
  tree exec_eval_args (tree t);
  bool exec_until_mark (tree t, path p, string var, int level);
  bool exec_until_quasi (tree t, path p, string var, int level);
  tree exec_quasiquoted (tree t);
  tree exec_copy (tree t);
  tree exec_if (tree t);
  bool exec_until_if (tree t, path p, string var, int level);
  tree exec_case (tree t);
  bool exec_until_case (tree t, path p, string var, int level);
  tree exec_while (tree t);
  bool exec_until_while (tree t, path p, string var, int level);
  tree exec_for_each (tree t);
  tree exec_use_package (tree t);
  tree exec_use_module (tree t);

  tree exec_or (tree t);
  tree exec_xor (tree t);
  tree exec_and (tree t);
  tree exec_not (tree t);
  tree exec_plus_minus (tree t);
  tree exec_min_max (tree t);
  tree exec_times_over (tree t);
  tree exec_divide (tree t);
  tree exec_modulo (tree t);
  tree exec_math_sqrt (tree t);
  tree exec_exp (tree t);
  tree exec_log (tree t);
  tree exec_pow (tree t);
  tree exec_cos (tree t);
  tree exec_sin (tree t);
  tree exec_tan (tree t);
  tree exec_merge (tree t);
  tree exec_length (tree t);
  tree exec_range (tree t);
  tree exec_number (tree t);
  tree exec_date (tree t);
  tree exec_translate (tree t);
  tree exec_change_case (tree t, tree nc, bool exec_flag, bool first);
  tree exec_change_case (tree t);
  tree exec_find_file (tree t);
  tree exec_find_file_upwards (tree t);
  tree exec_is_tuple (tree t);
  tree exec_lookup (tree t);
  tree exec_arg_recursive (tree t);
  tree exec_occurs_inside (tree t);
  tree exec_equal (tree t);
  tree exec_unequal (tree t);
  tree exec_less (tree t);
  tree exec_lesseq (tree t);
  tree exec_greater (tree t);
  tree exec_greatereq (tree t);
  tree exec_blend (tree t);
  tree exec_rgb_color (tree t);
  tree exec_rgb_access (tree t);

  tree exec_cm_length ();
  tree exec_mm_length ();
  tree exec_in_length ();
  tree exec_pt_length ();
  tree exec_bp_length ();
  tree exec_dd_length ();
  tree exec_pc_length ();
  tree exec_cc_length ();
  tree exec_fs_length ();
  tree exec_fbs_length ();
  tree exec_em_length ();
  tree exec_ln_length ();
  tree exec_sep_length ();
  tree exec_yfrac_length ();
  tree exec_ex_length ();
  tree exec_fn_length ();
  tree exec_fns_length ();
  tree exec_bls_length ();
  tree exec_fnbot_length ();
  tree exec_fntop_length ();
  tree exec_spc_length ();
  tree exec_xspc_length ();
  tree exec_par_length ();
  tree exec_paw_length ();
  tree exec_pag_length ();
  tree exec_gw_length ();
  tree exec_gh_length ();
  tree exec_gu_length ();
  tree exec_tmpt_length ();
  tree exec_px_length ();
  tree exec_guipx_length ();
  tree exec_lcorner_length ();
  tree exec_bcorner_length ();
  tree exec_rcorner_length ();
  tree exec_tcorner_length ();
  tree exec_ms_length ();
  tree exec_s_length ();
  tree exec_msec_length ();
  tree exec_sec_length ();
  tree exec_min_length ();
  tree exec_hr_length ();

  tree exec_hard_id (tree t);
  tree exec_script (tree t);
  tree exec_find_accessible (tree t);
  tree exec_set_binding (tree t);
  tree exec_get_binding (tree t);
  tree exec_has_binding (tree t);
  tree exec_get_attachment (tree t);
  tree exec_auto_id (tree t);
  bool auto_label_tag (tree& tag, tree& root, path& where);

  tree exec_pattern (tree t);

  tree exec_anim_static (tree t);
  tree exec_anim_dynamic (tree t);
  tree exec_morph (tree t);
  tree exec_anim_time ();
  tree exec_anim_portion ();

  tree exec_point (tree t);

  tree exec_eff_move (tree t);
  tree exec_eff_magnify (tree t);
  tree exec_eff_bubble (tree t);
  tree exec_eff_crop (tree t);
  tree exec_eff_turbulence (tree t);
  tree exec_eff_fractal_noise (tree t);
  tree exec_eff_hatch (tree t);
  tree exec_eff_dots (tree t);
  tree exec_eff_gaussian (tree t);
  tree exec_eff_oval (tree t);
  tree exec_eff_rectangular (tree t);
  tree exec_eff_motion (tree t);
  tree exec_eff_degrade (tree t);
  tree exec_eff_distort (tree t);
  tree exec_eff_gnaw (tree t);

  tree exec_box_info (tree t);
  tree exec_frame_direct (tree t);
  tree exec_frame_inverse (tree t);

  tree exec_rewrite (tree t);
  tree exec_include (tree t);
  bool exec_until_rewrite (tree t, path p, string var, int level);
  tree rewrite_inactive_arg (tree t, tree var, int i, bool bl, bool fl);
  tree rewrite_inactive_raw_data (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_document (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_concat (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_value (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_arg (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_style_with (tree t, tree var, bool b, bool f, bool o);
  tree rewrite_inactive_active (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_var_active (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_symbol (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_hybrid (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive_default (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive (tree t, tree var, bool block, bool flush);
  tree rewrite_inactive (tree t, tree var);

public:
  edit_env_rep (drd_info& drd,
		url base_file_name,
		hashmap<string,tree>& local_ref,
		hashmap<string,tree>& global_ref,
		hashmap<string,tree>& local_aux,
		hashmap<string,tree>& global_aux,
		hashmap<string,tree>& local_att,
		hashmap<string,tree>& global_att);
  void   style_init_env ();

  /* execution of trees and setting environment variables */
  tree   exec (tree t);
  void   exec_until (tree t, path p);
  bool   exec_until (tree t, path p, string var, int level);
  string exec_string (tree t);        /* should be inline */
  tree   expand (tree t, bool search_accessible= false);
  bool   depends (tree t, string s, int level);
  tree   rewrite (tree t);
  path   get_animation_ip (path ip);
  tree   animate (tree t);
  tree   checkout_animation (tree t);
  tree   commit_animation (tree t);
  tree   expand_morph (tree t);

  // The recording of the variables read (see env_table): by a bridge
  // around the typesetting of its subtree. read_unknown tells a bridge
  // that its record is not all it read (the reads of a bridge below it
  // were not known, or too many to be added).
  bool read_unknown;
  inline bool& zoom_read () { return env.zoom; }  // (see env_table)
  inline tree zoom_value () { return env.value (ZOOM_FACTOR); }
  inline env_id_set* read_recorder () { return env.rec; }
  inline bool read_all () { return env.all; }
  inline void record_reads (env_id_set* rec, bool all) {
    env.rec= rec; env.all= all; }
  // something which is not a variable is read (whatever a Scheme routine
  // looks at): the record of the variables read does not tell what the
  // result depends on
  inline void read_other () { if (env.rec != NULL) env.all= true; }
  // The references and the attachments which are looked up are recorded
  // too, with the values found: a name ("0" or "1" for the two parts of
  // a binding, "a" for an attachment, "i" for an included document, then
  // the key or the file) and a value, which lookup_value finds again
  array<string>* rec_keys;
  array<tree>*   rec_values;
  hashmap<string,bool>* rec_key_set; // (the keys of rec_keys, once each)
  inline void record_lookup (string name, tree value) {
    if (rec_keys == NULL || (*rec_key_set)->contains (name)) return;
    (*rec_key_set) (name)= true;
    (*rec_keys) << name; (*rec_values) << value; }
  // The bindings which are written (set-binding, label): what typesetting
  // gives besides boxes and the environment, which a bridge or a header
  // used again writes again, in the same order (the last one wins), as
  // tuples of the key, the value and the file or part
  array<tree>* rec_bindings;
  // The arrays of variables (the reads or the inputs of a bridge below)
  // which are all in the record already (add_reads, add_inputs): one
  // which is given again adds nothing, and is not gone through again
  // (held, so that another one is not taken for it at the same place)
  hashmap<pointer,bool>* rec_merged;
  array<array<int> >* rec_held;
  inline bool merged (array<int> a) {
    return rec_merged != NULL && (*rec_merged)->contains (a.operator-> ()); }
  inline void merging (array<int> a) {
    if (rec_merged == NULL || N(a) == 0) return;
    (*rec_merged) (a.operator-> ())= true; (*rec_held) << a; }
  void write_binding (string key, tree value, string extra);
  tree store_binding (string key, tree value, string extra);
  // An entry of an auxiliary table (<write|toc|...>: the table of contents,
  // the index...), which a complete pass makes: it is recorded with the
  // bindings, as <write|key|value>, so that a bridge used again writes it
  // again, and a complete pass need not typeset all again
  inline void store_aux (string key, tree value) {
    if (!complete) return;
    if (!local_aux->contains (key)) local_aux (key)= tree (DOCUMENT);
    local_aux (key) << value; }
  inline void write_aux (string key, tree value) {
    if (rec_bindings != NULL) (*rec_bindings) << tree (WRITE, key, value);
    store_aux (key, value); }
  inline void write_bindings (array<tree> l) {
    // (those of a bridge used again: the record above gets the same tuples,
    // which take the value stored when it is a new one, so that the next
    // time it is found by its pointer again)
    if (rec_bindings != NULL) (*rec_bindings) << l;
    for (int i=0; i<N(l); i++) {
      tree t= l[i];
      if (is_func (t, WRITE, 2)) { store_aux (t[0]->label, t[1]); continue; }
      tree stored= store_binding (t[0]->label, t[1], t[2]->label);
      if (stored.operator-> () != t[1].operator-> ()) t[1]= stored; } }
  // a record of the bindings written for the time of one typesetting,
  // added to the record around it when it stops (also on an exception)
  struct binding_record {
    edit_env_rep* env;
    array<tree>* outer;
    array<tree>* mine;
    inline binding_record (edit_env_rep* e, array<tree>* l):
      env (e), outer (e->rec_bindings), mine (l) {
        *mine= array<tree> (); env->rec_bindings= mine; }
    inline void stop () {
      if (mine == NULL) return;
      env->rec_bindings= outer;
      if (outer != NULL) (*outer) << (*mine);
      mine= NULL; }
    inline ~binding_record () { stop (); }
  };
  // The tree of the buffer which is being typeset, whose automatic labels
  // are numbered (exec_auto_id in env_exec.cpp), or a tree which is not
  // compound when there is none
  tree auto_buffer;
  struct typesetting_pass {
    edit_env_rep* env;
    tree outer;
    inline typesetting_pass (edit_env_rep* e, tree buffer):
      env (e), outer (e->auto_buffer) {
        env->typesetting++; env->auto_buffer= buffer; }
    inline ~typesetting_pass () {
      env->typesetting--; env->auto_buffer= outer; }
  };
  inline void record_lookups (array<string>* keys, array<tree>* values,
                              hashmap<string,bool>* key_set) {
    rec_keys= keys; rec_values= values; rec_key_set= key_set; }
  // A record of what is read for the time of one typesetting (of a bridge,
  // of a header): the record around it is the current one again when it
  // stops, and at the latest when it goes, also when an exception ends the
  // typesetting
  struct read_record {
    edit_env_rep* env;
    env_id_set* outer_rec;
    bool outer_all, outer_unknown, outer_zoom;
    bool zoom;  // (once stopped: the zoom factor was read, see env_table)
    bool stopped;
    array<string>* outer_keys;
    array<tree>* outer_values;
    hashmap<string,bool>* outer_key_set;
    hashmap<string,bool> key_set;
    array<int>* outer_in_vars;
    unsigned long long* outer_in_hash;
    hashmap<pointer,bool>* outer_merged;
    array<array<int> >* outer_held;
    hashmap<pointer,bool> merged;
    array<array<int> > held;
    inline read_record (edit_env_rep* e, env_id_set* rec,
                        array<string>* keys, array<tree>* values,
                        array<int>* in_vars= NULL,
                        unsigned long long* in_hash= NULL):
      env (e), outer_rec (e->env.rec), outer_all (e->env.all),
      outer_unknown (e->read_unknown), outer_zoom (e->env.zoom), zoom (false),
      stopped (false),
      outer_keys (e->rec_keys), outer_values (e->rec_values),
      outer_key_set (e->rec_key_set), key_set (false),
      outer_in_vars (e->env.in_vars), outer_in_hash (e->env.in_hash),
      outer_merged (e->rec_merged), outer_held (e->rec_held),
      merged (false) {
        env->record_reads (rec, false);
        env->record_lookups (keys, values, &key_set);
        env->env.in_vars= in_vars; env->env.in_hash= in_hash;
        env->rec_merged= &merged; env->rec_held= &held;
        env->read_unknown= false; env->env.zoom= false; }
    // nothing was read but what the record holds
    inline bool complete () { return !env->read_all () && !env->read_unknown; }
    inline void stop () {
      if (stopped) return;
      zoom= env->env.zoom;
      env->env.zoom= outer_zoom || zoom;
      env->record_reads (outer_rec, outer_all);
      env->record_lookups (outer_keys, outer_values, outer_key_set);
      env->env.in_vars= outer_in_vars; env->env.in_hash= outer_in_hash;
      env->rec_merged= outer_merged; env->rec_held= outer_held;
      env->read_unknown= outer_unknown;
      stopped= true; }
    inline ~read_record () { stop (); }
  };
  tree lookup_value (string name);
  tree lookup (string name);
  // The picture which an image or a pattern shows ("p" or "q", the base file
  // and the name as written in the document: picture_key), and the size
  // which the typesetting took from it, as a lookup (see rec_keys): the
  // pictures loaded again (Tools > Update > Pictures) type again the images
  // whose file or size changed, and nothing else
  url  image_file (url base, string label);
  url  pattern_file (url base, string label);
  string picture_key (char kind, string label);
  tree picture_value (string name);
  tree picture_lookup (char kind, string label, url file, int w, int h);
  // a fingerprint of the values of the variables which are not plain
  unsigned long long derived_state ();
  // a copy of the table of the variables (not recorded as a read)
  inline hashmap<string,tree> all_values () { return copy (env.h); }
  // a variable whose value is all there is to it: no state of the
  // environment is derived from it when it is written (the types for
  // which update (string) does nothing) or when another one is (it was
  // not read while a state was derived: env_derived_inputs)
  inline bool plain_variable (string s) {
    // (the zoom factor too: what is derived from it, the size of a pixel,
    // is read with get_pixel, which counts as a read of it)
    int type= var_type [s];
    return (type == Env_User || type == Env_Fixed || type == Env_Zoom ||
            type == Env_Paragraph || type == Env_Page) &&
           !env_derived_inputs ()->contains (env_var_id (s)); }
  // whether the environment is in the course of a typesetting pass (the
  // automatic labels: exec_auto_id), and the guard which says so
  int typesetting;

  inline void monitored_write (string s, tree t) {
    env.log_back (s); env.w (s)= t; }
  inline void monitored_write_update (string s, tree t) {
    env.log_back (s); env.w (s)= t; update (s); }
  inline void write (string s, tree t) { env.w (s)= t; }
  inline void write_update (string s, tree t) { env.w (s)= t; update (s); }
  inline tree local_begin (string s, tree t) {
    // tree r (env [s]); monitored_write_update (s, t); return r;
    tree& val= env (s); tree r (val); val= t; update (s); return r; }
  inline void local_end (string s, tree t) {
     env.w (s)= t; update (s); }
  inline tree local_begin_script () {
    return local_begin (MATH_LEVEL, as_string (index_level+1)); }
  inline void local_end_script (tree t) {
    local_end (MATH_LEVEL, t); }
  inline void assign (string s, tree t) {
    // (the value is made before the place of the variable is taken: making
    // it can add variables, an automatic label for instance, and the table
    // can then move its entries)
    t= exec (t); tree& val= env (s); if (val != t) {
      env.log_back (s); val= t; update (s); } }
  inline bool provides (string s) { return env.contains (s); }
  inline tree read (string s) { return env [s]; }
  tree local_begin_extents (box b);
  void local_end_extents (tree t);

  void write_default_env ();
  void write_prepared_env (hashmap<string,tree> pre);
  void write_prepared_env (hashmap<string,tree> pre, hashmap<string,tree> made);
  void prepared_fingerprint (hashmap<string,tree> pre);
  // the stamps of the tables of the references and attachments: the same
  // when none of them changed (see hashmap_rep::stamp, same_lookups)
  unsigned long long labels_stamp ();
  void write_env (hashmap<string,tree> user_env);
  void monitored_patch_env (hashmap<string,tree> patch);
  void patch_env (hashmap<string,tree> patch);
  void active_vars (hashmap<string,tree> patch, array<string>& vars);
  void read_env (hashmap<string,tree>& ret);
  void local_start (hashmap<string,tree>& prev_back);
  void local_update (hashmap<string,tree>& oldpat, hashmap<string,tree>& chg);
  void local_changes (hashmap<string,tree>& chg);
  inline array<int>* input_vars () { return env.in_vars; }
  inline unsigned long long* input_hash () { return env.in_hash; }
  // a hash of the values of the variables 'vars' (by env_var_id): those
  // which they have now, or (at_start) those which they had at the start
  // of the bridge which is typeset (back: before it wrote them)
  unsigned long long inputs_hash (array<int> vars, bool at_start);
  unsigned long long start_hash (int id);
  // the fingerprint of the variables and of the files (see files_hash)
  inline unsigned long long fingerprint () {
    return env.fingerprint () ^ files_hash (); }
  unsigned long long files_hash ();
  tree files_base;  // (the files of which files_hash is the hash, held:
  tree files_cur;   // a tree which goes could leave its place to another)
  unsigned long long files_h;
  // The table made by write_prepared_env, of which the fingerprints are
  // kept from one pass to the next (held: see files_base)
  hashmap<string,tree> prep_pre;
  hashmap<string,tree> prep_made; // (the table written back to, held)
  // (the table itself, and the same put back, still followed, for the
  // check mode of edit_typeset_rep::typeset_prepare)
  inline hashmap<string,tree> prepared_table () { return env.h; }
  inline void put_back_table (hashmap<string,tree> t) {
    env.h= t; env.follow= true; env.refingerprint (); }
  unsigned long long prep_fp, prep_sfp;
  int prep_sfp_version;
  void local_end (hashmap<string,tree>& prev_back);

  /* updating environment variables */
  ornament_parameters get_ornament_parameters ();
  art_box_parameters get_art_box_parameters (tree t);
  void   update_page_pars ();
  void   get_page_pars (SI& w, SI& h, SI& ww, SI& hh,
			SI& odd, SI& even, SI& top, SI& bottom);
  SI     get_page_width (bool deco);
  SI     get_pages_width (bool deco);
  SI     get_page_height (bool deco);
  tree   decode_arrow (tree t, string l, string h);
  int    get_script_size (int sz, int level);
  void   update_font ();
  void   update_color ();
  void   update_pattern_mode ();
  void   update_zoom ();
  void   update_mode ();
  void   update_info_level ();
  void   update_language ();
  void   update_geometry ();
  void   update_frame ();
  void   update_src_style ();
  void   update_src_special ();
  void   update_src_compact ();
  void   update_src_close ();
  void   update_dash_style ();
  void   update_dash_style_unit ();
  void   update_line_arrows ();
  void   update ();
  void   update (string env_var);

  /* lengths */
  bool      is_length (string s);
  bool      is_anylen (tree t);
  tree      tmlen_plus (tree t1, tree t2);
  tree      tmlen_min (tree t1, tree t2);
  tree      tmlen_max (tree t1, tree t2);
  tree      tmlen_times (double sc, tree t);
  tree      tmlen_over (tree t1, tree t2);
  double    tmlen_div (tree t1, tree t2);
  tree      tmlen_mod (tree t1, tree t2);

  void      get_length_unit (string l, SI& un, string& un_str);
  string    add_lengths (string l1, string l2);
  string    sub_lengths (string l1, string l2);
  string    max_lengths (string l1, string l2);
  string    min_lengths (string l1, string l2);
  string    multiply_length (double x, string l);
  double    divide_lengths (string l1, string l2);

  tree      as_tmlen (tree t);
  SI        as_length (tree t);
  SI        as_length (tree t, string perc);
  SI        as_eff_length (tree t);
  SI        as_real_length (tree t);
  space     as_hspace (tree t);
  space     as_vspace (tree t);
  point     as_point (tree t);

  /* retrieving environment variables */
  inline bool get_bool (string var) {
    tree t= env [var];
    if (is_compound (t)) return false;
    return as_bool (t->label); }
  inline int get_int (string var) {
    tree t= env [var];
    if (is_compound (t)) return 0;
    return as_int (t->label); }
  // the size of a pixel on the screen, which depends on the zoom: what is
  // made with it has read the zoom factor, and is made again when the zoom
  // changes (the rest is not: see edit_typeset_rep::set_zoom)
  inline SI get_pixel () { (void) env [ZOOM_FACTOR]; return zoom_pixel; }
  inline double get_double (string var) {
    tree t= env [var];
    if (is_compound (t)) return 0.0;
    return as_double (t->label); }
  inline string get_string (string var) {
    tree t= env [var];
    if (is_compound (t)) return "";
    return t->label; }
  inline SI get_length (string var) {
    tree t= env [var];
    return as_length (t); }
  inline space get_vspace (string var) {
    tree t= env [var];
    return as_vspace (t); }
  inline color get_color (string var) {
    tree t= env [var];
    return named_color (as_string (t), alpha); }

  friend class edit_env;
  friend tm_ostream& operator << (tm_ostream& out, edit_env env);
};

class edit_env {
  CONCRETE_NULL(edit_env);
  inline edit_env (edit_env_rep* rep2):
    rep(rep2) { INC_COUNT_NULL (this->rep); }
  edit_env (drd_info& drd,
	    url base_file_name,
	    hashmap<string,tree>& local_ref,
	    hashmap<string,tree>& global_ref,
	    hashmap<string,tree>& local_aux,
	    hashmap<string,tree>& global_aux,
	    hashmap<string,tree>& local_att,
	    hashmap<string,tree>& global_att);
};
CONCRETE_NULL_CODE(edit_env);

tm_ostream& operator << (tm_ostream& out, edit_env env);
tree texmacs_exec (edit_env env, tree cmd);
tree load_inclusion (url u); // implemented in tm_file.cpp
bool inclusion_busy (url u);
void inclusion_enter (url u);
void inclusion_leave (url u);
void reset_inclusion (url u);
bool refresh_inclusions ();
// (whether the typesetting looked up a picture, see edit_env_rep::picture_value)
bool typeset_pictures_used ();
// the included document of a file, as the cache of the inclusions has it
// (by the name of the file: as_string), and a number which changes when
// the cache changes (the lookups of the inclusions, see same_lookups)
tree inclusion_value (string name);
unsigned long long inclusions_stamp ();
struct inclusion_guard {
  // Marks u as being included during the guard's lifetime (exception-safe)
  url u;
  inclusion_guard (url u2): u (u2) { inclusion_enter (u); }
  ~inclusion_guard () { inclusion_leave (u); }
};
tree tree_extents (tree t);
bool is_percentage (tree t, string s);
bool is_percentage (tree t);
double as_percentage (tree t);
bool is_magnification (string s);
double get_magnification (string s);
int decode_alpha (string s);
array<double> get_control_times (tree t);

void set_graphical_value (tree var, tree val);
bool has_graphical_value (tree var);
tree get_graphical_value (tree var);
bool graphics_needs_update ();
void graphics_require_update (tree var);
void graphics_notify_update (tree var);

void players_set_elapsed (tree t, double el);
void players_set_speed (tree t, double sp);

#endif // defined ENV_H
