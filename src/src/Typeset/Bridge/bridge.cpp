
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
#include "Stack/stacker.hpp"
#include <stdint.h>

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
  removed (UNINIT), bound (UNINIT), stack_cache_ok (false), version (0),
  chunk_cache (NULL)
{
  static int bridges_made= 0;
  serial= bridges_made++;
}

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
  // (the variables which the old bridge left with the value UNINIT: the
  // new one compares them with those which it leaves, see typeset)
  new_br->uninit= br->uninit;
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

/******************************************************************************
* Merging runs of lines outside paper mode
*
* Outside paper mode, the lines of a bridge are merged into a single stack
* box before being handed to the pager, unless some of them carry floats or
* several columns.  In the latter case all lines used to be passed on
* individually, so that for a long document with a few figures the pager
* reprocessed every line of the document at each keystroke.  Instead, runs
* of ordinary lines are merged into chunks, exactly as the whole list would
* have been merged in the absence of floats, while the special items are
* kept as they are.  Chunk boundaries only depend on the lines themselves,
* so that an edit only changes the chunk in which it occurs, and chunks are
* cached from one typesetting pass to the next.
******************************************************************************/

#define CHUNK_MIN 16
#define CHUNK_MAX 256

struct line_chunk {
  array<page_item> src;  // the merged lines
  page_item        out;  // the resulting item
};

struct chunk_cache_rep {
  // chunks of the current and of the previous pass, by their first box,
  // and the path of the bridge which made them (the path of their stacks)
  hashmap<pointer,line_chunk> cur, prev;
  path ip;
};

bridge_rep::~bridge_rep () {
  if (chunk_cache != NULL) tm_delete (chunk_cache);
}

// The chunks of a bridge hold its lines until the next time it makes
// chunks: when it no longer does (its last float went away), they must go,
// or a line which is replaced later would not log the place it leaves
static inline void
drop_chunks (bridge_rep* br) {
  if (br->chunk_cache != NULL) {
    tm_delete (br->chunk_cache);
    br->chunk_cache= NULL;
  }
}

static inline bool
chunkable (page_item& it) {
  return it->type == PAGE_LINE_ITEM && N(it->fl) == 0 && it->nr_cols <= 1;
}

static inline bool
chunk_boundary (page_item& it) {
  // content defined boundaries (about one every 64 lines), depending on the
  // geometry of the line only: the same lines give the same chunks,also
  // after a full retypesetting
  box b= it->b;
  uint64_t h= (uint64_t) (uint32_t) b->w ();
  h= h * 0x9E3779B97F4A7C15ULL + (uint64_t) (uint32_t) b->y1;
  h= h * 0x9E3779B97F4A7C15ULL + (uint64_t) (uint32_t) b->y2;
  h= h * 0x9E3779B97F4A7C15ULL + (uint64_t) (uint32_t) b->y3;
  h= h * 0x9E3779B97F4A7C15ULL + (uint64_t) (uint32_t) b->y4;
  h= (h ^ (h >> 29)) * 0xBF58476D1CE4E5B9ULL;
  return (h >> 58) == 0;
}

static inline bool
same_space (space s1, space s2) {
  return s1->min == s2->min && s1->def == s2->def && s1->max == s2->max;
}

static bool
same_lines (array<page_item>& src, array<page_item>& l, int i1, int i2) {
  if (N(src) != i2 - i1) return false;
  for (int k= i1; k < i2; k++) {
    page_item& a= src[k-i1];
    page_item& b= l[k];
    if (a->b != b->b || a->penalty != b->penalty ||
        !same_space (a->spc, b->spc))
      return false;
  }
  return true;
}

static page_item
merge_line_run (path ip, array<page_item>& l, int i1, int i2) {
  array<box> bs;
  array<SI>  spc;
  for (int k= i1; k < i2; k++) {
    bs  << l[k]->b;
    spc << l[k]->spc->def;
  }
  box lb= stack_box (path (ip), bs, spc);
  lb= move_box (path (ip), lb, 0, bs[0]->y2);
  page_item it (lb);
  it->spc= l[i2-1]->spc;
  it->penalty= l[i2-1]->penalty;
  return it;
}

static array<page_item>
chunk_lines (bridge_rep* br, array<page_item> l) {
  if (br->chunk_cache == NULL) br->chunk_cache= tm_new<chunk_cache_rep> ();
  chunk_cache_rep* cc= br->chunk_cache;
  if (!strong_equal (cc->ip, br->ip)) {
    // the bridge has another path: the chunks made with the old one go
    cc->cur= hashmap<pointer,line_chunk> ();
    cc->ip = br->ip;
  }
  cc->prev= cc->cur;
  cc->cur = hashmap<pointer,line_chunk> ();
  array<page_item> out;
  int i= 0, n= N(l);
  // the first and the last item are never merged: the pager uses their
  // extents for the corrections at the top and the bottom of pages
  while (i < n) {
    if (i == 0 || i == n-1 || !chunkable (l[i])) { out << l[i]; i++; continue; }
    int j= i;
    while (j < n-1 && chunkable (l[j]) && j - i < CHUNK_MAX) {
      j++;
      // (many lines have the same extents: with a minimal length, chunks
      // remain long even if their common signature is a boundary)
      if (j - i >= CHUNK_MIN && chunk_boundary (l[j-1])) break;
    }
    if (j - i < 2) { out << l[i]; i= j; continue; }
    pointer key= (pointer) l[i]->b.operator-> ();
    if (typeset_caches_mode () != 0 && cc->prev->contains (key) &&
        same_lines (cc->prev (key).src, l, i, j)) {
      if (typeset_caches_mode () == 2) {
        array<page_item> made (1), kept (1);
        made[0]= merge_line_run (br->ip, l, i, j);
        kept[0]= cc->prev (key).out;
        if (!same_page_items (made, kept))
          typeset_cache_mismatch ("chunk of lines");
      }
      out << cc->prev (key).out;
      cc->cur (key)= cc->prev (key);
    }
    else {
      line_chunk ch;
      ch.src= range (l, i, j);
      ch.out= merge_line_run (br->ip, l, i, j);
      out << ch.out;
      cc->cur (key)= ch;
    }
    i= j;
  }
  cc->prev= hashmap<pointer,line_chunk> ();  // see stack_cache in typeset
  return out;
}

// the single item into which the lines of a bridge are merged outside
// paper mode (chunks of them when some carry floats or several columns)
static array<page_item>
merged_lines (bridge_rep* br) {
  array<page_item>& l= br->l;
  path ip= br->ip;
  bool flag= false;
  int i, n= N(l);
  for (i=0; i<n; i++)
    flag= flag || (N (l[i]->fl) != 0) || (l[i]->nr_cols > 1);
  if (flag) return chunk_lines (br, l);
  drop_chunks (br);
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
  return new_l;
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
*     table must not have been read as a whole, and no Scheme routine
*     must have been called, which may look at anything (read_other);
*   - the references and the attachments which it looked up, which are
*     recorded with the values found (at most LOOKUPS_MAX of them, with
*     those of the bridges below), still have these values: a paragraph
*     which refers to an equation whose number changed before it is
*     typeset again, one which refers to something which did not change
*     is not (the entries of an index, with their page numbers);
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
* A bridge which binds variables for the bridges below it (a with, an
* ornament, a locus, a formatting tag) writes them without a record of
* the change: they have their values back when it is done, and are not
* among its changes. Below it they are others than in the previous pass
* when it binds other values: it keeps what it bound ('bound'), and the
* variables whose binding differs are in old_patch while the bridges below
* it are typeset (bind_start and bind_end).
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

#define LOOKUPS_MAX 64

// the reads and the lookups of a bridge which was typeset or used again,
// for the record of the bridge above it
void
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

bool
same_page_items (array<page_item> l1, array<page_item> l2) {
  return lines_signature (l1) == lines_signature (l2) &&
         lines_contents (l1) == lines_contents (l2);
}

// 'now' is a tuple of the variables which the bridge binds and of their
// values. The result is what bind_end needs to put old_patch back, when
// the bridges below are typeset
tree
bridge_rep::bind_start (tree now) {
  tree outer (TUPLE);
  for (int i=0; i+1<N(now); i+=2) {
    if (!is_atomic (now[i])) continue;
    string var= now[i]->label;
    bool later= false; // (bound again further on)
    for (int j=i+2; j+1<N(now); j+=2)
      if (now[j] == now[i]) later= true;
    if (later) continue;
    // (the value bound in the previous pass, which is the last one of
    // the variable in 'bound')
    tree prev (UNINIT);
    bool same= false;
    if (is_func (bound, TUPLE))
      for (int j=0; j+1<N(bound); j+=2)
        if (bound[j] == now[i]) { prev= bound[j+1]; same= (prev == now[i+1]); }
    // below, the variable has the value of the previous pass when the
    // binding is the same, whatever it had outside
    if (same && !ttt->old_patch->contains (var)) continue;
    if (ttt->old_patch->contains (var))
      outer << tree (var) << tree ("1") << ttt->old_patch [var];
    else outer << tree (var) << tree ("0") << tree (UNINIT);
    if (same) ttt->old_patch->reset (var);
    else ttt->old_patch (var)= prev;
  }
  if (now != bound) bound= copy (now);
  return outer;
}

void
bridge_rep::bind_end (tree outer) {
  for (int i=N(outer)-3; i>=0; i-=3) {
    string var= outer[i]->label;
    if (outer[i+1] == "1") ttt->old_patch (var)= outer[i+2];
    else ttt->old_patch->reset (var);
  }
}

void
bridge_rep::my_resync () {
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
}

bool bridge_rep::my_reusable () { return true; }
bool bridge_rep::my_check_cache () { return false; }
void bridge_rep::my_stack_start () { ttt->local_start (l, sb); }
void bridge_rep::my_stack_end () { ttt->local_end (l, sb); }

void
bridge_rep::typeset (int desired_status) {
  my_resync ();

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
  bool reusable= (status==desired_status) && my_reusable ();
  bool unread= reusable && (N(ttt->old_patch)!=0) &&
               reads_known && alone && reads_mode () != 0 &&
               patch_unread (ttt->old_patch, reads, env) &&
               same_lookups (env, seen_keys, seen_values);
  // a bridge used again as in the classic cache (nothing changed around
  // it, as far as the bookkeeping of the changes says: the paragraphs
  // removed before it, the bindings above it)
  bool cached= reusable && (N(ttt->old_patch)==0);
  // the check mode typesets again both kinds of bridges used again, when
  bool check= (reads_mode () == 2 && (unread || (cached && reads_known))) ||
              ((unread || cached) && my_check_cache ());
  array<SI> old_lines, old_border;
  array<tree> old_contents;
  hashmap<string,tree> old_changes (UNINIT);
  if (check) {
    old_lines= lines_signature (l);
    old_border= border_signature (sb);
    old_contents= lines_contents (l);
    old_changes= changes;
  }
  bool made= false;
  if (cached && !check) {
    //cout << "cached" << LF;
    if (edit_profile.on) edit_profile.cached++;
    add_reads (env, reads_known, reads, seen_keys, seen_values);
    env->monitored_patch_env (changes);
    for (int i=0; i<N(uninit); i++) env->leave_uninit (uninit[i]);
    // cout << "changes       = " << changes << LF;
  }
  else if (unread && !check) {
    // as above; what the bridge writes is as in the previous pass
    if (edit_profile.on) { edit_profile.cached++; edit_profile.unread++; }
    add_reads (env, reads_known, reads, seen_keys, seen_values);
    env->monitored_patch_env (changes);
    for (int i=0; i<N(uninit); i++) env->leave_uninit (uninit[i]);
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
    my_stack_start ();
    env->local_start (prev_back);
    if (env->hl_lan != 0) env->lan->highlight (st);
    // the variables read by the typesetting of the subtree
    hashmap<int,bool> my_reads (false);
    array<string> my_keys;
    array<tree> my_values;
    array<string> my_uninit;
    edit_env_rep::read_record record (env.operator-> (), &my_reads,
                                      &my_keys, &my_values, &my_uninit);
    my_typeset (desired_status);
    bool recorded= record.complete () &&
                   N(my_reads) <= READS_MAX && N(my_keys) <= LOOKUPS_MAX;
    reads_known= recorded && alone;
    // the variables which it leaves with the value UNINIT: among those
    // which it read or wrote (every write goes through env_table, which
    // records it), and those of the bridges below it
    for (iterator<int> it= iterate (my_reads); it->busy (); ) {
      string var= env_var_name (it->next ());
      if (!contains (var, my_uninit)) my_uninit << var;
    }
    array<string> left;
    for (int i=0; i<N(my_uninit); i++)
      if (env->is_uninit (my_uninit[i])) left << my_uninit[i];
    my_uninit= left;
    reads= array<int> ();
    seen_keys= array<string> ();
    seen_values= array<tree> ();
    if (recorded) {
      for (iterator<int> it= iterate (my_reads); it->busy (); )
        reads << it->next ();
      merge_sort (reads);
      seen_keys= my_keys;
      seen_values= my_values;
    }
    record.stop ();
    add_reads (env, recorded, reads, seen_keys, seen_values);
    for (int i=0; i<N(my_uninit); i++)
      env->note_uninit (my_uninit[i]);
    env->local_update (ttt->old_patch, changes);
    // a variable which the bridge leaves with the value UNINIT now and not
    // in the previous pass, or the other way round, changed (the changes
    // do not tell, see rec_uninit)
    for (int i=0; i<N(my_uninit); i++)
      if (!contains (my_uninit[i], uninit))
        ttt->old_patch (my_uninit[i])= tree (UNINIT);
    for (int i=0; i<N(uninit); i++)
      if (!contains (uninit[i], my_uninit))
        ttt->old_patch (uninit[i])= tree (UNINIT);
    uninit= my_uninit;
    env->local_end (prev_back);
    my_stack_end ();
    if (check) {
      if (edit_profile.on) edit_profile.unread++;
      if (lines_signature (l) != old_lines || changes != old_changes ||
          border_signature (sb) != old_border ||
          lines_contents (l) != old_contents) {
        typeset_check_mismatches++;
        failed_error << "A bridge which read none of the variables which "
                     << "changed (" << ttt->old_patch << ") is typeset "
                     << "otherwise: " << st << LF;
      }
    }
    env->link_env= old_link_env;
    status= desired_status;
    // release the old lines now: a phrase box logs the area to be repainted
    // when it is destroyed, so a stale cache would leave it on the screen
    stack_cache= array<page_item> ();
    stack_cache_ok= false;
    version++;
    // cout << "old_patch     = " << ttt->old_patch << LF;
    // cout << "changes       = " << changes << LF;
    // cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;
    made= true;
  }
  my_insert (made);
  //cout << UNINDENT << "Typesetted " << st << ", " << desired_status << LF;
}

void
bridge_rep::my_insert (bool made) {
  (void) made;
  // ttt->insert_stack (l, sb);
  //if (N(l) == 0); else
  int mode= typeset_caches_mode ();
  if (ttt->paper || (N(l) <= 1)) {
    drop_chunks (this);
    ttt->insert_stack (l, sb);
  }
  else if (mode != 0 && stack_cache_ok && strong_equal (ip, stack_cache_ip)) {
    if (mode == 2 && !same_page_items (merged_lines (this), stack_cache))
      typeset_cache_mismatch ("merged stack of a paragraph");
    ttt->insert_stack (stack_cache, sb);
  }
  else {
    stack_cache= merged_lines (this);
    stack_cache_ip= ip;
    stack_cache_ok= true;
    ttt->insert_stack (stack_cache, sb);
  }

  //cout << "l   = " << l << LF;
  //cout << "sb  = " << sb << LF;
  //cout << "l   = " << ttt->l << LF;
  //cout << "a   = " << ttt->a << LF;
  //cout << "b   = " << ttt->b << LF;
}
