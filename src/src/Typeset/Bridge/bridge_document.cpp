
/******************************************************************************
* MODULE     : bridge_document.cpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"
#include "iterator.hpp"
#include <stdint.h>

/******************************************************************************
* Replay cache for runs of unchanged paragraphs
*
* At every typesetting pass, all paragraphs of the document are visited:
* unchanged ones only patch the environment with their cached changes and
* merge their cached lines into the global stack.  For long documents this
* per-paragraph work dominates the cost of a keystroke.  The paragraphs are
* therefore grouped in blocks; the first paragraph of each block (the head)
* is handled as usual, and the effect of the remaining ones (the tail) is
* recorded once and replayed as a whole when all of them are unchanged.
*
* Replaying is exact: after the head has been merged, the last line item of
* the stack comes from the head itself, so the merging of the tail does not
* depend on what precedes the block.  Its effect consists of a modified copy
* of that line item, the appended items, the "after" fields of the stack
* border and the composition of the environment patches of the tail.
*
* Block boundaries only depend on the paragraph bridges themselves (on the
* number of the bridge, bridge_rep::serial, which follows the order in
* which they were made, and not on where they are in memory: the blocks are
* the same from one session to the next), and blocks are cached by the
* number of their head bridge, which is never the one of another bridge:
* inserting or removing paragraphs, e.g. when pressing Return, only
* disturbs the block where it happens instead of shifting all the blocks
* which follow.
*
* For the record of the bridge above (see "What is typeset again" in
* bridge.cpp), a replayed tail has read what its bridges read when they
* were typeset: their reads and lookups are kept with the block, and given
* to add_reads as those of a bridge which is used again.
*
* TEXMACS_TYPESET_CACHES may be "off" (no block is replayed) or "check"
* (the tail is typeset all the same, and what it made is compared with
* the block).
******************************************************************************/

#define DOC_RUN_MAX 64

static inline bool
doc_run_boundary (bridge& br) {
  // about one paragraph in 32 starts a new block
  uint64_t h= (uint64_t) (uint32_t) br->serial;
  h= (h + 1) * 0x9E3779B97F4A7C15ULL;
  h= (h ^ (h >> 29)) * 0xBF58476D1CE4E5B9ULL;
  return (h >> 59) == 0;
}

struct doc_run {
  bool                 ok;
  bridge               head;      // the head bridge
  array<bridge>        brs;       // the tail bridges
  array<int>           vers;      // and their versions when recorded
  page_item            head_item; // last line item after merging the head
  int                  head_back; // its position, counted from the end
  page_item            repl;      // the same item after merging the tail
  array<page_item>     app;       // items appended by the tail
  SI                   height, sep, hor_sep, ver_sep, bot;
  space                vspc_after;
  bool                 nobr_after;
  // the border of the stack before the tail: the tail was shoved against
  // it, and a head without a line only changes this
  SI                   in_height, in_sep, in_hor_sep, in_ver_sep, in_bot;
  space                in_vspc_after;
  bool                 in_nobr_after;
  hashmap<string,tree> patch;     // composed environment changes
  bool                 known;     // the reads of the tail: all known,
  array<int>           reads;     // the variables (sorted env_var_id)
  array<string>        keys;      // and the lookups, with the values
  array<tree>          values;    // found
  doc_run (): ok (false), head_back (0), vspc_after (0), nobr_after (false),
              in_height (0), in_sep (0), in_hor_sep (0), in_ver_sep (0),
              in_bot (0), in_vspc_after (0), in_nobr_after (false),
              patch (UNINIT), known (false) {}
};

static int
last_line_item (array<page_item>& l) {
  int i= N(l) - 1;
  while (i >= 0 && l[i]->type != PAGE_LINE_ITEM) i--;
  return i;
}

bridge bridge_docrange (typesetter ttt, tree st, path ip, array<bridge>& brs,
			int begin, int end, bool divide);

class bridge_document_rep: public bridge_rep {
protected:
  array<bridge> brs;
  bridge acc; // binary splitting acceleration for long documents
  hashmap<string,tree> removed_last; // as bridge_rep::removed, for the
                                     // paragraphs removed at the end
  hashmap<int,doc_run> runs, old_runs; // blocks, by their head bridge
  void typeset_one (int i, int n, int desired_status,
                    array<line_item>& a, array<line_item>& b);
  bool may_replay (doc_run& run, int s, int e);
  void replay_tail (doc_run& run);
  void tail_reads (doc_run& run, int s, int e);
  void check_tail (doc_run& old, doc_run& run, doc_run& now);
  void record_tail (doc_run& run, int s, int e, int desired_status,
                    array<line_item>& a, array<line_item>& b);

public:
  bridge_document_rep (typesetter ttt, tree st, path ip);
  void initialize ();
  void initialize_acc ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
};

bridge_document_rep::bridge_document_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip)
{
  initialize ();
}

void
bridge_document_rep::initialize () {
  int i, n= N(st);
  brs= array<bridge> (n);
  for (i=0; i<n; i++)
    brs[i]= make_bridge (ttt, st[i], descend (ip, i));
  initialize_acc ();
}

void
bridge_document_rep::initialize_acc () {
  if (true || ttt->paper) acc= bridge ();
  else acc= bridge_docrange (ttt, st, ip, brs, 0, N(st), true);
}

bridge
bridge_document (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_document_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_document_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p) || is_func (u, DOCUMENT) || is_func (u, PARA),
	  "nil path");
  if (is_nil (p)) { st= u; initialize (); }
  else {
    if (is_atom (p)) {
      replace_bridge (brs[p->item], u, descend (ip, p->item));
      st= substitute (st, p->item, brs[p->item]->st);
    }
    else {
      brs[p->item]->notify_assign (p->next, u);
      st= substitute (st, p->item, brs[p->item]->st);
    }
    if (!is_nil (acc)) acc->notify_assign (p, u);
  }
  status= CORRUPTED;
}

void
bridge_document_rep::notify_insert (path p, tree u) {
  //cout << "Insert " << p << ", " << u << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p)) {
    int i, j, n= N(brs), pos= p->item, nr= N(u);
    array<bridge> brs2 (n+nr);
    if (pos>0) brs[pos-1]->notify_change (); // touch in case of surroundings
    if (pos<n) brs[pos  ]->notify_change (); // touch in case of surroundings
    for (i=0; i<pos; i++) brs2[i]= brs[i];
    for (j=0; j<nr ; j++) brs2[i+j]= make_bridge (ttt, u[j], descend (ip,i+j));
    for (; i<n; i++) {
      brs2[i+nr]= brs[i];
      brs2[i+nr]->ip->item += nr;
    }
    brs= brs2;
    st = (st (0, p->item) * u) * st (p->item, N(st));
    if (!is_nil (acc)) acc->notify_insert (p, u);
    // initialize_acc ();
  }
  else {
    brs[p->item]->notify_insert (p->next, u);
    st= substitute (st, p->item, brs[p->item]->st);
    if (!is_nil (acc)) acc->notify_assign (p->item, st[p->item]);
  }
  status= CORRUPTED;
}

void
bridge_document_rep::notify_remove (path p, int nr) {
  // cout << "Remove " << p << ", " << nr << " in " << st << "\n";
  ASSERT (!is_nil (p), "nil path");
  if (is_atom (p)) {
    int i, n= N(brs), pos= p->item;
    array<bridge> brs2 (n-nr);
    for (i=0; i<pos ; i++) brs2[i]= brs[i];
    for (; i<n-nr; i++) {
      brs2[i]= brs[i+nr];
      brs2[i]->ip->item -= nr;
    }
    bool change_flag= false;
    for (i=pos; i<pos+nr; i++)
      change_flag |= !brs[i]->changes->empty();
    // (the bridges which follow see in their inputs whether what the
    // removed paragraphs changed is now otherwise: see bridge.cpp)
    if (typeset_inputs_mode () != 0) change_flag= false;
    brs= brs2;
    n -= nr;
    st = st (0, pos) * st (pos+nr, N(st));
    if (pos>0) brs[pos-1]->notify_change (); // touch in case of surroundings
    if (pos<n) brs[pos  ]->notify_change (); // touch in case of surroundings
    if (change_flag) // touch brs[pos..n] for correct ``changes handling''
      for (i=pos; i<n; i++)
	brs[i]->notify_change ();
    if (!is_nil (acc)) acc->notify_remove (p, nr);
    // initialize_acc ();
  }
  else {
    brs[p->item]->notify_remove (p->next, nr);
    st= substitute (st, p->item, brs[p->item]->st);
    if (!is_nil (acc)) acc->notify_assign (p->item, st[p->item]);
  }
  status= CORRUPTED;
}

bool
bridge_document_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  bool flag= false;
  int i, n= N(brs);
  for (i=0; i<n; i++)
    flag= brs[i]->notify_macro (tp, var, l, p, u) || flag;
  if (flag) {
    status= CORRUPTED;
    if (!is_nil (acc)) acc->notify_change ();
  }
  return flag;
}

void
bridge_document_rep::notify_change () {
  status= CORRUPTED;
  if (!is_nil (acc)) acc->notify_change ();
  if (N(brs)>0) brs[0]->notify_change ();
  if (N(brs)>1) brs[N(brs)-1]->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_document_rep::my_exec_until (path p) {
  if (is_nil (acc)) {
    int i;
    for (i=0; i<p->item; i++)
      brs[i]->exec_until (path (right_index (brs[i]->st)), true);
    if (i<N(st)) brs[i]->exec_until (p->next);
  }
  else acc->my_exec_until (p);
}

bool
bridge_document_rep::my_typeset_will_be_complete () {
  if (is_nil (acc)) {
    int i, n= N(brs);
    for (i=0; i<n; i++)
      if (!brs[i]->my_typeset_will_be_complete ()) return false;
    return true;
  }
  else return acc->my_typeset_will_be_complete ();
}

void
bridge_document_rep::typeset_one (int i, int n, int desired_status,
                                  array<line_item>& a, array<line_item>& b) {
  //cout << "Typesetting " << st[i] << LF;
  int wanted= (i==n-1? desired_status & WANTED_MASK: WANTED_PARAGRAPH);
  ttt->a= (i==0  ? a: array<line_item> ());
  ttt->b= (i==n-1? b: array<line_item> ());
  brs[i]->typeset (PROCESSED+ wanted);
}

bool
bridge_document_rep::may_replay (doc_run& run, int s, int e) {
  // tail = paragraphs s..e-1, none of which is the first or the last one
  if (!run.ok || N(ttt->old_patch) != 0) return false;
  if (run.head != brs[s-1] || N(run.brs) != e - s) return false;
  for (int k= s; k < e; k++) {
    bridge_rep* br= brs[k].operator-> ();
    if (run.brs[k-s].operator-> () != br || run.vers[k-s] != br->version ||
        br->status != PROCESSED + WANTED_PARAGRAPH)
      return false;
    // the changes of the paragraphs removed before this one are compared
    // with the environment when it is typeset (bridge_rep::typeset)
    if (N(br->removed) != 0) return false;
  }
  int idx= last_line_item (ttt->l);
  if (idx < 0 || N(ttt->l) - idx != run.head_back) return false;
  if (ttt->l[idx].operator-> () != run.head_item.operator-> ()) return false;
  // the last line may be one of an earlier paragraph, when the head (or
  // what is between) has no line: the border must be the one the tail
  // was typeset against
  stack_border sb= ttt->sb;
  if (sb->height != run.in_height || sb->sep != run.in_sep ||
      sb->hor_sep != run.in_hor_sep || sb->ver_sep != run.in_ver_sep ||
      sb->bot != run.in_bot || sb->vspc_after != run.in_vspc_after ||
      sb->nobr_after != run.in_nobr_after)
    return false;
  return true;
}

void
bridge_document_rep::replay_tail (doc_run& run) {
  // (the bridges of the tail are where they were: their paths are shared
  // with the boxes they made, and changed in place by notify_insert and
  // notify_remove)
  int idx= last_line_item (ttt->l);
  stack_border sb= ttt->sb;
  ttt->l[idx]= run.repl;
  ttt->l << run.app;
  sb->height    = run.height;
  sb->sep       = run.sep;
  sb->hor_sep   = run.hor_sep;
  sb->ver_sep   = run.ver_sep;
  sb->bot       = run.bot;
  sb->vspc_after= run.vspc_after;
  sb->nobr_after= run.nobr_after;
  env->monitored_patch_env (run.patch);
  add_reads (env, run.known, run.reads, run.keys, run.values);
  if (edit_profile.on) edit_profile.cached += N(run.brs);
}

void
bridge_document_rep::check_tail (doc_run& old, doc_run& run, doc_run& now) {
  // the check mode: the block which would have been replayed, and the
  // one which the tail made when it was typeset again; its reads are
  // those which its bridges, used again, would have given (now), since
  // what a bridge reads when it is typeset again may differ
  array<page_item> l1, l2;
  l1 << old.repl << old.app;
  l2 << run.repl << run.app;
  string what;
  if (!run.ok) what << " (not recorded)";
  if (!same_page_items (l1, l2)) what << " (lines)";
  if (old.height != run.height || old.sep != run.sep ||
      old.hor_sep != run.hor_sep || old.ver_sep != run.ver_sep ||
      old.bot != run.bot || old.vspc_after != run.vspc_after ||
      old.nobr_after != run.nobr_after) what << " (border)";
  if (old.patch != run.patch) what << " (environment)";
  if (old.known != now.known || old.reads != now.reads) what << " (reads)";
  if (old.keys != now.keys || old.values != now.values) what << " (lookups)";
  if (N(what) != 0)
    typeset_cache_mismatch ("block of paragraphs replayed" * what);
}

static array<int>
union_reads (array<int> r1, array<int> r2) {
  // (both sorted)
  array<int> r;
  int i= 0, j= 0;
  while (i < N(r1) || j < N(r2))
    if (j == N(r2) || (i < N(r1) && r1[i] < r2[j])) r << r1[i++];
    else if (i == N(r1) || r2[j] < r1[i]) r << r2[j++];
    else { r << r1[i++]; j++; }
  return r;
}

void
bridge_document_rep::tail_reads (doc_run& run, int s, int e) {
  // the reads of the bridges s..e-1, which are added to the record of the
  // bridge above when they are used again
  run.known= true;
  for (int k= s; k < e && run.known; k++) {
    bridge_rep* br= brs[k].operator-> ();
    run.known= br->reads_known;
    run.reads = union_reads (run.reads, br->reads);
    run.keys  << br->seen_keys;
    run.values<< br->seen_values;
  }
  if (!run.known) {
    run.reads = array<int> ();
    run.keys  = array<string> ();
    run.values= array<tree> ();
  }
}

void
bridge_document_rep::record_tail (doc_run& run, int s, int e,
                                  int desired_status,
                                  array<line_item>& a, array<line_item>& b) {
  int n= N(st);
  int idx= last_line_item (ttt->l);
  page_item head_item= (idx >= 0? ttt->l[idx]: page_item ());
  int n0= N(ttt->l);
  stack_border sb0= ttt->sb;
  SI in_height= sb0->height, in_sep= sb0->sep, in_hor_sep= sb0->hor_sep;
  SI in_ver_sep= sb0->ver_sep, in_bot= sb0->bot;
  space in_vspc_after= sb0->vspc_after;
  bool in_nobr_after= sb0->nobr_after;
  for (int k= s; k < e; k++)
    typeset_one (k, n, desired_status, a, b);
  // nothing of the previous record is kept: it holds lines, which log
  // the place they leave when they go (see phrase_box_rep)
  run= doc_run ();
  if (idx < 0) return;
  hashmap<string,tree> patch (UNINIT);
  for (int k= s; k < e; k++) {
    bridge_rep* br= brs[k].operator-> ();
    if (br->status != PROCESSED + WANTED_PARAGRAPH) {
      run= doc_run ();
      return;
    }
    run.brs  << brs[k];
    run.vers << br->version;
    hashmap<string,tree> ch= br->changes;
    iterator<string> it= iterate (ch);
    while (it->busy ()) {
      string key= it->next ();
      patch (key)= ch [key];
    }
  }
  run.head_item = head_item;
  run.head_back = n0 - idx;
  run.repl      = ttt->l[idx];
  run.app       = range (ttt->l, n0, N(ttt->l));
  stack_border sb= ttt->sb;
  run.height    = sb->height;
  run.sep       = sb->sep;
  run.hor_sep   = sb->hor_sep;
  run.ver_sep   = sb->ver_sep;
  run.bot       = sb->bot;
  run.vspc_after= sb->vspc_after;
  run.nobr_after= sb->nobr_after;
  run.in_height    = in_height;
  run.in_sep       = in_sep;
  run.in_hor_sep   = in_hor_sep;
  run.in_ver_sep   = in_ver_sep;
  run.in_bot       = in_bot;
  run.in_vspc_after= in_vspc_after;
  run.in_nobr_after= in_nobr_after;
  tail_reads (run, s, e);
  run.head      = brs[s-1];
  run.patch     = patch;
  run.ok        = true;
}

void
bridge_document_rep::my_typeset (int desired_status) {
  //cout << INDENT;
  if (is_nil (acc)) {
    int i, n= N(st);
    array<line_item> a= ttt->a;
    array<line_item> b= ttt->b;
    old_runs= runs;
    runs= hashmap<int,doc_run> ();
    int mode= typeset_caches_mode ();
    for (int s= 0, e; s < n; s= e) {
      // block s..e-1; the last paragraph always forms a block of its own
      e= s + 1;
      while (e < n-1 && e - s < DOC_RUN_MAX && !doc_run_boundary (brs[e])) e++;
      typeset_one (s, n, desired_status, a, b);       // head
      if (mode != 0 && e - (s+1) >= 2) {
        int key= brs[s]->serial;
        if (old_runs->contains (key)) runs (key)= old_runs (key);
        doc_run& run= runs (key);
        if (!may_replay (run, s+1, e))
          record_tail (run, s+1, e, desired_status, a, b);
        else if (mode == 1) replay_tail (run);
        else {
          doc_run old= run, now;
          tail_reads (now, s+1, e);
          record_tail (run, s+1, e, desired_status, a, b);
          check_tail (old, run, now);
        }
      }
      else for (i= s+1; i < e; i++) typeset_one (i, n, desired_status, a, b);
    }
    if (N(removed_last) != 0) {
      env->compare_changes (ttt->old_patch, removed_last);
      removed_last= hashmap<string,tree> (UNINIT);
    }
    old_runs= hashmap<int,doc_run> ();
  }
  else acc->my_typeset (desired_status);
  //cout << UNINDENT;
}
