
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
* Blocks of paragraphs
*
* At every typesetting pass, all paragraphs of the document are visited:
* unchanged ones only patch the environment with their cached changes and
* merge their cached lines into the global stack.  For long documents this
* per-paragraph work dominates the cost of a keystroke.  The paragraphs are
* therefore grouped in blocks, which are bridges (bridge_block_rep): what a
* bridge records, its changes of the environment and its inputs (see "What
* is typeset again" in bridge.cpp), holds for a block as a whole, so that a
* block whose paragraphs did not change is used again as one bridge, also
* when the environment changed and none of the inputs of the block did.
*
* A block does not stack its paragraphs in a stack of its own, which would
* then be merged with the lines before it: the vertical space after a
* paragraph without lines at the start of the block would be lost (see
* merge_stack).  Its paragraphs are stacked where they go, and the block
* keeps what this did to the stack: the last line before the block, shoved
* against the first line of the block (repl), the line items it appended
* (app) and the border of the stack after it.  Using the block again does
* the same, provided that the stack is as it was when the block was made:
* the same last line, at the same place, and the same border.  Otherwise,
* or when one of its paragraphs changed, the block is typeset again, which
* visits its paragraphs (most of them are used again).
*
* The first and the last paragraph of the document are not in a block
* (they get the line items of the bridges around the document, ttt->a and
* ttt->b).  Block boundaries only depend on the paragraphs (on the number
* of a paragraph bridge, bridge_rep::serial, which follows the order in
* which they were made, and not on where they are in memory), and blocks
* are kept by the number of their first paragraph: inserting or removing
* paragraphs, e.g. when pressing Return, only disturbs the block where it
* happens instead of shifting all the blocks which follow.
*
* TEXMACS_TYPESET_CACHES may be "off" (no blocks) or "check" (a block which
* would be used again is typeset again, and what it makes is compared, as
* in the check mode of TEXMACS_TYPESET_READS).
******************************************************************************/

#define BLOCK_MAX 64

static inline bool
block_boundary (bridge& br) {
  // about one paragraph in 32 starts a new block
  uint64_t h= (uint64_t) (uint32_t) br->serial;
  h= (h + 1) * 0x9E3779B97F4A7C15ULL;
  h= (h ^ (h >> 29)) * 0xBF58476D1CE4E5B9ULL;
  return (h >> 59) == 0;
}

static int
last_line_item (array<page_item>& l) {
  int i= N(l) - 1;
  while (i >= 0 && l[i]->type != PAGE_LINE_ITEM) i--;
  return i;
}

// the fields of a border which merging a block changes, when there is a
// line before it
static void
copy_after (stack_border dest, stack_border src) {
  dest->height    = src->height;
  dest->sep       = src->sep;
  dest->hor_sep   = src->hor_sep;
  dest->ver_sep   = src->ver_sep;
  dest->bot       = src->bot;
  dest->vspc_after= src->vspc_after;
  dest->nobr_after= src->nobr_after;
}

static bool
same_after (stack_border sb1, stack_border sb2) {
  return sb1->height == sb2->height && sb1->sep == sb2->sep &&
         sb1->hor_sep == sb2->hor_sep && sb1->ver_sep == sb2->ver_sep &&
         sb1->bot == sb2->bot && sb1->vspc_after == sb2->vspc_after &&
         sb1->nobr_after == sb2->nobr_after;
}

class bridge_block_rep: public bridge_rep {
public:
  array<bridge>    brs;       // the paragraphs
  array<int>       vers;      // and their versions when the block was made
  bool             ok;        // what the block did to the stack is known:
  page_item        head_item; // the last line before it,
  int              head_back; // its position, counted from the end,
  stack_border     in;        // the border of the stack before it,
  page_item        repl;      // the same line, shoved against the block,
  array<page_item> app;       // the line items it appended,
  stack_border     out;       // the border of the stack after it
  int              start_n, start_i; // while it is typeset: the stack
  page_item        start_item;       // before it

public:
  bridge_block_rep (typesetter ttt, path ip);
  void set_paragraphs (array<bridge> brs2);

  // (the notifications go to the paragraphs, through bridge_document)
  void notify_assign (path p, tree u) {
    (void) p; (void) u; status= CORRUPTED; }
  bool notify_macro (int tp, string var, int l, path p, tree u) {
    (void) tp; (void) var; (void) l; (void) p; (void) u; return false; }
  void notify_change () { status= CORRUPTED; }

  void my_resync () {}
  bool my_reusable ();
  bool my_check_cache () { return typeset_caches_mode () == 2; }
  void my_stack_start ();
  void my_stack_end ();
  void my_insert (bool made);
  void my_typeset (int desired_status);
};

bridge_block_rep::bridge_block_rep (typesetter ttt, path ip):
  bridge_rep (ttt, tree (DOCUMENT), ip), ok (false), head_back (0),
  start_n (0), start_i (-1) {}

void
bridge_block_rep::set_paragraphs (array<bridge> brs2) {
  bool same= (N(brs2) == N(brs));
  for (int k=0; same && k<N(brs); k++)
    same= (brs2[k].operator-> () == brs[k].operator-> ());
  if (same) return;
  brs= brs2;
  status= CORRUPTED;
}

bool
bridge_block_rep::my_reusable () {
  if (!ok || N(vers) != N(brs)) return false;
  for (int k=0; k<N(brs); k++) {
    bridge_rep* br= brs[k].operator-> ();
    if (br->version != vers[k] || br->status != PROCESSED + WANTED_PARAGRAPH)
      return false;
  }
  int i= last_line_item (ttt->l);
  return i >= 0 && N(ttt->l) - i == head_back &&
         ttt->l[i].operator-> () == head_item.operator-> () &&
         same_after (ttt->sb, in);
}

void
bridge_block_rep::my_stack_start () {
  // (the block is not a paragraph which is typeset again: those are
  // counted when its paragraphs are)
  typeset_count_redone (-1);
  // (what the block made is made again)
  ok= false;
  repl= page_item ();
  app= array<page_item> ();
  start_n= N(ttt->l);
  start_i= last_line_item (ttt->l);
  start_item= (start_i >= 0? ttt->l[start_i]: page_item ());
  in= stack_border ();
  copy_after (in, ttt->sb);
}

void
bridge_block_rep::my_typeset (int desired_status) {
  (void) desired_status;
  for (int k=0; k<N(brs); k++) {
    ttt->a= array<line_item> ();
    ttt->b= array<line_item> ();
    bridge_record_next= true;
    brs[k]->typeset (PROCESSED + WANTED_PARAGRAPH);
  }
}

void
bridge_block_rep::my_stack_end () {
  vers= array<int> ();
  for (int k=0; k<N(brs); k++) vers << brs[k]->version;
  l= array<page_item> ();
  // (what the check mode compares: the lines and the border after it)
  sb= stack_border ();
  copy_after (sb, ttt->sb);
  if (start_i < 0) return;
  head_item= start_item;
  head_back= start_n - start_i;
  repl= ttt->l[start_i];
  app= range (ttt->l, start_n, N(ttt->l));
  out= stack_border ();
  copy_after (out, ttt->sb);
  start_item= page_item ();
  ok= true;
  l << repl << app;
}

void
bridge_block_rep::my_insert (bool made) {
  // the paragraphs of a block which was typeset are in the stack already
  if (made) return;
  ttt->l[last_line_item (ttt->l)]= repl;
  ttt->l << app;
  copy_after (ttt->sb, out);
  if (edit_profile.on) edit_profile.cached += N(brs) - 1;
}

static bridge
bridge_block (typesetter ttt, path ip) {
  return tm_new<bridge_block_rep> (ttt, ip);
}

bridge bridge_docrange (typesetter ttt, tree st, path ip, array<bridge>& brs,
			int begin, int end, bool divide);

// A document is typeset as paragraphs, or, when the environment variable
// code-lines is true (a source file, a text: see verbatim.cpp), as lines of
// code, by bridge_code (bridge_code.cpp), which makes no bridge, paragraph
// or box per line. The mode is chosen when the document is typeset (the
// variable is then an input of the bridge, see bridge.cpp): the bridges of
// the paragraphs, one for each line, are only made in the paragraph mode
#define DOC_UNKNOWN 0
#define DOC_PARAGRAPHS 1
#define DOC_CODE 2

bridge bridge_code (typesetter ttt, tree st, path ip);

static bool
has_paragraphs_line (tree doc) {
  // a line which holds paragraphs (a document, a with of a document...):
  // as a line of code, it would be typeset by the concater, its paragraphs
  // as lines which do not wrap; the document is then typeset as paragraphs
  // (a line of a source file is a string: the test is quick)
  for (int i=0; i<N(doc); i++)
    if (!is_atomic (doc[i]) && is_multi_paragraph (doc[i])) return true;
  return false;
}

class bridge_document_rep: public bridge_rep {
protected:
  int    mode; // DOC_UNKNOWN before the first typesetting
  bridge code; // the bridge of the lines of code (DOC_CODE)
  array<bridge> brs;
  bridge acc; // binary splitting acceleration for long documents
  hashmap<int,bridge> blocks; // by the number of their first paragraph
  bool big_body;  // (while it is typeset, in the body of a macro)
  void typeset_one (int i, int n, int desired_status,
                    array<line_item>& a, array<line_item>& b);

public:
  bridge_document_rep (typesetter ttt, tree st, path ip);
  void initialize ();
  void initialize_acc ();
  void set_mode (int m);
  int  wanted_mode ();

  void notify_assign (path p, tree u);
  void notify_insert (path p, tree u);
  void notify_remove (path p, int nr);
  void notify_split  (path p);
  void notify_join   (path p);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_exec_until (path p);
  bool my_typeset_will_be_complete ();
  void my_typeset (int desired_status);
  void my_typeset_paragraphs (int desired_status);
};

bridge_document_rep::bridge_document_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip), mode (DOC_UNKNOWN), big_body (false) {}

int
bridge_document_rep::wanted_mode () {
  // the mode of the document in the environment where it is: lines of code
  // when code-lines is true, unless it has no lines or a line of paragraphs
  return env->read ("code-lines") == "true" && N(st) > 0 &&
         !has_paragraphs_line (st)? DOC_CODE: DOC_PARAGRAPHS;
}

void
bridge_document_rep::set_mode (int m) {
  // the bridges of the mode m (those of the other mode are dropped)
  if (mode == m) return;
  mode= m;
  if (m == DOC_CODE) {
    brs= array<bridge> ();
    blocks= hashmap<int,bridge> ();
    acc= bridge ();
    code= bridge_code (ttt, st, ip);
  }
  else {
    code= bridge ();
    initialize ();
  }
  status= CORRUPTED;
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
  if (mode == DOC_CODE) {
    code->notify_assign (p, u); st= code->st; status= CORRUPTED; return; }
  if (is_nil (p) && mode == DOC_UNKNOWN) { st= u; status= CORRUPTED; return; }
  set_mode (DOC_PARAGRAPHS);
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
  if (mode == DOC_CODE) {
    code->notify_insert (p, u); st= code->st; status= CORRUPTED; return; }
  set_mode (DOC_PARAGRAPHS);
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
  if (mode == DOC_CODE) {
    code->notify_remove (p, nr); st= code->st; status= CORRUPTED; return; }
  set_mode (DOC_PARAGRAPHS);
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

void
bridge_document_rep::notify_split (path p) {
  if (mode == DOC_CODE) {
    code->notify_split (p); st= code->st; status= CORRUPTED; return; }
  bridge_rep::notify_split (p);
}

void
bridge_document_rep::notify_join (path p) {
  if (mode == DOC_CODE) {
    code->notify_join (p); st= code->st; status= CORRUPTED; return; }
  bridge_rep::notify_join (p);
}

bool
bridge_document_rep::notify_macro (int tp, string var, int l, path p, tree u) {
  if (mode == DOC_CODE) {
    bool flag= code->notify_macro (tp, var, l, p, u);
    if (flag) status= CORRUPTED;
    return flag;
  }
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
  if (mode == DOC_CODE) { code->notify_change (); return; }
  if (!is_nil (acc)) acc->notify_change ();
  if (N(brs)>0) brs[0]->notify_change ();
  if (N(brs)>1) brs[N(brs)-1]->notify_change ();
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_document_rep::my_exec_until (path p) {
  set_mode (wanted_mode ());
  if (mode == DOC_CODE) { code->my_exec_until (p); return; }
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
  // (not typeset yet, or in another mode now: the bridges of the mode, the
  // paragraphs or the lines, will all be made)
  if (mode == DOC_UNKNOWN || mode != wanted_mode ()) return true;
  if (mode == DOC_CODE) return code->my_typeset_will_be_complete ();
  if (is_nil (acc)) {
    int i, n= N(brs);
    for (i=0; i<n; i++)
      if (!brs[i]->my_typeset_will_be_complete ()) return false;
    return true;
  }
  else return acc->my_typeset_will_be_complete ();
}

// the size of a tree, counted up to a bound (the characters and the nodes)
static int
tree_size_upto (tree t, int bound) {
  if (is_atomic (t)) return 1 + N(t->label);
  int n= 1;
  for (int i=0; i<N(t) && n<bound; i++) n += tree_size_upto (t[i], bound - n);
  return n;
}

#define BIG_BODY 2000

void
bridge_document_rep::typeset_one (int i, int n, int desired_status,
                                  array<line_item>& a, array<line_item>& b) {
  //cout << "Typesetting " << st[i] << LF;
  int wanted= (i==n-1? desired_status & WANTED_MASK: WANTED_PARAGRAPH);
  ttt->a= (i==0  ? a: array<line_item> ());
  ttt->b= (i==n-1? b: array<line_item> ());
  // (the paragraphs of the body of a macro record when there are many or
  // when they are large)
  bridge_record_next= !bridge_in_macro || n >= 16 || big_body;
  brs[i]->typeset (PROCESSED+ wanted);
}

void
bridge_document_rep::my_typeset (int desired_status) {
  //cout << INDENT;
  // (lines of code, or paragraphs: see above)
  // (a document without lines is typeset as paragraphs: the code bridge
  // would have a line which is not in the tree; so is a document with a
  // line of paragraphs, see has_paragraphs_line)
  bool code_lines= (env->read ("code-lines") == "true");
  set_mode (wanted_mode ());
  if (mode == DOC_CODE) {
    code->typeset (desired_status);
    st= code->st;
    return;
  }
  // (code-lines is the mode of this document only: the documents inside
  // its paragraphs are typeset as paragraphs)
  tree old_mode;
  if (code_lines) old_mode= env->local_begin ("code-lines", "false");
  my_typeset_paragraphs (desired_status);
  if (code_lines) env->local_end ("code-lines", old_mode);
}

void
bridge_document_rep::my_typeset_paragraphs (int desired_status) {
  if (is_nil (acc)) {
    int i, n= N(st);
    array<line_item> a= ttt->a;
    array<line_item> b= ttt->b;
    big_body= (bridge_in_macro && n < 16 &&
               tree_size_upto (st, BIG_BODY) >= BIG_BODY);
    hashmap<int,bridge> old_blocks= blocks;
    blocks= hashmap<int,bridge> ();
    // (the blocks are bridges used again when their inputs are the same:
    // the classic cache would derive old_patch from their changes)
    bool use_blocks= (typeset_caches_mode () != 0 &&
                      typeset_inputs_mode () != 0);
    for (int s= 0, e; s < n; s= e) {
      // block s..e-1, without the first and the last paragraph
      e= s + 1;
      if (s == 0 || !use_blocks) {
        typeset_one (s, n, desired_status, a, b);
        continue;
      }
      while (e < n-1 && e - s < BLOCK_MAX && !block_boundary (brs[e])) e++;
      if (e - s < 3) {
        for (i= s; i < e; i++) typeset_one (i, n, desired_status, a, b);
        continue;
      }
      int key= brs[s]->serial;
      bridge blk= (old_blocks->contains (key)? old_blocks [key]:
                   bridge_block (ttt, ip));
      blocks (key)= blk;
      bridge_block_rep* br= (bridge_block_rep*) blk.operator-> ();
      br->set_paragraphs (range (brs, s, e));
      ttt->a= array<line_item> ();
      ttt->b= array<line_item> ();
      bridge_record_next= true;
      blk->typeset (PROCESSED + WANTED_PARAGRAPH);
    }
  }
  else acc->my_typeset (desired_status);
  //cout << UNINDENT;
}
