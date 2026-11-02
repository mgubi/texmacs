
/******************************************************************************
* MODULE     : edit_text.cpp
* DESCRIPTION: modification of text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_text.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "scheme.hpp"
#include "patch.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_text_rep::edit_text_rep (): return_depth (-1) {}
edit_text_rep::~edit_text_rep () {}

/******************************************************************************
* Correction routines for the edit tree
******************************************************************************/

void
edit_text_rep::correct_concat (path p, int done) {
  tree t (subtree (et, p));
  if (L(t) != CONCAT) {
    failed_error << "The tree was '" << t << "'\n";
    FAILED ("concat expected");
  }

  int i, n= N(t);
  if (n == 0) { assign (p, string ("")); return; }
  if (n == 1) { remove_node (p * 0); return; }
  for (i=done; i<n; i++) {
    if (t[i] == "") {
      remove (p * i, 1);
      correct_concat (p, i);
      return;
    }
    if ((i<n-1) && is_atomic (t[i]) && is_atomic (t[i+1])) {
      join (p * i);
      correct_concat (p, i);
      return;
    }
    if (is_concat (t[i])) {
      insert_node (p * 0, CONCAT);
      split (p * path (0, i));
      split (p * path (1, 1));
      remove_node (p * path (1, 0));
      if (subtree (et, p * 0) == tree (CONCAT)) remove (p * 0, 1);
      else join (p * 0);
      if (subtree (et, p * 1) == tree (CONCAT)) remove (p * 1, 1);
      else join (p * 0);
      remove_node (p * 0);
      correct_concat (p, max (i-1, 0));
      return;
    }
    else if (is_multi_paragraph (t[i]) &&
             is_document (subtree (et, path_up (p))))
      {
        if ((i+1)<n) {
          split (p * (i+1));
          correct_concat (path_inc (p));
        }
        if (i==0) remove_node (p * 0);
        else {
          split (p * i);
          remove_node (path_inc (p) * 0);
          correct_concat (p);
        }
        return;
      }
  }
}

void
edit_text_rep::correct (path p) {
  tree t (subtree (et, p));
  if (L(t) == CONCAT) correct_concat (p);
  if (t == "") correct (path_up (p));
}

/******************************************************************************
* Inserting line breaks (text-mode only)
******************************************************************************/

bool
edit_text_rep::pure_line (path p) {
  p= path_up (p);
  tree st= subtree (et, path_up (p));
  return
    is_document (st) ||
    ((is_func (st, WITH) || is_func (st, LOCUS) ||
      is_func (st, CANVAS) ||
      is_func (st, ORNAMENT) || is_func (st, ART_BOX) ||
      is_func (st, WITH_PACKAGE)) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    (is_extension (st) && (last_item (p) >= 0) && pure_line (p));
}

bool
edit_text_rep::accepts_return (path p) {
  tree st= subtree (et, path_up (p));
  return
    is_document (st) ||
    (is_func (st, SURROUND, 3) && (last_item (p) == 2)) ||
    (is_func (st, _FLOAT) && (last_item (p) == (N(st)-1))) ||
    (is_func (st, DATOMS) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    (is_func (st, MACRO) && (last_item (p) == (N(st)-1))) ||
    (is_func (st, XMACRO, 2) && (last_item (p) == 1)) ||
    ((is_func (st, WITH) || is_mod_active (st) ||
      is_func (st, STYLE_WITH) || is_func (st, VAR_STYLE_WITH) ||
      is_func (st, LOCUS) || is_func (st, WITH_PACKAGE) ||
      is_func (st, CANVAS)) &&
     (last_item (p) == (N(st)-1)) && pure_line (p)) ||
    ((is_func (st, ORNAMENT) || is_func (st, ART_BOX)) &&
     (last_item (p) == 0) && pure_line (p)) ||
    (is_extension (st) && (last_item (p) >= 0) && pure_line (p));
}

bool
edit_text_rep::insert_return () {
  // return_depth: the length of the path of the new paragraph, the head of
  // tp (the cursor may be inside a tag at the start of that paragraph)
  return_depth= -1;
  if (!accepts_paragraphs (tp)) return true;
  if (accepts_return (path_up (tp))) {
    // At the border of a tag which has no cursor position there (an inner
    // border, as hidden), the change corrects the cursor into the tag: the
    // tag would be split, not its paragraph
    path q= path_up (tp);
    int  l= last_item (tp);
    insert_node (q * 0, CONCAT);
    go_to (q * path (0, l));
  }
  path p= path_up (tp, 2);
  if (!is_document (subtree (et, path_up (p)))) {
    int l= last_item (tp);
    path q= path_up (tp);
    insert_node (p * 0, DOCUMENT);
    p= p * 0;
    go_to (p * path (last_item (q), l));
  }
  return_depth= N (p);

  // (where to split the line, found before the string is split: splitting
  // it at its start or end puts an empty string beside a tag with an inner
  // border, as hidden, and the change then corrects the cursor into that tag)
  path q= path_up (tp);
  if (is_atomic (subtree (et, q))) {
    split (tp);
    split (path_inc (q));
  }
  else if (last_item (tp)==right_index (subtree (et, q)))
    split (path_inc (q));
  else split (q);
  go_to (correct_cursor (et, path_inc (p) * 0));
  correct_concat (p);
  correct_concat (path_inc (p));
  return false;
}

void
edit_text_rep::remove_return (path p) {
  if (!is_document (subtree (et, path_up (p))))
    FAILED ("parent is not a document");

  if (!is_concat (subtree (et, p)))
    insert_node (p * 0, CONCAT);
  if (!is_concat (subtree (et, path_inc (p))))
    insert_node (path_inc (p) * 0, CONCAT);
  join (p);
  correct_concat (p);
}

/******************************************************************************
* Inserting a simple formula or tree
******************************************************************************/

bool
edit_text_rep::accepts_paragraphs (path p) {
  // can insert_return make a new paragraph at the cursor position p?
  // (not around the table format of a tabular: a position of scripts only)
  if (is_func (subtree (et, path_up (p)), TFORMAT)) return false;
  if (accepts_return (path_up (p))) return true;
  path q= path_up (p, 2);
  return is_concat (subtree (et, q)) && accepts_return (q);
}

bool
edit_text_rep::can_insert_at (tree t, path p) {
  // can t be inserted at the cursor position p? Not paragraphs inside a
  // line: in a tag of a line, a cell of a table without paragraphs, a formula
  if (!(rp < p) || !is_inside (et, p)) return false;
  if (is_document (t)) return accepts_paragraphs (p);
  if (!is_multi_paragraph (t)) return true;
  // (see make_return_after: a block goes before its paragraph)
  path q= p;
  while (!is_document (subtree (et, path_up (q)))) {
    q= path_up (q);
    if (!(rp < q)) return true;
  }
  return correct_cursor (et, p) == start (et, q) || accepts_paragraphs (p);
}

path
edit_text_rep::prepare_for_insert () {
  path p = path_up (tp);
  int  l = last_item (tp);
  tree st= subtree (et, p);

  if ((!is_document (st)) && is_multi_paragraph (st)) {
    if (!is_document (subtree (et, path_up (p))))
      insert_node (p * 0, DOCUMENT);
    else {
      if (l == 0) {
        insert (p, tree (DOCUMENT, ""));
        go_to (p * 0);
      }
      else {
        insert (path_inc (p), tree (DOCUMENT, ""));
        go_to (path_inc (p) * 0);
      }
    }
    return prepare_for_insert ();
  }

  if ((rp < p) && is_concat (subtree (et, path_up (p)))) {
    if (l==0) return p;
    if (is_compound (st) || (l==N(st->label))) return path_inc (p);
    split (tp);
    return path_inc (p);
  }
  
  insert_node (p * 0, CONCAT);
  if (is_atomic (st) && (l!=0) && (l!=N(st->label))) {
    split (p * path (0, l));
    return p * 1;
  }
  return p * ((l==0)? 0: 1);
}

void
edit_text_rep::insert_tree (tree t, path p_in_t) {
  bool cut= !as_bool (call ("like-emacs?"));
  bool tested= false;   // the paragraphs were tested for the cut selection
  bool cells = false;   // the selection is a selection of cells
  path old_tp= tp;
  range_set old_sel;
  bool old_selecting= false, old_shift= false;
  path old_mid;
  // The cut of cells may leave no place for the paragraphs: it is made
  // between marks of the history of this buffer, to be cancelled alone
  struct cut_mark_rep {
    edit_text_rep* ed; double m; bool on;
    cut_mark_rep (): ed (NULL), m (0), on (false) {}
    ~cut_mark_rep () { if (on) ed->mark_end (m); }
  } cut_mark;
  if (is_multi_paragraph (t)) {
    // (where selection_cut will leave the cursor: it corrects the range)
    path ip= tp;
    if (cut && selection_active_any ()) {
      if (selection_active_table ()) {
        // (the cut empties the cells: see below)
        ip  = selection_get_start ();
        selection_get_state (old_sel, old_selecting, old_shift, old_mid);
        cells= true;
      }
      else {
        ip    = selection_var_get_start ();
        tested= true;
      }
    }
    if (!can_insert_at (t, ip)) {
      // (nothing changes: the selection is not cut for nothing)
      set_message ("Several paragraphs cannot be inserted here", "insert");
      return;
    }
  }
  if (cells) {
    cut_mark.ed= this; cut_mark.m= new_marker (); cut_mark.on= true;
    mark_start (cut_mark.m);
  }
  if (cut) selection_cut ("none");
  if (cells && !can_insert_at (t, tp)) {
    // The cut left no place for paragraphs (the cells are not block cells,
    // or they lost their format): the cut is undone, the cells are not
    // cleared for nothing (only the changes since the mark)
    cut_mark.on= false;
    mark_cancel (cut_mark.m);
    go_to (old_tp);
    selection_set_state (old_sel, old_selecting, old_shift, old_mid);
    set_message ("Several paragraphs cannot replace cells", "insert");
    return;
  }
  if (tested && !can_insert_at (t, tp)) {
    // The cut leaves the cursor in the tag next to the range when it removes
    // the text up to the tag (a tag with an inner border, as hidden): the
    // paragraphs go at the border of that tag, where the range began
    for (path u= path_up (tp); rp < u; u= path_up (u))
      if (is_compound (subtree (et, u))) {
        path q;
        if (tp == end (et, u)) q= u * 1;
        else if (tp == start (et, u)) q= u * 0;
        else continue;
        if (can_insert_at (t, q)) {
          go_to (q);
          break;
        }
      }
  }
  if (is_atomic (t) && (p_in_t == end (t)) &&
      is_atomic (subtree (et, path_up (tp))))
    insert (tp, t);
  else if (is_document (t)) {
    if (subtree (et, path_up (tp)) == "" &&
        accepts_return (path_up (tp)) &&
        !is_func (subtree (et, path_up (tp, 2)), DOCUMENT))
      insert_node (path_up (tp) * 0, DOCUMENT);
    if (insert_return ()) return;
    path p= head (tp, return_depth);
    bool empty= (subtree (et, p) == "");
    if (empty) remove (p, 1);
    insert (p, t);
    path q= path_add (p, p_in_t->item) * p_in_t->next;
    go_to (correct_cursor (et, q));
    remove_return (path_dec (p));
    if (!empty) remove_return (path_add (p, N(t)-2));
  }
  else if (is_multi_paragraph (t)) {
    if (subtree (et, path_up (tp)) == "" &&
        accepts_return (path_up (tp)) &&
        !is_func (subtree (et, path_up (tp, 2)), DOCUMENT))
      insert_node (path_up (tp) * 0, DOCUMENT);
    return_depth= -1;
    if (make_return_after ()) return;
    path p= (return_depth >= 0? head (tp, return_depth):
                                search_parent_upwards (DOCUMENT));
    if (subtree (et, p) == "") remove (p, 1);
    insert (p, tree (DOCUMENT, t));
    go_to (correct_cursor (et, p * p_in_t));
  }
  else {
    path p= prepare_for_insert ();
    if (!is_concat (t)) { t= tree (CONCAT, t); p_in_t= path (0, p_in_t); }
    insert (p, t);
    path q= path_add (p, p_in_t->item) * p_in_t->next;
    go_to (correct_cursor (et, q));
    correct_concat (path_up (p));
  }
}

void
edit_text_rep::insert_tree (tree t) {
  t= copy (simplify_correct (t));
  insert_tree (t, end (t));
}

void
edit_text_rep::var_insert_tree (tree t, path p_in_t) {
  if (selection_active_any ()) {
    selection_cut ("primary");
    insert_tree (t, p_in_t);
    selection_paste ("primary");
  }
  else insert_tree (t, p_in_t);
}

/******************************************************************************
* Spaces
******************************************************************************/

static string
get_unit (tree t) {
  int i;
  string s= as_string (t);
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  return s (i, N(s));
}

static string
get_quantity (tree t) {
  int i;
  string s= as_string (t);
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  return s (0, i);
}

void
edit_text_rep::make_space (tree u) {
  if (is_atomic (u)) return;
  tree t= subtree (et, path_up (tp));

  int i, n= N(u);
  bool flag= is_func (t, L (u), n);
  for (i=0; i<n; i++) {
    if (!flag) break;
    string u1= get_unit (t[i]);
    string u2= get_unit (u[i]);
    double x1= as_double (get_quantity (t[i]));
    double x2= as_double (get_quantity (u[i]));
    flag= flag && (u1==u2) && ((x1*x2)>0);
  }
  
  if (flag) {
    for (i=0; i<n; i++) {
      double w1= as_double (get_quantity (t[i]));
      double w2= as_double (get_quantity (u[i]));
      string sum= as_string (w1+w2) * get_unit (t[i]);
      assign (path_up (tp) * i, sum);
    }
  }
  else insert_tree (u);
}

void
edit_text_rep::make_space (string w) {
  make_space (tree (SPACE, w));
}

void
edit_text_rep::make_space (string w, string y1, string y2) {
  insert_tree (tree (SPACE, w, y1, y2));
}

void
edit_text_rep::make_hspace (string s) {
  make_space (tree (HSPACE, s));
}

void
edit_text_rep::make_hspace (string smin, string sdef, string smax) {
  make_space (tree (HSPACE, smin, sdef, smax));
}

void
edit_text_rep::make_vspace_before (string s) {
  make_space (tree (VAR_VSPACE, s));
}

void
edit_text_rep::make_vspace_before (string smin, string sdef, string smax) {
  make_space (tree (VAR_VSPACE, smin, sdef, smax));
}

void
edit_text_rep::make_vspace_after (string s) {
  make_space (tree (VSPACE, s));
}

void
edit_text_rep::make_vspace_after (string smin, string sdef, string smax) {
  make_space (tree (VSPACE, smin, sdef, smax));
}

/******************************************************************************
* Insert formatting objects and miscellaneous
******************************************************************************/

void
edit_text_rep::make_htab (string spc) {
  insert_tree (tree (HTAB, spc));
}

void
edit_text_rep::make_image (
  string file_name, bool link, string w, string h, string x, string y)
{
  url image= url_unix (file_name);
  string type= "";
  tree t (IMAGE);
  if (link) {
    if (is_rooted (image, "default")) image= reroot (image, "file");
    // here we convert the filename to cork-universal because it is exposed in the document, in the <image> tag
    t << utf8_to_cork (as_string (image, URL_STANDARD));
  }
  else {
    string s;
    tree vim= verbatim (utf8_to_cork (as_string (image)));
    load_string (relative (get_name (), image), s, false);
    if (s == "") {
      set_message (concat ("File '", vim, "' not found"), "make image");
      return;
    }
    type= suffix (image);
    if (type == "") {
      set_message (concat ("File '", vim, "' is not an image"), "make image");
      return;
    }
    t << tuple (tree (RAW_DATA, s), as_string (tail (image)));
  }
  t << tree (w) << tree (h) << tree (x) << tree (y);
  insert_tree (t);
}
