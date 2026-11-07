
/******************************************************************************
* MODULE     : list_observer.cpp
* DESCRIPTION: Attach several observers to trees
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* An inverse path observer maintains the inverse path of the position
* of the corresponding tree with respect to the global meta-tree.
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "modification.hpp"
#include "blackbox.hpp"

#define DETACHED (-5)

/******************************************************************************
* Definition of the list_observer_rep class
******************************************************************************/

static int observer_size (observer o);

class list_observer_rep: public observer_rep {
  observer o1;
  observer o2;

public:
  int size; // the number of observers when it was made (see insert_observer)

  list_observer_rep (observer o1b, observer o2b):
    o1 (o1b), o2 (o2b), size (observer_size (o1b) + observer_size (o2b)) {}
  int get_type () { return OBSERVER_LIST; }
  tm_ostream& print (tm_ostream& out) {
    if (!is_nil (o1)) o1->print (out);
    if (!is_nil (o2)) o2->print (out);
    return out; }

  void announce (tree& ref, modification mod);
  void done     (tree& ref, modification mod);
  void touched  (tree& ref, path p);

  void notify_assign      (tree& ref, tree t);
  void notify_insert      (tree& ref, int pos, int nr);
  void notify_remove      (tree& ref, int pos, int nr);
  void notify_split       (tree& ref, int pos, tree prev);
  void notify_var_split   (tree& ref, tree t1, tree t2);
  void notify_join        (tree& ref, int pos, tree next);
  void notify_var_join    (tree& ref, tree t, int offset);
  void notify_assign_node (tree& ref, tree_label op);
  void notify_insert_node (tree& ref, int pos);
  void notify_remove_node (tree& ref, int pos);
  void notify_set_cursor  (tree& ref, int pos, tree data);
  void notify_detach      (tree& ref, tree closest, bool right);

  bool get_ip (path& ip);
  bool set_ip (path ip);
  bool get_position (tree& t, int& index);
  bool set_position (tree t, int index);
  observer& get_child (int which);
  list<observer> get_tree_pointers ();
  bool get_tree (tree& t);
  bool get_contents (int kind, blackbox& bb);
  bool set_highlight (int lan, int col, int start, int end);
  bool get_highlight (int lan, array<int>& cols);
};

/******************************************************************************
* Call back routines for announcements
******************************************************************************/

void
list_observer_rep::announce (tree& ref, modification mod) {
  if (!is_nil (o1)) o1->announce (ref, mod);
  if (!is_nil (o2)) o2->announce (ref, mod);
}

void
list_observer_rep::done (tree& ref, modification mod) {
  if (!is_nil (o1)) o1->done (ref, mod);
  if (!is_nil (o2)) o2->done (ref, mod);
}

void
list_observer_rep::touched (tree& ref, path p) {
  if (!is_nil (o1)) o1->touched (ref, p);
  if (!is_nil (o2)) o2->touched (ref, p);
}

/******************************************************************************
* Call back routines for modifications
******************************************************************************/

void
list_observer_rep::notify_assign (tree& ref, tree t) {
  if (!is_nil (o1)) o1->notify_assign (ref, t);
  if (!is_nil (o2)) o2->notify_assign (ref, t);
}

void
list_observer_rep::notify_insert (tree& ref, int pos, int nr) {
  if (!is_nil (o1)) o1->notify_insert (ref, pos, nr);
  if (!is_nil (o2)) o2->notify_insert (ref, pos, nr);
}

void
list_observer_rep::notify_remove (tree& ref, int pos, int nr) {
  if (!is_nil (o1)) o1->notify_remove (ref, pos, nr);
  if (!is_nil (o2)) o2->notify_remove (ref, pos, nr);
}

void
list_observer_rep::notify_split (tree& ref, int pos, tree prev) {
  if (!is_nil (o1)) o1->notify_split (ref, pos, prev);
  if (!is_nil (o2)) o2->notify_split (ref, pos, prev);
}

void
list_observer_rep::notify_var_split (tree& ref, tree t1, tree t2) {
  if (!is_nil (o1)) o1->notify_var_split (ref, t1, t2);
  if (!is_nil (o2)) o2->notify_var_split (ref, t1, t2);
}

void
list_observer_rep::notify_join (tree& ref, int pos, tree next) {
  if (!is_nil (o1)) o1->notify_join (ref, pos, next);
  if (!is_nil (o2)) o2->notify_join (ref, pos, next);
}

void
list_observer_rep::notify_var_join (tree& ref, tree t, int offset) {
  if (!is_nil (o1)) o1->notify_var_join (ref, t, offset);
  if (!is_nil (o2)) o2->notify_var_join (ref, t, offset);
}

void
list_observer_rep::notify_assign_node (tree& ref, tree_label op) {
  if (!is_nil (o1)) o1->notify_assign_node (ref, op);
  if (!is_nil (o2)) o2->notify_assign_node (ref, op);
}

void
list_observer_rep::notify_insert_node (tree& ref, int pos) {
  if (!is_nil (o1)) o1->notify_insert_node (ref, pos);
  if (!is_nil (o2)) o2->notify_insert_node (ref, pos);
}

void
list_observer_rep::notify_remove_node (tree& ref, int pos) {
  if (!is_nil (o1)) o1->notify_remove_node (ref, pos);
  if (!is_nil (o2)) o2->notify_remove_node (ref, pos);
}

void
list_observer_rep::notify_set_cursor (tree& ref, int pos, tree data) {
  if (!is_nil (o1)) o1->notify_set_cursor (ref, pos, data);
  if (!is_nil (o2)) o2->notify_set_cursor (ref, pos, data);
}

void
list_observer_rep::notify_detach (tree& ref, tree closest, bool right) {
  if (!is_nil (o1)) o1->notify_detach (ref, closest, right);
  if (!is_nil (o2)) o2->notify_detach (ref, closest, right);
}

/******************************************************************************
* Further methods for special types of observers
******************************************************************************/

bool
list_observer_rep::get_ip (path& ip) {
  return (!is_nil (o1) && o1->get_ip (ip)) ||
         (!is_nil (o2) && o2->get_ip (ip));
}

bool
list_observer_rep::set_ip (path ip) {
  return (!is_nil (o1) && o1->set_ip (ip)) ||
         (!is_nil (o2) && o2->set_ip (ip));
}

bool
list_observer_rep::get_position (tree& t, int& index) {
  return (!is_nil (o1) && o1->get_position (t, index)) ||
         (!is_nil (o2) && o2->get_position (t, index));
}

bool
list_observer_rep::set_position (tree t, int index) {
  return (!is_nil (o1) && o1->set_position (t, index)) ||
         (!is_nil (o2) && o2->set_position (t, index));
}

observer&
list_observer_rep::get_child (int which) {
  if (which == 0) return o1;
  else return o2;
}

list<observer>
list_observer_rep::get_tree_pointers () {
  list<observer> l;
  if (!is_nil (o1)) l= l * o1->get_tree_pointers ();
  if (!is_nil (o2)) l= l * o2->get_tree_pointers ();
  return l;
}

bool
list_observer_rep::get_tree (tree& t) {
  return (!is_nil (o1) && o1->get_tree (t)) ||
         (!is_nil (o2) && o2->get_tree (t));
}

bool
list_observer_rep::get_contents (int kind, blackbox& bb) {
  return (!is_nil (o1) && o1->get_contents (kind, bb)) ||
         (!is_nil (o2) && o2->get_contents (kind, bb));
}

bool
list_observer_rep::set_highlight (int lan, int col, int start, int end) {
  return (!is_nil (o1) && o1->set_highlight (lan, col, start, end)) ||
         (!is_nil (o2) && o2->set_highlight (lan, col, start, end));
}

bool
list_observer_rep::get_highlight (int lan, array<int>& cols) {
  return (!is_nil (o1) && o1->get_highlight (lan, cols)) ||
         (!is_nil (o2) && o2->get_highlight (lan, cols));
}

/******************************************************************************
* Creation of list observers
******************************************************************************/

observer
list_observer (observer o1, observer o2) {
  if (is_nil (o1)) return o2;
  if (is_nil (o2)) return o1;
  return tm_new<list_observer_rep> (o1, o2);
}

static int
observer_size (observer o) {
  if (is_nil (o)) return 0;
  if (o->get_type () != OBSERVER_LIST) return 1;
  return ((list_observer_rep*) o.operator -> ())->size;
}

static void
insert_balanced (observer& o, observer what) {
  // what is added at the right of the list o, which stays balanced as a
  // binary counter: a list whose left part has more observers than its
  // right part takes what in its right part, else it becomes the left part
  // of a new list. The functions of the observers call themselves on the
  // two parts of a list: a list which leaned to the left, as long as its
  // observers were many (a few thousand: the hits of a search), overflowed
  // the stack of the worker of Tau. It is as deep as the logarithm now
  // (the sizes are those at the creation of the lists: after a removal
  // they may be larger than they are, which unbalances, but no more).
  // The lists are changed in place, never made again: an observer may be
  // added while the list is told of a change (it is in the middle of it)
  if (!is_nil (o) && o->get_type () == OBSERVER_LIST) {
    list_observer_rep* rep= (list_observer_rep*) o.operator -> ();
    observer& l= o->get_child (0);
    observer& r= o->get_child (1);
    if (observer_size (l) > observer_size (r)) {
      insert_balanced (r, what);
      rep->size++;
      return;
    }
  }
  o= list_observer (o, what);
}

void
insert_observer (observer& o, observer what) {
  insert_balanced (o, what);
}

void
remove_observer (observer& o, observer what) {
  if (is_nil (o)) return;
  else if (o == what) o= nil_observer;
  else {
    remove_observer (o->get_child (0), what);
    remove_observer (o->get_child (1), what);
  }
}

void
clean_observers (observer& o) {
  if (is_nil (o)) return;
  if (o->get_type () == OBSERVER_IP) return;
  if (o->get_type () == OBSERVER_LIST) {
    clean_observers (o->get_child (0));
    clean_observers (o->get_child (1));
    if (is_nil (o->get_child (0))) o= o->get_child (1);
    else if (is_nil (o->get_child (1))) o= o->get_child (0);
  }
  else o= nil_observer;
}

void
attach_observer (tree& ref, observer o) {
  insert_observer (ref->obs, o);
}

void
detach_observer (tree& ref, observer o) {
  remove_observer (ref->obs, o);
}

void
clean_observers (tree& ref) {
  clean_observers (ref->obs);
}
