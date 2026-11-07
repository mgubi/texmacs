
/******************************************************************************
* MODULE     : db_sort.cpp
* DESCRIPTION: Sorting the output of TeXmacs database queries
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Database/database.hpp"
#include "analyze.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Lexicographical sorting of tuples
******************************************************************************/

// The last component of each tuple is the identifier, which is always
// sorted in ascending order and only serves as a final tie-break.
// The other components are sorted according to lex_sort_dirs.

static array<bool> lex_sort_dirs;

struct lex_sort_leq {
  static bool leq (strings& a1, strings& a2) {
    int i;
    for (i=0; i<N(a1) && i<N(a2); i++) {
      bool up= (i >= N(lex_sort_dirs) || lex_sort_dirs[i]);
      if (a1[i] < a2[i]) return up;
      if (a2[i] < a1[i]) return !up;
    }
    if (i<N(a2)) return true;
    if (i<N(a1)) return false;
    return true;
  }
};

static void
lex_sort (array<strings>& a, array<bool> dirs) {
  lex_sort_dirs= dirs;
  merge_sort_leq<strings, lex_sort_leq> (a);
  lex_sort_dirs= array<bool> ();
}

/******************************************************************************
* A posteriori sorting
******************************************************************************/

array<strings>
database_rep::build_sort_tuples (db_atoms ids, db_atoms attrs, db_time t) {
  array<strings> r;
  for (int i=0; i<N(ids); i++) {
    strings e;
    db_line_nrs nrs= id_lines[ids[i]];
    for (int a=0; a<N(attrs); a++) {
      string found;
      for (int j=0; j<N(nrs); j++) {
        db_line& l= db[nrs[j]];
        if ((t == 0) || (l->created <= t && t < l->expires))
          if (l->attr == attrs[a])
            found= from_atom (l->val);
      }
      e << found;
    }
    e << from_atom (ids[i]);
    r << e;
  }
  return r;
}

db_atoms
database_rep::sort_results (db_atoms ids, tree q, db_time t) {
  if (!is_tuple (q)) return ids;
  db_atoms attrs;
  array<bool> dirs;
  for (int i=0; i<N(q); i++)
    if (is_tuple (q[i], "order", 2) &&
        is_atomic (q[i][1]) &&
        is_quoted (q[i][1]->label) &&
        atom_encode->contains (scm_unquote (q[i][1]->label)) &&
        is_atomic (q[i][2])) {
      attrs << atom_encode [scm_unquote (q[i][1]->label)];
      dirs  << (q[i][2] != "#f");
    }
  //cout << "Sorting " << ids << ", " << attrs << ", " << dirs << LF;
  if (N(attrs) == 0) return ids;
  array<strings> a= build_sort_tuples (ids, attrs, t);
  //cout << "Tuples " << a << LF;
  lex_sort (a, dirs);
  //cout << "Sorted " << a << LF;
  db_atoms r;
  for (int i=0; i<N(a); i++)
    r << as_atom (a[i][N(a[i]) - 1]);
  //cout << "Result " << r << LF;
  return r;
}
