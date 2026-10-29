
/******************************************************************************
* MODULE     : tree_search.cpp
* DESCRIPTION: Searching inside trees
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tree_search.hpp"
#include "analyze.hpp"
#include "boot.hpp"
#include "drd_mode.hpp"
#include "drd_std.hpp"
#include <set>
#include <utility>

int  search_max_hits= 1000000;
bool blank_match_flag= false;
bool initial_match_flag= false;
bool partial_match_flag= false;
bool injective_match_flag= false;
bool cascaded_match_flag= false;
bool case_insensitive_match_flag= false;

void search (range_set& sel, tree t, tree what, path p);
bool match (tree t, tree what);
void select (range_set& sel, tree t, tree what, path p);

tree_label WILDCARD= UNKNOWN;
tree_label SELECT_REGION= UNKNOWN;
tree_label SEARCH_ANCHOR= UNKNOWN;

/******************************************************************************
* Initialization and useful subroutines
******************************************************************************/

static void
initialize_search () {
  WILDCARD= make_tree_label ("wildcard");
  SELECT_REGION= make_tree_label ("select-region");
  SEARCH_ANCHOR= make_tree_label ("search-anchor");
  blank_match_flag=
    (get_user_preference ("allow-blank-match", "on") == "on");
  initial_match_flag=
    (get_user_preference ("allow-initial-match", "on") == "on");
  partial_match_flag=
    (get_user_preference ("allow-partial-match", "on") == "on");
  injective_match_flag=
    (get_user_preference ("allow-injective-match", "on") == "on");
  cascaded_match_flag=
    (get_user_preference ("allow-cascaded-match", "on") == "on");
  case_insensitive_match_flag=
    (get_user_preference ("case-insensitive-match", "off") == "on");
  
  if (get_user_preference ("search-and-replace", "on") == "on") {
    blank_match_flag=
      (get_user_preference ("allow-blank-replace", "off") == "on");
    initial_match_flag=
      (get_user_preference ("allow-initial-replace", "off") == "on");
    partial_match_flag=
      (get_user_preference ("allow-partial-replace", "off") == "on");
    injective_match_flag=
      (get_user_preference ("allow-injective-replace", "off") == "on");
  }
}

static void
merge (range_set& sel, range_set ssel) {
  for (int i=0; i+1<N(ssel); i+=2)
    if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], ssel[i]))
      sel << ssel[i] << ssel[i+1];
}

static bool
matches_between_wildcards (tree t, tree what) {
  // a formula pattern of wildcards around one tag, and a tree which matches
  // that tag: the wildcards take nothing (#495), so that *(strong a)*
  // matches (strong a) alone, in a paragraph or an argument, as (strong a)
  // does (and so does a pattern with regions made of it)
  if (!is_concat (what) || is_atomic (t)) return false;
  int k= -1;
  for (int i=0; i<N(what); i++)
    if (!is_func (what[i], WILDCARD, 1)) {
      if (k >= 0 || is_atomic (what[i])) return false;
      k= i;
    }
  return k >= 0 && match (t, what[k]);
}

static bool
is_accessible_for_search (tree t, int i) {
  if (is_accessible_child (t, i)) return true;
  if (is_func (t, HIDDEN)) return true;
  if (get_access_mode () != DRD_ACCESS_SOURCE) return false;
  if (is_func (t, RAW_DATA)) return false;
  return i >= 0 && i < N(t);
}

/******************************************************************************
* Matching complex patterns inside strings
******************************************************************************/

static bool
match_atomic_at (string s, tree what, int pos, int i, int& start, int& end,
                 array<int>& fail) {
  // match what from its element i at pos (for i == 0: starting at pos).
  // fail[i], for a wildcard what[i] followed by a string: the smallest
  // position from which it is known to fail. It then fails from any later
  // position too (the candidates are fewer), and the result from i > 0 does
  // not depend on the start: so each wildcard scans the string once, also
  // over all the starts of match_atomic (quadratic and cubic before). This
  // assumes Cork strings, in which < and > only enclose symbols (< and >
  // themselves are <less> and <gtr>): positions then stay on characters
  if (i == 0) start= pos;
  if (i >= N(what)) {
    end= pos;
    if (initial_match_flag) return true;
    return pos == N(s);
  }
  if (is_atomic (what[i])) {
    return test (s, pos, what[i]->label) &&
           match_atomic_at (s, what, pos + N(what[i]->label), i+1,
                            start, end, fail);
  }
  else if (is_func (what[i], WILDCARD, 1)) {
    if (i+1 >= N(what)) {
      end= N(s);
      return true;
    }
    if (is_func (what[i+1], WILDCARD, 1))
      return match_atomic_at (s, what, pos, i+1, start, end, fail);
    if (!is_atomic (what[i+1])) return false;
    if (pos >= fail[i]) return false;
    int from= pos;
    while (pos < N(s)) {
      pos= tm_search_forwards (what[i+1]->label, pos, s);
      if (pos < 0) break;
      if (match_atomic_at (s, what, pos, i+1, start, end, fail)) return true;
      if (pos < N(s)) tm_char_forwards (s, pos);
    }
    fail[i]= from;
    return false;
  }
  else return false;
}

static array<int>
no_failures (tree what, string s) {
  array<int> fail (N(what));
  for (int k=0; k<N(what); k++) fail[k]= N(s) + 1;
  return fail;
}

static bool
match_atomic_at (string s, tree what, int pos, int i, int& start, int& end) {
  array<int> fail= no_failures (what, s);
  return match_atomic_at (s, what, pos, i, start, end, fail);
}

bool
match_atomic (string s, tree what, int pos, int i, int& start, int& end) {
  array<int> fail= no_failures (what, s);
  if (i != 0 || !partial_match_flag || N(what) == 0 || !is_atomic (what[0]))
    return match_atomic_at (s, what, pos, i, start, end, fail);
  // partial matches: the match may start at pos or after it; the starts are
  // tried one character after another by a loop (a recursion per character
  // overflowed the stack on long strings, #479)
  while (true) {
    if (match_atomic_at (s, what, pos, 0, start, end, fail)) return true;
    if (pos >= N(s)) return false;
    tm_char_forwards (s, pos);
  }
}

/******************************************************************************
* Matching
******************************************************************************/

static void search_anchored (range_set& sel, tree t, tree what, path p,
                             bool at_start, bool at_end);

// the arguments in which a formula pattern was not found when searching,
// with all the accessible trees inside them: the pattern is not found
// inside them either, so that it is not searched again in each nested
// argument (in nested fractions, a power of the depth). For one search only
static std::set<std::pair<tree_rep*, tree_rep*> > no_formula;
static array<tree> no_formula_keep;

static void
clear_no_formula () {
  no_formula.clear ();
  no_formula_keep= array<tree> ();
}

static void
mark_no_formula (tree t, tree what) {
  if (is_atomic (t)) return;
  std::pair<tree_rep*, tree_rep*> key (inside (t), inside (what));
  if (no_formula.count (key) != 0) return;
  no_formula.insert (key);
  no_formula_keep << t;
  // only the trees which the search went into: a hidden argument may be
  // shared with a visible one elsewhere
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i)) mark_no_formula (t[i], what);
}

static int
lone_element (tree what) {
  // the element of a formula pattern which is a tag between wildcards
  // (*_x, _x*...), or -1
  int k= -1;
  for (int i=0; i<N(what); i++)
    if (!is_func (what[i], WILDCARD, 1)) {
      if (k >= 0 || is_atomic (what[i])) return -1;
      k= i;
    }
  return k;
}

static bool
lone_arguments (tree t, tree e) {
  // is there, inside t, an argument which is a tag alone and matches e? It
  // is a formula of one child, which a tag between wildcards matches; the
  // search does not see it as a formula
  for (int i=0; i<N(t); i++)
    if (is_compound (t[i]) && is_accessible_for_search (t, i)) {
      if (!is_concat (t) && !is_concat (t[i]) && match (t[i], e))
        return true;
      if (lone_arguments (t[i], e)) return true;
    }
  return false;
}

static bool
match_formula (tree t, tree what) {
  // a formula what (a concat) in the argument of a tag of the pattern, and
  // a tag argument t: the formula is text, matched as when searching (#495:
  // a match may start and end inside a string, a wildcard takes the
  // shortest run, possibly empty), not child by child (a wildcard took
  // exactly one child, and Search skipped children, #501). As for a string
  // argument, the flags say where: anywhere in t with partial matches, at
  // its start with initial matches, else the whole argument
  tree u= t;
  if (!is_concat (t)) u= tree (CONCAT, t);
  range_set sel;
  if (partial_match_flag && cascaded_match_flag) {
    // anywhere inside t, also in the arguments of its tags which are a tag
    // alone, which the cascade of Replace matches as formulas (#501)
    if (no_formula.count (std::pair<tree_rep*, tree_rep*>
                            (inside (t), inside (what))) != 0)
      return false;
    search (sel, u, what, path ());
    if (N(sel) != 0) return true;
    int k= lone_element (what);
    if (k >= 0 && lone_arguments (t, what[k])) return true;
    mark_no_formula (t, what);
    return false;
  }
  if (partial_match_flag) {
    search (sel, u, what, path ());
    // without cascaded matches, in the formula itself, not inside its tags
    for (int i=0; i+1<N(sel); i+=2)
      if (N(sel[i]) <= 2 && N(sel[i+1]) <= 2) return true;
    return false;
  }
  search_anchored (sel, u, what, path (), true, !initial_match_flag);
  return N(sel) != 0;
}

bool
match_cascaded (tree t, tree what) {
  if (is_concat (what) && is_compound (t)) {
    if (match_formula (t, what)) return true;
    // with partial matches, match_formula searched inside all the tags of
    // t already: the cascade would search them again at each level (in
    // nested fractions, a power of the depth)
    if (partial_match_flag && cascaded_match_flag) return false;
    if (is_concat (t)) {
      // a formula pattern inside the tags of the formula t, not as a part of
      // t (a child of t, or a run of children, is matched above)
      if (cascaded_match_flag)
        for (int i=0; i<N(t); i++)
          if (is_compound (t[i]))
            for (int j=0; j<N(t[i]); j++)
              if (is_accessible_for_search (t[i], j))
                if (match_cascaded (t[i][j], what))
                  return true;
      return false;
    }
  }
  else if (match (t, what)) return true;
  if (cascaded_match_flag && is_compound (t))
    for (int i=0; i<N(t); i++)
      if (is_accessible_for_search (t, i))
        if (match_cascaded (t[i], what))
          return true;
  return false;
}

static string
case_fold (string s) {
  // the strings compared, in lower case when the case does not matter
  return case_insensitive_match_flag? locase_all (s): s;
}

static tree
case_fold_pattern (tree what) {
  // idem for the strings of a concat pattern
  if (!case_insensitive_match_flag) return what;
  tree w (L(what), N(what));
  for (int i=0; i<N(what); i++)
    w[i]= is_atomic (what[i])? tree (locase_all (what[i]->label)): what[i];
  return w;
}

bool
match (tree t, tree what) {
  if (blank_match_flag && what == "") return true;
  if (is_func (what, WILDCARD, 1)) return true;
  if (is_atomic (t)) {
    string s= case_fold (t->label);
    if (is_concat (what)) {
      int start, end;
      return match_atomic (s, case_fold_pattern (what), 0, 0, start, end);
    }
    if (!is_atomic (what)) return false;
    string w= case_fold (what->label);
    if (partial_match_flag) {
      int pos= 0;
      return tm_search_forwards (w, pos, s) != -1;
    }
    else if (initial_match_flag)
      return starts (s, w);
    else
      return s == w;
  }
  else if (injective_match_flag) {
    if (L(t) != L(what)) return false;
    int cur=0;
    for (int i=0; i<N(t); i++) {
      if (cur >= N(what)) return true;
      if (match_cascaded (t[i], what[cur])) cur++;
    }
    return cur >= N(what);
  }
  else {
    if (L(t) != L(what) || N(t) != N(what)) return false;
    for (int i=0; i<N(t); i++)
      if (!match_cascaded (t[i], what[i])) return false;
    return true;
  }
}

/******************************************************************************
* Matching complex patterns inside concat tags
******************************************************************************/

static bool
search_concat (tree t, tree what, int pos, int i,
               path p, path cur, path& p1, path& p2,
               array<range_set>& inside, array<bool>& failed);

static bool
only_wildcards (tree what, int i) {
  for (; i<N(what); i++)
    if (!is_func (what[i], WILDCARD, 1)) return false;
  return true;
}

static bool
matches_whole (string s, tree w) {
  // does w, a string or a concat of strings and wildcards, match all of s?
  // The first string at the start, the last one at the end, the others at
  // their first place in between: for wildcards which take any run, this is
  // exact, and linear (a match_atomic anchored at both ends tried every
  // place of every string)
  s= case_fold (s);
  if (is_atomic (w)) return s == case_fold (w->label);
  array<tree> e;  // the elements, two strings in a row joined
  for (int i=0; i<N(w); i++) {
    tree x= is_atomic (w[i])? tree (case_fold (w[i]->label)): w[i];
    if (is_atomic (x) && N(e) > 0 && is_atomic (e[N(e)-1]))
      e[N(e)-1]= tree (e[N(e)-1]->label * x->label);
    else e << x;
  }
  int n= N(e), lo= 0, hi= N(s), a= 0, b= n;
  if (n == 0) return N(s) == 0;
  if (is_atomic (e[0])) {
    if (!test (s, 0, e[0]->label)) return false;
    lo= N(e[0]->label);
    a= 1;
  }
  if (a == n) return lo == N(s);
  if (is_atomic (e[n-1])) {
    int at= N(s) - N(e[n-1]->label), c= lo;
    while (c < at) tm_char_forwards (s, c);
    if (c != at || !test (s, at, e[n-1]->label)) return false;
    hi= at;
    b= n-1;
  }
  for (int i=a; i<b; i++)
    if (is_atomic (e[i])) {
      int at= tm_search_forwards (e[i]->label, lo, s);
      if (at < 0 || at + N(e[i]->label) > hi) return false;
      lo= at + N(e[i]->label);
    }
  return true;
}

static int
concat_occurrence (string s, tree w, path q, path cur, bool wildcard) {
  // idem for a concat w of strings and wildcards which starts with a
  // string (followed by a wildcard): the start, not before cur, of the
  // match which ends at the end of s, or of any match before a wildcard;
  // -1 if none. If a match starts at an occurrence of the first string,
  // one starts at the first occurrence (its wildcard takes the gap), so
  // only that one is tried; the hits of search_string, the shortest and
  // without overlaps, missed it (#481)
  s= case_fold (s);
  w= case_fold_pattern (w);
  if (N(w) == 0 || !is_atomic (w[0]) || N(w[0]->label) == 0) return -1;
  int from= 0;
  if (!path_less_eq (cur, q * 0)) {
    if (!(q <= cur)) return -1;
    from= (cur / q)->item;
  }
  int at= tm_search_forwards (w[0]->label, from, s);
  if (at < 0) return -1;
  bool initial= initial_match_flag;
  initial_match_flag= wildcard;
  int start, end;
  bool ok= match_atomic_at (s, w, at, 0, start, end);
  initial_match_flag= initial;
  return ok? at: -1;
}

static int
string_occurrence (string s, string w, path q, path cur, bool wildcard) {
  // the start of the occurrence of w in the string s (at the path q) with
  // which a match starts, not before cur: the first one when a wildcard
  // follows (the match furthest to the left, as in the same text as one
  // string), the one which ends at the end of s otherwise; -1 if none.
  // The occurrences may overlap (aa in aaa), unlike those which
  // search_string finds (#477)
  s= case_fold (s);
  w= case_fold (w);
  if (N(w) == 0) return -1;
  int from= 0;
  if (!path_less_eq (cur, q * 0)) {
    if (!(q <= cur)) return -1;
    from= (cur / q)->item;
  }
  int at= tm_search_forwards (w, from, s);
  if (at < 0 || wildcard) return at;
  int last= N(s) - N(w);
  while (at < last) tm_char_forwards (s, at);
  return (at == last && test (s, last, w))? last: -1;
}

// the results of search_concat_sub: besides a match and a failure, the
// result of search_concat at the next child, with the same i (a scan for
// the start of a match, or a wildcard which takes this child). Then
// search_concat goes on in a loop, not by a recursion per child, which
// overflowed the stack in long formulas (#487). With SC_NEXT_KEEP, p1 keeps
// the start set here
#define SC_FAIL 0
#define SC_FOUND 1
#define SC_NEXT 2
#define SC_NEXT_KEEP 3

static int
search_concat_sub (tree t, tree what, int pos, int i,
                   path p, path cur, path& p1, path& p2,
                   array<range_set>& inside, array<bool>& failed) {
  //cout << "Search " << what << ", " << i
  //     << " in " << t << ", " << pos << "\n";
  if (i > 0 && only_wildcards (what, i)) {
    // the rest of the pattern is wildcards: they take the rest of the
    // formula, also when no tag follows in it, or nothing (#453)
    p2= (p * (N(t)-1)) * end (t[N(t)-1]);
    return true;
  }
  if (pos >= N(t)) return false;
  if (is_atomic (t[pos])) {
    if (i == 0 && N(inside[pos]) != 0 &&
        path_less_eq (cur, inside[pos][N(inside[pos])-2]))
      // this string has matches of its own after cur: the outer loop
      // takes them (here, p1 and p2 below could come from different
      // matches and give a wrong range, #473)
      return false;
    int j, k= i;
    while (k<N(what) &&
           (is_atomic (what[k]) || is_func (what[k], WILDCARD, 1))) k++;
    for (j=k; j>i; j--) {
      if (is_func (what[j-1], WILDCARD, 1)) continue;
      // the whole pattern inside this string: the matches of the string
      // itself, which the outer loop takes (here p1 and p2 would come from
      // different matches when there are several, #473)
      if (i == 0 && j == N(what)) continue;
      tree swhat= what (i, j);
      if (j == i+1) swhat= swhat[0];
      if (i == 0 && is_atomic (swhat)) {
        // a string starts the match: at the occurrence that the rest of the
        // pattern needs (c1 holds at i == 0, c2 by this choice)
        bool wildcard= is_func (what[j], WILDCARD, 1);
        int at= string_occurrence (t[pos]->label, swhat->label, p * pos, cur,
                                   wildcard);
        if (at < 0) continue;
        p1= (p * pos) * at;
        if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
          return true;
        continue;
      }
      if (i == 0 && !is_atomic (swhat) && is_atomic (swhat[0])) {
        // idem for strings and wildcards which start with a string
        bool wildcard= is_func (what[j], WILDCARD, 1);
        int at= concat_occurrence (t[pos]->label, swhat, p * pos, cur,
                                   wildcard);
        if (at < 0) continue;
        p1= (p * pos) * at;
        if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
          return true;
        continue;
      }
      if (i == 0 && is_func (what[0], WILDCARD, 1) &&
          j < N(what) && !is_func (what[j], WILDCARD, 1)) {
        // a leading wildcard and strings before an element which is not a
        // wildcard: the string ends as the part does (the wildcard takes
        // the rest of it), and the match starts at the start of the formula
        // (the shortest hits of search_string missed bb at the end of bbb,
        // #489)
        p1= (p * 0) * start (t[0]);
        if (!path_less_eq (cur, p1)) return false;
        if (!matches_whole (t[pos]->label, swhat)) continue;
        if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
          return true;
        continue;
      }
      if (i > 0 && j < N(what) && !is_func (what[j], WILDCARD, 1)) {
        // between two elements which are not wildcards: the part of the
        // pattern covers the whole string, as one match (a hit at its start
        // and another one at its end let aa match aaaa, #480)
        if (!matches_whole (t[pos]->label, swhat)) continue;
        if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
          return true;
        continue;
      }
      if (j < N(what) && is_func (what[j], WILDCARD, 1) &&
          (i > 0 || is_func (what[0], WILDCARD, 1))) {
        // a wildcard follows, after a tag or a leading wildcard: the part
        // covers the start of the string (after a tag) or any piece of it
        // (after a leading wildcard), and the wildcard takes the rest. With
        // the replace matching, the hits of search_string cover the whole
        // string, and <wildcard>bb missed abba (#488)
        tree w (CONCAT);
        if (is_atomic (swhat)) w << swhat; else w= copy (swhat);
        w << what[j];
        if (i == 0) {
          p1= (p * 0) * start (t[0]);
          if (!path_less_eq (cur, p1)) return false;
        }
        if (!matches_whole (t[pos]->label, w)) continue;
        if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
          return true;
        continue;
      }
      range_set sel;
      search (sel, t[pos], swhat, p * pos);
      if (N(sel) != 0) {
        bool c1= (i == 0 || sel[0] == (p * pos) * start (t[pos]));
        bool c2= (j == N(what) || sel[N(sel)-1] == (p * pos) * end (t[pos]));
        if (j<N(what) && is_func (what[j], WILDCARD, 1)) c2= true;
        if (i == 0) {
          p1= sel[N(sel)-2];
          // a wildcard follows: the hit starts at the first occurrence
          // after cur, not the last (the match furthest to the left, as in
          // the same text as one string)
          if (j < N(what) && is_func (what[j], WILDCARD, 1))
            for (int h=0; h+1<N(sel); h+=2)
              if (path_less_eq (cur, sel[h])) { p1= sel[h]; break; }
        }
        if (j == N(what)) p2= sel[1];
        if (i == 0 && is_func (what[0], WILDCARD, 1))
          p1= (p * 0) * start (t[0]);
        if (c1 && c2 && (i > 0 || path_less_eq (cur, p1))) {
          if (j >= N(what)) return true;
          if (search_concat (t, what, pos+1, j, p, cur, p1, p2, inside, failed))
            return true;
        }
      }
    }
    if (i == 0 || is_func (what[i], WILDCARD, 1)) return SC_NEXT;
    return false;
  }
  else if (is_func (what[i], WILDCARD, 1)) {
    if (i == 0) {
      p1= (p * 0) * start (t[0]);
      if (!path_less_eq (cur, p1)) return false;
    }
    if (i+1 < N(what) && is_func (what[i+1], WILDCARD, 1))
      return search_concat (t, what, pos, i+1, p, cur, p1, p2, inside, failed);
    if (i+1 >= N(what)) {
      p2= (p * (N(t)-1)) * end (t[N(t)-1]);
      return true;
    }
    if (search_concat (t, what, pos, i+1, p, cur, p1, p2, inside, failed))
      return true;
    return SC_NEXT_KEEP;
  }
  else {
    // a tag of the pattern and a tag of the formula: unless it is the whole
    // pattern, the element covers the start or the end of the tag, so it
    // matches the tag itself (a hit inside it would not do: searching it
    // there cost a search of the tag at each place, #501)
    range_set sel;
    if (N(what) > 1) {
      if (match (t[pos], what[i]))
        sel << (p * pos) * start (t[pos]) << (p * pos) * end (t[pos]);
    }
    else search (sel, t[pos], what[i], p * pos);
    if (N(sel) != 0) {
      bool c1= (i == 0 || sel[0] == (p * pos) * start (t[pos]));
      bool c2= (i == N(what)-1 || sel[N(sel)-1] == (p * pos) * end (t[pos]));
      if (i == 0) p1= sel[0];
      if (i == N(what)-1) p2= sel[1];
      if (c1 && c2 && (i > 0 || path_less_eq (cur, p1))) {
        if (i+1 >= N(what)) return true;
        if (search_concat (t, what, pos+1, i+1, p, cur, p1, p2, inside, failed))
          return true;
      }
    }
    if (i == 0) {
      if (N(inside[pos]) != 0) return false;
      return SC_NEXT;
    }
    return false;
  }
}

static bool
search_concat (tree t, tree what, int pos, int i,
               path p, path cur, path& p1, path& p2,
               array<range_set>& inside, array<bool>& failed) {
  // for i > 0, the result depends on pos and i only (cur is only used at
  // i == 0): remember the failures, so that a middle wildcard does not
  // scan the rest of the formula again from each start
  int from= pos, r= SC_FAIL;
  bool keep= false;
  path start1;
  for (; ; pos++) {
    if (i > 0 && pos < N(t) && failed[pos * N(what) + i]) {
      r= SC_FAIL;
      break;
    }
    r= search_concat_sub (t, what, pos, i, p, cur, p1, p2, inside, failed);
    if (r == SC_NEXT_KEEP && !keep) { keep= true; start1= p1; }
    if (r != SC_NEXT && r != SC_NEXT_KEEP) break;
  }
  if (r == SC_FOUND) {
    if (keep) p1= start1;
    return true;
  }
  // the scans from from to pos all fail
  if (i > 0)
    for (int q= from; q <= pos && q < N(t); q++)
      failed[q * N(what) + i]= true;
  return false;
}

/******************************************************************************
* Searching
******************************************************************************/

void
search_string (range_set& sel, string s, tree what, path p, int from= 0) {
  // the hits of what in s, from the offset from on
  string source= case_fold (s);

  if (is_atomic (what)) {
    string w= case_fold (what->label);
    if (N(w) == 0) {
      // an empty string is found in an empty string only, once (the loop
      // below would not move: #452)
      if (N(s) == 0) merge (sel, simple_range (p * 0, p * 0));
      return;
    }
    int pos= from;
    while (pos < N(s)) {
      int next= tm_search_forwards (w, pos, source);
      if (next < 0 || next >= N(s)) break;
      merge (sel, simple_range (p * next, p * (next + N(w))));
      pos= next + N(w);
    }
  }
  else if (is_concat (what)) {
    // in the text, the matching of Replace is the one of Search: a match
    // may start and end inside a string, as for a string alone above. The
    // partial and initial flags (allow-partial-match, allow-partial-replace
    // ...) only concern the arguments of tags (match).
    // With Replace, a pattern with a wildcard covered its whole string, but
    // a pattern which starts with a string and a wildcard could start
    // inside one (#495)
    bool partial= partial_match_flag, initial= initial_match_flag;
    partial_match_flag= initial_match_flag= true;
    tree w= case_fold_pattern (what);
    for (int pos=from; pos<N(s); ) {
      int start, end;
      if (match_atomic (source, w, pos, 0, start, end)) {
        merge (sel, simple_range (p * start, p * end));
        pos= end;
      }
      else break;
    }
    partial_match_flag= partial;
    initial_match_flag= initial;
  }
}

static bool
is_lone_tag (tree t, tree what) {
  // a paragraph t which is a tag alone, and a formula pattern with a leading
  // or a trailing wildcard: the paragraph is a formula of one child, so that
  // the wildcard carries a hit inside the tag to the start or the end of the
  // paragraph, as when the tag is in a formula (#501)
  return is_compound (t) && !is_concat (t) && !is_document (t) &&
         is_concat (what) && N(what) > 0 &&
         (is_func (what[0], WILDCARD, 1) ||
          is_func (what[N(what)-1], WILDCARD, 1));
}

static void
unwrap (range_set& sel, range_set ssel, path p) {
  // the paths in t at p of the paths ssel in the formula (concat t) at p
  for (int i=0; i<N(ssel); i++) sel << (p * (ssel[i] / p)->next);
}

static void
search_paragraph_tree (range_set& sel, tree t, tree what, path p) {
  // the hits of what in the paragraph t
  if (!is_lone_tag (t, what)) {
    search (sel, t, what, p);
    return;
  }
  range_set ssel, usel;
  search (ssel, tree (CONCAT, t), what, p);
  unwrap (usel, ssel, p);
  merge (sel, usel);
}

void
search_compound (range_set& sel, tree t, tree what, path p) {
  if (match (t, what) || matches_between_wildcards (t, what)) {
    merge (sel, simple_range (p * start (t), p * end (t)));
    return;
  }
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i)) {
      if (is_document (t)) search_paragraph_tree (sel, t[i], what, p * i);
      else search (sel, t[i], what, p * i);
    }
}

static void search_after (range_set& sel, tree t, tree what, path p,
                          path q);

static bool
crosses_document (tree t, path p, path q) {
  // is there a document between t (at p) and the position q inside it?
  tree u= t;
  path r= q / p;
  while (!is_nil (r) && !is_nil (r->next) && is_compound (u) &&
         r->item >= 0 && r->item < N(u)) {
    u= u[r->item];
    if (is_document (u)) return true;
    r= r->next;
  }
  return false;
}

void
search_concat (range_set& sel, tree t, tree what, path p, path from) {
  // search_concat (t, what, pos, 0, ...) scans forwards from pos for the
  // start of a match, until it reaches a compound child which contains a
  // match, or a string with matches after cur (with a leading wildcard,
  // a compound child only when it fails on cur). When such a scan fails,
  // the scans from the next positions fail too (cur only grows), up to
  // that child: skip them, instead of scanning again (quadratic in the
  // length of the concat, and worse for nested concats). The matches
  // inside each child are searched only once.
  // With a path from inside a compound child (search_after), the hits of
  // that child are those which start at from or after it
  array<range_set> inside (N(t));
  for (int pos=0; pos<N(t); pos++)
    if (!is_nil (from) && is_compound (t[pos]) && (p * pos) <= from &&
        !is_nil ((from / (p * pos))->next))
      search_after (inside[pos], t[pos], what, p * pos, from);
    else search (inside[pos], t[pos], what, p * pos);
  array<bool> failed (N(t) * N(what));
  for (int k=0; k<N(failed); k++) failed[k]= false;
  bool scan= true;
  for (int pos=0; pos<N(t); ) {
    range_set ssel= copy (inside[pos]);
    if (is_atomic (t[pos]) && N(sel) != 0 &&
        (p * pos) <= sel[N(sel)-1]) {
      // a match across the children ended inside this string: its own hits
      // from there (inside[pos] comes from its start, where a hit across
      // that end could hide the next one, #482), with both matchings since
      // they are the same in the text (#495)
      ssel= range_set ();
      search_string (ssel, t[pos]->label, what, p * pos,
                     (sel[N(sel)-1] / (p * pos))->item);
    }
    if (N(ssel) != 0) {
      // would a failed scan have stopped here? (cur has not changed since
      // it failed: the hits merged in between start before cur)
      path cur= (p * 0) * start (t[0]);
      if (N(sel) != 0) cur= sel[N(sel)-1];
      bool stop= is_compound (t[pos])?
        !is_func (what[0], WILDCARD, 1):
        path_less_eq (cur, ssel[N(ssel)-2]);
      // ssel is a list of (start, end) pairs; a leading (trailing) wildcard
      // extends the hits to the start (end) of the formula. Then they
      // overlap, and merge keeps the first one (#454). Not out of a
      // document inside the child, as in a theorem or an item: the
      // extension stops at the nearest enclosing document (the maintainer's
      // decision on #501)
      if (is_func (what[0], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i+=2)
          if (!crosses_document (t, p, ssel[i]))
            ssel[i]= (p * 0) * start (t[0]);
      if (is_func (what[N(what)-1], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i+=2)
          if (!crosses_document (t, p, ssel[i+1]))
            ssel[i+1]= (p * (N(t)-1)) * end (t[N(t)-1]);
      merge (sel, ssel);
      if (stop) scan= true;
      if (is_compound (t[pos])) {
        pos++;
        continue;
      }
      // a match across the children may still start in this string, after
      // the hits inside it (#461): scan from here, after cur
    }
    if (!scan) { pos++; continue; }
    path p1, p2, cur= (p * 0) * start (t[0]);
    if (N(sel) != 0) cur= sel[N(sel)-1];
    if (search_concat (t, what, pos, 0, p, cur, p1, p2, inside, failed)) {
      merge (sel, simple_range (p1, p2));
      int next= (p2 / p)->item;
      pos= max (pos+1, next);
    }
    else {
      scan= false;
      pos++;
    }
  }
}

void
search_concat (range_set& sel, tree t, tree what, path p) {
  search_concat (sel, t, what, p, path ());
}

static void
search_after (range_set& sel, tree t, tree what, path p, path q) {
  // the hits of what in t (at p) which start at q (inside t) or after it
  if (path_less_eq (q, p * start (t))) {
    search (sel, t, what, p);
    return;
  }
  path r= q / p;
  if (is_atomic (t)) {
    search_string (sel, t->label, what, p, r->item);
    return;
  }
  if (is_nil (r->next)) return;  // after t
  if (is_concat (t) && is_concat (what)) {
    // as for the hits after a match which ended at q: q is the end of a
    // previous hit, which merge takes into account
    range_set ssel;
    ssel << q << q;
    search_concat (ssel, t, what, p, q);
    for (int i=2; i<N(ssel); i++) sel << ssel[i];
    return;
  }
  int k= r->item;
  if (k < 0 || k >= N(t)) return;
  if (is_accessible_for_search (t, k)) search_after (sel, t[k], what, p * k, q);
  for (int i=k+1; i<N(t); i++)
    if (is_accessible_for_search (t, i))
      search (sel, t[i], what, p * i);
}

static tree
anchored (tree t, bool at_start, bool at_end) {
  // a paragraph t, or a part of the pattern, as a concat with an anchor at
  // its start and/or at its end: a tag which documents do not use
  tree a (SEARCH_ANCHOR, "\1"), r (CONCAT);
  if (at_start) r << a;
  if (is_concat (t))
    for (int k=0; k<N(t); k++) r << t[k];
  else if (t != "") r << t;
  if (at_end) r << a;
  return r;
}

static path
unanchored (path q, path p, tree t, bool at_start) {
  // the path in t (at p) of a path q in anchored (t, at_start, ...) at p
  int k= (q / p)->item, off= (at_start? 1: 0);
  int n= (is_concat (t)? N(t): (t == ""? 0: 1));
  if (k < off) return p * start (t);
  if (k >= off + n) return p * end (t);
  path r= (q / p)->next;
  if (is_concat (t)) return p * path (k - off, r);
  return p * r;
}

static void
search_anchored (range_set& sel, tree t, tree what, path p,
                 bool at_start, bool at_end) {
  // the match of what in the paragraph t which starts at its start and/or
  // ends at its end. The hits of search are the shortest ones, which need
  // not reach the end of t (a*b in abxb: ab), so the ends are anchored in
  // both t and what, and the hit, which contains the anchors, is the match
  // (#495)
  range_set ssel;
  search (ssel, anchored (t, at_start, at_end),
          anchored (what, at_start, at_end), p);
  if (N(ssel) < 2) return;
  sel << unanchored (ssel[0], p, t, at_start)
      << unanchored (ssel[1], p, t, at_start);
}

static void
search_paragraph (range_set& sel, tree t, tree what, path p,
                  bool at_start, bool at_end) {
  // the match of a part what of a pattern of several paragraphs in the
  // paragraph t, which starts at its start and/or ends at its end: the
  // first part (at_end), the middle ones (both) and the last one
  // (at_start). The hits of search may do, also through a wildcard which
  // extends a hit into a tag of t, which the anchored search does not see;
  // else the anchored search finds the match
  if (what == "") {
    // an empty part: the empty end (start) of any paragraph, as when the
    // selection ends (starts) with a paragraph break; in the middle, an
    // empty paragraph. With both matchings (the blank flags only concern
    // the arguments of tags)
    if (!at_start) sel << (p * end (t)) << (p * end (t));
    else if (!at_end) sel << (p * start (t)) << (p * start (t));
    else if (t == "") sel << (p * 0) << (p * 0);
    return;
  }
  range_set ssel, asel;
  search_paragraph_tree (ssel, t, what, p);
  int n= N(ssel);
  if (at_start && at_end) {
    // one match covers the paragraph, not several hits
    if (n == 2 && ssel[0] == p * start (t) && ssel[1] == p * end (t))
      sel << ssel[0] << ssel[1];
    else search_anchored (sel, t, what, p, true, true);
  }
  else if (at_start) {
    // the first hit is the shortest match from the start
    if (n != 0 && ssel[0] == p * start (t)) sel << ssel[0] << ssel[1];
    else search_anchored (sel, t, what, p, true, false);
  }
  else {
    // the leftmost start: of the hit which ends the paragraph, or of the
    // anchored match, which may start before it (a*b in abab)
    search_anchored (asel, t, what, p, false, true);
    if (n != 0 && ssel[n-1] == p * end (t) &&
        (N(asel) == 0 || path_less_eq (ssel[n-2], asel[0])))
      sel << ssel[n-2] << ssel[n-1];
    else sel << asel;
  }
}

static void
search_paragraph_after (range_set& sel, tree t, tree what, path p, path q) {
  // idem for the first part, which starts at q (where the previous match
  // ended, inside t) or after it: in the rest of t, as a paragraph. A
  // position inside a tag of t skips the rest of that tag
  path r= q / p;
  if (is_lone_tag (t, what)) {
    range_set ssel;
    search_paragraph_after (ssel, tree (CONCAT, t), what, p, p * path (0, r));
    unwrap (sel, ssel, p);
    return;
  }
  if (is_atomic (t)) {
    int o= r->item;
    range_set ssel;
    search_paragraph (ssel, t->label (o, N(t->label)), what, p, false, true);
    for (int k=0; k<N(ssel); k++)
      sel << (p * ((ssel[k] / p)->item + o));
    return;
  }
  if (!is_concat (t) || is_nil (r)) return;
  int k= r->item, o= 0;
  bool cut= is_atomic (t[k]) && !is_nil (r->next);
  // q inside a tag of t: the matches which start there are carried out of
  // it by a trailing wildcard (they were missed, #501)
  bool in_tag= is_compound (t[k]) && !is_nil (r->next) &&
               !is_nil (r->next->next);
  tree u (CONCAT);
  if (cut) {
    o= r->next->item;
    if (o < N(t[k]->label)) u << tree (t[k]->label (o, N(t[k]->label)));
    else { cut= false; k++; }
  }
  else if (!(is_compound (t[k]) && r->next == path (0))) k++;
  for (int c= (cut? k+1: k); c<N(t); c++) u << t[c];
  range_set ssel, usel;
  if (N(u) != 0) search_paragraph (ssel, u, what, p, false, true);
  for (int h=0; h<N(ssel); h++) {
    path x= ssel[h] / p;
    int c= x->item;
    if (cut && c == 0) usel << (p * path (k, path (x->next->item + o)));
    else usel << (p * path (c + k, x->next));
  }
  if (in_tag) {
    range_set hsel;
    search_after (hsel, t, what, p, q);
    int n= N(hsel);
    if (n != 0 && hsel[n-1] == p * end (t) &&
        (N(usel) == 0 || path_less_eq (hsel[n-2], usel[0]))) {
      sel << hsel[n-2] << hsel[n-1];
      return;
    }
  }
  sel << usel;
}

void
search_document (range_set& sel, tree t, tree what, path p) {
  // a pattern of several paragraphs: the first one ends a paragraph, the
  // middle ones cover theirs, and the last one starts its paragraph
  if (N(what) == 0) return;
  if (N(what) == 1) {
    // one paragraph: as this paragraph (what[1] was read)
    search (sel, t, what[0], p);
    return;
  }
  for (int i=0; i<N(t); i++) {
    bool found= false;
    range_set fsel;
    search_paragraph (fsel, t[i], what[0], p * i, false, true);
    bool lead= is_func (what[0], WILDCARD, 1) ||
               (is_concat (what[0]) && N(what[0]) > 0 &&
                is_func (what[0][0], WILDCARD, 1));
    if (N(fsel) != 0 && N(sel) != 0 && (p * i) <= sel[N(sel)-1] &&
        !path_less_eq (sel[N(sel)-1], fsel[0]) && !lead) {
      // the previous match ended in this paragraph, after this start: a
      // start after the end of that match (with a leading wildcard, the
      // part starts at the start of the paragraph)
      fsel= range_set ();
      search_paragraph_after (fsel, t[i], what[0], p * i, sel[N(sel)-1]);
    }
    if (N(fsel) != 0)
      if (N(sel) == 0 || path_less_eq (sel[N(sel)-1], fsel[0])) {
        int cur= 1;
        for (int j=i+1; j<N(t); j++) {
          if (cur == N(what) - 1) {
            range_set lsel;
            search_paragraph (lsel, t[j], what[cur], p * j, true, false);
            if (N(lsel) != 0) {
              // first the hits inside this paragraph before the match (in
              // a document inside a tag of it: an empty first part matches
              // every paragraph)
              range_set nsel, bsel;
              search (nsel, t[i], what, p * i);
              for (int h=0; h+1<N(nsel); h+=2)
                if (path_less_eq (nsel[h+1], fsel[0]))
                  bsel << nsel[h] << nsel[h+1];
              merge (sel, bsel);
              merge (sel, simple_range (fsel[0], lsel[1]));
              found= true;
              i= j-1;
            }
            break;
          }
          else {
            range_set msel;
            search_paragraph (msel, t[j], what[cur], p * j, true, true);
            if (N(msel) == 0) break;
            cur++;
            if (cur >= N(what)) break;
          }
        }
      }
    if (!found) search (sel, t[i], what, p * i);
  }
}

static bool
is_anchored (tree t) {
  // a pattern, or a paragraph, with an anchor at its start or at its end
  return is_concat (t) && N(t) > 0 &&
         (is_func (t[0], SEARCH_ANCHOR) || is_func (t[N(t)-1], SEARCH_ANCHOR));
}

void
search (range_set& sel, tree t, tree what, path p) {
  if (N(sel) > search_max_hits) return;
  // an anchored pattern is found only in the anchored paragraph, not inside
  // its children, which have no anchor: do not search them (match_formula
  // makes such a search at each tag, at each level of the cascade)
  if (is_anchored (what) && !is_anchored (t)) return;
  if (is_atomic (t))
    search_string (sel, t->label, what, p);
  else if (is_func (t, CONCAT) && is_func (what, CONCAT))
    search_concat (sel, t, what, p);
  else if (is_func (t, DOCUMENT) && is_func (what, DOCUMENT))
    search_document (sel, t, what, p);
  else
    search_compound (sel, t, what, p);
}

void
search (range_set& sel, tree t, tree what, path p, path pos) {
  if (is_document (what) || is_atomic (t))
    search (sel, t, what, p);
  else if (is_concat (what) && is_concat (t))
    search (sel, t, what, p);
  else {
    if (is_nil (pos)) search (sel, t, what, p);
    else if (!(is_anchored (what) && !is_anchored (t)) &&
             (match (t, what) || matches_between_wildcards (t, what)))
      // the cursor is inside a tag which matches: that tag is a hit, as in
      // search_compound, so that a search from the cursor selects the
      // enclosing match, as it does in a text (#523)
      merge (sel, simple_range (p * start (t), p * end (t)));
    else {
      int hits= 0;
      array<range_set> sub (N(t));
      if (pos->item >= 0 && pos->item < N(t))
        if (is_accessible_for_search (t, pos->item)) {
          if (is_document (t) && is_lone_tag (t[pos->item], what))
            search_paragraph_tree (sub[pos->item], t[pos->item], what,
                                   p * pos->item);
          else
            search (sub[pos->item], t[pos->item], what, p * pos->item,
                    pos->next);
          hits += N(sub[pos->item]);
        }
      for (int d=1; d<N(t); d++)
        for (int e=0; e<=1; e++) {
          if (hits > search_max_hits) break;
          int i= (e==0? pos->item + d: pos->item - d);
          if (i >= 0 && i < N(t))
            if (is_accessible_for_search (t, i)) {
              if (is_document (t))
                search_paragraph_tree (sub[i], t[i], what, p * i);
              else search (sub[i], t[i], what, p * i);
              hits += N(sub[i]);
            }
        }
      for (int i=0; i<N(t); i++) sel << sub[i];
    }
  }
}

/******************************************************************************
* Selections inside specified contexts
******************************************************************************/

bool
contains_select_region (tree t) {
  if (is_atomic (t)) return false;
  else if (is_func (t, SELECT_REGION, 1)) return true;
  else {
    for (int i=0; i<N(t); i++)
      if (contains_select_region (t[i]))
        return true;
    return false;
  }
}

static bool select_in_argument= false;

static tree without_empty_strings (tree what, bool join);

static tree
without_regions (tree what) {
  // the pattern which a select-region pattern matches: each region is its
  // content, spliced into a formula; an empty region, which selects a
  // whole argument, matches any content, as a wildcard
  if (is_atomic (what)) return what;
  if (is_func (what, SELECT_REGION, 1)) {
    if (is_empty (what[0])) return tree (WILDCARD, "");
    return without_regions (what[0]);
  }
  tree r (L(what));
  for (int i=0; i<N(what); i++) {
    tree w= without_regions (what[i]);
    if (is_concat (what) && is_concat (w))
      for (int k=0; k<N(w); k++) r << w[k];
    else r << w;
  }
  return r;
}

// the text of a hit, as a sequence of tokens: the characters of its strings
// and its tags, each with the positions before and after it. Where the hit
// goes out of a tag (a leading or trailing wildcard carries it to the start
// or end of the formula), a boundary, which only a wildcard can take
#define TOKEN_CHAR 0
#define TOKEN_TAG 1
#define TOKEN_BOUNDARY 2

struct search_token {
  int kind;
  tree t;  // the character, or the tag
  path at;  // the tag
  path before, after;
};

static void
tokens_between (array<search_token>& toks, tree u, path q, path a, path b) {
  // the tokens of u (at q) from a to b (relative to q; nil: from the start,
  // to the end)
  if (is_atomic (u)) {
    string s= u->label;
    int o= (is_nil (a)? 0: a->item), e= (is_nil (b)? N(s): b->item);
    while (o < e) {
      int o2= o;
      tm_char_forwards (s, o2);
      search_token tok;
      tok.kind= TOKEN_CHAR;
      tok.t= tree (case_fold (s (o, o2)));
      tok.before= q * o;
      tok.after= q * o2;
      toks << tok;
      o= o2;
    }
    return;
  }
  bool an= !is_nil (a) && !is_nil (a->next);
  bool bn= !is_nil (b) && !is_nil (b->next);
  if (is_concat (u)) {
    int k1= (is_nil (a)? 0: a->item), k2= (is_nil (b)? N(u)-1: b->item);
    for (int k=k1; k<=k2 && k<N(u); k++) {
      path sa= (k == k1 && !is_nil (a))? a->next: path ();
      path sb= (k == k2 && !is_nil (b))? b->next: path ();
      if (is_atomic (u[k])) tokens_between (toks, u[k], q * k, sa, sb);
      else {
        if (sa == path (1) || sb == path (0)) continue;
        tokens_between (toks, u[k], q * k, sa, sb);
      }
    }
    return;
  }
  if (!an && !bn) {
    if (a == path (1) || b == path (0)) return;
    search_token tok;
    tok.kind= TOKEN_TAG;
    tok.t= u;
    tok.at= q;
    tok.before= q * 0;
    tok.after= q * 1;
    toks << tok;
    return;
  }
  if (an && bn && a->item == b->item) {
    tokens_between (toks, u[a->item], q * a->item, a->next, b->next);
    return;
  }
  if (an) tokens_between (toks, u[a->item], q * a->item, a->next, path ());
  search_token tok;
  tok.kind= TOKEN_BOUNDARY;
  tok.before= tok.after= (an? q * path (a->item, end (u[a->item])): q * 0);
  toks << tok;
  if (bn) tokens_between (toks, u[b->item], q * b->item, path (), b->next);
}

// the elements of a pattern, for the tokens: characters, wildcards, tags,
// and the starts and ends of the regions
#define ELEM_CHAR 0
#define ELEM_WILDCARD 1
#define ELEM_TAG 2
#define ELEM_START 3
#define ELEM_END 4
#define ELEM_ONE 5

static void
pattern_elements (array<int>& kind, array<tree>& elem, tree what) {
  if (is_concat (what)) {
    for (int i=0; i<N(what); i++) pattern_elements (kind, elem, what[i]);
  }
  else if (is_atomic (what)) {
    string s= case_fold (what->label);
    for (int o=0; o<N(s); ) {
      int o2= o;
      tm_char_forwards (s, o2);
      kind << ELEM_CHAR; elem << tree (s (o, o2));
      o= o2;
    }
  }
  else if (is_func (what, WILDCARD, 1)) {
    kind << ELEM_WILDCARD; elem << what;
  }
  else if (is_func (what, SELECT_REGION, 1)) {
    // an empty region in a formula: exactly one character or one tag (the
    // maintainer's decision on #515; in a tag argument, it is the argument)
    kind << ELEM_START; elem << what;
    if (is_empty (what[0])) { kind << ELEM_ONE; elem << what; }
    else pattern_elements (kind, elem, what[0]);
    kind << ELEM_END; elem << what;
  }
  else { kind << ELEM_TAG; elem << what; }
}

static void
pattern_elements (array<int>& kind, array<tree>& elem, array<int>& child,
                  tree what) {
  // the elements of what, and the child of the formula what (0 for
  // another pattern) to which each belongs
  if (!is_concat (what)) {
    pattern_elements (kind, elem, what);
    while (N(child) < N(kind)) child << 0;
    return;
  }
  for (int i=0; i<N(what); i++) {
    pattern_elements (kind, elem, what[i]);
    while (N(child) < N(kind)) child << i;
  }
}

static bool
has_one (tree what) {
  // does the formula pattern what have an empty region at its level?
  array<int> kind;
  array<tree> elem;
  pattern_elements (kind, elem, what);
  for (int k=0; k<N(kind); k++)
    if (kind[k] == ELEM_ONE) return true;
  return false;
}

static void select_direct (range_set& sel, tree t, tree what, path p);

struct region_matcher {
  array<search_token> toks;
  array<int> kind;
  array<tree> elem;
  bool to_end;          // the match ends at the end of the tokens
  array<bool> failed;   // (element, token) from which no match exists
  array<int> wfail;     // a wildcard fails from this token on
  array<path> mark;     // the position of each start or end of a region
  array<int> at;        // the token of each start of a region
  array<int> from;      // the token of each element, in the match
  array<int> child;     // the child of a formula pattern of each element
  array<range_set> in;  // the regions inside each tag of the pattern
  int end;              // the token after the match

  void init (tree what, bool to_end2) {
    // (again on the same tokens: a new match, with the pattern what)
    kind= array<int> ();
    elem= array<tree> ();
    child= array<int> ();
    pattern_elements (kind, elem, child, what);
    to_end= to_end2;
    int nt= N(toks) + 1;
    failed= array<bool> (N(kind) * nt + nt);
    for (int k=0; k<N(failed); k++) failed[k]= false;
    wfail= array<int> (N(kind));
    for (int k=0; k<N(kind); k++) wfail[k]= nt;
    mark= array<path> (N(kind));
    at= array<int> (N(kind));
    from= array<int> (N(kind));
    in= array<range_set> (N(kind));
    end= 0;
  }

  path position (int ti, bool before) {
    // the position between the tokens ti-1 and ti
    if (before && ti < N(toks)) return toks[ti].before;
    if (ti > 0) return toks[ti-1].after;
    return toks[0].before;
  }

  bool run (int ei, int ti) {
    // the pattern from its element ei matches the tokens from ti on (to the
    // end if to_end); a wildcard takes as few tokens as it can (lazy). A
    // wildcard which fails from ti fails from any later token: it is not
    // tried again, so that each wildcard scans the tokens once
    int nt= N(toks) + 1;
    if (ei == N(kind)) {
      if (to_end && ti != N(toks)) return false;
      end= ti;
      return true;
    }
    if (failed[ei * nt + ti]) return false;
    bool ok= false;
    switch (kind[ei]) {
    case ELEM_START:
      if (run (ei+1, ti)) {
        mark[ei]= position (ti, true); at[ei]= ti; ok= true;
      }
      break;
    case ELEM_END:
      if (run (ei+1, ti)) { mark[ei]= position (ti, false); ok= true; }
      break;
    case ELEM_WILDCARD:
      if (ti >= wfail[ei]) break;
      for (int k=ti; k<=N(toks) && !ok; k++)
        if (run (ei+1, k)) ok= true;
      if (!ok) wfail[ei]= ti;
      break;
    case ELEM_ONE:
      ok= ti < N(toks) && toks[ti].kind != TOKEN_BOUNDARY &&
          run (ei+1, ti+1);
      break;
    case ELEM_CHAR:
      ok= ti < N(toks) && toks[ti].kind == TOKEN_CHAR &&
          toks[ti].t == elem[ei] && run (ei+1, ti+1);
      break;
    case ELEM_TAG:
      if (ti < N(toks) && toks[ti].kind == TOKEN_TAG) {
        tree t= toks[ti].t, w= elem[ei];
        if (!contains_select_region (w))
          ok= match (t, w) && run (ei+1, ti+1);
        else if (L(t) == L(w)) {
          range_set ssel;
          select_direct (ssel, t, w, toks[ti].at);
          if (N(ssel) != 0 && run (ei+1, ti+1)) {
            in[ei]= ssel; at[ei]= ti; ok= true;
          }
        }
      }
      break;
    }
    if (ok) from[ei]= ti;
    else failed[ei * nt + ti]= true;
    return ok;
  }

  int first_region () {
    // the token where the first region starts, after a match: a region of
    // the pattern, or one inside a tag of the pattern ((strong [a]))
    for (int k=0; k<N(kind); k++)
      if (kind[k] == ELEM_START || (kind[k] == ELEM_TAG && N(in[k]) != 0))
        return at[k];
    return N(toks);
  }

  void regions (range_set& sel) {
    // the regions of the match
    for (int k=0; k<N(kind); k++)
      if (kind[k] == ELEM_START) {
        int e= k+1, depth= 1;
        while (e < N(kind)) {
          if (kind[e] == ELEM_START) depth++;
          if (kind[e] == ELEM_END && --depth == 0) break;
          e++;
        }
        if (e >= N(kind)) continue;
        path r1= mark[k], r2= mark[e];
        if (path_less (r2, r1)) r2= r1;  // an empty region
        sel << r1 << r2;
      }
      else if (kind[k] == ELEM_TAG && N(in[k]) != 0)
        sel << in[k];
  }
};

static void select_hits (range_set& sel, tree t, tree what, path p,
                         bool first);
static bool select_in_hit (range_set& sel, tree u, path q, path a, path b,
                           tree what, bool no_carry= false);
static path hit_container (path h1, path h2);

static bool
select_in_formula (range_set& sel, tree u, path g, tree what) {
  // the regions of what in the formula (or string, or tag alone) u at g,
  // as when u is alone: what matches all of u, lazily, its outer wildcards
  // taking the rest, unless a hit inside a tag of u before its first region
  // comes first (the leftmost region); if it does not match, the first hit
  // inside the tags of u. Each formula is matched once: linear, not a
  // search at each level of nested tags
  if (is_atomic (u) && N(u->label) == 0) return false;
  if (is_document (u)) {
    for (int i=0; i<N(u); i++)
      if (select_in_formula (sel, u[i], g * i, what)) return true;
    return false;
  }
  region_matcher f;
  tokens_between (f.toks, u, g, path (), path ());
  if (N(f.toks) == 0) return false;
  f.init (what, true);
  bool ok= f.run (0, 0);
  int upto= ok? f.first_region (): N(f.toks);
  for (int k=0; k<upto && k<N(f.toks); k++)
    if (f.toks[k].kind == TOKEN_TAG) {
      tree t= f.toks[k].t;
      path p= f.toks[k].at;
      for (int j=0; j<N(t); j++)
        if (is_accessible_for_search (t, j))
          if (select_in_formula (sel, t[j], p * j, what)) return true;
    }
  if (!ok) return false;
  f.regions (sel);
  return true;
}

static bool
select_inside_tags (range_set& sel, region_matcher& m, int upto, tree what) {
  // the regions of a hit inside the tags m.toks[0..upto[, which a leading
  // and a trailing wildcard carry to the start and the end of the formula:
  // the regions which the pattern gives in the argument, as when it is
  // alone (the first such hit)
  bool arg= select_in_argument;
  select_in_argument= false;
  bool r= false;
  for (int k=0; k<upto && k<N(m.toks) && !r; k++)
    if (m.toks[k].kind == TOKEN_TAG) {
      tree t= m.toks[k].t;
      path p= m.toks[k].at;
      for (int j=0; j<N(t) && !r; j++)
        if (is_accessible_for_search (t, j))
          r= select_in_formula (sel, t[j], p * j, what);
    }
  select_in_argument= arg;
  return r;
}

static bool
select_in_hit (range_set& sel, tree u, path q, path a, path b, tree what,
               bool no_carry) {
  // the regions of what in the hit of its text from a to b, in u at q (not
  // carried out of a tag of it if no_carry)
  region_matcher m;
  tokens_between (m.toks, u, q, a, b);
  m.init (what, true);
  if (N(m.toks) == 0) {
    // an empty hit: the regions are empty, at its position, if the pattern
    // has nothing else than wildcards
    path pos= q * (is_nil (a)? start (u): a);
    for (int k=0; k<N(m.kind); k++)
      if (m.kind[k] != ELEM_START && m.kind[k] != ELEM_END &&
          m.kind[k] != ELEM_WILDCARD) return false;
    for (int k=0; k<N(m.kind); k++)
      if (m.kind[k] == ELEM_START) sel << pos << pos;
    return true;
  }
  // a leading and a trailing wildcard, also inside a region at an end of
  // the pattern ([*a]*, *[c*])
  int e1= 0, e2= N(m.kind) - 1;
  while (e1 < N(m.kind) && (m.kind[e1] == ELEM_START || m.kind[e1] == ELEM_END))
    e1++;
  while (e2 >= 0 && (m.kind[e2] == ELEM_START || m.kind[e2] == ELEM_END))
    e2--;
  bool carried= !no_carry && e1 <= e2 && m.kind[e1] == ELEM_WILDCARD &&
                m.kind[e2] == ELEM_WILDCARD;
  if (!m.run (0, 0))
    return carried && select_inside_tags (sel, m, N(m.toks), what);
  // the leftmost region: a hit inside an earlier tag, carried to the ends
  // of the formula, comes first
  if (carried && select_inside_tags (sel, m, m.first_region (), what))
    return true;
  m.regions (sel);
  return true;
}

static range_set
merge_sorted (range_set a, range_set b) {
  // the ranges of a and b, each in the order of their starts, in that order
  range_set r;
  int i=0, j=0;
  while (i+1 < N(a) || j+1 < N(b))
    if (j+1 >= N(b) || (i+1 < N(a) && path_less_eq (a[i], b[j]))) {
      r << a[i] << a[i+1]; i += 2;
    }
    else { r << b[j] << b[j+1]; j += 2; }
  return r;
}

static void
inside_tags (array<tree>& c, array<path>& cp, tree t, path p) {
  // the arguments of the tags of the formula t, or the children of t
  if (is_concat (t)) {
    for (int k=0; k<N(t); k++)
      if (is_compound (t[k]))
        for (int i=0; i<N(t[k]); i++)
          if (is_accessible_for_search (t[k], i)) {
            c << t[k][i]; cp << p * path (k, i);
          }
  }
  else if (is_compound (t))
    for (int i=0; i<N(t); i++)
      if (is_accessible_for_search (t, i)) { c << t[i]; cp << p * i; }
}

static void
select_tokens (range_set& sel, tree t, tree what, path p) {
  // a formula pattern with an empty region, which takes exactly one token,
  // which the search has no element for: the pattern is matched with the
  // tokens of each formula and string, leftmost and lazy, and then inside
  // the tags of the formula, as when searching. In a tag argument with
  // Replace, it covers the whole argument (or its start), as any pattern,
  // or is inside the tags of the argument (a cascaded match)
  range_set r;
  bool arg= select_in_argument && !partial_match_flag;
  region_matcher m;
  array<path> covered;  // the tags inside the hits of the formula
  // a tag alone (a paragraph or an argument) is a formula of one token, as
  // in the search; a document is not
  if (arg || !is_document (t)) {
    tokens_between (m.toks, t, p, path (), path ());
    m.init (what, arg && !initial_match_flag);
    if (arg) {
      if (N(m.toks) != 0 && m.run (0, 0)) { m.regions (sel); return; }
      if (!cascaded_match_flag) return;
    }
    else
      for (int s=0; s<N(m.toks) && N(r) <= search_max_hits; ) {
        if (m.run (0, s)) {
          range_set ssel;
          m.regions (ssel);
          merge (r, ssel);
          for (int k=s; k<m.end; k++)
            if (m.toks[k].kind == TOKEN_TAG) covered << m.toks[k].at;
          s= max (s+1, m.end);
        }
        else s++;
      }
  }
  if (is_compound (t)) {
    // inside the tags of the formula (or of t): in the order of the
    // document, then merged with the hits of the formula itself
    array<tree> c;
    array<path> cp;
    inside_tags (c, cp, t, p);
    range_set in;
    int ci= 0;
    for (int i=0; i<N(c) && N(in) <= search_max_hits; i++) {
      // not inside a tag which a hit of the formula covers
      path tag= is_concat (t)? path_up (cp[i]): p;
      while (ci < N(covered) && path_less (covered[ci], tag)) ci++;
      if (ci < N(covered) && covered[ci] == tag) continue;
      select_tokens (in, c[i], what, cp[i]);
    }
    r= merge_sorted (r, in);
  }
  merge (sel, r);
}

static path
hit_container (path h1, path h2) {
  // the tree in which a hit lies: the common part of its ends, which both
  // go on after it
  path c= common (h1, h2);
  while (N(c) >= min (N(h1), N(h2))) c= path_up (c);
  return c;
}

static void
select_hits (range_set& sel, tree t, tree what, path p, bool first) {
  // a formula pattern with regions: its text, the regions being their
  // content, is matched as any pattern (#495: in the text, a match may start
  // and end inside a string, and wildcards take the shortest runs, the same
  // when searching and replacing; in a tag argument, the flags say where).
  // It was matched child by child, with gaps (#500). The regions are then
  // found in each hit (in the first one only, if first)
  tree w= without_empty_strings (without_regions (what), true);
  range_set hits;
  if (select_in_argument && !partial_match_flag) {
    search_anchored (hits, t, w, p, true, !initial_match_flag);
    if (N(hits) == 0 && cascaded_match_flag) {
      // inside the tags of the argument, as a cascaded match
      array<tree> c;
      array<path> cp;
      inside_tags (c, cp, t, p);
      for (int i=0; i<N(c); i++) {
        range_set ssel;
        select_hits (ssel, c[i], what, cp[i], first);
        merge (sel, ssel);
        if (first && N(sel) != 0) return;
      }
      return;
    }
  }
  else search (hits, t, w, p);
  for (int i=0; i+1<N(hits); i+=2) {
    path c= hit_container (hits[i], hits[i+1]);
    if (!(p <= c)) continue;
    range_set ssel;
    if (!select_in_hit (ssel, subtree (t, c / p), c, hits[i] / c,
                        hits[i+1] / c, what)) continue;
    merge (sel, ssel);
    if (first) return;
  }
}

/******************************************************************************
* Aligning a pattern with a hit, for Replace (#520)
******************************************************************************/

static bool
is_content_child (tree u, int j) {
  // an argument of u which holds content: one which the search reaches, or
  // a hidden one of regular content (the body of a folded), not attributes
  // (the variable and the value of a with)
  if (is_accessible_for_search (u, j)) return true;
  if (u[j] == "" || (is_func (u, WITH) && j < N(u) - 1)) return false;
  int type= the_drd->get_type_child (u, j);
  return type == TYPE_REGULAR || type == TYPE_ADHOC || type == TYPE_UNKNOWN;
}

static bool
is_position (tree t, path p) {
  // is p a position in t (nil: anywhere)?
  if (is_nil (p)) return true;
  if (is_atomic (t)) return is_nil (p->next) && p->item >= 0 &&
                            p->item <= N(t->label);
  if (is_nil (p->next)) return p->item == 0 || p->item == 1;
  return p->item >= 0 && p->item < N(t) && is_position (t[p->item], p->next);
}

static path
argument_start (tree t, path a) {
  tree u= subtree (t, a);
  if (is_concat (u) && N(u) > 0) return a * path (0, 0);
  return a * 0;
}

static path
argument_of (tree t, path p) {
  // the argument (of a tag, or the root) in which the position p of t lies
  path q= path_up (p);
  while (!is_nil (q) && is_concat (subtree (t, path_up (q)))) q= path_up (q);
  return q;
}

static bool
match_whole (region_matcher& m, tree what) {
  // the pattern what with all the tokens of m
  m.init (what, true);
  return m.run (0, 0);
}

static bool
match_leftmost (region_matcher& m, tree what, bool to_end, bool any_start,
                int& start) {
  // the leftmost match of what in the tokens of m, at their start only
  // unless any_start: wildcards take as few tokens as they can, and a
  // trailing one the rest of them if to_end; start: its first token
  m.init (what, to_end);
  for (start=0; start<=N(m.toks); start++) {
    if (m.run (0, start)) return true;
    if (!any_start) break;
  }
  return false;
}

static bool
match_as_search (region_matcher& m, tree what, bool str, int& start) {
  // the pattern what with the tokens m of a hit, as the search matches it:
  // all of them, else, with partial (initial) matches, the leftmost match
  // (at the start), whose trailing wildcard takes the rest, as the search
  // carries an outer wildcard to the end of the formula; in a string (str)
  // argument of a tag pattern, with partial and without initial matches,
  // it ends at the end of the string, as match_atomic
  start= 0;
  if (match_whole (m, what)) return true;
  if (!(partial_match_flag || initial_match_flag) || N(m.kind) == 0)
    return false;
  bool to_end= m.kind[N(m.kind)-1] == ELEM_WILDCARD ||
               (str && partial_match_flag && !initial_match_flag);
  return match_leftmost (m, what, to_end, partial_match_flag, start);
}

static bool
match_alone (region_matcher& m, tree what, bool lazy, int& start) {
  // the pattern what in the tokens m of an argument, as when it is alone:
  // the leftmost match, a trailing wildcard taking the rest (if lazy: not
  // all of the argument first, as the search on it: a1xa2x with a*x gives
  // a1x; else all of it first, as a cascaded match in an argument)
  start= 0;
  if (!lazy && match_whole (m, what)) return true;
  m.init (what, true);
  if (N(m.kind) == 0) return false;
  bool to_end= m.kind[N(m.kind)-1] == ELEM_WILDCARD;
  return match_leftmost (m, what, to_end, true, start);
}

static bool
before_position (path x, path ref) {
  // does the position or path x come before the position ref: before it
  // in the tree, or around it (a tag which holds ref)?
  return path_less (x, ref) || (x <= ref && x != ref);
}

// the match which Replace replaces in a hit: its range, and its tokens
// with the elements of the pattern aligned with them, for the values of the
// wildcards (#520)
struct replace_match {
  path q1, q2;         // the range
  region_matcher m;    // the tokens of the match, from the position base
  path base;
  int start;           // the first token of the match
  bool in_arg;         // inside the argument arg (not the hit itself)
  path arg;
};

static bool
argument_match (tree t, path arg, tree what, bool lazy, path from,
                replace_match& r) {
  // the match of what in the argument arg of t, as when it is alone (its
  // tokens relative to it: short paths in deep nestings), not before the
  // position from (the start of the hit: the text before it, the output of
  // an earlier replacement, is not in it)
  tree u= subtree (t, arg);
  array<search_token> all;
  tokens_between (all, u, path (), path (), path ());
  bool cut= !is_nil (from) && arg <= from && arg != from;
  path rel= (cut? from / arg: path ());
  region_matcher m;
  for (int k=0; k<N(all); k++)
    if (!cut || !(before_position (all[k].before, rel) ||
                  (all[k].kind == TOKEN_TAG && all[k].at <= rel)))
      m.toks << all[k];
  int start;
  if (!match_alone (m, what, lazy, start)) return false;
  path a= argument_start (t, arg);
  r.q1= (N(m.toks) == 0? a: arg * m.position (start, true));
  r.q2= (N(m.toks) == 0? a: arg * m.position (m.end, false));
  r.m= m; r.base= arg; r.start= start; r.in_arg= true; r.arg= arg;
  return true;
}

static bool
first_argument_match (tree t, path at, tree what, bool lazy, path from,
                      replace_match& r) {
  // the first accessible argument of the tag of t at at, or of the tags
  // inside them, in the order of the document, where what matches (as when
  // it is alone): each argument once, and no further than the first match
  tree u= subtree (t, at);
  if (is_atomic (u)) return false;
  if (is_concat (u) || is_document (u)) {
    for (int k=0; k<N(u); k++)
      if (is_compound (u[k]) &&
          first_argument_match (t, at * k, what, lazy, from, r))
        return true;
    return false;
  }
  for (int j=0; j<N(u); j++)
    if (is_accessible_for_search (u, j)) {
      if (argument_match (t, at * j, what, lazy, from, r)) return true;
      if (is_compound (u[j]) &&
          first_argument_match (t, at * j, what, lazy, from, r))
        return true;
    }
  return false;
}

static bool
replace_range (tree t, path p1, path p2, tree what, replace_match& r) {
  // the match which Replace replaces in the hit from p1 to p2 in t (nil:
  // all of t). If the hit goes out of a tag or into it, or the pattern
  // matches inside a tag of the hit (a lone tag, a cascaded match), the
  // match inside the argument which holds its literal part, as when the
  // argument is alone (the maintainer's decision: the regions found on
  // b (strong F) c are those on F alone), the leftmost one there; the text
  // around it stays. False if the literal part of the match crosses the
  // border of a tag: no replacement keeps the text around it there
  region_matcher m;
  tokens_between (m.toks, t, path (), p1, p2);
  r.q1= p1; r.q2= p2; r.base= path (); r.in_arg= false;
  bool str= is_atomic (t) && is_nil (p1) && is_nil (p2);
  // (the argument of a hit is searched as the search on it, the leftmost
  // match; a whole tree, a cascaded match in an argument of a tag pattern,
  // takes all of the argument first, as the search matched it)
  bool lazy= !is_nil (p1) || !is_nil (p2);
  if (match_as_search (m, what, str, r.start)) {
    r.m= m;
    bool border= false;
    for (int k=0; k<N(m.toks); k++)
      if (m.toks[k].kind == TOKEN_BOUNDARY) border= true;
    if (!border) return true;
    // the literal part: from the first element which is not a wildcard to
    // the last one
    int e1= 0, e2= N(m.kind) - 1;
    while (e1 <= e2 && m.kind[e1] == ELEM_WILDCARD) e1++;
    while (e2 >= e1 && m.kind[e2] == ELEM_WILDCARD) e2--;
    if (e1 > e2) return false;
    int c1= m.from[e1], c2= (e2+1 < N(m.kind)? m.from[e2+1]: m.end);
    for (int k=c1; k<c2; k++)
      if (m.toks[k].kind == TOKEN_BOUNDARY) return false;
    if (c1 >= N(m.toks)) return false;
    return argument_match (t, argument_of (t, m.toks[c1].before), what,
                           lazy, p1, r);
  }
  // inside a tag of the hit: the first argument where it matches
  for (int k=0; k<N(m.toks); k++)
    if (m.toks[k].kind == TOKEN_TAG &&
        first_argument_match (t, m.toks[k].at, what, lazy, p1, r))
      return true;
  return false;
}

struct search_scope {
  // the flags of the search, and its cache of formulas, for one call
  search_scope () { initialize_search (); clear_no_formula (); }
  ~search_scope () { clear_no_formula (); }
};

static bool
wildcard_runs (tree t, tree what, replace_match& r, bool whole,
               array<tree>& runs) {
  // the run of each child of the formula pattern what in the match r of t
  // (a match may start and end inside a string, #495): Replace takes the
  // values of the wildcards from them; they were matched child by child,
  // which lost a wildcard which takes several children, or a part of a
  // string (#520). If whole (the match is replaced as a whole: a cascaded
  // match in an argument of a tag pattern) and it is inside an argument of
  // a tag of t, the values are in the tags around that argument; false if
  // the match does not take all of the argument, or these tags have other
  // content, which would be lost
  region_matcher& m= r.m;
  array<path> tags;
  if (whole && r.in_arg) {
    array<search_token> before, after;
    tokens_between (before, t, path (), path (), r.q1);
    tokens_between (after, t, path (), r.q2, path ());
    for (int k=0; k<N(before); k++)
      if (before[k].kind != TOKEN_BOUNDARY) return false;
    for (int k=0; k<N(after); k++)
      if (after[k].kind != TOKEN_BOUNDARY) return false;
    for (path q= r.arg; !is_nil (q); q= path_up (q)) {
      tree u= subtree (t, path_up (q));
      if (is_concat (u)) continue;
      for (int j=0; j<N(u); j++)
        if (j != last_item (q) && is_content_child (u, j)) return false;
      tags << path_up (q);
    }
  }
  // the first element of each child of the pattern, and the end
  array<int> first (N(what) + 1);
  for (int i=0, k=0; i<=N(what); i++) {
    while (k < N(m.child) && m.child[k] < i) k++;
    first[i]= k;
  }
  int ne= N(m.kind);
  for (int i=0; i<N(what); i++) {
    int s= (i == 0? r.start: first[i] < ne? m.from[first[i]]: m.end);
    int e= (i == N(what)-1 || first[i+1] >= ne? m.end: m.from[first[i+1]]);
    tree x= (e <= s? tree (""):
             simplify_correct (selection_compute (t,
                                 r.base * m.toks[s].before,
                                 r.base * m.toks[e-1].after)));
    if (is_func (what[i], WILDCARD, 1) && x != "")
      for (int k=0; k<N(tags); k++) {
        // (in a copy of the tag, which shares the other arguments)
        tree u= subtree (t, tags[k]);
        tree c (L(u), N(u));
        for (int j=0; j<N(u); j++) c[j]= u[j];
        c[(r.arg / tags[k])->item]= x;
        x= c;
      }
    runs << x;
  }
  return true;
}

static tree
path_as_tree (path p) {
  tree r (TUPLE);
  for (; !is_nil (p); p= p->next) r << as_string (p->item);
  return r;
}

array<tree>
search_replace_match (tree t, tree what, path p1, path p2) {
  // the match which Replace replaces in the hit from p1 to p2 in t (nil:
  // all of t, a match replaced as a whole): its start and its end (as
  // tuples of the items of their paths), the match itself, exactly (the
  // selection of the editor moves the ends of a range next to a script
  // over its base), and the value of each child of a formula pattern (see
  // wildcard_runs; none for another pattern); nil if Replace leaves the hit
  array<tree> r;
  if (!is_position (t, p1) || !is_position (t, p2) || path_less (p2, p1))
    return r;
  search_scope scope;
  replace_match rm;
  array<tree> runs;
  if (is_concat (what) && N(what) != 0 && !contains_select_region (what)) {
    if (!replace_range (t, p1, p2, what, rm) ||
        !wildcard_runs (t, what, rm, is_nil (p1) && is_nil (p2), runs))
      return r;
  }
  else {
    // a string or a tag pattern: the hit must be a match of it (a range
    // which is not one is never replaced)
    rm.q1= p1; rm.q2= p2;
    if (!is_document (what) && !contains_select_region (what) &&
        !match (simplify_correct (selection_compute (t, p1, p2)), what))
      return r;
  }
  bool all= is_nil (rm.q1) && is_nil (rm.q2);
  r << path_as_tree (rm.q1) << path_as_tree (rm.q2)
    << (all? t: simplify_correct (selection_compute (t, rm.q1, rm.q2)));
  r << runs;
  return r;
}

// cut_before (u, q) records, for each formula on the way to q, the
// indices in u of the children it keeps and which one of them was cut (-1
// when none), so that uncut maps a position back by a lookup
struct cut_level {
  array<int> orig;
  int cut;
};

static tree
cut_before (tree u, path q, array<cut_level>& lv) {
  // u without what comes before the position q in it: the text which a
  // search from q sees (a tag which q is in keeps its arguments, those
  // before the one of q empty). The cut copy is searched out of its
  // context: a pattern which depends on the text before q (an emptied
  // argument, a formula which loses its first children) can match it
  // differently than the document (the hits are verified by the callers)
  if (is_nil (q)) return u;
  if (is_atomic (u)) return u->label (q->item, N(u->label));
  if (is_concat (u)) {
    int l= N(lv);
    lv << cut_level ();
    array<int> orig;
    int cut= -1;
    tree s (CONCAT);
    int k= q->item;
    if (k < N(u)) {
      tree c= u[k], x= "";
      bool whole= !is_atomic (c) && q->next == path (0);
      if (whole) x= c;
      else if (is_atomic (c) || q->next != path (1))
        x= cut_before (c, q->next, lv);
      if (x != "") { cut= (whole? -1: 0); orig << k; s << x; }
    }
    for (int i=k+1; i<N(u); i++)
      if (u[i] != "") { orig << i; s << u[i]; }
    lv[l].orig= orig; lv[l].cut= cut;
    if (N(s) == 0) return "";
    if (N(s) == 1) return s[0];
    return s;
  }
  if (q == path (0)) return u;
  if (q == path (1)) return "";
  tree r (L(u), N(u));
  for (int i=0; i<N(u); i++)
    r[i]= (i < q->item? tree (""):
           i == q->item? cut_before (u[i], q->next, lv): u[i]);
  return r;
}

static path
uncut (tree u, path q, path h, array<cut_level>& lv, int d= 0) {
  // the position in u of the position h in cut_before (u, q, lv), whose
  // formulas are lv[d], lv[d+1]... The start of a cut tree (before a tag
  // whose text before q was cut, or no position in a formula left with one
  // child) is q itself: a position, never the path of a node, nor before a
  // tag which holds q
  if (is_nil (q)) return h;
  if (is_atomic (u)) return path (q->item + (is_nil (h)? 0: h->item));
  if (is_concat (u)) {
    array<int>& orig= lv[d].orig;
    int n= N(orig), i= 0;
    if (n == 0) return q;
    if (n > 1) {
      if (is_nil (h) || h->item >= n) return q;
      i= h->item; h= h->next;
    }
    else if (lv[d].cut != 0 && is_nil (h)) return q;
    if (lv[d].cut == i)
      return path (orig[i], uncut (u[orig[i]], q->next, h, lv, d+1));
    return path (orig[i], h);
  }
  if (q == path (0)) return h;
  if (q == path (1)) return q;
  if (is_nil (h)) return q;
  if (is_nil (h->next)) return (h->item == 0? q: h);
  if (h->item == q->item)
    return path (q->item, uncut (u[q->item], q->next, h->next, lv, d));
  return h;
}

static void
add_hit (array<path>& r, path r1, path r2, path cur) {
  // a hit which starts before cur, or at a tag around it whose text before
  // cur was cut, starts at cur: the text before the reference, the output
  // of an earlier replacement, is never in it (no cur: the whole text)
  if (!is_nil (cur) && before_position (r1, cur)) r1= cur;
  if (path_less (r2, r1)) r2= r1;
  r << r1 << r2;
}

static bool
has_text (tree u) {
  if (is_atomic (u)) return N(u->label) != 0;
  for (int i=0; i<N(u); i++)
    if (has_text (u[i])) return true;
  return false;
}

static bool
text_before (tree u, path h) {
  // is there a character of u before the position h in it?
  if (is_nil (h)) return false;
  if (is_atomic (u)) return h->item > 0;
  if (is_nil (h->next)) return h->item == 1 && has_text (u);
  for (int i=0; i<h->item && i<N(u); i++)
    if (has_text (u[i])) return true;
  return h->item < N(u) && text_before (u[h->item], h->next);
}

static array<path>
search_from_paragraph (tree t, tree what, path p, path q, int limit,
                       bool after_empty) {
  // the first hits of what in the paragraph t at p after the position q in
  // it (see search_from)
  array<path> r;
  array<cut_level> lv;
  tree d (DOCUMENT, cut_before (t, q, lv));
  range_set sel= search (d, what, path (), limit);
  bool narrow= is_concat (what) && N(what) != 0 &&
               !contains_select_region (what);
  for (int i=0; i+1<N(sel) && i < 2*limit; i+=2) {
    path h1= sel[i], h2= sel[i+1];
    if (narrow) {
      // the match in the hit which Replace replaces (see replace_range),
      // in the text after cur: a hit of a lone tag there is the tag
      search_scope scope;
      replace_match rm;
      if (replace_range (d, h1, h2, what, rm)) { h1= rm.q1; h2= rm.q2; }
    }
    if (is_nil (h1) || h1->item != 0 || is_nil (h2) || h2->item != 0)
      continue;
    if (after_empty && !text_before (d, h1)) continue;
    add_hit (r, p * uncut (t, q, h1->next, lv),
             p * uncut (t, q, h2->next, lv), is_nil (q)? path (): p * q);
  }
  return r;
}

array<path>
search_from (tree t, tree what, path p, path cur, int limit,
             bool after_empty) {
  // the first hits (at most limit) of what in t at p (a paragraph, or the
  // document) from the position cur on, as the search finds them in the
  // text which comes after cur, each one narrowed to the match which
  // Replace replaces (see replace_range): the next match after a
  // replacement (#520). In a document, the paragraph of cur and then the
  // next ones, until there are hits (a pattern of several paragraphs: the
  // document after cur at once). After an empty match at cur (after_empty)
  // the next one starts at least one character later, so that Replace,
  // which goes on after its replacement, always ends. Each call searches
  // the rest of the paragraph of cur again: Replace All costs O(n^2) in a
  // paragraph of n hits, but the steps cost more (1000 hits in a paragraph
  // of 5 kB: about 2 s, as 1000 hits in 1 MB of short paragraphs)
  array<path> r;
  if (!(p <= cur)) return r;
  path q= cur / p;
  if (!is_position (t, q)) return r;
  if (!is_document (t))
    return search_from_paragraph (t, what, p, q, limit, after_empty);
  if (is_document (what)) {
    array<cut_level> lv;
    tree d= cut_before (t, q, lv);
    range_set sel= search (d, what, path (), limit);
    for (int i=0; i+1<N(sel) && i < 2*limit; i+=2)
      if (!after_empty || text_before (d, sel[i]))
        add_hit (r, p * uncut (t, q, sel[i], lv),
               p * uncut (t, q, sel[i+1], lv), cur);
    return r;
  }
  if (is_nil (q)) return r;
  for (int k=q->item; k<N(t) && N(r) == 0; k++)
    r= (k == q->item?
        search_from_paragraph (t[k], what, p * k, q->next, limit, after_empty):
        search_from_paragraph (t[k], what, p * k, path (), limit, false));
  return r;
}

static void
select_text (range_set& sel, tree t, tree what, path p) {
  if (has_one (what)) select_tokens (sel, t, what, p);
  else select_hits (sel, t, what, p, false);
}

static bool
is_whole_region (tree w) {
  // a part of a document pattern which is an empty region: the whole
  // paragraph, as an argument of a tag, but never an empty one
  return is_func (w, SELECT_REGION, 1) && is_empty (w[0]);
}

static bool
match_part (range_set& sel, range_set& rsel, tree u, path q, tree w, int m,
            int n, path from, bool whole) {
  // the part w (number m of n) of a document pattern in the paragraph u at
  // q: from the start of u (m > 0) or from from on (m == 0, nil: anywhere),
  // to its end (m < n-1); the whole paragraph if whole (a tag argument with
  // Replace). The range of the match in sel, its regions in rsel
  bool at_start= (m > 0) || whole, at_end= (m < n-1) || whole;
  if (is_whole_region (w)) {
    if (u == "" || (!is_nil (from) && !path_less_eq (from, q * start (u))))
      return false;
    sel << q * start (u) << q * end (u);
    rsel << q * start (u) << q * end (u);
    return true;
  }
  if (has_one (w)) {
    // a part with an empty region, which takes one token: with the tokens
    // of the paragraph, leftmost and lazy
    region_matcher r;
    tokens_between (r.toks, u, q, path (), path ());
    if (N(r.toks) == 0) return false;
    r.init (w, at_end);
    // as in the search: a leading wildcard reaches the start of the
    // paragraph (so a second match does not start after the end of a
    // previous one there), a trailing one its end
    int e1= 0, e2= N(r.kind) - 1;
    while (e1 < N(r.kind) && r.kind[e1] == ELEM_START) e1++;
    while (e2 >= 0 && r.kind[e2] == ELEM_END) e2--;
    bool lead= e1 < N(r.kind) && r.kind[e1] == ELEM_WILDCARD;
    bool trail= e2 >= 0 && r.kind[e2] == ELEM_WILDCARD;
    if (m == 0 && lead && !is_nil (from) && !path_less_eq (from, q * start (u)))
      return false;
    int st= 0;
    if (!is_nil (from))
      while (st < N(r.toks) && !path_less_eq (from, r.toks[st].before)) st++;
    bool ok= false;
    if (at_start) ok= (st == 0) && r.run (0, 0);
    else for (; st<N(r.toks) && !ok; st++) ok= r.run (0, st);
    if (!ok) return false;
    st= (at_start? 0: st-1);
    sel << r.position (st, true)
        << (trail? q * end (u): r.position (r.end, false));
    r.regions (rsel);
    return true;
  }
  tree x= without_empty_strings (without_regions (w), true);
  range_set ssel;
  bool after= m == 0 && !is_nil (from) && !path_less_eq (from, q * start (u));
  // a leading wildcard reaches the start of the paragraph: no second match
  // starts in it after the end of the first one
  if (after && (is_func (x, WILDCARD, 1) ||
                (is_concat (x) && N(x) > 0 && is_func (x[0], WILDCARD, 1))))
    return false;
  if (after) search_paragraph_after (ssel, u, x, q, from);
  else search_paragraph (ssel, u, x, q, at_start, at_end);
  if (N(ssel) < 2) return false;
  if (contains_select_region (w)) {
    if (is_func (w, SELECT_REGION, 1)) rsel << ssel[0] << ssel[1];
    else if (!select_in_hit (rsel, u, q, ssel[0] / q, ssel[1] / q, w))
      return false;
  }
  sel << ssel[0] << ssel[1];
  return true;
}

static void
select_paragraphs_one (range_set& sel, tree t, tree what, path p) {
  // a document pattern with an empty region in one of its parts, which the
  // search has no element for: the paragraphs of each document are matched
  // part by part, so that a candidate which fails does not hide a match
  // which overlaps it (the hits of the search with a wildcard in its place
  // did, and made a leading empty region a leading wildcard)
  if (is_atomic (t) || N(sel) > search_max_hits) return;
  if (!is_document (t)) {
    for (int i=0; i<N(t); i++)
      if (is_accessible_for_search (t, i))
        select_paragraphs_one (sel, t[i], what, p * i);
    return;
  }
  int n= N(what), last= -1;
  path from;
  for (int i=0; i<N(t) && N(sel) <= search_max_hits; i++) {
    bool found= false;
    if (i + n <= N(t)) {
      range_set psel, rsel;
      bool ok= true;
      for (int m=0; m<n && ok; m++)
        ok= match_part (psel, rsel, t[i+m], p * (i+m), what[m], m, n,
                        (m == 0 && !is_nil (from) && (p * i) <= from)?
                        from: path (), false);
      if (ok) {
        // first the matches in the documents inside this paragraph which
        // end before this one, as in search_document
        range_set nsel, bsel;
        select_paragraphs_one (nsel, t[i], what, p * i);
        for (int h=0; h+1<N(nsel); h+=2)
          if (path_less_eq (nsel[h+1], psel[0]) &&
              (i != last || path_less_eq (from, nsel[h])))
            bsel << nsel[h] << nsel[h+1];
        merge (sel, bsel);
        merge (sel, rsel);
        from= psel[N(psel)-1];
        found= true;
        last= i + n - 1;
        i += n - 2;  // the next match may start in the last paragraph
      }
    }
    if (!found) {
      // the matches in the documents inside the paragraph; in the last one
      // of a match, after its end
      range_set nsel, asel;
      select_paragraphs_one (nsel, t[i], what, p * i);
      for (int h=0; h+1<N(nsel); h+=2)
        if (i != last || path_less_eq (from, nsel[h]))
          asel << nsel[h] << nsel[h+1];
      merge (sel, asel);
    }
  }
}

static void
select_paragraphs_whole (range_set& sel, tree t, tree what, path p) {
  // a document pattern in a tag argument with Replace: the whole argument,
  // paragraph by paragraph, or inside its tags (a cascaded match)
  if (is_atomic (t)) return;
  if (is_document (t) && N(t) == N(what)) {
    range_set psel, rsel;
    bool ok= true;
    for (int m=0; m<N(what) && ok; m++)
      ok= match_part (psel, rsel, t[m], p * m, what[m], m, N(what), path (),
                      true);
    if (ok) { merge (sel, rsel); return; }
  }
  if (!cascaded_match_flag) return;
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i)) {
      range_set ssel;
      select_paragraphs_whole (ssel, t[i], what, p * i);
      merge (sel, ssel);
    }
}

static void
select_paragraphs (range_set& sel, tree t, tree what, path p) {
  // idem for a document pattern: its paragraphs match as in a search, and
  // the regions are found in the parts of the hit
  if (N(what) == 1) {
    select (sel, t, what[0], p);
    return;
  }
  if (select_in_argument && !partial_match_flag) {
    select_paragraphs_whole (sel, t, what, p);
    return;
  }
  for (int m=0; m<N(what); m++)
    if (is_whole_region (what[m]) || has_one (what[m])) {
      select_paragraphs_one (sel, t, what, p);
      return;
    }
  tree w= without_empty_strings (without_regions (what), true);
  range_set hits;
  search (hits, t, w, p);
  for (int i=0; i+1<N(hits); i+=2) {
    path h1= hits[i], h2= hits[i+1];
    path c= hit_container (h1, h2);
    if (!(p <= c)) continue;
    tree d= subtree (t, c / p);
    int k1= (h1 / c)->item, k2= (h2 / c)->item;
    if (!is_document (d) || k2 - k1 + 1 != N(what)) continue;
    range_set ssel;
    bool ok= true;
    for (int m=0; m<N(what) && ok; m++) {
      if (!contains_select_region (what[m])) continue;
      int k= k1 + m;
      path a= (m == 0? (h1 / c)->next: path ());
      path b= (m == N(what)-1? (h2 / c)->next: path ());
      if (is_func (what[m], SELECT_REGION, 1)) {
        ssel << (is_nil (a)? c * path (k, start (d[k])): c * path (k, a))
             << (is_nil (b)? c * path (k, end (d[k])): c * path (k, b));
        continue;
      }
      ok= select_in_hit (ssel, d[k], c * k, a, b, what[m]);
    }
    if (ok) merge (sel, ssel);
  }
}

static void
select_direct_sub (range_set& sel, tree t, tree what, path p) {
  if (L(t) != L(what)) return;
  if (!injective_match_flag) {
    // the arguments one by one, as match does
    if (N(t) != N(what)) return;
    range_set r;
    for (int i=0; i<N(t); i++)
      if (contains_select_region (what[i])) {
        range_set ssel;
        select (ssel, t[i], what[i], p * i);
        if (N(ssel) == 0) return;
        r << ssel;
      }
      else if (!match_cascaded (t[i], what[i])) return;
    merge (sel, r);
    return;
  }
  int cur=0;
  for (int i=0; i<N(t); i++)
    if (contains_select_region (what[cur])) {
      range_set ssel;
      select (ssel, t[i], what[cur], p * i);
      if (N(ssel) == 0) continue;
      int cur2= cur+1;
      for (int i2=i+1; i2<N(t); i2++)
        if (cur2 >= N(what)) break;
        else if (contains_select_region (what[cur2])) return;
        else if (match_cascaded (t[i2], what[cur2])) cur2++;
      if (cur2 >= N(what)) merge (sel, ssel);
      continue;
    }
    else {
      if (match_cascaded (t[i], what[cur])) cur++;
      if (cur >= N(what)) return;
    }
}

static void
select_direct (range_set& sel, tree t, tree what, path p) {
  // the children of a tag (formulas and documents are matched as text) are
  // arguments, also the formulas inside them, at any depth
  bool arg= select_in_argument;
  select_in_argument= true;
  select_direct_sub (sel, t, what, p);
  select_in_argument= arg;
}

void
select_cascaded (range_set& sel, tree t, tree what, path p) {
  // a pattern anywhere inside t: in the text, or inside an argument of a
  // tag of the pattern, as match_cascaded inside an argument
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i))
      select (sel, t[i], what, p*i);
}

void
select (range_set& sel, tree t, tree what, path p) {
  if (is_func (what, SELECT_REGION, 1)) {
    if (is_empty (what[0]))
      merge (sel, simple_range (p * start (t), p * end (t)));
    else if (!select_in_argument)
      // a region which is the whole pattern: as any pattern with regions, so
      // that one carried out of a tag by its wildcards stays in the
      // argument where it is, as when the argument is alone
      select_hits (sel, t, what, p, false);
    else
      // the content of the region anywhere in t, also in a tag argument
      // with Replace: (strong [a]) selects the letters a inside strong text
      // (the example of the manual)
      search (sel, t, what[0], p);
  }
  else if (is_concat (what)) select_text (sel, t, what, p);
  else if (is_document (what)) select_paragraphs (sel, t, what, p);
  else {
    range_set ssel;
    select_direct (ssel, t, what, p);
    if (N(ssel) > 0) merge (sel, ssel);
    else select_cascaded (sel, t, what, p);
  }
}

/******************************************************************************
* Front end
******************************************************************************/

static tree
without_empty_strings (tree what, bool join) {
  // an empty string in a concat pattern matches the empty string: it is
  // left out, also inside the tags of the pattern (#452); two strings in a
  // row are joined, as tree-simplify does in the editor (the search of a
  // part of the pattern which starts with a string assumes it: #481),
  // unless the pattern selects a region, where each child of a concat is
  // matched with a child of its own
  if (is_atomic (what)) return what;
  tree r (L(what));
  for (int i=0; i<N(what); i++) {
    tree w= without_empty_strings (what[i], join);
    if (is_concat (what) && w == "") continue;
    if (join && is_concat (what) && is_atomic (w) &&
        N(r) > 0 && is_atomic (r[N(r)-1]))
      r[N(r)-1]= tree (r[N(r)-1]->label * w->label);
    else r << w;
  }
  if (is_concat (what) && N(r) == 0) return "";
  // a concat of one element is that element, as tree-simplify keeps it
  if (is_concat (what) && N(r) == 1 && N(what) > 1) return r[0];
  return r;
}

range_set
search (tree t, tree what, path p, int limit) {
  search_max_hits= limit;
  clear_no_formula ();
  initialize_search ();  // also the label select-region, needed just below
  what= without_empty_strings (what, !contains_select_region (what));
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
  clear_no_formula ();
  return sel;
}

range_set
search (tree t, tree what, path p, path pos, int limit) {
  search_max_hits= limit;
  clear_no_formula ();
  initialize_search ();  // also the label select-region, needed just below
  what= without_empty_strings (what, !contains_select_region (what));
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p, pos);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
  clear_no_formula ();
  return sel;
}

range_set
previous_search_hit (range_set sels, path cur, bool strict) {
  int i= (N(sels) >> 1) << 1;
  while (i >= 2 && !path_less_eq (sels[i-2], cur)) i -= 2;
  if (strict && i >= 2 && !path_less (sels[i-1], cur)) i -= 2;
  if (i >= 2) return range (sels, i-2, i);
  return range_set ();
}

range_set
next_search_hit (range_set sels, path cur, bool strict) {
  int i=0, n=N(sels);
  while (i+2 <= n && !path_less_eq (cur, sels[i+1])) i += 2;
  while (i+4 <= n && sels[i+1] == sels[i+2] && sels[i+1] == cur) i += 2;
  if (strict && i+2 <= n) i += 2;
  if (i+2 <= n) return range (sels, i, i+2);
  return range_set ();
}

range_set current_alt_selection (string name);

range_set
navigate_search_hit (path cur, bool forward, bool extreme, bool strict) {
  range_set sels= current_alt_selection ("alternate");
  if ((N(sels) & 1) == 1) sels= range (sels, 0, N(sels) - 1);
  if (N(sels) < 2) return range_set ();
  if (extreme && !forward) return range (sels, 0, 2);
  if (extreme && forward) return range (sels, N(sels)-2, N(sels));
  if (!forward) return previous_search_hit (sels, cur, strict);
  else return next_search_hit (sels, cur, strict);
}
