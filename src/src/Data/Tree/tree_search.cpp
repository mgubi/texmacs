
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
  if (partial_match_flag) {
    search (sel, u, what, path ());
    if (cascaded_match_flag) return N(sel) != 0;
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
    range_set sel;
    search (sel, t[pos], what[i], p * pos);
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

void
search_compound (range_set& sel, tree t, tree what, path p) {
  if (match (t, what)) {
    merge (sel, simple_range (p * start (t), p * end (t)));
    return;
  }
  for (int i=0; i<N(t); i++)
    if (is_accessible_for_search (t, i))
      search (sel, t[i], what, p * i);
}

static void search_after (range_set& sel, tree t, tree what, path p,
                          path q);

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
      // overlap, and merge keeps the first one (#454)
      if (is_func (what[0], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i+=2)
          ssel[i]= (p * 0) * start (t[0]);
      if (is_func (what[N(what)-1], WILDCARD, 1))
        for (int i=0; i+1<N(ssel); i+=2)
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
  search (ssel, t, what, p);
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

void
search (range_set& sel, tree t, tree what, path p) {
  if (N(sel) > search_max_hits) return;
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
    else {
      int hits= 0;
      array<range_set> sub (N(t));
      if (pos->item >= 0 && pos->item < N(t))
        if (is_accessible_for_search (t, pos->item)) {
          search (sub[pos->item], t[pos->item], what, p * pos->item, pos->next);
          hits += N(sub[pos->item]);
        }
      for (int d=1; d<N(t); d++)
        for (int e=0; e<=1; e++) {
          if (hits > search_max_hits) break;
          int i= (e==0? pos->item + d: pos->item - d);
          if (i >= 0 && i < N(t))
            if (is_accessible_for_search (t, i)) {
              search (sub[i], t[i], what, p * i);
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

static bool
match_child (tree t, int i, tree what) {
  // the child i of t matches what: in a formula or a document which is not
  // in the argument of a tag of the pattern, the strings are text and are
  // matched as when searching, also when replacing; the partial and
  // initial flags only concern the arguments of tags (#495)
  if (!(is_concat (t) || is_document (t)) || select_in_argument ||
      !is_atomic (t[i]))
    return match_cascaded (t[i], what);
  bool partial= partial_match_flag, initial= initial_match_flag;
  partial_match_flag= initial_match_flag= true;
  bool r= match_cascaded (t[i], what);
  partial_match_flag= partial;
  initial_match_flag= initial;
  return r;
}

static void
select_direct_sub (range_set& sel, tree t, tree what, path p) {
  if (L(t) != L(what)) return;
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
        else if (match_child (t, i2, what[cur2])) cur2++;
      if (cur2 >= N(what)) merge (sel, ssel);
      continue;
    }
    else {
      if (match_child (t, i, what[cur])) cur++;
      if (cur >= N(what)) return;
    }
}

void
select_direct (range_set& sel, tree t, tree what, path p) {
  // the children of a tag other than a formula or a document are arguments,
  // also the formulas inside them, at any depth
  bool arg= select_in_argument;
  if (L(t) == L(what) && !is_concat (t) && !is_document (t))
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
    else
      search (sel, t, what[0], p);
  }
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
  initialize_search ();  // also the label select-region, needed just below
  what= without_empty_strings (what, !contains_select_region (what));
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
  return sel;
}

range_set
search (tree t, tree what, path p, path pos, int limit) {
  search_max_hits= limit;
  initialize_search ();  // also the label select-region, needed just below
  what= without_empty_strings (what, !contains_select_region (what));
  range_set sel;
  //cout << "Search " << what << ", " << contains_select_region (what) << "\n";
  if (contains_select_region (what)) select (sel, t, what, p);
  else search (sel, t, what, p, pos);
  //cout << "Selected " << sel << "\n";
  search_max_hits= 1000000;
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
