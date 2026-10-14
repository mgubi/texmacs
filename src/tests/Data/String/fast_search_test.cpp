/******************************************************************************
* MODULE     : fast_search_test.cpp
* DESCRIPTION: tests of the fast multiple searches in a same string
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "fast_search.hpp"
#include "analyze.hpp"

/******************************************************************************
* Naive reference implementations
******************************************************************************/

static array<int>
naive_search_all (string s, string what) {
  array<int> r;
  for (int i=0; i + N(what) <= N(s); i++)
    if (s (i, i + N(what)) == what) r << i;
  return r;
}

static int
naive_search_next (string s, string what, int pos) {
  array<int> a= naive_search_all (s, what);
  for (int i=0; i<N(a); i++)
    if (a[i] >= pos) return a[i];
  return -1;
}

static int
naive_longest_common (string s1, string s2) {
  int best= 0;
  for (int i=0; i<N(s1); i++)
    for (int j=0; j<N(s2); j++) {
      int l= 0;
      while (i+l < N(s1) && j+l < N(s2) && s1[i+l] == s2[j+l]) l++;
      if (l > best) best= l;
    }
  return best;
}

static string
show (array<int> a) {
  string r= "[";
  for (int i=0; i<N(a); i++) {
    if (i > 0) r << ", ";
    r << as_string (a[i]);
  }
  return r * "]";
}

static unsigned int rnd_state= 12345;

static int
rnd (int n) {
  rnd_state= rnd_state * 1103515245 + 12345;
  return (int) ((rnd_state >> 16) % ((unsigned int) n));
}

// a random string over the first k letters of the given alphabet; small
// alphabets give many overlapping and repeated matches
static string
random_string (int n, string alphabet, int k) {
  string r (n);
  for (int i=0; i<n; i++) r[i]= alphabet[rnd (k)];
  return r;
}

/******************************************************************************
* Tests
******************************************************************************/

static void
test_get_string () {
  string_searcher ss ("hello world");
  CHECK_EQ (ss->get_string (), string ("hello world"));
  string_searcher empty;
  CHECK_EQ (empty->get_string (), string (""));
}

static void
test_simple () {
  string_searcher ss ("abracadabra");
  CHECK_EQ (show (ss->search_all ("abra")), string ("[0, 7]"));
  CHECK_EQ (show (ss->search_all ("a")), string ("[0, 3, 5, 7, 10]"));
  CHECK_EQ (show (ss->search_all ("cad")), string ("[4]"));
  CHECK_EQ (show (ss->search_all ("abracadabra")), string ("[0]"));
  CHECK_EQ (show (ss->search_all ("xyz")), string ("[]"));
  CHECK_EQ (ss->search_next ("abra", 0), 0);
  CHECK_EQ (ss->search_next ("abra", 1), 7);
  CHECK_EQ (ss->search_next ("abra", 7), 7);
  CHECK_EQ (ss->search_next ("abra", 8), -1);
  CHECK_EQ (ss->search_next ("zzz", 0), -1);
}

// overlapping occurrences are all reported
static void
test_overlapping () {
  string_searcher ss ("aaaaaa");
  CHECK_EQ (show (ss->search_all ("aa")), string ("[0, 1, 2, 3, 4]"));
  CHECK_EQ (show (ss->search_all ("aaa")), string ("[0, 1, 2, 3]"));
  CHECK_EQ (show (ss->search_all ("aaaaaa")), string ("[0]"));
  string_searcher tt ("abababab");
  CHECK_EQ (show (tt->search_all ("abab")), string ("[0, 2, 4]"));
  CHECK_EQ (show (tt->search_all ("bab")), string ("[1, 3, 5]"));
}

// the empty pattern matches at every position, including the end
static void
test_empty_pattern () {
  string_searcher ss ("abc");
  CHECK_EQ (show (ss->search_all ("")), string ("[0, 1, 2, 3]"));
  CHECK_EQ (ss->search_next ("", 0), 0);
  CHECK_EQ (ss->search_next ("", 2), 2);
  CHECK_EQ (ss->search_next ("", 3), 3);
  CHECK_EQ (ss->search_next ("", 4), -1);
  string_searcher empty ("");
  CHECK_EQ (show (empty->search_all ("")), string ("[0]"));
}

// bytes above 127 and the null byte are ordinary characters
// a pattern longer than the text, also an empty text, finds nothing (it
// used to index past the table of hash levels and crash)
static void
test_longer_than_text () {
  string_searcher ab ("ab");
  CHECK_EQ (show (ab->search_all ("abcd")), string ("[]"));
  CHECK_EQ (ab->search_next ("abcd", 0), -1);
  string_searcher abc ("abc");
  CHECK_EQ (show (abc->search_all ("abcd")), string ("[]"));
  string_searcher empty ("");
  CHECK_EQ (show (empty->search_all ("a")), string ("[]"));
  CHECK_EQ (empty->search_next ("a", 0), -1);
}

static void
test_binary () {
  string s ("\x00\xff\x80\x00\xff", 5);
  string_searcher ss (s);
  CHECK_EQ (show (ss->search_all (string ("\x00\xff", 2))), string ("[0, 3]"));
  CHECK_EQ (show (ss->search_all (string ("\xff\x80", 2))), string ("[1]"));
  CHECK_EQ (show (ss->search_all ("\xce\xb1")), string ("[]"));
  string_searcher tt ("x\xce\xb1y\xce\xb1");
  CHECK_EQ (show (tt->search_all ("\xce\xb1")), string ("[1, 4]"));
}

// compare with the naive search on many random strings and patterns,
// with patterns taken both inside the text and at random
static void
test_against_naive () {
  string alphabet= "ab\xe9\x01xyzt";
  int mismatches= 0;
  for (int round=0; round<300; round++) {
    int k= 1 + rnd (4);
    int n= rnd (80);
    string s= random_string (n, alphabet, k);
    string_searcher ss (s);
    for (int j=0; j<20; j++) {
      string what;
      int m= 1 + rnd (12);
      if (n > 0 && rnd (2) == 0) {
        int b= rnd (n);
        what= s (b, min (n, b + m));
      }
      else what= random_string (m, alphabet, k);
      array<int> got= ss->search_all (what);
      array<int> exp= naive_search_all (s, what);
      if (show (got) != show (exp)) {
        if (mismatches++ < 5)
          CHECK_MSG (false, "search_all (\"" * what * "\") in \"" * s *
                     "\" gives " * show (got) * ", not " * show (exp));
      }
      int pos= rnd (n + 2);
      int g= ss->search_next (what, pos);
      int e= naive_search_next (s, what, pos);
      if (g != e) {
        if (mismatches++ < 5)
          CHECK_MSG (false, "search_next (\"" * what * "\", " *
                     as_string (pos) * ") in \"" * s * "\" gives " *
                     as_string (g) * ", not " * as_string (e));
      }
    }
  }
  CHECK_EQ (mismatches, 0);
}

// long periodic strings exercise the hash levels for large powers of two,
// including the level 32 where the rotation of the hash is trivial
static void
test_long_periodic () {
  string s;
  for (int i=0; i<300; i++) s << "abcab";
  string_searcher ss (s);
  string pats[]= { "abcab", "cababcab", "bcababcababcababcababcababcababcabab",
                   s (7, 7 + 64), s (3, 3 + 100), s (0, 1024), s (1, N(s)) };
  for (int i=0; i<7; i++)
    CHECK_MSG (show (ss->search_all (pats[i])) ==
               show (naive_search_all (s, pats[i])),
               "periodic pattern " * as_string (i));
}

// a searcher answers many different queries on the same string
static void
test_reuse () {
  string s= random_string (500, "abc", 3);
  string_searcher ss (s);
  for (int b=0; b+5<=N(s); b+=13) {
    string what= s (b, b+5);
    array<int> got= ss->search_all (what);
    CHECK_MSG (show (got) == show (naive_search_all (s, what)),
               "query at " * as_string (b));
  }
}

// get_longest_common finds a longest common substring; the positions
// returned must delimit equal substrings of the maximal length
static void
test_longest_common () {
  int b1, e1, b2, e2;
  get_longest_common ("the quick brown fox", "a quick brown dog", b1, e1, b2, e2);
  CHECK_EQ (string ("the quick brown fox") (b1, e1), string (" quick brown "));
  CHECK_EQ (b1, 3);
  CHECK_EQ (b2, 1);
  get_longest_common ("abc", "xyz", b1, e1, b2, e2);
  CHECK_EQ (e1 - b1, 0);
  get_longest_common ("abcdef", "abcdef", b1, e1, b2, e2);
  CHECK_EQ (b1, 0); CHECK_EQ (e1, 6); CHECK_EQ (b2, 0); CHECK_EQ (e2, 6);
  int bad= 0;
  for (int round=0; round<200; round++) {
    string s1= random_string (1 + rnd (40), "abcd", 1 + rnd (4));
    string s2= random_string (1 + rnd (40), "abcd", 1 + rnd (4));
    get_longest_common (s1, s2, b1, e1, b2, e2);
    int l= naive_longest_common (s1, s2);
    bool ok= (e1 - b1 == l) && (e2 - b2 == l) &&
             0 <= b1 && e1 <= N(s1) && 0 <= b2 && e2 <= N(s2) &&
             s1 (b1, e1) == s2 (b2, e2);
    if (!ok && bad++ < 5)
      CHECK_MSG (false, "longest common of \"" * s1 * "\" and \"" * s2 *
                 "\" gives " * s1 (b1, e1) * ", expected length " *
                 as_string (l));
  }
  CHECK_EQ (bad, 0);
}

int
main () {
  RUN (test_get_string);
  RUN (test_simple);
  RUN (test_overlapping);
  RUN (test_empty_pattern);
  RUN (test_longer_than_text);
  RUN (test_binary);
  RUN (test_against_naive);
  RUN (test_long_periodic);
  RUN (test_reuse);
  RUN (test_longest_common);
  return test_report ();
}
