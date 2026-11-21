
/******************************************************************************
* MODULE     : to_scheme.cpp
* DESCRIPTION: conversion of scheme expressions to TeXmacs trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "analyze.hpp"
#include "drd_std.hpp"
#include "path.hpp"
#include "stack_guard.hpp"

/******************************************************************************
* Handling escape characters
******************************************************************************/

string
unslash (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if ((s[i]=='\\') && ((i+1)<n))
      switch (s[++i]) {
      case '0': r << ((char) 0); break;
      case 'n': r << '\n'; break;
      case 'r': r << '\r'; break;
      case 't': r << '\t'; break;
      default: r << s[i];
      }
    else r << s[i];
  return r;
}

/******************************************************************************
* Converting strings to scheme trees
******************************************************************************/

static bool
is_spc (char c) {
  return (c==' ') || (c=='\t') || (c=='\n') || (c=='\r');
}

// A text of Scheme nested too deep (the scheme: output of a plug-in, a .stm
// file, stm->texmacs) is cut at the depth which the editor takes, as a .tm
// file and a list of Scheme made into a tree are (tree_max_depth, #813):
// what is deeper is an error "nesting too deep", skipped without recursion,
// with one warning for each text which is cut

struct scheme_reader {
  string s;    // the text being read
  int    max;  // deeper lists are cut
  int    cut;  // the number of lists cut
  int    at;   // the least depth at which a list was cut
  scheme_reader (string s2):
    s (s2), max (tree_max_depth ()), cut (0), at (max) {}
  ~scheme_reader () {
    if (cut > 0)
      convert_warning << "Scheme text nested deeper than " << at
                      << " levels cut" << LF; }
  scheme_tree read (int& i, int depth);
  void skip_atom (int& i);
  void skip_string (int& i);
  void skip_comment (int& i);
  scheme_tree skip (int& i, int depth);
};

// (the tokens as read reads them: an atom up to a space, ( or ), with its
// escapes, so that ab"c and ab;c are atoms; a string or a comment only at
// the start of a token. Each loop stops at the end of the text, with i at
// most N(s))

void
scheme_reader::skip_atom (int& i) {
  while ((i<N(s)) && (!is_spc(s[i])) && (s[i]!='(') && (s[i]!=')')) {
    if ((i<N(s)-1) && (s[i]=='\\')) i++;
    i++;
  }
}

void
scheme_reader::skip_string (int& i) {
  // at the " of a string: i after its end
  i++;
  while ((i<N(s)) && (s[i]!='\"')) {
    if ((i<N(s)-1) && (s[i]=='\\')) i++;
    i++;
  }
  if (i<N(s)) i++;
}

void
scheme_reader::skip_comment (int& i) {
  // at the ; of a comment: i at the end of its line
  while ((i<N(s)) && (s[i]!='\n')) i++;
}

scheme_tree
scheme_reader::skip (int& i, int depth) {
  // at the ( of a list nested too deep: its error, and i after its ),
  // without recursion
  cut++;
  if (depth < at) at= depth;
  int j= i+1;
  while ((j<N(s)) && is_spc(s[j])) j++;
  int start= j;
  skip_atom (j);
  string name= s (start, j);
  int open= 0;
  while (i<N(s)) {
    char c= s[i];
    if (is_spc (c) || c == '\'') i++;
    else if (c == '(') { open++; i++; }
    else if (c == ')') { i++; if (--open == 0) break; }
    else if (c == '\"') skip_string (i);
    else if (c == ';') skip_comment (i);
    else skip_atom (i);
  }
  return tree_to_scheme_tree (texmacs_nesting_error (name));
}

scheme_tree
scheme_reader::read (int& i, int depth) {
  for (; i<N(s); i++)
    switch (s[i]) {

    case ' ':
    case '\t':
    case '\n':
    case '\r':
      break;
      case '(':
      {
        if (tree_too_deep (depth, max)) return skip (i, depth);
        scheme_tree p (TUPLE);
        i++;
        while (true) {
          while ((i<N(s)) && is_spc(s[i])) i++;
          if ((i>=N(s)) || (s[i]==')')) break;
          p << read (i, depth + 1);
        }
        if (i<N(s)) i++;
        return p;
      }
        
      case '\'':
        i++;
        if (tree_too_deep (depth, max)) {
          // (quotes of quotes: the quoted expression is skipped)
          while ((i<N(s)) && (is_spc(s[i]) || s[i]=='\'' || s[i]==';'))
            if (s[i]==';') skip_comment (i);
            else i++;
          if ((i<N(s)) && (s[i]=='(')) return skip (i, depth);
          cut++;
          if (depth < at) at= depth;
          if (i<N(s) && s[i]=='\"') skip_string (i);
          else skip_atom (i);
          return tree_to_scheme_tree (texmacs_nesting_error (""));
        }
        return scheme_tree (TUPLE, "\'", read (i, depth + 1));
        
      case '\"':
      { // "
        int start= i++;
        while ((i<N(s)) && (s[i]!='\"')) { // "
          if ((i<N(s)-1) && (s[i]=='\\')) i++;
          i++;
        }
        if (i<N(s)) i++;
        return scheme_tree (unslash (s (start, i)));
      }
        
      case ';':
        // (at the end of the text, i stays there: the loop of a list stops)
        while ((i<N(s)) && (s[i]!='\n')) i++;
        if (i >= N(s)) return "";
        break;
        
      default:
      {
        int start= i;
        while ((i<N(s)) && (!is_spc(s[i])) && (s[i]!='(') && (s[i]!=')')) {
          if ((i<N(s)-1) && (s[i]=='\\')) i++;
          i++;
        }
        return scheme_tree (unslash (s (start, i)));
      }
    }
  
  return "";
}

scheme_tree
string_to_scheme_tree (string s) {
  int i=0;
  scheme_reader r (s);
  return r.read (i, 0);
}

scheme_tree
block_to_scheme_tree (string s) {
  scheme_tree p (TUPLE);
  scheme_reader r (s);
  int i=0;
  while ((i<N(s)) && (is_spc (s[i]) || s[i]==')')) i++;
  while (i<N(s)) {
    p << r.read (i, 1);
    while ((i<N(s)) && (is_spc (s[i]) || s[i]==')')) i++;
  }
  return p;
}

/******************************************************************************
* Converting scheme trees to trees
******************************************************************************/

tree
scheme_tree_to_tree (scheme_tree t, hashmap<string,int> codes, bool flag) {
  if (is_atomic (t)) return scm_unquote (t->label);
  else if ((N(t) == 0) || is_compound (t[0])) {
    convert_error << "Invalid scheme tree " << t << "\n";
    return
      compound ("errput", 
                concat ("The tree was ", as_string (L(t)), ": ", tree (t)));
  }
  else {
    int i, n= N(t);
    tree_label code= (tree_label) codes [t[0]->label];
    if (flag) code= make_tree_label (t[0]->label);
    if (code == UNKNOWN) {
      tree u (EXPAND, n);
      u[0]= copy (t[0]);
      for (i=1; i<n; i++)
        u[i]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
    else {
      tree u (code, n-1);
      for (i=1; i<n; i++)
        u[i-1]= scheme_tree_to_tree (t[i], codes, flag);
      return u;
    }
  }
}

tree
scheme_tree_to_tree (scheme_tree t, string version) {
  version= scm_unquote (version);
  tree doc, error (_ERROR, "bad format or data");
  if (version_inf (version, "1.0.2.4"))
    doc= scheme_tree_to_tree (t, get_codes (version), false);
  else doc= scheme_tree_to_tree (t);
  if (!is_document (doc)) return error;
  return upgrade (doc, version);
}

tree
scheme_tree_to_tree (scheme_tree t) {
  return scheme_tree_to_tree (t, STD_CODE, true);
}

/******************************************************************************
* Converting scheme strings to trees
******************************************************************************/

tree
scheme_to_tree (string s) {
  return scheme_tree_to_tree (string_to_scheme_tree (s));
}

tree
scheme_document_to_tree (string s) {
  tree error (_ERROR, "bad format or data");
  if (starts (s, "(document (apply \"TeXmacs\" ") ||
      starts (s, "(document (expand \"TeXmacs\" ") ||
      starts (s, "(document (TeXmacs "))
  {
    int i, begin=27;
    if (starts (s, "(document (expand \"TeXmacs\" ")) begin= 28;
    if (starts (s, "(document (TeXmacs ")) begin= 19;
    for (i=begin; i<N(s); i++)
      if (s[i] == ')') break;
    string version= s (begin, i);
    tree t  = string_to_scheme_tree (s);
    return scheme_tree_to_tree (t, version);
  }
  return error;
}
