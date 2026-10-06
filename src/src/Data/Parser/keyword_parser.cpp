
/******************************************************************************
* MODULE     : keyword_parser.cpp
* DESCRIPTION: shared keyword parsing routines
* COPYRIGHT  : (C) 2020  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "keyword_parser.hpp"
#include "iterator.hpp"
#include "analyze.hpp"
#include "tree.hpp"
#include "scheme.hpp"

keyword_parser_rep::keyword_parser_rep () {
  current_keyword= "";
  keyword_group= hashmap<string, string>();
}

bool
keyword_parser_rep::can_parse (string s, int pos) {
  // the keywords of several words first ("mutable struct" in julia)
  if (can_parse_phrase (s, pos)) return true;
  // Read the whole identifier, so that keywords are not matched on prefixes
  // (as "for" in "for_each") and keywords with '_' or digits can match
  int i= pos;
  while (i<N(s) && (is_alpha (s[i]) || is_digit (s[i]) || s[i] == '_')) i++;
  string word= s (pos, i);
  bool hit= i>pos && keyword_group->contains (word);
  if (hit) current_keyword= word;
  return hit;
}

bool
keyword_parser_rep::can_parse_phrase (string s, int pos) {
  // keywords of several words, as "mutable struct" in julia, separated by
  // a single space and followed by the end of a word
  for (int i=0; i<N(phrases); i++) {
    string phrase= phrases[i];
    int end= pos + N(phrase);
    if (test (s, pos, phrase) &&
        (end >= N(s) || !(is_alpha (s[end]) || is_digit (s[end]) ||
                          s[end] == '_'))) {
      current_keyword= phrase;
      return true;
    }
  }
  return false;
}

void
keyword_parser_rep::do_parse (string s, int& pos) {
  pos+= N(current_keyword);
}

void
keyword_parser_rep::use_keywords_of_lang (string lang_code) {
  string use_modules= "(use-modules (prog " * lang_code * "-lang))";
  eval (use_modules);
  string get_list_of_keywords_tree= "(map tm->tree (" * lang_code * "-keywords))";
  list<tree> l= as_list_tree (eval (get_list_of_keywords_tree));
  if (DEBUG_PARSER)
    debug_packrat << "Keywords definition of [" << lang_code << "] loaded!\n";
  for (int i=0; i<N(l); i++) {
    tree group_words= l[i];
    string group= get_label (group_words);
    for (int j=0; j<N(group_words); j++) {
      string word= get_label (group_words[j]);
      put (word, group);
    }
  }
}
