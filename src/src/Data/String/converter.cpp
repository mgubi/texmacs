
/******************************************************************************
* MODULE     : converter.cpp
* DESCRIPTION: Applies dictionaries to strings in an efficient manner.
* COPYRIGHT  : (C) 2002  Felix Breuer
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "converter.hpp"
#include "convert.hpp"
#ifdef USE_ICONV
#include <iconv.h>
#endif
#include <errno.h>

RESOURCE_CODE (converter);

/******************************************************************************
* converter methods
******************************************************************************/

void
operator << (converter c, string str) {
  int index = 0;
  while (index < N(str))
    c->match(str, index);
}

string
apply (converter c, string str) {
  c->output = string();
  c << str;
  return flush(c);
}

string
flush (converter c) {
  string result = c->output;
  c->output = string();
  return result;
}

/******************************************************************************
* method for loading converters
******************************************************************************/
 
converter
load_converter (string from, string to) {
  string name= from * "-" * to;
  if (converter::instances -> contains (name))
    return converter (name);
  converter conv = tm_new<converter_rep> (from, to);
  return conv;
}

/******************************************************************************
* converter_rep methods
******************************************************************************/

inline bool
converter_rep::has_value(hashtree<char,string> node) {
  return node->label != nil_string;
}

inline void
converter_rep::match (string& str, int& index) {
  int forward = index;
  int last_match = -1;
  string value("");
  bool done = false;
  hashtree<char,string> node = ht;
  //cout << "[";
  while (!done && forward < N(str)) {
    if (node->contains (str[forward])) {
      node = node(str[forward]);
      //printf("->%x",str[forward]);
      if (has_value(node)) {
        last_match = forward;
        value = node->label;
      }
      forward++;
    }
    else done = true;
  }
  if (last_match==-1) {
    if (copy_unmatched)
      output << string(str[index]);
    index++;
  }
  else {
    //printf(":");for(int i = 0; i < N(value);i++) printf("%x ",value[i]);
    output << value;
    index = last_match + 1;
  }
  //cout << "]";
}

void
converter_rep::load () {
  // to handle each case individually seems unelegant, but there is simply more
  // to be done here than just loading a file.
  // cout << "TeXmacs] load converter " << from << " -> " << to << "\n";
  if (from=="Cork" && to=="UTF-8" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-fallback", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"emoji-names", BIT2BIT, UTF8, false);
    ht = dic;
  }
  else if (from=="UTF-8" && to=="Emoji") {
    // the names of the emoji (<emoji-grinning-face>), by their sequences
    // in UTF-8, for utf8_to_cork
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"emoji-names", UTF8, BIT2BIT, true);
    ht = dic;
  }
  else if (from=="LaTeX-emoji" && to=="Emoji") {
    // the other names of the LaTeX package emoji (\emoji{smile}, the old
    // names of CLDR): the names of TeXmacs, for the import of LaTeX
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"emoji-latex-aliases", BIT2BIT, BIT2BIT, false);
    ht = dic;
  }
  else if (from=="UTF-8" && to=="Cork") {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-symbol-oneway", UTF8, BIT2BIT, true);
    ht = dic;
  }
  else if (from=="Strict-Cork" && to=="UTF-8" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"emoji-names", BIT2BIT, UTF8, false);
    ht = dic;
  }
  else if (from=="UTF-8" && to=="HTML") {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic, "HTMLlat1"   , CHAR_ENTITY, ENTITY_NAME, true);
    hashtree_from_dictionary (dic, "HTMLspecial", CHAR_ENTITY, ENTITY_NAME, true);
    hashtree_from_dictionary (dic, "HTMLsymbol" , CHAR_ENTITY, ENTITY_NAME, true);
    ht = dic;
  }
  else if (from=="HTML" && to=="UTF-8") {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic, "HTMLlat1"   , ENTITY_NAME, CHAR_ENTITY, false);
    hashtree_from_dictionary (dic, "HTMLspecial", ENTITY_NAME, CHAR_ENTITY, false);
    hashtree_from_dictionary (dic, "HTMLsymbol" , ENTITY_NAME, CHAR_ENTITY, false);
    ht = dic;
  }
  else if (from=="T2A" && to=="UTF-8" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-fallback", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"t2atounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"emoji-names", BIT2BIT, UTF8, false);
    ht = dic;
  }  
  else if (from=="UTF-8" && to=="T2A" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-symbol-oneway", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"t2atounicode", UTF8, BIT2BIT, true);
    ht = dic;
  }  
  else if (from=="T2A.CY" && to=="CODEPOINT" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"t2atounicode", BIT2BIT, CHAR_ENTITY, false);
    hashtree_from_dictionary (dic,"t2atounicode", CHAR_ENTITY, CHAR_ENTITY, false);
    ht = dic;
  }
  else if (from=="CODEPOINT" && to=="T2A.CY" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"t2atounicode", CHAR_ENTITY, BIT2BIT, true);
    hashtree_from_dictionary (dic,"t2atounicode", CHAR_ENTITY, CHAR_ENTITY, true);
    ht = dic;
  }
  else if (from=="UTF-8" && to=="LaTeX" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"utf8tolatex", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic,"utf8tolatex-onedir", UTF8, BIT2BIT, false);
    ht = dic;
  }
  else if (from=="LaTeX" && to=="UTF-8" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"utf8tolatex", BIT2BIT, UTF8, true);
    hashtree_from_dictionary (dic,"utf8tolatex-back", BIT2BIT, UTF8, true);
    ht = dic;
  }
  else if (from=="Cork" && to=="ASCII") {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic, "cork-escaped-to-ascii", BIT2BIT, UTF8, false);
    ht = dic;
  }
  else if (from=="Cork" && to=="SourceCode" ) {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", BIT2BIT, UTF8, false);
      //hashtree_from_dictionary (dic,"cork-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-oneway", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-fallback", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"symbol-unicode-math", BIT2BIT, UTF8, false);
    hashtree_from_dictionary (dic,"cork-to-real-ascii", BIT2BIT, BIT2BIT, false);
    hashtree_from_dictionary (dic,"emoji-names", BIT2BIT, UTF8, false);
    ht = dic;
  }
  else if (from=="SourceCode" && to=="Cork") {
    hashtree<char,string> dic;
    hashtree_from_dictionary (dic,"corktounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-cork-oneway", UTF8, BIT2BIT, false);
    hashtree_from_dictionary (dic,"tmuniversaltounicode", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"tmuniversaltounicode-extra", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"unicode-symbol-oneway", UTF8, BIT2BIT, true);
    hashtree_from_dictionary (dic,"cork-to-real-ascii", UTF8, BIT2BIT, true);
    ht = dic;
  }
}

/******************************************************************************
* convenience functions
******************************************************************************/

bool
check_encoding (string input, string encoding) {
  if (encoding == "Cork") return true;
#ifdef USE_ICONV
  else return check_using_iconv (input, encoding);
#else
  (void) input;
  return true; // Assume everything is fine.
#endif
}

string 
convert (string input, string from, string to) {
  if (from == "Cork")
    return convert_from_cork (input, to);
  else if (to == "Cork")
    return convert_to_cork (input,from);
  else if (from == "LaTeX" && to == "UTF-8")
    return convert_LaTeX_to_utf8 (input);
  else if (from == "UTF-8" && to == "LaTeX")
    return convert_utf8_to_LaTeX (input);
#ifdef USE_ICONV
  else
    return convert_using_iconv (input, from, to);
#else
  return input; // can't do anything.
#endif
}

string 
convert_to_cork (string input, string from) {
  if (from == "UTF-8")
    return utf8_to_cork (input);
  else if (from == "SourceCode")
    return sourcecode_to_cork (input);
#ifdef USE_ICONV
  string str= convert_using_iconv (input, from, "UTF-8");
  return utf8_to_cork (str);
#else
  return input; // can't do anything.
#endif
}

string 
convert_from_cork (string input, string to) {
  if (to == "UTF-8")
    return cork_to_utf8 (input);
  else if (to == "SourceCode")
    return cork_to_sourcecode (input);
#ifdef USE_ICONV
  string str = cork_to_utf8 (input);
  return convert_using_iconv (str, "UTF-8", to);
#else
  return input; // can't do anything.
#endif
}

string
convert_LaTeX_to_utf8 (string input) {
  converter conv= load_converter ("LaTeX", "UTF-8");
  string output;
  output= apply (conv, input);
  return output;
}

string
convert_utf8_to_LaTeX (string input) {
  converter conv= load_converter ("UTF-8", "LaTeX");
  int i, start, n= N(input);
  string output, r;
  for (i=0; i<n; ) {
    if (((unsigned char) input[i]) < 128 && ((unsigned char) input[i]) > 31) {
      output << input[i++];
      continue;
    }
    else {
      start = i;
      unsigned int code= decode_from_utf8 (input, i);
      string unicode= input (start, i);
      r = apply (conv, unicode);
      if (r == unicode) {
        if (code == 10) output << "-";
        else {
          output << r;
          cout << "TeXmacs] non ascii character <#"
               << as_hexadecimal (code) << "> on output: "
               << unicode << "\nLaTeX output may not compile.\n";
          //output << "(error)";
        }
      }
      else output << r;
    }
  }
  return output;
}

/******************************************************************************
* The names of the emoji
******************************************************************************/

// The emoji of Unicode have names in the universal encoding, those of the
// LaTeX package emoji: <emoji-grinning-face> for U+1F600, and one name for a
// whole sequence, <emoji-family-man-woman-girl> for U+1F468 U+200D U+1F469
// U+200D U+1F467 (TeXmacs/langs/encoding/emoji-names.scm, made from the
// emoji-test.txt of Unicode by misc/emoji/make-emoji-names.py). A text in
// UTF-8 gets the names of its fully-qualified emoji (utf8_to_cork); the
// older form, a sequence of code points <#1F600>, stays valid.

static bool
match_emoji_name (converter conv, string s, int& i, string& name) {
  // the longest emoji of the table whose UTF-8 sequence starts at i
  hashtree<char,string> node= conv->ht;
  int j= i, last= -1;
  while (j < N(s) && node->contains (s[j])) {
    node= node (s[j++]);
    if (node->label != "") { last= j; name= node->label; }
  }
  if (last < 0) return false;
  i= last;
  return true;
}

static bool
match_emoji_name (converter conv, converter cconv, string s, int& i,
                  string& name) {
  // as above, but a character which has a symbol of its own keeps it
  // (U+2648 is <aries>, U+25FD <square>)
  int j= i;
  if (!match_emoji_name (conv, s, j, name)) return false;
  int k= i;
  decode_from_utf8 (s, k);
  if (k == j && apply (cconv, s (i, j)) != s (i, j)) return false;
  i= j;
  return true;
}

static bool
dictionary_value (converter conv, string key, string& val) {
  // the value of the key in the dictionary of conv, the whole key
  hashtree<char,string> node= conv->ht;
  for (int i= 0; i < N(key); i++) {
    if (!node->contains (key[i])) return false;
    node= node (key[i]);
  }
  val= node->label;
  return val != "";
}

static bool
is_emoji_token (string s, int i) {
  return s[i] == '<' && test (s, i, "<emoji-");
}

bool
is_emoji_name (string s) {
  // s is one symbol, the name of an emoji
  if (!starts (s, "<emoji-")) return false;
  int end= 0;
  tm_char_forwards (s, end);
  return end == N(s) && ends (s, ">") && cork_to_utf8 (s) != s;
}

string
emoji_code_points (string s) {
  // the code points of the emoji named s (<#1F600> for <emoji-grinning-face>,
  // the characters below 128 as themselves); "" if s is no emoji
  if (!is_emoji_name (s)) return "";
  string u= cork_to_utf8 (s), r;
  int i= 0, n= N(u);
  while (i < n) {
    unsigned int code= decode_from_utf8 (u, i);
    if (code < 128) r << ((char) code);
    else r << "<#" << as_hexadecimal (code) << ">";
  }
  return r;
}

string
emoji_to_code_points (string s) {
  // the emoji of s written as code points
  if (search_forwards ("<emoji-", s) < 0) return s;
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    if (is_emoji_token (s, i)) {
      int j= i;
      tm_char_forwards (s, j);
      string c= emoji_code_points (s (i, j));
      if (c != "") { r << c; i= j; continue; }
    }
    r << s[i++];
  }
  return r;
}

string
emoji_to_code_points (string s, array<int>& at) {
  // as above, and at[j] is the position in s of the position j of the
  // result, -1 inside the code points of a name (N(at) is one more than
  // the length of the result)
  at= array<int> ();
  string r;
  int i= 0, n= N(s);
  while (i < n) {
    if (is_emoji_token (s, i)) {
      int j= i;
      tm_char_forwards (s, j);
      string c= emoji_code_points (s (i, j));
      if (c != "") {
        at << i;
        for (int k= 1; k < N(c); k++) at << -1;
        r << c;
        i= j;
        continue;
      }
    }
    at << i;
    r << s[i++];
  }
  at << n;
  return r;
}

string
emoji_name (string s) {
  // the name of the emoji which s is, by its name, by its code points, or
  // partly by names (<emoji-man><#200D><emoji-woman>...); "" if s is not
  // one emoji, or a character with a symbol of its own (<aries>); a
  // variation selector after the emoji is that of the emoji
  if (is_emoji_name (s)) return s;
  string u= cork_to_utf8 (s), name;
  if (N(u) == 0) return "";
  int i= 0;
  converter conv = load_converter ("UTF-8", "Emoji");
  converter cconv= load_converter ("UTF-8", "Cork");
  if (!match_emoji_name (conv, cconv, u, i, name)) return "";
  while (i < N(u)) {
    unsigned int code= decode_from_utf8 (u, i);
    if (code != 0xfe0e && code != 0xfe0f) return "";
  }
  return name;
}

bool
has_emoji (string s) {
  // whether s has an emoji, by its name or by its code points
  if (search_forwards ("<emoji-", s) >= 0) return true;
  if (search_forwards ("<#", s) < 0) return false;
  return search_forwards ("<emoji-", utf8_to_cork (cork_to_utf8 (s))) >= 0;
}

string
emoji_names (string s) {
  // s with the emoji in code points, or partly by names, written by the
  // names of the whole sequences (<emoji-man><#200D><emoji-woman><#200D>
  // <emoji-girl> is <emoji-family-man-woman-girl>); the rest as it is
  if (!has_emoji (s)) return s;
  string u= emoji_to_code_points (s), utf;
  array<string> toks;
  array<int> at;
  for (int i= 0; i < N(u); ) {
    int j= i;
    tm_char_forwards (u, j);
    at << N(utf);
    toks << u (i, j);
    utf << cork_to_utf8 (u (i, j));
    i= j;
  }
  at << N(utf);
  converter conv = load_converter ("UTF-8", "Emoji");
  converter cconv= load_converter ("UTF-8", "Cork");
  string r;
  int k= 0;
  while (k < N(toks)) {
    int pos= at[k];
    string name;
    if (match_emoji_name (conv, cconv, utf, pos, name)) {
      int l= k;
      while (l < N(toks) && at[l] < pos) l++;
      if (at[l] == pos) { r << name; k= l; continue; }
    }
    r << toks[k++];
  }
  return r;
}

int
emoji_match (string s, int pos, string w) {
  // the end of w at pos in s, the emoji of both compared in code points
  // (<emoji-grinning-face> matches <#1F600>); -1 if w is not there
  string cw= emoji_to_code_points (w), acc;
  if (pos >= N(s) || (s[pos] != '<' && s[pos] != cw[0])) return -1;
  int i= pos;
  while (i < N(s) && N(acc) < N(cw)) {
    int j= i;
    tm_char_forwards (s, j);
    acc << emoji_to_code_points (s (i, j));
    i= j;
    if (!starts (cw, acc)) return -1;
  }
  return acc == cw? i: -1;
}

string
emoji_from_latex (string name) {
  // the emoji of \emoji{name} of the LaTeX package emoji: by its name,
  // which is ours, or by one of its other names (smile, +1, tada, the old
  // names of CLDR); "" if none
  string s= "<emoji-" * name * ">", val;
  if (is_emoji_name (s)) return s;
  converter conv= load_converter ("LaTeX-emoji", "Emoji");
  if (dictionary_value (conv, name, val)) {
    s= "<emoji-" * val * ">";
    if (is_emoji_name (s)) return s;
  }
  return "";
}

/******************************************************************************
* Conversions between UTF-8 and the universal encoding
******************************************************************************/

string
utf8_to_cork (string input) {
  converter conv= load_converter ("UTF-8", "Cork");
  converter econv= load_converter ("UTF-8", "Emoji");
  int start, i, n= N(input);
  string output;
  for (i=0; i<n; ) {
    start= i;
    string name;
    if (match_emoji_name (econv, conv, input, i, name)) {
      output << name;
      continue;
    }
    unsigned int code= decode_from_utf8 (input, i);
    string s= input (start, i);
    string r= apply (conv, s);
    if (r == s && (code >= 256 || (code >= 128 && N(s) > 1)))
      r= "<#" * as_hexadecimal (code) * ">";
    output << r;
  }
  return output;
}

string
var_utf8_to_cork (string input) {
  converter conv= load_converter ("UTF-8", "Cork");
  converter econv= load_converter ("UTF-8", "Emoji");
  int start, i, n= N(input);
  string output;
  for (i=0; i<n; ) {
    start= i;
    string name;
    if (match_emoji_name (econv, conv, input, i, name)) {
      output << name;
      continue;
    }
    unsigned int code= decode_from_utf8 (input, i);
    string s= input (start, i);
    string r= apply (conv, s);
    if ((r == s && (code >= 256 || (code >= 128 && N(s) > 1))) ||
        (code >= 0x2018 && code <= 0x201D))
      r= "<#" * as_hexadecimal (code) * ">";
    output << r;
  }
  return output;
}

string
sourcecode_to_cork (string input) {
  converter conv= load_converter ("SourceCode", "Cork");
  int start, i, n= N(input);
  string output;
  for (i=0; i<n; ) {
    start= i;
    unsigned int code= decode_from_utf8 (input, i);
    string s= input (start, i);
    string r= apply (conv, s);
    if (r == s && (code >= 256 || (code >= 128 && N(s) > 1)))
      r= "<#" * as_hexadecimal (code) * ">";
    output << r;
  }
  return output;
}

string
cork_to_utf8 (string input) {
  converter conv= load_converter ("Cork", "UTF-8");
  int start= 0, i, n= N(input);
  string r;
  for (i=0; i<n; i++)
    if (input[i] == '<' && i+1<n && input[i+1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i+2;
      while (i<n && input[i] != '>') i++;
      if (i >= n || i == start) { start -= 2; i= start + 1; continue; }
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i+1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
strict_cork_to_utf8 (string input) {
  converter conv= load_converter ("Strict-Cork", "UTF-8");
  int start= 0, i, n= N(input);
  string r;
  for (i=0; i<n; i++)
    if (input[i] == '<' && i+1<n && input[i+1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i+2;
      while (i<n && input[i] != '>') i++;
      if (i >= n || i == start) { start -= 2; i= start + 1; continue; }
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i+1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
cork_to_sourcecode (string input) {
  converter conv= load_converter ("Cork", "SourceCode");
  int start= 0, i, n= N(input);
  string r;
  for (i=0; i<n; i++)
    if (input[i] == '<' && i+1<n && input[i+1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i+2;
      while (i<n && input[i] != '>') i++;
      if (i >= n || i == start) { start -= 2; i= start + 1; continue; }
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i+1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
utf8_to_t2a (string input) {
  converter conv= load_converter ("UTF-8", "T2A");
  int start, i, n= N(input);
  string output;
  for (i=0; i<n; ) {
    start= i;
    unsigned int code= decode_from_utf8 (input, i);
    string s= input (start, i);
    string r= apply (conv, s);
    if (r == s && code >= 256)
      r= "<#" * as_hexadecimal (code) * ">";
    output << r;
  }
  return output;
}

string
cyrillic_subset_in_t2a_to_code_point (string input) {
  converter conv= load_converter ("T2A.CY", "CODEPOINT");
  string r, tmp;
  int i, j, n = N(input);

  for (i = 0 ; i < n ; i++) {
    if (input[i] == '<') {
      for (j = i + 1 ; j < n && input[j] != '>' ; j++) ;
      r << input(i, j+1);
      i = j;
    }
    else {
      tmp = apply (conv, input[i]);
      if (tmp == input[i]) r << tmp;
      else r << '<'*tmp*'>';
    }
  }
  return r;
}

string
code_point_to_cyrillic_subset_in_t2a (string input) {
  converter conv= load_converter ("CODEPOINT", "T2A.CY");
  string r, tmp;
  int i = 0, n = N(input);

  while (i < n) {
    int start= i;
    if (input[i] == '<') {
      i++;
      while (i < n && input[i] != '>') i++;
    }
    i++;
    string c= input (start, i);
    string s= apply (conv, c);
    // a code point which the converter left alone is not in T2A, and
    // must not be taken for the T2A byte of the same number
    if (s != c && N(s) == 5 && s[0] == '<' && s[1] == '#' && s[4] == '>')
      r << string ((char) from_hexadecimal (s (2, 4)));
    else r << s;
  }
  return r;
}

string
t2a_to_utf8 (string input) {
  converter conv= load_converter ("T2A", "UTF-8");
  int start= 0, i, n= N(input);
  string r;
  for (i=0; i<n; i++)
    if (input[i] == '<' && i+1<n && input[i+1] == '#') {
      r << apply (conv, input (start, i));
      start= i= i+2;
      while (i<n && input[i] != '>') i++;
      if (i >= n || i == start) { start -= 2; i= start + 1; continue; }
      r << encode_as_utf8 (from_hexadecimal (input (start, i)));
      start= i+1;
    }
  r << apply (conv, input (start, n));
  return r;
}

string
utf8_to_html (string input) {
  converter conv = load_converter ("UTF-8", "HTML");
  string s = apply (conv, input);
  return utf8_to_hex_entities(s);
}

string
html_to_utf8 (string input) {
  converter conv = load_converter ("HTML", "UTF-8");
  string s = apply (conv, input);
  return hex_entities_to_utf8 (s);
}

string
cork_to_ascii (string input) {
  converter conv = load_converter ("Cork", "ASCII");
  return apply (conv, input);
}


#ifdef USE_ICONV

class iconv_converter {
  string from;
  string to;
  iconv_t cd;
  bool show_errors;
  bool successful;
public:
  iconv_converter (string from, string to, bool errors=true);
  ~iconv_converter ();
  inline bool is_valid () { return cd != (iconv_t)-1; }
  inline bool is_successful () { return successful; }
  friend string apply (iconv_converter &conv, string input);
};

iconv_converter::iconv_converter (string from2, string to2, bool errors):
  from (from2), to (to2), show_errors (errors), successful (false)
{
  c_string from_cp (from);
  c_string to_cp   (to);
  cd= iconv_open (to_cp, from_cp);
  if (!is_valid() && show_errors)
    convert_error << "Initialization of iconv from " << from
                  << " to " << to << " failed\n";
  successful= true;
}

iconv_converter::~iconv_converter () {
  if (is_valid())
    iconv_close(cd);
}

// From the standard C++ library (remember, TeXmacs does _not_ use std!)
template<typename T>
inline size_t
iconv_adaptor(size_t(*iconv_func)(iconv_t, T, size_t *, char**, size_t*),
              iconv_t cd, char **inbuf, size_t *inbytesleft,
              char **outbuf, size_t *outbytesleft) {
  return iconv_func (cd, (T) ((void*) inbuf), inbytesleft,
                     outbuf, outbytesleft);
}

static int
iconv_convert_all (iconv_t cd, char* in, size_t in_left, string& out) {
  // convert all of in with cd, from its initial state; 0 or the error
  char buf[64];
  iconv (cd, NULL, NULL, NULL, NULL);
  while (in_left > 0) {
    char* out_cursor= buf;
    size_t out_left= sizeof (buf);
    size_t r= iconv_adaptor (iconv, cd, &in, &in_left, &out_cursor, &out_left);
    int err= errno;
    out << string (buf, out_cursor - buf);
    if (r == (size_t) -1 && err != E2BIG) return err;
  }
  return 0;
}

static string
iconv_replacement (string to) {
  // the character which replaces those which cannot be converted: U+FFFD
  // in the target encoding, or '?' if it has no U+FFFD (Latin-1, ASCII...)
  c_string to_cp (to);
  iconv_t cd= iconv_open (to_cp, "UTF-8");
  if (cd == (iconv_t) -1) return "?";
  string r;
  char fffd[]= "\xEF\xBF\xBD";
  char qm[]= "?";
  if (iconv_convert_all (cd, fffd, 3, r) != 0) {
    r= "";
    if (iconv_convert_all (cd, qm, 1, r) != 0) r= "?";
  }
  iconv_close (cd);
  return r;
}

static size_t
iconv_char_length (iconv_t cd, char* in, size_t in_left) {
  // the number of bytes of the character at in, if it is a character of
  // the source encoding (which the target encoding does not have), and 1
  // if it is not; cd converts from the source encoding to UTF-8
  if (cd == (iconv_t) -1) return 1;
  for (size_t k=1; k <= 8 && k <= in_left; k++) {
    string out;
    int err= iconv_convert_all (cd, in, k, out);
    if (err == 0) return k;
    if (err != EINVAL) break; // not an incomplete character
  }
  return 1;
}

string apply (iconv_converter &conv, string input) {
  // A character which cannot be converted, because it is not one of the
  // source encoding or the target encoding does not have it, is replaced
  // by iconv_replacement and the conversion goes on after it.
  if (! conv.is_valid()) {
    conv.successful= false;
    convert_error << "Conversion cancelled\n";
    return input;
  }
  string result;
  c_string in_cp (input);
  char* in_cursor= in_cp;
  size_t in_left= N(input);
  double expansion= 1.1;
  size_t out_counter= 0;
  bool failed= false;
  string replacement;
  iconv_t probe= (iconv_t) -1;
  while (in_left > 0) {
    size_t out_left= max (int (in_left * expansion), 1024);
    c_string out_cp (out_left);
    char* out_cursor= out_cp;
    size_t r = iconv_adaptor(iconv, conv.cd,
                             &in_cursor, &in_left, &out_cursor, &out_left);
    int err= errno;
    size_t used_out= out_cursor - out_cp;
    result << string(out_cp, used_out);
    out_counter += used_out;
    if (r == (size_t)-1 && err != E2BIG) {
      if (!failed) {
        if (conv.show_errors)
          convert_error << "Iconv conversion from " << conv.from
                        << " to " << conv.to << " failed\n";
        replacement= iconv_replacement (conv.to);
        c_string from_cp (conv.from);
        probe= iconv_open ("UTF-8", from_cp);
        failed= true;
      }
      // an incomplete character at the end (EINVAL) is replaced as a whole
      size_t skip= (err == EINVAL)? in_left:
                   iconv_char_length (probe, in_cursor, in_left);
      result << replacement;
      in_cursor += skip;
      in_left   -= skip;
    }
    expansion= max ((double) out_counter / (in_cursor - in_cp), 1.0) + 0.1;
  }
  if (probe != (iconv_t) -1) iconv_close (probe);
  conv.successful= !failed;
  return result;
}

#endif // defined USE_ICONV

bool check_using_iconv (string input, string encoding) {
#ifdef USE_ICONV
  iconv_converter conv (encoding, encoding, false);
  apply (conv, input);
  return conv.is_successful();
#else
  (void) input;
  (void) encoding;
  FAILED ("iconv not enabled");
  return false;
#endif
}

string
convert_using_iconv (string input, string from, string to) {
  if (from == to) return input;
#ifdef USE_ICONV
  iconv_converter conv (from, to, true);
  return apply (conv, input);
#else
  (void) input;
  (void) from;
  (void) to;
  FAILED ("iconv not enabled");
  return input;
#endif
}

/******************************************************************************
* Functions for hashtree handling
******************************************************************************/

void
put_prefix_code (string key, string value, hashtree<char,string> tree) {
  /*
  if (DEBUG_CONVERT) {
    hashtree<char,string> ht= find_node (key,tree);
    if (ht->label != "")
      debug_convert << "overwriting " << ht->label << " with " << value << '\n';
  }
  */
  find_node (key,tree)->set_label(value);
}

hashtree<char,string>
find_node (string key, hashtree<char,string> ht) {
  int i;
  for(i = 0; i < N(key); i++)
    ht = ht(key[i]);
  return ht;
}

static string
str_unquote (string s) {
  int start, end;
  for (start=0; start<N(s) && s[start] != '"'; start++) ;
  for (end=N(s)-1; end > start && s[end] != '"'; end--) ;
  return s (start+1, end);
}

void
hashtree_from_dictionary (
  hashtree<char,string> dic, string file_name, escape_type key_escape,
  escape_type val_escape, bool reverse)
{
  if (DEBUG_CONVERT) debug_convert << "Loading dictionary " << file_name << LF;
  string key_string, val_string, file;
  file_name = file_name * ".scm";
  if (load_string (url ("$TEXMACS_PATH/langs/encoding", file_name),
                   file, false)) {
    convert_error << "Couldn't open encoding dictionary " << file_name << LF;
    return;
  }
  tree t = block_to_scheme_tree (file);
  if (!is_tuple (t)) {
    convert_error << "Malformed encoding dictionary " << file_name << LF;
    return;
  }
  for (int i=0; i<N(t); i++) {
    if (is_func (t[i], TUPLE, 2) &&
        is_atomic (t[i][0]) && is_atomic (t[i][1]))
      {
        //cout << N(pairs[i]) << "\n" << as_string(pairs[i]) << "\n";
        reverse ? key_string = t[i][1]->label : key_string = t[i][0]->label;
        reverse ? val_string = t[i][0]->label : val_string = t[i][1]->label;
        if (is_quoted (key_string)) key_string = str_unquote (key_string);
        if (is_quoted (val_string)) val_string = str_unquote (val_string);
        if (key_escape == BIT2BIT)
          key_string = convert_escapes (key_string, false);
        else if (key_escape == UTF8)
          key_string = convert_escapes (key_string, true);
        else if (key_escape == CHAR_ENTITY)
          key_string = convert_char_entities (key_string);
        else if (key_escape == ENTITY_NAME)
          key_string = "&" * key_string * ";";
        if (val_escape == BIT2BIT)
          val_string = convert_escapes (val_string, false);
        else if (val_escape == UTF8)
          val_string = convert_escapes (val_string, true);
        else if (val_escape == CHAR_ENTITY)
          val_string = convert_char_entities (val_string);
        else if (val_escape == ENTITY_NAME)
          val_string = "&" * val_string * ";";
        //cout << "key: " << key_string << " val: " << val_string << "\n";
        put_prefix_code(key_string,val_string,dic);        
      }
  }
}

/***************************************************************************
* Functions for UTF-8 handling
* These functions are helper functions to convert escape string a la "#23F7"
* and HTML/XML character entities to and from UTF-8 byte sequences.
***************************************************************************/

int hex_digit_to_int(unsigned char c) {
  if (48 <= c && c <= 57)
    return c - 0x30;
  else if (65 <= c && c <= 70)
    return c - 0x41 + 0x0A;
  else if (97 <= c && c <= 102)
    return c - 0x61 + 0x0A;
  else
    return 0;
}

string
convert_escapes (string in, bool utf8) {
  // cout << "converting " << in ;
  string result;
  int i = 0;
  while (i < N(in)) {
    if (in[i]!='#') result << in[i++];
    else {
      i++;
      unsigned int num = 0;
      while (i < N(in) && is_hex_digit(in[i]))
        num = 0x10 * num + hex_digit_to_int((unsigned char) in[i++]);
      //cout << " to num "; printf("%x",num); cout << " then to ";
      if (utf8) result << encode_as_utf8 (num);
      else result << string((char)num);
    }
  }
  //for(int i = 0; i < N(result);i++)
  //  printf("%x ", (unsigned char)result[i]); printf("\n");
  return result;
}

string
convert_char_entities (string s) {
  int i, n=N(s);
  string r;
  for (i=0; i<n; /* noop */) {
    if (s[i] == '&' && i+1<n && s[i+1] == '#') {
      i += 2;
      bool okay= false;
      string rr= convert_char_entity(s, i, okay);
      if (okay) r << rr;
      else { r << "&#"; continue; }
    }
    else r << s[i++];
  }
  return r;
}

static unsigned int
as_unsigned_int (string s) {
  int i=0, n=N(s);
  unsigned int val=0;
  if (n==0) return 0;
  while (i<n) {
    if (s[i]<'0') break;
    if (s[i]>'9') break;
    val *= 10;
    val += (int) (s[i]-'0');
    i++;
  }
  return val;
}

string
convert_char_entity (string s, int& start, bool& success) {
  // start: position in s after the character entity marker "&#".
  success = false;
  int i= start;
  int n= N(s);
  unsigned int num= 0;
  if (i >= n) return "";
  else if (s[i] == 'x' || s[i] == 'X') {
    i++;
    // int j=i;
    while (i<n && is_hex_digit (s[i])) {
      success = true;
      num = 0x10 * num + hex_digit_to_int(s[i]);
      i++;
    }
    // if (success) cout << "hex-ent: " << s(j,i) ;
  }
  else {
    int j=i;
    while (i<n && is_digit (s[i])) {
      success = true;
      i++;
    }
    // if (success) cout << "dec-ent: " << s(j,i) ;
    num = as_unsigned_int (s(j,i));
  }
  if (success) {
    if (i<n && s[i]==';') i++;
    start= i;
    // cout << " --> (" << num << ") " << encode_as_utf8 (num) << '\n' ;
    return encode_as_utf8(num);
  }
  else return "";
}

string
encode_as_utf8 (unsigned int code) {
  if (/* 0x0 <= code && */ code <= 0x7F) {
    // 0x0ddddddd
    return string((char) code);
  }
  else if (0x80 <= code  && code <= 0x7FF) {
    // 0x110ddddd 0x10dddddd
    string str(2);
    str[0] = ((code >> 6) & 0x1F) | 0xC0;
    str[1] = (code & 0x3F) | 0x80;
    return str;
  } 
  else if (0x800 <= code && code <= 0xFFFF) {
    // 0x1110dddd 0x10dddddd 0x10dddddd
    string str(3);
    str[0] = ((code >> 12) & 0x0F) | 0xE0;
    str[1] = ((code >> 6) & 0x3F) | 0x80;
    str[2] = (code & 0x3F) | 0x80;
    return str;
  }
  else if (0x10000 <= code && code <= 0x1FFFFF) {
    // 0x11110uuu 0x10zzzzzz 0x10yyyyyy 0x10xxxxxx
    string str(4);
    str[0] = ((code >> 18) & 0x07) | 0xF0;
    str[1] = ((code >> 12) & 0x3F) | 0x80;
    str[2] = ((code >> 6) & 0x3F) | 0x80;
    str[3] = (code & 0x3F) | 0x80;
    return str;
  }
  else return "";
}

unsigned int
decode_from_utf8 (string s, int& i) {
  unsigned char c = s[i];
  if ((0x80 & c) == 0) {
    // 0x0ddddddd
    i++;
    return (unsigned int) c;
  }
  unsigned int code;
  int trail;
  if ((0xE0 & c) == 0xC0) {
    // 0x110ddddd 0x10dddddd
    trail = 1;
    code = c & 0x1F;
  }
  else if ((0xF0 & c) == 0xE0) {
    // 0x1110dddd 0x10dddddd 0x10dddddd
    trail = 2;
    code = c & 0x0F;
  }
  else if ((0xF8 & c) == 0xF0) {
    // 0x11110dddd 0x10dddddd 0x10dddddd 0x10dddddd
    trail = 3;
    code = c & 0x07;
  }
  else {
    // failsafe
    //cout << "failsafe: " << c << " (" << (unsigned int)(c) << ")\n";
    i++;
    return (unsigned int) c;
  }
  int start= i-1;
  for (; trail > 0; trail--) {
    i++;
    if (i >= N(s)) {
      // truncated at the end of the string: as a sequence broken earlier
      i= start+1;
      c= s[i++];
      return c;
    }
    c = s[i];
    if ((0xC0 & c) == 0x80)
      code = (code << 6) | (c & 0x3F);
    else {
      i= start+1;
      c= s[i++];
      return c;
    }
  }
  i++;
  return code;
}

string
utf8_to_hex_entities (string s) {
  string result;
  int i, n= N(s);
  for (i=0; i<n; ) {
    unsigned char c = s[i];
    if ((0x80 & c) == 0 || ((0xF8 & c) == 0xF8)) {
      result << c;
      i++;
    }
    else {
      unsigned int code= decode_from_utf8 (s, i);
      string hex= as_hexadecimal (code);
      while (N(hex) < 4) hex = "0" * hex;
      //cout << "entity: " << hex << " (" << code << ")\n";
      result << "&#x" << hex << ";";
    }
  }
  return result;
}

string
hex_entities_to_utf8 (string s) {
  string result;
  int i, n= N(s);
  for (i=0; i<n; )
    if (test (s, i, "&#x")) {
      int j= i+3;
      while (j < n && j < i+9 && is_hex_digit (s[j])) j++;
      if (j > i+3 && j < n && s[j] == ';') {
        unsigned int code= from_hexadecimal (s (i+3, j));
        if (code >= 0x80 && code <= 0x10FFFF &&
            (code < 0xD800 || code > 0xDFFF)) {
          result << encode_as_utf8 (code);
          i= j + 1;
        }
        else result << s[i++];
      }
      else result << s[i++];
    }
    else result << s[i++];
  return result;
}

string
utf8_to_utf16be_string (string s) {
  string result, hex;
  int i, n= N(s);
  for (i=0; i<n; ) {
    unsigned int code= decode_from_utf8 (s, i);
    // see e.g. https://en.wikipedia.org/wiki/UTF-16
    if (code >= 0x10000) {
      // supplementary planes
      unsigned int code2= code - 0x10000;
      unsigned int w1= 0xD800 + (code2 >> 10);
      unsigned int w2= 0xDC00 + (code2 & 0x3FF);
      hex= as_hexadecimal (w1);
      while (N(hex) < 4) hex = "0" * hex;
      result << hex;
      hex= as_hexadecimal (w2);
      while (N(hex) < 4) hex = "0" * hex;
      result << hex;
    } else {
      // basic planes
      string hex= as_hexadecimal (code);
      while (N(hex) < 4) hex = "0" * hex;
      result << hex;
    }
  }
  return result;
}

string
utf8_to_pdf_hex_string (string s) {
  return "<FEFF" * utf8_to_utf16be_string (cork_to_utf8 (s)) * ">";
}
