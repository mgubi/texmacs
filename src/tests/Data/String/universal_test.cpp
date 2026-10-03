/******************************************************************************
* MODULE     : universal_test.cpp
* DESCRIPTION: tests of UTF-8, the Cork encoding and the universal characters
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// TeXmacs strings are in the Cork encoding, extended by symbols <name> and
// by <#xxxx> for any other Unicode code point. utf8_to_cork and cork_to_utf8
// convert using the dictionaries of TeXmacs/langs/encoding, which the test
// harness finds through TEXMACS_PATH. universal.cpp implements the changes
// of case and the removal of accents on such strings.

#include "tm_test.hpp"
#include "universal.hpp"
#include "converter.hpp"

/******************************************************************************
* Helpers
******************************************************************************/

// an independent UTF-8 encoder, after RFC 3629
static string
reference_utf8 (unsigned int c) {
  string r;
  if (c < 0x80) r << (char) c;
  else if (c < 0x800) {
    r << (char) (0xC0 | (c >> 6));
    r << (char) (0x80 | (c & 0x3F));
  }
  else if (c < 0x10000) {
    r << (char) (0xE0 | (c >> 12));
    r << (char) (0x80 | ((c >> 6) & 0x3F));
    r << (char) (0x80 | (c & 0x3F));
  }
  else {
    r << (char) (0xF0 | (c >> 18));
    r << (char) (0x80 | ((c >> 12) & 0x3F));
    r << (char) (0x80 | ((c >> 6) & 0x3F));
    r << (char) (0x80 | (c & 0x3F));
  }
  return r;
}

// shows the bytes of a string, so that failures stay readable
static string
bytes (string s) {
  string r;
  for (int i=0; i<N(s); i++) {
    unsigned char c= s[i];
    if (c < 32 || c > 126) r << "\\x" << as_hexadecimal (c, 2);
    else r << s[i];
  }
  return r;
}

static string
cork_byte (int c) {
  string r (1); r[0]= (char) c;
  return r;
}

#define CHECK_BYTES(x, y) CHECK_EQ (bytes (x), bytes (y))

/******************************************************************************
* encode_as_utf8 and decode_from_utf8
******************************************************************************/

// every code point is encoded as RFC 3629 says and decoded back
static void
test_utf8_all_code_points () {
  int bad= 0;
  for (unsigned int c=0; c<=0x10FFFF; c++) {
    string s= encode_as_utf8 (c);
    int i= 0;
    unsigned int d= decode_from_utf8 (s, i);
    if (s != reference_utf8 (c) || d != c || i != N(s))
      if (bad++ < 5)
        CHECK_MSG (false, "code point " * as_hexadecimal ((int) c) *
                   " is encoded as " * bytes (s));
  }
  CHECK_EQ (bad, 0);
}

// the boundaries between the lengths of the encodings
static void
test_utf8_boundaries () {
  CHECK_BYTES (encode_as_utf8 (0x0), string ("\0", 1));
  CHECK_BYTES (encode_as_utf8 (0x7F), "\x7f");
  CHECK_BYTES (encode_as_utf8 (0x80), "\xc2\x80");
  CHECK_BYTES (encode_as_utf8 (0x7FF), "\xdf\xbf");
  CHECK_BYTES (encode_as_utf8 (0x800), "\xe0\xa0\x80");
  CHECK_BYTES (encode_as_utf8 (0xFFFF), "\xef\xbf\xbf");
  CHECK_BYTES (encode_as_utf8 (0x10000), "\xf0\x90\x80\x80");
  CHECK_BYTES (encode_as_utf8 (0x10FFFF), "\xf4\x8f\xbf\xbf");
  // four bytes encode up to 0x1FFFFF, which the encoder allows; beyond
  // that nothing is produced
  CHECK_BYTES (encode_as_utf8 (0x1FFFFF), "\xf7\xbf\xbf\xbf");
  CHECK_BYTES (encode_as_utf8 (0x200000), "");
  CHECK_BYTES (encode_as_utf8 (0xFFFFFFFF), "");
}

// decoding a string of several characters advances character by character
static void
test_utf8_decode_sequence () {
  string s= "a\xc3\xa9\xce\xb1\xe4\xb8\xad\xf0\x9f\x98\x80z";
  unsigned int expected[]= { 0x61, 0xE9, 0x3B1, 0x4E2D, 0x1F600, 0x7A };
  int ends[]= { 1, 3, 5, 8, 12, 13 };
  int i= 0;
  for (int k=0; k<6; k++) {
    CHECK_EQ (decode_from_utf8 (s, i), expected[k]);
    CHECK_EQ (i, ends[k]);
  }
}

// a byte which does not start a valid sequence is returned as it is, as if
// the string were in Latin-1, and the decoder advances by one byte
static void
test_utf8_invalid () {
  const char* bad[]= {
    "\x80",           // lone continuation byte
    "\xbf",
    "\xc3",           // truncated two byte sequence
    "\xc3x",          // lead byte followed by an ASCII character
    "\xe2\x82x",      // three byte sequence broken after two bytes
    "\xf0\x9f\x98x",  // four byte sequence broken after three bytes
    "\xf8\x88\x80\x80\x80", // five byte sequences are not decoded
    "\xfe",
    "\xff",
    // FIXME: a sequence truncated at the end of the string, such as
    // "\xe2\x82", is decoded by reading its last byte twice (U+2082)
    0 };
  for (int k=0; bad[k] != 0; k++) {
    string s= bad[k];
    int i= 0;
    unsigned int c= decode_from_utf8 (s, i);
    CHECK_MSG (c == (unsigned int) (unsigned char) s[0] && i == 1,
               "decoding of " * bytes (s) * " gives " *
               as_hexadecimal ((int) c) * " up to " * as_string (i));
  }
  // the decoder does not reject overlong forms or surrogates
  int i= 0;
  CHECK_EQ (decode_from_utf8 ("\xc0\xaf", i), (unsigned int) 0x2F);
  i= 0;
  CHECK_EQ (decode_from_utf8 ("\xed\xa0\x80", i), (unsigned int) 0xD800);
}

/******************************************************************************
* UTF-8 to Cork and back
******************************************************************************/

// known conversions: Cork bytes, symbols and the generic escape
static void
test_utf8_to_cork_known () {
  CHECK_BYTES (utf8_to_cork (""), "");
  CHECK_BYTES (utf8_to_cork ("Hello, world!"), "Hello, world!");
  CHECK_BYTES (utf8_to_cork ("a<b>c"), "a<less>b<gtr>c");
  CHECK_BYTES (utf8_to_cork ("\xc3\xa9"), "\xe9");        // e acute
  CHECK_BYTES (utf8_to_cork ("\xc3\x9f"), "\xff");        // sharp s
  CHECK_BYTES (utf8_to_cork ("\xc5\x81"), "\x8a");        // L with stroke
  CHECK_BYTES (utf8_to_cork ("\xc2\xa7"), "\x9f");        // section sign
  CHECK_BYTES (utf8_to_cork ("\xc2\xbf"), "\xbe");        // inverted question
  CHECK_BYTES (utf8_to_cork ("\xe2\x80\x9c"), "\x10");    // left double quote
  CHECK_BYTES (utf8_to_cork ("\xe2\x80\x9d"), "\x11");    // right double quote
  CHECK_BYTES (utf8_to_cork ("\xe2\x80\x98"), "`");       // left single quote
  CHECK_BYTES (utf8_to_cork ("`"), string ("\0", 1));     // grave accent
  CHECK_BYTES (utf8_to_cork ("\xce\xb1"), "<alpha>");
  CHECK_BYTES (utf8_to_cork ("\xce\x91"), "<Alpha>");
  CHECK_BYTES (utf8_to_cork ("\xe2\x88\x9e"), "<infty>");
  CHECK_BYTES (utf8_to_cork ("\xc3\x97"), "<times>");
  CHECK_BYTES (utf8_to_cork ("\xc2\xa0"), "<varspace>");  // no-break space
  CHECK_BYTES (utf8_to_cork ("\xd0\x96"), "<#416>");      // Cyrillic Zhe
  CHECK_BYTES (utf8_to_cork ("\xe4\xb8\xad"), "<#4E2D>"); // CJK
  CHECK_BYTES (utf8_to_cork ("\xe2\x82\xac"), "<#20AC>"); // euro sign
  CHECK_BYTES (utf8_to_cork ("\xf0\x9f\x98\x80"), "<#1F600>");
  CHECK_BYTES (utf8_to_cork ("\xf0\x9d\x90\x80"), "<b-up-A>");
  CHECK_BYTES (utf8_to_cork ("x\xce\xb1+\xe4\xb8\xad=\xc3\xa9"),
               "x<alpha>+<#4E2D>=\xe9");
}

// invalid UTF-8 bytes are kept as they are
static void
test_utf8_to_cork_invalid () {
  CHECK_BYTES (utf8_to_cork ("\xc3x"), "\xc3x");
  CHECK_BYTES (utf8_to_cork ("a\x80z"), "a\x80z");
  CHECK_BYTES (utf8_to_cork ("\xff\xfe"), "\xff\xfe");
}

// known conversions back to UTF-8, including lower case hexadecimal
// digits in the escapes
static void
test_cork_to_utf8_known () {
  CHECK_BYTES (cork_to_utf8 (""), "");
  CHECK_BYTES (cork_to_utf8 ("plain text"), "plain text");
  CHECK_BYTES (cork_to_utf8 ("<less><gtr>"), "<>");
  CHECK_BYTES (cork_to_utf8 ("<alpha><Alpha>"), "\xce\xb1\xce\x91");
  CHECK_BYTES (cork_to_utf8 ("<varepsilon>"), "\xce\xb5");
  CHECK_BYTES (cork_to_utf8 ("<infty>"), "\xe2\x88\x9e");
  CHECK_BYTES (cork_to_utf8 ("<#4E2D>"), "\xe4\xb8\xad");
  CHECK_BYTES (cork_to_utf8 ("<#4e2d>"), "\xe4\xb8\xad");
  CHECK_BYTES (cork_to_utf8 ("<#3c>"), "<");
  CHECK_BYTES (cork_to_utf8 ("<#1F600>"), "\xf0\x9f\x98\x80");
  CHECK_BYTES (cork_to_utf8 ("\xe9"), "\xc3\xa9");
  CHECK_BYTES (cork_to_utf8 ("\x10\x11"), "\xe2\x80\x9c\xe2\x80\x9d");
  CHECK_BYTES (cork_to_utf8 ("`"), "\xe2\x80\x98");
  CHECK_BYTES (cork_to_utf8 ("\x7f"), "\xe2\x80\x90");   // hyphen
  // one way conversions of Cork
  CHECK_BYTES (cork_to_utf8 ("\xdf"), "SS");
  CHECK_BYTES (cork_to_utf8 ("\x1a"), "j");
  CHECK_BYTES (cork_to_utf8 ("\x18"), "0");
  // unknown symbols are left alone
  CHECK_BYTES (cork_to_utf8 ("<no-such-symbol>"), "<no-such-symbol>");
  CHECK_BYTES (cork_to_utf8 ("a<alpha>b<#4E2D>c"),
               "a\xce\xb1" "b\xe4\xb8\xad" "c");
  CHECK_BYTES (strict_cork_to_utf8 ("<alpha><#4E2D>\xe9"),
               "\xce\xb1\xe4\xb8\xad\xc3\xa9");
}

// round trips of code points in several scripts; for code points which
// have no symbol, utf8_to_cork must produce the generic <#xxxx> escape
static void
test_round_trip_scripts () {
  struct { unsigned int start, end; } ranges[]= {
    { 0x21, 0x3B }, { 0x3F, 0x5F }, { 0x61, 0x7E }, // ASCII but < > `
    { 0xC0, 0xFF },                                 // Latin-1 letters
    { 0x100, 0x17F },                               // Latin extended A
    { 0x391, 0x3A1 }, { 0x3A3, 0x3A9 },             // Greek capitals
    { 0x3B1, 0x3C9 },                               // Greek small
    { 0x400, 0x4FF },                               // Cyrillic
    { 0x5D0, 0x5EA },                               // Hebrew
    { 0x4E00, 0x4FFF }, { 0x9F00, 0x9FA5 },         // CJK
    { 0xAC00, 0xAD00 },                             // Hangul
    { 0x1F600, 0x1F64F },                           // emoticons
    { 0x20000, 0x20100 }                            // CJK extension B
  };
  int bad= 0;
  for (int r=0; r<15; r++)
    for (unsigned int c= ranges[r].start; c <= ranges[r].end; c++) {
      string u= encode_as_utf8 (c);
      string k= utf8_to_cork (u);
      bool ok= cork_to_utf8 (k) == u;
      if (starts (k, "<#"))
        ok= ok && k == "<#" * as_hexadecimal ((int) c) * ">";
      if (!ok && bad++ < 10)
        CHECK_MSG (false, "code point " * as_hexadecimal ((int) c) *
                   " becomes " * bytes (k) * " and " *
                   bytes (cork_to_utf8 (k)));
    }
  CHECK_EQ (bad, 0);
}

// a long mixed string survives the round trip
static void
test_round_trip_text () {
  string u;
  for (int k=0; k<50; k++)
    u << "Gr\xc3\xbc\xc3\x9f" "e \xce\xb1\xce\xb2 \xd0\x96 \xe4\xb8\xad "
      << "\xf0\x9f\x98\x80 \xe2\x80\x9cq\xe2\x80\x9d <x> & #1; ";
  CHECK (cork_to_utf8 (utf8_to_cork (u)) == u);
  // control characters are not text in Cork: 0x0A is the dot accent
  CHECK_BYTES (cork_to_utf8 (utf8_to_cork ("\n")), "\xcb\x99");
}

// Cork bytes come back from UTF-8, except for the one way conversions
static void
test_round_trip_cork_bytes () {
  int bad= 0;
  for (int c=1; c<256; c++) {
    if (c == 0x18 || c == 0x1A || c == 0xDF) continue;  // one way
    if (c == '<' || c == '>') continue;  // become <less> and <gtr>
    string k= cork_byte (c);
    string u= cork_to_utf8 (k);
    if (utf8_to_cork (u) != k && bad++ < 10)
      CHECK_MSG (false, "Cork byte " * as_hexadecimal (c, 2) * " becomes " *
                 bytes (u) * " and " * bytes (utf8_to_cork (u)));
  }
  CHECK_EQ (bad, 0);
}

/******************************************************************************
* Changes of case
******************************************************************************/

// ASCII and the Cork bytes, where the cases are 0x20 apart
static void
test_case_cork () {
  CHECK_EQ (uni_upcase_char ("a"), string ("A"));
  CHECK_EQ (uni_locase_char ("Z"), string ("z"));
  CHECK_EQ (uni_upcase_char ("1"), string ("1"));
  CHECK_EQ (uni_locase_char ("<"), string ("<"));
  for (int c=0x80; c<0xA0; c++) {
    CHECK_BYTES (uni_locase_char (cork_byte (c)), cork_byte (c+0x20));
    CHECK_BYTES (uni_upcase_char (cork_byte (c+0x20)), cork_byte (c));
  }
  for (int c=0xC0; c<0xE0; c++) {
    CHECK_BYTES (uni_locase_char (cork_byte (c)), cork_byte (c+0x20));
    CHECK_BYTES (uni_upcase_char (cork_byte (c+0x20)), cork_byte (c));
  }
  CHECK (is_uni_locase_char ("\xe9"));
  CHECK (!is_uni_locase_char ("\xc9"));
  CHECK (is_uni_upcase_char ("\xc9"));
}

// the case of the escapes <#xxxx> follows Unicode in the ranges handled
static void
test_case_escapes () {
  // Latin extended A
  CHECK_EQ (uni_locase_char ("<#100>"), string ("<#101>"));
  CHECK_EQ (uni_upcase_char ("<#101>"), string ("<#100>"));
  CHECK_EQ (uni_locase_char ("<#139>"), string ("<#13A>"));
  CHECK_EQ (uni_upcase_char ("<#13A>"), string ("<#139>"));
  CHECK_EQ (uni_locase_char ("<#14A>"), string ("<#14B>"));
  CHECK_EQ (uni_locase_char ("<#17D>"), string ("<#17E>"));
  CHECK_EQ (uni_upcase_char ("<#17E>"), string ("<#17D>"));
  CHECK_EQ (uni_upcase_char ("<#138>"), string ("<#138>"));  // kra
  // Greek
  CHECK_EQ (uni_locase_char ("<#391>"), string ("<#3B1>"));
  CHECK_EQ (uni_upcase_char ("<#3C9>"), string ("<#3A9>"));
  CHECK_EQ (uni_locase_char ("<#386>"), string ("<#3AC>"));
  CHECK_EQ (uni_upcase_char ("<#3AC>"), string ("<#386>"));
  CHECK_EQ (uni_locase_char ("<#38C>"), string ("<#3CC>"));
  CHECK_EQ (uni_upcase_char ("<#3CE>"), string ("<#38F>"));
  // Cyrillic
  CHECK_EQ (uni_locase_char ("<#401>"), string ("<#451>"));
  CHECK_EQ (uni_upcase_char ("<#451>"), string ("<#401>"));
  CHECK_EQ (uni_locase_char ("<#416>"), string ("<#436>"));
  CHECK_EQ (uni_upcase_char ("<#44F>"), string ("<#42F>"));
  CHECK_EQ (uni_locase_char ("<#460>"), string ("<#461>"));
  CHECK_EQ (uni_upcase_char ("<#461>"), string ("<#460>"));
  // FIXME: from 0x4C1 to 0x4CE the capitals are odd, which is not handled
  // lower case hexadecimal digits are understood, and the result is
  // written in upper case
  CHECK_EQ (uni_locase_char ("<#3a3>"), string ("<#3C3>"));
  // outside of the handled ranges nothing changes
  CHECK_EQ (uni_locase_char ("<#4E2D>"), string ("<#4E2D>"));
  CHECK_EQ (uni_upcase_char ("<#1F600>"), string ("<#1F600>"));
}

// the case of each letter pair round trips in the handled ranges
static void
test_case_pairs () {
  int bad= 0;
  for (int c=0x391; c<=0x3A9; c++) {
    // FIXME: 0x3A2 is unassigned, yet it is the upper case of 0x3C2
    if (c == 0x3A2) continue;
    string up= "<#" * as_hexadecimal (c) * ">";
    string lo= "<#" * as_hexadecimal (c + 0x20) * ">";
    if ((uni_locase_char (up) != lo || uni_upcase_char (lo) != up) && bad++ < 5)
      CHECK_MSG (false, "Greek pair " * up * " " * lo);
  }
  for (int c=0x400; c<=0x42F; c++) {
    string up= "<#" * as_hexadecimal (c) * ">";
    string lo= "<#" * as_hexadecimal (c < 0x410? c + 0x50: c + 0x20) * ">";
    if ((uni_locase_char (up) != lo || uni_upcase_char (lo) != up) && bad++ < 5)
      CHECK_MSG (false, "Cyrillic pair " * up * " " * lo);
  }
  CHECK_EQ (bad, 0);
}

// the symbols of the Greek letters
static void
test_case_greek_symbols () {
  CHECK_EQ (uni_upcase_char ("<alpha>"), string ("<Alpha>"));
  CHECK_EQ (uni_locase_char ("<Alpha>"), string ("<alpha>"));
  CHECK_EQ (uni_upcase_char ("<omega>"), string ("<Omega>"));
  CHECK_EQ (uni_upcase_char ("<varepsilon>"), string ("<Epsilon>"));
  CHECK_EQ (uni_upcase_char ("<varphi>"), string ("<Phi>"));
  CHECK_EQ (uni_locase_char ("<Gamma>"), string ("<gamma>"));
  CHECK_EQ (uni_upcase_char ("<infty>"), string ("<infty>"));
  CHECK_EQ (uni_locase_char ("<infty>"), string ("<infty>"));
}

// the functions on whole strings work character by character
static void
test_case_strings () {
  string s= "ab<alpha>\xe9<#3B2>z";
  CHECK_BYTES (uni_upcase_all (s), "AB<Alpha>\xc9<#392>Z");
  CHECK_BYTES (uni_locase_all ("AB<Alpha>\xc9<#392>Z"), s);
  CHECK_EQ (uni_upcase_first ("<alpha>beta"), string ("<Alpha>beta"));
  CHECK_EQ (uni_upcase_first ("hello World"), string ("Hello World"));
  CHECK_EQ (uni_locase_first ("HELLO"), string ("hELLO"));
  CHECK_EQ (uni_Locase_all ("HELLO <Alpha>"), string ("Hello <alpha>"));
  CHECK_EQ (uni_Locase_all ("<Alpha>BC"), string ("<Alpha>bc"));
  CHECK_EQ (uni_upcase_all (""), string (""));
  CHECK_EQ (uni_upcase_first (""), string (""));
  CHECK_EQ (uni_Locase_all (""), string (""));
}

/******************************************************************************
* Accents, transliteration and letters
******************************************************************************/

static void
test_accents () {
  CHECK_EQ (uni_unaccent_char ("\xe9"), string ("e"));       // e acute
  CHECK_EQ (uni_unaccent_char ("\xc0"), string ("A"));       // A grave
  CHECK_EQ (uni_unaccent_char ("\x80"), string ("A"));       // A breve
  CHECK_EQ (uni_unaccent_char ("\xa8"), string ("l"));       // l acute
  CHECK_EQ (uni_unaccent_char ("a"), string (""));
  CHECK_EQ (uni_unaccent_all ("caf\xe9 cr\xe8me"), string ("cafe creme"));
  CHECK_EQ (uni_unaccent_all ("<alpha>x"), string ("<alpha>x"));
  // the accents themselves, as Cork characters
  CHECK_BYTES (uni_get_accent_char ("\xe9"), "\x01");
  CHECK_BYTES (uni_get_accent_char ("\xe8"), string ("\0", 1));
  CHECK_BYTES (uni_get_accent_char ("\xe7"), "\x0b");
  array<string> l= get_accented_list ();
  CHECK (N(l) > 100);
  for (int i=0; i<N(l); i++)
    CHECK_MSG (uni_unaccent_char (l[i]) != "", "accented " * bytes (l[i]));
}

static void
test_translit () {
  CHECK_EQ (uni_translit ("<#416><#436>"), string ("ZHzh"));
  CHECK_EQ (uni_translit ("<#41a><#41A>"), string ("KK"));
  CHECK_EQ (uni_translit ("<#41C><#43E><#441><#43A><#432><#430>"),
            string ("Moskva"));
  CHECK_EQ (uni_translit ("caf\xe9"), string ("cafe"));
  CHECK_EQ (uni_translit ("<alpha><#4E2D>"), string ("<alpha><#4E2D>"));
}

static void
test_letters () {
  CHECK (uni_is_letter ("a"));
  CHECK (uni_is_letter ("Z"));
  CHECK (!uni_is_letter ("1"));
  CHECK (!uni_is_letter ("+"));
  CHECK (uni_is_letter ("\xe9"));
  // above 127 every Cork character is a letter, sharp s and the ligatures
  // included, but the section sign, the inverted marks and the pound sign
  CHECK (uni_is_letter ("\xff"));   // sharp s
  CHECK (uni_is_letter ("\xdf"));   // SS
  CHECK (uni_is_letter ("\xd7"));   // OE
  CHECK (uni_is_letter ("\xf7"));   // oe
  CHECK (uni_is_letter ("\x80"));   // A breve
  CHECK (!uni_is_letter ("\x9f"));  // section sign
  CHECK (!uni_is_letter ("\xbd"));  // inverted exclamation mark
  CHECK (!uni_is_letter ("\xbe"));  // inverted question mark
  CHECK (!uni_is_letter ("\xbf"));  // pound sign
  CHECK (uni_is_letter ("<alpha>"));
  CHECK (uni_is_letter ("<Omega>"));
  CHECK (uni_is_letter ("<varphi>"));
  CHECK (!uni_is_letter ("<infty>"));
  CHECK (uni_is_letter ("<#3B1>"));
  CHECK (uni_is_letter ("<#416>"));
  CHECK (!uni_is_letter ("<#4E2D>"));
}

// bibliographic order ignores case and accents
static void
test_before () {
  CHECK (uni_before ("apple", "Banana"));
  CHECK (!uni_before ("Zebra", "apple"));
  CHECK (uni_before ("\xc9" "cole", "ecolf"));
  CHECK (uni_before ("ecole", "\xc9" "COLE"));
  CHECK (uni_before ("\xc9" "COLE", "ecole"));
}

int
main () {
  RUN (test_utf8_all_code_points);
  RUN (test_utf8_boundaries);
  RUN (test_utf8_decode_sequence);
  RUN (test_utf8_invalid);
  RUN (test_utf8_to_cork_known);
  RUN (test_utf8_to_cork_invalid);
  RUN (test_cork_to_utf8_known);
  RUN (test_round_trip_scripts);
  RUN (test_round_trip_text);
  RUN (test_round_trip_cork_bytes);
  RUN (test_case_cork);
  RUN (test_case_escapes);
  RUN (test_case_pairs);
  RUN (test_case_greek_symbols);
  RUN (test_case_strings);
  RUN (test_accents);
  RUN (test_translit);
  RUN (test_letters);
  RUN (test_before);
  return test_report ();
}
