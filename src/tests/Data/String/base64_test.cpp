/******************************************************************************
* MODULE     : base64_test.cpp
* DESCRIPTION: tests of the base64 encoding
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "base64.hpp"
#include "analyze.hpp"

/******************************************************************************
* Helpers
******************************************************************************/

// a reference encoder written from RFC 4648, without the line breaks
// which encode_base64 inserts every 60 input bytes
static string
reference_base64 (string s) {
  static const char* alpha=
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  string r;
  int i, n= N(s);
  for (i=0; i<n; i+=3) {
    unsigned int b0= (unsigned char) s[i];
    unsigned int b1= (i+1<n)? (unsigned char) s[i+1]: 0;
    unsigned int b2= (i+2<n)? (unsigned char) s[i+2]: 0;
    unsigned int w = (b0 << 16) | (b1 << 8) | b2;
    r << alpha[(w >> 18) & 63] << alpha[(w >> 12) & 63];
    r << ((i+1<n)? alpha[(w >> 6) & 63]: '=');
    r << ((i+2<n)? alpha[w & 63]: '=');
  }
  return r;
}

static string
remove_newlines (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] != '\n') r << s[i];
  return r;
}

// deterministic pseudo-random bytes covering the whole range 0..255
static string
pseudo_random_bytes (int n, unsigned int seed) {
  string r (n);
  for (int i=0; i<n; i++) {
    seed= seed * 1103515245 + 12345;
    r[i]= (char) ((seed >> 16) & 0xff);
  }
  return r;
}

/******************************************************************************
* Tests
******************************************************************************/

// the test vectors of RFC 4648, section 10
static void
test_rfc_vectors () {
  CHECK_EQ (encode_base64 (""), string (""));
  CHECK_EQ (encode_base64 ("f"), string ("Zg=="));
  CHECK_EQ (encode_base64 ("fo"), string ("Zm8="));
  CHECK_EQ (encode_base64 ("foo"), string ("Zm9v"));
  CHECK_EQ (encode_base64 ("foob"), string ("Zm9vYg=="));
  CHECK_EQ (encode_base64 ("fooba"), string ("Zm9vYmE="));
  CHECK_EQ (encode_base64 ("foobar"), string ("Zm9vYmFy"));
  CHECK_EQ (decode_base64 (""), string (""));
  CHECK_EQ (decode_base64 ("Zg=="), string ("f"));
  CHECK_EQ (decode_base64 ("Zm8="), string ("fo"));
  CHECK_EQ (decode_base64 ("Zm9v"), string ("foo"));
  CHECK_EQ (decode_base64 ("Zm9vYg=="), string ("foob"));
  CHECK_EQ (decode_base64 ("Zm9vYmE="), string ("fooba"));
  CHECK_EQ (decode_base64 ("Zm9vYmFy"), string ("foobar"));
}

// the last two characters of the alphabet and the bytes 0x00 and 0xff,
// which a signed char could get wrong
static void
test_extreme_bytes () {
  CHECK_EQ (encode_base64 (string ("\xfb\xff", 2)), string ("+/8="));
  CHECK_EQ (decode_base64 ("+/8="), string ("\xfb\xff", 2));
  CHECK_EQ (encode_base64 (string ("\0\0\0", 3)), string ("AAAA"));
  CHECK_EQ (decode_base64 ("AAAA"), string ("\0\0\0", 3));
  CHECK_EQ (encode_base64 (string ("\xff\xff\xff", 3)), string ("////"));
  CHECK_EQ (decode_base64 ("////"), string ("\xff\xff\xff", 3));
  CHECK_EQ (encode_base64 (string ("\0", 1)), string ("AA=="));
  CHECK_EQ (decode_base64 ("AA=="), string ("\0", 1));
}

// every single byte, and every pair of bytes with a fixed first one
static void
test_all_byte_values () {
  for (int c=0; c<256; c++) {
    string s (1); s[0]= (char) c;
    string e= encode_base64 (s);
    CHECK_MSG (e == reference_base64 (s), "encoding of byte " * as_string (c));
    CHECK_MSG (decode_base64 (e) == s, "round trip of byte " * as_string (c));
    string t (2); t[0]= (char) (255 - c); t[1]= (char) c;
    CHECK_MSG (decode_base64 (encode_base64 (t)) == t,
               "round trip of pair " * as_string (c));
  }
  string all (256);
  for (int c=0; c<256; c++) all[c]= (char) c;
  CHECK_EQ (remove_newlines (encode_base64 (all)), reference_base64 (all));
  CHECK_EQ (decode_base64 (encode_base64 (all)), all);
}

// all lengths, so that the three padding cases and the line breaks are hit
static void
test_round_trip_lengths () {
  for (int n=0; n<=400; n++) {
    string s= pseudo_random_bytes (n, n + 17);
    string e= encode_base64 (s);
    string ok= reference_base64 (s);
    CHECK_MSG (remove_newlines (e) == ok, "encoding of length " * as_string (n));
    CHECK_MSG (decode_base64 (e) == s, "round trip of length " * as_string (n));
    int pad= (3 - n % 3) % 3;
    CHECK_MSG (N(ok) == 4 * ((n + 2) / 3), "size for length " * as_string (n));
    CHECK_MSG (pad == 0 || ends (ok, pad == 1? string ("="): string ("==")),
               "padding for length " * as_string (n));
  }
}

// encode_base64 breaks the output after every 60 input bytes (80 characters)
static void
test_line_breaks () {
  string s60= pseudo_random_bytes (60, 1);
  CHECK_EQ (search_forwards ("\n", encode_base64 (s60)), -1);
  CHECK_EQ (N (encode_base64 (s60)), 80);
  for (int n=61; n<=63; n++) {
    string e= encode_base64 (pseudo_random_bytes (n, 2));
    CHECK_EQ (search_forwards ("\n", e), 80);
    CHECK_EQ (N(e), 85);
  }
  string e= encode_base64 (pseudo_random_bytes (180, 3));
  CHECK_EQ (N(e), 240 + 2);
  CHECK_EQ (e[80], '\n');
  CHECK_EQ (e[161], '\n');
  CHECK_EQ (search_forwards ("\n", 162, e), -1);
}

// the decoder skips characters outside of the alphabet, so that line breaks
// of any kind and other white space do not matter
static void
test_decode_ignores_whitespace () {
  CHECK_EQ (decode_base64 ("Zm9v\nYmFy"), string ("foobar"));
  CHECK_EQ (decode_base64 ("Zm9v\r\nYmFy\r\n"), string ("foobar"));
  CHECK_EQ (decode_base64 (" Z m 9 v Y m E = "), string ("fooba"));
  CHECK_EQ (decode_base64 ("Zm\t9vYg=\n="), string ("foob"));
  string s= pseudo_random_bytes (300, 5);
  string e= remove_newlines (encode_base64 (s));
  string spread;
  for (int i=0; i<N(e); i++) {
    spread << e[i];
    if (i % 7 == 6) spread << "\r\n";
  }
  CHECK (decode_base64 (spread) == s);
}

// decoding stops at the first padding character
static void
test_decode_stops_at_padding () {
  CHECK_EQ (decode_base64 ("Zg==Zm9v"), string ("f"));
  CHECK_EQ (decode_base64 ("Zm8=Zm9v"), string ("fo"));
}

// a final group may come without its padding, padding after a complete
// group adds nothing, and bytes outside of ASCII are skipped like other
// characters outside of the alphabet
static void
test_decode_unusual_input () {
  CHECK_EQ (decode_base64 ("QUI"), string ("AB"));
  CHECK_EQ (decode_base64 ("QQ"), string ("A"));
  CHECK_EQ (decode_base64 ("Zm9vYmE"), string ("fooba"));
  CHECK_EQ (decode_base64 ("Q"), string (""));
  CHECK_EQ (decode_base64 ("="), string (""));
  CHECK_EQ (decode_base64 ("QUJD="), string ("ABC"));
  CHECK_EQ (decode_base64 ("QUJD=="), string ("ABC"));
  CHECK_EQ (decode_base64 ("QU\xffJ\x80" "D"), string ("ABC"));
  CHECK_EQ (decode_base64 ("\xc3\xa9QUJD"), string ("ABC"));
  for (int n=0; n<=60; n++) {
    string s= pseudo_random_bytes (n, n + 3);
    string e= encode_base64 (s);
    string bare;
    for (int i=0; i<N(e); i++)
      if (e[i] != '=') bare << e[i];
    CHECK_MSG (decode_base64 (bare) == s,
               "unpadded round trip of length " * as_string (n));
  }
}

int
main () {
  RUN (test_rfc_vectors);
  RUN (test_decode_unusual_input);
  RUN (test_extreme_bytes);
  RUN (test_all_byte_values);
  RUN (test_round_trip_lengths);
  RUN (test_line_breaks);
  RUN (test_decode_ignores_whitespace);
  RUN (test_decode_stops_at_padding);
  return test_report ();
}
