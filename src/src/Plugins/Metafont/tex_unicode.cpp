
/******************************************************************************
* MODULE     : tex_unicode.cpp
* DESCRIPTION: the Unicode characters of the glyphs of the TeX math fonts,
*              for the text layer of the PDF writers
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tex_unicode.hpp"
#include "load_tfm.hpp"
#include "file.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "hashmap.hpp"

// The Unicode character of the symbol name of an encoding file, or -1.
// The variants of the big symbols stand for the symbol itself: large-(-1,
// big-sum-2 and cmex-int-1 are (, sum and int.
static int
tex_symbol_unicode (string name) {
  for (int pass=0; pass<3; pass++) {
    string sym= (N(name) > 1? "<" * name * ">": name);
    string u  = cork_to_utf8 (sym);
    int i= 0;
    if (N(u) > 0) {
      unsigned int code= decode_from_utf8 (u, i);
      if (i == N(u) && (N(name) == 1 || u != sym)) return (int) code;
    }
    if (pass == 0) {
      int n= N(name);
      while (n > 0 && is_digit (name[n-1])) n--;
      if (n > 1 && n < N(name) && name[n-1] == '-') name= name (0, n-1);
    }
    else if (pass == 1) {
      if (starts (name, "large-")) name= name (6, N(name));
      else if (starts (name, "big-")) name= name (4, N(name));
      else if (starts (name, "wide-")) name= name (5, N(name));
      else if (starts (name, "cmex-")) name= name (5, N(name));
      else break;
    }
  }
  return -1;
}

// A Greek capital or small letter (also bold, italic...), and a Latin one
static bool
greek_letter (int u) {
  return (u >= 0x391 && u <= 0x3c9) || (u >= 0x1d6a8 && u <= 0x1d7c9);
}

static bool
latin_letter (int u) {
  return (u >= 'A' && u <= 'Z') || (u >= 'a' && u <= 'z') ||
         (u >= 0x1d400 && u <= 0x1d6a3);
}

// The symbols of the encoding file enc are added to the table of the Unicode
// characters by position; the first symbol at a position is the main one,
// but a Latin letter wins over a Greek one of the same shape listed before
// it (math-bold-1: b-Alpha, then b-up-A); false when there is no such file
static bool
tex_encoding_unicode (string enc, array<int>& table) {
  string s;
  url u ("$TEXMACS_HOME_PATH/fonts/enc:$TEXMACS_PATH/fonts/enc", enc * ".enc");
  if (load_string (u, s, false)) return false;
  int i, n= N(s), pos= 0;
  for (i=0; i<n; i++)
    if (s[i] == '\"') {
      string name;
      for (i++; i<n && s[i] != '\"'; i++) {
        if (s[i] == '\\' && i+1 < n) i++;
        name << s[i];
      }
      // (in grmn.enc the symbols theta, phi and kappa come before the
      // letters at their places: the letters win, as the glyphs say)
      bool var= (pos < 256 && enc == "grmn" &&
                 (table[pos] == 0x3d1 || table[pos] == 0x3d5 ||
                  table[pos] == 0x3f0));
      if (N(name) > 0 && pos < 256) {
        // the "-" of the symbol fonts is the minus sign, not a hyphen
        int u= (name == "-" && enc == "cmsy")? 0x2212:
               tex_symbol_unicode (name);
        if (table[pos] < 0) table[pos]= u;
        else if (var && u >= 0x391 && u <= 0x3c9) table[pos]= u;
        else if (greek_letter (table[pos]) && latin_letter (u))
          table[pos]= u;
      }
      pos++;
    }
    else if (is_digit (s[i])) {
      int start= i;
      while (i+1 < n && is_digit (s[i+1])) i++;
      pos= as_int (s (start, i+1));
    }
  return true;
}

// the OT1 encoding (and its typewriter variant): the positions of no
// ASCII character
static int
tex_ot1_unicode (int c, bool tt) {
  static const int low[32]= {
    0x393, 0x394, 0x398, 0x39B, 0x39E, 0x3A0, 0x3A3, 0x3A5,
    0x3A6, 0x3A8, 0x3A9, 0xFB00, 0xFB01, 0xFB02, 0xFB03, 0xFB04,
    0x131, 0x237, 0x60, 0xB4, 0x2C7, 0x2D8, 0xAF, 0x2DA,
    0xB8, 0xDF, 0xE6, 0x153, 0xF8, 0xC6, 0x152, 0xD8 };
  if (c < 0 || c > 0x7f) return 0xfffd;
  if (tt) {
    // (cmtt: arrows, quote, ¡ ¿ at 0x0B-0x0F, ASCII above)
    static const int ttlow[5]= { 0x2191, 0x2193, 0x27, 0xA1, 0xBF };
    if (c >= 0x0b && c <= 0x0f) return ttlow[c - 0x0b];
    if (c < 32) return low[c];
    return c;
  }
  if (c < 32) return low[c];
  switch (c) {
  case 0x22: return 0x201D;
  case 0x3C: return 0xA1;
  case 0x3E: return 0xBF;
  case 0x5C: return 0x201C;
  case 0x5E: return 0x2C6;
  case 0x5F: return 0x2D9;
  case 0x7B: return 0x2013;
  case 0x7C: return 0x2014;
  case 0x7D: return 0x2DD;
  case 0x7E: return 0x2DC;
  case 0x7F: return 0xA8;
  default: return c;
  }
}

// the double-struck letter or digit of a character (make_bbb)
static int
double_struck (int u) {
  if (u == 'C') return 0x2102;
  if (u == 'H') return 0x210D;
  if (u == 'N') return 0x2115;
  if (u == 'P') return 0x2119;
  if (u == 'Q') return 0x211A;
  if (u == 'R') return 0x211D;
  if (u == 'Z') return 0x2124;
  if (u >= 'A' && u <= 'Z') return 0x1D538 + (u - 'A');
  if (u >= 'a' && u <= 'z') return 0x1D552 + (u - 'a');
  if (u >= '0' && u <= '9') return 0x1D7D8 + (u - '0');
  return u;
}

// The Unicode character of the glyph at position ch of a TeX font in the
// Cork encoding (the EC fonts) or the T2A encoding (the Cyrillic fonts),
// for the text layer: the positions are not Unicode code points, and
// differ from Latin-1 for some letters (oe at 0xF7, sharp s at 0xFF)
int
tex_glyph_unicode (int ch, bool t2a) {
  static int cork_table[256], t2a_table[256];
  static bool done= false;
  if (!done) {
    for (int c=0; c<256; c++) {
      string s (1);
      s[0]= (char) c;
      for (int k=0; k<2; k++) {
        string u= (k == 0? cork_to_utf8 (s): t2a_to_utf8 (s));
        int i= 0;
        unsigned int code= (N(u) == 0? c: decode_from_utf8 (u, i));
        // a position without a single character keeps its number
        if (N(u) == 0 || i != N(u)) code= c;
        (k == 0? cork_table: t2a_table)[c]= (int) code;
      }
    }
    done= true;
  }
  if (ch < 0 || ch > 255) return ch;
  return t2a? t2a_table[ch]: cork_table[ch];
}

// The Adobe standard encoding (the raw PostScript fonts of TeX, rptmr...):
// ASCII, but for the quotes, and above it the letters and accents which
// TeXmacs draws in them (tex_font.cpp, ADOBE_accents)
static int
tex_adobe_unicode (int c) {
  static const int high[96]= {
    -1, 0xA1, 0xA2, 0xA3, 0x2044, 0xA5, 0x192, 0xA7,
    0xA4, 0x27, 0x201C, 0xAB, 0x2039, 0x203A, 0xFB01, 0xFB02,
    -1, 0x2013, 0x2020, 0x2021, 0xB7, -1, 0xB6, 0x2022,
    0x201A, 0x201E, 0x201D, 0xBB, 0x2026, 0x2030, -1, 0xBF,
    -1, 0x60, 0xB4, 0x2C6, 0x2DC, 0xAF, 0x2D8, 0x2D9,
    0xA8, -1, 0x2DA, 0xB8, -1, 0x2DD, 0x2DB, 0x2C7,
    0x2014, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 0xC6, -1, 0xAA, -1, -1, -1, -1,
    0x141, 0xD8, 0x152, 0xBA, -1, -1, -1, -1,
    -1, 0xE6, -1, -1, -1, 0x131, -1, -1,
    0x142, 0xF8, 0x153, 0xDF, -1, -1, -1, -1 };
  if (c == 0x27) return 0x2019;
  if (c == 0x60) return 0x2018;
  if (c >= 0x20 && c < 0x7f) return c;
  if (c >= 0xa0 && c <= 0xff) return high[c - 0xa0];
  return -1;
}

// Whether the encoding file enc only gives letters and digits as themselves
// (alphanum, capital, alpha...: the letters of a style, the glyphs of a
// symbol font drawn as plain letters)
static bool
tex_encoding_plain (string enc) {
  static hashmap<string,bool> plain (false);
  if (plain->contains (enc)) return plain[enc];
  string s;
  url u ("$TEXMACS_HOME_PATH/fonts/enc:$TEXMACS_PATH/fonts/enc", enc * ".enc");
  bool r= false;
  if (!load_string (u, s, false)) {
    r= true;
    int n= N(s);
    for (int i=0; i<n && r; i++)
      if (s[i] == '\"') {
        int j= i+1;
        while (j < n && s[j] != '\"') j++;
        if (j - i - 1 != 1 || (!is_alpha (s[i+1]) && !is_digit (s[i+1])))
          r= false;
        i= j;
      }
      else if (s[i] == '[') r= false;
  }
  plain (enc)= r;
  return r;
}

array<int>
tex_encoding_table (string encoding, string metric, bool text) {
  static hashmap<string,array<int> > tables;
  if (encoding == "" || encoding == "unicode" || encoding == "mixed" ||
      starts (encoding, "bbb:")) return array<int> ();
  string key= encoding * "@" * metric * (text? "": "@symbols");
  if (tables->contains (key)) return tables[key];
  array<int> table (256);
  for (int c=0; c<256; c++) table[c]= -1;
  bool any= false, symbols= false;
  // (the encodings of the letters as themselves last, whatever their
  // place: the symbols of the glyphs first, ℝ of msbm, not R)
  array<string> list, last;
  int i= 0, n= N(encoding);
  while (i < n) {
    int j= i;
    while (j < n && encoding[j] != '+') j++;
    string e= encoding (i, j);
    i= j + 1;
    if (tex_encoding_plain (e)) last << e; else list << e;
  }
  list << last;
  // (a code given as a plain letter or digit by one encoding, a symbol by
  // another, is the symbol, whatever the order: x of cmbx drawn by a bold
  // math font as x and by a math font as the bold x is the bold x)
  array<int> main= copy (table);
  for (int k=0; k<N(list); k++) {
    string e= list[k];
    for (int c=0; c<256; c++) table[c]= -1;
    if (e == "t2a" || (text && e == "cork")) {
      for (int c=0; c<256; c++)
        if (table[c] < 0) table[c]= tex_glyph_unicode (c, e == "t2a");
      any= true;
    }
    else if (text && (e == "ot1" || e == "ot1-tt")) {
      for (int c=0; c<128; c++)
        if (table[c] < 0) table[c]= tex_ot1_unicode (c, e == "ot1-tt");
      any= true;
    }
    else if (text && e == "adobe") {
      for (int c=0; c<256; c++)
        if (table[c] < 0) table[c]= tex_adobe_unicode (c);
      any= true;
    }
    else if (e != "cork" && e != "ot1" && e != "ot1-tt" && e != "adobe" &&
             e != "unicode" && e != "mixed" &&
             tex_encoding_unicode (e, table))
      any= symbols= true;
    for (int c=0; c<256; c++) {
      int u= table[c];
      if (u < 0) continue;
      int m= main[c];
      bool plain= (m < 0x80 && (is_alpha ((char) m) || is_digit ((char) m)));
      if (m < 0 || (plain && u >= 0x80)) main[c]= u;
    }
  }
  table= main;
  if (!any) table= array<int> ();
  else if (symbols && metric != "" &&
           tex_font_metric::instances->contains (metric)) {
    // the larger variants of a symbol follow it in the list of its metric
    tex_font_metric tfm (metric);
    for (int c= tfm->bc; c <= tfm->ec && c < 256; c++)
      if (table[c] >= 0) {
        int d= c;
        for (int len=0; len<256 && tfm->tag (d) == 2; len++) {
          d= tfm->rem (d);
          if (d < tfm->bc || d > tfm->ec) break;
          if (table[d] < 0) table[d]= table[c];
        }
        // the largest variant may be built from pieces (top, middle,
        // bottom, repeated): each piece is the delimiter, which is what
        // a search or a copy of the text expects
        if (d >= tfm->bc && d <= tfm->ec && tfm->tag (d) == 3) {
          int piece[4]= { tfm->top (d), tfm->mid (d),
                          tfm->bot (d), tfm->rep (d) };
          for (int k=0; k<4; k++)
            if (piece[k] > 0 && piece[k] < 256 && table[piece[k]] < 0)
              table[piece[k]]= table[c];
        }
      }
  }
  // (code 161 of a TeX font of no character there is the glyph of code 0,
  // see requires_hack_notdef_for_tex_font)
  if (N(table) > 161 && table[161] < 0) table[161]= table[0];
  tables (key)= table;
  return table;
}

int
tex_encoding_char (string encoding, string metric, int c) {
  int u;
  if (starts (encoding, "bbb:")) {
    u= tex_encoding_char (encoding (4, N(encoding)), metric, c);
    if (u != 0xfffd) u= double_struck (u);
  }
  else if (encoding == "unicode") u= c;
  else {
    array<int> table= tex_encoding_table (encoding, metric, true);
    u= (c >= 0 && c < N(table) && table[c] >= 0)? table[c]: 0xfffd;
  }
  if (u < 0x20 || (u >= 0x7f && u <= 0x9f) || u == 0xfeff || u == 0xfffe ||
      u > 0x10ffff) u= 0xfffd;
  return u;
}
