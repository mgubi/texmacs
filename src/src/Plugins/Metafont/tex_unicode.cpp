
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

// The encodings (fonts/enc) of the TeX math fonts, by the name of the font
// without its size
static const char* tex_math_encodings[][3]= {
  { "cmr", "cmr", "" }, { "cmbx", "cmr", "" },
  { "cmmi", "cmmi", "" }, { "cmmib", "cmmi", "" },
  { "cmsy", "cmsy", "math-cal" }, { "cmbsy", "cmsy", "math-cal" },
  { "cmex", "cmex", "rubber-cmex" }, { "cmexb", "cmex", "rubber-cmex" },
  { "msam", "msam", "" }, { "msbm", "msbm", "math-bbb" },
  { "eufm", "math-frak", "" }, { "eufb", "math-frak", "" },
  { "bbm", "math-bbb", "" }, { "bbmbx", "math-bbb", "" },
  { "grmn", "grmn", "math-upgreek" }, { "grxn", "grmn", "math-upgreek" },
  { "stmary", "stmary", "rubber-stmary" },
  { "wasy", "wasy", "rubber-wasy" }, { "wasyb", "wasy", "rubber-wasy" },
  { "", "", "" }
};

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

// The symbols of the encoding file enc are added to the table of the Unicode
// characters by position; the first symbol at a position is the main one
static void
tex_encoding_unicode (string enc, array<int>& table) {
  string s;
  url u ("$TEXMACS_HOME_PATH/fonts/enc:$TEXMACS_PATH/fonts/enc", enc * ".enc");
  if (load_string (u, s, false)) return;
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
      if (N(name) > 0 && pos < 256 && (table[pos] < 0 || var)) {
        // the "-" of the symbol fonts is the minus sign, not a hyphen
        int u= (name == "-" && enc == "cmsy")? 0x2212:
               tex_symbol_unicode (name);
        if (!var || (u >= 0x391 && u <= 0x3c9)) table[pos]= u;
      }
      pos++;
    }
    else if (is_digit (s[i])) {
      int start= i;
      while (i+1 < n && is_digit (s[i+1])) i++;
      pos= as_int (s (start, i+1));
    }
}

// The Unicode characters of the glyphs of a TeX math font, by position, for
// the text layer; an empty table for the other fonts, and -1 at a position
// whose symbol has no single character
array<int>
tex_math_unicode (string fontname) {
  static hashmap<string,array<int> > tables;
  if (tables->contains (fontname)) return tables[fontname];
  array<int> table;
  int k, n= 0;
  while (n < N(fontname) && is_alpha (fontname[n])) n++;
  string base= fontname (0, n);
  // the CB Greek fonts (grmn, grbx, grmi, gttn, gsmn...) have the encoding
  // of grmn: their text letters, accents and punctuation
  if (N(base) == 4 && (starts (base, "gr") || starts (base, "gt") ||
                       starts (base, "gs")))
    base= "grmn";
  // the fonts of the pieces of slanted lines and of circles (LaTeX's
  // picture mode) have no characters: a table of none, rather than their
  // codes, which are letters (@, P...)
  if ((base == "line" || base == "linew" || base == "lcircle" ||
       base == "lcirclew") && n < N(fontname) && is_digit (fontname[n])) {
    table= array<int> (256);
    for (int i=0; i<256; i++) table[i]= -1;
    tables (fontname)= table;
    return table;
  }
  for (k=0; tex_math_encodings[k][0][0] != '\0'; k++)
    if (base == tex_math_encodings[k][0]) break;
  if (tex_math_encodings[k][0][0] != '\0' &&
      n < N(fontname) && is_digit (fontname[n])) {
    table= array<int> (256);
    for (int i=0; i<256; i++) table[i]= -1;
    for (int j=1; j<3; j++)
      if (tex_math_encodings[k][j][0] != '\0')
        tex_encoding_unicode (tex_math_encodings[k][j], table);
    // the larger variants of a symbol follow it in the list of its metric
    while (n < N(fontname) && is_digit (fontname[n])) n++;
    string tfm_name= fontname (0, n) * ".tfm";
    if (tex_font_metric::instances->contains (tfm_name)) {
      tex_font_metric tfm (tfm_name);
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
          // a search or a copy of the text expects; Hummus writes no
          // mapping as the character 0, not as nothing
          if (d >= tfm->bc && d <= tfm->ec && tfm->tag (d) == 3) {
            int piece[4]= { tfm->top (d), tfm->mid (d),
                            tfm->bot (d), tfm->rep (d) };
            for (int k=0; k<4; k++)
              if (piece[k] > 0 && piece[k] < 256 && table[piece[k]] < 0)
                table[piece[k]]= table[c];
          }
        }
    }
    // see requires_hack_notdef_for_tex_font
    table[161]= table[0];
  }
  tables (fontname)= table;
  return table;
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

// Whether the font file u is a Cyrillic TeX font in the T2A encoding: the LH
// fonts of TeXmacs (fonts/type1/la) and the T2A fonts of cm-super are named
// la + two letters for the shape + the size in hundredths of a point
// (larm1000, larm700, labx1728, larm0500). Other TeX fonts begin with la
// too: lasy10 and lasyb10 (the LaTeX symbols), which are not in T2A.
bool
is_t2a_font_file (url u) {
  if (suffix (u) != "pfb") return false;
  string s= basename (u);
  int n= N(s);
  if (n < 7 || n > 8 || !starts (s, "la")) return false;
  if (!is_alpha (s[2]) || !is_alpha (s[3])) return false;
  for (int i= 4; i < n; i++)
    if (!is_digit (s[i])) return false;
  return true;
}
