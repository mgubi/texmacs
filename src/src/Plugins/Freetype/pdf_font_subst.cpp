
/******************************************************************************
* MODULE     : pdf_font_subst.cpp
* DESCRIPTION: fonts to embed for the fonts which a PDF picture names only
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pdf_font_subst.hpp"
#include "free_type.hpp"
#include "tt_file.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "hashmap.hpp"

/******************************************************************************
* The standard encodings (ISO 32000-1, annex D), the names of their glyphs
* and their characters (the Adobe glyph list)
******************************************************************************/

static const char* enc_standard_names[256]= {
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "space", "exclam", "quotedbl", "numbersign", "dollar", "percent", "ampersand", "quoteright",
  "parenleft", "parenright", "asterisk", "plus", "comma", "hyphen", "period", "slash",
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "colon", "semicolon", "less", "equal", "greater", "question",
  "at", "A", "B", "C", "D", "E", "F", "G",
  "H", "I", "J", "K", "L", "M", "N", "O",
  "P", "Q", "R", "S", "T", "U", "V", "W",
  "X", "Y", "Z", "bracketleft", "backslash", "bracketright", "asciicircum", "underscore",
  "quoteleft", "a", "b", "c", "d", "e", "f", "g",
  "h", "i", "j", "k", "l", "m", "n", "o",
  "p", "q", "r", "s", "t", "u", "v", "w",
  "x", "y", "z", "braceleft", "bar", "braceright", "asciitilde", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "exclamdown", "cent", "sterling", "fraction", "yen", "florin", "section",
  "currency", "quotesingle", "quotedblleft", "guillemotleft", "guilsinglleft", "guilsinglright", "fi", "fl",
  "", "endash", "dagger", "daggerdbl", "periodcentered", "", "paragraph", "bullet",
  "quotesinglbase", "quotedblbase", "quotedblright", "guillemotright", "ellipsis", "perthousand", "", "questiondown",
  "", "grave", "acute", "circumflex", "tilde", "macron", "breve", "dotaccent",
  "dieresis", "", "ring", "cedilla", "", "hungarumlaut", "ogonek", "caron",
  "emdash", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "AE", "", "ordfeminine", "", "", "", "",
  "Lslash", "Oslash", "OE", "ordmasculine", "", "", "", "",
  "", "ae", "", "", "", "dotlessi", "", "",
  "lslash", "oslash", "oe", "germandbls", "", "", "", "",
};
static const int enc_standard_uni[256]= {
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  32, 33, 34, 35, 36, 37, 38, 8217,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71,
  72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 91, 92, 93, 94, 95,
  8216, 97, 98, 99, 100, 101, 102, 103,
  104, 105, 106, 107, 108, 109, 110, 111,
  112, 113, 114, 115, 116, 117, 118, 119,
  120, 121, 122, 123, 124, 125, 126, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, 161, 162, 163, 8260, 165, 402, 167,
  164, 39, 8220, 171, 8249, 8250, 64257, 64258,
  -1, 8211, 8224, 8225, 183, -1, 182, 8226,
  8218, 8222, 8221, 187, 8230, 8240, -1, 191,
  -1, 96, 180, 710, 732, 175, 728, 729,
  168, -1, 730, 184, -1, 733, 731, 711,
  8212, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, 198, -1, 170, -1, -1, -1, -1,
  321, 216, 338, 186, -1, -1, -1, -1,
  -1, 230, -1, -1, -1, 305, -1, -1,
  322, 248, 339, 223, -1, -1, -1, -1,
};

static const char* enc_win_ansi_names[256]= {
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "space", "exclam", "quotedbl", "numbersign", "dollar", "percent", "ampersand", "quotesingle",
  "parenleft", "parenright", "asterisk", "plus", "comma", "hyphen", "period", "slash",
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "colon", "semicolon", "less", "equal", "greater", "question",
  "at", "A", "B", "C", "D", "E", "F", "G",
  "H", "I", "J", "K", "L", "M", "N", "O",
  "P", "Q", "R", "S", "T", "U", "V", "W",
  "X", "Y", "Z", "bracketleft", "backslash", "bracketright", "asciicircum", "underscore",
  "grave", "a", "b", "c", "d", "e", "f", "g",
  "h", "i", "j", "k", "l", "m", "n", "o",
  "p", "q", "r", "s", "t", "u", "v", "w",
  "x", "y", "z", "braceleft", "bar", "braceright", "asciitilde", "bullet",
  "Euro", "bullet", "quotesinglbase", "florin", "quotedblbase", "ellipsis", "dagger", "daggerdbl",
  "circumflex", "perthousand", "Scaron", "guilsinglleft", "OE", "bullet", "Zcaron", "bullet",
  "bullet", "quoteleft", "quoteright", "quotedblleft", "quotedblright", "bullet", "endash", "emdash",
  "tilde", "trademark", "scaron", "guilsinglright", "oe", "bullet", "zcaron", "Ydieresis",
  "space", "exclamdown", "cent", "sterling", "currency", "yen", "brokenbar", "section",
  "dieresis", "copyright", "ordfeminine", "guillemotleft", "logicalnot", "hyphen", "registered", "macron",
  "degree", "plusminus", "twosuperior", "threesuperior", "acute", "mu", "paragraph", "periodcentered",
  "cedilla", "onesuperior", "ordmasculine", "guillemotright", "onequarter", "onehalf", "threequarters", "questiondown",
  "Agrave", "Aacute", "Acircumflex", "Atilde", "Adieresis", "Aring", "AE", "Ccedilla",
  "Egrave", "Eacute", "Ecircumflex", "Edieresis", "Igrave", "Iacute", "Icircumflex", "Idieresis",
  "Eth", "Ntilde", "Ograve", "Oacute", "Ocircumflex", "Otilde", "Odieresis", "multiply",
  "Oslash", "Ugrave", "Uacute", "Ucircumflex", "Udieresis", "Yacute", "Thorn", "germandbls",
  "agrave", "aacute", "acircumflex", "atilde", "adieresis", "aring", "ae", "ccedilla",
  "egrave", "eacute", "ecircumflex", "edieresis", "igrave", "iacute", "icircumflex", "idieresis",
  "eth", "ntilde", "ograve", "oacute", "ocircumflex", "otilde", "odieresis", "divide",
  "oslash", "ugrave", "uacute", "ucircumflex", "udieresis", "yacute", "thorn", "ydieresis",
};
static const int enc_win_ansi_uni[256]= {
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71,
  72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 91, 92, 93, 94, 95,
  96, 97, 98, 99, 100, 101, 102, 103,
  104, 105, 106, 107, 108, 109, 110, 111,
  112, 113, 114, 115, 116, 117, 118, 119,
  120, 121, 122, 123, 124, 125, 126, 8226,
  8364, 8226, 8218, 402, 8222, 8230, 8224, 8225,
  710, 8240, 352, 8249, 338, 8226, 381, 8226,
  8226, 8216, 8217, 8220, 8221, 8226, 8211, 8212,
  732, 8482, 353, 8250, 339, 8226, 382, 376,
  32, 161, 162, 163, 164, 165, 166, 167,
  168, 169, 170, 171, 172, 45, 174, 175,
  176, 177, 178, 179, 180, 181, 182, 183,
  184, 185, 186, 187, 188, 189, 190, 191,
  192, 193, 194, 195, 196, 197, 198, 199,
  200, 201, 202, 203, 204, 205, 206, 207,
  208, 209, 210, 211, 212, 213, 214, 215,
  216, 217, 218, 219, 220, 221, 222, 223,
  224, 225, 226, 227, 228, 229, 230, 231,
  232, 233, 234, 235, 236, 237, 238, 239,
  240, 241, 242, 243, 244, 245, 246, 247,
  248, 249, 250, 251, 252, 253, 254, 255,
};

static const char* enc_mac_roman_names[256]= {
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "space", "exclam", "quotedbl", "numbersign", "dollar", "percent", "ampersand", "quotesingle",
  "parenleft", "parenright", "asterisk", "plus", "comma", "hyphen", "period", "slash",
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "colon", "semicolon", "less", "equal", "greater", "question",
  "at", "A", "B", "C", "D", "E", "F", "G",
  "H", "I", "J", "K", "L", "M", "N", "O",
  "P", "Q", "R", "S", "T", "U", "V", "W",
  "X", "Y", "Z", "bracketleft", "backslash", "bracketright", "asciicircum", "underscore",
  "grave", "a", "b", "c", "d", "e", "f", "g",
  "h", "i", "j", "k", "l", "m", "n", "o",
  "p", "q", "r", "s", "t", "u", "v", "w",
  "x", "y", "z", "braceleft", "bar", "braceright", "asciitilde", "",
  "Adieresis", "Aring", "Ccedilla", "Eacute", "Ntilde", "Odieresis", "Udieresis", "aacute",
  "agrave", "acircumflex", "adieresis", "atilde", "aring", "ccedilla", "eacute", "egrave",
  "ecircumflex", "edieresis", "iacute", "igrave", "icircumflex", "idieresis", "ntilde", "oacute",
  "ograve", "ocircumflex", "odieresis", "otilde", "uacute", "ugrave", "ucircumflex", "udieresis",
  "dagger", "degree", "cent", "sterling", "section", "bullet", "paragraph", "germandbls",
  "registered", "copyright", "trademark", "acute", "dieresis", "", "AE", "Oslash",
  "", "plusminus", "", "", "yen", "mu", "", "",
  "", "", "", "ordfeminine", "ordmasculine", "", "ae", "oslash",
  "questiondown", "exclamdown", "logicalnot", "", "florin", "", "", "guillemotleft",
  "guillemotright", "ellipsis", "space", "Agrave", "Atilde", "Otilde", "OE", "oe",
  "endash", "emdash", "quotedblleft", "quotedblright", "quoteleft", "quoteright", "divide", "",
  "ydieresis", "Ydieresis", "fraction", "currency", "guilsinglleft", "guilsinglright", "fi", "fl",
  "daggerdbl", "periodcentered", "quotesinglbase", "quotedblbase", "perthousand", "Acircumflex", "Ecircumflex", "Aacute",
  "Edieresis", "Egrave", "Iacute", "Icircumflex", "Idieresis", "Igrave", "Oacute", "Ocircumflex",
  "", "Ograve", "Uacute", "Ucircumflex", "Ugrave", "dotlessi", "circumflex", "tilde",
  "macron", "breve", "dotaccent", "ring", "cedilla", "hungarumlaut", "ogonek", "caron",
};
static const int enc_mac_roman_uni[256]= {
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71,
  72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 91, 92, 93, 94, 95,
  96, 97, 98, 99, 100, 101, 102, 103,
  104, 105, 106, 107, 108, 109, 110, 111,
  112, 113, 114, 115, 116, 117, 118, 119,
  120, 121, 122, 123, 124, 125, 126, -1,
  196, 197, 199, 201, 209, 214, 220, 225,
  224, 226, 228, 227, 229, 231, 233, 232,
  234, 235, 237, 236, 238, 239, 241, 243,
  242, 244, 246, 245, 250, 249, 251, 252,
  8224, 176, 162, 163, 167, 8226, 182, 223,
  174, 169, 8482, 180, 168, -1, 198, 216,
  -1, 177, -1, -1, 165, 181, -1, -1,
  -1, -1, -1, 170, 186, -1, 230, 248,
  191, 161, 172, -1, 402, -1, -1, 171,
  187, 8230, 32, 192, 195, 213, 338, 339,
  8211, 8212, 8220, 8221, 8216, 8217, 247, -1,
  255, 376, 8260, 164, 8249, 8250, 64257, 64258,
  8225, 183, 8218, 8222, 8240, 194, 202, 193,
  203, 200, 205, 206, 207, 204, 211, 212,
  -1, 210, 218, 219, 217, 305, 710, 732,
  175, 728, 729, 730, 184, 733, 731, 711,
};

static const char* enc_symbol_names[256]= {
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "space", "exclam", "universal", "numbersign", "existential", "percent", "ampersand", "suchthat",
  "parenleft", "parenright", "asteriskmath", "plus", "comma", "minus", "period", "slash",
  "zero", "one", "two", "three", "four", "five", "six", "seven",
  "eight", "nine", "colon", "semicolon", "less", "equal", "greater", "question",
  "congruent", "Alpha", "Beta", "Chi", "Delta", "Epsilon", "Phi", "Gamma",
  "Eta", "Iota", "theta1", "Kappa", "Lambda", "Mu", "Nu", "Omicron",
  "Pi", "Theta", "Rho", "Sigma", "Tau", "Upsilon", "sigma1", "Omega",
  "Xi", "Psi", "Zeta", "bracketleft", "therefore", "bracketright", "perpendicular", "underscore",
  "radicalex", "alpha", "beta", "chi", "delta", "epsilon", "phi", "gamma",
  "eta", "iota", "phi1", "kappa", "lambda", "mu", "nu", "omicron",
  "pi", "theta", "rho", "sigma", "tau", "upsilon", "omega1", "omega",
  "xi", "psi", "zeta", "braceleft", "bar", "braceright", "similar", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "", "", "", "", "", "", "", "",
  "Euro", "Upsilon1", "minute", "lessequal", "fraction", "infinity", "florin", "club",
  "diamond", "heart", "spade", "arrowboth", "arrowleft", "arrowup", "arrowright", "arrowdown",
  "degree", "plusminus", "second", "greaterequal", "multiply", "proportional", "partialdiff", "bullet",
  "divide", "notequal", "equivalence", "approxequal", "ellipsis", "arrowvertex", "arrowhorizex", "carriagereturn",
  "aleph", "Ifraktur", "Rfraktur", "weierstrass", "circlemultiply", "circleplus", "emptyset", "intersection",
  "union", "propersuperset", "reflexsuperset", "notsubset", "propersubset", "reflexsubset", "element", "notelement",
  "angle", "gradient", "registerserif", "copyrightserif", "trademarkserif", "product", "radical", "dotmath",
  "logicalnot", "logicaland", "logicalor", "arrowdblboth", "arrowdblleft", "arrowdblup", "arrowdblright", "arrowdbldown",
  "lozenge", "angleleft", "registersans", "copyrightsans", "trademarksans", "summation", "parenlefttp", "parenleftex",
  "parenleftbt", "bracketlefttp", "bracketleftex", "bracketleftbt", "bracelefttp", "braceleftmid", "braceleftbt", "braceex",
  "", "angleright", "integral", "integraltp", "integralex", "integralbt", "parenrighttp", "parenrightex",
  "parenrightbt", "bracketrighttp", "bracketrightex", "bracketrightbt", "bracerighttp", "bracerightmid", "bracerightbt", "",
};
static const int enc_symbol_uni[256]= {
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  32, 33, 8704, 35, 8707, 37, 38, 8715,
  40, 41, 8727, 43, 44, 8722, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  8773, 913, 914, 935, 8710, 917, 934, 915,
  919, 921, 977, 922, 923, 924, 925, 927,
  928, 920, 929, 931, 932, 933, 962, 8486,
  926, 936, 918, 91, 8756, 93, 8869, 95,
  63717, 945, 946, 967, 948, 949, 966, 947,
  951, 953, 981, 954, 955, 181, 957, 959,
  960, 952, 961, 963, 964, 965, 982, 969,
  958, 968, 950, 123, 124, 125, 8764, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  8364, 978, 8242, 8804, 8260, 8734, 402, 9827,
  9830, 9829, 9824, 8596, 8592, 8593, 8594, 8595,
  176, 177, 8243, 8805, 215, 8733, 8706, 8226,
  247, 8800, 8801, 8776, 8230, 63718, 63719, 8629,
  8501, 8465, 8476, 8472, 8855, 8853, 8709, 8745,
  8746, 8835, 8839, 8836, 8834, 8838, 8712, 8713,
  8736, 8711, 63194, 63193, 63195, 8719, 8730, 8901,
  172, 8743, 8744, 8660, 8656, 8657, 8658, 8659,
  9674, 9001, 63720, 63721, 63722, 8721, 63723, 63724,
  63725, 63726, 63727, 63728, 63729, 63730, 63731, 63732,
  -1, 9002, 8747, 8992, 63733, 8993, 63734, 63735,
  63736, 63737, 63738, 63739, 63740, 63741, 63742, -1,
};

// The characters of the glyph names of the TeX fonts which TeXmacs
// ships (their Type 1 programs), as MuPDF reads them (fz_unicode_from_
// glyph_name, of the TeX glyph list): those of no other source
static const char* tex_glyph_names[]= {
  "dotlessj", "0237",
  "Digamma", "03DD",
  "Finv", "2132",
  "Gmir", "2141",
  "Ifractur", "2111",
  "Omegainv", "2127",
  "Rfractur", "211C",
  "Yen", "00A5",
  "angbracketleft", "27E8",
  "angbracketright", "27E9",
  "anticlockwise", "27F2",
  "approxorequal", "224A",
  "archleftdown", "21B6",
  "archrightdown", "21B7",
  "arrowbothv", "2195",
  "arrowdblbothv", "21D5",
  "arrowleftbothalf", "21BD",
  "arrowlefttophalf", "21BC",
  "arrownortheast", "2197",
  "arrownorthwest", "2196",
  "arrowparrleftright", "21C6",
  "arrowparrrightleft", "21C4",
  "arrowrightbothalf", "21C1",
  "arrowrighttophalf", "21C0",
  "arrowsoutheast", "2198",
  "arrowsouthwest", "2199",
  "arrowtailleft", "21A2",
  "arrowtailright", "21A3",
  "arrowtripleleft", "21DA",
  "arrowtripleright", "21DB",
  "asteriskcentered", "2217",
  "bardbl", "2225",
  "beth", "2136",
  "between", "226C",
  "ceilingleft", "2308",
  "ceilingright", "2309",
  "check", "2713",
  "circleR", "00AE",
  "circleS", "24C8",
  "circleasterisk", "229B",
  "circlecopyrt", "20DD",
  "circledivide", "2298",
  "circledot", "2299",
  "circleequal", "229C",
  "circleminus", "2296",
  "circlering", "229A",
  "clockwise", "27F3",
  "complement", "2201",
  "coproduct", "2A3F",
  "curlyleft", "21AB",
  "curlyright", "21AC",
  "cwm", "200C",
  "daleth", "2138",
  "dblarrowdwn", "21CA",
  "dblarrowheadleft", "219E",
  "dblarrowheadright", "21A0",
  "dblarrowup", "21C8",
  "defines", "225C",
  "diamondmath", "22C4",
  "diamondsolid", "2666",
  "difference", "224F",
  "dividemultiply", "22C7",
  "dotplus", "2214",
  "downfall", "22CE",
  "downslope", "29F9",
  "epsilon1", "03F5",
  "epsiloninv", "03F6",
  "equaldotleftright", "2252",
  "equaldotrightleft", "2253",
  "equalorfollows", "22DF",
  "equalorgreater", "2A96",
  "equalorless", "2A95",
  "equalorprecedes", "22DE",
  "equalorsimilar", "2242",
  "equalsdots", "2251",
  "equivasymptotic", "224D",
  "flat", "266D",
  "floorleft", "230A",
  "floorright", "230B",
  "follownotdbleqv", "2ABA",
  "follownotslnteql", "2AB6",
  "followornoteqvlnt", "22E9",
  "follows", "227B",
  "followsequal", "2AB0",
  "followsorcurly", "227D",
  "followsorequal", "227F",
  "forces", "22A9",
  "forcesbar", "22AA",
  "fork", "22D4",
  "frown", "2322",
  "geomequivalent", "224E",
  "greaterdbleqlless", "2A8C",
  "greaterdblequal", "2267",
  "greaterdot", "22D7",
  "greaterlessequal", "22DB",
  "greatermuch", "226B",
  "greaternotdblequal", "2A8A",
  "greaternotequal", "2A88",
  "greaterorapproxeql", "2A86",
  "greaterorequalslant", "2A7E",
  "greaterornotdbleql", "2269",
  "greaterornotequal", "2269",
  "greaterorsimilar", "2273",
  "harpoondownleft", "21C3",
  "harpoondownright", "21C2",
  "harpoonleftright", "21CC",
  "harpoonrightleft", "21CB",
  "harpoonupleft", "21BF",
  "harpoonupright", "21BE",
  "integerdivide", "2216",
  "intercal", "22BA",
  "interrobang", "203D",
  "interrobangdown", "2E18",
  "intersectiondbl", "22D2",
  "intersectionsq", "2293",
  "latticetop", "22A4",
  "lessdbleqlgreater", "2A8B",
  "lessdblequal", "2266",
  "lessdot", "22D6",
  "lessequalgreater", "22DA",
  "lessmuch", "226A",
  "lessnotdblequal", "2A89",
  "lessnotequal", "2A87",
  "lessorapproxeql", "2A85",
  "lessorequalslant", "2A7D",
  "lessornotdbleql", "2268",
  "lessornotequal", "2268",
  "lessorsimilar", "2272",
  "lscript", "2113",
  "maltesecross", "2720",
  "measuredangle", "2221",
  "multicloseleft", "22C9",
  "multicloseright", "22CA",
  "multimap", "22B8",
  "multiopenleft", "22CB",
  "multiopenright", "22CC",
  "nand", "22BC",
  "natural", "266E",
  "negationslash", "0338",
  "notapproxequal", "2247",
  "notarrowboth", "21AE",
  "notarrowleft", "219A",
  "notarrowright", "219B",
  "notbar", "2224",
  "notdblarrowboth", "21CE",
  "notdblarrowleft", "21CD",
  "notdblarrowright", "21CF",
  "notexistential", "2204",
  "notfollows", "2281",
  "notforces", "22AE",
  "notforcesextra", "22AF",
  "notgreaterequal", "2271",
  "notlessequal", "2270",
  "notsatisfies", "22AD",
  "notsimilar", "2241",
  "notsubseteql", "2288",
  "notsubsetoreql", "228A",
  "notsuperseteql", "2289",
  "notsupersetoreql", "228B",
  "nottriangeqlleft", "22EC",
  "nottriangeqlright", "22ED",
  "nottriangleleft", "22EA",
  "nottriangleright", "22EB",
  "notturnstile", "22AC",
  "orunderscore", "22BB",
  "owner", "220B",
  "perpcorrespond", "2A5E",
  "pertenthousand", "2031",
  "pi1", "03D6",
  "planckover2pi", "210F",
  "planckover2pi1", "210F",
  "precedenotdbleqv", "2AB9",
  "precedenotslnteql", "2AB5",
  "precedeornoteqvlnt", "22E8",
  "precedesequal", "2AAF",
  "precedesorcurly", "227C",
  "precedesorequal", "227E",
  "prime", "2032",
  "primereverse", "2035",
  "revasymptequal", "22CD",
  "revsimilar", "223D",
  "rho1", "03F1",
  "rightanglene", "231D",
  "rightanglenw", "231C",
  "rightanglese", "231F",
  "rightanglesw", "231E",
  "ringinequal", "2256",
  "satisfies", "22A8",
  "sharp", "266F",
  "shiftleft", "21B0",
  "shiftright", "21B1",
  "similarequal", "2243",
  "slurabove", "2322",
  "slurbelow", "2323",
  "smile", "2323",
  "sphericalangle", "2222",
  "square", "25A1",
  "squaredot", "22A1",
  "squareimage", "228F",
  "squareminus", "229F",
  "squaremultiply", "22A0",
  "squareoriginal", "2290",
  "squareplus", "229E",
  "squaresolid", "25A0",
  "squiggleleftright", "21AD",
  "squiggleright", "21DD",
  "star", "22C6",
  "subsetdbl", "22D0",
  "subsetdblequal", "2AC5",
  "subsetnoteql", "228A",
  "subsetornotdbleql", "2ACB",
  "subsetsqequal", "2291",
  "supersetdbl", "22D1",
  "supersetdblequal", "2AC6",
  "supersetnoteql", "228B",
  "supersetornotdbleql", "2ACC",
  "supersetsqequal", "2292",
  "triangle", "25B3",
  "triangledownsld", "25BC",
  "triangleinv", "25BD",
  "triangleleft", "25C1",
  "triangleleftequal", "22B4",
  "triangleleftsld", "25C0",
  "triangleright", "25B7",
  "trianglerightequal", "22B5",
  "trianglerightsld", "25B6",
  "trianglesolid", "25B2",
  "turnstileleft", "22A2",
  "turnstileright", "22A3",
  "uniondbl", "22D3",
  "unionmulti", "228E",
  "unionsq", "2294",
  "uprise", "22CF",
  "upslope", "29F8",
  "vector", "20D7",
  "visiblespace", "2423",
  "wreathproduct", "2240",
  NULL };

// the character of a glyph name: of the standard encodings, uniXXXX,
// uXXXX[XX], or a symbol of TeXmacs (<alpha>); -1 when unknown
static int
name_unicode (string n) {
  static hashmap<string,int> known (-2);
  if (N(known) == 0) {
    const char** names[4]= { enc_standard_names, enc_win_ansi_names,
                             enc_mac_roman_names, enc_symbol_names };
    const int* unis[4]= { enc_standard_uni, enc_win_ansi_uni,
                          enc_mac_roman_uni, enc_symbol_uni };
    for (int k=0; k<4; k++)
      for (int c=0; c<256; c++)
        if (names[k][c][0] != '\0' && unis[k][c] > 0 &&
            !known->contains (names[k][c]))
          known (names[k][c])= unis[k][c];
    // the glyphs of the Symbol font which the Adobe glyph list puts in the
    // private use area: their characters
    const char* sym[]= {
      "arrowvertex", "23D0", "arrowhorizex", "23AF", "radicalex", "203E",
      "registersans", "00AE", "registerserif", "00AE",
      "copyrightsans", "00A9", "copyrightserif", "00A9",
      "trademarksans", "2122", "trademarkserif", "2122",
      "parenlefttp", "239B", "parenleftex", "239C", "parenleftbt", "239D",
      "parenrighttp", "239E", "parenrightex", "239F", "parenrightbt", "23A0",
      "bracketlefttp", "23A1", "bracketleftex", "23A2", "bracketleftbt", "23A3",
      "bracketrighttp", "23A4", "bracketrightex", "23A5",
      "bracketrightbt", "23A6", "bracelefttp", "23A7", "braceleftmid", "23A8",
      "braceleftbt", "23A9", "braceex", "23AA", "bracerighttp", "23AB",
      "bracerightmid", "23AC", "bracerightbt", "23AD", "integralex", "23AE",
      NULL };
    for (int i=0; sym[i] != NULL; i+=2)
      known (sym[i])= from_hexadecimal (sym[i+1]);
    for (int i=0; tex_glyph_names[i] != NULL; i+=2)
      if (!known->contains (tex_glyph_names[i]))
        known (tex_glyph_names[i])= from_hexadecimal (tex_glyph_names[i+1]);
  }
  if (N(n) == 0) return -1;
  // (aNN: a number, ZapfDingbats or a font's own: says nothing)
  if (N(n) >= 2 && n[0] == 'a' && is_digit (n[1])) {
    bool num= true;
    for (int i=1; i<N(n); i++) if (!is_digit (n[i])) num= false;
    if (num) return -1;
  }
  if (known->contains (n)) return known [n];
  int dot= search_forwards (".", n);
  if (dot > 0) n= n (0, dot);
  if (known->contains (n)) return known [n];
  // (uniXXXX and uXXXX[XX] of hexadecimal digits only, of either case:
  // uparrow, utilde are names; uniXXXXYYYY, a ligature, is no single
  // character)
  if (N(n) == 7 && starts (n, "uni")) {
    bool hex= true;
    for (int i=3; i<N(n); i++)
      if (!is_digit (n[i]) && !(n[i] >= 'A' && n[i] <= 'F') &&
          !(n[i] >= 'a' && n[i] <= 'f')) hex= false;
    if (hex) return from_hexadecimal (n (3, 7));
  }
  // (a ligature uniXXXXYYYY[ZZZZ]: the ones of a presentation form, ff, fi,
  // fl, ffi, ffl; another is no single character)
  if ((N(n) == 11 || N(n) == 15) && starts (n, "uni")) {
    bool hex= true;
    for (int i=3; i<N(n); i++)
      if (!is_digit (n[i]) && !(n[i] >= 'A' && n[i] <= 'F') &&
          !(n[i] >= 'a' && n[i] <= 'f')) hex= false;
    if (hex) {
      string seq;
      for (int i=3; i<N(n); i+=4) {
        int c= from_hexadecimal (n (i, i+4));
        seq << (c < 128? (char) c: '?');
      }
      if (seq == "ff") return 0xfb00;
      if (seq == "fi") return 0xfb01;
      if (seq == "fl") return 0xfb02;
      if (seq == "ffi") return 0xfb03;
      if (seq == "ffl") return 0xfb04;
    }
    return -1;
  }
  if (N(n) >= 5 && N(n) <= 7 && n[0] == 'u' && !starts (n, "uni")) {
    bool hex= true;
    for (int i=1; i<N(n); i++)
      if (!is_digit (n[i]) && !(n[i] >= 'A' && n[i] <= 'F') &&
          !(n[i] >= 'a' && n[i] <= 'f')) hex= false;
    if (hex) return from_hexadecimal (n (1, N(n)));
  }
  string u= cork_to_utf8 ("<" * n * ">");
  int i= 0;
  if (N(u) > 0 && u[0] != '<') {
    unsigned int code= decode_from_utf8 (u, i);
    if (i == N(u)) return (int) code;
  }
  return -1;
}

int
pdf_glyph_name_unicode (string name) {
  return name_unicode (name);
}

array<string>
pdf_encoding_names (string base_font, string encoding, array<string> diffs) {
  const char** t= enc_standard_names;
  if (encoding == "WinAnsiEncoding") t= enc_win_ansi_names;
  else if (encoding == "MacRomanEncoding") t= enc_mac_roman_names;
  else if (encoding == "" && search_forwards ("Symbol", base_font) >= 0)
    t= enc_symbol_names;
  array<string> r (256);
  for (int c=0; c<256; c++) {
    r[c]= string (t[c]);
    if (c < N(diffs) && N(diffs[c]) > 0) r[c]= diffs[c];
  }
  return r;
}

/******************************************************************************
* The font to embed instead
******************************************************************************/

// The shipped font for a font name: the TeX Gyre clones of the URW fonts
// for the standard ones (Times, Helvetica, Courier, Palatino...), STIX for
// Symbol, else by its class (serif, sans serif, monospaced)
static string
substitute_name (string base, bool& symbolic) {
  int plus= search_forwards ("+", base);
  if (plus == 6) base= base (7, N(base));
  string s= locase_all (base);
  symbolic= false;
  // (fonts of pictograms: no font of letters in their place)
  if (search_forwards ("dingbat", s) >= 0 ||
      search_forwards ("wingding", s) >= 0 ||
      search_forwards ("webding", s) >= 0 ||
      search_forwards ("marlett", s) >= 0) return "";
  // (STIX has all the glyphs of the Symbol font, the pieces of the large
  // delimiters included)
  if (search_forwards ("symbol", s) >= 0) {
    symbolic= true;
    return "STIXTwoMath-Regular";
  }
  bool bold= (search_forwards ("bold", s) >= 0 ||
              search_forwards ("black", s) >= 0 ||
              search_forwards ("heavy", s) >= 0 ||
              search_forwards ("semibold", s) >= 0 ||
              search_forwards ("demi", s) >= 0);
  bool ital= (search_forwards ("italic", s) >= 0 ||
              search_forwards ("oblique", s) >= 0 ||
              search_forwards ("slant", s) >= 0);
  string fam= "texgyretermes";
  if (search_forwards ("courier", s) >= 0 || search_forwards ("mono", s) >= 0 ||
      search_forwards ("consol", s) >= 0 || search_forwards ("typewriter", s) >= 0 ||
      search_forwards ("fixed", s) >= 0)
    fam= "texgyrecursor";
  else if (search_forwards ("palatino", s) >= 0 ||
           search_forwards ("palladio", s) >= 0 ||
           search_forwards ("pagella", s) >= 0) fam= "texgyrepagella";
  else if (search_forwards ("bookman", s) >= 0) fam= "texgyrebonum";
  else if (search_forwards ("century", s) >= 0 ||
           search_forwards ("schoolbook", s) >= 0) fam= "texgyreschola";
  else if (search_forwards ("avant", s) >= 0) fam= "texgyreadventor";
  else if (search_forwards ("helvetica", s) >= 0 ||
           search_forwards ("arial", s) >= 0 ||
           search_forwards ("sans", s) >= 0 ||
           search_forwards ("verdana", s) >= 0 ||
           search_forwards ("calibri", s) >= 0 ||
           search_forwards ("tahoma", s) >= 0 ||
           search_forwards ("geneva", s) >= 0 ||
           search_forwards ("lucida", s) >= 0 ||
           search_forwards ("futura", s) >= 0 ||
           search_forwards ("gothic", s) >= 0) fam= "texgyreheros";
  string style= (bold && ital? "bolditalic": bold? "bold":
                 ital? "italic": "regular");
  return fam * "-" * style;
}

#ifdef USE_FREETYPE
extern FT_UInt (*ft_get_name_index) (FT_Face face, const FT_String* name);
extern FT_Error (*ft_get_glyph_name) (FT_Face face, FT_UInt glyph,
                                      FT_Pointer buffer, FT_UInt size);
#endif

bool
pdf_font_substitute (string base_font, array<string> names,
                     pdf_font_subst& r) {
#ifdef USE_FREETYPE
  if (ft_initialize () || ft_get_glyph_name == NULL ||
      ft_get_name_index == NULL) return false;
  bool symbolic;
  string sub= substitute_name (base_font, symbolic);
  if (N(sub) == 0) return false;
  url u= tt_font_find (sub);
  if (is_none (u)) return false;
  c_string path (concretize (u));
  FT_Face face;
  if (ft_new_face (ft_library, path, 0, &face)) return false;
  (void) ft_select_charmap (face, ft_encoding_unicode);
  double em= (double) face->units_per_EM;
  if (em <= 0) em= 1000;
  int k1000= 1000;
  r.file= u;
  r.ps_name= replace (as_string (face->family_name), " ", "") * "-" *
             replace (as_string (face->style_name), " ", "");
  r.unicode= array<int> (256);
  r.glyph= array<string> (256);
  array<int> adv (256);
  r.first= 256; r.last= -1; r.missing= 0; r.missing_width= 0;
  if (!ft_load_glyph (face, 0, FT_LOAD_NO_SCALE))
    r.missing_width= (int) (face->glyph->metrics.horiAdvance * k1000 / em + 0.5);
  r.gids= array<int> ();
  r.gids << 0;
  for (int c=0; c<256; c++) {
    r.unicode[c]= -1; r.glyph[c]= ""; adv[c]= r.missing_width;
    if (c >= N(names) || N(names[c]) == 0) continue;
    int uc= name_unicode (names[c]);
    FT_UInt gid= (uc > 0? ft_get_char_index (face, uc): 0);
    if (gid == 0) {
      c_string cn (names[c]);
      gid= ft_get_name_index (face, (FT_String*) (char*) cn);
    }
    // (a code of no glyph: .notdef, rather than the glyph of the code in
    // the encoding of the font program)
    r.glyph[c]= ".notdef";
    // (a code of no glyph, and of no character: U+FFFD in the map, which
    // maps every code used, PDF/UA)
    r.unicode[c]= (uc > 0? uc: 0xfffd);
    if (gid == 0) { r.missing++; continue; }
    char buf[128];
    buf[0]= '\0';
    if (ft_get_glyph_name (face, gid, buf, sizeof (buf)) || buf[0] == '\0' ||
        ft_load_glyph (face, gid, FT_LOAD_NO_SCALE)) {
      r.missing++; continue; }
    r.glyph[c]= string (buf);
    r.unicode[c]= uc;
    r.gids << (int) gid;
    adv[c]= (int) (face->glyph->metrics.horiAdvance * k1000 / em + 0.5);
    if (c < r.first) r.first= c;
    r.last= c;
  }
  if (r.last < 0) { ft_done_face (face); return false; }
  // the glyphs once each, in order (the subsetters want them so)
  for (int i=1; i<N(r.gids); i++)
    for (int j=i; j>0 && r.gids[j] < r.gids[j-1]; j--) {
      int t= r.gids[j]; r.gids[j]= r.gids[j-1]; r.gids[j-1]= t; }
  array<int> uniq;
  for (int i=0; i<N(r.gids); i++)
    if (N(uniq) == 0 || uniq[N(uniq)-1] != r.gids[i]) uniq << r.gids[i];
  r.gids= uniq;
  r.widths= array<int> ();
  for (int c= r.first; c <= r.last; c++) r.widths << adv[c];
  r.bbox[0]= (int) (face->bbox.xMin * k1000 / em);
  r.bbox[1]= (int) (face->bbox.yMin * k1000 / em);
  r.bbox[2]= (int) (face->bbox.xMax * k1000 / em);
  r.bbox[3]= (int) (face->bbox.yMax * k1000 / em);
  r.ascent = (int) (face->ascender  * k1000 / em);
  r.descent= (int) (face->descender * k1000 / em);
  r.cap_height= r.ascent;
  FT_UInt h= ft_get_char_index (face, 'H');
  if (h != 0 && !ft_load_glyph (face, h, FT_LOAD_NO_SCALE))
    r.cap_height= (int) (face->glyph->metrics.horiBearingY * k1000 / em);
  bool ital= (face->style_flags & FT_STYLE_FLAG_ITALIC) != 0;
  bool bold= (face->style_flags & FT_STYLE_FLAG_BOLD) != 0;
  bool mono= FT_IS_FIXED_WIDTH (face);
  r.italic= (ital? -12: 0);
  r.stem_v= (bold? 140: 80);
  // FixedPitch 1, Serif 2, Symbolic 4, Nonsymbolic 32, Italic 64
  r.flags= (symbolic? 4: 32) | (mono? 1: 0) | (ital? 64: 0) |
           (search_forwards ("heros", sub) < 0 && !symbolic? 2: 0);
  ft_done_face (face);
  return true;
#else
  (void) base_font; (void) names; (void) r;
  return false;
#endif
}

string
pdf_cff_program (url file) {
#ifdef USE_FREETYPE
  if (ft_initialize () || ft_load_sfnt_table == NULL) return "";
  c_string path (concretize (file));
  FT_Face face;
  if (ft_new_face (ft_library, path, 0, &face)) return "";
  FT_ULong tag= FT_MAKE_TAG ('C', 'F', 'F', ' ');
  FT_ULong len= 0;
  string r;
  if (!ft_load_sfnt_table (face, tag, 0, NULL, &len) && len > 0) {
    r= string ((int) len);
    if (ft_load_sfnt_table (face, tag, 0, (FT_Byte*) &r[0], &len)) r= "";
  }
  ft_done_face (face);
  return r;
#else
  (void) file;
  return "";
#endif
}

string
pdf_simple_to_unicode (array<int> unicode) {
  int n= 0;
  for (int c=0; c<N(unicode); c++) if (unicode[c] > 0) n++;
  string r;
  r << "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
    << "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n"
    << "/CMapName /Adobe-Identity-UCS def /CMapType 2 def\n"
    << "1 begincodespacerange <00> <FF> endcodespacerange\n";
  int done= 0;
  for (int c=0; c<N(unicode); c++) {
    if (unicode[c] <= 0) continue;
    if (done % 100 == 0) r << as_string (min (100, n - done)) << " beginbfchar\n";
    int u= unicode[c];
    string hex;
    if (u > 0xffff) {
      int v= u - 0x10000;
      hex= as_hexadecimal (0xd800 + (v >> 10), 4) *
           as_hexadecimal (0xdc00 + (v & 0x3ff), 4);
    }
    else hex= as_hexadecimal (u, 4);
    r << "<" << as_hexadecimal (c, 2) << "> <" << hex << ">\n";
    done++;
    if (done % 100 == 0 || done == n) r << "endbfchar\n";
  }
  r << "endcmap CMapName currentdict /CMap defineresource pop end end\n";
  return r;
}
