
/******************************************************************************
* MODULE     : tt_tools.cpp
* DESCRIPTION: Direct access of True Type font (independent from FreeType)
* COPYRIGHT  : (C) 2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tt_tools.hpp"
#include "tt_file.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "iterator.hpp"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#define U8  uint8_t
#define U16 uint16_t
#define U32 uint32_t
#define U64 uint64_t
#define S16 int16_t
#else
#define U8  unsigned char
#define U16 unsigned short
#define U32 unsigned int
#define U64 unsigned long long int
#define S16 short
#endif

string strip_suffix (string name);

/******************************************************************************
* Data access
******************************************************************************/

string
get_sub (string s, int i, int j) {
  i= min (max (i, 0), N(s));
  j= min (max (j, 0), N(s));
  return s (i, j);
}

string
get_tag (string s, int i) {
  return get_sub (s, i, i+4);
}

U8
get_U8 (string s, int i) {
  return *((U8*) ((void*) (&s[i])));
}

U16
get_U16 (string s, int i) {
  return (((U16) get_U8 (s, i)) << 8) + get_U8 (s, i+1);
}

U32
get_U32 (string s, int i) {
  return (((U32) get_U16 (s, i)) << 16) + get_U16 (s, i+2);
}

S16
get_S16 (string s, int i) {
  return (S16) get_U16 (s, i);
}

string
pack_U32 (U32 i) {
  string r;
  r << ((char) ( i >> 24));
  r << ((char) ((i >> 16) & 255));
  r << ((char) ((i >>  8) & 255));
  r << ((char) ( i        & 255));
  return r;
}

/******************************************************************************
* True Type font collections
******************************************************************************/

bool
tt_is_collection (string tt) {
  return get_tag (tt, 0) == "ttcf";
}

int
tt_nr_fonts (string tt) {
  if (!tt_is_collection (tt)) return 1;
  else return get_U32 (tt, 8);
}

int
tt_header_index (string tt, int i) {
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return 0;
  else return get_U32 (tt, 12 + 4*i);
}

/******************************************************************************
* Font table management
******************************************************************************/

bool
tt_correct_version (string tt, int i) {
  int h= tt_header_index (tt, i);
  return
    get_U32 (tt, h) == 0x00010000 ||
    get_tag (tt, h) == "OTTO" ||
    get_tag (tt, h) == "true" ||
    get_tag (tt, h) == "typ1";
}

int
tt_nr_tables (string tt, int i) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  int h= tt_header_index (tt, i);
  return get_U16 (tt, h + 4);
}

string
tt_table_tag (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_tag (tt, h + 12 + 16 * k);
}

U32
tt_table_start (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  return get_U32 (tt, h + 20 + 16 * k);
}

string
tt_table (string tt, int i, int k) {
  ASSERT (tt_correct_version (tt, i), "true type font expected");
  ASSERT (k >= 0 && k < tt_nr_tables (tt, i), "index out of range");
  int h= tt_header_index (tt, i);
  int start= get_U32 (tt, h + 20 + 16 * k);
  int len  = get_U32 (tt, h + 24 + 16 * k);
  return get_sub (tt, start, start + len);
}

string
tt_table (string tt, int i, string tag) {
  for (int k=0; k<tt_nr_tables (tt, i); k++)
    if (tt_table_tag (tt, i, k) == tag)
      return tt_table (tt, i, k);
  return "";
}

/******************************************************************************
* Extracting a subfont
******************************************************************************/

string
tt_extract_subfont (string tt, int i) {
  if (i < 0 || i >= tt_nr_fonts (tt))
    cout << "TeXmacs] error, invalid TrueType subfont " << i
         << " out of " << tt_nr_fonts (tt) << "\n";
  ASSERT (i >= 0 && i < tt_nr_fonts (tt), "index out of range");
  if (!tt_is_collection (tt)) return tt;
  string r;
  int h= tt_header_index (tt, i);
  r << get_sub (tt, h, h + 12);
  int nr_tabs= tt_nr_tables (tt, i);
  int offset= 12 + 16 * nr_tabs;
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    r << get_sub (tt, taboff, taboff + 8);
    r << pack_U32 (offset);
    r << get_sub (tt, taboff + 12, taboff + 16);
    int len= get_U32 (tt, taboff + 12);
    offset += (((len + 3) >> 2) << 2);
  }
  for (int k=0; k < nr_tabs; k++) {
    int taboff= h + 12 + 16 * k;
    int start= get_U32 (tt, taboff + 8);
    int len  = get_U32 (tt, taboff + 12);
    int plen = (((len + 3) >> 2) << 2);
    r << get_sub (tt, start, start + plen);
  }
  return r;
}

/******************************************************************************
* Name table
******************************************************************************/

int
name_format (string nt) {
  return get_U16 (nt, 0);
}

int
name_nr_records (string nt) {
  return get_U16 (nt, 2);
}

int
name_storage_offset (string nt) {
  return get_U16 (nt, 4);
}

string
name_record (string nt, int i) {
  ASSERT (i >= 0 && i < name_nr_records (nt), "index out of range");
  return get_sub (nt, 6 + 12*i, 18 + 12*i);
}

int
name_record_platform_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 0);
}

int
name_record_encoding_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 2);
}

int
name_record_language_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 4);
}

int
name_record_name_id (string nt, int i) {
  return get_U16 (name_record (nt, i), 6);
}

string
name_record_string (string nt, int i) {
  int off= name_storage_offset (nt);
  string rec= name_record (nt, i);
  int start= get_U16 (rec, 10) + off;
  int len= get_U16 (rec, 8);
  return get_sub (nt, start, start + len);
}

string
filter_english (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] >= ' ' && (((unsigned int) s[i]) <= 127))
      r << s[i];
  return r;
}

string
name_record_english_string (string nt, int name_id) {
  for (int i=0; i < name_nr_records (nt); i++)
    if (name_record_name_id (nt, i) == name_id) {
      if ((name_record_platform_id (nt, i) == 1 &&
           name_record_language_id (nt, i) == 0) ||
          (name_record_platform_id (nt, i) == 3 &&
           name_record_language_id (nt, i) == 0x0409))
        return filter_english (name_record_string (nt, i));
    }
  return "";
}

string
name_record_family (string nt) {
  return name_record_english_string (nt, 1);
}

string
name_record_shape (string nt) {
  return name_record_english_string (nt, 2);
}

/******************************************************************************
* Dump all font info to the screen
******************************************************************************/

void
tt_dump (string tt) {
  for (int i=0; i<tt_nr_fonts (tt); i++) {
    cout << HRULE << "Font " << i << LF << HRULE;

    cout << "Font tables:";
    for (int k=0; k<tt_nr_tables (tt, i); k++)
      cout << " " << tt_table_tag (tt, i, k);
    cout << LF;

    string nt= tt_table (tt, i, "name");
    cout << "Font family: " << name_record_family (nt) << "\n";
    cout << "Font shape: " << name_record_shape (nt) << "\n";
    cout << "Name table" << LF;
    for (int k=0; k<name_nr_records (nt); k++)
      cout << "  " << name_record_name_id (nt, k)
           << " [" << name_record_language_id (nt, k)
           << ", " << name_record_platform_id (nt, k) << "]"
           << " -> " << name_record_string (nt, k) << LF;
  }
  dump_mathtable (cout, parse_mathtable (tt));
}

void
tt_dump (url u) {
  string tt;
  if (!load_string (u, tt, false))
    tt_dump (tt);
  else cout << "file not found";
}

/******************************************************************************
* Get the font family and available shapes
******************************************************************************/

void
move_to_shape (string& fam, string& shape, string what, string by) {
  int pos= search_forwards (" " * what, 0, fam);
  if (pos < 0) return;
  fam  = fam (0, pos) * fam (pos + N(what) + 1, N(fam));
  if (N(by) == 0) return;
  if (shape == "Regular") shape= by;
  else shape= by * " " * shape;
}

scheme_tree
tt_font_name (url u) {
  string tt;
  tree r (TUPLE);
  if (load_string (u, tt, false)) return r;
  for (int i=0; i < tt_nr_fonts (tt); i++) {
    if (!tt_correct_version (tt, i)) return tree (TUPLE);
    string nt = tt_table (tt, i, "name");
    string fam= name_record_family (nt);
    string sh = name_record_shape (nt);

    // Some basic normalization of family name
    move_to_shape (fam, sh, "Narrow", "Narrow");
    move_to_shape (fam, sh, "Condensed", "Condensed");
    move_to_shape (fam, sh, "Extended", "Extended");
    move_to_shape (fam, sh, "Wide", "Wide");
    move_to_shape (fam, sh, "Caption", "Caption");
    move_to_shape (fam, sh, "Semilight", "SemiLight");
    move_to_shape (fam, sh, "SemiLight", "SemiLight");
    move_to_shape (fam, sh, "Semi Light", "SemiLight");
    move_to_shape (fam, sh, "Ultralight", "Thin");
    move_to_shape (fam, sh, "UltraLight", "Thin");
    move_to_shape (fam, sh, "Ultra Light", "Thin");
    move_to_shape (fam, sh, "Light", "Light");
    move_to_shape (fam, sh, "Medium", "");
    move_to_shape (fam, sh, "Semibold", "SemiBold");
    move_to_shape (fam, sh, "SemiBold", "SemiBold");
    move_to_shape (fam, sh, "Semi Bold", "SemiBold");
    move_to_shape (fam, sh, "Demibold", "DemiBold");
    move_to_shape (fam, sh, "DemiBold", "DemiBold");
    move_to_shape (fam, sh, "Demi Bold", "DemiBold");
    move_to_shape (fam, sh, "Bold", "Bold");
    move_to_shape (fam, sh, "Extrabold", "ExtraBold");
    move_to_shape (fam, sh, "ExtraBold", "ExtraBold");
    move_to_shape (fam, sh, "Extra Bold", "ExtraBold");
    move_to_shape (fam, sh, "Heavy", "Heavy");
    move_to_shape (fam, sh, "Black", "Black");
    move_to_shape (fam, sh, "Italic", "Italic");
    move_to_shape (fam, sh, "Oblique", "Oblique");

    while (fam != "" && !is_alpha (fam[0])) fam= fam (1, N(fam));
    if (upcase_all (fam) == fam) fam= locase_all (fam);
    fam= upcase_first (fam);
    if (starts (fam, "STIX")) fam= "Stix" * fam (4, N(fam));
    // End normalization of family name
    
    r << tuple (fam, sh);
  }
  return r;
}

url
tt_unpack (string s) {
  if (!is_int (suffix (url (s)))) return url_none ();
  url dir= url ("$TEXMACS_HOME_PATH/fonts/unpacked");
  if (!exists (dir)) mkdir (dir);
  url name= dir * url (s * ".ttf");
  if (exists (name)) return name;
  //cout << "Extracting " << name << "\n";
  int i= as_int (suffix (url (s)));
  s= strip_suffix (s);
  url u= tt_font_find (s);
  if (is_none (u)) return url_none ();
  string ttc;
  if (load_string (u, ttc, false)) return url_none ();
  string tt= tt_extract_subfont (ttc, i);
  if (save_string (name, tt, false)) return url_none ();
  return name;
}

/******************************************************************************
 * OpenType MATH table
 ******************************************************************************/

unsigned int
ot_mathtable_rep::get_init_glyphID (unsigned int glyphID) {
  // init cache
  if (N (get_init_glyphID_cache) == 0) {
    auto it= iterate (ver_glyph_variants);
    while (it->busy ()) {
      unsigned int        gid= it->next ();
      array<unsigned int> v  = ver_glyph_variants (gid);
      for (int i=0; i < N(v); i++) {
        get_init_glyphID_cache (v[i])= gid;
      }
    }
    it= iterate (hor_glyph_variants);
    while (it->busy ()) {
      unsigned int        gid= it->next ();
      array<unsigned int> v  = hor_glyph_variants (gid);
      for (int i=0; i < N(v); i++) {
        get_init_glyphID_cache (v[i])= gid;
      }
    }
  }

  // look up cache
  if (get_init_glyphID_cache->contains (glyphID)) {
    return get_init_glyphID_cache (glyphID);
  }

  return glyphID;
}

bool
ot_mathtable_rep::has_kerning (unsigned int glyphID, bool top, bool left) {
  if (!math_kern_info->contains (glyphID)) return false;
  auto ki= math_kern_info (glyphID);
  return (top ? (left ? ki.hasTopLeft : ki.hasTopRight)
              : (left ? ki.hasBottomLeft : ki.hasBottomRight));
}

int
ot_mathtable_rep::get_kerning (unsigned int glyphID, int height, bool top,
                               bool left) {
  // should be called after has_kerning
  auto& ki= math_kern_info (glyphID);
  MathKernTable& kt=
      top ? (left ? ki.topLeft : ki.topRight) 
          : (left ? ki.bottomLeft : ki.bottomRight);
  int idx= 0, n= (int) kt.heightCount;
  // only one kerning value
  if (n == 0) {
    return kt.kernValues[0].value;
  }
  // find the kerning value
  if (height < kt.correctionHeight[0].value) {
    idx= 0; // below the first height entry
  }
  else if (height >= kt.correctionHeight[n - 1].value) {
    idx= n; // above the last height entry
  }
  else {
    for (idx= 1; idx <= n - 1; idx++) {
      if (height >= kt.correctionHeight[idx - 1].value &&
          height < kt.correctionHeight[idx].value) {
        break;
      }
    }
  }
  // cout << "get_kerning : height " << height << " -> " << idx << " -> "
  //  << kt.kernValues[idx] << LF;
  return kt.kernValues[idx].value;
}

/******************************************************************************
 * Reading the tables of a font file which may be truncated or wrong
 ******************************************************************************/

// One table of a font file. Every offset and every count comes from the
// file, so each read is checked against the length of the table: a read
// outside gives zero and marks the table as bad, after which every read
// gives zero. The number of reads is limited as well, because wrong offsets
// can make the same records be read over and over, whereas a sound table is
// read about once.
struct ot_table {
  string    s;
  long long n;
  long long reads;
  bool      ok;

  ot_table (string s2)
      : s (s2), n (N (s2)), reads (16 * ((long long) N (s2)) + 4096),
        ok (true) {}

  inline bool has (long long i, long long len) {
    return i >= 0 && len >= 0 && i + len <= n;
  }
  inline unsigned int u16 (long long i) {
    if (!ok || !has (i, 2) || --reads < 0) { ok= false; return 0; }
    return (((unsigned int) ((U8) s[(int) i])) << 8) + ((U8) s[(int) i + 1]);
  }
  inline int s16 (long long i) {
    return (int) ((S16) u16 (i));
  }
  inline unsigned int u32 (long long i) {
    unsigned int hi= u16 (i);
    return (hi << 16) + u16 (i + 2);
  }
  inline string tag (long long i) {
    if (!ok || !has (i, 4)) { ok= false; return ""; }
    return s ((int) i, (int) i + 4);
  }
  // nr records of the given size at offset i, when the table holds them
  inline int count (long long i, long long nr, long long size) {
    if (!ok || !has (i, nr * size)) { ok= false; return 0; }
    return (int) nr;
  }
};

static inline unsigned int
raw_U16 (const char* buf, int i) {
  return (((unsigned int) ((U8) buf[i])) << 8) + ((U8) buf[i + 1]);
}

static inline unsigned int
raw_U32 (const char* buf, int i) {
  return (raw_U16 (buf, i) << 16) + raw_U16 (buf, i + 2);
}

static inline bool
raw_tag (const char* buf, int i, const char* tag) {
  return buf[i] == tag[0] && buf[i + 1] == tag[1] &&
         buf[i + 2] == tag[2] && buf[i + 3] == tag[3];
}

// The table with the given tag of the first font in a file, or the empty
// string. The directory is read in the buffer of the file, so that only the
// table itself is copied. A table which the file cuts short is returned as
// far as it goes.
string
ot_find_table (const char* buf, int n, const char* tag) {
  if (buf == NULL || n < 12) return "";
  long long h= 0;
  if (raw_tag (buf, 0, "ttcf")) {
    if (n < 16 || raw_U32 (buf, 8) == 0) return "";
    h= raw_U32 (buf, 12);
    if (h + 12 > n) return "";
  }
  int hh= (int) h;
  if (!(raw_U32 (buf, hh) == 0x00010000 || raw_tag (buf, hh, "OTTO") ||
        raw_tag (buf, hh, "true") || raw_tag (buf, hh, "typ1")))
    return "";
  int nr= (int) raw_U16 (buf, hh + 4);
  for (int k= 0; k < nr; k++) {
    long long rec= h + 12 + 16 * k;
    if (rec + 16 > n) return "";
    if (!raw_tag (buf, (int) rec, tag)) continue;
    long long start= raw_U32 (buf, (int) rec + 8);
    long long len  = raw_U32 (buf, (int) rec + 12);
    if (start >= n) return "";
    if (len > n - start) len= n - start;
    return string (buf + start, (int) len);
  }
  return "";
}

// a helper function to parse the coverage table
// return an array of glyphID
static array<unsigned int>
parse_coverage_table (ot_table& t, long long offset) {
  int format= t.u16 (offset);
  array<unsigned int> coverage;
  if (format == 1) {
    int glyphCount= t.count (offset + 4, t.u16 (offset + 2), 2);
    for (int i= 0; i < glyphCount; i++) {
      unsigned int glyphID= t.u16 (offset + 4 + 2 * i);
      coverage << glyphID;
    }
  }
  else if (format == 2) {
    int rangeCount= t.count (offset + 4, t.u16 (offset + 2), 6);
    for (int i= 0; i < rangeCount; i++) {
      unsigned int startGlyphID= t.u16 (offset + 4 + 6 * i);
      unsigned int endGlyphID= t.u16 (offset + 4 + 6 * i + 2);
      // the ranges are disjoint, so together they cannot hold more glyphs
      // than a font has
      if (endGlyphID < startGlyphID ||
          N (coverage) + (int) (endGlyphID - startGlyphID) + 1 > 65536) {
        t.ok= false;
        break;
      }
      for (unsigned int glyphID= startGlyphID; glyphID <= endGlyphID;
           glyphID++) {
        coverage << glyphID;
      }
    }
  }
  else {
    if (t.ok && DEBUG_VERBOSE)
      debug_fonts << "parse_coverage_table : coverage format " << format
                  << " not supported." << LF;
    t.ok= false;
  }
  if (!t.ok) return array<unsigned int> ();
  return coverage;
}

// a helper function to parse the MathValueRecord
static MathValueRecord
parse_math_record (ot_table& t, long long parent_table_offset,
                   long long record_offset) {
  MathValueRecord record;
  int value= t.s16 (parent_table_offset + record_offset);
  unsigned int deviceOffset=
      t.u16 (parent_table_offset + record_offset + 2);
  // the device table is not used, and the shortest one is two bytes shorter
  // than what is read here: it is left out when the table ends before
  if (deviceOffset > 0 && t.has (parent_table_offset + deviceOffset, 8)) {
    long long deviceAbsOffset= parent_table_offset + deviceOffset;
    unsigned int startSize= t.u16 (deviceAbsOffset);
    unsigned int endSize= t.u16 (deviceAbsOffset + 2);
    unsigned int deltaFormat= t.u16 (deviceAbsOffset + 4);
    unsigned int deltaValues= t.u16 (deviceAbsOffset + 6);
    record.hasDevice= true;
    record.deviceTable= {startSize, endSize, deltaFormat, deltaValues};
  }
  record.value= value;
  return record;
}

// a helper function to parse the MathGlyphConstruction table
// return true if the sub table GlyphAssembly is not NULL
static bool
parse_construction (ot_table& t, long long construction_offset,
                    array<unsigned int>& variantGlyph,
                    array<unsigned int>& advanceMeasurement,
                    GlyphAssembly& assembly) {
  unsigned int glyphAssemblyOffset= t.u16 (construction_offset);
  int variantCount=
      t.count (construction_offset + 4, t.u16 (construction_offset + 2), 4);

  // MathGlyphVariantRecord
  for (int i= 0; i < variantCount; i++) {
    unsigned int mathGlyphVariantGlyph=
        t.u16 (construction_offset + 4 + 4*i);
    unsigned int mathGlyphVariantAdvanceMeasurement=
        t.u16 (construction_offset + 4 + 4*i + 2);
    variantGlyph << mathGlyphVariantGlyph;
    advanceMeasurement << mathGlyphVariantAdvanceMeasurement;
  }

  // GlyphAssembly table, may be NULL
  if (glyphAssemblyOffset > 0) {
    long long glyphAssemblyAbsOffset=
        construction_offset + glyphAssemblyOffset;
    assembly.italicsCorrection=
        parse_math_record (t, glyphAssemblyAbsOffset, 0);
    int partCount= t.count (glyphAssemblyAbsOffset + 6,
                            t.u16 (glyphAssemblyAbsOffset + 4), 10);
    // GlyphPart records
    for (int j= 0, offset= 6; j < partCount; j++) {
      array<unsigned int> part (5);
      for (int k= 0; k < 5; k++, offset+= 2) {
        part[k]= t.u16 (glyphAssemblyAbsOffset + offset);
      }
      assembly.partRecords << GlyphPartRecord{part[0], part[1], part[2],
                                              part[3], part[4]};
    }
    assembly.partCount= partCount;
  }
  return (glyphAssemblyOffset > 0);
}

// a helper function to parse the MathVariants table; count is the number
// of constructions the table announces for this coverage
static void
parse_variants (ot_table& t, long long var_offset, long long coverage_offset,
                long long construction_offset, int count,
                hashmap<unsigned int, array<unsigned int>>& glyph_variants,
                hashmap<unsigned int, array<unsigned int>>& glyph_variants_adv,
                hashmap<unsigned int, GlyphAssembly>& glyph_assembly) {

  auto coverage= parse_coverage_table (t, coverage_offset);
  int  coverage_N= min (N (coverage), t.count (construction_offset, count, 2));
  for (int i= 0; i < coverage_N && t.ok; i++) {
    unsigned int glyph= coverage[i];
    unsigned int glyphConstructionOffset=
        t.u16 (construction_offset + 2 * i);
    if (glyphConstructionOffset == 0) continue;
    array<unsigned int> variants;
    array<unsigned int> adv;
    GlyphAssembly assembly;
    bool has_assemply= parse_construction (t,
      var_offset + glyphConstructionOffset, variants, adv, assembly);
    glyph_variants (glyph)= variants;
    glyph_variants_adv (glyph)= adv;
    if (has_assemply) {
      glyph_assembly (glyph)= assembly;
    }
  }
}

// a helper function to parse the MathKern table
static MathKernTable
parse_math_kern_table (ot_table& t, long long offset) {
  unsigned int  heightCount= t.u16 (offset);
  // heightCount heights and one more kern value
  if (t.count (offset + 2, 2 * heightCount + 1, 4) == 0) heightCount= 0;
  MathKernTable kern (heightCount);
  for (unsigned int i= 0; i < heightCount; i++) {
    kern.correctionHeight[i]= parse_math_record (t, offset, 2 + 4 * i);
  }
  for (unsigned int i= 0; i < heightCount + 1; i++) {
    kern.kernValues[i]=
        parse_math_record (t, offset, 2 + 4 * heightCount + 4 * i);
  }
  return kern;
}

// a helper function to parse the MathKernInfo table
static void
parse_math_kern_info_table (ot_table& t, long long offset,
                            hashmap<unsigned int, MathKernInfoRecord>& table) {
  unsigned int mathKernCoverageOffset= t.u16 (offset);
  unsigned int mathKernCount         = t.u16 (offset + 2);
  if (mathKernCoverageOffset == 0) return; // no coverage, no kerning
  auto coverage= parse_coverage_table (t, offset + mathKernCoverageOffset);
  int  coverage_N= min (N (coverage), t.count (offset + 4, mathKernCount, 8));
  for (int i= 0; i < coverage_N && t.ok; i++) {
    unsigned int glyphID= coverage[i];
    MathKernInfoRecord record;
    unsigned int topRightMathKernOffset= t.u16 (offset + 4 + 8 * i);
    unsigned int topLeftMathKernOffset= t.u16 (offset + 4 + 8 * i + 2);
    unsigned int bottomRightMathKernOffset=
        t.u16 (offset + 4 + 8 * i + 4);
    unsigned int bottomLeftMathKernOffset= t.u16 (offset + 4 + 8 * i + 6);
    if (topRightMathKernOffset > 0) {
      record.topRight=
          parse_math_kern_table (t, offset + topRightMathKernOffset);
      record.hasTopRight= true;
    }
    if (topLeftMathKernOffset > 0) {
      record.topLeft=
          parse_math_kern_table (t, offset + topLeftMathKernOffset);
      record.hasTopLeft= true;
    }
    if (bottomRightMathKernOffset > 0) {
      record.bottomRight=
          parse_math_kern_table (t, offset + bottomRightMathKernOffset);
      record.hasBottomRight= true;
    }
    if (bottomLeftMathKernOffset > 0) {
      record.bottomLeft=
          parse_math_kern_table (t, offset + bottomLeftMathKernOffset);
      record.hasBottomLeft= true;
    }
    table (glyphID)= record;
  }
}

// a helper function to parse the MathValueRecords with coverage table,
// used in parsing MathItalicsCorrectionInfo and MathTopAccentAttachment;
// the number of records follows the coverage offset in the parent table
static void
parse_record_with_coverage (
    ot_table& t, long long parent_table_offset, int coverage_offset,
    int record_offset, hashmap<unsigned int, MathValueRecord>& record_map) {
  if (coverage_offset == 0) return; // NULL coverage: nothing is covered
  long long coverage_abs_offset = parent_table_offset + coverage_offset;
  array<unsigned int> coverage= parse_coverage_table (t, coverage_abs_offset);
  int count= t.u16 (parent_table_offset + 2);
  int coverage_N= min (N (coverage),
                       t.count (parent_table_offset + record_offset, count, 4));
  for (int i= 0; i < coverage_N && t.ok; i++) {
    unsigned int glyphID= coverage[i];
    record_map (glyphID)=
        parse_math_record (t, parent_table_offset, record_offset + 4 * i);
  }
}

// parse the OpenType MATH constants table.
static void
parse_constant (ot_table& t, long long offset, MathConstantsTable& table) {
  int scriptPercentScaleDown= t.s16 (offset);
  int scriptScriptPercentScaleDown= t.s16 (offset + 2);
  unsigned int delimitedSubFormulaMinHeight= t.u16 (offset + 4);
  unsigned int displayOperatorMinHeight= t.u16 (offset + 6);
  for (int i= 0; i < otmathConstantsRecordsEnd; i++) {
    table.records[i]= parse_math_record (t, offset, 8 + 4 * i);
  }
  int radicalDegreeBottomRaisePercent=
      t.s16 (offset + 8 + 4 * otmathConstantsRecordsEnd);
  table.scriptPercentScaleDown= scriptPercentScaleDown;
  table.scriptScriptPercentScaleDown= scriptScriptPercentScaleDown;
  table.delimitedSubFormulaMinHeight= delimitedSubFormulaMinHeight;
  table.displayOperatorMinHeight= displayOperatorMinHeight;
  table.radicalDegreeBottomRaisePercent= radicalDegreeBottomRaisePercent;
}

// parse the OpenType MATH table of the file held in buf. A table which
// cannot be read in full, or which has no constants, is not used at all.
ot_mathtable
parse_mathtable (const char* buf, int n) {
  string tt= ot_find_table (buf, n, "MATH");
  if (N (tt) == 0) return {};
  ot_table     t (tt);
  ot_mathtable table (tm_new<ot_mathtable_rep> ());

  // MATH Header
  table->majorVersion= t.u16 (0);
  table->minorVersion= t.u16 (2);
  int mathConstantsOffset= t.u16 (4);
  int mathGlyphInfoOffset= t.u16 (6);
  int mathVariantsOffset= t.u16 (8);
  table->minConnectorOverlap= 0;

  // version check
  if ((table->majorVersion != 1) || (table->minorVersion != 0)) return {};

  // parse MathConstants table
  if (mathConstantsOffset == 0) return {};
  parse_constant (t, mathConstantsOffset, table->constants_table);

  // MathGlyphInfo table (may be NULL)
  if (mathGlyphInfoOffset > 0) {
    int mathItalicsCorrectionInfoOffset= t.u16 (mathGlyphInfoOffset + 0);
    int mathTopAccentAttachmentOffset= t.u16 (mathGlyphInfoOffset + 2);
    int extendedShapeCoverageOffset= t.u16 (mathGlyphInfoOffset + 4);
    int mathKernInfoOffset= t.u16 (mathGlyphInfoOffset + 6);

    // MathItalicsCorrectionInfo table (may be NULL)
    if (mathItalicsCorrectionInfoOffset > 0) {
      int mathItalicsCorrectionInfoAbsOffset=
          mathGlyphInfoOffset + mathItalicsCorrectionInfoOffset;
      int italicsCorrectionCoverageOffset=
          t.u16 (mathItalicsCorrectionInfoAbsOffset + 0);
      parse_record_with_coverage (t, mathItalicsCorrectionInfoAbsOffset,
                                  italicsCorrectionCoverageOffset, 4,
                                  table->italics_correction);
    }

    // MathTopAccentAttachment table (may be NULL)
    if (mathTopAccentAttachmentOffset > 0) {
      int mathTopAccentAttachmentAbsOffset=
          mathGlyphInfoOffset + mathTopAccentAttachmentOffset;
      int topAccentCoverageOffset=
          t.u16 (mathTopAccentAttachmentAbsOffset + 0);
      parse_record_with_coverage (t, mathTopAccentAttachmentAbsOffset,
                                  topAccentCoverageOffset, 4,
                                  table->top_accent);
    }

    // ExtendedShapeCoverage table (may be NULL)
    if (extendedShapeCoverageOffset > 0) {
      auto extendedShapeCoverage= parse_coverage_table (
          t, mathGlyphInfoOffset + extendedShapeCoverageOffset);
      for (int i= 0;  i< N(extendedShapeCoverage); i++) {
        table->extended_shape_coverage->insert (extendedShapeCoverage[i]);
      }
    }

    // MathKernInfo table (may be NULL)
    if (mathKernInfoOffset > 0)
      parse_math_kern_info_table (t, mathGlyphInfoOffset + mathKernInfoOffset,
                                  table->math_kern_info);
  }

  // MathVariants table (may be NULL)
  if (mathVariantsOffset > 0) {
    table->minConnectorOverlap= t.u16 (mathVariantsOffset + 0);
    // both coverage offsets may be NULL, for a font with no vertical or no
    // horizontal variants; adding zero to the parent offset would make the
    // parser read minConnectorOverlap as a coverage format
    int vertGlyphCoverageRel = t.u16 (mathVariantsOffset + 2);
    int horizGlyphCoverageRel= t.u16 (mathVariantsOffset + 4);
    int vertGlyphCoverageOffset = mathVariantsOffset + vertGlyphCoverageRel;
    int horizGlyphCoverageOffset= mathVariantsOffset + horizGlyphCoverageRel;
    int vertGlyphCount= t.u16 (mathVariantsOffset + 6);
    int horizGlyphCount= t.u16 (mathVariantsOffset + 8);

    // parse vertical variants
    if (vertGlyphCoverageRel > 0)
      parse_variants (t, mathVariantsOffset, vertGlyphCoverageOffset,
                      mathVariantsOffset + 10, vertGlyphCount,
                      table->ver_glyph_variants,
                      table->ver_glyph_variants_adv,
                      table->ver_glyph_assembly);

    // parse horizontal variants
    if (horizGlyphCoverageRel > 0)
      parse_variants (t, mathVariantsOffset, horizGlyphCoverageOffset,
                      mathVariantsOffset + 10 + 2 * vertGlyphCount,
                      horizGlyphCount,
                      table->hor_glyph_variants,
                      table->hor_glyph_variants_adv,
                      table->hor_glyph_assembly);
  }

  if (!t.ok) return {};
  return table;
}

ot_mathtable
parse_mathtable (const string& buf) {
  string b= buf;
  if (N (b) == 0) return {};
  return parse_mathtable (&b[0], N (b));
}

/******************************************************************************
 * OpenType GSUB: substitutions of one glyph by one glyph (types 1 and 2) and
 * alternate substitutions (type 3)
 ******************************************************************************/

static void
parse_gsub_subtable (ot_table& t, int type, long long off, ot_gsub_map& m) {
  if (type == 7) { // extension: the real type and a 32 bit offset
    int       ext_type= t.u16 (off + 2);
    long long ext_off = t.u32 (off + 4);
    // an extension does not lead to another extension
    if (ext_type == 7) return;
    parse_gsub_subtable (t, ext_type, off + ext_off, m);
    return;
  }
  int format= t.u16 (off);
  // Only single (type 1), multiple (type 2) and alternate (type 3)
  // substitutions are read. In any other lookup the fields below mean
  // something else, so the coverage table must not even be parsed.
  if (!((type == 1 && (format == 1 || format == 2)) ||
        (type == 2 && format == 1) ||
        (type == 3 && format == 1)))
    return;
  int cov_off= t.u16 (off + 2);
  array<unsigned int> cov= parse_coverage_table (t, off + cov_off);
  if (type == 1 && format == 1) {
    int delta= t.s16 (off + 4);
    for (int i= 0; i < N(cov); i++)
      if (!m->contains (cov[i])) {
        array<unsigned int> a;
        a << (unsigned int) ((cov[i] + delta) & 0xffff);
        m (cov[i])= a;
      }
  }
  else if (type == 1 && format == 2) {
    int count= t.count (off + 6, t.u16 (off + 4), 2);
    for (int i= 0; i < N(cov) && i < count; i++)
      if (!m->contains (cov[i])) {
        array<unsigned int> a;
        a << (unsigned int) t.u16 (off + 6 + 2*i);
        m (cov[i])= a;
      }
  }
  else if (type == 2) {
    // a multiple substitution replaces a glyph by a sequence; some fonts
    // keep their one to one substitutions in such a lookup, and only the
    // sequences of one glyph are taken
    int count= t.count (off + 6, t.u16 (off + 4), 2);
    for (int i= 0; i < N(cov) && i < count && t.ok; i++) {
      long long seq_off= off + t.u16 (off + 6 + 2*i);
      if (t.u16 (seq_off) != 1) continue;
      if (!m->contains (cov[i])) {
        array<unsigned int> a;
        a << (unsigned int) t.u16 (seq_off + 2);
        m (cov[i])= a;
      }
    }
  }
  else if (type == 3 && format == 1) {
    int count= t.count (off + 6, t.u16 (off + 4), 2);
    for (int i= 0; i < N(cov) && i < count && t.ok; i++) {
      long long set_off= off + t.u16 (off + 6 + 2*i);
      int n= t.count (set_off + 2, t.u16 (set_off), 2);
      array<unsigned int> alts;
      for (int k= 0; k < n; k++)
        alts << (unsigned int) t.u16 (set_off + 2 + 2*k);
      if (!m->contains (cov[i])) m (cov[i])= alts;
    }
  }
}

static void
insert_sorted (array<int>& a, int x) {
  int i= 0;
  while (i < N(a) && a[i] < x) i++;
  if (i < N(a) && a[i] == x) return;
  a << x;
  for (int j= N(a) - 1; j > i; j--) a[j]= a[j-1];
  a[i]= x;
}

// The feature records with the given tag which apply when no language is
// asked for: the ones of the default language system of each script. The
// records of a particular language, like the small capitals of Turkish with
// their dotted i, are left out. The Latin script comes first, then the
// default script, then the others, and the first record which maps a glyph
// decides. A table without any script gives all its records.
static array<int>
default_feature_records (ot_table& t, string feature) {
  array<int> r;
  int script_list  = t.u16 (4);
  int feature_list = t.u16 (6);
  int feature_count= t.count (feature_list + 2, t.u16 (feature_list), 6);
  int script_count = t.count (script_list + 2, t.u16 (script_list), 6);
  if (script_count == 0) {
    for (int f= 0; f < feature_count; f++)
      if (t.tag (feature_list + 2 + 6*f) == feature) r << f;
    return r;
  }
  for (int pass= 0; pass < 3; pass++)
    for (int s= 0; s < script_count && t.ok; s++) {
      int    rec= script_list + 2 + 6*s;
      string tag= t.tag (rec);
      int    which= (tag == "latn"? 0: (tag == "DFLT"? 1: 2));
      if (which != pass) continue;
      int script= script_list + t.u16 (rec + 4);
      int def   = t.u16 (script);
      if (def == 0) continue;
      int sys= script + def;
      int nf = t.count (sys + 6, t.u16 (sys + 4), 2);
      for (int k= 0; k < nf; k++) {
        int f= t.u16 (sys + 6 + 2*k);
        if (f >= feature_count || contains (f, r)) continue;
        if (t.tag (feature_list + 2 + 6*f) == feature) r << f;
      }
    }
  return r;
}

static ot_gsub_map
parse_gsub_feature_table (string gsub, string feature) {
  ot_gsub_map m;
  if (N (gsub) < 10) return m;
  ot_table t (gsub);
  int feature_list= t.u16 (6);
  int lookup_list = t.u16 (8);
  int lookup_count= t.count (lookup_list + 2, t.u16 (lookup_list), 2);
  array<int> recs= default_feature_records (t, feature);
  for (int i= 0; i < N(recs) && t.ok; i++) {
    int rec = feature_list + 2 + 6 * recs[i];
    int feat= feature_list + t.u16 (rec + 4);
    int nl  = t.count (feat + 4, t.u16 (feat + 2), 2);
    // the lookups of a feature apply in the order of the lookup list
    array<int> lookups;
    for (int l= 0; l < nl; l++) {
      int li= t.u16 (feat + 4 + 2*l);
      if (li < lookup_count) insert_sorted (lookups, li);
    }
    for (int l= 0; l < N(lookups) && t.ok; l++) {
      int lookup= lookup_list + t.u16 (lookup_list + 2 + 2 * lookups[l]);
      int type  = t.u16 (lookup);
      int nsub  = t.count (lookup + 6, t.u16 (lookup + 4), 2);
      for (int s= 0; s < nsub && t.ok; s++)
        parse_gsub_subtable (t, type, lookup + t.u16 (lookup + 6 + 2*s), m);
    }
  }
  if (!t.ok) return ot_gsub_map ();
  return m;
}

// The feature tags of the GSUB table for which something can be applied,
// in the order of the table and without repetitions: what a font is able
// to do, for a menu to propose. A feature made of ligatures or of
// contextual substitutions only is not listed, since they are not read.
array<string>
parse_gsub_tags (const char* buf, int n) {
  array<string> r;
  string gsub= ot_find_table (buf, n, "GSUB");
  if (N (gsub) < 10) return r;
  ot_table t (gsub);
  int feature_list = t.u16 (6);
  int feature_count= t.count (feature_list + 2, t.u16 (feature_list), 6);
  array<string> tags;
  for (int f= 0; f < feature_count; f++) {
    string tag= t.tag (feature_list + 2 + 6*f);
    if (N (tag) == 4 && !contains (tag, tags)) tags << tag;
  }
  if (!t.ok) return r;
  for (int i= 0; i < N(tags); i++)
    if (N (parse_gsub_feature_table (gsub, tags[i])) != 0) r << tags[i];
  return r;
}

array<string>
parse_gsub_tags (const string& buf) {
  string b= buf;
  if (N (b) == 0) return array<string> ();
  return parse_gsub_tags (&b[0], N (b));
}

/******************************************************************************
 * OpenType GSUB: ligatures (type 4), as the sequences of emoji
 ******************************************************************************/

static void
parse_gsub_ligature_subtable (ot_table& t, int type, long long off,
                              ot_gsub_ligatures& m) {
  if (type == 7) {
    int       ext_type= t.u16 (off + 2);
    long long ext_off = t.u32 (off + 4);
    if (ext_type == 7) return;
    parse_gsub_ligature_subtable (t, ext_type, off + ext_off, m);
    return;
  }
  if (type != 4 || t.u16 (off) != 1) return;
  array<unsigned int> cov= parse_coverage_table (t, off + t.u16 (off + 2));
  int count= t.count (off + 6, t.u16 (off + 4), 2);
  for (int i= 0; i < N(cov) && i < count && t.ok; i++) {
    long long set_off= off + t.u16 (off + 6 + 2*i);
    int n= t.count (set_off + 2, t.u16 (set_off), 2);
    array<array<unsigned int> > ligs;
    if (m->contains (cov[i])) ligs= m[cov[i]];
    for (int k= 0; k < n && t.ok; k++) {
      long long lig_off= set_off + t.u16 (set_off + 2 + 2*k);
      int nc= t.u16 (lig_off + 2);
      if (nc < 1) continue;
      array<unsigned int> lig;
      lig << (unsigned int) t.u16 (lig_off);
      for (int c= 1; c < nc; c++)
        lig << (unsigned int) t.u16 (lig_off + 4 + 2*(c-1));
      ligs << lig;
    }
    m (cov[i])= ligs;
  }
}

ot_gsub_ligatures
parse_gsub_ligatures (const char* buf, int n, string feature) {
  return parse_gsub_ligatures_table (ot_find_table (buf, n, "GSUB"), feature);
}

ot_gsub_ligatures
parse_gsub_ligatures_table (string gsub, string feature) {
  ot_gsub_ligatures m;
  if (N (gsub) < 10) return m;
  ot_table t (gsub);
  int feature_list= t.u16 (6);
  int lookup_list = t.u16 (8);
  int lookup_count= t.count (lookup_list + 2, t.u16 (lookup_list), 2);
  array<int> recs= default_feature_records (t, feature);
  array<int> lookups;
  for (int i= 0; i < N(recs) && t.ok; i++) {
    int rec = feature_list + 2 + 6 * recs[i];
    int feat= feature_list + t.u16 (rec + 4);
    int nl  = t.count (feat + 4, t.u16 (feat + 2), 2);
    for (int l= 0; l < nl; l++) {
      int li= t.u16 (feat + 4 + 2*l);
      if (li < lookup_count) insert_sorted (lookups, li);
    }
  }
  for (int l= 0; l < N(lookups) && t.ok; l++) {
    int lookup= lookup_list + t.u16 (lookup_list + 2 + 2 * lookups[l]);
    int type  = t.u16 (lookup);
    int nsub  = t.count (lookup + 6, t.u16 (lookup + 4), 2);
    for (int s= 0; s < nsub && t.ok; s++)
      parse_gsub_ligature_subtable (t, type, lookup + t.u16 (lookup + 6 + 2*s),
                                    m);
  }
  if (!t.ok) return ot_gsub_ligatures ();
  return m;
}

/******************************************************************************
 * AAT morx: the substitutions of the fonts of Apple, which have no GSUB (the
 * emoji of Apple Color Emoji join into one picture by its ligatures). The
 * subtables of the default features of the chains, of three kinds: non
 * contextual (type 4), contextual (type 1) and ligatures (type 2), each a
 * state machine on the classes of the glyphs, as Apple and HarfBuzz read
 * them; the rearrangements and the insertions (types 0 and 5) are left out
 * (Apple Color Emoji has them for features which are off).
 ******************************************************************************/

static const unsigned int AAT_DELETED= 0xffff;

// the value of a glyph in a lookup table (formats 0, 2, 4, 6, 8, 10), or -1
static int
aat_lookup (ot_table& t, long long off, unsigned int g, int nglyphs) {
  int format= t.u16 (off);
  if (format == 0) {
    if ((int) g >= nglyphs) return -1;
    return (int) t.u16 (off + 2 + 2 * (long long) g);
  }
  if (format == 2 || format == 4 || format == 6) {
    // a binary search header, then the segments (or the single glyphs)
    int unit= t.u16 (off + 2), n= t.u16 (off + 4);
    if (unit < (format == 6? 4: 6)) return -1;
    int lo= 0, hi= n - 1;
    while (lo <= hi && t.ok) {
      int mid= (lo + hi) / 2;
      long long e= off + 12 + (long long) mid * unit;
      unsigned int last= t.u16 (e);
      unsigned int first= (format == 6)? last: t.u16 (e + 2);
      if (last == 0xffff && first == 0xffff) { hi= mid - 1; continue; }
      if (g > last) lo= mid + 1;
      else if (g < first) hi= mid - 1;
      else if (format == 6) return (int) t.u16 (e + 2);
      else if (format == 2) return (int) t.u16 (e + 4);
      else return (int) t.u16 (off + t.u16 (e + 4) + 2 * (long long) (g - first));
    }
    return -1;
  }
  if (format == 8) {
    unsigned int first= t.u16 (off + 2), cnt= t.u16 (off + 4);
    if (g < first || g >= first + cnt) return -1;
    return (int) t.u16 (off + 6 + 2 * (long long) (g - first));
  }
  if (format == 10) {
    int unit= t.u16 (off + 2);
    unsigned int first= t.u16 (off + 4), cnt= t.u16 (off + 6);
    if (g < first || g >= first + cnt) return -1;
    long long e= off + 8 + (long long) (g - first) * unit;
    if (unit == 1) return (int) (t.u16 (e) >> 8);
    if (unit == 2) return (int) t.u16 (e);
    if (unit == 4) return (int) t.u32 (e);
    return -1;
  }
  return -1;
}

// the class of the glyph at i in a state table (0 the end of the text, 1 out
// of the table, 2 a deleted glyph)
static int
aat_class (ot_table& t, long long cls, array<unsigned int>& g, int i,
           int nglyphs) {
  if (i >= N(g)) return 0;
  if (g[i] == AAT_DELETED) return 2;
  int c= aat_lookup (t, cls, g[i], nglyphs);
  return (c < 0)? 1: c;
}

static void
aat_noncontextual (ot_table& t, long long body, array<unsigned int>& g,
                   int nglyphs) {
  for (int i= 0; i < N(g); i++)
    if (g[i] != AAT_DELETED) {
      int r= aat_lookup (t, body, g[i], nglyphs);
      if (r >= 0) g[i]= (unsigned int) r;
    }
}

static void
aat_contextual (ot_table& t, long long body, array<unsigned int>& g,
                int nglyphs) {
  unsigned int ncl= t.u32 (body);
  long long cls= body + t.u32 (body + 4), sa= body + t.u32 (body + 8);
  long long et = body + t.u32 (body + 12), st= body + t.u32 (body + 16);
  int state= 0, mark= 0, i= 0, guard= 8 * N(g) + 64;
  bool mark_set= false;
  while (t.ok && guard-- > 0) {
    int klass= aat_class (t, cls, g, i, nglyphs);
    int e= t.u16 (sa + 2 * ((long long) state * ncl + klass));
    long long en= et + 8 * (long long) e;
    int new_state= t.u16 (en), flags= t.u16 (en + 2);
    int mark_index= t.u16 (en + 4), cur_index= t.u16 (en + 6);
    if (i < N(g) || mark_set) {
      if (mark_index != 0xffff && mark < N(g) && g[mark] != AAT_DELETED) {
        int r= aat_lookup (t, st + t.u32 (st + 4 * (long long) mark_index),
                           g[mark], nglyphs);
        if (r >= 0) g[mark]= (unsigned int) r;
      }
      int cur= min (i, N(g) - 1);
      if (cur_index != 0xffff && cur >= 0 && g[cur] != AAT_DELETED) {
        int r= aat_lookup (t, st + t.u32 (st + 4 * (long long) cur_index),
                           g[cur], nglyphs);
        if (r >= 0) g[cur]= (unsigned int) r;
      }
    }
    if (flags & 0x8000) { mark_set= true; mark= i; }
    state= new_state;
    if (i >= N(g)) break;
    if (!(flags & 0x4000)) i++;
  }
}

static void
aat_ligature (ot_table& t, long long body, array<unsigned int>& g,
              int nglyphs) {
  unsigned int ncl= t.u32 (body);
  long long cls= body + t.u32 (body + 4), sa= body + t.u32 (body + 8);
  long long et = body + t.u32 (body + 12), la= body + t.u32 (body + 16);
  long long ct = body + t.u32 (body + 20), lt= body + t.u32 (body + 24);
  array<int> stack;  // the positions of the components
  int state= 0, i= 0, guard= 8 * N(g) + 64;
  while (t.ok && guard-- > 0) {
    int klass= aat_class (t, cls, g, i, nglyphs);
    int e= t.u16 (sa + 2 * ((long long) state * ncl + klass));
    long long en= et + 6 * (long long) e;
    int new_state= t.u16 (en), flags= t.u16 (en + 2), act= t.u16 (en + 4);
    // (a glyph which does not advance is not a component twice)
    if ((flags & 0x8000) && i < N(g) &&
        (N(stack) == 0 || stack[N(stack) - 1] != i)) {
      if (N(stack) >= 32) {
        for (int k= 1; k < N(stack); k++) stack[k-1]= stack[k];
        stack->resize (N(stack) - 1);
      }
      stack << i;
    }
    if ((flags & 0x2000) && N(stack) > 0) {
      long long a= la + 4 * (long long) act;
      unsigned int lig_index= 0, action= 0;
      int cursor= N(stack);
      do {
        if (cursor == 0) { stack= array<int> (); break; }
        cursor--;
        int pos= stack[cursor];
        action= t.u32 (a); a += 4;
        int offset= (int) (action & 0x3fffffff);
        if (offset & 0x20000000) offset |= (int) 0xc0000000;
        lig_index += t.u16 (ct + 2 * ((long long) g[pos] + offset));
        if (action & 0xc0000000) {
          // store or last: the ligature at the place of this component,
          // which stays on the stack, and the components after it deleted
          g[pos]= t.u16 (lt + 2 * (long long) lig_index);
          for (int k= N(stack) - 1; k > cursor; k--) g[stack[k]]= AAT_DELETED;
          stack->resize (cursor + 1);
        }
      }
      while (!(action & 0x80000000) && t.ok);
    }
    state= new_state;
    if (i >= N(g)) break;
    if (!(flags & 0x4000)) i++;
  }
}

array<unsigned int>
aat_morx_apply (string morx, array<unsigned int> glyphs, int nglyphs) {
  ot_table t (morx);
  if (N(morx) < 8 || t.u16 (0) < 2) return glyphs;
  array<unsigned int> g= copy (glyphs);
  unsigned int nchains= t.u32 (4);
  long long p= 8;
  for (unsigned int c= 0; c < nchains && t.ok; c++) {
    unsigned int flags= t.u32 (p), clen= t.u32 (p + 4);
    unsigned int nfeat= t.u32 (p + 8), nsub= t.u32 (p + 12);
    long long q= p + 16 + 12 * (long long) nfeat;
    for (unsigned int s= 0; s < nsub && t.ok; s++) {
      unsigned int len= t.u32 (q), cov= t.u32 (q + 4), sff= t.u32 (q + 8);
      long long body= q + 12;
      if ((sff & flags) != 0) {
        bool reverse= (cov & 0x40000000) != 0;
        if (reverse) for (int k= 0; k < N(g)/2; k++) {
          unsigned int x= g[k]; g[k]= g[N(g)-1-k]; g[N(g)-1-k]= x; }
        switch (cov & 0xff) {
        case 1: aat_contextual (t, body, g, nglyphs); break;
        case 2: aat_ligature (t, body, g, nglyphs); break;
        case 4: aat_noncontextual (t, body, g, nglyphs); break;
        default: break;
        }
        if (reverse) for (int k= 0; k < N(g)/2; k++) {
          unsigned int x= g[k]; g[k]= g[N(g)-1-k]; g[N(g)-1-k]= x; }
      }
      if (len < 12) break;
      q += len;
    }
    if (clen < 16) break;
    p += clen;
  }
  if (!t.ok) return glyphs;
  array<unsigned int> r;
  for (int k= 0; k < N(g); k++) if (g[k] != AAT_DELETED) r << g[k];
  return r;
}

ot_gsub_map
parse_gsub_feature (const char* buf, int n, string feature) {
  return parse_gsub_feature_table (ot_find_table (buf, n, "GSUB"), feature);
}

ot_gsub_map
parse_gsub_feature (const string& buf, string feature) {
  string b= buf;
  if (N (b) == 0) return ot_gsub_map ();
  return parse_gsub_feature (&b[0], N (b), feature);
}

/******************************************************************************
 * OpenType GPOS: pair kerning
 ******************************************************************************/

bool
ot_gpos_kern_rep::empty () {
  return N (lookups) == 0;
}

// Each lookup of the feature moves the pair on its own, so their values
// add up. Inside a lookup the first subtable which knows the pair decides:
// a subtable of pairs when it lists the pair, a subtable of classes as soon
// as it covers the left glyph.
int
ot_gpos_kern_rep::get (unsigned int left, unsigned int right) {
  unsigned int key= (left << 16) | (right & 0xffff);
  if (cache->contains (key)) return cache[key];
  int r= 0;
  for (int i= 0; i < N (lookups); i++) {
    array<ot_kern_subtable>& subs= lookups[i];
    for (int j= 0; j < N (subs); j++) {
      ot_kern_subtable& k= subs[j];
      if (!k.coverage->contains (left)) continue;
      if (!k.by_class) {
        if (!k.pairs->contains (key)) continue;
        r += k.pairs[key];
        break;
      }
      int c1= k.class1[left], c2= k.class2[right];
      if (c1 >= k.class1_count || c2 >= k.class2_count) continue;
      if (N (k.values) != 0) r += k.values[c1 * k.class2_count + c2];
      break;
    }
  }
  cache (key)= r;
  return r;
}

// number of bytes of a value record with the given value format
static int
value_record_size (int format) {
  int n= 0;
  for (int bit= 1; bit <= 0x80; bit <<= 1)
    if ((format & bit) != 0) n += 2;
  return n;
}

// offset of the x advance inside a value record, -1 when it has none
static int
x_advance_offset (int format) {
  if ((format & 0x0004) == 0) return -1;
  int off= 0;
  if ((format & 0x0001) != 0) off += 2;
  if ((format & 0x0002) != 0) off += 2;
  return off;
}

// glyph -> class; class 0 is the default and is not stored
static void
parse_class_def (ot_table& t, long long off, hashmap<unsigned int, int>& m) {
  int format= t.u16 (off);
  if (format == 1) {
    unsigned int start= t.u16 (off + 2);
    int          n    = t.count (off + 6, t.u16 (off + 4), 2);
    for (int i= 0; i < n; i++) {
      int c= t.u16 (off + 6 + 2 * i);
      if (c != 0) m (start + i)= c;
    }
  }
  else if (format == 2) {
    int n    = t.count (off + 4, t.u16 (off + 2), 6);
    int total= 0;
    for (int i= 0; i < n; i++) {
      unsigned int s= t.u16 (off + 4 + 6 * i);
      unsigned int e= t.u16 (off + 4 + 6 * i + 2);
      int          c= t.u16 (off + 4 + 6 * i + 4);
      // the ranges are disjoint, as those of a coverage
      if (e < s || total + (int) (e - s) + 1 > 65536) {
        t.ok= false;
        break;
      }
      total += (int) (e - s) + 1;
      if (c != 0)
        for (unsigned int g= s; g <= e; g++) m (g)= c;
    }
  }
}

// read one pair adjustment subtable; false when there is nothing to keep
static bool
parse_pair_pos (ot_table& t, long long off, ot_kern_subtable& k) {
  int format  = t.u16 (off);
  if (format != 1 && format != 2) return false;
  int cov_off = t.u16 (off + 2);
  int vf1     = t.u16 (off + 4);
  int vf2     = t.u16 (off + 6);
  // a subtable which does not change the advance still decides for its
  // pairs, with a value of zero
  int xadv    = x_advance_offset (vf1);
  int sz1= value_record_size (vf1), sz2= value_record_size (vf2);
  array<unsigned int> cov= parse_coverage_table (t, off + cov_off);
  for (int i= 0; i < N (cov); i++) k.coverage->insert (cov[i]);
  if (format == 1) {
    k.by_class= false;
    int n= t.count (off + 10, t.u16 (off + 8), 2);
    for (int i= 0; i < N (cov) && i < n && t.ok; i++) {
      long long set_off= off + t.u16 (off + 10 + 2 * i);
      int m= t.count (set_off + 2, t.u16 (set_off), 2 + sz1 + sz2);
      for (int j= 0; j < m; j++) {
        long long    rec   = set_off + 2 + j * (2 + sz1 + sz2);
        unsigned int second= t.u16 (rec);
        unsigned int key   = (cov[i] << 16) | second;
        int          v     = (xadv < 0? 0: t.s16 (rec + 2 + xadv));
        if (!k.pairs->contains (key)) k.pairs (key)= v;
      }
    }
  }
  else {
    k.by_class= true;
    parse_class_def (t, off + t.u16 (off + 8), k.class1);
    parse_class_def (t, off + t.u16 (off + 10), k.class2);
    k.class1_count= t.u16 (off + 12);
    k.class2_count= t.u16 (off + 14);
    if (k.class1_count <= 0 || k.class2_count <= 0) return false;
    int rec_size= sz1 + sz2;
    long long nr= ((long long) k.class1_count) * k.class2_count;
    if (!t.has (off + 16, nr * rec_size)) {
      t.ok= false;
      return false;
    }
    if (xadv >= 0) {
      k.values= array<int> ((int) nr);
      long long base= off + 16;
      for (int a= 0; a < k.class1_count; a++)
        for (int b= 0; b < k.class2_count; b++) {
          long long rec= base + ((long long) (a * k.class2_count + b)) * rec_size;
          k.values[a * k.class2_count + b]= t.s16 (rec + xadv);
        }
    }
  }
  return t.ok;
}

ot_gpos_kern
parse_gpos_kern (const char* buf, int n) {
  string gpos= ot_find_table (buf, n, "GPOS");
  if (N (gpos) < 10) return {};
  ot_table t (gpos);
  ot_gpos_kern r (tm_new<ot_gpos_kern_rep> ());
  int feature_list= t.u16 (6);
  int lookup_list = t.u16 (8);
  int feature_count= t.count (feature_list + 2, t.u16 (feature_list), 6);
  int lookup_count = t.count (lookup_list + 2, t.u16 (lookup_list), 2);
  // the lookups of all 'kern' features, in the order of the lookup list
  array<int> lookups;
  for (int f= 0; f < feature_count && t.ok; f++) {
    int rec= feature_list + 2 + 6 * f;
    if (t.tag (rec) != "kern") continue;
    int feat= feature_list + t.u16 (rec + 4);
    int nl  = t.count (feat + 4, t.u16 (feat + 2), 2);
    for (int l= 0; l < nl; l++) {
      int li= t.u16 (feat + 4 + 2 * l);
      if (li < lookup_count) insert_sorted (lookups, li);
    }
  }
  for (int l= 0; l < N (lookups) && t.ok; l++) {
    int lookup= lookup_list + t.u16 (lookup_list + 2 + 2 * lookups[l]);
    int type  = t.u16 (lookup);
    int nsub  = t.count (lookup + 6, t.u16 (lookup + 4), 2);
    array<ot_kern_subtable> subs;
    for (int s= 0; s < nsub && t.ok; s++) {
      long long sub= lookup + t.u16 (lookup + 6 + 2 * s);
      if (type == 9) { // extension positioning
        if (t.u16 (sub + 2) != 2) continue;
        sub += (long long) t.u32 (sub + 4);
      }
      else if (type != 2) continue;
      ot_kern_subtable k;
      if (parse_pair_pos (t, sub, k)) subs << k;
    }
    if (N (subs) != 0) r->lookups << subs;
  }
  if (!t.ok) return {};
  return r;
}

ot_gpos_kern
parse_gpos_kern (const string& buf) {
  string b= buf;
  if (N (b) == 0) return {};
  return parse_gpos_kern (&b[0], N (b));
}

ot_mathtable
parse_mathtable (url u) {
  string tt;
  if (!load_string (u, tt, false)) return parse_mathtable (tt);
  else return {};
}

array<string>
as_hexadecimal (array<unsigned int> a) {
  array<string> as = array<string> ();
  for (int i=0; i<N(a); i++) {
    as << as_hexadecimal (a [i]);
  }
  return as;
}

void
dump_mathtable (tm_ostream& str, ot_mathtable table) {
  if (is_nil(table)) return;
  
  str << "MATH table " << table->majorVersion << " "
       << table->minorVersion << LF;
  {
    str << "Vertical variants" << LF;
    iterator<unsigned int> it= iterate (table->ver_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->ver_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->ver_glyph_variants[glyph]) << LF;
    }
    str << "Vertical assembly" << LF;
    it= iterate (table->ver_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << table->ver_glyph_assembly[glyph].partCount
           << ") : ";
      auto v= table->ver_glyph_assembly[glyph].partRecords;
      for (int j= 0; j<N(v); j++) {
        str << as_hexadecimal (v[j].glyphID) << " ";
      }
      str << LF;
    }
  }
  {
    str << "Horizontal variants" << LF;
    iterator<unsigned int> it= iterate (table->hor_glyph_variants);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " variants ("
           << N(table->hor_glyph_variants[glyph])
           << ") : " << as_hexadecimal (table->hor_glyph_variants[glyph]) << LF;
    }
    str << "Horizontal assembly" << LF;
    it= iterate (table->hor_glyph_assembly);
    while (it->busy())
    {
      int glyph= it->next();
      str << "glyph: " << as_hexadecimal (glyph) << " assembly ("
           << table->hor_glyph_assembly[glyph].partCount
           << ") : ";
      auto v= table->hor_glyph_assembly[glyph].partRecords;
      for (int j= 0; j<N(v); j++) {
        str << as_hexadecimal (v[j].glyphID) << " ";
      }
      str << LF;
    }
  }
}
