
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

#ifdef HAVE_STDINT_H
#include <stdint.h>
#define U8  uint8_t
#define U16 uint16_t
#define U32 uint32_t
#define F32 uint32_t
#define U64 uint64_t
#else
#define U8  unsigned char
#define U16 unsigned short
#define U32 unsigned int
#define U64 unsigned long long int
#endif
#ifndef S16
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

F32
get_F32 (string s, int i) {
  return (F32) get_U32 (s, i);
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
    get_F32 (tt, h) == 0x00010000 ||
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
