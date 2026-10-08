
/******************************************************************************
* MODULE     : zip_files.cpp
* DESCRIPTION: reading and writing zip archives
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "zip_files.hpp"
#include "file.hpp"

/******************************************************************************
* Inflate (RFC 1951): the three kinds of blocks, with canonical Huffman codes
* decoded one bit at a time. Simple before fast: the files are small.
******************************************************************************/

#define ZIP_MAX_BITS 15
#define ZIP_MAX_LCODES 286
#define ZIP_MAX_DCODES 30
#define ZIP_FIX_LCODES 288

struct inflate_state {
  string in;         // the compressed data
  int    pos, end;   // where it is read, and where it ends
  int    bit_buf;    // the bits which were read and not used
  int    bit_cnt;
  string out;        // the result, of a size which is known
  int    out_pos;
  bool   error;
};

struct huffman_code {
  short count  [ZIP_MAX_BITS + 1];  // the number of codes of each length
  short symbol [ZIP_FIX_LCODES];    // the symbols, ordered by their code
};

static int
inflate_bits (inflate_state& s, int need) {
  // need <= 16
  long val= s.bit_buf;
  while (s.bit_cnt < need) {
    if (s.pos >= s.end) { s.error= true; return 0; }
    val |= ((long) (unsigned char) s.in[s.pos++]) << s.bit_cnt;
    s.bit_cnt += 8;
  }
  s.bit_buf= (int) (val >> need);
  s.bit_cnt -= need;
  return (int) (val & ((1L << need) - 1));
}

static int
inflate_symbol (inflate_state& s, huffman_code& h) {
  int code= 0, first= 0, index= 0;
  for (int len= 1; len <= ZIP_MAX_BITS; len++) {
    code |= inflate_bits (s, 1);
    if (s.error) return -1;
    int count= h.count[len];
    if (code - count < first) return h.symbol[index + (code - first)];
    index += count;
    first += count;
    first <<= 1;
    code <<= 1;
  }
  s.error= true;
  return -1;
}

static bool
huffman_build (huffman_code& h, short* length, int n) {
  // the code from the lengths of its n symbols; false for lengths which
  // describe more codes than there are
  short offs[ZIP_MAX_BITS + 1];
  for (int len= 0; len <= ZIP_MAX_BITS; len++) h.count[len]= 0;
  for (int sym= 0; sym < n; sym++) h.count[length[sym]]++;
  int left= 1;
  for (int len= 1; len <= ZIP_MAX_BITS; len++) {
    left <<= 1;
    left -= h.count[len];
    if (left < 0) return false;
  }
  offs[1]= 0;
  for (int len= 1; len < ZIP_MAX_BITS; len++)
    offs[len + 1]= offs[len] + h.count[len];
  for (int sym= 0; sym < n; sym++)
    if (length[sym] != 0) h.symbol[offs[length[sym]]++]= (short) sym;
  return true;
}

static void
inflate_codes (inflate_state& s, huffman_code& lcode, huffman_code& dcode) {
  static const short lens[29]= {
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 };
  static const short lext[29]= {
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
  static const short dists[30]= {
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577 };
  static const short dext[30]= {
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 };
  int out_size= N (s.out);
  while (true) {
    int sym= inflate_symbol (s, lcode);
    if (s.error) return;
    if (sym < 256) {
      if (s.out_pos >= out_size) { s.error= true; return; }
      s.out[s.out_pos++]= (char) sym;
    }
    else if (sym == 256) return;
    else {
      sym -= 257;
      if (sym >= 29) { s.error= true; return; }
      int len= lens[sym] + inflate_bits (s, lext[sym]);
      int dsym= inflate_symbol (s, dcode);
      if (s.error || dsym >= 30) { s.error= true; return; }
      int dist= dists[dsym] + inflate_bits (s, dext[dsym]);
      if (s.error || dist > s.out_pos || s.out_pos + len > out_size) {
        s.error= true; return; }
      for (int i= 0; i < len; i++, s.out_pos++)
        s.out[s.out_pos]= s.out[s.out_pos - dist];
    }
  }
}

static void
inflate_stored (inflate_state& s) {
  s.bit_buf= 0; s.bit_cnt= 0;
  if (s.pos + 4 > s.end) { s.error= true; return; }
  int len= ((unsigned char) s.in[s.pos]) | (((unsigned char) s.in[s.pos+1]) << 8);
  int nlen= ((unsigned char) s.in[s.pos+2]) | (((unsigned char) s.in[s.pos+3]) << 8);
  s.pos += 4;
  if (len != ((~nlen) & 0xffff) || s.pos + len > s.end ||
      s.out_pos + len > N (s.out)) { s.error= true; return; }
  for (int i= 0; i < len; i++) s.out[s.out_pos++]= s.in[s.pos++];
}

static void
inflate_fixed (inflate_state& s) {
  static bool done= false;
  static huffman_code lcode, dcode;
  if (!done) {
    short lengths[ZIP_FIX_LCODES];
    int sym= 0;
    for (; sym < 144; sym++) lengths[sym]= 8;
    for (; sym < 256; sym++) lengths[sym]= 9;
    for (; sym < 280; sym++) lengths[sym]= 7;
    for (; sym < ZIP_FIX_LCODES; sym++) lengths[sym]= 8;
    huffman_build (lcode, lengths, ZIP_FIX_LCODES);
    for (sym= 0; sym < ZIP_MAX_DCODES; sym++) lengths[sym]= 5;
    huffman_build (dcode, lengths, ZIP_MAX_DCODES);
    done= true;
  }
  inflate_codes (s, lcode, dcode);
}

static void
inflate_dynamic (inflate_state& s) {
  static const short order[19]= {
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };
  short lengths[ZIP_MAX_LCODES + ZIP_MAX_DCODES];
  huffman_code lcode, dcode;
  int nlen = inflate_bits (s, 5) + 257;
  int ndist= inflate_bits (s, 5) + 1;
  int ncode= inflate_bits (s, 4) + 4;
  if (s.error || nlen > ZIP_MAX_LCODES || ndist > ZIP_MAX_DCODES) {
    s.error= true; return; }
  int index= 0;
  for (; index < ncode; index++) lengths[order[index]]= (short) inflate_bits (s, 3);
  for (; index < 19; index++) lengths[order[index]]= 0;
  if (s.error || !huffman_build (lcode, lengths, 19)) { s.error= true; return; }
  index= 0;
  while (index < nlen + ndist) {
    int sym= inflate_symbol (s, lcode);
    if (s.error) return;
    if (sym < 16) lengths[index++]= (short) sym;
    else {
      int len= 0, rep;
      if (sym == 16) {
        if (index == 0) { s.error= true; return; }
        len= lengths[index - 1];
        rep= 3 + inflate_bits (s, 2);
      }
      else if (sym == 17) rep= 3 + inflate_bits (s, 3);
      else rep= 11 + inflate_bits (s, 7);
      if (s.error || index + rep > nlen + ndist) { s.error= true; return; }
      while (rep-- > 0) lengths[index++]= (short) len;
    }
  }
  if (lengths[256] == 0) { s.error= true; return; }
  // (codes which are not complete are allowed: a single distance)
  if (!huffman_build (lcode, lengths, nlen)) { s.error= true; return; }
  if (!huffman_build (dcode, lengths + nlen, ndist)) { s.error= true; return; }
  inflate_codes (s, lcode, dcode);
}

bool
inflate_string (string in, int start, int size, int out_size, string& out) {
  // the out_size bytes which the size bytes of in from start on inflate to
  if (start < 0 || size < 0 || start + size > N (in) || out_size < 0)
    return false;
  inflate_state s;
  s.in= in; s.pos= start; s.end= start + size;
  s.bit_buf= 0; s.bit_cnt= 0;
  s.out= string (out_size); s.out_pos= 0;
  s.error= false;
  int last;
  do {
    last= inflate_bits (s, 1);
    int type= inflate_bits (s, 2);
    if (s.error) break;
    if (type == 0) inflate_stored (s);
    else if (type == 1) inflate_fixed (s);
    else if (type == 2) inflate_dynamic (s);
    else s.error= true;
  } while (!last && !s.error);
  if (s.error || s.out_pos != out_size) return false;
  out= s.out;
  return true;
}

/******************************************************************************
* CRC-32
******************************************************************************/

unsigned int
crc32_string (string s) {
  static bool done= false;
  static unsigned int table[256];
  if (!done) {
    for (unsigned int i= 0; i < 256; i++) {
      unsigned int c= i;
      for (int k= 0; k < 8; k++)
        c= (c & 1) ? (0xedb88320u ^ (c >> 1)) : (c >> 1);
      table[i]= c;
    }
    done= true;
  }
  unsigned int c= 0xffffffffu;
  int n= N (s);
  for (int i= 0; i < n; i++)
    c= table[(c ^ (unsigned char) s[i]) & 0xff] ^ (c >> 8);
  return c ^ 0xffffffffu;
}

/******************************************************************************
* The directory of an archive
******************************************************************************/

static unsigned int
zip_u16 (string s, int i) {
  return ((unsigned int) (unsigned char) s[i]) |
         (((unsigned int) (unsigned char) s[i+1]) << 8);
}

static unsigned int
zip_u32 (string s, int i) {
  return zip_u16 (s, i) | (zip_u16 (s, i+2) << 16);
}

struct zip_entry {
  string name;
  int    method;   // 0: stored, 8: deflated
  int    csize;    // the size in the archive
  int    usize;    // the size of the data
  int    offset;   // where the local header is
};

static int
zip_directory_end (string zip) {
  // the record which ends the archive: it is searched from the end, since
  // a comment of up to 64 Kb may follow it
  int n= N (zip);
  for (int i= n - 22; i >= 0 && i >= n - 22 - 65535; i--)
    if (zip_u32 (zip, i) == 0x06054b50u) return i;
  return -1;
}

static bool
zip_directory (string zip, array<zip_entry>& dir) {
  int n= N (zip);
  int e= zip_directory_end (zip);
  if (e < 0) return false;
  int count= (int) zip_u16 (zip, e + 10);
  int pos  = (int) zip_u32 (zip, e + 16);
  for (int k= 0; k < count; k++) {
    if (pos < 0 || pos + 46 > n || zip_u32 (zip, pos) != 0x02014b50u)
      return false;
    zip_entry z;
    z.method= (int) zip_u16 (zip, pos + 10);
    z.csize = (int) zip_u32 (zip, pos + 20);
    z.usize = (int) zip_u32 (zip, pos + 24);
    int nlen= (int) zip_u16 (zip, pos + 28);
    int xlen= (int) zip_u16 (zip, pos + 30);
    int clen= (int) zip_u16 (zip, pos + 32);
    z.offset= (int) zip_u32 (zip, pos + 42);
    if (pos + 46 + nlen > n || z.csize < 0 || z.usize < 0 || z.offset < 0)
      return false;
    z.name= zip (pos + 46, pos + 46 + nlen);
    dir << z;
    pos += 46 + nlen + xlen + clen;
  }
  return true;
}

bool
zip_is_archive (string zip) {
  return N (zip) >= 22 && zip_u32 (zip, 0) == 0x04034b50u &&
         zip_directory_end (zip) >= 0;
}

array<string>
zip_entries (string zip) {
  array<zip_entry> dir;
  array<string> r;
  if (!zip_directory (zip, dir)) return r;
  for (int i= 0; i < N (dir); i++) r << dir[i].name;
  return r;
}

bool
zip_read (string zip, string name, string& data) {
  array<zip_entry> dir;
  if (!zip_directory (zip, dir)) return false;
  int n= N (zip);
  for (int i= 0; i < N (dir); i++)
    if (dir[i].name == name) {
      zip_entry& z= dir[i];
      // the sizes of the local header are its own
      if (z.offset + 30 > n || zip_u32 (zip, z.offset) != 0x04034b50u)
        return false;
      int start= z.offset + 30 + (int) zip_u16 (zip, z.offset + 26) +
                 (int) zip_u16 (zip, z.offset + 28);
      if (start < 0 || start + z.csize > n) return false;
      if (z.method == 0) {
        if (z.csize != z.usize) return false;
        data= zip (start, start + z.csize);
        return true;
      }
      if (z.method == 8)
        return inflate_string (zip, start, z.csize, z.usize, data);
      return false;
    }
  return false;
}

/******************************************************************************
* Writing an archive, with its entries stored
******************************************************************************/

static void
zip_put16 (string& s, unsigned int x) {
  s << (char) (x & 0xff) << (char) ((x >> 8) & 0xff);
}

static void
zip_put32 (string& s, unsigned int x) {
  zip_put16 (s, x & 0xffff);
  zip_put16 (s, (x >> 16) & 0xffff);
}

string
zip_write (array<string> names, array<string> datas) {
  // The entries in their order: the office formats want some first. The
  // date is always the same one (1 January 1980), so that the same entries
  // make the same archive.
  string r, dir;
  int n= min (N (names), N (datas));
  for (int i= 0; i < n; i++) {
    unsigned int crc= crc32_string (datas[i]);
    unsigned int size= (unsigned int) N (datas[i]);
    unsigned int offset= (unsigned int) N (r);
    zip_put32 (r, 0x04034b50u);
    zip_put16 (r, 10);            // version needed
    zip_put16 (r, 0x0800);        // flags: the names are in UTF-8
    zip_put16 (r, 0);             // stored
    zip_put16 (r, 0);             // time
    zip_put16 (r, 0x0021);        // date
    zip_put32 (r, crc);
    zip_put32 (r, size);
    zip_put32 (r, size);
    zip_put16 (r, (unsigned int) N (names[i]));
    zip_put16 (r, 0);             // no extra field
    r << names[i] << datas[i];
    zip_put32 (dir, 0x02014b50u);
    zip_put16 (dir, 20);          // version made by
    zip_put16 (dir, 10);          // version needed
    zip_put16 (dir, 0x0800);
    zip_put16 (dir, 0);
    zip_put16 (dir, 0);
    zip_put16 (dir, 0x0021);
    zip_put32 (dir, crc);
    zip_put32 (dir, size);
    zip_put32 (dir, size);
    zip_put16 (dir, (unsigned int) N (names[i]));
    zip_put16 (dir, 0);           // no extra field
    zip_put16 (dir, 0);           // no comment
    zip_put16 (dir, 0);           // disk
    zip_put16 (dir, 0);           // internal attributes
    zip_put32 (dir, 0);           // external attributes
    zip_put32 (dir, offset);
    dir << names[i];
  }
  unsigned int dir_offset= (unsigned int) N (r);
  r << dir;
  zip_put32 (r, 0x06054b50u);
  zip_put16 (r, 0);
  zip_put16 (r, 0);
  zip_put16 (r, (unsigned int) n);
  zip_put16 (r, (unsigned int) n);
  zip_put32 (r, (unsigned int) N (dir));
  zip_put32 (r, dir_offset);
  zip_put16 (r, 0);
  return r;
}

/******************************************************************************
* Archives in files. The last one which was read is kept: a converter reads
* the entries of an archive one after the other.
******************************************************************************/

static string zip_cache_name;
static int    zip_cache_date= 0;
static string zip_cache_data;

static bool
zip_file_load (url u, string& zip) {
  string name= as_string (u);
  int date= last_modified (u, false);
  if (name == zip_cache_name && date == zip_cache_date && N (zip_cache_data) > 0) {
    zip= zip_cache_data;
    return true;
  }
  if (load_string (u, zip, false)) return false;
  zip_cache_name= name;
  zip_cache_date= date;
  zip_cache_data= zip;
  return true;
}

array<string>
zip_file_entries (url u) {
  string zip;
  if (!zip_file_load (u, zip)) return array<string> ();
  return zip_entries (zip);
}

string
zip_file_read (url u, string name) {
  string zip, data;
  if (!zip_file_load (u, zip)) return "";
  if (!zip_read (zip, name, data)) return "";
  return data;
}

bool
zip_file_has (url u, string name) {
  array<string> l= zip_file_entries (u);
  for (int i= 0; i < N (l); i++)
    if (l[i] == name) return true;
  return false;
}

bool
zip_file_write (url u, array<string> names, array<string> datas) {
  // true when the archive was written
  zip_cache_name= "";
  zip_cache_data= "";
  return !save_string (u, zip_write (names, datas), false);
}
