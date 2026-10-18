/******************************************************************************
* MODULE     : ts_xz.c
* DESCRIPTION: unpacking the tables of tree-sitter shipped compressed by xz
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The decoder of XZ Embedded (xz/, 0BSD), compiled as one unit, with the
   checks CRC32 and CRC64 (the default of the xz command). */

#define XZ_USE_CRC64
#include "xz/xz_crc32.c"
#include "xz/xz_crc64.c"
#include "xz/xz_dec_lzma2.c"
#include "xz/xz_dec_stream.c"

#include "ts_dynamic.h"

long
ts_xz_decompress (const unsigned char* in, size_t n, unsigned char** out) {
  /* the content of the xz stream in[0..n): its length, the bytes in *out
     (allocated with malloc), or -1 when the stream is not valid */
  static int initialized= 0;
  if (!initialized) { xz_crc32_init (); xz_crc64_init (); initialized= 1; }
  struct xz_dec* dec= xz_dec_init (XZ_DYNALLOC, (uint32_t) 1 << 28);
  if (dec == NULL) return -1;
  size_t size= n * 8 + 4096, used= 0;
  unsigned char* buf= (unsigned char*) malloc (size);
  struct xz_buf b;
  b.in= in; b.in_pos= 0; b.in_size= n;
  enum xz_ret r= XZ_OK;
  while (buf != NULL) {
    b.out= buf; b.out_pos= used; b.out_size= size;
    r= xz_dec_run (dec, &b);
    used= b.out_pos;
    if (r != XZ_OK) break;
    if (used == size) {
      unsigned char* bigger= (unsigned char*) realloc (buf, size * 2);
      if (bigger == NULL) { free (buf); buf= NULL; break; }
      buf= bigger; size *= 2;
    }
    else if (b.in_pos == b.in_size) { r= XZ_DATA_ERROR; break; }
  }
  xz_dec_end (dec);
  if (buf == NULL || r != XZ_STREAM_END) { free (buf); return -1; }
  *out= buf;
  return (long) used;
}
