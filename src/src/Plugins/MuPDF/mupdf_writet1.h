
/******************************************************************************
* MODULE     : mupdf_writet1.h
* DESCRIPTION: What mupdf_writet1.c, the Type 1 subsetter of pdfTeX, expects
*              of its surroundings
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*              the macros marked "from ptexmac.h" are (C) the pdfTeX team
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* mupdf_writet1.c is pdfTeX's writet1.c, kept as close to the original as it
* can be. It was written against ptexlib.h and takes from it a font map, an
* output stream, kpathsea and GNU libavl. This header supplies the same
* names on top of the C library and of a handful of functions implemented
* in mupdf_type1.c:
*
*   - the font it reads is opened by its path,
*   - what it writes goes to a growable buffer (fb_putchar, fb_offset),
*   - the set of glyphs to keep is an ordered set of names (the "gl_tree"),
*     for which a sorted array stands in for libavl,
*   - a failure unwinds with longjmp instead of ending the process.
*
* The macros which shape the buffers are copied from pdfTeX's ptexmac.h so
* that writet1.c keeps behaving as it does there.
*
******************************************************************************/

#ifndef MUPDF_WRITET1_H
#define MUPDF_WRITET1_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>
#include <ctype.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

/******************************************************************************
* The types of pdfTeX
******************************************************************************/

typedef int integer;
typedef unsigned char byte;
typedef int boolean;
#ifndef __cplusplus
#ifndef true
#define true  1
#define false 0
#endif
#endif

#define notdef ".notdef"   /* the empty glyph slot */

/* the keys of a font descriptor (from ptexmac.h) */
#define ASCENT_CODE         0
#define CAPHEIGHT_CODE      1
#define DESCENT_CODE        2
#define ITALIC_ANGLE_CODE   3
#define STEMV_CODE          4
#define XHEIGHT_CODE        5
#define FONTBBOX1_CODE      6
#define FONTBBOX2_CODE      7
#define FONTBBOX3_CODE      8
#define FONTBBOX4_CODE      9
#define FONTNAME_CODE       10
#define FONT_KEYS_NUM       (FONTNAME_CODE + 1)

typedef struct {
  const char* pdfname;
  const char* t1name;
  float value;
  boolean valid;
} key_entry;

typedef struct {
  int val;
  boolean set;
} intparm;

extern const key_entry font_key[FONT_KEYS_NUM];

/* An ordered set of strings, which is what writet1.c uses the AVL trees of
   pdfTeX for: the glyphs to keep, and the glyphs the font turned out to
   have. The names of libavl are kept, so that the code reads as it did. */
struct avl_table;
struct avl_traverser { struct avl_table* tab; int pos; };

struct avl_table* t1_new_string_set (void);
void  avl_destroy (struct avl_table* t, void (*destroy) (void*, void*));
void* avl_find (struct avl_table* t, const void* item);
void** avl_probe (struct avl_table* t, void* item);
void  avl_t_init (struct avl_traverser* trav, struct avl_table* t);
void* avl_t_first (struct avl_traverser* trav, struct avl_table* t);
void* avl_t_next (struct avl_traverser* trav);

/* the font map entry and the font descriptor of pdfTeX, cut down to what
   writet1.c reads */
typedef struct {
  char*  ff_name;      /* the file of the font program */
  double slant, extend;
  int    type1;
  int    included;
  int    subsetted;
} fm_entry;

typedef struct {
  char* ff_name;
  char* ff_path;
} ff_entry;

typedef struct {
  fm_entry* fm;
  char*     fontname;     /* owned: writet1.c frees and replaces it */
  char*     subset_tag;   /* six upper case letters, or NULL */
  boolean   ff_found;
  intparm   font_dim[FONT_KEYS_NUM];
  struct avl_table* gl_tree;      /* the glyphs to keep, by name */
  struct avl_table* tx_tree;      /* unused here, always NULL */
  struct avl_table* all_glyphs;   /* the glyphs the font has, or NULL */
  char**    builtin_glyph_names;  /* set by writet1.c, owned by it */
} fd_entry;

#define is_type1(fm)      ((fm)->type1)
#define is_included(fm)   ((fm)->included)
#define is_subsetted(fm)  ((fm)->subsetted)
#define is_truetype(fm)   (!(fm)->type1)
#define fm_slant(fm)      ((fm)->slant)
#define fm_extend(fm)     ((fm)->extend)

/******************************************************************************
* What the surroundings provide (mupdf_type1.c)
******************************************************************************/

void* xmalloc_t1 (size_t n);
void* xrealloc_t1 (void* p, size_t n);
char* xstrdup_t1 (const char* s);
void  xfree_t1 (void* p);
#define xtalloc(n,t)       ((t*) xmalloc_t1 ((size_t) (n) * sizeof (t)))
#define xretalloc(p,n,t)   ((p)= (t*) xrealloc_t1 (p, (size_t) (n) * sizeof (t)))
#define xstrdup(s)         xstrdup_t1 (s)
#define xfree(p)           do { xfree_t1 (p); (p)= NULL; } while (0)

/* TEXMACS: writet1.c declares these two itself */
extern char*  cur_file_name;
extern size_t last_ptr_index;

void    fb_putchar (int c);
integer fb_offset (void);
extern char* fb_array;      /* what has been written so far */
int     t1_open_named (FILE** f, const char* name);
#define xfclose(f,name)   do { if (f) fclose (f); (f)= NULL; } while (0)
#define open_input(f,fmt,mode)  t1_open_named (f, cur_file_name)
#define kpse_type1_format 0
#define kpse_enc_format   0
#define FOPEN_RBIN_MODE   "rb"

void pdftex_fail (const char* fmt, ...);
void pdftex_warn (const char* fmt, ...);
void tex_printf (const char* fmt, ...);

ff_entry* check_ff_exist (char* ff_name, boolean is_tt);
FILE*     xfopen (const char* name, const char* mode);
void      recorder_record_input (const char* name);
extern char* nameoffile;
/* the glyph set replaces the AVL trees of pdfTeX: one allocator, one
   comparison, and avl_create hands back a set of strings */
struct avl_allocator;
extern struct avl_allocator* avl_xallocator;
struct avl_table* avl_create (int (*cmp) (const void*, const void*, void*),
                              void* param, struct avl_allocator* alloc);
void      set_cur_file_name (char* name);
void      make_subset_tag (fd_entry* fd);

/******************************************************************************
* The buffer macros of pdfTeX (from ptexmac.h, unchanged)
******************************************************************************/

#define SMALL_BUF_SIZE      256
#define PRINTF_BUF_SIZE     1024

#define check_buf(size, buf_size)                           \
    if ((unsigned)(size) > (unsigned)(buf_size))             \
        pdftex_fail("buffer overflow at file %s, line %d", __FILE__,  __LINE__ )

#define append_char_to_buf(c, p, buf, buf_size) do {        \
    if (c == 9)                                             \
        c = 32;                                             \
    if (c == 13 || c == EOF)                                \
        c = 10;                                             \
    if (c != ' ' || (p > buf && p[-1] != 32)) {             \
        check_buf(p - buf + 1, (buf_size));                 \
        *p++ = c;                                           \
    }                                                       \
} while (0)

#define append_eol(p, buf, buf_size) do {                   \
    check_buf(p - buf + 2, (buf_size));                     \
    if (p - buf > 1 && p[-1] != 10)                         \
        *p++ = 10;                                          \
    if (p - buf > 2 && p[-2] == 32) {                       \
        p[-2] = 10;                                         \
        p--;                                                \
    }                                                       \
    *p = 0;                                                 \
} while (0)

#define remove_eol(p, buf) do {                             \
    p = strend(buf) - 1;                                    \
    if (*p == 10)                                           \
        *p = 0;                                             \
} while (0)

#define skip(p, c)   if (*p == c)  p++

#define alloc_array(T, n, s) do {                           \
    if (T##_array == NULL) {                                \
        T##_limit = (s);                                    \
        if ((unsigned)(n) > (unsigned)(T##_limit))          \
            T##_limit = (n);                                \
        T##_array = xtalloc(T##_limit, T##_entry);          \
        T##_ptr = T##_array;                                \
    }                                                       \
    else if ((unsigned)(T##_ptr - T##_array + (n)) > (unsigned)(T##_limit)) {   \
        last_ptr_index = T##_ptr - T##_array;               \
        T##_limit *= 2;                                     \
        if ((unsigned)(T##_ptr - T##_array + (n)) > (unsigned)(T##_limit))      \
            T##_limit = T##_ptr - T##_array + (n);          \
        if ((unsigned)(T##_limit) > INT_MAX)                \
            pdftex_fail(#T "_array exceeds size limit");    \
        xretalloc(T##_array, T##_limit, T##_entry);         \
        T##_ptr = T##_array + last_ptr_index;               \
    }                                                       \
} while (0)

#define define_array(T)                                     \
T##_entry      *T##_ptr, *T##_array = NULL;                 \
size_t          T##_limit

#define strend(s)           strchr(s, 0)

static inline int
str_prefix (const char* s, const char* p) {
  return strncmp (s, p, strlen (p)) == 0;
}

/******************************************************************************
* The entry point of the subsetter
******************************************************************************/

/* Reads the Type 1 font in the file `path` and writes a subset of it which
   has the glyphs named in `names` (`n` of them, ".notdef" is always kept).
   Returns the font program, which the caller frees with free (), and puts
   the three lengths a PDF /FontFile wants in *len1, *len2, *len3, and in
   *psname the name the subset carries, "ABCDEF+Original" (the caller frees
   that too). NULL and a message in *err when the font cannot be subsetted. */
unsigned char* mupdf_t1_subset (const char* path, const char** names, int n,
                                int* size, int* len1, int* len2, int* len3,
                                char** psname, const char** err);

#ifdef __cplusplus
}
#endif

#endif /* MUPDF_WRITET1_H */
