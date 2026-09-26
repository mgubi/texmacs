
/******************************************************************************
* MODULE     : mupdf_type1.c
* DESCRIPTION: Subsetting a Type 1 font, around pdfTeX's writet1.c
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* mupdf_writet1.c is pdfTeX's Type 1 subsetter, kept as it is there. This
* file is everything it expects around it: memory, the file it reads, the
* buffer it writes, the ordered sets it calls AVL trees, and the reporting.
* MuPDF subsets TrueType and CFF but not Type 1, and every TeX font is a
* Type 1, so without this a document embeds some eighty kilobytes per font
* instead of one or two (see docs/pdf-output-with-mupdf.md).
*
******************************************************************************/

#include "mupdf_writet1.h"
#include <stdarg.h>
#include <setjmp.h>

void writet1 (fd_entry* fd);    /* mupdf_writet1.c */
void t1_free (void);

/******************************************************************************
* Memory
******************************************************************************/

static jmp_buf t1_jump;
static const char* t1_error= NULL;
static int t1_jump_set= 0;

void*
xmalloc_t1 (size_t n) {
  void* p= malloc (n == 0 ? 1 : n);
  if (p == NULL) pdftex_fail ("out of memory");
  return p;
}

void*
xrealloc_t1 (void* p, size_t n) {
  void* q= realloc (p, n == 0 ? 1 : n);
  if (q == NULL) pdftex_fail ("out of memory");
  return q;
}

char*
xstrdup_t1 (const char* s) {
  char* p;
  if (s == NULL) return NULL;
  p= (char*) xmalloc_t1 (strlen (s) + 1);
  strcpy (p, s);
  return p;
}

void
xfree_t1 (void* p) { if (p != NULL) free (p); }

/******************************************************************************
* Reporting. writet1.c gives up by calling pdftex_fail, which ends the
* process in pdfTeX; here it unwinds to mupdf_t1_subset, which then leaves
* the font alone.
******************************************************************************/

static char t1_message[512];

void
pdftex_fail (const char* fmt, ...) {
  va_list args;
  va_start (args, fmt);
  vsnprintf (t1_message, sizeof (t1_message), fmt, args);
  va_end (args);
  t1_error= t1_message;
  if (t1_jump_set) longjmp (t1_jump, 1);
  else abort ();   /* cannot happen: nothing calls writet1 from elsewhere */
}

void
pdftex_warn (const char* fmt, ...) {
  va_list args;
  va_start (args, fmt);
  vsnprintf (t1_message, sizeof (t1_message), fmt, args);
  va_end (args);
  /* a warning is not fatal and the font is still written; it is reported
     through *err only when the subsetting fails outright */
}

void
tex_printf (const char* fmt, ...) { (void) fmt; }

/******************************************************************************
* The file which is read and the buffer which is written
******************************************************************************/

char*  cur_file_name= NULL;
char*  nameoffile= NULL;
size_t last_ptr_index= 0;

char*  fb_array= NULL;      /* what writet1.c has written */
static size_t fb_len= 0, fb_cap= 0;

void
fb_putchar (int c) {
  if (fb_len + 1 > fb_cap) {
    fb_cap= (fb_cap == 0) ? 0x4000 : 2 * fb_cap;
    fb_array= (char*) xrealloc_t1 (fb_array, fb_cap);
  }
  fb_array[fb_len++]= (char) c;
}

integer
fb_offset (void) { return (integer) fb_len; }

FILE*
xfopen (const char* name, const char* mode) {
  FILE* f= fopen (name, mode);
  if (f == NULL) pdftex_fail ("cannot open %s", name);
  return f;
}

void recorder_record_input (const char* name) { (void) name; }

int
t1_open_named (FILE** f, const char* name) {
  *f= fopen (name, "rb");
  return *f != NULL;
}

void
set_cur_file_name (char* name) { cur_file_name= name; }

/* the font file is given by its path, so there is nothing to look up */
ff_entry*
check_ff_exist (char* ff_name, boolean is_tt) {
  static ff_entry ff;
  (void) is_tt;
  ff.ff_name= ff_name;
  ff.ff_path= ff_name;
  return &ff;
}

/* the keys of a font descriptor; writet1.c only reads their names */
const key_entry font_key[FONT_KEYS_NUM] = {
  { "Ascent", "Ascender", 0, 0 },
  { "CapHeight", "CapHeight", 0, 0 },
  { "Descent", "Descender", 0, 0 },
  { "ItalicAngle", "ItalicAngle", 0, 0 },
  { "StemV", "StdVW", 0, 0 },
  { "XHeight", "XHeight", 0, 0 },
  { "FontBBox", "FontBBox", 0, 0 },
  { "", "", 0, 0 },
  { "", "", 0, 0 },
  { "", "", 0, 0 },
  { "FontName", "FontName", 0, 0 }
};

/******************************************************************************
* The ordered sets which writet1.c calls AVL trees. They hold a few hundred
* items and are built once, so a sorted array is enough and the whole of
* libavl is not needed.
******************************************************************************/

struct avl_table {
  int (*cmp) (const void*, const void*, void*);
  void*  param;
  void** items;
  int    n, cap;
};

struct avl_allocator { int unused; };
static struct avl_allocator the_allocator= { 0 };
struct avl_allocator* avl_xallocator= &the_allocator;

struct avl_table*
avl_create (int (*cmp) (const void*, const void*, void*),
            void* param, struct avl_allocator* alloc) {
  struct avl_table* t= (struct avl_table*) xmalloc_t1 (sizeof (struct avl_table));
  (void) alloc;
  t->cmp= cmp; t->param= param;
  t->items= NULL; t->n= 0; t->cap= 0;
  return t;
}

static int
t1_cmp_string (const void* a, const void* b, void* p) {
  (void) p;
  return strcmp ((const char*) a, (const char*) b);
}

struct avl_table*
t1_new_string_set (void) { return avl_create (t1_cmp_string, NULL, NULL); }

/* the index of item, or the index where it would go (in *found: 1 when it
   is already there) */
static int
avl_search (struct avl_table* t, const void* item, int* found) {
  int lo= 0, hi= t->n;
  *found= 0;
  while (lo < hi) {
    int mid= (lo + hi) / 2;
    int c= t->cmp (item, t->items[mid], t->param);
    if (c == 0) { *found= 1; return mid; }
    if (c < 0) hi= mid; else lo= mid + 1;
  }
  return lo;
}

void*
avl_find (struct avl_table* t, const void* item) {
  int found, i;
  if (t == NULL) return NULL;
  i= avl_search (t, item, &found);
  return found ? t->items[i] : NULL;
}

void**
avl_probe (struct avl_table* t, void* item) {
  int found, i, j;
  i= avl_search (t, item, &found);
  if (found) return &t->items[i];
  if (t->n + 1 > t->cap) {
    t->cap= (t->cap == 0) ? 64 : 2 * t->cap;
    t->items= (void**) xrealloc_t1 (t->items, (size_t) t->cap * sizeof (void*));
  }
  for (j= t->n; j > i; j--) t->items[j]= t->items[j-1];
  t->items[i]= item;
  t->n++;
  return &t->items[i];
}

void
avl_destroy (struct avl_table* t, void (*destroy) (void*, void*)) {
  int i;
  if (t == NULL) return;
  if (destroy != NULL)
    for (i=0; i<t->n; i++) destroy (t->items[i], t->param);
  xfree_t1 (t->items);
  xfree_t1 (t);
}

void
avl_t_init (struct avl_traverser* trav, struct avl_table* t) {
  trav->tab= t; trav->pos= 0;
}

void*
avl_t_first (struct avl_traverser* trav, struct avl_table* t) {
  trav->tab= t; trav->pos= 0;
  if (t == NULL || t->n == 0) return NULL;
  trav->pos= 1;
  return t->items[0];
}

void*
avl_t_next (struct avl_traverser* trav) {
  struct avl_table* t= trav->tab;
  if (t == NULL || trav->pos >= t->n) return NULL;
  return t->items[trav->pos++];
}

/******************************************************************************
* The subset tag: six upper case letters in front of the font name, which
* say that this is not the whole font. Two subsets of the same font with
* the same glyphs must get the same tag, and different glyphs a different
* one, so it is a hash of the names.
******************************************************************************/

void
make_subset_tag (fd_entry* fd) {
  unsigned int h= 2166136261u;   /* FNV-1a over the names and the font */
  struct avl_traverser t;
  char* g;
  int i;
  static char tag[8];
  for (g= (char*) avl_t_first (&t, fd->gl_tree); g != NULL;
       g= (char*) avl_t_next (&t)) {
    const char* p;
    for (p= g; *p != 0; p++) { h ^= (unsigned char) *p; h *= 16777619u; }
    h ^= '/'; h *= 16777619u;
  }
  if (fd->fontname != NULL) {
    const char* p;
    for (p= fd->fontname; *p != 0; p++) { h ^= (unsigned char) *p; h *= 16777619u; }
  }
  for (i=0; i<6; i++) { tag[i]= 'A' + (char) (h % 26); h /= 26; }
  tag[6]= 0;
  fd->subset_tag= tag;
}

/******************************************************************************
* The entry point
******************************************************************************/

static void
t1_free_item (void* item, void* param) { (void) param; xfree_t1 (item); }

unsigned char*
mupdf_t1_subset (const char* path, const char** names, int n,
                 int* size, int* len1, int* len2, int* len3,
                 char** psname, const char** err) {
  fd_entry fd;
  fm_entry fm;
  int i;
  unsigned char* res= NULL;
  extern integer t1_length1, t1_length2, t1_length3;

  if (err != NULL) *err= NULL;
  if (psname != NULL) *psname= NULL;
  t1_error= NULL;
  fb_array= NULL; fb_len= 0; fb_cap= 0;

  memset (&fd, 0, sizeof (fd));
  memset (&fm, 0, sizeof (fm));
  fm.ff_name= (char*) path;
  fm.slant= 0; fm.extend= 0;
  fm.type1= 1; fm.included= 1; fm.subsetted= 1;
  fd.fm= &fm;
  fd.fontname= NULL;
  fd.subset_tag= NULL;
  fd.gl_tree= t1_new_string_set ();
  fd.tx_tree= NULL;
  fd.all_glyphs= NULL;
  for (i=0; i<FONT_KEYS_NUM; i++) { fd.font_dim[i].val= 0; fd.font_dim[i].set= 0; }

  /* .notdef is always in a Type 1 font and writet1.c expects to find it */
  avl_probe (fd.gl_tree, xstrdup_t1 (".notdef"));
  for (i=0; i<n; i++)
    if (names[i] != NULL && names[i][0] != 0 &&
        avl_find (fd.gl_tree, names[i]) == NULL)
      avl_probe (fd.gl_tree, xstrdup_t1 (names[i]));

  t1_jump_set= 1;
  if (setjmp (t1_jump) == 0) {
    cur_file_name= (char*) path;
    writet1 (&fd);
    if (!fd.ff_found) pdftex_fail ("the font file was not found");
  }
  t1_jump_set= 0;

  if (t1_error == NULL && fb_array != NULL && fb_len > 0) {
    res= (unsigned char*) fb_array;
    if (psname != NULL && fd.subset_tag != NULL && fd.fontname != NULL) {
      /* the name the subset carries, which the PDF must repeat in its
         /BaseFont and in the /FontName of the descriptor */
      *psname= (char*) xmalloc_t1 (strlen (fd.fontname) + 8);
      sprintf (*psname, "%s+%s", fd.subset_tag, fd.fontname);
    }
    if (size != NULL) *size= (int) fb_len;
    if (len1 != NULL) *len1= t1_length1;
    if (len2 != NULL) *len2= t1_length2;
    if (len3 != NULL) *len3= t1_length3;
    fb_array= NULL;   /* the caller owns it now */
  }
  else {
    if (err != NULL) *err= (t1_error != NULL) ? t1_error : "no output";
    xfree_t1 (fb_array);
  }
  fb_array= NULL; fb_len= fb_cap= 0;

  /* the names of the glyphs are ours, the built in encoding is writet1.c's
     and holds either a name it allocated or the static notdef */
  avl_destroy (fd.gl_tree, t1_free_item);
  if (fd.builtin_glyph_names != NULL) {
    for (i=0; i<256; i++)
      if (fd.builtin_glyph_names[i] != NULL &&
          strcmp (fd.builtin_glyph_names[i], notdef) != 0)
        xfree_t1 (fd.builtin_glyph_names[i]);
    xfree_t1 (fd.builtin_glyph_names);
  }
  xfree_t1 (fd.fontname);
  t1_free ();
  return res;
}
