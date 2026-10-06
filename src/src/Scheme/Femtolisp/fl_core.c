/******************************************************************************
* MODULE     : fl_core.c
* DESCRIPTION: femtolisp (aggregate compilation) and its C interface
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The sources of femtolisp (femtolisp/, see README) are compiled here as one
   unit, so that the interface below can use the internals of the interpreter
   (its stack, the type of the cvalues...). The library llt is in fl_llt.c. */

#ifndef NDEBUG
#define NDEBUG
#endif
#define USE_COMPUTED_GOTO

#include "femtolisp/flisp.c"
#include "femtolisp/builtins.c"
#include "femtolisp/string.c"
#include "femtolisp/equalhash.c"
#include "femtolisp/table.c"
#include "femtolisp/iostream.c"

#include "fl_tm.h"

fltm_value fltm_nil, fltm_true, fltm_false, fltm_unspecified;

/******************************************************************************
* Initialization
******************************************************************************/

/* (%unconstant! sym): makes a builtin redefinable, set! ignores constants */
static value_t
fltm_unconstant (value_t* args, uint32_t nargs) {
  argcount ("%unconstant!", nargs, 1);
  symbol_t* sym= tosymbol (args[0], "%unconstant!");
  sym->flags &= ~0x1;
  return FL_T;
}

/* (%gc): collects the garbage now */
static value_t
fltm_gc_builtin (value_t* args, uint32_t nargs) {
  (void) args;
  argcount ("%gc", nargs, 0);
  gc (0);
  return FL_T;
}

/* (%heap-size): the size of the heap, in bytes */
static value_t
fltm_heap_size (value_t* args, uint32_t nargs) {
  (void) args;
  argcount ("%heap-size", nargs, 0);
  return size_wrap (heapsize);
}

/******************************************************************************
* Strings of bytes
*******************************************************************************
* The strings of TeXmacs are strings of bytes, as in Guile 1.8 and S7: a
* character is a byte (a wchar between 0 and 255), and the indices count
* bytes. The string functions of femtolisp decode UTF-8 instead.
******************************************************************************/

static int
fltm_byte_of (value_t c, char* fname) {
  if (iscprim (c) && cp_class ((cprim_t*) ptr (c)) == wchartype)
    return (int) (*(uint32_t*) cp_data ((cprim_t*) ptr (c)) & 0xff);
  type_error (fname, "wchar", c);
}

static size_t
fltm_index (value_t s, value_t i, size_t max, char* fname) {
  if (!isfixnum (i) || numval (i) < 0 || (size_t) numval (i) > max)
    bounds_error (fname, s, i);
  return (size_t) numval (i);
}

/* (string char ...) */
static value_t
fltm_string_of_chars (value_t* args, uint32_t nargs) {
  uint32_t i;
  for (i=0; i<nargs; i++) (void) fltm_byte_of (args[i], "string");
  value_t r= cvalue_string (nargs);
  char* d= (char*) cvalue_data (r);
  for (i=0; i<nargs; i++) d[i]= (char) fltm_byte_of (args[i], "string");
  return r;
}

/* (list->string l) */
static value_t
fltm_list_to_string (value_t* args, uint32_t nargs) {
  argcount ("list->string", nargs, 1);
  size_t n= 0, i= 0;
  value_t l;
  for (l= args[0]; iscons (l); l= cdr_ (l), n++)
    (void) fltm_byte_of (car_ (l), "list->string");
  value_t r= cvalue_string (n);
  char* d= (char*) cvalue_data (r);
  for (l= args[0]; iscons (l); l= cdr_ (l), i++)
    d[i]= (char) fltm_byte_of (car_ (l), "list->string");
  return r;
}

/* (make-string n [char]) */
static value_t
fltm_make_string (value_t* args, uint32_t nargs) {
  if (nargs < 1 || nargs > 2) argcount ("make-string", nargs, 1);
  if (!isfixnum (args[0]) || numval (args[0]) < 0)
    type_error ("make-string", "index", args[0]);
  size_t n= (size_t) numval (args[0]);
  int c= nargs == 2? fltm_byte_of (args[1], "make-string"): ' ';
  value_t r= cvalue_string (n);
  if (n > 0) memset (cvalue_data (r), c, n);
  return r;
}

/* (string-ref s i) */
static value_t
fltm_string_ref (value_t* args, uint32_t nargs) {
  argcount ("string-ref", nargs, 2);
  if (!fl_isstring (args[0])) type_error ("string-ref", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  if (n == 0) bounds_error ("string-ref", args[0], args[1]);
  size_t i= fltm_index (args[0], args[1], n-1, "string-ref");
  return mk_wchar (((unsigned char*) cvalue_data (args[0])) [i]);
}

/* (string-set! s i c) */
static value_t
fltm_string_set (value_t* args, uint32_t nargs) {
  argcount ("string-set!", nargs, 3);
  if (!fl_isstring (args[0])) type_error ("string-set!", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  if (n == 0) bounds_error ("string-set!", args[0], args[1]);
  size_t i= fltm_index (args[0], args[1], n-1, "string-set!");
  ((char*) cvalue_data (args[0])) [i]= (char) fltm_byte_of (args[2], "string-set!");
  return FL_UNSPECIFIED;
}

/* (substring s start [end]) */
static value_t
fltm_substring (value_t* args, uint32_t nargs) {
  if (nargs < 2 || nargs > 3) argcount ("substring", nargs, 3);
  if (!fl_isstring (args[0])) type_error ("substring", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  size_t i1= fltm_index (args[0], args[1], n, "substring");
  size_t i2= nargs == 3? fltm_index (args[0], args[2], n, "substring"): n;
  if (i2 < i1) bounds_error ("substring", args[0], args[2]);
  value_t r= cvalue_string (i2 - i1);
  if (i2 > i1)
    memcpy (cvalue_data (r), ((char*) cvalue_data (args[0])) + i1, i2 - i1);
  return r;
}

/* (string->list s [start [end]]) */
static value_t
fltm_string_to_list (value_t* args, uint32_t nargs) {
  if (nargs < 1 || nargs > 3) argcount ("string->list", nargs, 1);
  if (!fl_isstring (args[0])) type_error ("string->list", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  size_t i1= nargs >= 2? fltm_index (args[0], args[1], n, "string->list"): 0;
  size_t i2= nargs == 3? fltm_index (args[0], args[2], n, "string->list"): n;
  value_t l= FL_NIL;
  PUSH (args[0]);
  while (i2 > i1) {
    i2--;
    PUSH (l);
    value_t c= mk_wchar (((unsigned char*) cvalue_data (Stack[SP-2])) [i2]);
    l= fl_cons (c, POP ());
  }
  POPN (1);
  return l;
}

/* (string-index-of s byte start end): the index of the byte, or #f */
static value_t
fltm_string_index_of (value_t* args, uint32_t nargs) {
  argcount ("%string-index-of", nargs, 4);
  if (!fl_isstring (args[0]))
    type_error ("%string-index-of", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  int c= fltm_byte_of (args[1], "%string-index-of");
  size_t i1= fltm_index (args[0], args[2], n, "%string-index-of");
  size_t i2= fltm_index (args[0], args[3], n, "%string-index-of");
  const char* d= (const char*) cvalue_data (args[0]);
  if (i2 > i1) {
    const char* p= (const char*) memchr (d + i1, c, i2 - i1);
    if (p != NULL) return fixnum (p - d);
  }
  return FL_F;
}

/* (%string-search s pattern start): the index of pattern in s, or #f */
static value_t
fltm_string_search (value_t* args, uint32_t nargs) {
  argcount ("%string-search", nargs, 3);
  if (!fl_isstring (args[0])) type_error ("%string-search", "string", args[0]);
  if (!fl_isstring (args[1])) type_error ("%string-search", "string", args[1]);
  size_t n= cvalue_len (args[0]), m= cvalue_len (args[1]);
  size_t i= fltm_index (args[0], args[2], n, "%string-search");
  const char* s= (const char*) cvalue_data (args[0]);
  const char* p= (const char*) cvalue_data (args[1]);
  if (m == 0) return fixnum (i);
  for (; i + m <= n; i++)
    if (s[i] == p[0] && memcmp (s + i, p, m) == 0) return fixnum (i);
  return FL_F;
}

/* (%string-compare a b): -1, 0 or 1, comparing the bytes */
static value_t
fltm_string_compare (value_t* args, uint32_t nargs) {
  argcount ("%string-compare", nargs, 2);
  if (!fl_isstring (args[0])) type_error ("%string-compare", "string", args[0]);
  if (!fl_isstring (args[1])) type_error ("%string-compare", "string", args[1]);
  size_t n= cvalue_len (args[0]), m= cvalue_len (args[1]);
  int c= memcmp (cvalue_data (args[0]), cvalue_data (args[1]), n < m? n: m);
  if (c == 0) c= (n < m? -1: (n > m? 1: 0));
  return fixnum (c < 0? -1: (c > 0? 1: 0));
}

/* (%string-copy s): a fresh copy */
static value_t
fltm_string_copy (value_t* args, uint32_t nargs) {
  argcount ("%string-copy", nargs, 1);
  if (!fl_isstring (args[0])) type_error ("%string-copy", "string", args[0]);
  size_t n= cvalue_len (args[0]);
  value_t r= cvalue_string (n);
  if (n > 0) memcpy (cvalue_data (r), cvalue_data (args[0]), n);
  return r;
}

/* (%read-byte port), (%peek-byte port): the next byte as a char, or eof */
static value_t
fltm_read_byte (value_t* args, uint32_t nargs) {
  argcount ("%read-byte", nargs, 1);
  ios_t* s= toiostream (args[0], "%read-byte");
  int c= ios_getc (s);
  if (c == IOS_EOF) return FL_EOF;
  return mk_wchar ((unsigned char) c);
}

static value_t
fltm_peek_byte (value_t* args, uint32_t nargs) {
  argcount ("%peek-byte", nargs, 1);
  ios_t* s= toiostream (args[0], "%peek-byte");
  int c= ios_peekc (s);
  if (c == IOS_EOF) return FL_EOF;
  return mk_wchar ((unsigned char) c);
}

/* (%write-byte port char) */
static value_t
fltm_write_byte (value_t* args, uint32_t nargs) {
  argcount ("%write-byte", nargs, 2);
  ios_t* s= toiostream (args[0], "%write-byte");
  ios_putc ((char) fltm_byte_of (args[1], "%write-byte"), s);
  return FL_T;
}

/* (system command): the status of the command, as Guile's system */
static value_t
fltm_system (value_t* args, uint32_t nargs) {
  argcount ("system", nargs, 1);
  char* cmd= tostring (args[0], "system");
  return fixnum (system (cmd));
}

/* (%builtin-name f): the name of a builtin, or #f */
static value_t
fltm_builtin_name (value_t* args, uint32_t nargs) {
  argcount ("%builtin-name", nargs, 1);
  value_t f= args[0];
  if (isbuiltin (f)) return symbol (builtin_names[uintval (f)]);
  if (iscbuiltin (f)) {
    void* s= ptrhash_get (&reverse_dlsym_lookup_table, ptr (f));
    if (s != HT_NOTFOUND) return (value_t) s;
  }
  return FL_F;
}

/* (%keyword? x): a symbol :name (without allocating its name) */
static value_t
fltm_keywordp (value_t* args, uint32_t nargs) {
  argcount ("%keyword?", nargs, 1);
  if (!issymbol (args[0])) return FL_F;
  const char* n= symbol_name (args[0]);
  return (n[0] == ':' && n[1] != '\0')? FL_T: FL_F;
}

/* (%symbol? x): a symbol of Guile: neither a keyword nor #<unspecified> */
static value_t
fltm_symbolp (value_t* args, uint32_t nargs) {
  argcount ("%symbol?", nargs, 1);
  value_t x= args[0];
  if (!issymbol (x) || x == FL_UNSPECIFIED) return FL_F;
  const char* n= symbol_name (x);
  return (n[0] == ':' && n[1] != '\0')? FL_F: FL_T;
}

/* (%table-ref table key): the value of key, or #f (ahash-ref, hash-ref) */
static value_t
fltm_table_ref (value_t* args, uint32_t nargs) {
  argcount ("%table-ref", nargs, 2);
  value_t a[3]= { args[0], args[1], FL_F };
  return fl_table_get (a, 3);
}

/* (string-length s): the number of bytes */
static value_t
fltm_string_length_builtin (value_t* args, uint32_t nargs) {
  argcount ("string-length", nargs, 1);
  if (!fl_isstring (args[0])) type_error ("string-length", "string", args[0]);
  return fixnum (cvalue_len (args[0]));
}

static builtinspec_t fltm_builtin_info[]= {
  { "string-length", fltm_string_length_builtin },
  { "%keyword?", fltm_keywordp },
  { "%symbol?", fltm_symbolp },
  { "%table-ref", fltm_table_ref },
  { "%builtin-name", fltm_builtin_name },
  { "system", fltm_system },
  { "%read-byte", fltm_read_byte },
  { "%peek-byte", fltm_peek_byte },
  { "%write-byte", fltm_write_byte },
  { "%string", fltm_string_of_chars },
  { "list->string", fltm_list_to_string },
  { "make-string", fltm_make_string },
  { "string-ref", fltm_string_ref },
  { "string-set!", fltm_string_set },
  { "substring", fltm_substring },
  { "string->list", fltm_string_to_list },
  { "%string-index-of", fltm_string_index_of },
  { "%string-search", fltm_string_search },
  { "%string-compare", fltm_string_compare },
  { "%string-copy", fltm_string_copy },
  { "%unconstant!", fltm_unconstant },
  { "%gc", fltm_gc_builtin },
  { "%heap-size", fltm_heap_size },
  { NULL, NULL }
};

/* prints the functions of the frames of the stack, without allocating */
static void
fltm_print_frames (void) {
  uint32_t top= curr_frame;
  int n= 0;
  fprintf (stderr, "femtolisp: out of memory in\n");
  ios_flush (ios_stderr);
  while (top > 0 && n < 60) {
    uint32_t sz= Stack[top-3]+1;
    uint32_t bp= top-5-sz;
    value_t f= Stack[bp];
    const char* name= "?";
    if (isfunction (f) && issymbol (fn_name (f)))
      name= symbol_name (fn_name (f));
    else if (isbuiltin (f)) name= builtin_names[uintval (f)];
    fprintf (stderr, "  #%d %s ", n, name);
    if (isfunction (f) && !isbuiltin (f)) {
      /* the source kept by the compiler (*keep-source*), abbreviated */
      value_t vals= fn_vals (f);
      if (isvector (vals) && vector_size (vals) > 0) {
        value_t c= vector_elt (vals, vector_size (vals) - 1);
        if (iscons (c) && car_ (c) == symbol ("%source")) {
          value_t pl= symbol_value (printlengthsym);
          value_t pv= symbol_value (printlevelsym);
          set (printlengthsym, fixnum (4));
          set (printlevelsym, fixnum (3));
          fl_print (ios_stderr, cdr_ (c));
          set (printlengthsym, pl);
          set (printlevelsym, pv);
        }
      }
    }
    if (sz > 1) {
      value_t pl= symbol_value (printlengthsym);
      value_t pv= symbol_value (printlevelsym);
      set (printlengthsym, fixnum (6));
      set (printlevelsym, fixnum (4));
      fprintf (stderr, " ARG ");
      fl_print (ios_stderr, Stack[top-1]? FL_NIL: Stack[bp+1]);
      set (printlengthsym, pl);
      set (printlevelsym, pv);
    }
    fprintf (stderr, "\n");
    ios_flush (ios_stderr);
    top= Stack[top-4];
    n++;
  }
}

int
fltm_init (size_t heap_size, const char* boot, size_t boot_length) {
  int failed= 0;
  fl_init (heap_size);
  fltm_nil= FL_NIL;
  fltm_true= FL_T;
  fltm_false= FL_F;
  fltm_unspecified= FL_UNSPECIFIED;
  fl_out_of_memory_hook= fltm_print_frames;
  assign_global_builtins (fltm_builtin_info);
  {
    FL_TRY_EXTERN {
      value_t f= fl_buffer (NULL, 0);
      ios_t* s= value2c (ios_t*, f);
      ios_write (s, (char*) boot, boot_length);
      ios_seek (s, 0);
      failed= fl_load_system_image (f);
      if (!failed)
        (void) fl_applyn (0, symbol_value (symbol ("__init_globals")));
    }
    FL_CATCH_EXTERN {
      ios_puts ("femtolisp: fatal error during the initialization:\n",
                ios_stderr);
      fl_print (ios_stderr, fl_lasterror);
      ios_putc ('\n', ios_stderr);
      failed= 1;
    }
  }
  return failed;
}

void
fltm_set_gc_roots (void (*roots) (fltm_value (*reloc) (fltm_value))) {
  fl_gc_extra_roots= (void (*)(value_t (*)(value_t))) roots;
}

void
fltm_heap_bounds (void** from, void** to, size_t* size) {
  *from= fromspace; *to= tospace; *size= heapsize;
}

int
fltm_is_valid (fltm_value v) {
  /* a value which the garbage collector can relocate: an immediate value, or
     a pointer into the current heap (fromspace) or outside the managed heap */
  uptrint_t t= tag (v);
  unsigned char* p= (unsigned char*) ptr (v);
  if (t == TAG_CONS)
    return p >= fromspace && p < fromspace + heapsize * 2;
  return 1;
}

void
fltm_gc (void) {
  gc (0);
}

/******************************************************************************
* Predicates
******************************************************************************/

int
fltm_is_list (fltm_value x) {
  value_t slow= x;
  while (1) {
    if (x == FL_NIL) return 1;
    if (!iscons (x)) return 0;
    x= cdr_ (x);
    if (x == FL_NIL) return 1;
    if (!iscons (x)) return 0;
    x= cdr_ (x);
    slow= cdr_ (slow);
    if (x == slow) return 0; /* circular */
  }
}

static int
fltm_is_integer_type (numerictype_t t) {
  return t <= T_UINT64;
}

int
fltm_is_integer (fltm_value x) {
  if (isfixnum (x)) return 1;
  if (iscprim (x)) {
    cprim_t* c= (cprim_t*) ptr (x);
    return c->type != wchartype && fltm_is_integer_type (cp_numtype (c));
  }
  return 0;
}

int
fltm_is_number (fltm_value x) {
  return fl_isnumber (x);
}

int
fltm_is_string (fltm_value x) {
  return fl_isstring (x);
}

int
fltm_is_procedure (fltm_value x) {
  return isfunction (x) || isbuiltin (x) || iscbuiltin (x);
}

int
fltm_is_equal (fltm_value a, fltm_value b) {
  int r= 0;
  FL_TRY_EXTERN {
    r= (fl_equal (a, b) == FL_T);
  }
  FL_CATCH_EXTERN {
    r= 0;
  }
  return r;
}

/******************************************************************************
* Constructors and accessors
******************************************************************************/

fltm_value
fltm_cons (fltm_value a, fltm_value b) {
  return fl_cons (a, b);
}

fltm_value
fltm_integer (int64_t i) {
  if (fits_fixnum (i)) return fixnum (i);
  return mk_int64 (i);
}

fltm_value
fltm_double (double x) {
  return mk_double (x);
}

fltm_value
fltm_string (const char* s, size_t n) {
  value_t r= cvalue_string (n);
  if (n > 0) memcpy (cvalue_data (r), s, n);
  return r;
}

fltm_value
fltm_symbol (const char* s) {
  return symbol ((char*) s);
}

int64_t
fltm_to_integer (fltm_value x) {
  if (isfixnum (x)) return numval (x);
  if (iscprim (x)) {
    cprim_t* c= (cprim_t*) ptr (x);
    void* d= cp_data (c);
    switch (cp_numtype (c)) {
    case T_INT8:   return *(int8_t*) d;
    case T_UINT8:  return *(uint8_t*) d;
    case T_INT16:  return *(int16_t*) d;
    case T_UINT16: return *(uint16_t*) d;
    case T_INT32:  return *(int32_t*) d;
    case T_UINT32: return *(uint32_t*) d;
    case T_INT64:  return *(int64_t*) d;
    case T_UINT64: return (int64_t) *(uint64_t*) d;
    case T_FLOAT:  return (int64_t) *(float*) d;
    case T_DOUBLE: return (int64_t) *(double*) d;
    }
  }
  return 0;
}

double
fltm_to_double (fltm_value x) {
  if (isfixnum (x)) return (double) numval (x);
  if (iscprim (x)) {
    cprim_t* c= (cprim_t*) ptr (x);
    if (c->type != wchartype)
      return conv_to_double (cp_data (c), cp_numtype (c));
  }
  return 0.0;
}

const char*
fltm_string_data (fltm_value x) {
  return (const char*) cvalue_data (x);
}

size_t
fltm_string_length (fltm_value x) {
  return cvalue_len (x);
}

const char*
fltm_symbol_name (fltm_value x) {
  return symbol_name (x);
}

/******************************************************************************
* Opaque values
******************************************************************************/

typedef struct {
  cvtable_t vtable;
  fltm_opaque_ops ops;
  fltype_t* type;
} fltm_opaque_type;

static fltm_opaque_type*
fltm_type_of (value_t v) {
  /* the vtable is the first field of fltm_opaque_type */
  return (fltm_opaque_type*) cv_class ((cvalue_t*) ptr (v))->vtable;
}

static void
fltm_opaque_print (value_t self, ios_t* f) {
  fltm_opaque_type* t= fltm_type_of (self);
  char* s= t->ops.to_string (*(void**) cvalue_data (self));
  ios_write (f, s, strlen (s));
  free (s);
}

static void
fltm_opaque_finalize (value_t self) {
  fltm_opaque_type* t= fltm_type_of (self);
  t->ops.finalize (*(void**) cvalue_data (self));
}

static int
fltm_opaque_equal (value_t a, value_t b) {
  fltm_opaque_type* t= fltm_type_of (a);
  return t->ops.equal (*(void**) cvalue_data (a), *(void**) cvalue_data (b));
}

static uptrint_t
fltm_opaque_hash (value_t self) {
  fltm_opaque_type* t= fltm_type_of (self);
  return t->ops.hash (*(void**) cvalue_data (self));
}

void*
fltm_define_opaque_type (const char* name, fltm_opaque_ops* ops) {
  fltm_opaque_type* t= (fltm_opaque_type*) calloc (1, sizeof (fltm_opaque_type));
  t->vtable.print= fltm_opaque_print;
  t->vtable.finalize= fltm_opaque_finalize;
  t->vtable.equal= fltm_opaque_equal;
  t->vtable.hash= fltm_opaque_hash;
  t->ops= *ops;
  t->type= define_opaque_type (symbol ((char*) name), sizeof (void*),
                               &t->vtable, NULL);
  return t;
}

fltm_value
fltm_make_opaque (void* type, void* p) {
  fltm_opaque_type* t= (fltm_opaque_type*) type;
  value_t v= cvalue (t->type, sizeof (void*));
  *(void**) cvalue_data (v)= p;
  return v;
}

int
fltm_is_opaque (fltm_value x, void* type) {
  return iscvalue (x) &&
    cv_class ((cvalue_t*) ptr (x)) == ((fltm_opaque_type*) type)->type;
}

void*
fltm_opaque_pointer (fltm_value x) {
  return *(void**) cvalue_data (x);
}

/******************************************************************************
* Global variables
******************************************************************************/

void
fltm_define (const char* name, fltm_value v) {
  PUSH (v);
  symbol_t* sym= (symbol_t*) ptr (symbol ((char*) name));
  sym->flags &= ~0x1;
  sym->binding= POP ();
}

fltm_value
fltm_lookup (const char* name) {
  value_t v= symbol_value (symbol ((char*) name));
  return v == UNBOUND? FL_F: v;
}

fltm_value
fltm_builtin (const char* name, fltm_builtin_t f) {
  return cbuiltin ((char*) name, (builtin_t) f);
}

/******************************************************************************
* Calls and errors
******************************************************************************/

/* the error e as Guile gives it, (key subr message args rest), with
   (%guile-error e) when it is defined */
static value_t
fltm_guile_error (value_t e) {
  value_t g= symbol_value (symbol ("%guile-error"));
  value_t r= e;
  if (g != UNBOUND && g != FL_F) {
    PUSH (e);
    FL_TRY_EXTERN {
      r= fl_applyn (1, g, Stack[SP-1]);
    }
    FL_CATCH_EXTERN {
      r= Stack[SP-1];
    }
    POPN (1);
  }
  return r;
}

/* reports the error e with (%report-error e), or prints it */
static void
fltm_report (value_t e) {
  value_t r= symbol_value (symbol ("%report-error"));
  int done= 0;
  PUSH (e);
  if (r != UNBOUND && r != FL_F) {
    FL_TRY_EXTERN {
      (void) fl_applyn (1, r, Stack[SP-1]);
      done= 1;
    }
    FL_CATCH_EXTERN {
      done= 0;
    }
  }
  if (!done) {
    ios_puts ("Scheme error: ", ios_stderr);
    fl_print (ios_stderr, Stack[SP-1]);
    ios_putc ('\n', ios_stderr);
    ios_flush (ios_stderr);
  }
  POPN (1);
}

int
fltm_apply (fltm_value f, fltm_value args, fltm_value* res) {
  int failed= 0;
  value_t r= FL_UNSPECIFIED;
  {
    FL_TRY_EXTERN {
      r= fl_apply (f, args);
    }
    FL_CATCH_EXTERN {
      // reported here, where the frames of the error are still known
      failed= 1;
      r= fl_lasterror;
      fltm_report (r);
      r= fltm_guile_error (r);
    }
  }
  *res= r;
  return failed;
}

int
fltm_eval_string (const char* s, size_t n, fltm_value* res) {
  int failed= 0;
  value_t r= FL_UNSPECIFIED;
  uint32_t saved= SP;
  {
    FL_TRY_EXTERN {
      value_t f= fl_buffer (NULL, 0);
      ios_t* in= value2c (ios_t*, f);
      ios_write (in, (char*) s, n);
      ios_seek (in, 0);
      PUSH (f);
      PUSH (FL_UNSPECIFIED);
      while (1) {
        value_t e= fl_read_sexpr (Stack[SP-2]);
        if (e == FL_EOF) break;
        Stack[SP-1]= fl_applyn (1, symbol_value (evalsym), e);
      }
      r= Stack[SP-1];
    }
    FL_CATCH_EXTERN {
      failed= 1;
      r= fl_lasterror;
      fltm_report (r);
      r= fltm_guile_error (r);
    }
  }
  SP= saved;
  *res= r;
  return failed;
}

void
fltm_raise (const char* key, const char* subr, const char* message,
            fltm_value args) {
  value_t l;
  PUSH (args);
  PUSH (symbol ((char*) key));
  PUSH (cvalue_static_cstring (subr));
  PUSH (cvalue_static_cstring (message));
  /* (key subr message args), fl_listn protects its arguments */
  l= fl_listn (4, Stack[SP-3], Stack[SP-2], Stack[SP-1], Stack[SP-4]);
  POPN (4);
  fl_raise (l);
}
