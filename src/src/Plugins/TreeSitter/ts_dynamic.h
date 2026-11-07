/******************************************************************************
* MODULE     : ts_dynamic.h
* DESCRIPTION: tree-sitter languages made from tables generated in Scheme
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TS_DYNAMIC_H
#define TS_DYNAMIC_H

#include "tree-sitter/include/tree_sitter/api.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* A language from the data written by ts-export-language (ts-export.scm),
   or NULL (with a message in error, if error is not NULL) when the data is
   invalid or when TSDYN_MAX_LANGUAGES languages are already loaded. */
const TSLanguage *tsdyn_language_new (const char *data, size_t length,
                                      const char **error);
void tsdyn_language_delete (const TSLanguage *language);

#define TSDYN_MAX_LANGUAGES 32

/* The content of an xz stream (ts_xz.c): its length and the bytes in *out,
   allocated with malloc, or -1 when the stream is not valid. */
long ts_xz_decompress (const unsigned char *in, size_t n, unsigned char **out);

#ifdef __cplusplus
}
#endif

#endif // TS_DYNAMIC_H
