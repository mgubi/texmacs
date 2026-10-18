/******************************************************************************
* MODULE     : compare.c
* DESCRIPTION: compare the tables made in TeXmacs with those of parser.c
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The language of the src/parser.c of tree-sitter generate (the reference)
   against the one read by ts_dynamic.c from a .tsdyn file of the cache: the
   counts, the names, metadata and public symbols, the alias sequences, the
   alias map, the supertypes, and the parse tables up to the numbering of the
   states (the map of the states follows the shifts and the gotos from 0 and
   1; tree-sitter orders the states of the same size by an unstable sort, so
   the numbers, and with them the lex states and the primary states, may
   differ). Queries given as arguments are compiled with both languages.

   T=src/Plugins/TreeSitter; G=<grammar repository>
   clang -O1 -w -DLANGFN=tree_sitter_<name> -I$T/tree-sitter/include      -I$T/tree-sitter/src -I$T -I$G/src compare.c $T/ts_dynamic.c      $G/src/parser.c [$G/src/scanner.c] -o compare
   ./compare <file.tsdyn> [query...]      (TSQ_ALL=1: all the differences) */

#include "lib.c"
#include "ts_dynamic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef LANGFN
#define LANGFN tree_sitter_julia
#endif
const TSLanguage *LANGFN (void);

static char *slurp (const char *f, size_t *n) {
  FILE *p = fopen (f, "rb"); fseek (p, 0, SEEK_END); *n = ftell (p); rewind (p);
  char *b = malloc (*n + 1); fread (b, 1, *n, p); fclose (p); return b;
}

static int diffs = 0;
static int limit = 40;
#define REPORT(...) do { if (diffs++ < limit) printf (__VA_ARGS__); } while (0)

int main (int argc, char **argv) {
  if (getenv ("TSQ_ALL")) limit = 1 << 30;
  size_t n; char *d = slurp (argv[1], &n); const char *err = NULL;
  const TSLanguage *a = LANGFN ();
  const TSLanguage *b = tsdyn_language_new (d, n, &err);
  if (!b) { printf ("dyn: %s\n", err); return 1; }
  for (int k = 2; k < argc; k++) {
    uint32_t off; TSQueryError e;
    TSQuery *q = ts_query_new (a, argv[k], strlen (argv[k]), &off, &e);
    printf ("ref query %d", e); if (q) ts_query_delete (q);
    q = ts_query_new (b, argv[k], strlen (argv[k]), &off, &e);
    printf (", dyn query %d: %s\n", e, argv[k]); if (q) ts_query_delete (q);
  }
#define CMP(f) if (a->f != b->f) REPORT (#f " %u %u\n", (unsigned) a->f, (unsigned) b->f)
  CMP (symbol_count); CMP (alias_count); CMP (token_count); CMP (external_token_count);
  CMP (state_count); CMP (large_state_count); CMP (production_id_count); CMP (field_count);
  CMP (max_alias_sequence_length); CMP (keyword_capture_token); CMP (supertype_count);
  uint32_t ns = a->symbol_count + a->alias_count;
  for (uint32_t i = 0; i < ns; i++) {
    if (strcmp (a->symbol_names[i], b->symbol_names[i])) REPORT ("name %u %s %s\n", i, a->symbol_names[i], b->symbol_names[i]);
    if (memcmp (&a->symbol_metadata[i], &b->symbol_metadata[i], sizeof (TSSymbolMetadata)))
      REPORT ("metadata %u %s\n", i, a->symbol_names[i]);
    if (a->public_symbol_map[i] != b->public_symbol_map[i])
      REPORT ("public %u %s: %u %u\n", i, a->symbol_names[i], a->public_symbol_map[i], b->public_symbol_map[i]);
  }
  for (uint32_t p = 0; p < a->production_id_count; p++)
    for (uint32_t k = 0; k < a->max_alias_sequence_length && k < b->max_alias_sequence_length; k++) {
      TSSymbol x = a->alias_sequences[p * a->max_alias_sequence_length + k];
      TSSymbol y = b->alias_sequences[p * b->max_alias_sequence_length + k];
      if (x != y) REPORT ("alias seq %u %u: %u %s (named %d) / %u %s (named %d)\n", p, k, x, a->symbol_names[x], a->symbol_metadata[x].named, y, b->symbol_names[y], b->symbol_metadata[y].named);
    }
  // alias map: symbol count alias... 0
  for (int w = 0; w < 2; w++) {
    const uint16_t *x = w? b->alias_map: a->alias_map; const TSLanguage *l = w? b: a;
    printf ("%s alias map:", w? "dyn": "ref");
    while (*x) { unsigned sym = *x++, cnt = *x++; printf (" %s ->", l->symbol_names[sym]);
      for (unsigned k = 0; k < cnt; k++) { printf (" %u:%s", *x, l->symbol_names[*x]); x++; } printf (";"); }
    printf ("\n"); }
  // the parse tables, up to the numbering of the states: the map of the
  // states is built along the shifts and the gotos from states 0 and 1
  if (a->state_count == b->state_count) {
    uint32_t nst = a->state_count;
    int32_t *m = malloc (nst * sizeof (int32_t)), *inv = malloc (nst * sizeof (int32_t));
    uint32_t *queue = malloc (nst * sizeof (uint32_t)); uint32_t qh = 0, qt = 0;
    for (uint32_t i = 0; i < nst; i++) m[i] = inv[i] = -1;
    #define MAP(x, y) do { uint32_t _x = (x), _y = (y); \
      if (m[_x] < 0 && inv[_y] < 0) { m[_x] = _y; inv[_y] = _x; queue[qt++] = _x; } \
      else if (m[_x] != (int32_t) _y) REPORT ("state map conflict %u: %d %u\n", _x, m[_x], _y); } while (0)
    MAP (0, 0); MAP (1, 1);
    while (qh < qt) {
      uint32_t s = queue[qh++], t = m[s];
      if (memcmp (&a->lex_modes[s], &b->lex_modes[t], sizeof (TSLexerMode)))
        REPORT ("lex mode %u/%u: %u/%u/%u %u/%u/%u\n", s, t, a->lex_modes[s].lex_state, a->lex_modes[s].external_lex_state, a->lex_modes[s].reserved_word_set_id, b->lex_modes[t].lex_state, b->lex_modes[t].external_lex_state, b->lex_modes[t].reserved_word_set_id);
      for (uint32_t sym = 0; sym < a->symbol_count; sym++) {
        uint16_t x = ts_language_lookup (a, s, sym), y = ts_language_lookup (b, t, sym);
        if (sym < a->token_count) {
          const TSParseActionEntry *ea = &a->parse_actions[x], *eb = &b->parse_actions[y];
          if (ea->entry.count != eb->entry.count || ea->entry.reusable != eb->entry.reusable) {
            REPORT ("entry %u/%u %s\n", s, t, a->symbol_names[sym]); continue; }
          for (unsigned k = 0; k < ea->entry.count; k++) {
            TSParseAction p = ea[1 + k].action, q = eb[1 + k].action;
            if (p.type != q.type) { REPORT ("action type %u/%u %s\n", s, t, a->symbol_names[sym]); continue; }
            if (p.type == TSParseActionTypeShift) {
              if (p.shift.extra != q.shift.extra || p.shift.repetition != q.shift.repetition)
                REPORT ("shift flags %u/%u %s\n", s, t, a->symbol_names[sym]);
              if (!p.shift.extra) MAP (p.shift.state, q.shift.state);
            } else if (p.type == TSParseActionTypeReduce) {
              if (p.reduce.symbol != q.reduce.symbol || p.reduce.child_count != q.reduce.child_count ||
                  p.reduce.dynamic_precedence != q.reduce.dynamic_precedence || p.reduce.production_id != q.reduce.production_id)
                REPORT ("reduce %u/%u %s\n", s, t, a->symbol_names[sym]);
            }
          }
        } else if ((x == 0) != (y == 0)) REPORT ("goto %u/%u %s: %u %u\n", s, t, a->symbol_names[sym], x, y);
        else if (x) MAP (x, y);
      }
    }
    printf ("states reached: %u of %u\n", qt, nst);
    for (uint32_t i = 0; i < nst; i++)
      if (m[i] >= 0 && a->primary_state_ids[i] < nst && m[a->primary_state_ids[i]] != (int32_t) b->primary_state_ids[m[i]])
        REPORT ("primary state %u\n", i);
  }
  for (uint32_t i = 0; i < a->supertype_count && i < b->supertype_count; i++) {
    TSSymbol s = a->supertype_symbols[i];
    if (s != b->supertype_symbols[i]) REPORT ("supertype %u: %u %u\n", i, s, b->supertype_symbols[i]);
    TSMapSlice x = a->supertype_map_slices[s], y = b->supertype_map_slices[s];
    if (x.length != y.length) REPORT ("supertype %s: %u %u children\n", a->symbol_names[s], x.length, y.length);
    else for (unsigned k = 0; k < x.length; k++)
      if (a->supertype_map_entries[x.index + k] != b->supertype_map_entries[y.index + k])
        REPORT ("supertype %s child %u: %s %s\n", a->symbol_names[s], k, a->symbol_names[a->supertype_map_entries[x.index + k]], a->symbol_names[b->supertype_map_entries[y.index + k]]);
  }
  printf ("%d differences\n", diffs);
  return 0;
}
