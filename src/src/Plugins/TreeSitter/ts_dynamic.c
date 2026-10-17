/******************************************************************************
* MODULE     : ts_dynamic.c
* DESCRIPTION: tree-sitter languages made from tables generated in Scheme
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The generator of progs/treesit (a port of tree-sitter generate to Scheme)
   writes its tables with ts-export-language. Here they fill a TSLanguage of
   the C runtime of tree-sitter, so that no C code is generated per grammar:

   - every state is a "large" state (one row of parse_table per state);
   - the lex functions interpret the lex tables; since the runtime calls
     them without the language, each loaded language takes one of
     TSDYN_MAX_LANGUAGES slots, with its own pair of functions;
   - the tables used only by queries are minimal (no alias map, every state
     primary, no supertype map). */

#include "ts_dynamic.h"
#include "tree-sitter/src/parser.h"   /* the definition of TSLanguage */
#include <stdlib.h>
#include <string.h>

/******************************************************************************
* Lex tables
******************************************************************************/

typedef struct {
  int32_t start, end;   /* half-open */
} DynRange;

typedef struct {
  int32_t next;
  bool skip;
  uint32_t first_range, range_count;
} DynTransition;

typedef struct {
  int32_t accept, eof;
  uint32_t first, count;
  int16_t ascii[128];   /* transition of each ASCII character, or -1 */
} DynLexState;

typedef struct {
  DynLexState *states;
  uint32_t state_count;
  DynTransition *transitions;
  DynRange *ranges;
} DynLexTable;

static bool
dyn_run_lex (const DynLexTable *t, TSLexer *lexer, TSStateId start) {
  bool result = false;
  uint32_t state = start;
  if (state >= t->state_count) return false;
  for (;;) {
    const DynLexState *s = &t->states[state];
    if (s->accept >= 0) {
      result = true;
      lexer->result_symbol = (TSSymbol) s->accept;
      lexer->mark_end (lexer);
    }
    if (lexer->eof (lexer)) {
      if (s->eof < 0) return result;
      lexer->advance (lexer, false);
      state = (uint32_t) s->eof;
      continue;
    }
    int32_t c = lexer->lookahead;
    int32_t found = -1;
    if (c >= 0 && c < 128) found = s->ascii[c];
    else {
      for (uint32_t i = 0; i < s->count && found < 0; i++) {
        const DynTransition *tr = &t->transitions[s->first + i];
        for (uint32_t k = 0; k < tr->range_count; k++) {
          const DynRange *r = &t->ranges[tr->first_range + k];
          if (c < r->start) break;
          if (c < r->end) { found = (int32_t) i; break; }
        }
      }
    }
    if (found < 0) return result;
    const DynTransition *tr = &t->transitions[s->first + found];
    lexer->advance (lexer, tr->skip);
    state = (uint32_t) tr->next;
  }
}

/******************************************************************************
* Languages
******************************************************************************/

typedef struct {
  TSLanguage language;   /* first, so that the TSLanguage is the DynLanguage */
  int slot;
  DynLexTable lexer, keyword_lexer;
  char *name;
  char **symbol_names;
  char **field_names;
  uint32_t name_count;
} DynLanguage;

static DynLanguage *dyn_slots[TSDYN_MAX_LANGUAGES];

#define DYN_SLOT(i) \
  static bool dyn_lex_##i (TSLexer *l, TSStateId s) { \
    return dyn_run_lex (&dyn_slots[i]->lexer, l, s); } \
  static bool dyn_keyword_lex_##i (TSLexer *l, TSStateId s) { \
    return dyn_run_lex (&dyn_slots[i]->keyword_lexer, l, s); }

DYN_SLOT(0)  DYN_SLOT(1)  DYN_SLOT(2)  DYN_SLOT(3)  DYN_SLOT(4)  DYN_SLOT(5)
DYN_SLOT(6)  DYN_SLOT(7)  DYN_SLOT(8)  DYN_SLOT(9)  DYN_SLOT(10) DYN_SLOT(11)
DYN_SLOT(12) DYN_SLOT(13) DYN_SLOT(14) DYN_SLOT(15) DYN_SLOT(16) DYN_SLOT(17)
DYN_SLOT(18) DYN_SLOT(19) DYN_SLOT(20) DYN_SLOT(21) DYN_SLOT(22) DYN_SLOT(23)
DYN_SLOT(24) DYN_SLOT(25) DYN_SLOT(26) DYN_SLOT(27) DYN_SLOT(28) DYN_SLOT(29)
DYN_SLOT(30) DYN_SLOT(31)

#define DYN_FN(i) dyn_lex_##i
#define DYN_KW(i) dyn_keyword_lex_##i

static bool (*const dyn_lex_fns[TSDYN_MAX_LANGUAGES]) (TSLexer *, TSStateId) = {
  DYN_FN(0), DYN_FN(1), DYN_FN(2), DYN_FN(3), DYN_FN(4), DYN_FN(5), DYN_FN(6),
  DYN_FN(7), DYN_FN(8), DYN_FN(9), DYN_FN(10), DYN_FN(11), DYN_FN(12),
  DYN_FN(13), DYN_FN(14), DYN_FN(15), DYN_FN(16), DYN_FN(17), DYN_FN(18),
  DYN_FN(19), DYN_FN(20), DYN_FN(21), DYN_FN(22), DYN_FN(23), DYN_FN(24),
  DYN_FN(25), DYN_FN(26), DYN_FN(27), DYN_FN(28), DYN_FN(29), DYN_FN(30),
  DYN_FN(31) };

static bool (*const dyn_keyword_fns[TSDYN_MAX_LANGUAGES]) (TSLexer *, TSStateId) = {
  DYN_KW(0), DYN_KW(1), DYN_KW(2), DYN_KW(3), DYN_KW(4), DYN_KW(5), DYN_KW(6),
  DYN_KW(7), DYN_KW(8), DYN_KW(9), DYN_KW(10), DYN_KW(11), DYN_KW(12),
  DYN_KW(13), DYN_KW(14), DYN_KW(15), DYN_KW(16), DYN_KW(17), DYN_KW(18),
  DYN_KW(19), DYN_KW(20), DYN_KW(21), DYN_KW(22), DYN_KW(23), DYN_KW(24),
  DYN_KW(25), DYN_KW(26), DYN_KW(27), DYN_KW(28), DYN_KW(29), DYN_KW(30),
  DYN_KW(31) };

/******************************************************************************
* Reading the data
******************************************************************************/

typedef struct {
  const char *p, *end;
  bool failed;
} DynReader;

static void
dyn_skip (DynReader *r) {
  while (r->p < r->end && (*r->p == ' ' || *r->p == '\n' || *r->p == '\t'))
    r->p++;
}

static long
dyn_int (DynReader *r) {
  dyn_skip (r);
  if (r->p >= r->end) { r->failed = true; return 0; }
  bool negative = false;
  if (*r->p == '-') { negative = true; r->p++; }
  if (r->p >= r->end || *r->p < '0' || *r->p > '9') {
    r->failed = true; return 0; }
  long v = 0;
  while (r->p < r->end && *r->p >= '0' && *r->p <= '9')
    v = 10 * v + (*r->p++ - '0');
  return negative ? -v : v;
}

static char *
dyn_string (DynReader *r) {
  long n = dyn_int (r);
  if (r->failed || r->p >= r->end || *r->p != ':' || n < 0 || r->p + 1 + n > r->end) {
    r->failed = true; return NULL; }
  r->p++;
  char *s = malloc ((size_t) n + 1);
  memcpy (s, r->p, (size_t) n);
  s[n] = '\0';
  r->p += n;
  return s;
}

static void
dyn_lex_table (DynReader *r, DynLexTable *t) {
  uint32_t n = (uint32_t) dyn_int (r);
  if (r->failed) return;
  t->state_count = n;
  t->states = calloc (n ? n : 1, sizeof (DynLexState));
  size_t tcap = 16, rcap = 16, tn = 0, rn = 0;
  t->transitions = malloc (tcap * sizeof (DynTransition));
  t->ranges = malloc (rcap * sizeof (DynRange));
  for (uint32_t i = 0; i < n && !r->failed; i++) {
    DynLexState *s = &t->states[i];
    s->accept = (int32_t) dyn_int (r);
    s->eof = (int32_t) dyn_int (r);
    s->count = (uint32_t) dyn_int (r);
    s->first = (uint32_t) tn;
    for (int c = 0; c < 128; c++) s->ascii[c] = -1;
    for (uint32_t k = 0; k < s->count && !r->failed; k++) {
      if (tn == tcap) {
        tcap *= 2; t->transitions = realloc (t->transitions, tcap * sizeof (DynTransition)); }
      DynTransition *tr = &t->transitions[tn++];
      tr->next = (int32_t) dyn_int (r);
      tr->skip = dyn_int (r) != 0;
      tr->range_count = (uint32_t) dyn_int (r);
      tr->first_range = (uint32_t) rn;
      for (uint32_t j = 0; j < tr->range_count && !r->failed; j++) {
        if (rn == rcap) {
          rcap *= 2; t->ranges = realloc (t->ranges, rcap * sizeof (DynRange)); }
        DynRange *g = &t->ranges[rn++];
        g->start = (int32_t) dyn_int (r);
        g->end = (int32_t) dyn_int (r);
        for (int32_t c = g->start; c < g->end && c < 128; c++)
          if (c >= 0) s->ascii[c] = (int16_t) k;
      }
      if (tr->next < 0 || (uint32_t) tr->next >= n) r->failed = true;
    }
    if (s->eof >= (int32_t) n) r->failed = true;
  }
}

static void
dyn_free_lex_table (DynLexTable *t) {
  free (t->states);
  free (t->transitions);
  free (t->ranges);
}

void
tsdyn_language_delete (const TSLanguage *language) {
  DynLanguage *d = (DynLanguage *) language;
  if (!d) return;
  if (d->slot >= 0 && dyn_slots[d->slot] == d) dyn_slots[d->slot] = NULL;
  TSLanguage *l = &d->language;
  free ((void *) l->parse_table);
  free ((void *) l->parse_actions);
  free ((void *) l->field_map_slices);
  free ((void *) l->field_map_entries);
  free ((void *) l->symbol_metadata);
  free ((void *) l->public_symbol_map);
  free ((void *) l->alias_map);
  free ((void *) l->alias_sequences);
  free ((void *) l->lex_modes);
  free ((void *) l->external_scanner.states);
  free ((void *) l->external_scanner.symbol_map);
  free ((void *) l->primary_state_ids);
  free ((void *) l->reserved_words);
  if (d->symbol_names)
    for (uint32_t i = 0; i < d->name_count; i++) free (d->symbol_names[i]);
  free (d->symbol_names);
  if (d->field_names)
    for (uint32_t i = 1; i <= l->field_count; i++) free (d->field_names[i]);
  free (d->field_names);
  free (d->name);
  dyn_free_lex_table (&d->lexer);
  dyn_free_lex_table (&d->keyword_lexer);
  free (d);
}

#define DYN_FAIL(msg) do { if (error) *error = (msg); \
    tsdyn_language_delete (&d->language); return NULL; } while (0)

const TSLanguage *
tsdyn_language_new (const char *data, size_t length, const char **error) {
  DynReader rd = { data, data + length, false };
  DynReader *r = &rd;
  if (error) *error = NULL;
  int slot = -1;
  for (int i = 0; i < TSDYN_MAX_LANGUAGES; i++)
    if (!dyn_slots[i]) { slot = i; break; }
  if (slot < 0) { if (error) *error = "too many languages"; return NULL; }
  dyn_skip (r);
  if (length < 6 || strncmp (r->p, "tsdyn1", 6) != 0) {
    if (error) *error = "not the data of ts-export-language"; return NULL; }
  r->p += 6;

  DynLanguage *d = calloc (1, sizeof (DynLanguage));
  d->slot = -1;
  TSLanguage *l = &d->language;
  d->name = dyn_string (r);
  uint32_t symbol_count = (uint32_t) dyn_int (r);
  uint32_t alias_count = (uint32_t) dyn_int (r);
  uint32_t token_count = (uint32_t) dyn_int (r);
  uint32_t external_count = (uint32_t) dyn_int (r);
  uint32_t state_count = (uint32_t) dyn_int (r);
  uint32_t field_count = (uint32_t) dyn_int (r);
  uint32_t production_count = (uint32_t) dyn_int (r);
  uint32_t max_alias = (uint32_t) dyn_int (r);
  uint32_t keyword_capture = (uint32_t) dyn_int (r);
  if (r->failed || symbol_count > 65000 || state_count > 65000 || max_alias > 255)
    DYN_FAIL ("bad header");
  l->abi_version = TREE_SITTER_LANGUAGE_VERSION;
  l->symbol_count = symbol_count;
  l->alias_count = alias_count;
  l->token_count = token_count;
  l->external_token_count = external_count;
  l->state_count = state_count;
  l->large_state_count = state_count;
  l->production_id_count = production_count;
  l->field_count = field_count;
  l->max_alias_sequence_length = (uint16_t) max_alias;
  l->keyword_capture_token = (TSSymbol) keyword_capture;
  l->name = d->name;

  /* symbols */
  uint32_t names = symbol_count + alias_count;
  d->name_count = names;
  d->symbol_names = calloc (names, sizeof (char *));
  for (uint32_t i = 0; i < names && !r->failed; i++)
    d->symbol_names[i] = dyn_string (r);
  TSSymbolMetadata *metadata = calloc (names, sizeof (TSSymbolMetadata));
  TSSymbol *public_map = calloc (names, sizeof (TSSymbol));
  for (uint32_t i = 0; i < names; i++) {
    long m = dyn_int (r);
    metadata[i].visible = (m & 1) != 0;
    metadata[i].named = (m & 2) != 0;
    metadata[i].supertype = (m & 4) != 0;
    public_map[i] = (TSSymbol) i;
  }
  l->symbol_names = (const char * const *) d->symbol_names;
  l->symbol_metadata = metadata;
  l->public_symbol_map = public_map;
  d->field_names = calloc (field_count + 1, sizeof (char *));
  for (uint32_t i = 1; i <= field_count && !r->failed; i++)
    d->field_names[i] = dyn_string (r);
  l->field_names = (const char * const *) d->field_names;
  if (r->failed) DYN_FAIL ("bad symbols");

  /* action lists, flattened as in parser.c */
  uint32_t list_count = (uint32_t) dyn_int (r);
  size_t cap = 64, n = 0;
  TSParseActionEntry *actions = calloc (cap, sizeof (TSParseActionEntry));
  uint32_t *list_offset = calloc (list_count ? list_count : 1, sizeof (uint32_t));
  for (uint32_t i = 0; i < list_count && !r->failed; i++) {
    bool reusable = dyn_int (r) != 0;
    uint32_t count = (uint32_t) dyn_int (r);
    if (n + count + 1 > cap) {
      while (n + count + 1 > cap) cap *= 2;
      actions = realloc (actions, cap * sizeof (TSParseActionEntry));
    }
    list_offset[i] = (uint32_t) n;
    memset (&actions[n], 0, (count + 1) * sizeof (TSParseActionEntry));
    actions[n].entry.count = (uint8_t) count;
    actions[n].entry.reusable = reusable;
    n++;
    for (uint32_t k = 0; k < count; k++) {
      TSParseAction *a = &actions[n++].action;
      switch (dyn_int (r)) {
      case 0:
        a->shift.type = TSParseActionTypeShift;
        a->shift.state = (TSStateId) dyn_int (r);
        a->shift.extra = dyn_int (r) != 0;
        a->shift.repetition = dyn_int (r) != 0;
        break;
      case 1:
        a->reduce.type = TSParseActionTypeReduce;
        a->reduce.symbol = (TSSymbol) dyn_int (r);
        a->reduce.child_count = (uint8_t) dyn_int (r);
        a->reduce.dynamic_precedence = (int16_t) dyn_int (r);
        a->reduce.production_id = (uint16_t) dyn_int (r);
        break;
      case 2: a->type = TSParseActionTypeAccept; break;
      case 3: a->type = TSParseActionTypeRecover; break;
      default: r->failed = true;
      }
    }
  }
  l->parse_actions = actions;
  if (r->failed || n > 65535) { free (list_offset); DYN_FAIL ("bad actions"); }

  /* the parse table */
  uint16_t *table = calloc ((size_t) state_count * symbol_count, sizeof (uint16_t));
  for (uint32_t s = 0; s < state_count && !r->failed; s++) {
    uint32_t count = (uint32_t) dyn_int (r);
    for (uint32_t k = 0; k < count && !r->failed; k++) {
      uint32_t symbol = (uint32_t) dyn_int (r);
      uint32_t value = (uint32_t) dyn_int (r);
      if (symbol >= symbol_count) { r->failed = true; break; }
      if (symbol < token_count) {
        if (value >= list_count) { r->failed = true; break; }
        value = list_offset[value];
      }
      table[(size_t) s * symbol_count + symbol] = (uint16_t) value;
    }
  }
  free (list_offset);
  l->parse_table = table;
  if (r->failed) DYN_FAIL ("bad parse table");

  /* lex modes, reserved words */
  TSLexerMode *modes = calloc (state_count ? state_count : 1, sizeof (TSLexerMode));
  for (uint32_t s = 0; s < state_count; s++) {
    modes[s].lex_state = (uint16_t) dyn_int (r);
    modes[s].external_lex_state = (uint16_t) dyn_int (r);
    modes[s].reserved_word_set_id = (uint16_t) dyn_int (r);
  }
  l->lex_modes = modes;
  uint32_t reserved_count = (uint32_t) dyn_int (r);
  uint32_t reserved_size = (uint32_t) dyn_int (r);
  TSSymbol *reserved = calloc ((size_t) reserved_count * reserved_size + 1, sizeof (TSSymbol));
  for (uint32_t i = 0; i < reserved_count * reserved_size; i++)
    reserved[i] = (TSSymbol) dyn_int (r);
  l->reserved_words = reserved;
  l->max_reserved_word_set_size = (uint16_t) reserved_size;

  /* aliases and fields */
  TSSymbol *aliases = calloc ((size_t) production_count * max_alias + 1, sizeof (TSSymbol));
  for (uint32_t i = 0; i < production_count * max_alias; i++)
    aliases[i] = (TSSymbol) dyn_int (r);
  l->alias_sequences = aliases;
  TSMapSlice *slices = calloc (production_count + 1, sizeof (TSMapSlice));
  size_t fcap = 16, fn = 0;
  TSFieldMapEntry *fields = calloc (fcap, sizeof (TSFieldMapEntry));
  for (uint32_t i = 0; i < production_count && !r->failed; i++) {
    uint32_t count = (uint32_t) dyn_int (r);
    slices[i].index = (uint16_t) fn;
    slices[i].length = (uint16_t) count;
    for (uint32_t k = 0; k < count; k++) {
      if (fn == fcap) { fcap *= 2; fields = realloc (fields, fcap * sizeof (TSFieldMapEntry)); }
      fields[fn].field_id = (TSFieldId) dyn_int (r);
      fields[fn].child_index = (uint8_t) dyn_int (r);
      fields[fn].inherited = dyn_int (r) != 0;
      fn++;
    }
  }
  l->field_map_slices = slices;
  l->field_map_entries = fields;
  uint16_t *alias_map = calloc (1, sizeof (uint16_t));
  l->alias_map = alias_map;

  /* external tokens (the scanner itself is to be set by the caller) */
  TSSymbol *external_map = calloc (external_count + 1, sizeof (TSSymbol));
  for (uint32_t i = 0; i < external_count; i++)
    external_map[i] = (TSSymbol) dyn_int (r);
  uint32_t external_states = (uint32_t) dyn_int (r);
  bool *valid = calloc ((size_t) external_states * external_count + 1, sizeof (bool));
  for (uint32_t i = 0; i < external_states * external_count; i++)
    valid[i] = dyn_int (r) != 0;
  l->external_scanner.symbol_map = external_map;
  l->external_scanner.states = valid;

  TSStateId *primary = calloc (state_count ? state_count : 1, sizeof (TSStateId));
  for (uint32_t s = 0; s < state_count; s++) primary[s] = (TSStateId) s;
  l->primary_state_ids = primary;

  /* the lexers */
  dyn_lex_table (r, &d->lexer);
  dyn_lex_table (r, &d->keyword_lexer);
  if (r->failed) DYN_FAIL ("bad lex tables");

  d->slot = slot;
  dyn_slots[slot] = d;
  l->lex_fn = dyn_lex_fns[slot];
  l->keyword_lex_fn = d->keyword_lexer.state_count > 0 ? dyn_keyword_fns[slot] : NULL;
  return l;
}
