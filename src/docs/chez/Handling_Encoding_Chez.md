# Handling Encoding in Chez Scheme (`wip-chez`)

## 1. Problem Statement: String Encoding Duality

In TeXmacs C++, a single `string` type is overloaded for three distinct payloads:
1. **UTF-8 text** — UI strings, URLs, HTML, Markdown, modern LaTeX, etc.
2. **8-bit Cork (T1) font strings** — `.tm` document content, font metrics, hyphenation, spellchecking (`0x00`–`0xFF`, e.g. `0xA3` = `č`, `0xE9` = `é`, `0xE1` = `á`).
3. **Raw binary blobs** — PNG/JPEG images, TrueType font tables, embedded raw data.

In Guile 1.8 and s7 (`s7_tm.cpp`), Scheme strings are raw 8-bit byte arrays (`s7_make_string_with_length` / `s7_string`), so all three pass through Scheme untouched.  
In **Chez Scheme**, however, native strings are 32-bit Unicode scalar arrays (`uint32_t`). Decoding raw Cork or binary bytes with `Sstring_utf8` replaces invalid UTF-8 bytes (`0x80`–`0xFF`) with `U+FFFD` (`\ufffd`), which later becomes `'?'` (`0x3F`).

---

## 2. Observed Failure: `conversions.scm` Benchmark vs. `mgubi/texmacs` (`wip_other_schemes` / s7)

Comparing `docs/chez/bench/conversions.scm` (`wip-chez` `954e85471`) against `mgubi/texmacs` (`wip_other_schemes`, built with `--with-scheme=s7`):

- `bigtable-test.tex` is byte-identical (`16,142` bytes).
- `change-log.en.tm` contains raw Cork bytes in three names:
  - Line 114: `Peter Rap\xa3an` (`0xA3` = `č`)
  - Line 1039: `Johann Dr\xe9o` (`0xE9` = `é`)
  - Line 1611: `M\xe1rcio Laurini` (`0xE1` = `á`)
- **Exported `change-log.en.tex`:**
  - `mgubi` (s7): `Peter Rap{\v c}an`, `Johann Dr{\'e}o`, `M{\'a}rcio Laurini`
  - `wip-chez`: `Peter Rap?an`, `Johann Dr?o`, `M?rcio Laurini`
- **Exported `change-log.en.html`:**
  - `mgubi` (s7): `Peter Rap&#x010D;an`, `Johann Dr&eacute;o`, `M&aacute;rcio Laurini`
  - `wip-chez`: `Peter Rap?an`, `Johann Dr?o`, `M?rcio Laurini`

### Root Cause
1. `import_tree` (`src/Texmacs/Data/new_buffer.cpp`) loads `change-log.en.tm` as raw Cork bytes into C++ `string s` and calls `generic_to_tree(s, "texmacs-document")` -> `call("generic->texmacs", s, "texmacs-document")` (`src/Data/Convert/Generic/generic.cpp`).
2. `call_scheme` in `src/Scheme/Chez/chez_tm.cpp` runs `to_scheme_arg(a)` on every argument, which calls `bytevector_to_sstring(bv)` -> `Sstring_utf8(...)`.
3. Because `0xA3`, `0xE9`, and `0xE1` are invalid UTF-8 sequences, `Sstring_utf8` turns them into `U+FFFD` *before* `generic->texmacs` runs.
4. When the parsed result returns to C++ (`texmacs_document_to_tree` / `utf8_to_cork`), `U+FFFD` becomes ASCII `'?'` (`63`).

### Other C++ -> Scheme `call(...)` Sites Affected
- `call("generic->texmacs", s, "texmacs-document")` (`src/Data/Convert/Generic/generic.cpp:60`)
- `call("texmacs->generic", doc, "texmacs-document")` (`src/Data/Convert/Generic/generic.cpp:65`)
- `call("spell-user-words", lan)` (`src/System/Language/language.cpp:461`) — spellchecker/hyphenation words are in Cork
- `call("tmtex-env-patch", t, l)` (`src/Edit/Editor/edit_typeset.cpp:849`) — tree/stree leaves are in Cork
- `string-load` on `.tm` files (`TeXmacs/progs/kernel/boot/boot-chez.scm:882-903`) — fails `tm-text-transcoder` UTF-8 decoding and falls back to returning a `bytevector` instead of a `string`

---

## 3. Why the Current Whitelist Approach Is Fragile

Currently:
- **Scheme -> C++** (`src/Scheme/Chez/build-glue-chez.scm`) uses a hardcoded **procedure-name whitelist** (`"cork->utf8"`, `"utf8->cork"`, `"tree->stree"`, `"stree->tree"`, etc.) to pick `recur-string->cork` / `recur-cork->string` instead of `recur-string->utf8` / `recur-utf8->string`.
- **C++ -> Scheme** (`src/Scheme/Chez/chez_tm.cpp`) has **no** dispatch at all and assumes all `bytevector`s are UTF-8 in `to_scheme_arg`.

This breaks whenever a C++ -> Scheme `call(...)` passes Cork data (like `generic->texmacs` with `"texmacs-document"`), or whenever a new glue procedure is added without updating the whitelist.

---

## 4. Systematic Solution: Type-Based Dispatching

Instead of guessing by procedure name, distinguish the three payload kinds **in the C++ type system** at the FFI boundary. Because TeXmacs `string` is a lightweight refcounted handle (`sizeof(void*)`), wrapper types have zero copy overhead.

### 4.1 Define Explicit C++ Wrapper Types

```cpp
// src/Kernel/Types/string.hpp (or src/Scheme/Chez/chez_types.hpp)

// UTF-8 text remains plain `string` (the common case for UI, URLs, HTML, etc.)

// 8-bit Cork (T1) encoded string
struct cork_string {
  string data;
  cork_string () {}
  explicit cork_string (const string& s) : data (s) {}
};

// Raw binary bytes (PNG, JPEG, fonts, unparsed blobs)
struct raw_bytes {
  string data;
  raw_bytes () {}
  explicit raw_bytes (const string& s) : data (s) {}
};
```

### 4.2 Direct Type-Driven Conversion in `chez_tm.cpp`

Instead of converting everything to a `bytevector` and re-walking it in `to_scheme_arg` / `from_scheme_result`, each C++ type converts directly to its exact Chez representation:

| C++ Type | Chez Scheme Representation | `*_to_tmscm` | `tmscm_to_*` |
|---|---|---|---|
| `string` (UTF-8) | Chez `string` (Unicode) | `Sstring_utf8` | `string->utf8` -> `string` |
| `cork_string` | Chez `string` (Latin-1 container, `U+0000`–`U+00FF`) | 1 byte -> 1 `string_char` | `Sstring_ref` low byte -> `cork_string` |
| `raw_bytes` | Chez `bytevector` (`#vu8(...)`) | `Smake_bytevector` | `Sbytevector_data` -> `raw_bytes` |

```cpp
// C++ -> Chez
inline tmscm
cork_to_tmscm (const cork_string& c) {
  iptr n = (iptr) N (c.data);
  tmscm s = Smake_string (n, 0);
  for (iptr i = 0; i < n; i++)
    Sstring_set (s, i, (unsigned char) c.data[i]); // lossless 0x00..0xFF
  return s;
}

inline tmscm
bytes_to_tmscm (const raw_bytes& b) {
  iptr n = (iptr) N (b.data);
  tmscm bv = Smake_bytevector (n, 0);
  memcpy (Sbytevector_data (bv), &b.data[0], n);
  return bv;
}

// Chez -> C++
inline cork_string
tmscm_to_cork (tmscm s) {
  if (Sstringp (s)) {
    iptr n = Sstring_length (s);
    string r ((int) n);
    for (iptr i = 0; i < n; i++)
      r[i] = (char) (Sstring_ref (s, i) & 0xFF);
    return cork_string (r);
  }
  if (Sbytevectorp (s)) { // also accept raw bytevectors seamlessly
    iptr n = Sbytevector_length (s);
    string r ((int) n);
    memcpy (&r[0], Sbytevector_data (s), n);
    return cork_string (r);
  }
  FAILED ("string or bytevector expected in tmscm_to_cork");
  return cork_string ();
}
```

And `to_scheme_arg` / `from_scheme_result` become unnecessary for strings if `tmscm` objects already carry their true Chez type (`string` vs `bytevector`):
- `call("generic->texmacs", cork_string (s), fm)` (when `fm` is `"texmacs-document"` or `"texmacs-snippet"`) produces a lossless Latin-1 Chez `string` where `(string? s)`, `string-starts?`, `substring`, etc. all work in Scheme without `U+FFFD`.
- `call("generic->texmacs", s, "html-document")` passes `string s`, which `string_to_tmscm` converts with `Sstring_utf8`.
- `call("...", raw_bytes (bin))` passes a Chez `bytevector` that never gets decoded as UTF-8.

### 4.3 Overloaded `call(...)` / `object` Construction on the C++ Side

In `src/Scheme/Scheme/object.hpp` (or `chez_tm.cpp`):
```cpp
inline object::object (const cork_string& c) : rep (cork_to_tmscm (c)) {}
inline object::object (const raw_bytes& b)   : rep (bytes_to_tmscm (b)) {}
```
Then C++ callers only annotate non-UTF-8 payloads at the call site:
```cpp
// src/Data/Convert/Generic/generic.cpp
tree
generic_to_tree (string s, string fm) {
  if (fm == "texmacs-document" || fm == "texmacs-snippet")
    return as_tree (call ("generic->texmacs", cork_string (s), fm));
  return as_tree (call ("generic->texmacs", s, fm));
}
```
(Or for `"texmacs-document"` / `"texmacs-snippet"`, `import_loaded_tree` / `generic_to_tree` can also fast-path directly to C++ `texmacs_document_to_tree(s)`.)

### 4.4 Type-Driven Glue Generation (`build-glue-chez.scm`)

In `src/Scheme/Glue/build-glue-basic.scm`, glue entries declare their parameter and return types:
```scheme
;; Before: both are `string`, so build-glue-chez.scm needs a name whitelist
(cork->utf8 cork_to_utf8 (string string))
(utf8->cork utf8_to_cork (string string))

;; After: types express the encoding directly
(cork->utf8 cork_to_utf8 (string cork_string))
(utf8->cork utf8_to_cork (cork_string string))
(parse-texmacs texmacs_document_to_tree (tree cork_string))
(serialize-texmacs tree_to_texmacs (cork_string tree))
```
Then `build-glue-chez.scm` simply maps each declared type to its marshaller — **no procedure-name whitelist needed**:
- `string` -> `recur-string->utf8` / `recur-utf8->string` (or `Sstring_utf8` in C)
- `cork_string` -> `recur-string->cork` / `recur-cork->string` (or `cork_to_tmscm` / `tmscm_to_cork` in C)
- `raw_bytes` -> pass `bytevector` directly (`bytes_to_tmscm` / `tmscm_to_bytes`)

For `tree->stree` / `stree->tree` (whose `object` tree leaves are Cork strings), `tree_to_stree` can return `cork_string` leaves (or `stree` can be a `cork_object` type in glue) so `build-glue-chez.scm` dispatches on the `cork_object` type as well.

### 4.5 Compatibility with Guile and s7

In `guile_tm.hpp` and `s7_tm.hpp`, `cork_string` and `raw_bytes` are trivial pass-throughs to `string_to_tmscm (x.data)` / `tmscm_to_string (x)` — zero behavior change for Guile or s7.

---

## 5. Keeping Upstream Glue Untouched: `*chez-encoding-overrides*` (Implemented)

Because `glue_basic.cpp` already passes raw `bytevector`s via `string_to_tmscm` / `tmscm_to_string`, we do **not** need to modify `build-glue-*.scm` or regenerate `glue_basic.cpp` to get per-argument encoding control on Scheme -> C++ calls.

Instead, `src/Scheme/Chez/build-glue-chez.scm` now defines `*chez-encoding-overrides*`:
```scheme
;; Format: (proc-name (ret-enc arg1-enc arg2-enc ...))
;; Encodings: 'cork | 'utf8 | 'raw | #f (default from spec type)
(define *chez-encoding-overrides*
  '(("cork->utf8"                (utf8 cork))
    ("cork->sourcecode"          (utf8 cork))
    ("escape-to-ascii"           (utf8 cork))
    ("parse-texmacs"             (utf8 cork))
    ("parse-texmacs-snippet"     (utf8 cork))
    ("utf8->cork"                (cork utf8))
    ("sourcecode->cork"          (cork utf8))
    ("serialize-texmacs"         (cork utf8))
    ("serialize-texmacs-snippet" (cork utf8))))
```
`lookup-encoding-override` checks arity against `build-glue-*.scm` at generation time so any upstream signature change fails loudly.

---

## 6. `generic->texmacs` Call Trace & Why the 2nd Argument Matters

1. **C++ entry** (`src/Data/Convert/Generic/generic.cpp:59-61`):
   ```cpp
   tree generic_to_tree (string s, string fm) {
     return as_tree (call ("generic->texmacs", s, fm));
   }
   ```
2. **C++ -> Chez FFI** (`src/Scheme/Chez/chez_tm.cpp:506-510`):
   `call_scheme` wraps `s` and `fm` as Chez bytevectors (`string_to_tmscm`), then `to_scheme_arg` decodes **both** with `Sstring_utf8(...)` before invoking Scheme.
3. **Scheme dispatcher** (`TeXmacs/progs/kernel/texmacs/tm-convert.scm:290-292`):
   `(generic->texmacs s fm)` runs `(convert s fm "texmacs-tree")`.

The 2nd argument `fm` selects both the input format and the converter:
- **`fm` = `"texmacs-document"` / `"texmacs-snippet"`** -> dispatches to `parse-texmacs` / `parse-texmacs-snippet` (`TeXmacs/progs/convert/rewrite/init-rewrite.scm:38-48`). `s` is **8-bit Cork**, so `Sstring_utf8` corrupts bytes `>= 0x80` to `U+FFFD` (`?`) before `parse-texmacs` runs.
- **`fm` = `"html-document"`, `"latex-document"`, `"markdown-document"`, etc.** -> `s` is **UTF-8**, so `Sstring_utf8` is required.

### Options for Fixing C++ -> Scheme (`call_scheme`)
- **Option A (Chez-only override in `chez_tm.cpp`):** In `call_scheme` (`src/Scheme/Chez/chez_tm.cpp`), check procedure name + format (e.g. `"generic->texmacs"` when `fm` is `"texmacs-document"` / `"texmacs-snippet"`, or `"spell-user-words"`) and decode `bytevector` arguments as Latin-1 instead of `Sstring_utf8`. Zero diff to upstream C++ files.
- **Option B (Fast-path `"texmacs-document"` in C++ + `cork_string`):** Since `parse-texmacs` is itself C++ (`texmacs_document_to_tree`), `generic_to_tree` / `import_loaded_tree` calling `texmacs_document_to_tree(s)` directly for `"texmacs-document"` / `"texmacs-snippet"` avoids the C++ -> Scheme -> C++ round-trip entirely (while Option A or `cork_string` handles remaining Cork `call(...)` sites like `texmacs->generic` and `spell-user-words`).
