# Plan: TMU and TMX, TeXmacs documents in Unicode

Status: proposal, 2026-10-19. Reference tree: `upsilon_texmacs` checkout at
`~/t/lab/fix/plan-ref` (paths below are relative to its `src/`). Mogan sources
read from a shallow clone of `MoganLab/mogan` (HEAD `af724b028e`, 2026-10-19),
now deleted. A web search found no public spec or blog post about TMU; the
code and Mogan's `devel/*.md` task notes are the only sources.

## 1. Summary

- `.tm` is a 7-bit-safe serialization of a tree whose strings are in TeXmacs'
  internal **Cork** encoding. Non-Cork characters are entities: `\<#5173\>`.
  Old TeXmacs versions read this format and never silently misread it.
- **TMU** (Mogan, 2024) uses the same grammar. Before writing, it converts every
  string from Cork to UTF-8, and it converts back after reading. Its header is
  `<TMU|<tuple|1.1.0|XMACS_VERSION>>`. Mogan now uses `.tmu` for new
  documents, drafts, backups and its own docs.
- Mogan's TMU is close to lossless but not quite. It leaks control characters,
  shows backquotes as `‘`, uses a slightly different Cork table and skips the
  version upgrade.
- Recommendation:
  - (a) Read and write TMU as an extra format, byte-compatible with Mogan where
    it matters.
  - (b) Define **TMX** as "TMU with a strict, specified escaping": every
    character that is not safe to write literally is written as `\<#x\>`.
    TMX files stay readable by Mogan as TMU.
  - (c) Only later, and behind a preference, make it the default for new files.
    Never put UTF-8 into a file whose header is `<TeXmacs|…>`. Upstream tried
    that in 2022 and reverted it as "dangerous" (see §3.4).

## 2. What `.tm` is, precisely

### 2.1 Encoding of strings
- In memory, every TeXmacs string is a byte string in a Cork-like encoding:
  - Bytes `0x00–0xFF` are the T1/Cork glyphs. The table is
    `TeXmacs/langs/encoding/corktounicode.scm`. Examples: `0x10/0x11` = `“ ”`,
    `0x15` = `–`, `0x60` = `‘` (line 108), `0x00` = `` ` `` (line 10),
    `0x17` = U+2060 (line 33), `0xFF` = `ß` (line 267).
  - A character outside Cork is the entity `<#hex>`, e.g. `<#5173>`.
  - A named symbol is `<name>`, e.g. `<alpha>`, `<leq>`, `<less>` (a literal
    `<`), `<gtr>`. A well-formed string therefore never contains a bare `<`
    byte that is not the start of a symbol.
- The conversions are in `src/Data/String/converter.cpp`:
  - `utf8_to_cork` (:326) and `cork_to_utf8` (:378).
  - Both use `converter_rep::load` (:102). This merges `corktounicode`,
    `tmuniversaltounicode` (1542 lines: `<alpha>`→α, `<leq>`→≤…), several
    one-way tables and `symbol-unicode-math`.
- Because of these merged tables, `cork_to_utf8` is **lossy**: `<alpha>` and
  `<#3B1>` both become `α`. In the other direction, `utf8_to_cork("α")` gives
  `<alpha>`, not `<#3B1>`, and `unicode-cork-oneway.scm` folds U+00A0, U+2009
  and U+2019 to ASCII.
- So these functions **cannot** be reused for a lossless file format. A plain
  bijection on the 256 bytes plus `<#x>` is needed. Mogan's "Herk" codec is
  exactly that (§3.2).

### 2.2 Grammar and escaping (writer `src/Data/Convert/Texmacs/totm.cpp`, reader `fromtm.cpp`)
- **Tags:**
  - Inline form: `<tag|a1|a2>`.
  - Block form for arguments that are documents: `<\tag>…<|tag>…</tag>`
    (`tm_writer::apply`, totm.cpp:196-252).
  - `<\collection>` (:296-308).
  - Raw data is a **bare** `<#hex bytes>` (:263-271; reader fromtm.cpp:273-284).
  - `expand` with an alphabetic macro name is written as the macro name itself
    (:284-295).
- **Escapes in strings** (`tm_writer::write`, totm.cpp:140-171):
  - `\\`, `\<`, `\|`, `\>`.
  - `\ ` for a significant space.
  - `\n \t \0`.
  - Other control bytes as `\@…\_`. For example, Cork `0x15` (en dash) is
    written `\U` and the quotes are `\P…\Q`.
  - Bytes ≥ 0x80 are written raw (Latin-1-looking Cork bytes).
  - So a non-Cork character appears as `\<#5173\>` (escaped `<` and `>`
    around an entity). A bare `<#…>` would be raw data.
  - The reader's `decode` (fromtm.cpp:69-87) inverts all of these.
- **Whitespace:**
  - A run of blanks with at most one newline is one space. Two or more newlines
    are a paragraph break between `document` children (fromtm.cpp:100-112).
  - An empty paragraph is `\;` (totm.cpp:122-138).
  - The writer wraps lines at 78 *bytes*. It breaks only at spaces and indents
    by 2 per block level, up to 20 (`flush` :61-108, `cr` :45-59).
  - A final space is protected (:337-341, an Upsilon fix, commit 6444a81efe).
  - `\`+newline is a line continuation in the reader (:91-94).
- **Reading is byte-wise:** `read_char` returns exactly one byte
  (fromtm.cpp:89-98).

### 2.3 Header and versioning
- The first element is `<TeXmacs|2.1.4>`. Upsilon is 2.1.5 per
  `misc/m4/tm_version.m4`. In the docs tree, 371 files say 2.1.4, and many say
  1.99.x.
- `texmacs_document_to_tree` (fromtm.cpp:358-413) reads the version and parses
  with `get_codes(version)` (upgradetm.cpp:42), which renames old tags. It then
  calls `upgrade(doc, version)` (upgradetm.cpp:4248), a chain of
  `version_inf_eq` steps up to 2.1.2.
- Pre-1.0 formats (`edit…`, `TeXmacs…`) are also handled.
- **Format detection:**
  - `texmacs-recognizes?` checks the prefix `<TeXmacs`
    (`progs/convert/rewrite/init-rewrite.scm:20-30`), for suffixes `tm ts tp`.
  - `import_loaded_tree` (`src/Texmacs/Data/new_buffer.cpp:450-462`) maps the
    suffix to a format through Scheme `format-from-suffix`
    (`src/Data/Convert/Generic/generic.cpp:39-51`, `System/Files/file.cpp:486`).
    It sniffs `(document (TeXmacs` as `stm`.
  - Saving goes through `export_tree` → `texmacs->generic` (new_buffer.cpp:527-550).
- **Places hard-wired to `.tm`/`"texmacs"`:**
  - autosave (`tm-files.scm:396-398` accepts only `texmacs`/`stm`)
  - `has-faithful-format?` (:156)
  - `load_style_tree` (`.ts`, new_buffer.cpp:493-510)
  - `include` (`Style/Evaluate/evaluate_control.cpp:138`,
    `Typeset/Env/env_exec.cpp:1243`)
  - `tree_cache.cpp:596`, `indexation.cpp:160`
  - the merge driver (§6)
  - about 30 `"tm"` literals in `progs/`
- A test currently asserts that the `tmu` format does not exist:
  `progs/check/convert-more-test.scm:982`.

## 3. What TMU is, precisely

### 3.1 Implementation in Mogan
- `TeXmacs/plugins/tmu/progs/tmu/tmu-format.scm`, which is lazily loaded by
  `init-research.scm:529`:
  - `(define-format tmu (:name "TMU") (:suffix "tmu"))`. No `:must-recognize`.
  - `texmacs->tmu = serialize-tmu ∘ herk-tree->utf8-tree`.
  - `tmu->texmacs = utf8-tree->herk-tree ∘ parse-tmu`.
- Glue: `src/Scheme/L4/glue_convert.lua:59-80` and
  `src/Scheme/L3/glue_moebius.lua:690-699`.
- Codec: `moebius/Data/Convert/tmu.cpp`. It is a copy of `fromtm/totm` with
  these changes:
  - `read_char` decodes one **UTF-8 code point** (:303-314).
  - `decode` handles only `\;` and `\\`; every other `\x` gives `x` (:286-300).
  - The writer escapes only space, `\`, `<`, `|` and `>` (:646-671). It has no
    `\n \t \0` or `\@..\_` escapes.
  - There is no 78-column wrapping: there is no `xpos`, so each paragraph is
    one line (`flush` :606-613).
  - It does not protect a final space. It appends a final `"\n"` (:833).
  - Header (:811-835): `<TeXmacs|…>` is replaced by `<TMU|<tuple|1.1.0|2026.3.0>>`.
    `TMU_VERSION` is `"1.1.0"` (:564). The second field is Mogan's own
    `XMACS_VERSION`, for example 2025.2.2.
  - `tmu_document_to_tree` (:841-868) requires the exact prefix
    `<TMU|<tuple|`, with no BOM and no leading blank. It parses with
    `get_codes(xmacs_version)`, which for any year-numbered version means the
    current codes. It inserts `<TeXmacs|2.1.4>`, and it **does not call
    `upgrade`**.
  - The style flattening `<style|<tuple|x>>`→`<style|x>` is the same as in
    `.tm` (totm.cpp:321-333).
- Tree conversion: `tree_herk_to_utf8` / `tree_utf8_to_herk`
  (`moebius/Data/Tree/tree_traverse.cpp:837-868`). They map every atomic label
  and skip `raw-data`.
- Mogan's use of TMU:
  - New buffers and drafts are `.tmu` (`src/Texmacs/Data/new_buffer.cpp:226`,
    `tm-files.scm:1221,1259`).
  - Backups are TMU (devel/200_60.md).
  - Documentation is migrating: 346 `.tmu` files in the clone, plugin docs
    "tmu first, tm fallback" (devel/214_1.md, 214_6.md).
  - The doc style is `tmudoc`. Links point to `*.tmu`.

### 3.2 "Herk" = Mogan's lossless Cork↔UTF-8 bijection
- `lolly/lolly/data/herk.cpp` (2026) is a fixed 256-entry table (:19-49) plus
  `<#hex>` entities:
  - `herk_to_utf8` (:156-173) maps each byte through the table and each
    `<#x>` to code point x. It leaves `<name>` symbols as ASCII.
  - `utf8_to_herk` (:136-154) maps the inverse table. Any other code point
    becomes `<#X>` (or `<#0X>` below 16). Printable ASCII stays as it is.
- Compared with our `corktounicode.scm`, **five bytes differ** (checked by
  script):

  | Cork byte | Herk | Upsilon `corktounicode` |
  |---|---|---|
  | 0x17 | U+200B | U+2060 |
  | 0x18 | U+2080 | (none) |
  | 0x1A | U+0237 | (none) |
  | 0x7F | U+00AD | U+2010 |
  | 0xDF | U+1E9E | (`SS`, one-way) |

- Named symbols are **not** converted. `<alpha>` is written in TMU as
  `\<alpha\>` and a literal `<` as `\<less\>`, exactly as in `.tm`. Only Cork
  bytes and `<#x>` entities become real Unicode.

### 3.3 Defects of TMU as a lossless format (what we must not copy)
1. **Control characters leak.** A Cork string that contains `<#9>` (a tab; our
   source-tabs work stores tabs this way) or `<#A>` becomes a raw TAB/LF in
   the file. The reader then collapses it as whitespace, so the data is lost.
   Mogan's own `decode` also turns `\t` into `t`.
2. **Backquote.** Cork `0x60` is `‘` in both tables, and ASCII `` ` `` maps to
   Cork `0x00` (grave accent).
   - In our doc tree, 294 files contain `` ` ``, for example ``` ``Text before'' ```
     and Scheme `` `(with …) ``. In TMU these show as `‘‘Text before''` and
     `‘(with …)`.
   - Worse, an LLM or a human who types `` ` `` in a `.tmu` file gets Cork
     `0x00`, a different character.
3. **Invisible or ambiguous code points are written raw**: U+00AD, U+200B,
   U+2060, U+FEFF, combining marks at the start of a string, bidi controls,
   non-NFC sequences. These are lossless but invisible in review. A text editor
   that normalizes to NFC would break the round trip.
4. **No upgrade step.** An old TeXmacs tree saved as TMU by an old Mogan is not
   migrated, and the version field is Mogan's calendar version, not a format
   version.
5. **Strict header**: a BOM or a leading newline makes it "bad format".

### 3.4 History
- Upstream TeXmacs svn r13889 (commit `f6d509e1db`, 2022, by Darcy Shen) added
  a preference "tm format with utf8" that wrote `<#xxxx>` as UTF-8 *inside
  `.tm`*. Joris reverted it in r13916 (`040ac16d52`, "Revert dangerous change
  to TeXmacs format").
- The danger: any older TeXmacs reads those UTF-8 bytes as Cork and silently
  corrupts the text.
- TMU's distinct suffix and header avoid this. We must keep that property.

## 4. Options

### (a) TMU as an extra format (read + write)
- **Read:** accept `<TMU|<tuple|M|X>>`. Optionally also accept a BOM and
  leading blanks. Parse UTF-8, then convert with a Herk-compatible table, so
  that Mogan's 0x17/0x7F/0xDF choices round-trip. Then `upgrade(doc, "2.1.4")`
  when `M == "1.1.0"`, because Mogan TMU means TeXmacs 2.1.4 trees.
- **Write:** UTF-8 tree with the Herk table, keeping 78-column wrapping
  optional. Characters that TMU cannot carry (C0 controls, the invisible list
  in §3.3) are written as `\<#x\>`. Mogan reads `\<#9\>` → `<#9>` → identical
  Cork, so this stays compatible.
- **Value:** Mogan users can send us files. The tag set is a separate issue:
  Mogan-only tags such as `stem-doc-id` and `tmudoc` load as unknown macros.
- **Cost:** small. About 600 lines of C++ (shared with (b)) plus about 40 lines
  of Scheme.

### (b) TMX: our own UTF-8 format, specified and strict
- Define TMX as **TMU grammar with a written spec**. Proposed header:
  `<TMX|<tuple|1.0|2.1.5>>`. The second field is a **TeXmacs format version**,
  fed to `get_codes`/`upgrade`. Then make the escaping canonical:
  - Every code point other than `\ < | >`, space and the cases below is written
    literally in NFC-stable form.
  - These are always written `\<#x\>`:
    - C0/C1 controls, U+007F
    - U+00AD, U+200B–U+200F, U+202A–U+202E, U+2060–U+2064, U+FEFF
    - noncharacters, lone surrogates
    - a combining mark at the start of a string
    - any code point whose NFC differs from itself
  - Named symbols stay `\<name\>`. `<alpha>` vs `<#3B1>` is a real distinction
    (math symbol vs text letter).
  - The backquote fix:
    - Cork `0x60` ↔ U+0060, so ASCII is identity on 0x20–0x7E.
    - Cork `0x00` (grave accent) ↔ U+02CB, or always written as an escape;
      the maintainer decides (Q1).
    - U+2018 ↔ `<#2018>`, as `var_utf8_to_cork` (converter.cpp:343) already
      does for U+2018–U+201D.
  - Keep the `.tm` escapes `\n \t \0` *only* on the read side, for robustness.
    Never write them, because Mogan misreads them.
- TMX is then a **superset reader** (TMX, TMU and `.tm` headers) with a
  **canonical writer**.
- A TMX file read by Mogan as TMU is correct except for `` ` `` and the five
  table bytes. If maximum Mogan compatibility matters, use the Herk table
  verbatim and accept `‘`. See Q1.
- **Value:** we own the spec, and it carries an upgrade path and a format
  version.
- **Cost:** about the same as (a). The two share one codec with a "dialect"
  flag.

### (c) UTF-8 by default for new files
- Phase it: first a preference, `File > Save as > TMX` and the
  `save-as-utf8` scripts. Then the default for *new* buffers only. Existing
  `.tm` files keep their format on save, as `has-faithful-format?` already does.
- Never rewrite user files silently.
- Ship the docs tree as `.tm` until TMX has been stable for at least one
  release.

## 5. Compatibility

| Reader \ file | `.tm` | `.tmu` (Mogan) | `.tmx` (ours) |
|---|---|---|---|
| GNU TeXmacs ≤ 2.1.4 | yes | not recognized (suffix unknown → "generic"; `texmacs-recognizes?` fails) — no corruption | same — no corruption |
| Mogan | yes | yes | yes if renamed `.tmu` and header `TMU` (plan: our writer can emit either header) |
| Upsilon after plan | yes | yes | yes |

- Older TeXmacs never misreads TMU/TMX, because the header is unknown. That is
  the safety property the 2022 revert was about.
- To share with GNU TeXmacs users, `.tm` export stays one click away and must
  stay the default for "Export for TeXmacs".
- Mogan-specific trees (new tags) are an orthogonal issue. Log the unknown
  tags on import.

## 6. Round-trip and lossless guarantees

- **Guarantee G1 (tree level):**
  `parse_tmx(serialize_tmx(T)) == T` for every tree `T` that `.tm` can
  represent: every atomic label is an arbitrary Cork byte string, and every
  `raw-data` is arbitrary bytes. The proof obligations are:
  - The byte-to-UTF-8 map is injective on 256 bytes. No UTF-8 image may equal
    the image of `<#x>` for any x. Herk satisfies this; our modified table must
    be checked by a unit test over all 256 bytes and a sample of
    `<#x>`, x < 0x110000.
  - A Cork string may contain a *non-canonical* entity, e.g. `<#41>` (= `A`),
    lower-case hex, or `<#0041>`. Converting it to UTF-8 and back gives `A`.
    So TMX must **escape any entity whose UTF-8→Cork image would differ from
    its source**. Simplest rule: write `\<#x\>` literally when `x < 0x80`, when
    x is in the Cork table, or when the digits are not canonical upper-case.
    `utf8_to_cork` emits upper-case; check how existing docs spell them.
  - Malformed `<#` with no `>`: write the `<` escaped (`\<`). Our
    `cork_to_utf8` already treats this as a literal (converter.cpp:386).
  - Whitespace semantics are inherited unchanged from `.tm` (`\ `, `\;`,
    blank runs), so the only new rule is the C0 escaping above.
- **Guarantee G2 (file level):** `serialize(parse(F)) == F` for files written
  by our writer (canonical form). This is needed for clean diffs.
- **Not guaranteed:** byte identity of Mogan-written TMU after our re-save
  (no wrapping vs wrapping, Herk table differences). The trees are identical.

## 7. LLM-friendliness

- **What an LLM sees today:** Chinese text in `.tm` is
  `<tmdoc-title|\<#5173\>\<#4E8E\>GNU <TeXmacs>…>`, and `1998\U2021` for
  "1998–2021". German umlauts are raw Latin-1 bytes. These are invalid UTF-8,
  so many tools show `?` or refuse the file. Our own memory note
  "tm-files-cork-encoding" exists because agents kept writing raw UTF-8 into
  `.tm` files.
- **In TMX:** `<tmdoc-title|关于GNU <TeXmacs>…>`, `1998–2021`, `Größe`. The
  file is valid UTF-8, so editors, grep and git all work.
- **Measured size** (script over all 1670 `TeXmacs/doc/**/*.tm`, converting
  Cork bytes, control escapes and `\<#x\>`):

  | Docs | Size change |
  |---|---|
  | total | 9.42 MB → 9.36 MB |
  | zh | −19 % (458 KB → 369 KB) |
  | de/fr/es/pt/pl | +1–2 % (Latin-1 → 2-byte UTF-8) |

  1209 of the 1670 files change at all.
- **Token cost** (estimate, to be measured with the token-counting API in
  phase 1): `\<#5173\>` is 9 ASCII bytes, typically about 5–7 BPE tokens. The
  same CJK character is about 1–2 tokens. So CJK documents should cost
  roughly 3–5× fewer tokens. European text gains little in size but becomes
  *legible*: `\P…\Q` and `\U` disappear.
- **Writing errors:**
  - LLMs reliably emit UTF-8. With TMX, what they write is what is meant.
  - Remaining pitfalls, to spell out in a short "TMX for LLMs" doc page:
    - `\<` `\>` `\|` `\\` escaping
    - `<` in text as `\<less\>`
    - `\ ` for double spaces
    - blank line = paragraph
  - Mogan's TMU backquote mapping would be a *silent* trap for LLMs (`` ` ``
    becomes a grave accent). This is the main argument for the TMX table
    change (Q1).
- **Optional later:** an `.stm`-like "TMX-S" (S-expression in UTF-8), similar
  to Mogan's `.stem` (`plugins/stem/progs/stem/stem-format.scm`, which is
  `texmacs->stm` over a Herk UTF-8 tree). It is useful for tool APIs, not for
  files.

## 8. Git diffs and the structured merge driver

- **Diffs:**
  - Better: real characters instead of entities, and no non-UTF-8 bytes (git
    currently shows Latin-1 bytes as `<E4>`).
  - Worse, or at least different: `.tm` wraps at 78 bytes, so one edit can
    reflow the rest of a paragraph. TMU puts each paragraph on one line, which
    gives no reflow churn but very long lines, so use `git diff --word-diff`.
  - The 78-*byte* counter would also wrap CJK at about 26 characters. If we
    keep wrapping, count code points or display width.
  - Proposal: TMX writes **one paragraph per line** (Mogan-compatible) and adds
    a `.gitattributes` hint `diff=texmacs` with `wordRegex` covering CJK
    characters one by one.
- **Merge driver** (`progs/version/git-drivers.scm`):
  - `load-document` (:35-37) and `git-merge-files` (:49-50) hard-code the
    `"texmacs"` format. With `.tmx` the result would be written back as
    **`.tm` into a `.tmx` file**. Fix: use `(url-format (driver-url ours))` on
    both sides and refuse to merge when base/ours/theirs differ in format.
    Different formats can occur across a conversion commit, so: parse each
    with its own format and write in "ours".
  - `attributes-line` (:95) is `*.tm` only. It should install `*.tm`, `*.tmx`
    and `*.tmu`, and `git-merge-driver-installed?` must accept any of them.
  - `git-texmacs-file?` (`version-git.scm:43-44`) should add `tmx` and `tmu`.
  - `version-compare.scm` `denormalize-string` (:40-49) splits on spaces only.
    A CJK paragraph is therefore one token, and any edit conflicts the whole
    paragraph. Mogan fixed this (devel/0873.md) by splitting per `<#…>`. This
    is independent of the file format but becomes more visible with Unicode
    files. Port the fix, splitting per Cork entity, in phase 2.
  - A merge is format-agnostic on trees, so a TMX commit merged with a `.tm`
    branch works once the loaders use the right format.

## 9. Phased plan

**Phase 0: spec and tests first (1–2 days)**
- Write `src/docs/tmx-format.md`: grammar from §2.2, the escaping table, the
  Cork↔Unicode table (Herk plus the backquote decision), the header, and
  versioning.
- `tests/Data/String/cork_unicode_test.cpp`: bijection over 256 bytes and
  `<#x>` for x in [0, 0x10FFFF] (sampled), non-canonical entities, malformed
  `<#`.
- Golden files: a few `.tmu` taken from Mogan, each reviewed by hand and not
  run, stored under `tests/documents/tmu/`. Mind the GPL headers, which are
  fine.

**Phase 1: codec (3–4 days)**
- `src/src/Data/String/converter.cpp`: add `cork_to_unicode_strict` /
  `unicode_to_cork_strict` (table-driven, no dictionaries), with a `herk` flag
  for TMU.
- New `src/src/Data/Convert/Texmacs/fromtmx.cpp` and `totmx.cpp`:
  - Refactor `tm_reader`/`tm_writer` into a template or flags: `utf8`,
    `wrap`, `escape_set`, `header`.
  - Do not fork-copy as Mogan did, because 600 duplicated lines will drift.
  - Declare them in `convert.hpp`. Add the files to `misc/wasm/sources.txt`
    and to CMake.
- Glue (`build-glue-basic.scm` → `glue_basic.cpp`): `parse-tmx`,
  `serialize-tmx`, `parse-tmu`, `serialize-tmu`, snippet variants.
- `progs/convert/rewrite/init-rewrite.scm`:
  - `define-format tmx` (`:suffix "tmx"`, `:must-recognize` on `<TMX|`)
  - `define-format tmu` (`:suffix "tmu"`, recognizes `<TMU|`)
  - converters
- Update `convert-more-test.scm:982`.
- Effort: about 800 lines of C++ and 100 of Scheme.

**Phase 2: integration (2–3 days)**
- `tm-files.scm`:
  - `has-faithful-format?` (:156) and autosave (:398) accept `tmx`/`tmu`.
  - `new_buffer.cpp:425` `attach_subformat` treats them like `texmacs`.
  - `format-determine` sniffs `<TMX|`/`<TMU|` regardless of suffix.
  - `file-chooser.scm` filters.
- `include`, `load_style_tree`, `tree_cache`, `indexation`: call a single
  `any_texmacs_document_to_tree(s)` that dispatches on the header. Style files
  stay `.ts` (Cork) for now.
- `version/git-drivers.scm`, `version-git.scm`, `version-compare.scm` as in §8.
- `server/`, `tools/ai/ai-attachments.scm`, `doc/help-funcs.scm`: audit the
  ~30 `"tm"` literals.
- The AI tools: let LLM-facing APIs read and write TMX snippets instead of
  Cork `.tm`.

**Phase 3: browser/wasm and docs (1 day)**
- Add the new sources to `misc/wasm/sources.txt`. Add a "Recent changes" entry
  on the browser help page.
- User doc page "TMX format", plus a short LLM-oriented cheat sheet.

**Phase 4: optional default (later, after a release)**
- Preference "Default format for new documents: TeXmacs (.tm) / Unicode (.tmx)".
- Possibly convert `src/TeXmacs/doc` to TMX in a single commit. This needs
  link rewriting `*.tm` → `*.tmx` in `<branch|…>`/`<hlink|…>`, or resolver
  fallback ".tmx then .tm" as Mogan does (devel/214_1.md).

## 10. Test strategy

1. **Unit:** the codec tables (phase 0), escapes for every C0 byte, `\ `
   spacing, `\;`, final space, raw-data, collections, `expand` names.
2. **Corpus round-trip:** for every `*.tm`, `*.ts` and `*.tp` under
   `src/TeXmacs` (2056 files) and the `tests/documents` corpus, check:
   - `T = parse-texmacs(file)`, `S = serialize-tmx(T)`
   - `parse-tmx(S) == T` (tree equality)
   - `serialize-tmx(parse-tmx(S)) == S` (canonical)
   - `S` is valid UTF-8, with no C0 characters except `\n`
   - the TMU dialect gives the same tree when read back

   Run headless with `(quit-TeXmacs)` (S7 has no `(quit)`), as a harness
   suite `tmx`, with backends Guile/S7/femtolisp and wasm/node. Report per
   file. Expect the docs to pass at 100 %.
3. **Adversarial strings:** a generator of random Cork byte strings mixing
   `<#x>`, `<name>`, malformed `<#`, `<#0041>`, tabs, NUL, U+FEFF and
   combining marks, checked for G1.
4. **Mogan interop:** read the golden `.tmu` samples and compare with
   expected `.tm` (tree equality). Write TMU, hand it to Mogan manually
   (cannot be automated here), and check one Chinese and one code-heavy doc.
5. **Merge driver:** extend the `git` harness suite with a three-way merge of
   `.tmx` files, and a mixed `.tm`/`.tmx` conversion commit.
6. **No regressions:** `.tm` output must stay byte-identical for the whole doc
   tree when the writer is refactored. Diff the old vs new `tree_to_texmacs`
   over the corpus.

## 11. Risks

- **Refactoring `tm_reader`/`tm_writer`** risks subtle `.tm` regressions.
  Mitigation: test 6, plus keep the `.tm` path untouched until the corpus diff
  is empty.
- **Diverging from Mogan** (the backquote and the five table bytes) means a
  TMX file opened in Mogan shows `` ` `` where Cork `0x60` was. That
  difference is mild, and arguably Mogan's bug. Alternatively, adopt Herk
  verbatim (Q1).
- **Two Unicode formats** confuse users. Possible mitigations: expose only
  "TMX" in menus, keep "TMU" as import/export.
- **Very long lines** in one-paragraph-per-line output for big tables or
  graphics. Mogan lives with it, but look at the line length distribution on
  the corpus first.
- **Spec drift with Mogan's TMU** (1.1.0 today, uncontrolled). Pin to 1.1.0 and
  log unknown TMU versions.
- **NFC normalization by editors or LLM tooling**: covered by escaping non-NFC
  code points, but text that someone else wrote in NFD will be normalized on
  their side. That is acceptable.

## 12. Open questions for the maintainer

1. **Backquote/table:**
   - TMX table = Herk verbatim (max Mogan compatibility, `` ` `` shows as `‘`)?
   - Or ASCII-identity for `0x60` (LLM-safe, as recommended)?
   - And which Unicode character for Cork `0x00`/`0x17`/`0x7F`/`0xDF`?
2. **Suffix and header:** `.tmx` + `<TMX|…>`, or simply adopt `.tmu`/`<TMU|…>`
   with our stricter writer (one fewer format, but we then co-own a spec we do
   not control)?
3. **Line wrapping:** one paragraph per line (Mogan, fewer churn diffs) vs
   78-column wrapping counted in code points (closer to `.tm`)?
4. **Named symbols:** keep `\<alpha\>` and `\<less\>` as is? Or also write
   `<less>`/`<gtr>`/`<ldots>` as Unicode in *text* mode (requires
   mode-awareness in the writer, and is not purely an encoding change)?
5. **Version field:** should TMX carry the TeXmacs format version (2.1.5) only,
   or also an Upsilon build id?
6. **Should style files** (`.ts`) and project files (`.tp`) get Unicode
   variants, or stay Cork?
7. **Docs tree:** convert to TMX eventually (phase 4), or keep `.tm` forever
   for upstream compatibility?
8. **Mogan-only tags** (`stem-doc-id`, `tmudoc`, …) on TMU import: keep, strip,
   or map?
