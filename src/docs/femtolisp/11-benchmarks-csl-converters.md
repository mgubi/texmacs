# 11. Benchmarks of 2026-10-10: the CSL processor and the converters

femtolisp, S7 and Guile 1.8 on Scheme code which did not exist when the
interpreter was ported: the processor of the Citation Style Language
(branch `wip_csl`, about 4000 lines of Scheme) and the converters to and
from Markdown, Word (.docx) and OpenDocument (.odt). None of this code
needed a change to run on femtolisp.

## 11.1 Setup

- macOS 15.7 on an Apple M4, other work running. The builds were alternated
  on each workload; a number is the median of the warm runs, and a range is
  given where the runs disagreed.
- Three Qt builds of one source tree, which differ only by
  `--with-scheme=femtolisp|s7|guile` (Guile 1.8.7), run with
  `QT_QPA_PLATFORM=offscreen`, each with its own home, warmed by a first
  run.
  - CSL (11.2): `maxs_femto` (dae21f624e) with the CSL work of `wip_csl`
    (e57f86e833) applied as a patch. This tree is two days older than
    `upsilon_texmacs`: its `compat-s7.scm` lacks `string-trim-both`, so the S7
    build got the file of `wip_csl` (two more functions).
  - Converters (11.3): the merge of `wip_csl` and `maxs_femto` (no
    conflict), which has the converters of `upsilon_texmacs` of 2026-10-09.
- The scripts are in [bench](bench): the workloads `csl`, `csl-suites`,
  `converters` and `converter-suites` of `run.sh`.

## 11.2 The CSL processor

`bench/csl.scm`, one process:

- the 845 fixtures of the test suite of CSL (each one parses a style in
  XML and items in JSON, makes a processor and renders citations or a
  bibliography);
- the bibliographies of 300 references (`bench/mkbib.py`: repeated authors
  and years, so that the disambiguation works) in the styles APA, Chicago
  author-date and IEEE: sorting, disambiguation, rendering.

| seconds | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| 845 fixtures | 6.8 | 7.2 | 13.2 |
| 3 bibliographies of 300 references | 1.75 | 3.6 | 6.3 |
| parsing the .bib of 300 entries (mostly C++) | 0.52 | 0.54 | 0.65 |
| the whole process, with its start | 10.6 | 12.8 | 22.5 |
| peak memory | 397 MB | 377 MB | 583 MB |

`bench/csl-suites.scm`, the regression suites (documents are loaded,
updated and typeset, so the C++ of TeXmacs does most of the work):

| seconds | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| suite `csl` (128 checks) | 7.6 | 7.3 | 9.0 |
| suite `bibtex` (164 checks) | 1.84 | 1.81 | 2.21 |
| peak memory | 266 MB | 510 MB | 274 MB |

- femtolisp and Guile give the same results on the fixtures (646 pass,
  136 fail, no error), and the suites pass with the three interpreters.
- **S7 raised 4 errors `car: argument is a free cell` in each run of the
  fixtures** (644 or 643 pass) with `(*s7* 'cache-macro-expansions?)` on,
  which is how TeXmacs runs it. With the cache off the results are those of
  the others, at the same speed (7.3 s and 3.6 s).
- The first run of femtolisp, which compiles the files into its cache, took
  15.1 s for the whole process against 10.6 s; a part of the difference is
  the setup of a new home, which every interpreter pays (Guile: 29 s
  against 22 s).
- One round was slower for S7 (9.0 s on the fixtures against 7.0 and 7.2):
  differences below 10% between femtolisp and S7 mean nothing here.

## 11.3 The converters

`bench/converters.scm`: ten documents of the TeXmacs tree (the change log,
three pages of the developer documentation of 40 kB each, six pages of the
manual; 220 kB of `.tm` files) are exported to Markdown, .docx, .odt, LaTeX
and HTML and imported back from the first three, five times in one process.
The three interpreters produce outputs of the same sizes. Warm rounds, in
milliseconds for the ten documents:

| ms | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| TeXmacs to Markdown | 183 | 70 | 700–1130 |
| Markdown to TeXmacs | 294 | 200 | 900–1370 |
| TeXmacs to .docx | 167 | 121 | 720–1130 |
| .docx to TeXmacs | 155 | 185 | 470–640 |
| TeXmacs to .odt | 188 | 148 | 830–1370 |
| .odt to TeXmacs | 141 | 130 | 410–720 |
| TeXmacs to LaTeX | 553 | 696 | 2420–3900 |
| TeXmacs to HTML | 74 | 168 | 360–550 |
| peak memory | 245 MB | 463 MB | 230–280 MB |

The first round, which loads the converters, in milliseconds:

| ms | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| TeXmacs to Markdown | 240 | 116 | 960 |
| Markdown to TeXmacs | 305 | 255 | 1100 |
| TeXmacs to .docx | 187 | 153 | 890 |
| .docx to TeXmacs | 175 | 190 | 580 |
| TeXmacs to LaTeX | 720 | 890 | 3450 |

(with a warm cache of compiled files for femtolisp; in its very first run,
which compiles the converters, the export to Markdown took 404 ms)

`bench/converter-suites.scm`, the regression suites:

| seconds | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| suite `markdown` | 0.93–1.04 | 0.75 | 2.24 |
| suite `office` | 0.40 | 0.32 | 0.92 |

- The two runs of Guile differ by 50% (the machine was busier during the
  second one), hence the ranges; femtolisp and S7 varied by a few percent.
- **S7 is the fastest on the new converters, clearly so on the export to
  Markdown (2.6 times faster than femtolisp)**, and femtolisp on the older
  ones (LaTeX, HTML) and on the import of .docx. Why the Markdown exporter
  is slow on femtolisp was not looked at.
- Guile is 3 to 10 times slower than the faster of the two others.

## 11.4 Why the Markdown converters are slower on femtolisp

Investigated on 2026-10-11, with the builds of 11.3.

**Where.** The four phases of the round trip, warm, in milliseconds for the
ten documents:

| ms | femtolisp | S7 |
|---|---:|---:|
| TeXmacs tree to Markdown tree (`tmmarkdown.scm`) | 33 | 36 |
| Markdown tree to text (`markdownout.scm`) | 137 | 35 |
| text to Markdown tree (`markdownin.scm`) | 145 | 65–77 |
| Markdown tree to TeXmacs tree (`markdowntm.scm`) | 135 | 140 |

The work on trees is as fast as with S7. The two slow phases are loops over
the characters of strings: `string-ref`, comparisons of indices, `char=?`,
`in?` on short lists of characters, `char-alphabetic?`, one string for each
character and `apply string-append` at the end.

**What.** Sampling the serializer in a loop (`sample`, 3 s): 81% of the
samples are in the loop of the bytecode interpreter itself (`apply_cl`),
9% in the collector, 2% in the C functions for strings. The time is spent
running bytecode, not in the built-in functions and not collecting.

**Why.** An operation which the compiler turns into an instruction costs
the same as in S7; one which is a function of the Scheme library costs a
call, and several when it is defined with other library functions. 2
million evaluations in a loop, in milliseconds (the empty loop: 21 and 25):

| ms | femtolisp | S7 | defined in femtolisp as |
|---|---:|---:|---|
| `<`, `=`, `+`, `car`, `eqv?`, `string-ref`, `string-length` | 25–36 | 21–30 | an instruction, or a C function |
| `(> a b)` | 257–296 | 24–28 | `(apply nary< (reverse l))`, any number of arguments |
| `(>= a b)`, `(<= a b)` | 125–164 | 22–24 | `nary-compare` with a closure, any number of arguments |
| `(max a b)`, `(min a b)` | 123 | 16–22 | a fold over the rest of the arguments |
| `(modulo a b)` | 225 | 22 | `remainder`, `zero?`, `negative?` |
| `(char-numeric? c)`, `(char-whitespace? c)` | 348–360 | 18–24 | two calls of `>=` and `<=` |
| `(char-alphabetic? c)` | 751 | 25 | two functions which call `>=` and `<=` |
| `(char-upcase c)` | 425 | 23 | the same |
| `(memv c l)`, `(member c l)`, `(in? c l)`, 3 elements | 183–212 | 28–51 | recursive functions |
| `(assoc k l)`, 3 elements | 209 | 33 | a recursive function |
| `(char=? a b)`, `(string=? a b)` | 59–62 | 16–26 | functions with a rest argument |
| `(string<? a b)` | 166 | 23 | a closure around `%string-compare` |
| `(string-append a b)` | 192 | 47 | `apply` of the C function, then a copy |

The first three rows of slow functions come from `r5rs-femtolisp.scm`,
which redefines `>`, `<=` and `>=` for any number of arguments (patch
0017 did it for `<` and `=` in the compiler, which keeps the instruction
for two arguments); the others are the definitions of `system.lsp` and
of `r5rs-femtolisp.scm`.

**How much.** Two experiments, without changing the interpreter:

1. The same functions redefined at run time with a fast path for two
   arguments and no call of another library function (`>` is `(< b a)`,
   `char-numeric?` is two `<`...), with the same results:

   | | as built | fast paths | S7 |
   |---|---:|---:|---:|
   | Markdown tree to text | 137 ms | 101 ms | 35 ms |
   | text to Markdown tree | 145 ms | 120 ms | 65–77 ms |
   | TeXmacs to Markdown, whole | 187 ms | 153 ms | 70 ms |
   | CSL, 845 fixtures | 7.7 s | 6.4 s | 7.2 s |
   | CSL, 3 bibliographies | 1.85 s | 1.58 s | 3.6 s |

   (one pair of runs for the CSL rows; a second pair was disturbed by
   other work on the machine)

2. The loop of `mdout-escape` (the test of each character of a text) on
   one million characters, written as it is and rewritten by hand with only
   operations which are instructions (`eqv?` for `char=?`, a chain of
   `eqv?` for `in?`, `<` for the other comparisons):

   | ms | femtolisp | femtolisp, fast paths | S7 |
   |---|---:|---:|---:|
   | reading the characters with their neighbours | 339 | 151 | 87 |
   | with the tests, as written | 635 | 469 | 122–149 |
   | with the tests, instructions only | 246 | 246 | 105 |

So the calls of library functions are 60% of the time of this loop. With
none of them left, femtolisp is still 2.3 times slower than S7 on it: S7
compiles such a loop of fixnum and character tests into direct C calls,
and the bytecode of femtolisp is slower there, while it is faster on the
code which allocates and calls closures (`cons` 40 against 68, a call of a
`lambda` 36 against 82, `apply` 35 against 66).

**What could be done**, from the cheapest:

- fast paths for two arguments in `r5rs-femtolisp.scm` and `system.lsp`
  (experiment 1): no change to the interpreter, 15–25% on the string loops
  and on the CSL processor;
- the compiler could treat `(> a b)`, `(>= a b)`, `(<= a b)` and
  `(char=? a b)` with two arguments as it treats `<` and `=` (they are
  `(< b a)`, `(not (< a b))`, `(not (< b a))` and `(eqv? a b)`), and
  `memv`, `member`, `assoc`, the character predicates and `string-append`
  could be C functions of `fl_core.c`: this is what experiment 2 measures
  for the escaping loop (635 to about 250 ms);
- the converters could avoid one string and one test of nine characters for
  each character (find the next special character with `string-index` or a
  table, copy the text between two of them with `substring`): faster with
  every interpreter.

**What was done** (2026-10-11): the first and the third of these.

- `r5rs-femtolisp.scm`: `>`, `>=`, `<=`, `min`, `max`, `char<?` and the
  like with two arguments allocate nothing and call no library function;
  the functions on characters use `<` only; `quotient` and `remainder` test
  fixnums first; `modulo` calls only `remainder`. The results are the same
  for every number of arguments. 2 million evaluations: `>` 76 ms (was
  257–296), `>=` and `<=` 77 (125–164), `max` 66 (123), `char-numeric?` 81
  (348), `char-alphabetic?` 106 (751), `char-upcase` 108 (425).
- The Markdown converters (branch `markdown-faster-loops`, from
  `upsilon_texmacs`): the serializer and the tokenizer of the parser pass the
  ordinary characters with one look at a table and copy the text between
  two special characters in one piece; the lines are found by searching
  the ends of lines; the predicates on characters of the parser compare
  codes with `<`. The outputs are the same, byte for byte, for the ten
  documents and for a few texts with other ends of lines and escapes, with
  each interpreter.

With both, warm, in milliseconds for the ten documents (before in
parentheses, where it was measured; S7 and Guile only have the change of
the converters):

| ms | femtolisp | S7 | Guile 1.8 |
|---|---:|---:|---:|
| Markdown tree to text | 18–24 (137) | 10–11 (35) | 116–150 |
| text to Markdown tree | 82 (145) | 70 (65–77) | 342 |
| TeXmacs to Markdown, whole | 54–61 (183) | 56–58 (70) | 360–380 (700–1130) |
| Markdown to TeXmacs, whole | 230 (294) | 208 (200) | 670–700 (900–1370) |

and the CSL processor on femtolisp: the fixtures in 6.1 s (was 6.8), the
three bibliographies in 1.54 s (was 1.75). The export to Markdown is now as
fast on femtolisp as on S7; the import is 10% slower.

While running these, S7 raised the error `car: argument is a free cell`
once in the conversions and once in the CSL fixtures with
`cache-macro-expansions?` off: the defect noted in 11.2 is not only in the
cache of macro expansions.

## 11.5 What it says

- On code written without femtolisp in mind, it gives the same results as
  Guile and is between 2.6 times slower and 2.3 times faster than S7,
  depending on the code; on the CSL processor, the longest of these
  workloads, it is the fastest.
- S7 uses about twice the memory of femtolisp in the workloads where the
  typesetter and the converters run (460–510 MB against 245–266 MB), and
  about the same in the run of the fixtures.
- The errors of S7 in the fixtures of CSL are a defect to look for in S7 or
  in its local patches; they were also seen once with the cache of macro
  expansions off, in another build.
