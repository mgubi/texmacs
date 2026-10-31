# Unit Tests

## Guide to Run Unit Tests for scheme
```
TeXmacs -x "(run-all-tests)" -q
```
or launch a Scheme session and then run `(run-all-tests)`.

## Guide to Run Unit Tests for cpp

First, compile the whole project with the tests enabled. That harness
links `Qt5::Test` (`tests/CMakeLists.txt`), which the tests no longer use
(they are plain C++ since 2026-10-18), so it needs a Qt 5 configuration;
use the autotools harness described below otherwise. At
present the top-level `CMakeLists.txt` neither adds the `tests` directory
nor defines `BUILD_TESTS`, so the recipe below needs that wiring first;
the autotools harness works as it is.
```
cd texmacs/
mkdir build/ && cd build/
cmake -DBUILD_TESTS=ON ..
make -j8
```

Then, run your unit tests:
```
ctest                # run all
ctest -R analyze     # run unit tests with name containing `analyze`
```

### Advanced Topic
You may also run the unit tests via the binaries under `${cmake_build_dir}/tests/`
``` bash
tests/converter_test
```

However, this specific unit test will fail. For `utf8_to_cork`, we need to set
the `TEXMACS_PATH` to find the dictionaries. You may specify it manually:
``` bash
TEXMACS_PATH=/path/to/somewhere tests/converter_test
```

Or just using ctest(we've set the necessary environment variables):
``` bash
ctest -R converter_test
```

## Unit tests with the autotools build

The CMake harness above needs a CMake build. With the usual
`./configure && make` build, `tests/Makefile` compiles the same test sources
against the objects in `src/Objects`:

```
make -C tests                      # build and run all tests
make -C tests run-path_test        # one test, with its own output
```

The tests are written in ordinary TeXmacs C++ over `tests/tm_test.hpp`,
which brings `CHECK`, `CHECK_MSG`, `CHECK_EQ` (any two values which `==`
compares and a `tm_ostream` prints) and `SKIP`, and a `main` which lists its
tests with `RUN` and returns `test_report ()`. They need no Qt, so every
build runs all of them (Qt, Vue, Cocoa); a test of Qt code skips its checks
in a build without Qt (`qt_utilities_test`). The tests inherited from the
CMake harness were written against QtTest and were ported on 2026-10-18,
with the same checks.

The OpenType tests use the fonts shipped in `TeXmacs/fonts/truetype`, so a
plain `make -C tests` runs them. `make -C tests TM_TEST_FONT_DIR=/path/to/fonts`
adds a directory, searched recursively, for the fonts which are not shipped,
Asana Math for instance; a test which needs such a font and does not find it
reports itself as skipped.
`opentype_font_test` covers the MATH constants, variants and assemblies of
the fonts it finds, the corrections and kerning hooks, and the profiles:
`test_profile_file` reads `fonts-opentype.scm` and checks every key and
group, which profile first claims a shared text companion, and that each
installed math font is an OpenType math font known to the database under the
profile's name.

The tests run with `TEXMACS_PATH` set to the source tree and a scratch
`TEXMACS_HOME_PATH` under `tests/build`, so they never touch `~/.TeXmacs_Upsilon`.

`xml_test` includes `Xml/xml.cpp`, which is also part of the main build,
for its static `is_xml_name`: it renames the public functions of that copy
with macros, so that they do not clash with those of the build.

The renders and the pixel diffs depend on the PDF writer of the build. With
the native (Hummus) PDF renderer the exported fonts are real subsets, and
without it every glyph becomes a Type 3 bitmap, which changes the
anti-aliasing of every page. Configure with

```
CPPFLAGS='-I/opt/homebrew/opt/libpng/include/libpng16 -I/opt/homebrew/include'
```

on macOS to get the native renderer ("hummus support for native pdf
exports... enabled"), and rerun `make SAFE_TEXMACS_REV` afterwards, since a
reconfigure overwrites `TeXmacs/SVNREV` with the output of `svnversion`.

Because dependency tracking may be disabled in the main build, run
`make -C tests check-stale` after changing a header and remove the listed
objects before rebuilding.

## Document regression tests

`tests/documents/check.sh` typesets the documents in
`tests/documents/samples` without a window and compares them with
references:

```
tests/documents/check.sh               # all samples
tests/documents/check.sh math tables   # some of them
tests/documents/check.sh -u            # accept the current output
tests/documents/check.sh -u -p         # ... and keep pixel references
tests/documents/check.sh -p            # compare pixels too
```

Each sample is loaded, its references and table of contents are generated
and it is typeset again twice (`tests/documents/export.scm`, the steps of
Document > Update > All, which without a window have to be forced), then
printed to PDF. `mutool` extracts the number of pages and the text of each
page, which is compared with `tests/documents/ref/<sample>.txt`. These
references are committed: a change in line breaks, page breaks, numbering,
references or glyphs shows up as a diff in `tests/build/documents`. A PDF
which `mutool` reads with a syntax error fails too. Pixel references depend
on the machine and the build, so `-p` keeps them in `tests/build/documents`
and never commits them.

The samples cover an article (title, abstract, sections, lists, footnote,
references, theorems), mathematics, tables, a drawing, program code,
languages with accented letters, and a book (table of contents, chapters,
page breaks, appendix). They use only the fonts which come with TeXmacs.
Like every `.tm` file they are Cork-encoded: accented letters are Cork
bytes, and Cyrillic letters `<#4xx>` code points, not UTF-8. The text of
the references is the text layer of the PDF, which for the TeX fonts is
translated from their Cork and T2A positions to Unicode.

When a change is intended, run `check.sh -u` and commit the new references
together with the change, so that the diff of the references documents what
moved.

## Scheme tests

`tests/scheme/check.sh` runs the Scheme test suites without a window and
exits with their status:

```
tests/scheme/check.sh                     # the regression suites, and typeset-check
tests/scheme/check.sh typeset-check       # the typeset suite in the check modes
tests/scheme/check.sh integration         # the integration suites
tests/scheme/check.sh glue tmhtml         # some suites, by name
tests/scheme/check.sh path/to/foo-test.scm  # a suite not listed yet
```

The suites are listed in `TeXmacs/progs/check/check-master.scm`: the
regression suites, which `run-all-tests` runs, and the integration suites
(server backup, cache, notifications and tmfs), which have side effects and
which `run-integration-tests` runs. Both run every suite, also after one
has failed, and return the number of failed suites; `run-regression-suite`
runs one suite by name. `typeset-check` runs the `typeset` suite again
with the caches of the typesetting in their check modes
(`TEXMACS_TYPESET_READS=check`, `TEXMACS_TYPESET_CACHES=check` and the
page switches), which make each cached result again and count the
differences: its checks of `typeset-check-mismatches` find nothing
otherwise. `integration-test-group` adds the tests which
fail to `integration-failure-total`, which is how their failures are
counted.

New suites use `TeXmacs/progs/check/check-lib.scm`: `check=`, `check-true`,
`check-false` and `check-error` run every check and report a failure with
the expression which failed, and the suite of `foo-test.scm` is a function
`foo-test-failures` which returns their number:

```scheme
(texmacs-module (check lists-test)
  (:use (check check-lib)))

(tm-define (lists-test-failures)
  (check-suite "lists")
  (check-group "sublists")
  (check= (sublist '(a b c d) 1 3) '(b c))
  (check-error (car '()) #t)
  (check-end))
```

Such a file runs with `check.sh path/to/lists-test.scm` while it is being
written, and joins the others once it is in the `:use` list and the table
of suites of `check-master.scm`.

Four suites test the Scheme library on which the rest is built:
`lists-test.scm` (`kernel/library/list.scm`, the abbreviations and macros
of `kernel/boot/abbrevs.scm`, `kernel/library/iterator.scm`), `base-test.scm`
(the strings, numbers and characters of `kernel/library/base.scm`, and the
hash tables of `kernel/boot/ahash-table.scm`), `trees-test.scm` (trees,
content and the `tm-` functions, modifications and patches, on detached
trees) and `define-test.scm` (`tm-define` and its overloading by condition
and mode, `former`, properties, modes and sub-modes, `lazy-define` and the
module macros). What needs a buffer, the cursor or the GUI is left out;
checks which fail because of a bug in the sources are left out with a
`FIXME` at their place.

`search-test.scm` tests the search engine of Edit > Search and Edit >
Replace (`src/Data/Tree/tree_search.cpp`) on detached trees: text, tags
(the examples of the manual), formulas, wildcards, select-region, patterns
of several paragraphs, the limit on the hits and the search from a
position, case-insensitive matching, each for the two ways of matching of
Edit > Search and Edit > Replace, and the time of a search in a long
formula.

Three more test the document level. `latex-test.scm` converts to and from
LaTeX (special characters, accents, structure, formulas, tables, theorems,
macros, whole documents) and checks round trips, including the ones which
lose information on purpose. `formats-test.scm` does the same for the .tm,
Scheme, TMML, HTML and plain text formats, round-trips a common table of
samples through the TeXmacs formats, and checks the format registry.
`markdown-test.scm` checks the Markdown converters step by step (the parser,
the serializer, the import and the export) and their round trips.
`office-test.scm` does so for the Word and OpenDocument converters, on small
archives which it makes from their XML, and for the zip archives.
`editing-test.scm` opens buffers and edits them through the commands a user
or a plugin uses (inserting, the cursor, selections and the clipboard,
structured editing, the environment, undo and redo, saving and exporting),
each action wrapped like a key press of the event loop so that it reaches
the undo history. Moving the cursor by characters and lines needs a window
and is left out.

Five more reach further into the system. `typeset-test.scm` checks the
typesetter as Scheme sees it: evaluation of the style language, lengths,
the environment and the numbering at paths, the extents of boxes (text,
mathematics, tables), line breaking, hyphenation, paragraphs and pages.
`bibtex-test.scm` checks the .bib parser, the BibTeX engine and its styles,
the export to .bib and the bibliography of a document. `database-test.scm`
checks the TeXmacs database (fields, history, queries, persistence and the
Scheme layer) on databases of its own in the temporary directory.
`crypto-test.scm` checks base64, tree hashes, passwords, the encrypted
blocks and documents, and GnuPG and GnuTLS, which it skips when they are
missing (it never touches `~/.gnupg`). `plugins-test.scm` checks plugin
configuration, the protocol of plugin answers, and live shell and Python
sessions when they are installed, stopping every process it starts.

`TeXmacs/progs/check/glue-test.scm` tests the glue between C++ and Scheme
(`src/Scheme/Glue`). It reads the declarations of `build-glue-*.scm` from
the source tree (1181 functions) and checks that each one is bound to a
procedure with the declared number of arguments, and that a first
argument of the wrong type is refused with `wrong-type-arg` before any C++
code runs. It then checks that values cross the glue unchanged: trees and
Scheme trees, content given as a string, a tree or a Scheme tree, strings
with every byte, integers up to the limits of a C int (and `out-of-range`
beyond), paths, urls, lists of strings, booleans and doubles. The
functions which Scheme code redefines are listed and left out.

An error in an expression given with `-x` keeps TeXmacs from quitting, so
the runner catches every error and exits itself, and stops a run after
`TM_TEST_TIMEOUT` seconds (600 by default); `TM_TEST_HOME` chooses the
scratch home directory.

## Visual regression for math typesetting

`tests/opentype/render-samples.sh` renders every document in
`tests/opentype/samples/` to PDF and PNG (one PNG per page, via mutool) in
`tests/build/vis`, with the git revision in the file name:

```
TM_TEST_FONT_DIR=/path/to/fonts tests/opentype/render-samples.sh
tests/opentype/render-samples.sh -c reference-dir   # pixel diff with ImageMagick
```

`render-samples.sh` takes `-o outdir`, `-r dpi` (150 by default) and `-c
refdir`; `TM_HAND_TUNED=off` renders with the hand-tuned customizations
switched off and adds `-notuned` to the file names, `FORCE_DB=1` rebuilds
the local font database, and `TEXMACS_HOME_PATH` defaults to
`tests/build/home`.

`tests/opentype/check.sh` runs the unit tests and both sample renders, tuned
and untuned, and is the script to run before a commit. When `tests/build/ref`
exists it diffs the tuned renders against it and prints the number of differing
pixels; refresh it on purpose, by copying the accepted renders of
`tests/build/vis` over it under their plain names (`math-overview-1.png`,
`math-showcase-3.png`, ...).

`tests/opentype/compare-lualatex.sh` typesets the formula pairs of
`tests/opentype/compare/` twice with the same OpenType math font, once
through `unicode-math` under LuaLaTeX and once through TeXmacs, and stacks
the two renders in one PNG so they can be compared line by line:

```
tests/opentype/compare-lualatex.sh            # all pairs
tests/opentype/compare-lualatex.sh radicals-bars
```

`tests/opentype/missing-symbols.py` answers a different question: which
mathematical symbols TeXmacs has no name for. It reads the symbol list of
`unicode-math` and the tables of `TeXmacs/langs/encoding` and writes
`doc/math-symbol-coverage.md`:

```
python3 tests/opentype/missing-symbols.py \
  -t /path/to/unicode-math-table.tex -o doc/math-symbol-coverage.md
```

With `--emit` it prints draft Scheme for a chosen family instead: the lines
to put in place of the comments that hold their place in the encoding table,
and a `std-symbols.scm` group built from the unicode-math class, which is
what gives a symbol its spacing.

```
python3 tests/opentype/missing-symbols.py -t /path/to/unicode-math-table.tex \
  --emit --block "Mathematical operators" --min-fonts 10
```

With `--check` it verifies the tables themselves, that no name is given two
code points and no code point two two-way names, and exits non-zero on a
failure. `check.sh` runs it when `TM_UNICODE_MATH_TABLE` points at the list.

With `--tables DIR` it writes two proposal tables into `DIR`, in the shape
of the tables of `TeXmacs/langs/encoding`: `tmuniversaltounicode-extra.scm`
with the entries that are ready, and
`tmuniversaltounicode-extra-candidates.scm` with the rest, every line
commented and carrying the reason. `confirmed-symbols.txt` next to the
script lists the names whose shape was compared by eye with the glyph of
the code point, which is what moves them from the second file to the
first. `src/Data/String/converter.cpp` names the first one in every
`hashtree_from_dictionary` chain that reads `tmuniversaltounicode`, so its
two hundred symbols are converted like any other; the candidates wait for
the same treatment: the candidates file is committed in
`TeXmacs/langs/encoding` but not loaded. (`--include-extra` was meant to count them as covered,
to see what would remain; since every entry of the candidates file is a
comment, it changes only the notes column of the report.) Since the first table is in service, the script counts
its symbols as named and would write only the few left over: give
`--tables` a scratch directory, never `TeXmacs/langs/encoding` itself,
which would replace the table in service, and merge what it proposes by
hand. `--all` lists every missing symbol, including those few fonts draw,
`--class`, with `--emit`, restricts the draft to one unicode-math class, `--texmacs` points to another tree, and font files
given as arguments replace the default set, which is every math font in
`TeXmacs/fonts/truetype`. `--check` also lists the named symbols that no
math font draws.


The sample `math-overview.tm` typesets the same formulas with TeX fonts,
the shipped TeX Gyre and STIX fonts, and several OpenType math fonts, so the
effect of a change on each code path can be compared side by side.
`math-showcase.tm` tours every MATH feature font by font, and
`math-variants.tm` shows math roman, math sans serif, math typewriter and
bold mathematics for the profiled fonts. `math-symbols-extra.tm`, generated
by `missing-symbols.py --sample`, shows the 200 symbols of
`tmuniversaltounicode-extra.scm` in tables: a name the conversion tables
fail to serve appears there as a box instead of a glyph.
`math-extensible.tm` gives a page to each of the default TeXmacs fonts and
six OpenType math fonts (Latin Modern, STIX Two, Libertinus, Fira, New
Computer Modern, TeX Gyre Pagella) with everything which stretches: over-
and underbraces (curly, round and square), wide accents over one letter,
three letters and a long formula, arrows over and under formulas, long
arrows with short and long labels, square roots up to nine rows, growing
delimiters up to twelve rows, and big operators. It shows what happens
when a font runs out of sizes: a wide check or breve beyond the largest
variant is drawn by TeXmacs, a long arrow always covers its labels, and a
slash without variants (Fira Math) keeps being stretched.

## Checks of the Cocoa interface

`tests/cocoa/check.sh`, run from the top of the source tree of a Cocoa
build (`--with-gui=cocoa`), or with `BIN=` the executable of a
`TeXmacs.app`, starts TeXmacs several times (about 15 seconds each, in
homes of their own) and checks failures which used to be silent or to
abort it (#621): a dialog title with accents in French (#468), closing the
console with several tabs (#619), an idle console, which must not be
typeset again and again (#620), the tooltips of the icon bars (#637),
dialogs with negative sizes (one warning for each place, and the dialogs
are shown) and an error thrown at an entry point from AppKit (reported
once, and TeXmacs goes on, #650, #666), a document of 7*10^6 points scrolled
to its end, the window queries of a console and a console which opens at its
end (#661). Two more cases check the focus of
the console: when it opens by itself on a warning, the document stays the key
and main window and the current buffer, and gets the keys typed; a click, or
opening it from the menu, gives it the focus; its close leaves the document
focused (#679). They make TeXmacs the active application, which takes the
keyboard from whoever works on the screen: they run on GitHub Actions or with
`TM_COCOA_FOCUS=1` (`0` turns them off), and are skipped otherwise. A case in
which TeXmacs says it is not active is skipped, except on GitHub Actions, where
it fails; a crash always fails. It also fails on any other error
caught at an entry point from AppKit, except known harmless messages (a
list in the script, extended with `TM_COCOA_HARMLESS`, an extended regular
expression). The windows are shown: Cocoa has no offscreen mode. The
workflow `upsilon-macos.yml` runs it on the application of the Cocoa disk
image.

## Other tools

`tests/opentype/font-gallery.sh` renders one specimen per math font into a
PNG, for the gallery of `doc/OPENTYPEMATH.md`: without arguments it asks
TeXmacs for the installed profiled fonts (`opentype-math-font-list`) and
puts the TeX fonts and the first STIX in front as references, and
families can be given instead, separated by `|`. Options: `-o outdir`
(default `src/opentype-math`), `-r dpi` (300) and `-w width` (1100).

`tests/opentype/compare-lualatex.sh` takes `-f font-file` and `-m family`
to compare another font than Latin Modern Math, and writes to
`tests/build/compare`; the pairs are `big-operators`, `radicals-bars` and
`scripts-fractions`. It needs LuaLaTeX, mutool and ImageMagick.

`tests/opentype/assembly-report.py [-g CHAR] font.otf ...` prints the parts
of the glyph assemblies of a math font, and
`tests/opentype/survey-math-fonts.py [-t table] font.otf ...` the
statistics of `doc/opentype-math-fonts-survey.md`. Both need fontTools.
