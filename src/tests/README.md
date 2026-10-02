# Unit Tests

## Guide to Run Unit Tests for scheme
```
TeXmacs -x "(run-all-tests)" -q
```
or launch a Scheme session and then run `(run-all-tests)`.

## Guide to Run Unit Tests for cpp

First, compile the whole project.
```
cd texmacs/
mkdir build/ && cd build/
cmake ..
make -j8
```

Then, run your unit tests:
```
ctest // run all
ctest -R analyze // run unit tests with name containing `analyze`
```

### Advanced Topic
You may also run the unit tests via the binaries under `${cmake_build_dir}/tests/`
``` bash
tests/converter_test
```

However, this specify unit test will fail. For `utf8_to_cork`, we need to set
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

The tests inherited from the CMake harness are written against QtTest, which
is what `tests/CMakeLists.txt` expects of them. New tests are written in
ordinary TeXmacs C++ over `tests/tm_test.hpp`, which brings `CHECK`,
`CHECK_MSG`, `CHECK_EQ` and `SKIP`, and a `main` which lists its tests with
`RUN` and returns `test_report ()`: a test then needs no QtTest idiom (the
Makefile still links QtTest into every binary, with the macOS
`-framework QtTest`). `tests/Makefile` builds both kinds, and runs `moc` only
on the sources which declare a `Q_OBJECT`.

The tests run with `TEXMACS_PATH` set to the source tree and a scratch
`TEXMACS_HOME_PATH` under `tests/build`, so they never touch `~/.TeXmacs`.

Two test sources are left out of this harness: `xml_test`, which includes a
source file that is already part of the main build, and `mac_images_test`,
whose functions `mac_images.h` does not declare in a Qt 6 build.

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
bytes, not UTF-8. The references record the current output, not the ideal
one: the text layer of the PDF currently maps the Cork glyphs of oe, sharp
s and the Spanish inverted marks to the wrong characters, and the
reference of `languages` holds those characters until the writer is fixed.

When a change is intended, run `check.sh -u` and commit the new references
together with the change, so that the diff of the references documents what
moved.

## Scheme tests

`tests/scheme/check.sh` runs the Scheme test suites without a window and
exits with their status:

```
tests/scheme/check.sh                     # the regression suites
tests/scheme/check.sh integration         # the integration suites
tests/scheme/check.sh glue tmhtml         # some suites, by name
tests/scheme/check.sh path/to/foo-test.scm  # a suite not listed yet
```

The suites are listed in `TeXmacs/progs/check/check-master.scm`: the
regression suites, which `run-all-tests` runs, and the integration suites
(server backup, cache, notifications and tmfs), which have side effects and
which `run-integration-tests` runs. Both run every suite, also after one
has failed, and return the number of failed suites; `run-regression-suite`
runs one suite by name. `integration-test-group` adds the tests which
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

Three more test the document level. `latex-test.scm` converts to and from
LaTeX (special characters, accents, structure, formulas, tables, theorems,
macros, whole documents) and checks round trips, including the ones which
lose information on purpose. `formats-test.scm` does the same for the .tm,
Scheme, TMML, HTML and plain text formats, round-trips a common table of
samples through the TeXmacs formats, and checks the format registry.
`editing-test.scm` opens buffers and edits them through the commands a user
or a plugin uses (inserting, the cursor, selections and the clipboard,
structured editing, the environment, undo and redo, saving and exporting),
each action wrapped like a key press of the event loop so that it reaches
the undo history. Moving the cursor by characters and lines needs a window
and is left out.

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
