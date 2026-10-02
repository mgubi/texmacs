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
bytes, and Cyrillic letters `<#4xx>` code points, not UTF-8. The text of
the references is the text layer of the PDF, which for the TeX fonts is
translated from their Cork and T2A positions to Unicode.

When a change is intended, run `check.sh -u` and commit the new references
together with the change, so that the diff of the references documents what
moved.
