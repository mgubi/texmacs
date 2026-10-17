#!/usr/bin/env python3
###############################################################################
# MODULE     : corpus.py
# DESCRIPTION: run the corpus tests of a tree-sitter grammar in TeXmacs
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# usage: corpus.py LANGUAGE CORPUS... [-v]
#
# LANGUAGE is a language of progs/treesit/ts-registry.scm (its grammar in
# TeXmacs/misc/tree-sitter, its scanner if any), CORPUS the files or
# directories of test/corpus of the grammar's repository. The inputs are
# parsed by the C runtime in one TeXmacs without window (TeXmacs/bin/
# texmacs.bin of this tree, a fresh TEXMACS_HOME_PATH unless TM_TEST_HOME is
# set) and the trees are compared as `tree-sitter test` does: the field
# names only when the expected tree has some, and for the tests marked
# :error only whether the tree has an error.

import os, re, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
texmacs = os.path.join(os.path.dirname(os.path.dirname(here)), "TeXmacs")

def corpus_files(paths):
    for p in paths:
        if os.path.isdir(p):
            for f in sorted(os.listdir(p)):
                if f.endswith(".txt"): yield os.path.join(p, f)
        else:
            yield p

header_re = re.compile(rb"^(={3,})([^=\r\n]*)\r?\n", re.M)

def parse_corpus(path):
    # list of (name, attributes, input, expected)
    data = open(path, "rb").read()
    heads = list(header_re.finditer(data))
    tests = []
    k = 0
    while k + 1 < len(heads):
        h1, h2 = heads[k], heads[k + 1]
        if h1.group(2) != h2.group(2):
            k += 1
            continue
        lines = data[h1.end():h2.start()].decode("utf-8").splitlines()
        name = " ".join(l.strip() for l in lines if l.strip() and not l.startswith(":"))
        attrs = [l.strip() for l in lines if l.startswith(":")]
        end = heads[k + 2].start() if k + 2 < len(heads) else len(data)
        body = data[h2.end():end]
        suffix = re.escape(h1.group(2))
        dividers = list(re.finditer(rb"^-{3,}" + suffix + rb"\r?\n", body, re.M))
        if not dividers:
            k += 2
            continue
        d = dividers[-1]
        text = body[:d.start()]
        if text.endswith(b"\n"): text = text[:-1]
        if text.endswith(b"\r"): text = text[:-1]
        tests.append((name, attrs, text, body[d.end():].decode("utf-8")))
        k += 2
    return tests

def normalize(s):
    # the comment lines, as tree-sitter test removes them
    s = re.sub(r"(?m)^\s*;.*$", "", s)
    s = re.sub(r"\s+", " ", s).strip()
    s = re.sub(r"\( ", "(", s)
    s = re.sub(r" \)", ")", s)
    return s

def strip_fields(s):
    return re.sub(r"[\w-]+: ", "", s)

def run(lan, tests, work):
    for i, t in enumerate(tests):
        open(os.path.join(work, "%d.in" % i), "wb").write(t[2])
    driver = os.path.join(work, "run.scm")
    open(driver, "w").write("""
(catch #t
  (lambda ()
    (use-modules (treesit ts-registry))
    (unless (url-exists? (treesit-cache-url "%(lan)s"))
      (treesit-generate-cache "%(lan)s"))
    (let ((id (treesit-language "%(lan)s")))
      (do ((i 0 (+ i 1))) ((= i %(n)d))
        (let ((f (lambda (ext) (system->url (string-append "%(work)s/"
                                                         (number->string i) ext)))))
          (string-save (tree-sitter-parse-sexp id (string-load (f ".in"))) (f ".out"))))))
  (lambda args (display* "corpus error: " args "\\n")))
(exit 0)
""" % {"lan": lan, "n": len(tests), "work": work})
    env = dict(os.environ)
    env["TEXMACS_PATH"] = texmacs
    env["TEXMACS_HOME_PATH"] = os.environ.get("TM_TEST_HOME", os.path.join(work, "home"))
    os.makedirs(env["TEXMACS_HOME_PATH"], exist_ok=True)
    out = subprocess.run([os.path.join(texmacs, "bin", "texmacs.bin"), "-headless",
                          "-x", '(load "%s")' % driver],
                         env=env, capture_output=True, text=True, timeout=1800)
    for l in (out.stdout + out.stderr).splitlines():
        if "corpus error" in l: print(l)

def main():
    args = [a for a in sys.argv[1:] if a != "-v"]
    verbose = "-v" in sys.argv
    if len(args) < 2:
        print(__doc__ or "usage: corpus.py LANGUAGE CORPUS... [-v]")
        sys.exit(2)
    lan = args[0]
    tests = []
    for f in corpus_files(args[1:]):
        for t in parse_corpus(f):
            if not any(a.startswith(":skip") for a in t[1]):
                tests.append((os.path.basename(f),) + t)
    work = tempfile.mkdtemp(prefix="corpus-")
    run(lan, [t[1:] for t in tests], work)
    failed = 0
    for i, (f, name, attrs, text, expected) in enumerate(tests):
        try:
            actual = open(os.path.join(work, "%d.out" % i), encoding="utf-8").read()
        except OSError:
            actual = "(no output)"
        if any(a.startswith(":error") for a in attrs):
            ok = "ERROR" in actual or "MISSING" in actual
        else:
            exp = normalize(expected)
            act = normalize(actual)
            if not re.search(r"[\w-]+: ", exp): act = strip_fields(act)
            ok = exp == act
        if not ok:
            failed += 1
            print("FAIL %s: %s" % (f, name))
            if verbose:
                print("  expected: " + normalize(expected))
                print("  actual:   " + normalize(actual))
    print("%s: %d tests, %d failed" % (lan, len(tests), failed))
    sys.exit(1 if failed else 0)

main()
