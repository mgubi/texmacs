#!/usr/bin/env python3
###############################################################################
# MODULE     : highlights.py
# DESCRIPTION: compare the highlight captures of TeXmacs and tree-sitter
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# usage: highlights.py LANGUAGE GRAMMAR-DIR FILE...
#
# The captures of the highlight query of LANGUAGE (TeXmacs/misc/tree-sitter/
# LANGUAGE/highlights.scm, after the queries it inherits) in each FILE, as
# the C runtime in TeXmacs finds them, are compared, in order, with those of
# `tree-sitter query -c -p GRAMMAR-DIR` (the repository of the grammar, which
# the command compiles with its scanner). TeXmacs runs as in corpus.py.

import os, re, subprocess, sys, tempfile

here = os.path.dirname(os.path.abspath(__file__))
texmacs = os.path.join(os.path.dirname(os.path.dirname(here)), "TeXmacs")

def query_source(lan):
    s = open(os.path.join(texmacs, "misc", "tree-sitter", lan, "highlights.scm")).read()
    first = s.split("\n", 1)[0]
    pre = "; inherits: "
    if not first.startswith(pre): return s
    return "".join(query_source(x) + "\n" for x in first[len(pre):].split(",")) + s

def ours(lan, files, work):
    driver = os.path.join(work, "run.scm")
    open(driver, "w").write("""
(catch #t
  (lambda ()
    (use-modules (treesit ts-registry) (treesit ts-highlight))
    (unless (url-exists? (treesit-cache-url "%(lan)s"))
      (treesit-generate-cache "%(lan)s"))
    (let* ((id (treesit-language "%(lan)s"))
           (q (tree-sitter-query-new id (treesit-query-source "%(lan)s")))
           (names (tree-sitter-query-capture-names q)))
      (do ((i 0 (+ i 1))) ((= i %(n)d))
        (let* ((f (lambda (ext) (system->url (string-append "%(work)s/"
                                                          (number->string i) ext))))
               (r (tree-sitter-query-captures q (string-load (f ".in")))))
          (with p (open-output-file (url->system (f ".out")))
            (let loop ((l r))
              (when (pair? l)
                (display (string-append (number->string (car l)) " "
                                        (number->string (cadr l)) " "
                                        (list-ref names (caddr l)) "\\n")
                         p)
                (loop (cdddr l))))
            (close-output-port p))))))
  (lambda args (display* "highlights error: " args "\\n")))
(exit 0)
""" % {"lan": lan, "n": len(files), "work": work})
    for i, f in enumerate(files):
        open(os.path.join(work, "%d.in" % i), "wb").write(open(f, "rb").read())
    env = dict(os.environ)
    env["TEXMACS_PATH"] = texmacs
    env["TEXMACS_HOME_PATH"] = os.environ.get("TM_TEST_HOME", os.path.join(work, "home"))
    os.makedirs(env["TEXMACS_HOME_PATH"], exist_ok=True)
    out = subprocess.run([os.path.join(texmacs, "bin", "texmacs.bin"), "-headless",
                          "-x", '(load "%s")' % driver],
                         env=env, capture_output=True, text=True, timeout=1800)
    for l in (out.stdout + out.stderr).splitlines():
        if "highlights error" in l: print(l)
    r = []
    for i in range(len(files)):
        p = os.path.join(work, "%d.out" % i)
        r.append([tuple(l.split()) for l in open(p)] if os.path.exists(p) else None)
    return r

cap_re = re.compile(r"capture: \d+ - ([\w.]+), start: \((\d+), (\d+)\), end: \((\d+), (\d+)\)")

def theirs(grammar, query, path):
    data = open(path, "rb").read()
    starts = [0] + [i + 1 for i, b in enumerate(data) if b == 10]
    out = subprocess.run(["tree-sitter", "query", "-c", "-p", grammar, query, path],
                         capture_output=True, text=True)
    r = []
    for l in out.stdout.splitlines():
        m = cap_re.search(l)
        if m:
            r0, c0, r1, c1 = map(int, m.groups()[1:])
            r.append((str(starts[r0] + c0), str(starts[r1] + c1), m.group(1)))
    return r

def main():
    if len(sys.argv) < 4:
        print("usage: highlights.py LANGUAGE GRAMMAR-DIR FILE...")
        sys.exit(2)
    lan, grammar, files = sys.argv[1], sys.argv[2], sys.argv[3:]
    work = tempfile.mkdtemp(prefix="highlights-")
    query = os.path.join(work, "query.scm")
    open(query, "w").write(query_source(lan))
    mine = ours(lan, files, work)
    bad = 0
    for f, m in zip(files, mine):
        t = theirs(grammar, query, f)
        if m == t:
            print("%s: %d captures, equal" % (f, len(t)))
            continue
        bad += 1
        if m is None:
            print("%s: no output from TeXmacs" % f)
            continue
        k = next((i for i, (x, y) in enumerate(zip(m, t)) if x != y), min(len(m), len(t)))
        print("%s: DIFFERENT (%d against %d captures), first at %d: ours %s, theirs %s"
              % (f, len(m), len(t), k, m[k] if k < len(m) else "-", t[k] if k < len(t) else "-"))
    sys.exit(1 if bad else 0)

main()
