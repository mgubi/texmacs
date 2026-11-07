#!/usr/bin/env python3
###############################################################################
# MODULE     : check-grammars.py
# DESCRIPTION: check the grammars of tree-sitter against their repositories
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# usage: check-grammars.py REPOS [LANGUAGE...] [--update LANGUAGE]
#
# REPOS is a directory with clones of the repositories of the grammars
# (tree-sitter-c, tree-sitter-ocaml...), named as in the first line of
# TeXmacs/misc/tree-sitter/<language>/SOURCE. For each language (all by
# default), it reports:
#   - the commit of the clone and that of SOURCE;
#   - whether the grammar.json of the clone is ours (some are patched: see
#     SOURCE);
#   - the corpus tests of the clone, run in TeXmacs (corpus.py);
#   - the highlight captures of the files of test/highlight of the clone,
#     compared with those of `tree-sitter query` (highlights.py), when the
#     clone has a src/parser.c (tree-sitter compiles it).
# The corpus and the captures use the grammar of TeXmacs, not that of the
# clone: after an update, they say whether it still works.
#
# --update LANGUAGE copies the grammar.json of the clone into TeXmacs and
# writes its commit into SOURCE; the queries are not copied (some are ours
# or patched). Then: ship-tables.sh LANGUAGE (the shipped tables), the
# checks of this script, and the treesit suite.

import os
import re
import subprocess
import sys

here = os.path.dirname(os.path.abspath(__file__))
top = os.path.normpath(os.path.join(here, "..", ".."))
base = os.path.join(top, "TeXmacs", "misc", "tree-sitter")

def source_info(lan):
    # (path of grammar.json in the repository, repository name, commit)
    text = open(os.path.join(base, lan, "SOURCE")).read()
    g = re.search(r"(\S*grammar\.json)", text)
    r = re.search(r"https://github\.com/[^/\s]+/([^/\s]+?)(?:\.git)?[\s,)]", text)
    c = re.search(r"\(commit ([0-9a-f]+)\)", text)
    if not (g and r and c): return None
    return g.group(1), r.group(1), c.group(1)

def git_head(d):
    try:
        return subprocess.check_output(["git", "-C", d, "rev-parse", "HEAD"],
                                       stderr=subprocess.DEVNULL).decode().strip()
    except Exception:
        return None

def run(cmd):
    p = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    lines = [l for l in p.stdout.decode(errors="replace").split("\n") if l.strip()]
    return lines[-1] if lines else "(no output)"

def check(repos, lan):
    info = source_info(lan)
    if not info:
        print("%-12s no repository in SOURCE" % lan)
        return
    path, repo, commit = info
    clone = os.path.join(repos, repo)
    if not os.path.isdir(clone):
        print("%-12s no clone %s" % (lan, clone))
        return
    sub = os.path.dirname(os.path.dirname(path))   # grammars/ocaml for grammars/ocaml/src/
    gdir = os.path.join(clone, sub) if sub else clone
    head = git_head(clone)
    ours = open(os.path.join(base, lan, "grammar.json")).read()
    theirs_path = os.path.join(clone, path)
    theirs = open(theirs_path).read() if os.path.exists(theirs_path) else None
    print("%-12s clone %s, SOURCE %s, grammar %s" %
          (lan, (head or "?")[:10], commit[:10],
           "same" if theirs == ours else ("missing" if theirs is None else "differs")))
    corpus = os.path.join(gdir, "test", "corpus")
    if not os.path.isdir(corpus): corpus = os.path.join(clone, "test", "corpus")
    if os.path.isdir(corpus):
        print("             corpus: " + run([sys.executable, os.path.join(here, "corpus.py"),
                                              lan, corpus]))
    hl = os.path.join(gdir, "test", "highlight")
    if not os.path.isdir(hl): hl = os.path.join(clone, "test", "highlight")
    if os.path.isdir(hl) and os.path.exists(os.path.join(gdir, "src", "parser.c")):
        files = sorted(os.path.join(hl, f) for f in os.listdir(hl))
        out = subprocess.run([sys.executable, os.path.join(here, "highlights.py"),
                              lan, gdir] + files,
                             stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        lines = [l for l in out.stdout.decode(errors="replace").split("\n") if ": " in l]
        bad = [l for l in lines if "equal" not in l]
        print("             highlights: %d files, %d different" % (len(lines), len(bad)))
        for l in bad: print("               " + l)

def update(repos, lan):
    path, repo, commit = source_info(lan)
    clone = os.path.join(repos, repo)
    head = git_head(clone)
    src = os.path.join(clone, path)
    open(os.path.join(base, lan, "grammar.json"), "w").write(open(src).read())
    s = os.path.join(base, lan, "SOURCE")
    text = open(s).read().replace("(commit %s)" % commit, "(commit %s)" % head, 1)
    open(s, "w").write(text)
    print("%s: grammar.json of %s, commit %s; now run ship-tables.sh %s and the checks"
          % (lan, repo, head, lan))

def main():
    args = sys.argv[1:]
    if not args: sys.exit(__doc__ or "usage: check-grammars.py REPOS [LANGUAGE...] [--update LANGUAGE]")
    repos = os.path.abspath(args[0])
    if len(args) == 3 and args[1] == "--update":
        update(repos, args[2])
        return
    lans = args[1:] or sorted(d for d in os.listdir(base)
                              if os.path.exists(os.path.join(base, d, "grammar.json")))
    for lan in lans: check(repos, lan)

main()
