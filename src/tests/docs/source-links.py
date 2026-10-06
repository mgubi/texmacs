#!/usr/bin/env python3
# Check the links from the developer documentation to the source files.
#
# usage: tests/docs/source-links.py [--convert] [dir...]
#
# The documentation refers to source files with
#   <source-link|shown text|path[:line]>
# where path is relative to the src directory of the repository (see
# TeXmacs/progs/doc/source-links.scm).  Without options, every .tm file under
# the given directories (default: TeXmacs/doc/devel) is checked:
#   BROKEN   a source-link whose file is not in the repository
#   CANDIDATE  a <verbatim|...> which names exactly one source file and could
#            be a source-link
# The exit status is 1 if there are broken links.  With --convert, the
# candidates are rewritten into source-links.
#
# A name is resolved by its longest matching suffix among the files of
# git ls-files; a leading src/ (a path from the root of the repository) is
# dropped.  Ties prefer src/ (the C++ kernel), then TeXmacs/progs, ..., and
# Plugins/Qt over its Qt6 fork.  Ambiguous and unknown names are left alone.
# The .tm files are Cork-encoded: they are read and written as latin-1.

import collections, os, re, subprocess, sys

here = os.path.dirname(os.path.abspath(__file__))
src = os.path.normpath(os.path.join(here, "..", ".."))     # TEXMACS_SOURCE_PATH

EXT = r"(?:cpp|hpp|cc|c|h|mm|m|scm|ts|py|js|cmake|sh|in|txt|ac|bat|ps1)"
VERBATIM = re.compile(r"<verbatim\|([A-Za-z0-9_./+-]+\." + EXT + r")(?::(\d+))?>")
LINK = re.compile(r"<source-link\|([^|<>]*)\|([^|<>]*)>")
PREFERRED = ["src/", "TeXmacs/progs/", "TeXmacs/packages/",
             "TeXmacs/styles/", "plugins/"]

def source_files():
    out = subprocess.run(["git", "-C", src, "ls-files", "--cached",
                          "--others", "--exclude-standard", "."],
                         capture_output=True, text=True, check=True).stdout
    return set(out.split())

def suffix_index(files):
    index = collections.defaultdict(list)
    for f in files:
        parts = f.split("/")
        for i in range(len(parts)):
            index["/".join(parts[i:])].append(f)
    return index

def resolve(index, name):
    if name.startswith("./"): name = name[2:]
    c = index.get(name, [])
    if not c and name.startswith("src/"): c = index.get(name[4:], [])
    if len(c) > 1:
        c = [f for f in c if not f.startswith("src/Plugins/Qt6/")] or c
    if len(c) > 1:
        for p in PREFERRED:
            cc = [f for f in c if f.startswith(p)]
            if cc:
                c = cc
                break
    return c[0] if len(c) == 1 else None

def main(argv):
    convert = "--convert" in argv
    dirs = [a for a in argv if not a.startswith("--")] or \
           [os.path.join(src, "TeXmacs", "doc", "devel")]
    files = source_files()
    index = suffix_index(files)
    broken = candidates = 0
    for d in dirs:
        for root, _, names in os.walk(d):
            for n in sorted(names):
                if not n.endswith(".tm"): continue
                p = os.path.join(root, n)
                rel = os.path.relpath(p, src)
                with open(p, encoding="latin-1") as f: s = f.read()
                for m in LINK.finditer(s):
                    path = re.sub(r":\d+$", "", m.group(2))
                    if path not in files:
                        broken += 1
                        print("BROKEN\t%s\t%s" % (rel, m.group(2)))
                def sub(m):
                    nonlocal candidates
                    name, line = m.group(1), m.group(2)
                    target = resolve(index, name)
                    if not target: return m.group(0)
                    candidates += 1
                    suffix = ":" + line if line else ""
                    if not convert:
                        print("CANDIDATE\t%s\t%s\t%s" % (rel, name, target))
                    return "<source-link|%s%s|%s%s>" % (name, suffix,
                                                        target, suffix)
                t = VERBATIM.sub(sub, s)
                if convert and t != s:
                    with open(p, "w", encoding="latin-1") as f: f.write(t)
    print("%d broken, %d %s" % (broken, candidates,
                                "converted" if convert else "candidates"),
          file=sys.stderr)
    return 1 if broken else 0

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
