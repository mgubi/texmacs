#!/usr/bin/env python3
###############################################################################
# MODULE     : gen-scheme-indents.py
# DESCRIPTION: the query of indentation of Scheme, from the keywords of TeXmacs
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# usage: gen-scheme-indents.py   (from anywhere in the tree)
#
# Writes TeXmacs/misc/tree-sitter/scheme/indents.scm: every list and vector
# is a node of Lisp indentation (@indent.lisp), and the lists whose first
# element is a keyword of the lists nullary-indent, unary-indent,
# binary-indent and ternary-indent of TeXmacs/progs/tm-mode.el (those which
# utils/misc/tm-keywords.scm reads) get its arity (indent.arity). The rule
# itself is in syntax_region_indent (Plugins/TreeSitter/syntax_region.cpp).
# Run it again when tm-mode.el changes.

import os
import re

here = os.path.dirname(os.path.abspath(__file__))
top = os.path.normpath(os.path.join(here, "..", ".."))
source = os.path.join(top, "TeXmacs", "progs", "tm-mode.el")
target = os.path.join(top, "TeXmacs", "misc", "tree-sitter", "scheme", "indents.scm")

def strip_comments(s):
    return "\n".join(line.split(";", 1)[0] for line in s.split("\n"))

def setqs(s):
    # name -> list of symbols, or ("append", names...) or ("alias", name)
    r = {}
    for m in re.finditer(r"\(setq\s+([\w-]+)\s+", s):
        name, i = m.group(1), m.end()
        depth, j = 0, i
        while True:
            c = s[j]
            if c == "(": depth += 1
            elif c == ")":
                if depth == 0: break
                depth -= 1
            j += 1
        body = s[i:j].strip()
        if body.startswith("'("):
            r[name] = body[2:-1].split()
        elif body.startswith("(append"):
            r[name] = ("append",) + tuple(body[len("(append"):-1].split())
        else:
            r[name] = ("alias", body)
    return r

def resolve(table, name):
    v = table[name]
    if isinstance(v, list): return v
    if v[0] == "alias": return resolve(table, v[1])
    return [x for n in v[1:] for x in resolve(table, n)]

def main():
    table = setqs(strip_comments(open(source).read()))
    out = ["; The indentation of Scheme as utils/misc/tm-keywords.scm and\n"
           "; prog/scheme-edit.scm compute it, for syntax_region_indent\n"
           "; (Plugins/TreeSitter/syntax_region.cpp): every list and vector is a\n"
           "; node of Lisp indentation, and the lists whose first element is a\n"
           "; keyword get its arity. Generated from progs/tm-mode.el by\n"
           "; misc/tree-sitter/gen-scheme-indents.py: do not edit.\n\n"
           "(list) @indent.lisp\n"
           "(vector) @indent.lisp\n"]
    for arity, name in enumerate(["nullary-indent", "unary-indent",
                                  "binary-indent", "ternary-indent"]):
        words = resolve(table, name)
        lines, cur = [], " "
        for w in words:
            item = ' "%s"' % w
            if len(cur) + len(item) > 76:
                lines.append(cur)
                cur = " "
            cur += item
        lines.append(cur)
        out.append("\n((list . (symbol) @_op) @indent.lisp\n"
                   " (#any-of? @_op\n  %s)\n"
                   ' (#set! indent.arity "%d"))\n'
                   % ("\n  ".join(l.strip() for l in lines), arity))
    open(target, "w").write("".join(out))
    print(target)

main()
