#!/usr/bin/env python3
###############################################################################
# MODULE     : gen-closers.py
# DESCRIPTION: the keywords which close the blocks of each grammar
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# usage: gen-closers.py   (from anywhere in the tree)
#
# Writes TeXmacs/misc/tree-sitter/<grammar>/closers.scm for each grammar
# which has some: the list of the keywords which close a block (end, fi,
# done...), the last member of a sequence of the grammar whose first member
# is a keyword too, with more than tokens between them. The editor tries
# them when Return opens a block (progs/treesit/ts-edit.scm). Run it again
# when a grammar changes.

import json
import os
import re

here = os.path.dirname(os.path.abspath(__file__))
top = os.path.normpath(os.path.join(here, "..", ".."))
base = os.path.join(top, "TeXmacs", "misc", "tree-sitter")

keyword = re.compile(r"^[A-Za-z_][A-Za-z_]*$")

def first_string(x):
    # the keyword which a rule starts with, if any
    if not isinstance(x, dict): return None
    t = x.get("type")
    if t == "STRING": return x["value"]
    if t in ("SEQ",) and x["members"]: return first_string(x["members"][0])
    if t in ("PREC", "PREC_LEFT", "PREC_RIGHT", "PREC_DYNAMIC", "FIELD",
             "ALIAS", "TOKEN", "IMMEDIATE_TOKEN"):
        return first_string(x["content"])
    return None

def last_string(x):
    if not isinstance(x, dict): return None
    t = x.get("type")
    if t == "STRING": return x["value"]
    if t == "SEQ" and x["members"]: return last_string(x["members"][-1])
    if t in ("PREC", "PREC_LEFT", "PREC_RIGHT", "PREC_DYNAMIC", "FIELD",
             "ALIAS", "TOKEN", "IMMEDIATE_TOKEN"):
        return last_string(x["content"])
    return None

def closers(rules):
    found = []
    def walk(x):
        if isinstance(x, dict):
            if x.get("type") == "SEQ" and len(x["members"]) >= 3:
                a, b = first_string(x["members"][0]), last_string(x["members"][-1])
                # something which is not a mere token between them (not
                # new.target)
                inner = any(m.get("type") != "STRING" for m in x["members"][1:-1])
                if (a and b and a != b and keyword.match(a) and keyword.match(b)
                        and inner and b not in found):
                    found.append(b)
            for v in x.values(): walk(v)
        elif isinstance(x, list):
            for v in x: walk(v)
    walk(rules)
    return found

def main():
    for lan in sorted(os.listdir(base)):
        g = os.path.join(base, lan, "grammar.json")
        if not os.path.exists(g): continue
        found = closers(json.load(open(g))["rules"])
        out = os.path.join(base, lan, "closers.scm")
        if found:
            open(out, "w").write(
                ";; The keywords which close the blocks of the grammar, made by\n"
                ";; misc/tree-sitter/gen-closers.py from grammar.json: do not edit.\n"
                "(%s)\n" % " ".join('"%s"' % c for c in found))
            print(lan, found)
        elif os.path.exists(out):
            os.remove(out)

main()
