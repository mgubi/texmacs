#!/usr/bin/env python3
"""The variables which the typesetter sets itself in a part of a tree.

The style rewriter evaluates the style before the typesetting: a macro which
reads such a variable (the row of a cell, the math level of a script, the
colour of a link...) must find there what the typesetter finds. The rewriter
gets these variables from one table, src/src/Style/Evaluate/
evaluate_locals.cpp (decision 1.1 of docs/pure-style-decisions.md).

This finds every place of src/src/Typeset and src/src/Edit where the
typesetter sets a variable (local_begin, write, write_update,
monitored_write_update, assign, local_begin_script, local_begin_extents, the
with lists of the bridges, env (...)= ...), with the function around it,
and checks that each pair (function, variable) is in the table or in the
reviewed exceptions below; and that each pair of the table is still a place
of the typesetter.

usage: tests/style/typesetter-vars.py [-v]

  -v   also list, for each variable, the macros of the styles which read it

The exit code is 1 when a place is neither in the table nor reviewed, or
when the table names a place which no longer sets its variable: add the
place to the table (the primitive, its children, the value when it is
always the same, a reference otherwise), or give the reason here why the
rewriter is not concerned.
"""

import os
import re
import sys

here = os.path.dirname(os.path.abspath(__file__))
top = os.path.dirname(os.path.dirname(here))
src = os.path.join(top, 'src')
table_file = os.path.join(src, 'Style', 'Evaluate', 'evaluate_locals.cpp')

# (function, variable): why the rewriter is not concerned ('*': any variable)
reviewed = {
    ('table_rep::typeset', 'cell-format'):
        'the format of a table, given by the rewriter (evaluate_formatting, evaluate_table)',
    ('table_rep::typeset_subtable', 'cell-format'):
        'the format of a table, given by the rewriter (evaluate_formatting, evaluate_table)',
    ('cell_rep::typeset', 'cell-format'):
        'the format of a table, given by the rewriter (evaluate_formatting, evaluate_table)',
    ('table_rep::format_table', 'cell-format'):
        'the format of a table, given by the rewriter (evaluate_formatting, evaluate_table)',
    ('cell_rep::format_cell', 'cell-format'):
        'the format of a table, given by the rewriter (evaluate_formatting, evaluate_table)',
    ('concater_rep::typeset_blue', '*'):
        'inactive markup, which is the typesetter\'s (decision 1.5)',
    ('pager_rep::make_header', '*'):
        'the headers and footers, whose values are quoted (decision 1.3)',
    ('pager_rep::make_footer', '*'):
        'the headers and footers, whose values are quoted (decision 1.3)',
    ('pager_rep::pages_make_page', '*'):
        'the number of the page, for the headers and footers (decision 1.3)',
    ('concater_rep::typeset_main', 'preamble'):
        'the source mode, where the typesetter gets the document (decision 2.8)',
    ('bridge_auto_rep::my_typeset', 'preamble'):
        'the source mode, where the typesetter gets the document (decision 2.8)',
    ('make_lazy', 'preamble'):
        'the source mode, where the typesetter gets the document (decision 2.8)',
    ('concater_rep::typeset_main', 'par-no-first'):
        'the flag of no-indent, consumed by the next paragraph; read by no style',
    ('lazy_paragraph_rep::lazy_paragraph_rep', 'par-no-first'):
        'the flag of no-indent, consumed by the next paragraph; read by no style',
    ('lazy_paragraph_rep::format_paragraph', 'par-no-first'):
        'the flag of no-indent, consumed by the next paragraph; read by no style',
    ('lazy_paragraph_rep::handle_decoration', 'xoff-decorations'):
        'the decorations of the lines of a paragraph, after the evaluation',
    ('lazy_table_rep::produce', '*'):
        'the lines of a table broken over pages, after its cells are typeset',
    ('concater_rep::typeset_lprime', 'math-level'):
        'the argument is a string of primes',
    ('concater_rep::typeset_rprime', 'math-level'):
        'the argument is a string of primes',
    ('edit_env_rep::local_begin_extents', '*'):
        'the definition of local_begin_extents, whose callers are in the table',
    ('edit_env_rep::exec_auto_id', '*'):
        'the automatic labels (decision 2.8)',
    ('edit_env_rep::exec_use_module', 'the-modules'):
        'use-module, which the rewriter evaluates itself (evaluate_use_module)',
    ('edit_env_rep::exec', 'the-modules'):
        'use-module, which the rewriter evaluates itself (evaluate_use_module)',
    ('edit_graphics_rep::set_graphical_object', '*'):
        'the graphics editor, not the typesetting',
    ('edit_main_rep::print_doc', '*'):
        'the whole document, before the rewriting for print (style_rewrite_printed)',
    ('restricted_exec', '*'):
        'the environment of the editor, not the typesetting',
    ('edit_typeset_rep::typeset_exec_until', '*'):
        'the environment at the cursor, computed on the document (decision 2.3)',
    ('edit_env_rep::exec_table', 'cell-format'):
        'the value of a table (exec), with the format given by the rewriter',
    ('edit_env_rep::exec_until_table', 'cell-format'):
        'the environment at the cursor in a table (decision 2.3)',
    ('edit_env_rep::local_end_extents', '*'):
        'the end of local_begin_extents, which restores the lengths',
    ('edit_main_rep::nr_pages', '*'):
        'the whole document, before the rewriting for print (style_rewrite_printed)',
    ('edit_main_rep::print_snippet', '*'):
        'the whole snippet, before it is typeset',
    ('edit_typeset_rep::environment_update', '*'):
        'the environment of the whole document, which the rewriter gets too',
    ('edit_typeset_rep::style_rewrite_printed', '*'):
        'the environment of the whole document, which the rewriter gets too',
}


def variables():
    """The names of the C++ constants of the variables, from vars.cpp."""
    names = {}
    with open(os.path.join(src, 'Data', 'Drd', 'vars.cpp'), encoding='latin-1') as f:
        for m in re.finditer(r'^string (\w+) \("([^"]*)"\);', f.read(), re.M):
            names[m.group(1)] = m.group(2)
    return names


HEADER = re.compile(r'^(?:[\w<>,*&]+\s+)*(~?[A-Za-z_]\w*(?:::~?\w+)?)\s*\(')
DEFINE = re.compile(r'^#define\s+(\w+)')


def function_at(lines, i):
    """The function (or the macro of a #define) around line i."""
    j = i
    while j >= 0:
        line = lines[j]
        m = DEFINE.match(line)
        if m and all(l.rstrip().endswith('\\') for l in lines[j:i]):
            return m.group(1)
        if line and line[0] not in ' \t#{}/*' and not line.rstrip().endswith(';'):
            m = HEADER.match(line)
            if m and m.group(1) not in ('if', 'for', 'while', 'switch', 'return'):
                return m.group(1)
        j -= 1
    return '?'


SETTERS = [
    re.compile(r'\b(?:local_begin|write|write_update|monitored_write_update|assign)'
               r'\s*\(\s*([A-Z][A-Z0-9_]*|"[a-z][a-z0-9-]*")\s*,'),
    re.compile(r'\btuple\s*\(\s*([A-Z][A-Z0-9_]*)\s*,'),
    re.compile(r'\benv\s*\(\s*([A-Z][A-Z0-9_]*|"[a-z][a-z0-9-]*")\s*\)\s*=[^=]'),
]
EXTENTS = ['w-length', 'h-length', 'l-length', 'b-length', 'r-length', 't-length']


def places(names):
    """The pairs (function, variable) set by the typesetter, with a place."""
    found = {}
    skip = ('env_default.cpp', 'env_semantics.cpp', 'env.hpp')
    for d in ('Typeset', 'Edit'):
        for root, _, files in os.walk(os.path.join(src, d)):
            for fn in sorted(files):
                if not fn.endswith(('.cpp', '.hpp')) or fn in skip:
                    continue
                path = os.path.join(root, fn)
                with open(path, encoding='latin-1') as f:
                    lines = f.read().split('\n')
                for i, line in enumerate(lines):
                    code = line.split('//')[0]
                    vs = []
                    for rx in SETTERS:
                        for m in rx.finditer(code):
                            v = m.group(1)
                            v = v.strip('"') if v.startswith('"') else names.get(v)
                            if v:
                                vs.append(v)
                    if re.search(r'\blocal_begin_script\s*\(\s*\)', code):
                        vs.append('math-level')
                    if re.search(r'\blocal_begin_extents\s*\(\s*\w', code):
                        vs.extend(EXTENTS)
                    if not vs:
                        continue
                    site = function_at(lines, i)
                    for v in vs:
                        place = '%s:%d' % (os.path.relpath(path, src), i + 1)
                        found.setdefault((site, v), place)
    return found


def table():
    """The pairs (function, variable) of evaluate_locals.cpp."""
    with open(table_file, encoding='latin-1') as f:
        text = f.read()
    pairs = set()
    entry = re.compile(r'\{\s*([A-Z_]+),\s*(-?\d+),\s*(-?\d+),\s*"([^"]+)",\s*'
                       r'(?:NULL|"[^"]*"),\s*((?:"[^"]*"\s*)+)')
    for m in entry.finditer(text):
        sites = ''.join(re.findall(r'"([^"]*)"', m.group(5))).split()
        for s in sites:
            pairs.add((s, m.group(4)))
    return pairs


def readers(var):
    """The macros of the styles which read the variable."""
    pat = '<value|%s>' % var
    out = []
    for r in ('packages', 'styles'):
        base = os.path.join(top, 'TeXmacs', r)
        for root, _, files in os.walk(base):
            for fn in sorted(files):
                if not fn.endswith('.ts'):
                    continue
                path = os.path.join(root, fn)
                with open(path, encoding='latin-1') as f:
                    s = f.read()
                at = s.find(pat)
                while at >= 0:
                    a = s.rfind('<assign|', 0, at)
                    name = s[a + 8:s.find('|', a + 8)] if a >= 0 else '?'
                    out.append('%s: %s' % (os.path.relpath(path, os.path.join(top, 'TeXmacs')), name))
                    at = s.find(pat, at + 1)
    return sorted(set(out))


def main():
    verbose = '-v' in sys.argv[1:]
    found = places(variables())
    tab = table()
    status = 0
    for (site, var), place in sorted(found.items()):
        if (site, var) in tab:
            continue
        why = reviewed.get((site, var)) or reviewed.get((site, '*'))
        if why:
            if verbose:
                print('reviewed  %s, %s: %s' % (site, var, why))
            continue
        print('NEW       %s sets %s (%s)' % (site, var, place))
        status = 1
    for site, var in sorted(tab - set(found)):
        print('STALE     the table names %s for %s, which no longer sets it' % (site, var))
        status = 1
    if verbose:
        for var in sorted({v for _, v in tab}):
            for r in readers(var):
                print('read      %s by %s' % (var, r))
    n = len([p for p in found if p in tab])
    m = len([p for p in found if p not in tab and
             (p in reviewed or (p[0], '*') in reviewed)])
    print('%d places of the typesetter: %d in the table, %d reviewed%s' %
          (len(found), n, m, '' if status == 0 else '; FAILED'))
    return status


if __name__ == '__main__':
    sys.exit(main())
