#!/usr/bin/env python3
###############################################################################
# MODULE     : pack-fs.py
# DESCRIPTION: The files of TeXmacs for the WASIX build in the Wasmer SDK
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
#   pack-fs.py <TeXmacs dir> <boot list> <output prefix>
#
# Writes <output prefix>.bin, the concatenation of the files, and
# <output prefix>.json, their index ([path, offset, size], the paths as the
# program sees them, under /texmacs). The runners of the SDK (sdk-run.mjs,
# the page) give them to the package of TeXmacs as its files.
#
# The files are those which TeXmacs opens when it starts and typesets a
# document: the boot list of the Emscripten build (misc/wasm/boot-files.txt,
# its files only) and the small groups which it reads at unforeseeable
# times, as the boot package of the Emscripten build (misc/wasm/package.py),
# and the Type 1 fonts (13 MB: the TeX fonts of the formulas; the TrueType
# and OpenType fonts, 75 MB, only as far as the boot list has them)

import json, os, sys

GROUPS = ['progs/', 'styles/', 'packages/', 'texts/', 'plugins/', 'langs/encoding/',
          'fonts/tfm/', 'fonts/enc/', 'fonts/virtual/', 'fonts/type1/',
          'misc/pixmaps/neoclassical/light/']
FILES = ['SVNREV', 'fonts/font-database.scm', 'fonts/font-characteristics.scm',
         'fonts/font-features.scm', 'fonts/font-substitutions.scm',
         'fonts/pdf-font-issues.scm', 'misc/pixmaps/light/TeXmacs.svg']
# (as misc/wasm/package.py: the programs and the documentation of the plugins)
EXCLUDE_DIRS = ['bin', 'doc']

def main ():
    tm, boot, prefix = sys.argv[1:4]
    names = set (FILES)
    for g in GROUPS:
        for root, dirs, files in os.walk (os.path.join (tm, g)):
            if g == 'plugins/':
                dirs[:] = [d for d in dirs if d not in EXCLUDE_DIRS]
            for f in files:
                if f != '.DS_Store':
                    names.add (os.path.relpath (os.path.join (root, f), tm))
    with open (boot) as b:
        for line in b:
            p = line.strip ()
            if p.startswith ('/texmacs/'):
                rel = p[len ('/texmacs/'):]
                if os.path.isfile (os.path.join (tm, rel)):
                    names.add (rel)
    index, offset = [], 0
    with open (prefix + '.bin', 'wb') as out:
        for rel in sorted (names):
            with open (os.path.join (tm, rel), 'rb') as f:
                data = f.read ()
            out.write (data)
            index.append (['/texmacs/' + rel, offset, len (data)])
            offset += len (data)
    with open (prefix + '.json', 'w') as out:
        json.dump (index, out, separators= (',', ':'))
    print ('%s: %d files, %.1f MB' % (prefix, len (index), offset / 1e6))

main ()
