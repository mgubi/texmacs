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
#   pack-fs.py <TeXmacs dir> <output prefix>
#
# Writes <output prefix>.bin, the concatenation of the files, and
# <output prefix>.json, their index ([path, offset, size], the paths as the
# program sees them, under /texmacs). The runners of the SDK (sdk-run.mjs,
# the page) give them to the package of TeXmacs as its files.
#
# The whole tree of TEXMACS_PATH is shipped, so that the program finds what
# it finds on the desktop, but the binaries of the plugins (bin/), which
# are no programs of WASIX

import json, os, sys

EXCLUDE_DIRS = ['bin']
EXCLUDE_FILES = ['.DS_Store']

def main ():
    tm, prefix = sys.argv[1:3]
    names = []
    for root, dirs, files in os.walk (tm):
        dirs[:] = sorted (d for d in dirs if d not in EXCLUDE_DIRS)
        for f in sorted (files):
            if f not in EXCLUDE_FILES:
                names.append (os.path.relpath (os.path.join (root, f), tm))
    index, offset = [], 0
    with open (prefix + '.bin', 'wb') as out:
        for rel in names:
            with open (os.path.join (tm, rel), 'rb') as f:
                data = f.read ()
            out.write (data)
            index.append (['/texmacs/' + rel, offset, len (data)])
            offset += len (data)
    with open (prefix + '.json', 'w') as out:
        json.dump (index, out, separators= (',', ':'))
    print ('%s: %d files, %.1f MB' % (prefix, len (index), offset / 1e6))

main ()
