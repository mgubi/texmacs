#!/usr/bin/env python3
###############################################################################
# MODULE     : package.py
# DESCRIPTION: The files of TeXmacs for the browser build, in packages
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
#   package.py <TeXmacs dir> <output dir> <boot list>
#
# Writes <output dir>/texmacs-files.json and the packages it names. A package
# is the concatenation of its files; the manifest gives, for each file, its
# package, offset and size. The page (packages.js) loads the boot package
# before TeXmacs starts, and the others one after the other once it runs; a
# file read before its package has come is fetched alone (a byte range).
#
# The boot package holds the files TeXmacs opens when it starts (the boot
# list: see misc/wasm/boot-files.txt and docs/wasm/README.md) and some whole
# groups which are small and read at unforeseeable times (the Scheme code,
# the styles, the metrics of the fonts, the icons of the default theme).
# The other packages, in the order of their loading, follow PACKAGES below.

import hashlib, json, os, subprocess, sys, fnmatch

EXCLUDE = ['bin', 'plugins/*/bin', 'plugins/*/doc', 'misc/images/windows',
           '*.DS_Store', 'CMakeLists.txt']

BOOT_GROUPS = ['progs/', 'styles/', 'packages/', 'texts/', 'plugins/',
               'langs/encoding/', 'fonts/tfm/', 'fonts/enc/', 'fonts/virtual/',
               'misc/pixmaps/light/']
BOOT_FILES = ['fonts/font-database.scm', 'fonts/font-characteristics.scm',
              'fonts/font-features.scm', 'fonts/font-substitutions.scm',
              'fonts/pdf-font-issues.scm']

# the other packages, in the order they are loaded: (name, prefixes); a
# package larger than CHUNK is split
PACKAGES = [
  ('fonts', ['fonts/']),
  ('icons', ['misc/pixmaps/']),
  ('langs', ['langs/']),
  ('doc',   ['doc/']),
  ('misc',  ['']),
]
CHUNK = 4 * 1024 * 1024

def excluded (rel):
  return any (fnmatch.fnmatch (rel, e) or rel.startswith (e + '/') for e in EXCLUDE)

def main ():
  if len (sys.argv) != 4:
    sys.exit ('usage: package.py <TeXmacs dir> <output dir> <boot list>')
  root, out, boot_list = sys.argv[1:]
  files = []
  for d, ds, fs in os.walk (root):
    rd = os.path.relpath (d, root)
    rd = '' if rd == '.' else rd + '/'
    ds[:] = sorted (x for x in ds if not excluded (rd + x))
    for f in sorted (fs):
      if not excluded (rd + f): files.append (rd + f)
  boot = set ()
  for line in open (boot_list):
    p = line.strip ()
    if p.startswith ('/texmacs/'): p = p[len ('/texmacs/'):]
    if p and not p.startswith ('#'): boot.add (p)
  groups = [('boot', [])] + [(name, []) for name, _ in PACKAGES]
  for rel in files:
    if rel in boot or rel in BOOT_FILES or any (rel.startswith (g) for g in BOOT_GROUPS):
      groups[0][1].append (rel)
      continue
    for i, (name, prefixes) in enumerate (PACKAGES):
      if any (rel.startswith (p) for p in prefixes):
        groups[i + 1][1].append (rel)
        break
  # the chunks: a package larger than CHUNK becomes several
  chunks = []
  for name, rels in groups:
    part, size, n = [], 0, 1
    for rel in rels:
      s = os.path.getsize (os.path.join (root, rel))
      if part and size + s > CHUNK and name != 'boot':
        chunks.append ((name + '-' + str (n), part)); n += 1
        part, size = [], 0
      part.append (rel); size += s
    if part: chunks.append ((name if n == 1 else name + '-' + str (n), part))
  os.makedirs (out, exist_ok = True)
  manifest = { 'root': '/texmacs', 'packages': [] }
  written = set ()
  for name, rels in chunks:
    data, entries = bytearray (), []
    for rel in rels:
      b = open (os.path.join (root, rel), 'rb').read ()
      entries.append ([rel, len (data), len (b)])
      data += b
    digest = hashlib.sha1 (data).hexdigest ()[:10]
    fname = 'tm-%s-%s.pack' % (name, digest)
    written.add (fname)
    # the name has the digest: a package which is there already is the same
    if not os.path.exists (os.path.join (out, fname)):
      open (os.path.join (out, fname), 'wb').write (data)
    manifest['packages'].append ({ 'name': name, 'url': fname, 'size': len (data),
                                   'boot': name == 'boot', 'files': entries })
    print ('%-10s %5d files %7.2f MB  %s' % (name, len (rels), len (data) / 1e6, fname))
  # the packages of a previous build go, with their compressed copies
  for f in os.listdir (out):
    if f.startswith ('tm-') and f.split ('.pack')[0] + '.pack' not in written:
      os.remove (os.path.join (out, f))
  json.dump (manifest, open (os.path.join (out, 'texmacs-files.json'), 'w'),
             separators = (',', ':'))

main ()
