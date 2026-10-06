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
# the styles, the metrics of the fonts, the icons of the default set in the
# light theme: neoclassical, see init_texmacs.cpp).
# The other packages, in the order of their loading, follow PACKAGES below.
#
# The fonts themselves (LAZY: the OpenType and Type 1 files, 42 MB, two
# thirds of the whole) are not in a package: each is a file of its own
# (tm-font-<digest>.<ext>), which the page fetches when TeXmacs first reads
# it and keeps in the cache of the browser for the next visits; the manifest
# lists them under "lazy". Those of the boot list stay in the boot package.

import gzip, hashlib, json, os, re, subprocess, sys, fnmatch

EXCLUDE = ['bin', 'plugins/*/bin', 'plugins/*/doc', 'misc/images/windows',
           '*.DS_Store', 'CMakeLists.txt']

# the documentation of the plugins which work in the browser, kept although
# that of the others is not (in the package doc)
PLUGIN_DOCS = ['plugins/tikz/doc', 'plugins/javascript/doc', 'plugins/asymptote/doc',
               'plugins/ai/doc', 'plugins/python/doc', 'plugins/r/doc']

BOOT_GROUPS = ['progs/', 'styles/', 'packages/', 'texts/', 'plugins/',
               'langs/encoding/', 'fonts/tfm/', 'fonts/enc/', 'fonts/virtual/',
               'misc/pixmaps/neoclassical/light/']
BOOT_FILES = ['fonts/font-database.scm', 'fonts/font-characteristics.scm',
              'fonts/font-features.scm', 'fonts/font-substitutions.scm',
              'fonts/pdf-font-issues.scm',
              'misc/pixmaps/light/TeXmacs.svg'] # the one icon of light/ which
                                                # neoclassical/light has not

# the other packages, in the order they are loaded: (name, prefixes); a
# package larger than CHUNK is split
PACKAGES = [
  ('fonts', ['fonts/']),
  ('icons', ['misc/pixmaps/']),
  ('langs', ['langs/']),
  ('doc',   ['doc/'] + [d + '/' for d in PLUGIN_DOCS]),
  ('misc',  ['']),
]
CHUNK = 4 * 1024 * 1024

# the fonts which are fetched only when TeXmacs reads them
LAZY_DIRS = ['fonts/truetype/', 'fonts/type1/']
LAZY_EXTS = ['.otf', '.ttf', '.ttc', '.pfb']

def lazy (rel):
  return (any (rel.startswith (d) for d in LAZY_DIRS) and
          os.path.splitext (rel)[1].lower () in LAZY_EXTS)

def excluded (rel):
  if any (rel == d or rel.startswith (d + '/') for d in PLUGIN_DOCS): return False
  return any (fnmatch.fnmatch (rel, e) or rel.startswith (e + '/') for e in EXCLUDE)

# SVNREV holds the version TeXmacs checks its files against: that of the
# browser build (ALTERNATIVE_VERSION of misc/wasm/config.h), whatever the
# source tree has (configure writes it, a checkout has none: the CI)
def build_version ():
  config = os.path.join (os.path.dirname (os.path.abspath (__file__)), 'config.h')
  for line in open (config):
    m = re.match (r'#define ALTERNATIVE_VERSION "(.*)"', line)
    if m: return m.group (1)
  sys.exit ('package.py: no ALTERNATIVE_VERSION in ' + config)

def file_bytes (root, rel):
  if rel == 'SVNREV': return (build_version () + '\n').encode ()
  return open (os.path.join (root, rel), 'rb').read ()

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
      if not excluded (rd + f) and rd + f != 'SVNREV': files.append (rd + f)
  files.append ('SVNREV')
  boot = set ()
  for line in open (boot_list):
    p = line.strip ()
    if p.startswith ('/texmacs/'): p = p[len ('/texmacs/'):]
    if p and not p.startswith ('#'): boot.add (p)
  groups = [('boot', [])] + [(name, []) for name, _ in PACKAGES]
  lazies = []
  for rel in files:
    if rel in boot or rel in BOOT_FILES or any (rel.startswith (g) for g in BOOT_GROUPS) \
       and not any (rel.startswith (d + '/') for d in PLUGIN_DOCS):
      groups[0][1].append (rel)
      continue
    if lazy (rel):
      lazies.append (rel)
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
      s = len (file_bytes (root, rel)) if rel == 'SVNREV' else os.path.getsize (os.path.join (root, rel))
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
      b = file_bytes (root, rel)
      entries.append ([rel, len (data), len (b)])
      data += b
    digest = hashlib.sha1 (data).hexdigest ()[:10]
    fname = 'tm-%s-%s.pack' % (name, digest)
    written.add (fname)
    # the name has the digest: a package which is there already is the same
    if not os.path.exists (os.path.join (out, fname)):
      open (os.path.join (out, fname), 'wb').write (data)
    # a gzip copy, which the page decompresses itself (packages.js): the
    # servers of static files (GitHub Pages) do not send the brotli copies
    # of serve.mjs; the package itself stays, for the byte ranges of a file
    # needed before its package
    gzname = fname + '.gz'
    if not os.path.exists (os.path.join (out, gzname)):
      open (os.path.join (out, gzname), 'wb').write (gzip.compress (bytes (data), 9, mtime = 0))
    written.add (gzname)
    manifest['packages'].append ({ 'name': name, 'url': fname, 'size': len (data),
                                   'gz': gzname,
                                   'boot': name == 'boot', 'files': entries })
    print ('%-10s %5d files %7.2f MB  %s' % (name, len (rels), len (data) / 1e6, fname))
  # the fonts fetched on demand: a file each, named by its digest (a file
  # which is there already is the same, and two copies of a font are one)
  manifest['lazy'] = []
  lazy_size = 0
  for rel in lazies:
    b = file_bytes (root, rel)
    fname = 'tm-font-%s%s' % (hashlib.sha1 (b).hexdigest ()[:12],
                              os.path.splitext (rel)[1].lower ())
    written.add (fname)
    if not os.path.exists (os.path.join (out, fname)):
      open (os.path.join (out, fname), 'wb').write (b)
    manifest['lazy'].append ([rel, fname, len (b)])
    lazy_size += len (b)
  print ('%-10s %5d files %7.2f MB  tm-font-*' % ('lazy', len (lazies), lazy_size / 1e6))
  # the packages and fonts of a previous build go, with their compressed copies
  for f in os.listdir (out):
    if f.startswith ('tm-') and f not in written and \
       f.split ('.pack')[0] + '.pack' not in written:
      os.remove (os.path.join (out, f))
  json.dump (manifest, open (os.path.join (out, 'texmacs-files.json'), 'w'),
             separators = (',', ':'))

main ()
