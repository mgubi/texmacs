#!/usr/bin/env python3
# The facts of a build of the browser version, as a section of its help page
# (Help > TeXmacs in the browser, which includes it before "Recent changes"):
#
#   python3 misc/wasm/build-info.py <src> <build dir> <MuPDF dir> <ThorVG prefix> <out.tm>
#
# run by misc/wasm/Makefile once the files of the page are packaged: the
# commit and the date of the build, the components with their versions,
# their origins and the checksums (or commits) of their sources, the sizes
# of the program and of the files of TeXmacs (from the manifest), and the
# plug-ins of the page with the programs they run. The file is written in
# the source tree, where package.py finds it (it is not in git).

import hashlib, json, os, re, shutil, subprocess, sys, time

src, build, mupdf, thorvg, out = sys.argv[1:6]
web = os.path.join (build, 'out', 'web')

def read (p):
  try:
    with open (p, 'r', encoding='latin-1') as f: return f.read ()
  except OSError: return ''

def grep (p, pattern, group=1):
  m = re.search (pattern, read (p), re.M)
  return m.group (group) if m else ''

def run (*args):
  try: return subprocess.run (args, capture_output=True, text=True, check=True).stdout.strip ()
  except Exception: return ''

def sha256 (p):
  try:
    h = hashlib.sha256 ()
    with open (p, 'rb') as f:
      for b in iter (lambda: f.read (1 << 20), b''): h.update (b)
    return h.hexdigest ()
  except OSError: return ''

def size (p):
  try: return os.path.getsize (p)
  except OSError: return 0

def dir_size (d):
  t = 0
  for root, ds, fs in os.walk (d):
    for f in fs:
      # the copies compressed for the servers do not count, the files served compressed do
      if f.endswith ('.br') or (f.endswith ('.gz') and f[:-3] in fs): continue
      t += size (os.path.join (root, f))
  return t

def mb (n): return '%.1f MB' % (n / 1e6) if n >= 1e5 else '%d KB' % round (n / 1e3)

# the text of a TeXmacs document (ASCII: see the Cork encoding of .tm files)
def tm (s):
  s = str (s).replace ('\\', '\\\\').replace ('<', '\\<less\\>').replace ('>', '\\<gtr\\>')
  s = s.replace ('|', '\\|')
  return ''.join (c if ord (c) < 128 else '?' for c in s)

def verb (s): return '<verbatim|' + tm (s) + '>' if s else '--'
def name (s): return '<name|' + tm (s) + '>'
def short (h, n=12): return h[:n] if h else ''

# --- this build
version = grep (os.path.join (src, 'misc', 'wasm', 'tm_configure.hpp'), r'TEXMACS_VERSION "([^"]*)"')
commit = run ('git', '-C', src, 'rev-parse', 'HEAD')
commit_date = run ('git', '-C', src, 'log', '-1', '--format=%cd', '--date=format:%Y-%m-%d %H:%M %z')
branch = os.environ.get ('GITHUB_REF_NAME') or run ('git', '-C', src, 'rev-parse', '--abbrev-ref', 'HEAD')
dirty = run ('git', '-C', src, 'status', '--porcelain', '--untracked-files=no', '--', '.')
built = time.strftime ('%Y-%m-%d %H:%M UTC', time.gmtime ())
repo = 'https://github.com/' + os.environ.get ('GITHUB_REPOSITORY', 'mgubi/texmacs')

# --- the components
emcc = shutil.which ('emcc') or ''
ems_version = (run ('emcc', '--version').split ('\n') or [''])[0]
m = re.search (r'(\d+\.\d+\.\d+)', ems_version); ems_version = m.group (1) if m else ems_version
ems_root = os.path.dirname (os.path.realpath (emcc)) if emcc else ''
if ems_root.endswith ('/bin') and os.path.isdir (os.path.join (ems_root, '..', 'libexec')):
  ems_root = os.path.join (ems_root, '..', 'libexec')
sdl_port = os.path.join (ems_root, 'tools', 'ports', 'sdl3.py')
sdl_version = grep (sdl_port, r"^VERSION = '([^']*)'")
sdl_sha512 = grep (sdl_port, r"^HASH = '([^']*)'")

mupdf_version = grep (os.path.join (mupdf, 'include', 'mupdf', 'fitz', 'version.h'), r'FZ_VERSION "([^"]*)"')
mupdf_tgz = os.path.join (os.path.dirname (mupdf.rstrip ('/')), 'mupdf-%s-source.tar.gz' % mupdf_version)
mupdf_sha = sha256 (mupdf_tgz)
third = os.path.join (mupdf, 'thirdparty')
ft = os.path.join (third, 'freetype', 'include', 'freetype', 'freetype.h')
freetype = '.'.join (grep (ft, r'define FREETYPE_%s\s+(\d+)' % k) for k in ('MAJOR', 'MINOR', 'PATCH'))
zlib = grep (os.path.join (third, 'zlib', 'zlib.h'), r'define ZLIB_VERSION "([^"]*)"')
libjpeg = grep (os.path.join (third, 'libjpeg', 'jversion.h'), r'define JVERSION\s+"(\S+)')
openjpeg = ''
for root, ds, fs in os.walk (os.path.join (third, 'openjpeg')):
  for f in fs:
    if f.startswith ('opj_config') and f.endswith ('.h'):
      openjpeg = openjpeg or grep (os.path.join (root, f), r'OPJ_PACKAGE_VERSION "([^"]*)"')
lcms = ''
for root, ds, fs in os.walk (os.path.join (third, 'lcms2', 'include')):
  for f in fs:
    v = grep (os.path.join (root, f), r'define LCMS_VERSION\s+\(?(\d+)')
    if v and not lcms: lcms = '%d.%d' % (int (v) // 1000, (int (v) % 1000) // 10)
jbig2 = '.'.join (grep (os.path.join (third, 'jbig2dec', 'jbig2.h'), r'define JBIG2_VERSION_%s\s+\(?(\d+)' % k)
                  for k in ('MAJOR', 'MINOR', 'PATCH')).strip ('.')

thorvg_h = os.path.join (thorvg, 'include', 'thorvg-1', 'thorvg.h') if thorvg else ''
thorvg_version = '.'.join (grep (thorvg_h, r'define TVG_VERSION_%s\s+(\d+)' % k)
                           for k in ('MAJOR', 'MINOR', 'MICRO')).strip ('.') if thorvg_h else ''
thorvg_commit = run ('git', '-C', os.path.join (os.path.dirname (thorvg.rstrip ('/')), 'src'), 'rev-parse', 'HEAD') if thorvg else ''

s7_h = os.path.join (src, 'src', 'Scheme', 'S7', 's7.h')
s7_version = grep (s7_h, r'define S7_VERSION "([^"]*)"')
s7_date = grep (s7_h, r'define S7_DATE "([^"]*)"')
s7_patches = sorted (f for f in os.listdir (os.path.join (src, 'src', 'Scheme', 'S7', 'patches'))
                     if f.endswith ('.patch')) if os.path.isdir (os.path.join (src, 'src', 'Scheme', 'S7', 'patches')) else []
clay_version = grep (os.path.join (src, 'src', 'Plugins', 'Vue', 'clay.h'), r'VERSION:\s*(\S+)')

def pinned (script):
  p = os.path.join (src, 'misc', 'wasm', script)
  return grep (p, r'^V=(\S+)'), grep (p, r'^SHA=(\S+)')
hunspell_version, hunspell_sha = pinned ('get-hunspell.sh')
tikzjax_version, tikzjax_sha = pinned ('get-tikzjax.sh')
asymptote_version, asymptote_sha = pinned ('get-asymptote.sh')
pyodide_version = grep (os.path.join (src, 'plugins', 'python', 'web', 'tm-python.mjs'), r"PYODIDE_VERSION = '([^']*)'")
webr_version = grep (os.path.join (src, 'plugins', 'r', 'web', 'tm-r.mjs'), r"WEBR_VERSION = '([^']*)'")

# --- the sizes
def sizes (f):
  p = os.path.join (web, f)
  return size (p), size (p + '.br'), size (p + '.gz')

wasm = sizes ('texmacs.wasm')
js = sizes ('texmacs.js')
try:
  with open (os.path.join (web, 'texmacs-files.json')) as f: manifest = json.load (f)
except (OSError, ValueError): manifest = { 'packages': [], 'lazy': [] }
kinds = [('progs/', 'the Scheme code of TeXmacs'), ('fonts/', 'the fonts'),
         ('doc/', 'the documentation'), ('packages/', 'the style packages'),
         ('styles/', 'the styles'), ('langs/', 'the languages (dictionaries, hyphenation)'),
         ('misc/pixmaps/', 'the icons'), ('misc/', 'the other supporting files'),
         ('plugins/', 'the plug-ins (Scheme code, documentation)')]
by_kind = { k: [0, 0] for k, _ in kinds }; other = [0, 0]
def count (path, n):
  for k, _ in kinds:
    if path.startswith (k): by_kind[k][0] += n; by_kind[k][1] += 1; return
  other[0] += n; other[1] += 1
packs = []
for p in manifest.get ('packages', []):
  for f in p.get ('files', []): count (f[0], f[2])
  packs.append ((p['name'], p.get ('size', 0), size (os.path.join (web, p['url'] + '.br')),
                 len (p.get ('files', [])), p.get ('boot', False)))
lazy = manifest.get ('lazy', [])
lazy_n = 0; lazy_bytes = 0; lazy_br = 0
for f in lazy:
  path = f.get ('path') if isinstance (f, dict) else f[0]
  n = f.get ('size', 0) if isinstance (f, dict) else f[-1]
  url = f.get ('url') if isinstance (f, dict) else (f[1] if len (f) > 2 else '')
  if isinstance (path, str): count (path, n)
  lazy_n += 1; lazy_bytes += n
  if url: lazy_br += size (os.path.join (web, url + '.br'))

# --- the document
L = []
def line (s=''): L.append (s)
# a centered table, its first row the heads (in bold, with a rule below)
def table (rows):
  rows[0] = ['<strong|' + c + '>' for c in rows[0]]
  line ('  <\\center>')
  line ('    <tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<table|' + '|'.join (
        '<row|' + '|'.join ('<cell|' + c + '>' for c in r) + '>' for r in rows) + '>>>')
  line ('  </center>')
  line ()
def items (xs):
  line ('  <\\itemize>')
  for x in xs: line ('    <item>' + x); line ()
  L.pop ()
  line ('  </itemize>')
  line ()
# (a sha512 in two lines of 64 digits)
def checksum (kind, h):
  if not h: return 'no checksum (not found)'
  return kind + ' ' + ' '.join ('<verbatim|' + tm (h[i:i+64]) + '>' for i in range (0, len (h), 64))

line ('<TeXmacs|%s>' % (version or '2.1.5'))
line ()
line ('<style|tmdoc>')
line ()
line ('<\\body>')
line ('  <section|This build>')
line ()
line ('  This page was made by the build of the commit ' +
      '<hlink|' + verb (short (commit)) + '|' + tm (repo + '/commit/' + commit) + '> of ' +
      verb (branch) + ' (' + tm (commit_date) + ')' +
      (', with changes which were not committed' if dirty else '') +
      ', on ' + tm (built) + ', from the sources of <TeXmacs> ' + tm (version) + '.')
line ()
line ('  <subsection|The components of the program>')
line ()
line ('  They are compiled into ' + verb ('texmacs.wasm') + ' by ' + name ('Emscripten') + ' ' +
      tm (ems_version) + ' (the programs of the plug-ins are apart, see below):')
line ()
rows = [['Component', 'Version', 'What it does']]
rows.append (['<TeXmacs>', tm (version), 'the editor and the typesetter'])
rows.append ([name ('S7') + ' Scheme', tm (s7_version), 'the extension language, in place of ' + name ('Guile')])
rows.append ([name ('MuPDF'), tm (mupdf_version), 'the pictures, PDF, the pixels without the GPU'])
bundled = [(n, v, w) for n, v, w in (
  ('FreeType', freetype, 'the glyphs of the fonts'), ('zlib', zlib, 'compression'),
  ('libjpeg', libjpeg, 'JPEG pictures'), ('OpenJPEG', openjpeg, 'JPEG 2000 pictures'),
  ('Little CMS', lcms, 'colour management'), ('jbig2dec', jbig2, 'JBIG2 pictures')) if v]
for n, v, w in bundled: rows.append (['  ' + name (n), tm (v), w + ' (in ' + name ('MuPDF') + ')'])
rows.append ([name ('SDL'), tm (sdl_version), 'the window, the events, the keyboard'])
if thorvg_version:
  rows.append ([name ('ThorVG'), tm (thorvg_version), 'the drawing with the GPU (WebGL2)'])
rows.append ([name ('Clay'), tm (clay_version), 'the layout of the interface'])
rows.append ([name ('Hunspell'), tm (hunspell_version), 'the spell checker'])
table (rows)
line ('  Their sources:')
line ()
src_items = [
  '<TeXmacs>: the commit ' + verb (commit) + ' of <hlink|' + tm (repo) + '|' + tm (repo) + '>.',
  name ('S7') + ' ' + tm (s7_version) + ' (' + tm (s7_date) + ') of <hlink|ccrma.stanford.edu/software/snd|https://ccrma.stanford.edu/software/snd/snd/s7.html>, ' +
  'in the sources of <TeXmacs> (' + verb ('src/Scheme/S7') + '), with ' + tm (len (s7_patches)) + ' patches.',
  name ('MuPDF') + ': ' + verb ('mupdf-%s-source.tar.gz' % mupdf_version) + ' of <hlink|mupdf.com|https://mupdf.com/releases>, ' +
  checksum ('sha256', mupdf_sha) + '; the libraries it bundles come with it.',
  name ('SDL') + ': ' + verb ('release-%s.zip' % sdl_version) + ' of <hlink|github.com/libsdl-org/SDL|https://github.com/libsdl-org/SDL>, ' +
  'as the port of ' + name ('Emscripten') + ', ' + checksum ('sha512', sdl_sha512) + '.']
if thorvg_version:
  src_items.append (name ('ThorVG') + ': the commit ' + verb (thorvg_commit) +
                    ' of <hlink|github.com/thorvg/thorvg|https://github.com/thorvg/thorvg>.')
src_items += [
  name ('Clay') + ': ' + verb ('clay.h') + ' of <hlink|github.com/nicbarker/clay|https://github.com/nicbarker/clay>, in the sources of <TeXmacs> (' + verb ('src/Plugins/Vue') + ').',
  name ('Hunspell') + ': ' + verb ('hunspell-%s.tar.gz' % hunspell_version) + ' of <hlink|github.com/hunspell/hunspell|https://github.com/hunspell/hunspell>, ' +
  checksum ('sha256', hunspell_sha) + '.']
items (src_items)
line ('  <subsection|The sizes>')
line ()
line ('  The program is ' + verb ('texmacs.wasm') + ', ' + tm (mb (wasm[0])) + ' (' + tm (mb (wasm[1])) +
      ' downloaded, compressed with brotli), with ' + verb ('texmacs.js') + ', ' + tm (mb (js[0])) +
      ' (' + tm (mb (js[1])) + '). The files of <TeXmacs>, by kind:')
line ()
rows = [['Files', 'Number', 'Size']]
for k, what in kinds:
  if by_kind[k][1]: rows.append ([what + ' (' + verb (k) + ')', tm (by_kind[k][1]), tm (mb (by_kind[k][0]))])
if other[1]: rows.append (['the others', tm (other[1]), tm (mb (other[0]))])
table (rows)
line ('  They come in packages, which the browser keeps: the boot package before ' +
      '<TeXmacs> starts, the others in the background; and the fonts one by one, ' +
      'when a document first uses them:')
line ()
rows = [['Package', 'Files', 'Size', 'Downloaded']]
for n, s, br, nf, boot in packs:
  rows.append ([verb (n), tm (nf), tm (mb (s)), tm (mb (br)) if br else '--'])
if lazy_n:
  rows.append (['the fonts', tm (lazy_n), tm (mb (lazy_bytes)), tm (mb (lazy_br)) if lazy_br else '--'])
table (rows)
line ('  <subsection|The plug-ins>')
line ()
line ('  The plug-ins which work in the page (<menu|Help|Plug-ins>), and the programs they run:')
line ()
rows = [['Plug-in', 'Runs', 'Size']]
rows.append ([name ('Python'), name ('Pyodide') + ' ' + tm (pyodide_version) + ' (' + name ('Python') + ' 3.14)', 'about 10 MB'])
rows.append ([name ('R'), name ('webR') + ' ' + tm (webr_version) + ' (' + name ('R') + ')', '--'])
rows.append ([name ('TikZ'), name ('TikZJax') + ' ' + tm (tikzjax_version) + ' (<TeX>)', tm (mb (dir_size (os.path.join (web, 'tikzjax'))))])
rows.append ([name ('Asymptote'), name ('Asymptote-web') + ' ' + tm (asymptote_version), tm (mb (dir_size (os.path.join (web, 'asymptote'))))])
rows.append ([name ('JavaScript'), 'the engine of the browser', '--'])
rows.append (['AI', 'the services of the providers', '--'])
table (rows)
line ('  Where the programs come from:')
line ()
items ([
  name ('Pyodide') + ': <hlink|cdn.jsdelivr.net/pyodide/v' + tm (pyodide_version) + '|https://cdn.jsdelivr.net/pyodide/v' +
  tm (pyodide_version) + '/full/>, loaded at the first input of a session (then kept by the browser).',
  name ('webR') + ': <hlink|webr.r-wasm.org/v' + tm (webr_version) + '|https://webr.r-wasm.org/v' + tm (webr_version) +
  '/>, loaded at the first input of a session (then kept by the browser).',
  name ('TikZJax') + ': the package ' + verb ('@rod2ik/tikzjax') + ' of npm, ' + checksum ('sha256', tikzjax_sha) +
  ', served with the page (' + verb ('tikzjax/') + ').',
  name ('Asymptote-web') + ': the package ' + verb ('asymptote-web') + ' of npm, ' + checksum ('sha256', asymptote_sha) +
  ', served with the page (' + verb ('asymptote/') + ').',
  'AI: the services chosen in the preferences, with your keys, through the network.'])
line ('</body>')
line ()
line ('<initial|<\\collection>')
line ('</collection>>')

with open (out, 'w', encoding='ascii') as f: f.write ('\n'.join (L) + '\n')
print ('build-info: ' + out)
