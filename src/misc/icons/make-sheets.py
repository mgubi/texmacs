#!/usr/bin/env python3
###############################################################################
# MODULE     : make-sheets.py
# DESCRIPTION: Specimen sheets (PDF) and toolbar pictures of the icon sets
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# Writes, for each icon set (classical, monochrome, neo-classical):
#   doc/icons/<set>.pdf           every icon with its name, on light and on
#                                 dark pages (vector: the SVG files as they are)
#   doc/icons/<set>-toolbars.png  the main, text and focus toolbars, light and
#                                 dark, as pictured in the README
#
# Usage (from src/): misc/icons/make-sheets.py
# Needs rsvg-convert.

import os, re, subprocess, sys

PIX= os.path.join ("TeXmacs", "misc", "pixmaps")
SETS= [("classical", "Classical", PIX),
       ("monochrome", "Monochrome", os.path.join (PIX, "monochrome")),
       ("neoclassical", "Neo-classical", os.path.join (PIX, "neoclassical"))]
OUT= os.path.join ("doc", "icons")
THEMES= {"light": ("#F3F3F3", "#3A3A3C", "#8A8A8E"),
         "dark": ("#2B2D31", "#E6E6EA", "#9A9AA0")}

MAIN= ["new", "open", "save", "build", "print", "preferences", "cancel", None,
       "cut", "copy", "paste", "find", "replace", "spell", "undo", "redo", None,
       "back", "reload", "forward"]
TEXT= ["title", "chapter", "section", "theorem", "list", "program", None,
       "emphasize", "strong", "verbatim", "italic", "bold", "typewriter",
       "smallcaps", "color", None, "math", "table", "image", "link", "shell"]
FOCUS= ["focus_search", "focus_help", "focus_prefs", "focus_delete", None,
        "insert_left", "insert_right", "delete_left", "delete_right", None,
        "similar_previous", "similar_next", "search_previous", "search_next",
        None, "show_hidden", "lock_closed", "go", "stop"]

###############################################################################
# Nesting the SVG files of the icons in one document
###############################################################################

def icon_file (d, theme, name):
  f= os.path.join (d, theme, "tm_%s.svg" % name)
  if os.path.exists (f): return f
  f= os.path.join (PIX, theme, "tm_%s.svg" % name)   # the classical fallback
  if os.path.exists (f): return f
  xpm= os.path.join (PIX, "traditional", "--x17", "tm_%s.xpm" % name)
  if os.path.exists (xpm):
    # an original that only existed as a bitmap: embed it as an image
    import base64
    png= subprocess.run (["magick", xpm, "-filter", "point", "-resize", "400%", "png:-"],
                         capture_output= True).stdout
    if png:
      tmp= os.path.join (OUT, ".tmp", "tm_%s-%s.svg" % (name, theme))
      open (tmp, "w").write ('<svg xmlns="http://www.w3.org/2000/svg" '
                             'xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 68 68">'
                             '<image width="68" height="68" xlink:href="data:image/png;base64,%s"/>'
                             '</svg>' % base64.b64encode (png).decode ())
      return tmp
  return None

def nested (path, k, x, y, size):
  # the icon as a nested <svg>, its ids and style classes prefixed with k
  s= open (path, encoding= "utf-8").read ()
  m= re.search (r"<svg\b([^>]*)>(.*)</svg>", s, re.S)
  if not m: return ""
  attrs, body= m.group (1), m.group (2)
  # editor metadata of the original icons (undeclared namespaces once nested)
  body= re.sub (r"<metadata\b.*?</metadata>", "", body, flags= re.S)
  body= re.sub (r"<(sodipodi|inkscape):[\w-]+\b[^>]*/>", "", body)
  body= re.sub (r"<(sodipodi|inkscape):([\w-]+)\b.*?</\1:\2>", "", body, flags= re.S)
  body= re.sub (r'\s(sodipodi|inkscape|rdf|cc|dc):[\w.:-]+="[^"]*"', "", body)
  vb= re.search (r'viewBox="([^"]*)"', attrs)
  if vb: viewbox= vb.group (1)
  else:
    w= re.search (r'\bwidth="([0-9.]+)', attrs)
    h= re.search (r'\bheight="([0-9.]+)', attrs)
    viewbox= "0 0 %s %s" % (w.group (1) if w else 24, h.group (1) if h else 24)
  keep= " ".join (a for a in re.findall (r'\s((?:fill|stroke|style)[\w-]*="[^"]*")', attrs))
  p= "i%d-" % k
  body= re.sub (r'\bid="([^"]+)"', lambda m: 'id="%s%s"' % (p, m.group (1)), body)
  body= re.sub (r'url\(\s*#([^)\s]+)\s*\)', lambda m: "url(#%s%s)" % (p, m.group (1)), body)
  body= re.sub (r'href="#([^"]+)"', lambda m: 'href="#%s%s"' % (p, m.group (1)), body)
  body= re.sub (r'class="([^"]+)"',
                lambda m: 'class="%s"' % " ".join (p + c for c in m.group (1).split ()), body)
  body= re.sub (r"(<style[^>]*>)(.*?)(</style>)",
                lambda m: m.group (1) + re.sub (r"\.([A-Za-z_][\w-]*)", r".%s\1" % p, m.group (2)) + m.group (3),
                body, flags= re.S)
  return ('<svg x="%g" y="%g" width="%g" height="%g" viewBox="%s" %s>%s</svg>'
          % (x, y, size, size, viewbox, keep, body))

def document (w, h, bg, content):
  return ('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" '
          'width="%g" height="%g" viewBox="0 0 %g %g">'
          '<rect width="100%%" height="100%%" fill="%s"/>%s</svg>' % (w, h, w, h, bg, content))

def escape (t):
  return t.replace ("&", "&amp;").replace ("<", "&lt;")

###############################################################################
# Specimen sheets
###############################################################################

def sheets (key, title, d, tmp):
  names= sorted (f[3:-4] for f in os.listdir (os.path.join (d, "light"))
                 if f.startswith ("tm_") and f.endswith (".svg"))
  if key == "classical": names= [n for n in names if n != "TeXmacs"]
  cols, rows, cw, ch, size= 9, 9, 104, 84, 40
  per= cols * rows
  pages= []
  for theme in ("light", "dark"):
    bg, fg, dim= THEMES[theme]
    for start in range (0, len (names), per):
      chunk= names[start:start + per]
      w, h= cols * cw + 60, rows * ch + 110
      out= ['<text x="30" y="46" font-family="Helvetica, Arial" font-size="22" '
            'font-weight="bold" fill="%s">TeXmacs icons: %s</text>' % (fg, title),
            '<text x="30" y="72" font-family="Helvetica, Arial" font-size="13" fill="%s">'
            '%s theme, icons %d-%d of %d</text>'
            % (dim, theme.capitalize (), start + 1, start + len (chunk), len (names))]
      for i, n in enumerate (chunk):
        x= 30 + (i % cols) * cw; y= 95 + (i // cols) * ch
        f= icon_file (d, theme, n)
        if f: out.append (nested (f, start + i + (0 if theme == "light" else 10000),
                                  x + (cw - size) / 2, y, size))
        out.append ('<text x="%g" y="%g" text-anchor="middle" font-family="Menlo, monospace" '
                    'font-size="8" fill="%s">%s</text>' % (x + cw / 2, y + size + 16, dim, escape (n)))
      page= os.path.join (tmp, "%s-%s-%d.svg" % (key, theme, start))
      open (page, "w", encoding= "utf-8").write (document (w, h, bg, "".join (out)))
      pages.append (page)
  pdf= os.path.join (OUT, "%s.pdf" % key)
  subprocess.run (["rsvg-convert", "-f", "pdf", "-o", pdf] + pages, check= True)
  return len (names)

###############################################################################
# Toolbars
###############################################################################

def toolbars (key, title, d, tmp):
  size, step, gap, pad= 24, 32, 12, 12
  def strip (names, theme, x0, y0):
    out= []; x= x0
    for n in names:
      if n is None:
        out.append ('<rect x="%g" y="%g" width="1" height="%g" fill="%s"/>'
                    % (x + gap / 2 - .5, y0 + 4, size, THEMES[theme][2]))
        x += gap; continue
      f= icon_file (d, theme, n)
      if f: out.append (nested (f, hash ((theme, n, x0, y0)) & 0xffffff,
                                x + (step - size) / 2, y0 + 4, size))
      x += step
    return "".join (out), x
  rows= [MAIN, TEXT, FOCUS]
  width= max (sum (gap if n is None else step for n in r) for r in rows) + 2 * pad
  height= len (rows) * (size + 16) + 2 * pad
  parts= []
  for i, theme in enumerate (("light", "dark")):
    bg= THEMES[theme][0]; content= ""
    for j, r in enumerate (rows):
      s, _= strip (r, theme, pad, pad + j * (size + 16))
      content += s
    parts.append ('<g transform="translate(0 %g)"><rect width="%g" height="%g" fill="%s"/>%s</g>'
                  % (i * height, width, height, bg, content))
  svg= os.path.join (tmp, "%s-toolbars.svg" % key)
  open (svg, "w", encoding= "utf-8").write (document (width, 2 * height, "#FFFFFF", "".join (parts)))
  png= os.path.join (OUT, "%s-toolbars.png" % key)
  subprocess.run (["rsvg-convert", "-z", "2", "-o", png, svg], check= True)

def main ():
  os.makedirs (OUT, exist_ok= True)
  tmp= os.path.join (OUT, ".tmp")
  os.makedirs (tmp, exist_ok= True)
  for key, title, d in SETS:
    n= sheets (key, title, d, tmp)
    toolbars (key, title, d, tmp)
    print ("%s: %d icons" % (title, n))
  for f in os.listdir (tmp): os.remove (os.path.join (tmp, f))
  os.rmdir (tmp)

if __name__ == "__main__":
  main ()
