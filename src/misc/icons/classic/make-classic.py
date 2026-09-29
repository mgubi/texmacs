#!/usr/bin/env python3
###############################################################################
# MODULE     : make-classic.py
# DESCRIPTION: The classic TeXmacs icons, in colour, modernized
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# A second icon set, next to the monochrome one of ../make-icons.py: the
# compositions and colours of the original TeXmacs icons (light pages with
# grey outlines, khaki folders, green and red block arrows, lavender lenses,
# dashed selections, glove hands), redrawn flat on the 24x24 grid with
# rounded corners and one outline weight.
#
# The icons are written to TeXmacs/misc/pixmaps/classic/{light,dark}. To try
# them, put that directory first in TEXMACS_PIXMAP_PATH; when set, the
# variable replaces the whole default path, which must then be repeated
# (the toolbars take their sizes from the bitmaps in pixmaps/modern):
#   P=$TEXMACS_PATH/misc/pixmaps
#   TEXMACS_PIXMAP_PATH=$P/classic:$P:$P/modern/32x32/settings:\
#     $P/modern/32x32/table:$P/modern/24x24/main:$P/modern/20x20/mode:\
#     $P/modern/16x16/focus:$P/traditional/--x17
#
# Usage (from src/): misc/icons/classic/make-classic.py [names...]
#
# Classes, turned into presentation attributes (the Qt SVG renderer ignores
# style sheets):
#   o      outline                paper   white paper with its outline
#   kh     khaki (folders, frames)  khd   darker khaki (inside of a folder)
#   g      green (adds, loads)      r     red (removes, saves, errors)
#   lav    lavender (lenses)        br    brown (wood)
#   met    light metal              dk    dark metal
#   skin   glove                    grey  grey block (a selection's content)
#   dash   dashed outline (a selection)  rs    red stroke
#   ink    letters (emb: thickened)
#   thin wide ow  stroke width modifiers (ow: a double outline, half of it
#                 covered by a fill drawn on top)

import os, re, sys

STROKE= 1.0
SCALE= 1.05            # drawings are scaled around the centre, strokes too
OUTLINE= "dark"        # grey | tone (darker tone of the fill) | dark (near black)
PALETTE= "soft"        # modern | soft
SHADING= "soft"        # flat | soft (a light vertical gradient on every fill)
JOINS= "miter"         # round | miter

MODERN = {
 "light": dict (line="#6E6E6E", paper="#FAFAFA", kh="#D8D0A4", khl="#8A8258",
                khd="#B5AC7E", g="#2FA046", gl="#1D6E2D", r="#B8322A", rl="#7E1D17",
                lav="#DEE0FB", br="#8E6443", brl="#5C3D26", met="#D6D6D8",
                metl="#7C7C80", dk="#5A5A5E", dkl="#2E2E30", skin="#FFF3EA",
                skinl="#3C3C3C", grey="#BDBDBD", ink="#303032", white="#FFFFFF"),
 "dark":  dict (line="#B4B4B8", paper="#4B4D53", kh="#9A9168", khl="#D6CDA0",
                khd="#7C7452", g="#3DB655", gl="#9BE3A8", r="#E0463C", rl="#F5A39C",
                lav="#4A4F7A", br="#9A7050", brl="#D8B89A", met="#6A6A70",
                metl="#C8C8CC", dk="#8A8A90", dkl="#D8D8DC", skin="#5A5048",
                skinl="#E4DCD4", grey="#7A7A80", ink="#E6E6EA", white="#FFFFFF"),
}


WHITE, BLACK, TOOLBAR_DARK= "#FFFFFF", "#000000", "#2B2D31"
LINES= ("line", "khl", "gl", "rl", "brl", "metl", "dkl", "skinl")

def mix (a, b, t):
  ca= [int (a[i:i+2], 16) for i in (1, 3, 5)]
  cb= [int (b[i:i+2], 16) for i in (1, 3, 5)]
  return "#%02X%02X%02X" % tuple (round (x + (y - x) * t) for x, y in zip (ca, cb))

def dark_of (p):
  # a dark theme version of a light palette: lighter lines, deeper fills
  q= {}
  for k, v in p.items ():
    if k in LINES: q[k]= mix (v, WHITE, .55)
    elif k == "white": q[k]= v
    else: q[k]= mix (v, TOOLBAR_DARK, .35)
  q["paper"]= "#4B4D53"; q["ink"]= "#E6E6EA"; q["lav"]= mix (p["lav"], TOOLBAR_DARK, .6)
  return q

SOFT_LIGHT= dict (line="#8A8A86", paper="#FFFFFF", kh="#EAE3C6", khl="#A89F78",
                  khd="#D8CFAC", g="#7CC68A", gl="#4E9A5C", r="#E08C84", rl="#B0574F",
                  lav="#ECEDFF", br="#C29A78", brl="#8E6A4E", met="#E6E6E8",
                  metl="#9A9AA0", dk="#9A9AA0", dkl="#6A6A70", skin="#FFF6EF",
                  skinl="#6A6660", grey="#D4D4D4", ink="#4A4A48", white="#FFFFFF")
SOFT= {"light": SOFT_LIGHT, "dark": dark_of (SOFT_LIGHT)}
PAL= SOFT if PALETTE == "soft" else MODERN

def outline (p, key):
  if OUTLINE == "grey": return p["line"]
  if OUTLINE == "dark": return "#D8D8DC" if p["ink"] == "#E6E6EA" else "#48484B"
  return p[key]

def attributes (cls, p):
  cl= cls.split ()
  fill= "none"; stroke= "none"; width= STROKE; extra= ""
  pairs= { "paper": ("paper", "line"), "kh": ("kh", "khl"), "khd": ("khd", "khl"),
           "g": ("g", "gl"), "r": ("r", "rl"), "lav": ("lav", "line"),
           "br": ("br", "brl"), "met": ("met", "metl"), "dk": ("dk", "dkl"),
           "skin": ("skin", "skinl") }
  for c, (f, s) in pairs.items ():
    if c in cl: fill= p[f]; stroke= outline (p, s)
  if "grey" in cl: fill= p["grey"]
  if "o" in cl: stroke= outline (p, "line")
  if "ink" in cl: fill= p["ink"]
  if "rs" in cl: stroke= p["r"]
  if "emb" in cl: stroke= p["ink"]; width= 0.6
  if "nostroke" in cl: stroke= "none"
  if "ow" in cl: width= 2 * STROKE
  if "dash" in cl: stroke= p["ink"]; extra= ' stroke-dasharray="2 1.6"'
  if "thin" in cl: width= STROKE * .8
  if "wide" in cl: width= max (2.2, STROKE * 1.5)
  if "emb" in cl: width= 0.6
  a= 'fill="%s"' % fill
  if stroke != "none": a += ' stroke="%s" stroke-width="%g"' % (stroke, width)
  return a + extra

def svg (body, p):
  body= re.sub (r'class="([^"]*)"', lambda m: attributes (m.group (1), p), body)
  defs= ""
  if SHADING == "soft":
    grads= {}
    def grad (m):
      nonlocal defs
      c= m.group (1)
      if c not in grads:
        grads[c]= "g%d" % len (grads)
        defs += ('<linearGradient id="%s" x1="0" y1="0" x2="0" y2="1">'
                 '<stop offset="0" stop-color="%s"/><stop offset="1" stop-color="%s"/>'
                 '</linearGradient>' % (grads[c], mix (c, WHITE, .28), mix (c, BLACK, .06)))
      return 'fill="url(#%s)"' % grads[c]
    body= re.sub (r'fill="(#[0-9A-Fa-f]{6})"', grad, body)
  body= ('<g transform="translate(12 12) scale(%g) translate(-12 -12)">%s</g>'
         % (SCALE, body))
  joins= ('stroke-linecap="round" stroke-linejoin="round"' if JOINS == "round" else
          'stroke-linecap="butt" stroke-linejoin="miter" stroke-miterlimit="4"')
  return ('<?xml version="1.0" encoding="UTF-8"?>\n'
          '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" '
          'viewBox="0 0 24 24">\n'
          + (' <defs>%s</defs>\n' % defs if defs else '') +
          ' <g %s>%s</g>\n'
          '</svg>\n' % (joins, body))

###############################################################################
# Shapes
###############################################################################

PAGE= ('<path class="paper" d="M5 2.75h9l4.75 4.75V20.5a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V3.75'
       'a1 1 0 0 1 1-1z"/><path class="o" d="M14 2.75V7.5h4.75"/>')
SHEET= '<rect class="paper" x="3.5" y="3" width="14" height="18.5" rx="1"/>'
FOLDER_BACK= ('<path class="khd" d="M3 20V7.5a1 1 0 0 1 1-1h4.25l1.5 1.75h9.25a1 1 0 0 1 1 1V20z"/>'
              '<rect class="paper" x="6" y="10" width="12" height="9" rx=".5"/>')
FOLDER_FRONT= '<path class="kh" d="M1.75 12.5h20.5l-2.25 8.5H4z"/>'
HAND=('<path class="skin" d="M13 7.25H4.25a1.75 1.75 0 0 0 0 3.5H10"/>'
      '<path class="skin" d="M10 7.5c1-1.3 2.2-2.2 3.8-2.2H17c1.1 0 2 .9 2 2v9.2c0 1.1-.9 2-2 2h-4.5'
      'c-1 0-1.8-.8-1.8-1.8c0-.9.7-1.6 1.6-1.6h-.8c-.9 0-1.6-.7-1.6-1.6s.7-1.6 1.6-1.6h-.2'
      'c-.9 0-1.6-.7-1.6-1.6"/>'
      '<rect class="skin" x="19" y="6.5" width="2.75" height="11.5" rx="1"/>')

UNDO= ('<path class="paper ow" d="M3.00 12.50 A9 9 0 1 0 5.64 6.14 L8.46 8.96 A5 5 0 1 1 7.00 12.50Z"/>'
       '<path class="paper" d="M8 7.5 5.5 1 .5 12h11z"/>'
       '<path class="paper nostroke" d="M3.00 12.50 A9 9 0 1 0 5.64 6.14 L8.46 8.96 A5 5 0 1 1 7.00 12.50Z"/>')

ICONS = [
 ("new", "Create a new document",
  '<rect class="paper" x="3.5" y="4.5" width="13" height="17" rx="1"/>'
  '<path class="g" d="M15.75 1.75h2.5v3.5h3.5v2.5h-3.5v3.5h-2.5v-3.5h-3.5v-2.5h3.5z"/>'),
 ("open", "Load a file",
  FOLDER_BACK + '<path class="g" d="M12 1.75 16.75 6.5h-3v6h-3.5v-6h-3z"/>' + FOLDER_FRONT),
 ("save", "Save this buffer",
  FOLDER_BACK + '<path class="r" d="M12 14.25 16.75 9.5h-3v-7.5h-3.5v7.5h-3z"/>' + FOLDER_FRONT),
 ("build", "Update this buffer",
  SHEET + '<g transform="rotate(45 13.5 9.5)"><rect class="br" x="12.4" y="11.5" width="2.2" height="10" rx="1"/>'
  '<rect class="dk" x="8" y="7.25" width="11" height="4.5" rx="1"/></g>'),
 ("print", "Print",
  '<rect class="paper" x="6.5" y="2" width="11" height="10" rx=".75"/>'
  '<path class="kh" d="M2.5 11.5h19v6a1.5 1.5 0 0 1-1.5 1.5H4a1.5 1.5 0 0 1-1.5-1.5z"/>'
  '<path class="khd" d="M5 15.5h14v5.5H5z"/><circle class="g" cx="18.25" cy="13.75" r=".9"/>'),
 ("preferences", "Change the TeXmacs preferences",
  '<g transform="rotate(45 12 12)"><g transform="translate(12 12)">'
  '<path class="met" d="M-1.5-9.9A4.4 4.4 0 1 0 1.5-9.9L1.5-7-1.5-7Z"/>'
  '<rect class="met" x="-1.5" y="-3" width="3" height="12.5" rx="1.5"/></g></g>'
  '<g transform="rotate(-45 12 12)"><g transform="translate(12 12)">'
  '<path class="met" d="M-.75-10.5h1.5V1h-1.5z"/>'
  '<rect class="kh" x="-2.25" y="1" width="4.5" height="9" rx="1.6"/></g></g>'),
 ("cancel", "Close",
  '<path class="kh" d="M3 3.5h11v17H3z"/><path class="khd" d="M6 6.5h5v11H6z"/>'
  '<path class="r" d="M22 12 16.5 6.75v3H9.5v4.5h7v3z"/>'),
 ("cut", "Cut text",
  PAGE + '<path class="grey nostroke" d="M6.5 6h4.5v2.5H9v3.5H6.5z"/>'
  '<rect class="dash" x="9" y="8.5" width="7.5" height="7" rx=".3"/>'),
 ("copy", "Copy text",
  PAGE + '<rect class="dash" x="6.5" y="6" width="6.5" height="6" rx=".3"/>'
  '<rect class="dash" x="9.5" y="10.5" width="6.5" height="6" rx=".3"/>'),
 ("paste", "Paste text",
  PAGE + '<rect class="dash" x="6.5" y="6" width="6.5" height="6" rx=".3"/>'
  '<path class="grey nostroke" d="M9 12h6.5v6H9z"/>'),
 ("find", "Find text",
  PAGE + '<path class="br" d="M13.4 15.9 17.6 20.1a1.1 1.1 0 0 0 1.6-1.6L15 14.3z"/>'
  '<circle class="lav" cx="11" cy="12" r="4.25"/>'),
 ("replace", "Query replace",
  PAGE + '<path class="ink emb" d="M11.86 13.07 8.98 5.43H8.02L5.14 13.07H5.96L6.81 10.82H10.01L10.84 13.07ZM9.77 10.21H7.04C7.6 8.63 7.19 9.79 7.75 8.22C7.98 7.57 8.32 6.63 8.4 6.23H8.41C8.43 6.38 8.51 6.65 8.76 7.38Z"/><path class="ink emb" d="M17.07 18.75C17.07 17.79 16.11 17.01 14.95 16.82C15.95 16.57 16.77 15.93 16.77 15.1C16.77 14.09 15.6 13.18 14.04 13.18H11.43V20.82H14.33C15.92 20.82 17.07 19.83 17.07 18.75ZM15.92 15.11C15.92 15.77 15.14 16.52 13.62 16.52H12.34V13.8H13.73C14.95 13.8 15.92 14.38 15.92 15.11ZM16.18 18.74C16.18 19.56 15.21 20.2 14.02 20.2H12.34V17.19H13.94C15.1 17.19 16.18 17.86 16.18 18.74Z"/><path class="rs" d="M8.25 13.25v1.75a1.5 1.5 0 0 0 1.5 1.5h1.75M10.25 14.75l1.75 1.75-1.75 1.75"/>'),
 ("spell", "Check text for spelling errors",
  PAGE + '<path class="br" d="M13.4 15.9 17.6 20.1a1.1 1.1 0 0 0 1.6-1.6L15 14.3z"/>'
  '<circle class="lav" cx="11" cy="12" r="4.25"/><path class="rs wide" d="M9.25 10.25l3.5 3.5M12.75 10.25l-3.5 3.5"/>'),
 ("undo", "Undo last changes",
  # as the original, in layers: the outline of a thick ring, the head, and
  # the body of the ring over the base of the head
  UNDO),
 ("redo", "Redo undone changes",
  '<g transform="matrix(-1 0 0 1 24 0)">%s</g>' % UNDO),
 ("back", "Browse back", HAND),
 ("reload", "Reload",
  # the two swooshes of the original
  '<path class="g" d="M1.5 9.5 7 17l5.5-7.5-3 1v-2c0-2 1-3.5 2.5-4s2.9-.16 4.5 0 5.5 1 5.5 1'
  '-3.78-1.88-6-2.5-5.5-1.5-8-.5-3.5 3-3.5 5v3z"/>'
  '<path class="dk" d="M22.5 14.5 17 7l-5.5 7.5 3-1v2c0 2-1 3.5-2.5 4s-2.9.16-4.5 0-5.5-1-5.5-1'
  ' 3.78 1.88 6 2.5 5.5 1.5 8 .5 3.5-3 3.5-5v-3z"/>'),
 ("forward", "Browse forward", '<g transform="matrix(-1 0 0 1 24 0)">%s</g>' % HAND),
]


###############################################################################
# The other icons: converted from the original ones
###############################################################################

# The originals (the icons of TeXmacs before the monochrome set, cleaned of
# editor metadata) are kept in originals/. Their shapes are kept as they are;
# their colours are mapped onto the palette of the set, their fills get the
# soft gradient, and a dark version is derived colour by colour.

ORIGINALS= os.path.join (os.path.dirname (os.path.abspath (__file__)), "originals")
KEEP= set ("""british bulgarian chinese croatian czech danish dutch english esperanto
  finnish french german greek hungarian italian japanese korean polish portuguese
  romanian russian slovak slovene spanish swedish taiwanese ukrainian""".split ())

# original colour -> palette key, for the colours of the original set
KNOWN= {
  "c0c0a0": "kh", "d8d8c0": "kh", "c4c4a0": "kh", "c0c0a3": "kh", "e0e0c0": "kh",
  "a0a080": "khd", "909070": "khd", "b8b8a0": "khd",
  "808060": "khl", "404030": "khl", "606040": "khl", "7f8060": "khl",
  "008000": "g", "009000": "g", "409040": "g", "60a060": "g", "50a050": "g",
  "006000": "gl", "306030": "gl", "408040": "gl",
  "800000": "r", "c04040": "r", "c00000": "r", "de1818": "r", "e61717": "r",
  "ff0000": "r", "cc0000": "r", "a06060": "r",
  "600000": "rl", "8f3636": "rl", "8e3737": "rl", "603030": "rl", "804040": "rl",
  "e0e0ff": "lav", "e8e8ff": "lav", "c0c0e0": "lav",
  "806040": "br", "81553a": "br", "c0a082": "br", "663d19": "brl", "604830": "brl",
  "fff1e8": "skin", "f8f8f8": "paper", "ffffff": "paper", "fffff6": "paper",
}
NEUTRAL_LINES= ("000000", "303032", "404040", "606060", "777777", "7f7f7f",
                "808080", "808081", "80807f", "828080", "909090", "90908f", "606260")

def expand (c):
  c= c.lower ()
  return c if len (c) == 6 else "".join (x + x for x in c)

def lightness (c):
  r, g, b= [int (c[i:i+2], 16) / 255 for i in (0, 2, 4)]
  return (max (r, g, b) + min (r, g, b)) / 2

def neutral (c):
  r, g, b= [int (c[i:i+2], 16) for i in (0, 2, 4)]
  return max (r, g, b) - min (r, g, b) < 24

def map_colour (c, kind, theme):
  # kind: "fill" or "stroke"; returns a colour of the set, for the theme
  c= expand (c)
  light= PAL["light"]
  if kind == "stroke" and c in NEUTRAL_LINES:
    col= outline (light, "line")
  elif c in KNOWN:
    col= light[KNOWN[c]]
    if kind == "stroke" and KNOWN[c] in ("kh", "khd", "g", "r", "lav", "br", "skin"):
      col= mix (col, BLACK, .35)
  elif kind == "stroke":
    col= mix ("#" + c, WHITE, .1)
  elif lightness (c) > .9:
    col= "#" + c
  else:
    col= mix ("#" + c, WHITE, .22)
  if theme == "dark":
    if kind == "stroke": col= mix (col, WHITE, .55)
    elif lightness (col[1:]) > .9: col= "#4B4D53"
    elif neutral (c) and lightness (c) < .5: col= mix (col, WHITE, .75)
    else: col= mix (col, TOOLBAR_DARK, .35)
  return col.upper ()

def convert (src, theme):
  s= open (src, encoding= "utf-8").read ()
  def rep (m):
    return "%s%s%s" % (m.group (1), m.group (2),
                       map_colour (m.group (3), "stroke" if m.group (1) == "stroke" else "fill", theme))
  s= re.sub (r'(fill|stroke|stop-color)(=\"|:\s*)#([0-9a-fA-F]{6}|[0-9a-fA-F]{3})\b', rep, s)
  if SHADING == "soft":
    grads= {}
    def grad (m):
      c= m.group (3)
      if lightness (c[1:].lower ()) > .92: return m.group (0)
      if c not in grads: grads[c]= "cg%d" % len (grads)
      return "%s%surl(#%s)" % (m.group (1), m.group (2), grads[c])
    s= re.sub (r'(fill)(=\"|:\s*)(#[0-9A-F]{6})', grad, s)
    if grads:
      defs= "".join ('<linearGradient id="%s" x1="0" y1="0" x2="0" y2="1">'
                     '<stop offset="0" stop-color="%s"/><stop offset="1" stop-color="%s"/>'
                     '</linearGradient>' % (i, mix (c, WHITE, .28), mix (c, BLACK, .06))
                     for c, i in grads.items ())
      s= re.sub (r"(<svg[^>]*>)", lambda m: m.group (1) + "<defs>" + defs + "</defs>", s, count= 1)
  # shapes without a fill are black by default: give the default explicitly
  s= re.sub (r"<svg\b", '<svg fill="%s"' % map_colour ("000000", "fill", theme), s, count= 1)
  return s

def original_names ():
  return sorted (f[3:-4] for f in os.listdir (ORIGINALS)
                 if f.startswith ("tm_") and f.endswith (".svg"))


def monochrome_drawings ():
  # the icons which never had an SVG original (the table toolbar only had
  # bitmaps) are taken from the drawings of the monochrome set
  import runpy
  here= os.path.dirname (os.path.abspath (__file__))
  return runpy.run_path (os.path.join (here, "..", "make-icons.py"), run_name= "icons")

def from_monochrome (M, body, theme):
  light= PAL["light"]
  p= dict (M["NEUTRAL"][theme])
  ink= outline (PAL[theme], "line")
  p.update (ink= ink, sec= light["kh"], metal= light["met"], body= light["kh"],
            folder= light["kh"], folderl= light["khl"], blue= "#6F86D6",
            red= light["r"], lens= light["lav"], on= WHITE, paper= "#FFFFFF")
  if theme == "dark":
    for k in ("sec", "metal", "body", "folder", "lens", "red", "blue"):
      p[k]= mix (p[k], TOOLBAR_DARK, .35)
    p["paper"]= "#4B4D53"
  text= M["svg"] (body, p)
  # selections in blue, as in the original table icons
  text= text.replace ('fill="%s" fill-opacity=".4"' % ink, 'fill="%s"' %
                      (mix ("#8FA2EA", TOOLBAR_DARK, .35) if theme == "dark" else "#A9B8F0"))
  return text

def main ():
  names= [a for a in sys.argv[1:] if not a.startswith ("--")]
  out= os.path.join ("TeXmacs", "misc", "pixmaps", "classic")
  drawn= set ()
  for name, tip, body in ICONS:
    drawn.add (name)
    if names and name not in names: continue
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      os.makedirs (d, exist_ok= True)
      with open (os.path.join (d, "tm_%s.svg" % name), "w") as f:
        f.write (svg (body, PAL[theme]))
  for name in original_names ():
    if name in drawn or (names and name not in names): continue
    src= os.path.join (ORIGINALS, "tm_%s.svg" % name)
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      os.makedirs (d, exist_ok= True)
      text= open (src, encoding= "utf-8").read () if name in KEEP else convert (src, theme)
      with open (os.path.join (d, "tm_%s.svg" % name), "w", encoding= "utf-8") as f:
        f.write (text)
  M= monochrome_drawings ()
  done= drawn | set (original_names ())
  for name, tip, body in M["ICONS"]:
    if name in done or (names and name not in names): continue
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      with open (os.path.join (d, "tm_%s.svg" % name), "w", encoding= "utf-8") as f:
        f.write (from_monochrome (M, body, theme))

if __name__ == "__main__":
  main ()
