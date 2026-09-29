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
#   ink    letters (emb: thickened)     b     blue (accents), bf: solid blue
#   ln lnb lng lnr   lines of text: dark, blue, green, red
#   wf wl  white fill, white line (on a coloured sign)
#   #RRGGBB  a literal colour (paint cards), with the outline
#   bold   a heavier near-black outline (icons shown small)
#   tagf tagm tagx  solid dark tag, its light plus and light cross (as the
#          originals); redx redf  red cross (line, fill)
#   thin wide ow  stroke width modifiers (ow: a double outline, half of it
#                 covered by a fill drawn on top)

import os, re, sys

STROKE= 1.0
LINE= 2.0              # lines of text
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
                skinl="#3C3C3C", grey="#BDBDBD", ink="#303032", white="#FFFFFF",
                b="#9AAAE8", bl="#5064B4"),
 "dark":  dict (line="#B4B4B8", paper="#4B4D53", kh="#9A9168", khl="#D6CDA0",
                khd="#7C7452", g="#3DB655", gl="#9BE3A8", r="#E0463C", rl="#F5A39C",
                lav="#4A4F7A", br="#9A7050", brl="#D8B89A", met="#6A6A70",
                metl="#C8C8CC", dk="#8A8A90", dkl="#D8D8DC", skin="#5A5048",
                skinl="#E4DCD4", grey="#7A7A80", ink="#E6E6EA", white="#FFFFFF",
                b="#5A6AA8", bl="#AFC0FF"),
}


WHITE, BLACK, TOOLBAR_DARK= "#FFFFFF", "#000000", "#2B2D31"
INK, INK_DARK= "#2A2A2C", "#E6E6EA"
GREY_DARK, GREY_MID= "#5E5E62", "#8C8C90"
RED_STRONG, RED_STRONG_DARK= "#C3342A", "#EE5145"
LINES= ("line", "khl", "gl", "rl", "brl", "metl", "dkl", "skinl", "bl")

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
                  skinl="#6A6660", grey="#D4D4D4", ink="#4A4A48", white="#FFFFFF",
                  b="#9AAAE8", bl="#5064B4")
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
           "skin": ("skin", "skinl"), "b": ("b", "bl") }
  for c, (f, s) in pairs.items ():
    if c in cl: fill= p[f]; stroke= outline (p, s)
  if "grey" in cl: fill= p["grey"]
  if "o" in cl: stroke= outline (p, "line")
  if "ink" in cl: fill= p["ink"]
  if "rs" in cl: stroke= p["r"]
  if "emb" in cl: stroke= p["ink"]; width= 0.6
  if "nostroke" in cl: stroke= "none"
  for c, k in (("ln", None), ("lnb", "bl"), ("lng", "g"), ("lnr", "rl")):
    if c in cl:
      stroke= p[k] if k else (INK_DARK if p["ink"] == "#E6E6EA" else INK); width= LINE
  if "bf" in cl: fill= p["bl"]; stroke= "none"
  if "wf" in cl: fill= WHITE; stroke= "none"
  dark= p["ink"] == "#E6E6EA"
  if "bold" in cl: stroke= INK_DARK if dark else INK; width= 1.6
  if "sig" in cl: fill= INK_DARK if dark else INK; stroke= fill; width= 1.1
  if "tagf" in cl: fill= "#8A8A90" if dark else "#58585D"; stroke= "#C6C6CC" if dark else INK
  if "tagm" in cl: stroke= "#E2F6E4"; width= 2.4
  if "tagx" in cl: stroke= "#FFD2CD"; width= 2.6
  if "redx" in cl: stroke= "#FF3B30" if dark else "#E8261B"; width= 2.6
  if "redf" in cl: fill= "#EE5145" if dark else "#D64A3E"; stroke= outline (p, "line")
  for c in cl:
    if re.match (r"^#[0-9A-Fa-f]{6}$", c):
      # a literal colour (paint cards): deeper on the dark toolbar
      fill= c if p["ink"] != "#E6E6EA" else mix (c, TOOLBAR_DARK, .3)
      stroke= outline (p, "line")
  if "screen" in cl: fill= "#2F3A33" if p["ink"] != "#E6E6EA" else "#1E2420"; stroke= outline (p, "line")
  if "wl" in cl: stroke= WHITE; width= 2.4
  if "ow" in cl: width= 2 * STROKE
  if "dash" in cl: stroke= p["ink"]; extra= ' stroke-dasharray="2 1.6"'
  if "thin" in cl: width= STROKE * .8
  if "wide" in cl: width= max (2.2, STROKE * 1.5)
  if "emb" in cl: width= 0.6
  a= 'fill="%s"' % fill
  if stroke != "none": a += ' stroke="%s" stroke-width="%g"' % (stroke, width)
  return a + extra

FLAT= set ()                # icons drawn without gradients (the focus toolbar)

def svg (body, p, flat= False):
  body= re.sub (r'class="([^"]*)"', lambda m: attributes (m.group (1), p), body)
  defs= ""
  if SHADING == "soft" and not flat:
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

# The text mode toolbar, redrawn after the originals: lines of text with blue
# accents, without a page.
TEXT_ICONS = [
 ('title', 'Enter title information',
  '<rect class="b" x="6.5" y="3" width="11" height="3.5" rx=".6"/><path class="ln" d="M3 10.5H21M3 14H21M3 17.5H21M3 21H14"/>'),
 ('chapter', 'Start a new chapter',
  '<rect class="b" x="3" y="3" width="10" height="4" rx=".6"/><path class="ln" d="M3 10.5H21M3 14H21M3 17.5H21M3 21H14"/>'),
 ('section', 'Start a new section',
  '<path class="ln" d="M3 3.5H21M3 7H17"/><rect class="b" x="3" y="10" width="8" height="3" rx=".6"/><path class="ln" d="M3 16.5H21M3 20H15"/>'),
 ('block', 'Insert a section block',
  '<rect class="paper" x="3" y="3" width="18" height="18" rx="1.5"/><path class="b" d="M4.5 3h15A1.5 1.5 0 0 1 21 4.5V7.5H3V4.5A1.5 1.5 0 0 1 4.5 3z"/><path class="ln thin" d="M6 11H18M6 14.5H18M6 18H14"/>'),
 ('theorem', 'Insert an enunciation',
  '<path class="bf" d="M6.82 2.79V2.72C6.82 2.41 6.75 2.26 6.36 2.26H1.08C0.74 2.26 0.62 2.37 0.62 2.72V2.79C0.62 3.25 0.85 3.25 1.12 3.25L3.03 3.22V8.34C3.03 8.68 3.13 8.8 3.48 8.8H3.97C4.33 8.8 4.42 8.67 4.42 8.34V3.22L6.32 3.25C6.59 3.25 6.82 3.25 6.82 2.79ZM13.88 8.34V2.66C13.88 2.36 13.81 2.2 13.42 2.2H12.94C12.57 2.2 12.48 2.33 12.48 2.66V4.92H9.47V2.66C9.47 2.36 9.4 2.2 9.01 2.2H8.53C8.19 2.2 8.07 2.31 8.07 2.66V8.34C8.07 8.68 8.18 8.8 8.53 8.8H9.01C9.37 8.8 9.47 8.67 9.47 8.34V5.81H12.48V8.34C12.48 8.64 12.55 8.8 12.94 8.8H13.42C13.78 8.8 13.88 8.67 13.88 8.34Z"/><path class="ln" d="M13.5 5.5H21M3 10H21M3 14H21M3 18H21M3 21.5H13"/>'),
 ('prominent', 'Insert a prominent piece of text',
  '<rect class="grey o" x="2.5" y="4" width="19" height="16" rx="2"/><path class="lnb" d="M6 8.5H18M6 12H18M6 15.5H14"/>'),
 ('var_prominent', 'Insert a prominent piece of text',
  '<path class="lnb" d="M8 4.5H21M8 12H21M8 19.5H15"/><path class="ln" d="M3 8.25H21M3 15.75H21"/>'),
 ('program', 'Insert a computer program',
  '<path class="lnb" d="M3 4.5H8.5M6.5 13.5H10.5"/><path class="ln" d="M10.5 4.5H16M6.5 9H19.5M12.5 13.5H20M6.5 18H14M3 21H8"/>'),
 ('list', 'Insert a list',
  '<circle class="paper" cx="5" cy="6" r="2"/><circle class="paper" cx="5" cy="12" r="2"/><circle class="paper" cx="5" cy="18" r="2"/><path class="ln" d="M9.5 6H21M9.5 12H21M9.5 18H17"/>'),
 ('footnote', 'Insert a footnote',
  '<path class="ln" d="M3 3.5H21M3 7H21M3 10.5H16"/><path class="ln thin" d="M3 14H9"/><path class="lnb" d="M3 17H21M3 20.5H17"/>'),
 ('margin', 'Insert a marginal note',
  '<path class="ln" d="M3 3.5H15M3 7H15M3 10.5H15M3 14H15M3 17.5H15M3 21H11"/><path class="lnb" d="M17.5 3.5H21M17.5 7H21"/>'),
 ('floating', 'Insert a floating object',
  '<path class="ln" d="M3 3.5H21M3 20.5H21"/><rect class="paper" x="5.5" y="6.5" width="13" height="11" rx="1"/><path class="g" d="M6.5 16.5l3.5-4.5 2.5 3 2-2.5 3 4z"/><circle class="r" cx="15.25" cy="9.75" r="1.25"/>'),
 ('multicol', 'Start multicolumn context',
  '<path class="ln" d="M3 3.5H10.5M3 7H10.5M3 10.5H10.5M3 14H10.5M3 17.5H10.5M3 21H8.5M13.5 3.5H21M13.5 7H21M13.5 10.5H21M13.5 14H21M13.5 17.5H19"/>'),
 ('pageins', 'Insert a note or a floating object',
  '<path class="ln" d="M3 3.5H21M3 20.5H21"/><rect class="b" x="3" y="7.5" width="12" height="9" rx="2"/><path class="ln" d="M17.5 9.5H21M17.5 14.5H21"/>'),
 ('index', 'Insert automatically generated content',
  '<path class="ln" d="M3 4H9M5 9.5H10M5 15H11M3 20.5H8.5"/><path class="dash" d="M10.5 4H17M11.5 9.5H17M12.5 15H17M10 20.5H17"/><path class="lnb" d="M18.5 4H21M18.5 9.5H21M18.5 15H21M18.5 20.5H21"/>'),
 ('parstyle', 'Set paragraph mode',
  '<path class="ln" d="M3 3.5H21M3 7.5H21M3 11.5H15"/><path class="lnb" d="M4.5 17.5h15M7 15l-2.5 2.5L7 20M17 15l2.5 2.5L17 20"/>'),
 ('align_left', 'Align text to the left',
  '<path class="ln" d="M3 4.5H21M3 9.5H15M3 14.5H19M3 19.5H12"/>'),
 ('align_center', 'Center text',
  '<path class="ln" d="M3 4.5H21M6.5 9.5H17.5M4.5 14.5H19.5M7.5 19.5H16.5"/>'),
 ('align_right', 'Align text to the right',
  '<path class="ln" d="M3 4.5H21M9 9.5H21M5 14.5H21M12 19.5H21"/>'),
 ('align_justify', 'Justify text',
  '<path class="ln" d="M3 4.5H21M3 9.5H21M3 14.5H21M3 19.5H14"/>'),
 ('parindent', 'Set paragraph margins',
  '<path class="ln" d="M10 4.5H21M3 9.5H21M3 14.5H21M3 19.5H15"/><path class="lnb" d="M3 4.5h4.5M5.5 2.5l2 2-2 2"/>'),
]
ICONS= ICONS + TEXT_ICONS


# The focus toolbar: the icons whose originals look dated, redrawn.
FOCUS_ICONS = [
 ('go', 'Go',
  '<circle class="g" cx="12" cy="12" r="9.5"/><path class="wf" d="M9.75 7.5v9l7.25-4.5z"/>'),
 ('stop', 'Stop',
  '<path class="r" d="M8.2 2.5h7.6l5.7 5.7v7.6l-5.7 5.7H8.2l-5.7-5.7V8.2z"/><path class="wl" d="M7.5 12h9"/>'),
 ('customized', 'Customized',
  '<circle class="skin" cx="12" cy="7" r="4"/><path class="b" d="M4 21.5c0-4.6 3.6-8 8-8s8 3.4 8 8z"/>'),
 ('stateless', 'Stateless',
  '<rect class="dash" x="2.5" y="2.5" width="19" height="19" rx="3"/><path class="ink" d="M15.93 7.8C15.93 5.35 12.55 5.35 11.85 5.35C9.21 5.35 8.07 6.55 8.07 7.76C8.07 8.67 8.77 8.98 9.25 8.98C9.85 8.98 10.44 8.56 10.44 7.78C10.44 6.89 9.62 6.64 9.59 6.62C10.08 6.26 10.86 6.03 11.73 6.03C13.48 6.03 13.5 6.77 13.5 7.67C13.5 8.5 13.35 8.81 12.97 9.19C11.79 10.42 11.26 11.89 11.26 13.12V13.84C11.26 14.26 11.26 14.36 11.7 14.36C12.15 14.36 12.15 14.24 12.15 13.79V13.27C12.15 11.32 13.92 10.16 14.64 9.74C15.02 9.51 15.93 9 15.93 7.8ZM13.18 17.17C13.18 16.35 12.51 15.69 11.7 15.69C10.88 15.69 10.21 16.35 10.21 17.17C10.21 17.98 10.88 18.65 11.7 18.65C12.51 18.65 13.18 17.98 13.18 17.17Z"/>'),
 ('theme', 'Theme',
  '<circle class="paper" cx="12" cy="12" r="9"/><path class="ink" d="M12 3a9 9 0 0 1 0 18z"/>'),
 ('show_hidden', 'Show hidden',
  '<rect class="paper" x="3" y="3" width="18" height="18" rx="1"/><path class="ink" d="M3 3h18L3 21z"/>'),
 ('focus_style', 'Style',
  '<path class="paper" d="M1.75 12S5.5 5 12 5s10.25 7 10.25 7S18.5 19 12 19 1.75 12 1.75 12z"/><circle class="lav" cx="12" cy="12" r="4.25"/><circle class="ink" cx="12" cy="12" r="1.9"/>'),
 ('view', 'View',
  '<path class="paper" d="M1.75 12S5.5 5 12 5s10.25 7 10.25 7S18.5 19 12 19 1.75 12 1.75 12z"/><circle class="lav" cx="12" cy="12" r="4.25"/><circle class="ink" cx="12" cy="12" r="1.9"/>'),
 ('like', 'Cite TeXmacs',
  '<rect class="b" x="2.5" y="10.5" width="4.5" height="10.5" rx="1"/><path class="skin" d="M7 11.5 10.6 4.4c.4-.8 1.4-1.2 2.2-.8.9.4 1.3 1.3 1 2.2L12.9 9.5h5.5a2 2 0 0 1 2 2.4l-1.3 7a2 2 0 0 1-2 1.6H7z"/>'),
 ('lock_closed', 'Locked',
  '<path class="dk" d="M6 11V7.5a6 6 0 0 1 12 0V11h-3V7.5a3 3 0 0 0-6 0V11z"/><rect class="r" x="2.5" y="10" width="19" height="12" rx="1.5"/><circle class="ink" cx="12" cy="16" r="1.75"/>'),
 ('lock_open', 'Unlocked',
  '<path class="dk" d="M6 11V7.5a6 6 0 0 1 11.6-2.2l-2.8 1.1A3 3 0 0 0 9 7.5V11z"/><rect class="g" x="2.5" y="10" width="19" height="12" rx="1.5"/><circle class="ink" cx="12" cy="16" r="1.75"/>'),
 ('shell', 'Start an interactive session',
  '<path class="kh" d="M9.5 17.5h5l.75 3h-6.5z"/><rect class="kh" x="6" y="20" width="12" height="2" rx=".75"/><rect class="kh" x="1.75" y="2.5" width="20.5" height="15.5" rx="2"/><rect class="screen" x="3.75" y="4.5" width="16.5" height="11.5" rx=".75"/><path class="lng" d="M6.25 7.5 9 10.25 6.25 13M10.5 13.25h4.5"/>'),
 ('link', 'Insert a link',
  '<path class="met" fill-rule="evenodd" transform="rotate(45 8.4 8.4)" d="M6.40 3.90h4.00a4.50 4.50 0 0 1 0 9.00h-4.00a4.50 4.50 0 0 1 0 -9.00zM6.40 6.60h4.00a1.80 1.80 0 0 1 0 3.60h-4.00a1.80 1.80 0 0 1 0 -3.60z"/><path class="met" fill-rule="evenodd" transform="rotate(45 15.6 15.6)" d="M13.60 11.10h4.00a4.50 4.50 0 0 1 0 9.00h-4.00a4.50 4.50 0 0 1 0 -9.00zM13.60 13.80h4.00a1.80 1.80 0 0 1 0 3.60h-4.00a1.80 1.80 0 0 1 0 -3.60z"/><path class="met" transform="rotate(45 8.4 8.4)" d="M10.40 3.90a4.50 4.50 0 0 1 0 9.00L10.40 10.20a1.80 1.80 0 0 0 0 -3.60z"/>'),
 ('animate', 'Animation',
  '<rect class="#9A9CA2" x="4.25" y="1.5" width="15.5" height="21" rx="1.5"/><rect class="wf" x="5.1" y="3" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="3" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="5.1" y="6.6" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="6.6" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="5.1" y="10.2" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="10.2" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="5.1" y="13.8" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="13.8" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="5.1" y="17.4" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="17.4" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="5.1" y="20.8" width="1.5" height="1.7" rx=".3"/><rect class="wf" x="17.4" y="20.8" width="1.5" height="1.7" rx=".3"/><rect class="paper" x="7.5" y="2.4" width="9" height="6" rx=".6"/><rect class="paper" x="7.5" y="9" width="9" height="6" rx=".6"/><rect class="paper" x="7.5" y="15.6" width="9" height="6" rx=".6"/><path class="#F2C94C" d="M10.50 2.80L11.15 4.51L12.97 4.60L11.55 5.74L12.03 7.50L10.50 6.50L8.97 7.50L9.45 5.74L8.03 4.60L9.85 4.51Z"/><path class="#F2C94C" d="M12.00 9.40L12.65 11.11L14.47 11.20L13.05 12.34L13.53 14.10L12.00 13.10L10.47 14.10L10.95 12.34L9.53 11.20L11.35 11.11Z"/><path class="#F2C94C" d="M13.50 16.00L14.15 17.71L15.97 17.80L14.55 18.94L15.03 20.70L13.50 19.70L11.97 20.70L12.45 18.94L11.03 17.80L12.85 17.71Z"/>'),
 ('color', 'Select a foreground color',
  '<path class="#E6C38E" d="M12 2.75c5.3 0 9.5 3.7 9.5 8.15 0 3-2.3 4.5-4.45 4.5h-1.75c-1.15 0-1.8.95-1.35 2 .6 1.25.35 3.85-2.15 3.85-5.3 0-9.3-4.1-9.3-9.1S6.7 2.75 12 2.75z"/><circle class="#EC6A5E" cx="7.2" cy="13" r="2.3"/><circle class="#F2C94C" cx="9.3" cy="7.3" r="2.3"/><circle class="#6F9BEA" cx="15.3" cy="6.9" r="2.3"/>'),
 ('image', 'Insert a picture',
  '<rect class="#E2F0F4 nostroke" x="2.5" y="4" width="19" height="16" rx="2.2"/><path class="#C9DFB2 nostroke" d="M3.2 14.3c4.2 1.2 10.3 1.3 17.6-.9v4.4a1.5 1.5 0 0 1-1.5 1.5H4.7a1.5 1.5 0 0 1-1.5-1.5z"/><g transform="translate(2.75 4.25) scale(.95 1.05)"><path class="#8E6443 nostroke" d="m4.2143 12.882h1.7857l1.1905-8.8825h-1.1905z"/><path class="#5E8F2F nostroke" d="m6 3.9991s0.5-2.4871 2-3.1089c1.5-0.62178 4.7452-0.96626 4.7452-0.96626s-3.1481 1.519-4.1593 2.2098c-1.0112 0.69077-1.6496 3.0813-1.6496 3.0813z"/><path class="#5E8F2F nostroke" d="m6 3.9991h1.1905s-0.75527-2.2021-1.7857-2.9608c-1.0304-0.75873-4.1667-0.74021-4.1667-0.74021s2.6048 1.5049 3.5389 2.1406c0.93409 0.63574 1.223 1.5605 1.223 1.5605z"/><path class="#5E8F2F nostroke" d="m6 3.9991 0.94564 0.89866s1.5891-0.50081 2.5302-0.34408c0.94112 0.15673 3.2729 1.2427 3.2729 1.2427s-1.6632-2.5305-2.7314-2.8544c-1.0682-0.32391-4.0174 1.0571-4.0174 1.0571z"/><path class="#5E8F2F nostroke" d="m6.723 4.8069 0.46749-0.80781s-2.1923-0.91609-3.3903-0.62137-3.0618 3.1193-3.0618 3.1193 2.5691-1.4465 3.5801-1.6505 2.4044-0.039615 2.4044-0.039615z"/></g><rect class="o" x="2.5" y="4" width="19" height="16" rx="2.2"/>'),
 ('exit_image', 'Exit graphics mode',
  '<g transform="translate(.4 -.9) scale(.74 1.06)"><rect class="#E2F0F4 nostroke" x="2.5" y="4" width="19" height="16" rx="2.2"/><path class="#C9DFB2 nostroke" d="M3.2 14.3c4.2 1.2 10.3 1.3 17.6-.9v4.4a1.5 1.5 0 0 1-1.5 1.5H4.7a1.5 1.5 0 0 1-1.5-1.5z"/><g transform="translate(2.75 4.25) scale(.95 1.05)"><path class="#8E6443 nostroke" d="m4.2143 12.882h1.7857l1.1905-8.8825h-1.1905z"/><path class="#5E8F2F nostroke" d="m6 3.9991s0.5-2.4871 2-3.1089c1.5-0.62178 4.7452-0.96626 4.7452-0.96626s-3.1481 1.519-4.1593 2.2098c-1.0112 0.69077-1.6496 3.0813-1.6496 3.0813z"/><path class="#5E8F2F nostroke" d="m6 3.9991h1.1905s-0.75527-2.2021-1.7857-2.9608c-1.0304-0.75873-4.1667-0.74021-4.1667-0.74021s2.6048 1.5049 3.5389 2.1406c0.93409 0.63574 1.223 1.5605 1.223 1.5605z"/><path class="#5E8F2F nostroke" d="m6 3.9991 0.94564 0.89866s1.5891-0.50081 2.5302-0.34408c0.94112 0.15673 3.2729 1.2427 3.2729 1.2427s-1.6632-2.5305-2.7314-2.8544c-1.0682-0.32391-4.0174 1.0571-4.0174 1.0571z"/><path class="#5E8F2F nostroke" d="m6.723 4.8069 0.46749-0.80781s-2.1923-0.91609-3.3903-0.62137-3.0618 3.1193-3.0618 3.1193 2.5691-1.4465 3.5801-1.6505 2.4044-0.039615 2.4044-0.039615z"/></g><rect class="o" x="2.5" y="4" width="19" height="16" rx="2.2"/></g><path class="#D64A3E" d="M23.25 12 18 6.5v3.25h-5.25v4.5H18v3.25z"/>'),
 ('enter_image', 'Enter graphics mode',
  '<g transform="translate(.4 -.9) scale(.74 1.06)"><rect class="#E2F0F4 nostroke" x="2.5" y="4" width="19" height="16" rx="2.2"/><path class="#C9DFB2 nostroke" d="M3.2 14.3c4.2 1.2 10.3 1.3 17.6-.9v4.4a1.5 1.5 0 0 1-1.5 1.5H4.7a1.5 1.5 0 0 1-1.5-1.5z"/><g transform="translate(2.75 4.25) scale(.95 1.05)"><path class="#8E6443 nostroke" d="m4.2143 12.882h1.7857l1.1905-8.8825h-1.1905z"/><path class="#5E8F2F nostroke" d="m6 3.9991s0.5-2.4871 2-3.1089c1.5-0.62178 4.7452-0.96626 4.7452-0.96626s-3.1481 1.519-4.1593 2.2098c-1.0112 0.69077-1.6496 3.0813-1.6496 3.0813z"/><path class="#5E8F2F nostroke" d="m6 3.9991h1.1905s-0.75527-2.2021-1.7857-2.9608c-1.0304-0.75873-4.1667-0.74021-4.1667-0.74021s2.6048 1.5049 3.5389 2.1406c0.93409 0.63574 1.223 1.5605 1.223 1.5605z"/><path class="#5E8F2F nostroke" d="m6 3.9991 0.94564 0.89866s1.5891-0.50081 2.5302-0.34408c0.94112 0.15673 3.2729 1.2427 3.2729 1.2427s-1.6632-2.5305-2.7314-2.8544c-1.0682-0.32391-4.0174 1.0571-4.0174 1.0571z"/><path class="#5E8F2F nostroke" d="m6.723 4.8069 0.46749-0.80781s-2.1923-0.91609-3.3903-0.62137-3.0618 3.1193-3.0618 3.1193 2.5691-1.4465 3.5801-1.6505 2.4044-0.039615 2.4044-0.039615z"/></g><rect class="o" x="2.5" y="4" width="19" height="16" rx="2.2"/></g><path class="#D64A3E" d="M9.5 12 15 6.75v3h8v4.5h-8v3z"/>'),
 ('camera', 'Take a snapshot',
  '<path class="#5E5E62" d="M8.75 7 9.9 4.9c.25-.45.7-.65 1.2-.65h1.8c.5 0 .95.2 1.2.65L15.25 7z"/><rect class="#5E5E62" x="2" y="6.75" width="20" height="13.5" rx="2.75"/><circle class="#C8C8CC" cx="12" cy="13.5" r="4.6"/><circle class="lav" cx="12" cy="13.5" r="2.6"/><circle class="wf" cx="18.5" cy="9.5" r=".9"/>'),
 ('insert_right', 'Insert',
  '<g><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagm" d="M10.5 8v8M6.5 12h8"/></g>'),
 ('delete_right', 'Delete',
  '<g><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagx" d="M7.5 8.5l6 7M13.5 8.5l-6 7"/></g>'),
 ('insert_left', 'Insert',
  '<g transform="matrix(-1 0 0 1 24 0)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagm" d="M10.5 8v8M6.5 12h8"/></g>'),
 ('delete_left', 'Delete',
  '<g transform="matrix(-1 0 0 1 24 0)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagx" d="M7.5 8.5l6 7M13.5 8.5l-6 7"/></g>'),
 ('insert_up', 'Insert',
  '<g transform="rotate(-90 12 12)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagm" d="M10.5 8v8M6.5 12h8"/></g>'),
 ('delete_up', 'Delete',
  '<g transform="rotate(-90 12 12)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagx" d="M7.5 8.5l6 7M13.5 8.5l-6 7"/></g>'),
 ('insert_down', 'Insert',
  '<g transform="rotate(90 12 12)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagm" d="M10.5 8v8M6.5 12h8"/></g>'),
 ('delete_down', 'Delete',
  '<g transform="rotate(90 12 12)"><path class="tagf" d="M4.5 4h8.75l7.5 8-7.5 8H4.5A1.5 1.5 0 0 1 3 18.5v-13A1.5 1.5 0 0 1 4.5 4z"/><path class="tagx" d="M7.5 8.5l6 7M13.5 8.5l-6 7"/></g>'),
 ('focus_delete', 'Delete',
  '<path class="redf" d="M5 7.25 7.25 5 12 9.75 16.75 5 19 7.25 14.25 12 19 16.75 16.75 19 12 14.25 7.25 19 5 16.75 9.75 12z"/>'),
 ('entry_remove', 'Remove the entry',
  '<rect class="paper" x="2" y="3" width="17" height="14" rx="1.5"/><path class="b" d="M3.5 3h14A1.5 1.5 0 0 1 19 4.5V7H2V4.5A1.5 1.5 0 0 1 3.5 3z"/><path class="ln thin" d="M4.5 10.5h9M4.5 13.5h6"/><path class="redx" d="M13.5 13.5l8 8M21.5 13.5l-8 8"/>'),
 ('prefs_general', 'Preferences',
  '<rect class="paper bold" x="1.5" y="2.5" width="14" height="11" rx="1.2"/><path class="b bold" d="M2.7 2.5h11.6a1.2 1.2 0 0 1 1.2 1.2V6h-14V3.7a1.2 1.2 0 0 1 1.2-1.2z"/><rect class="paper bold" x="8.5" y="10" width="14" height="11" rx="1.2"/><path class="b bold" d="M9.7 10h11.6a1.2 1.2 0 0 1 1.2 1.2V13.5h-14v-2.3a1.2 1.2 0 0 1 1.2-1.2z"/>'),
 ('prefs_keyboard', 'Preferences',
  '<rect class="kh bold" x="1.5" y="6.5" width="21" height="13" rx="2"/><rect class="ink nostroke" x="4.5" y="9.5" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="8.5" y="9.5" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="12.5" y="9.5" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="16.5" y="9.5" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="4.5" y="13" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="8.5" y="13" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="12.5" y="13" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="16.5" y="13" width="2.6" height="2.4" rx=".5"/><rect class="ink nostroke" x="6.5" y="16.3" width="11" height="1.9" rx=".5"/>'),
 ('prefs_convert', 'Preferences',
  '<rect class="paper bold" x="1.5" y="2.5" width="10" height="13" rx="1"/><rect class="kh bold" x="12.5" y="8.5" width="10" height="13" rx="1"/><path class="g bold" d="M5.5 10.5h7V7l6 5.25-6 5.25V14h-7z"/>'),
 ('prefs_other', 'Preferences',
  '<path class="met bold" d="M10.16 4.63L10.44 2.12L13.56 2.12L13.84 4.63L15.91 5.49L17.88 3.91L20.09 6.12L18.51 8.09L19.37 10.16L21.88 10.44L21.88 13.56L19.37 13.84L18.51 15.91L20.09 17.88L17.88 20.09L15.91 18.51L13.84 19.37L13.56 21.88L10.44 21.88L10.16 19.37L8.09 18.51L6.12 20.09L3.91 17.88L5.49 15.91L4.63 13.84L2.12 13.56L2.12 10.44L4.63 10.16L5.49 8.09L3.91 6.12L6.12 3.91L8.09 5.49Z"/><circle class="paper bold" cx="12" cy="12" r="3.3"/>'),
 ('prefs_security', 'Preferences',
  '<circle class="kh bold" cx="7" cy="12" r="5.25"/><circle class="paper bold" cx="6" cy="12" r="1.6"/><path class="kh bold" d="M12 10.5h10v3h-2v3h-3v-3h-2v2h-3z"/>'),
 ('math_preferences', 'Preferences for editing mathematical formulas',
  '<path class="met bold" d="M6.21 2.67L6.35 1.00L8.65 1.00L8.79 2.67L10.00 3.17L11.29 2.09L12.91 3.71L11.83 5.00L12.33 6.21L14.00 6.35L14.00 8.65L12.33 8.79L11.83 10.00L12.91 11.29L11.29 12.91L10.00 11.83L8.79 12.33L8.65 14.00L6.35 14.00L6.21 12.33L5.00 11.83L3.71 12.91L2.09 11.29L3.17 10.00L2.67 8.79L1.00 8.65L1.00 6.35L2.67 6.21L3.17 5.00L2.09 3.71L3.71 2.09L5.00 3.17Z"/><circle class="paper bold" cx="7.5" cy="7.5" r="2.1"/><path class="ink sig" d="M23.38 20.26H23.05C22.73 21.24 21.86 22.03 20.75 22.41C20.55 22.47 19.65 22.79 17.74 22.79H12.21L16.72 17.21C16.81 17.09 16.84 17.05 16.84 17C16.84 16.95 16.83 16.93 16.75 16.82L12.52 11.02H17.67C19.15 11.02 22.14 11.11 23.05 13.54H23.38L22.26 10.5H11.48C11.13 10.5 11.12 10.51 11.12 10.92L15.87 17.44L11.25 23.15C11.15 23.28 11.13 23.3 11.13 23.36C11.13 23.5 11.25 23.5 11.48 23.5H22.26Z"/>'),
]
ICONS= ICONS + FOCUS_ICONS



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
  "ff0000": "r", "cc0000": "r", "a06060": "r", "9f5f5f": "r", "810000": "r",
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
  elif c in KNOWN and KNOWN[c] == "r" and kind == "fill":
    col= RED_STRONG             # small red marks: strong and flat
  elif c in KNOWN:
    col= light[KNOWN[c]]
    if kind == "stroke":
      # coloured outlines: a deep tone of their colour
      col= mix (col, BLACK, .62)
  elif kind == "stroke" and neutral (c):
    # lighter grey lines of the originals (grids, borders, small marks) must
    # still read on the toolbar
    col= "#6E6E72" if lightness (c) < .8 else "#8C8C90"
  elif kind == "stroke":
    col= mix ("#" + c, BLACK, .55 if lightness (c) > .4 else .12)
  elif neutral (c) and lightness (c) < .35:
    col= INK                    # letters and symbols: solid ink
  elif neutral (c) and lightness (c) < .6:
    col= GREY_DARK              # grey bars and borders
  elif neutral (c) and lightness (c) < .8:
    col= GREY_MID               # grids and light marks
  elif lightness (c) > .9:
    col= "#" + c
  else:
    col= mix ("#" + c, WHITE, .22)
  if theme == "dark":
    if kind == "stroke": col= mix (col, WHITE, .55)
    elif lightness (col[1:]) > .9: col= "#4B4D53"
    elif col == INK: col= INK_DARK
    elif col == RED_STRONG: col= RED_STRONG_DARK
    elif col == GREY_DARK: col= "#B8B8BC"
    elif col == GREY_MID: col= "#8E8E94"
    elif neutral (c) and lightness (c) < .5: col= mix (col, WHITE, .75)
    else: col= mix (col, TOOLBAR_DARK, .35)
  return col.upper ()

def convert (src, theme, flat= False):
  s= open (src, encoding= "utf-8").read ()
  def rep (m):
    return "%s%s%s" % (m.group (1), m.group (2),
                       map_colour (m.group (3), "stroke" if m.group (1) == "stroke" else "fill", theme))
  s= re.sub (r'(fill|stroke|stop-color)(=\"|:\s*)#([0-9a-fA-F]{6}|[0-9a-fA-F]{3})\b', rep, s)
  # outlines are solid (some originals halve them with an opacity)
  s= re.sub (r'\sstroke-opacity="[^"]*"', "", s)
  s= re.sub (r'stroke-opacity:\s*[0-9.]+;?', "", s)
  if SHADING == "soft" and not flat:
    grads= {}
    def grad (m):
      c= m.group (3)
      if lightness (c[1:].lower ()) > .92 or c in (INK, INK_DARK, GREY_DARK, GREY_MID, "#B8B8BC", "#8E8E94",
                                                   RED_STRONG, RED_STRONG_DARK):
        return m.group (0)
      if c not in grads: grads[c]= "cg%d" % len (grads)
      return "%s%surl(#%s)" % (m.group (1), m.group (2), grads[c])
    s= re.sub (r'(fill)(=\"|:\s*)(#[0-9A-F]{6})', grad, s)
    if grads:
      defs= "".join ('<linearGradient id="%s" x1="0" y1="0" x2="0" y2="1">'
                     '<stop offset="0" stop-color="%s"/><stop offset="1" stop-color="%s"/>'
                     '</linearGradient>' % (i, mix (c, WHITE, .28), mix (c, BLACK, .06))
                     for c, i in grads.items ())
      s= re.sub (r"(<svg[^>]*>)", lambda m: m.group (1) + "<defs>" + defs + "</defs>", s, count= 1)
  if flat:
    # gradients of the originals become the colour of their first stop
    stops= {}
    for m in re.finditer (r'<(?:linear|radial)Gradient\b([^>]*)>(.*?)</(?:linear|radial)Gradient>|'
                          r'<(?:linear|radial)Gradient\b([^>]*)/>', s, re.S):
      attrs= m.group (1) or m.group (3) or ""
      gid= re.search (r'\bid="([^"]+)"', attrs)
      if not gid: continue
      first= re.search (r'stop-color[=:]"?\s*(#[0-9A-Fa-f]{6})', m.group (2) or "")
      href= re.search (r'href="#([^"]+)"', attrs)
      stops[gid.group (1)]= first.group (1) if first else ("@" + href.group (1) if href else None)
    def solid (gid, depth= 0):
      v= stops.get (gid)
      if v and v.startswith ("@") and depth < 4: return solid (v[1:], depth + 1)
      return v
    s= re.sub (r'url\(#([^)]+)\)', lambda m: solid (m.group (1)) or m.group (0), s)
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
  M= monochrome_drawings ()
  # icons of the focus bar: those drawn at 16 pixels in the original set,
  # the focus toolbar itself, and the table icons which only had bitmaps
  focus_dir= os.path.join ("TeXmacs", "misc", "pixmaps", "modern", "16x16", "focus")
  FLAT.update (f[3:-4] for f in os.listdir (focus_dir)
               if f.startswith ("tm_") and f.endswith (".png") and "_x" not in f)
  FLAT.update (n for n, t, b in M["FOCUS_ICONS"])
  FLAT.update (n for n, t, b in M["TABLE_MORE_ICONS"])
  FLAT.update (("prefs_general", "prefs_keyboard", "prefs_convert", "prefs_other",
                "prefs_security", "math_preferences"))
  out= os.path.join ("TeXmacs", "misc", "pixmaps", "classic")
  drawn= set ()
  for name, tip, body in ICONS:
    drawn.add (name)
    if names and name not in names: continue
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      os.makedirs (d, exist_ok= True)
      with open (os.path.join (d, "tm_%s.svg" % name), "w") as f:
        f.write (svg (body, PAL[theme], name in FLAT))
  for name in original_names ():
    if name in drawn or (names and name not in names): continue
    src= os.path.join (ORIGINALS, "tm_%s.svg" % name)
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      os.makedirs (d, exist_ok= True)
      text= (open (src, encoding= "utf-8").read () if name in KEEP
             else convert (src, theme, name in FLAT))
      with open (os.path.join (d, "tm_%s.svg" % name), "w", encoding= "utf-8") as f:
        f.write (text)
  done= drawn | set (original_names ())
  for name, tip, body in M["ICONS"]:
    if name in done or (names and name not in names): continue
    for theme in ("light", "dark"):
      d= os.path.join (out, theme)
      with open (os.path.join (d, "tm_%s.svg" % name), "w", encoding= "utf-8") as f:
        f.write (from_monochrome (M, body, theme))

if __name__ == "__main__":
  main ()
