#!/usr/bin/env python3
###############################################################################
# MODULE     : make-icons.py
# DESCRIPTION: Source of the TeXmacs toolbar icons, and their generation
# COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# Each icon is drawn once, on a 24x24 grid, in the manner of the macOS
# symbols; this script writes it, by default in the monochrome variant, with
# the light and the dark palettes into
# TeXmacs/misc/pixmaps/{light,dark}/tm_<name>.svg, and the PNG fallbacks
# (1x, 2x, 4x, from the light version) used by Qt builds without an SVG
# renderer into TeXmacs/misc/pixmaps/modern/24x24/main.
#
# Usage (from src/): misc/icons/make-icons.py [options] [names...]
#   --png            also write the PNG fallbacks
#   --tint=#RRGGBB   tint of the monochrome icons (default: graphite)
#   --colour         the colour variant instead, with
#   --accent=#RRGGBB its accent colour (default: the macOS system blue)
#   --out=DIR        write DIR/{light,dark}/tm_<name>.svg instead
# The PNGs need rsvg-convert.
#
# Drawings are written with classes, which the script turns into
# presentation attributes, since the Qt SVG renderer ignores style sheets.
# Nothing is painted in the colour of the toolbar, which varies (and the Qt
# renderer has no masks): overlaps are separated by outlines or paper rings.
#   o       outline in ink               paper   fill of white paper
#   sec     secondary grey fill          lens    fill of a magnifier lens
#   folder  blue folder, with its line   folderl line of a folder
#   fb fm   fills accent, metal          sb sr   strokes accent, red
#   fa      fill of a body: the accent, a light tone in monochrome
#   wh      stroke on a badge            paperf  paper fill (ring of a badge)
#   thin wide        stroke width modifiers
# In the monochrome variant everything is in the tint and its light tones;
# in the colour variant the accent marks the part that acts. In both, red
# marks an error (the cross of the spell checker) and nothing else.

import os, re, subprocess, sys

PAGE='M6.5 2.75h7.25l4.5 4.5v12.5c0 .83-.67 1.5-1.5 1.5H6.5c-.83 0-1.5-.67-1.5-1.5V4.25c0-.83.67-1.5 1.5-1.5z'
FOLD='M13.75 2.75v3c0 .83.67 1.5 1.5 1.5h3'
FOLDER='M3 6.25c0-.97.78-1.75 1.75-1.75h3.9c.52 0 1.01.23 1.34.63L11.5 6.75h7.75c.97 0 1.75.78 1.75 1.75v9.75c0 .97-.78 1.75-1.75 1.75H4.75C3.78 20 3 19.22 3 18.25z'
def badge(cls, glyph):
  return (f'<circle class="paperf" cx="17.5" cy="17.5" r="6.25"/><circle class="{cls}" cx="17.5" cy="17.5" r="5"/>'
          f'<path class="wh" d="{glyph}"/>')
HAND=('<path class="paper o" d="M13 7.25H4.25a1.75 1.75 0 0 0 0 3.5H10"/>'
      '<path class="paper o" d="M10 7.5c1-1.3 2.2-2.2 3.8-2.2H17c1.1 0 2 .9 2 2v9.2c0 1.1-.9 2-2 2h-4.5c-1 0-1.8-.8-1.8-1.8'
      'c0-.9.7-1.6 1.6-1.6h-.8c-.9 0-1.6-.7-1.6-1.6s.7-1.6 1.6-1.6h-.2c-.9 0-1.6-.7-1.6-1.6"/>'
      '<rect class="paper o" x="19" y="6.5" width="2.75" height="11.5" rx="1"/>')
ICONS = [
 ("new","Create a new document",
  f'<path class="paper o" d="{PAGE}"/><path class="o" d="{FOLD}"/>'+badge("fb","M17.5 15.1v4.8M15.1 17.5h4.8")),
 ("open","Load a file",
  f'<path class="folder" d="{FOLDER}"/>'
  '<path class="folder" d="M3 20 5.55 11.3c.14-.47.57-.8 1.06-.8h14.4c.55 0 .94.53.78 1.05L19.4 19.1c-.18.54-.68.9-1.25.9z"/>'),
 ("save","Save this buffer",
  '<path class="fa o" d="M5.5 3.25h10.9l4.35 4.35v10.9a2.25 2.25 0 0 1-2.25 2.25H5.5a2.25 2.25 0 0 1-2.25-2.25V5.5A2.25 2.25 0 0 1 5.5 3.25z"/>'
  '<path class="fm o" d="M7.75 3.25h8v5h-8z"/><path class="paper" d="M12.75 4.5h1.75v2.5h-1.75z"/>'
  '<rect class="paper o" x="6.5" y="12.25" width="11" height="8.5" rx=".75"/>'
  '<path class="o thin" d="M8.75 15h6.5M8.75 17.5h6.5"/>'),
 ("build","Update this buffer",
  '<g transform="rotate(45 14.5 8.5)"><rect class="sec o" x="13.4" y="10.75" width="2.2" height="12.25" rx="1.1"/>'
  '<rect class="fm o" x="8.5" y="5.75" width="12" height="5" rx="1.4"/></g>'),
 ("print","Print",
  '<path class="paper o" d="M7 9V4.25C7 3.56 7.56 3 8.25 3h7.5c.69 0 1.25.56 1.25 1.25V9"/>'
  '<path class="sec o" d="M6.5 17H5a2 2 0 0 1-2-2v-4.25C3 9.78 3.78 9 4.75 9h14.5c.97 0 1.75.78 1.75 1.75V15a2 2 0 0 1-2 2h-1.5"/>'
  '<path class="paper o" d="M7 13.5h10v6.25c0 .69-.56 1.25-1.25 1.25h-7.5C7.56 21 7 20.44 7 19.75z"/>'
  '<path class="o thin" d="M9.5 16.5h5M9.5 18.5h3.5"/><circle class="fb" cx="17.75" cy="11.4" r="1"/>'),
 ("preferences","Change the TeXmacs preferences",
  '<path class="fm o" d="M10.08 4.85L10.35 2.64L13.65 2.64L13.92 4.85L15.70 5.59L17.45 4.22L19.78 6.55L18.41 8.30L19.15 10.08L21.36 10.35L21.36 13.65L19.15 13.92L18.41 15.70L19.78 17.45L17.45 19.78L15.70 18.41L13.92 19.15L13.65 21.36L10.35 21.36L10.08 19.15L8.30 18.41L6.55 19.78L4.22 17.45L5.59 15.70L4.85 13.92L2.64 13.65L2.64 10.35L4.85 10.08L5.59 8.30L4.22 6.55L6.55 4.22L8.30 5.59Z"/>'
  '<circle class="paper o" cx="12" cy="12" r="3.2"/>'),
 ("cancel","Close",
  '<rect class="paper" x="3.5" y="3" width="10.5" height="18" rx="2"/>'
  '<path class="o" d="M14 8.5V5a2 2 0 0 0-2-2H5.5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2H12a2 2 0 0 0 2-2v-3.5"/>'
  '<path class="sb" d="M9 12h11.5M17.25 8.75 20.5 12l-3.25 3.25"/>'),
 ("cut","Cut text",
  '<path class="o" d="M8.6 15.3 17.8 3M15.4 15.3 6.2 3"/><circle class="fm" cx="12" cy="10.7" r="1"/>'
  '<circle class="sb paperf" cx="6.5" cy="17.5" r="3"/><circle class="sb paperf" cx="17.5" cy="17.5" r="3"/>'),
 ("copy","Copy text",
  '<path class="sec o" d="M8.5 6.5V4.5c0-.83.67-1.5 1.5-1.5h6l4 4v9c0 .83-.67 1.5-1.5 1.5H16"/>'
  '<path class="paper o" d="M5.5 6.5h6l4 4v9c0 .83-.67 1.5-1.5 1.5H5.5c-.83 0-1.5-.67-1.5-1.5V8c0-.83.67-1.5 1.5-1.5z"/>'
  '<path class="o" d="M11.5 6.5V9c0 .83.67 1.5 1.5 1.5h2.5"/><path class="sb thin" d="M6.75 14h6M6.75 17h4.5"/>'),
 ("paste","Paste text",
  '<rect class="sec o" x="4" y="4" width="16" height="18" rx="2"/>'
  '<rect class="paper o" x="6.75" y="7.5" width="10.5" height="12" rx=".8"/>'
  '<path class="sb thin" d="M9 11.5h6M9 14h6M9 16.5h3.5"/>'
  '<rect class="fm o" x="8.5" y="2.5" width="7" height="3.5" rx="1.2"/>'),
 ("find","Find text",
  f'<path class="paper o" d="{PAGE}"/><path class="o" d="{FOLD}"/><path class="o thin" d="M8 10h5M8 13h3"/>'
  '<circle class="lens o" cx="15" cy="15" r="3.9"/>'
  '<path class="o wide" d="M17.9 17.9 20.8 20.8"/>'),
 ("replace","Query replace",
  '<path class="o" d="M3.25 11.5 6.75 2.75l3.5 8.75M4.7 8.1h4.1"/>'
  '<path class="o" d="M14 12.75h3.3a2.05 2.05 0 0 1 0 4.1H14zM14 16.85h3.8a2.08 2.08 0 0 1 0 4.15H14z"/>'
  '<path class="sb" d="M6.75 14v2.25c0 1.1.9 2 2 2h2.75M10.25 16l2.25 2.25-2.25 2.25"/>'),
 ("spell","Check text for spelling errors",
  '<circle class="lens o" cx="10.25" cy="10.25" r="6.5"/><path class="o wide" d="M15.1 15.1 20.5 20.5"/>'
  '<path class="sr" d="M8 8l4.5 4.5M12.5 8 8 12.5"/>'),
 ("undo","Undo last changes",
  '<path class="o" d="M9 14.5 4 9.5l5-5"/><path class="o" d="M4 9.5h10.25a5.25 5.25 0 0 1 0 10.5H11"/>'),
 ("redo","Redo undone changes",
  '<path class="o" d="M15 14.5l5-5-5-5"/><path class="o" d="M20 9.5H9.75a5.25 5.25 0 0 0 0 10.5H13"/>'),
 ("back","Browse back", HAND),
 ("reload","Reload",
  '<path class="o" d="M20 12a8 8 0 1 1-2.34-5.66L20 8.5"/><path class="o" d="M20 3.5v5h-5"/>'),
 ("forward","Browse forward", f'<g transform="matrix(-1 0 0 1 24 0)">{HAND}</g>'),
]
GROUPS = [["new","open","save","build","print","preferences","cancel"],
          ["cut","copy","paste","find","replace","spell","undo","redo"],
          ["back","reload","forward"]]
ACCENT= "#007AFF"      # the macOS system blue
TINT= "#3A3A3C"        # ink of the monochrome variant: graphite
RED= {"light": "#FF3B30", "dark": "#FF453A"}
WHITE= "#FFFFFF"

NEUTRAL = {
 "light": dict (ink="#1D1D1F", bg="#F3F3F3", paper="#FFFFFF", sec="#D8D8DD",
                metal="#8E8E93", on="#FFFFFF"),
 "dark":  dict (ink="#E6E6EA", bg="#2B2D31", paper="#45474D", sec="#5A5C63",
                metal="#9A9AA0", on="#FFFFFF"),
}

def mix (a, b, t):
  # the colour a fraction t of the way from a to b
  ca= [int (a[i:i+2], 16) for i in (1, 3, 5)]
  cb= [int (b[i:i+2], 16) for i in (1, 3, 5)]
  return "#%02X%02X%02X" % tuple (round (x + (y - x) * t) for x, y in zip (ca, cb))

def palette (theme, accent= ACCENT):
  # the colour variant: neutral objects, the accent for what acts, and
  # folders and lenses in lighter tones of the accent
  p= dict (NEUTRAL[theme]); p["red"]= RED[theme]
  p["body"]= accent if theme == "light" else mix (accent, WHITE, .08)
  if theme == "light":
    p.update (blue= accent, folder= mix (accent, WHITE, .45),
              folderl= mix (accent, WHITE, .2), lens= mix (accent, WHITE, .86))
  else:
    p.update (blue= mix (accent, WHITE, .08), folder= mix (accent, WHITE, .3),
              folderl= mix (accent, WHITE, .6), lens= mix (accent, p["bg"], .65))
  return p

def monochrome (theme, tint= TINT):
  # the monochrome variant, the default: the same drawings in one tint and
  # its tones, red being kept for what signals an error
  p= dict (NEUTRAL[theme])
  if theme == "light":
    ink= tint; sec= mix (tint, WHITE, .8)
  else:
    ink= mix (tint, WHITE, .8); sec= mix (tint, p["paper"], .5)
  p.update (ink= ink, sec= sec, metal= sec, body= sec, folder= sec, folderl= ink,
            blue= ink, red= RED[theme], lens= p["paper"], on= p["paper"])
  return p

STROKE= 1.6

def attributes (cls, p):
  cl= cls.split ()
  fill= "none"; stroke= "none"; width= STROKE
  if "o" in cl: stroke= p["ink"]
  for c, k in (("paper","paper"),("paperf","paper"),("sec","sec"),("lens","lens"),
               ("fb","blue"),("fa","body"),("fm","metal")):
    if c in cl: fill= p[k]
  if "folder" in cl: fill= p["folder"]; stroke= p["folderl"]
  for c, k in (("folderl","folderl"),("sb","blue"),("sr","red")):
    if c in cl: stroke= p[k]
  if "wh" in cl: stroke= p["on"]; width= 1.7
  if "thin" in cl: width= 1.2
  if "wide" in cl: width= 2.4
  a= 'fill="%s"' % fill
  if stroke != "none": a += ' stroke="%s" stroke-width="%g"' % (stroke, width)
  return a

def svg (body, p):
  body= re.sub (r'class="([^"]*)"', lambda m: attributes (m.group (1), p), body)
  return ('<?xml version="1.0" encoding="UTF-8"?>\n'
          '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" '
          'viewBox="0 0 24 24">\n'
          ' <g stroke-linecap="round" stroke-linejoin="round">%s</g>\n'
          '</svg>\n' % body)

def option (args, name, default):
  for a in args:
    if a.startswith ("--%s=" % name): return a[len (name) + 3:]
  return default

def main ():
  args= sys.argv[1:]
  png= "--png" in args
  colour= "--colour" in args
  accent= option (args, "accent", ACCENT)
  tint= option (args, "tint", TINT)
  pix= os.path.join ("TeXmacs", "misc", "pixmaps")
  out= option (args, "out", pix)
  names= [a for a in args if not a.startswith ("--")]
  for name, tip, body in ICONS:
    if names and name not in names: continue
    for theme in ("light", "dark"):
      p= palette (theme, accent) if colour else monochrome (theme, tint)
      d= os.path.join (out, theme)
      os.makedirs (d, exist_ok= True)
      with open (os.path.join (d, "tm_%s.svg" % name), "w") as f:
        f.write (svg (body, p))
    if png and out == pix:
      src= os.path.join (pix, "light", "tm_%s.svg" % name)
      main_dir= os.path.join (pix, "modern", "24x24", "main")
      for tag, size in (("", 24), ("_x2", 48), ("_x4", 96)):
        dest= os.path.join (main_dir, "tm_%s%s.png" % (name, tag))
        subprocess.run (["rsvg-convert", "-w", str (size), "-h", str (size),
                         src, "-o", dest], check= True)

if __name__ == "__main__":
  main ()
