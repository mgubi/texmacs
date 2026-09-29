#!/usr/bin/env python3
# The logo of TeXmacs Vue: the chip of the logo of TeXmacs, seen from the
# front, and on it the mirrored Sigma of TeX Gyre Pagella Bold, embossed;
# a light outline lifts the dark chip off a dark background.
#
#   python3 misc/icons/vue-logo/make-vue-logo.py
#
# from the top of the source tree, writes TeXmacs/misc/images/texmacs-vue.svg
# (the logo), texmacs-vue-small.svg (for 32 px and less: three wider pins a
# side, a larger Sigma, a thicker outline) and texmacs-vue-<n>.png. It needs
# fontTools and rsvg-convert; the files it writes are in the tree, so that
# the build needs neither.

import os, subprocess
from fontTools.ttLib import TTFont
from fontTools.pens.svgPathPen import SVGPathPen
from fontTools.pens.boundsPen import BoundsPen
from fontTools.pens.transformPen import TransformPen

FONT = 'TeXmacs/fonts/truetype/texgyre/texgyrepagella-bold.otf'
OUT = 'TeXmacs/misc/images'
SIZES_SMALL = (16, 32)
SIZES = (48, 64, 128, 192, 256, 512)

f = TTFont(FONT); gs = f.getGlyphSet(); name = f.getBestCmap()[0x03A3]
bp = BoundsPen(gs); gs[name].draw(bp); X0, Y0, X1, Y1 = bp.bounds

def sigma(cx, cy, h):
    # the Sigma of height h centred on (cx, cy), mirrored (x -> -x), with y
    # downwards
    s = h / (Y1 - Y0); w = (X1 - X0) * s
    pen = SVGPathPen(gs)
    gs[name].draw(TransformPen(pen, (-s, 0, 0, -s, cx + w / 2 + X0 * s, cy + h / 2 + Y0 * s)))
    return pen.getCommands()

BODY = 'x="9.5" y="8.5" width="45" height="47" rx="6"'
HALO = '#E6E9ED'

def logo(small):
    # the pins, copper, on the left and the right sides of the body
    if small: ys, pin = [16 + i * 13 for i in range(3)], 'width="6" height="7" rx="1.4"'
    else:     ys, pin = [14.5 + i * 10 for i in range(4)], 'width="6" height="5" rx="1.2"'
    at = [(x, y) for y in ys for x in (4.5, 53.5)]
    pins = ''.join(f'<rect x="{x}" y="{y}" {pin} fill="url(#pin)" stroke="#7E3B2C" stroke-width="1"/>' for x, y in at)
    # the light outline around the chip: the body and the pins, stroked wide
    hw = 3.4 if small else 2.6
    halo = (''.join(f'<rect x="{x}" y="{y}" {pin} fill="{HALO}" stroke="{HALO}" stroke-width="{hw}"/>' for x, y in at)
            + f'<rect {BODY} fill="{HALO}" stroke="{HALO}" stroke-width="{hw + 1.5}"/>')
    d = sigma(32, 32, 31 if small else 28)
    return f'''<?xml version="1.0" encoding="UTF-8"?>
<!-- The logo of TeXmacs Vue, made by misc/icons/vue-logo/make-vue-logo.py:
     the Sigma is the one of TeX Gyre Pagella Bold, mirrored -->
<svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" viewBox="0 0 64 64">
 <defs>
  <linearGradient id="body" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#4B4B50"/><stop offset="1" stop-color="#232326"/></linearGradient>
  <linearGradient id="glyph" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#FFFFFF"/><stop offset="1" stop-color="#BFC0C4"/></linearGradient>
  <linearGradient id="pin" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#F2A98C"/><stop offset="1" stop-color="#D2694F"/></linearGradient>
 </defs>
 <g stroke-linejoin="round">{halo}</g>
 {pins}
 <rect {BODY} fill="url(#body)" stroke="#161618" stroke-width="1.5"/>
 <rect x="11.5" y="10.5" width="41" height="43" rx="4.5" fill="none" stroke="#FFFFFF" stroke-opacity="0.10" stroke-width="1"/>
 <path d="{d}" transform="translate(0.8 1.1)" fill="#000000" fill-opacity="0.55"/>
 <path d="{d}" fill="url(#glyph)" stroke="#0E0E10" stroke-width="0.6" stroke-linejoin="round"/>
</svg>
'''

def render(svg, n):
    out = os.path.join(OUT, f'texmacs-vue-{n}.png')
    subprocess.run(['rsvg-convert', '-w', str(n), '-h', str(n), svg, '-o', out], check=True)
    return out

big = os.path.join(OUT, 'texmacs-vue.svg')
small = os.path.join(OUT, 'texmacs-vue-small.svg')
open(big, 'w').write(logo(False))
open(small, 'w').write(logo(True))
for n in SIZES_SMALL: print(render(small, n))
for n in SIZES: print(render(big, n))
