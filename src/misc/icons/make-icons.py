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
# renderer, next to the existing ones in TeXmacs/misc/pixmaps/modern.
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
#   gl      solid ink: letters, bars     dots    dotted line (leaders)
#   emb     outline thickening a letter
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
# The mode toolbar in text mode (structure, paragraphs, text styles) and the
# insertion icons shared by the modes. Letters are Latin Modern outlines.
TEXT_ICONS = [
 ('title', 'Enter title information',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><rect class="gl" x="7.5" y="5.25" width="9" height="2.3" rx=".6"/><path class="o thin" d="M9.5 9.75H14.5M7 13.5H17M7 16H17M7 18.5H15"/>'),
 ('chapter', 'Start a new chapter',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="gl emb" d="M11.45 12.18V11.62H9.77V4.7C9.77 4.44 9.77 4.32 9.46 4.32C9.33 4.32 9.31 4.32 9.2 4.4C8.27 5.09 7.04 5.09 6.79 5.09H6.55V5.65H6.79C6.98 5.65 7.64 5.64 8.35 5.41V11.62H6.68V12.18C7.21 12.14 8.48 12.14 9.07 12.14C9.65 12.14 10.93 12.14 11.45 12.18Z"/><path class="o thin" d="M12 10.75H17M7 14H17M7 16.5H17M7 19H14"/>'),
 ('section', 'Start a new section',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="o thin" d="M7 5.75H17M7 8.25H14.5"/><rect class="gl" x="7" y="10.75" width="6.5" height="1.9" rx=".6"/><path class="o thin" d="M7 15H17M7 17.5H15"/>'),
 ('block', 'Insert a section block',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><rect class="gl" x="7" y="5" width="5.5" height="1.8" rx=".6"/><rect class="sec o thin" x="6.5" y="9" width="11" height="10" rx="1"/><path class="o thin" d="M8.5 12H15.5M8.5 14.5H15.5M8.5 17H13.5"/>'),
 ('theorem', 'Insert an enunciation',
  '<path class="gl emb" d="M8.67 5.77 8.4 2.64H-0.35L-0.62 5.77H-0.01C0.1 4.3 0.23 3.25 2.11 3.25H3.11V10.8H1.12V11.41C1.82 11.37 3.26 11.37 4.03 11.37C4.8 11.37 6.24 11.37 6.94 11.41V10.8H4.95V3.25H5.94C7.8 3.25 7.93 4.29 8.06 5.77ZM17.22 11.41V10.8H16.33V7.43C16.33 6.07 15.63 5.56 14.33 5.56C13.08 5.56 12.41 6.32 12.13 6.81H12.12V2.39L9.81 2.49V3.1C10.62 3.1 10.71 3.1 10.71 3.61V10.8H9.81V11.41L11.45 11.37L13.09 11.41V10.8H12.19V8.08C12.19 6.67 13.31 6.03 14.13 6.03C14.57 6.03 14.85 6.3 14.85 7.29V10.8H13.95V11.41L15.59 11.37Z"/><path class="o" d="M16.5 11H21M3 14.5H21M3 17.75H21M3 21H15"/>'),
 ('prominent', 'Insert a prominent piece of text',
  '<rect class="sec o" x="2.75" y="4.25" width="18.5" height="15.5" rx="2.5"/><path class="o thin" d="M6.5 8.5H17.5M6.5 12H17.5M6.5 15.5H13.5"/>'),
 ('var_prominent', 'Insert a prominent piece of text',
  '<rect class="gl" x="3" y="4.5" width="2.2" height="15" rx="1.1"/><path class="o thin" d="M8.5 6.5H20.5M8.5 10.25H20.5M8.5 14H20.5M8.5 17.75H16"/>'),
 ('program', 'Insert a computer program',
  '<path class="o" d="M3 5H13.5M6.5 9H18M10 13H20.5M6.5 17H15M3 21H7.5"/>'),
 ('list', 'Insert a list',
  '<circle class="gl" cx="5" cy="6" r="1.6"/><circle class="gl" cx="5" cy="12" r="1.6"/><circle class="gl" cx="5" cy="18" r="1.6"/><path class="o" d="M9 6H20.5M9 12H20.5M9 18H17"/>'),
 ('footnote', 'Insert a footnote',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="o thin" d="M7 5.75H17M7 8.25H17M7 10.75H14.5M7 14H11M8.75 16.25H17M7 18.5H15"/><circle class="gl" cx="7.3" cy="16.1" r=".8"/>'),
 ('margin', 'Insert a marginal note',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="o thin" d="M6.5 5.75H13.5M6.5 8.25H13.5M6.5 10.75H13.5M6.5 13.25H13.5M6.5 15.75H13.5M6.5 18.25H11.5"/><path class="o" d="M15.5 5.75H17.75M15.5 8.25H17.75"/>'),
 ('floating', 'Insert a floating object',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="o thin" d="M7 5.5H17M7 19H15"/><rect class="sec o thin" x="7" y="8" width="10" height="8.25" rx=".75"/><path class="o thin" d="M7.5 15.25l3-3.25 2.5 2.25 1.5-1.25 2 2"/>'),
 ('multicol', 'Start multicolumn context',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><path class="o thin" d="M6.5 6H11M6.5 8.5H11M6.5 11H11M6.5 13.5H11M6.5 16H11M6.5 18.5H9.5M13 6H17.5M13 8.5H17.5M13 11H17.5M13 13.5H17.5M13 16H15.5"/>'),
 ('pageins', 'Insert a note or a floating object',
  '<rect class="paper o" x="4" y="2.5" width="16" height="19" rx="2"/><rect class="sec o thin" x="6.75" y="5" width="10.5" height="4" rx=".75"/><path class="o thin" d="M7 11.5H17M7 14H17M7 16.5H17M7 19H13"/>'),
 ('index', 'Insert automatically generated content',
  '<path class="o" d="M3 5H9M3 10H11M5 15H9.5M3 20H10"/><path class="dots" d="M11.25 5H17.75M13.25 10H17.75M11.75 15H17.75M12.25 20H17.75"/><path class="o" d="M19.5 5H21M19.5 10H21M19.5 15H21M19.5 20H21"/>'),
 ('parstyle', 'Set paragraph mode',
  '<path class="o" d="M3 4.5H21M3 8.5H21M3 12.5H15"/><path class="o" d="M4 18h16M6.5 15.5 4 18l2.5 2.5M17.5 15.5 20 18l-2.5 2.5"/>'),
 ('align_left', 'Align text to the left',
  '<path class="o" d="M3 5.5H21M3 9.5H15M3 13.5H19M3 17.5H12"/>'),
 ('align_center', 'Center text',
  '<path class="o" d="M3 5.5H21M6.5 9.5H17.5M4.5 13.5H19.5M7.5 17.5H16.5"/>'),
 ('align_right', 'Align text to the right',
  '<path class="o" d="M3 5.5H21M9 9.5H21M5 13.5H21M12 17.5H21"/>'),
 ('align_justify', 'Justify text',
  '<path class="o" d="M3 5.5H21M3 9.5H21M3 13.5H21M3 17.5H14"/>'),
 ('parindent', 'Set paragraph margins',
  '<path class="o" d="M10 5.5H21M3 9.5H21M3 13.5H21M3 17.5H15"/><path class="o thin" d="M3 5.5h4.5M5.75 3.75 7.5 5.5 5.75 7.25"/>'),
 ('emphasize', 'Emphasize text',
  '<path class="gl emb" d="M17.98 13.95C17.98 13.72 17.73 13.72 17.66 13.72C17.43 13.72 17.41 13.74 17.22 14.23C16 17.12 15.27 18.49 11.93 18.49H9.37C8.72 18.49 8.72 18.43 8.72 18.28C8.72 18.15 8.78 17.94 8.8 17.82L10.25 12.04H12.08C13.5 12.04 13.65 12.38 13.65 12.95C13.65 13.09 13.63 13.41 13.46 14.02C13.46 14.02 13.42 14.18 13.42 14.25C13.42 14.5 13.63 14.5 13.76 14.5C13.99 14.5 14.05 14.46 14.13 14.08L15.29 9.46C15.31 9.4 15.35 9.19 15.35 9.19C15.35 8.93 15.1 8.93 15.02 8.93C14.74 8.93 14.74 9 14.62 9.44C14.22 10.93 13.82 11.39 12.12 11.39H10.42L11.7 6.22C11.87 5.53 12.01 5.51 12.71 5.51H15.33C17.6 5.51 18.12 6.14 18.12 7.63C18.12 8.2 18.08 8.47 18.02 8.98C18 9.08 17.98 9.27 17.98 9.33C17.98 9.59 18.21 9.59 18.29 9.59C18.59 9.59 18.61 9.5 18.65 9.12L19.13 5.41C19.2 4.86 19.07 4.86 18.63 4.86H8.82C8.46 4.86 8.25 4.86 8.25 5.24C8.25 5.51 8.4 5.51 8.82 5.51C10.04 5.51 10.04 5.68 10.04 5.87C10.04 5.87 10.04 6.04 9.96 6.35L7.14 17.61C6.97 18.32 6.85 18.49 5.42 18.49C5.08 18.49 4.85 18.49 4.85 18.89C4.85 19.14 5.04 19.14 5.38 19.14H15.44C15.86 19.14 15.88 19.12 16.02 18.8L17.89 14.21C17.91 14.16 17.98 13.95 17.98 13.95Z"/>'),
 ('strong', 'Write strong text',
  '<path class="gl emb" d="M17.36 14.94C17.36 12.61 15.65 10.91 13.74 10.51L10.7 9.86C9.86 9.67 8.68 8.95 8.68 7.67C8.68 6.77 9.27 5.47 11.37 5.47C13.05 5.47 15.17 6.18 15.65 9.04C15.74 9.54 15.74 9.58 16.18 9.58C16.68 9.58 16.68 9.48 16.68 9V5.15C16.68 4.75 16.68 4.57 16.3 4.57C16.14 4.57 16.12 4.59 15.89 4.8L14.94 5.72C13.72 4.75 12.36 4.57 11.35 4.57C8.16 4.57 6.65 6.58 6.65 8.79C6.65 10.15 7.34 11.12 7.78 11.58C8.81 12.61 9.52 12.76 11.81 13.26C13.66 13.66 14.02 13.72 14.48 14.16C14.79 14.48 15.32 15.02 15.32 15.99C15.32 17 14.77 18.45 12.59 18.45C10.99 18.45 7.8 18.03 7.63 14.9C7.61 14.52 7.61 14.41 7.15 14.41C6.65 14.41 6.65 14.54 6.65 15.02V18.85C6.65 19.25 6.65 19.43 7.02 19.43C7.21 19.43 7.25 19.39 7.42 19.25L8.39 18.28C9.77 19.31 11.73 19.43 12.59 19.43C16.05 19.43 17.36 17.06 17.36 14.94Z"/>'),
 ('verbatim', 'Write verbatim text',
  '<path class="gl emb" d="M17.37 5.85C17.37 5.19 16.8 5.19 16.47 5.19H14.22C13.89 5.19 13.32 5.19 13.32 5.85C13.32 6.53 13.78 6.53 14.55 6.53L12.86 13.27C12.57 14.43 12.15 16.1 12 17.05H11.98C11.87 16.28 11.78 15.9 11.58 15.09L9.45 6.53C10.22 6.53 10.68 6.53 10.68 5.85C10.68 5.19 10.11 5.19 9.78 5.19H7.53C7.2 5.19 6.63 5.19 6.63 5.85C6.63 6.53 7.09 6.53 7.89 6.53L10.79 17.97C11.01 18.81 11.36 18.81 12 18.81C12.59 18.81 13.01 18.81 13.21 17.99L16.11 6.53C16.91 6.53 17.37 6.53 17.37 5.85Z"/>'),
 ('sansserif', 'Use a sans serif font',
  '<path class="gl emb" d="M16.78 15.32C16.78 13.95 16.13 12.94 15.66 12.44C14.68 11.39 13.98 11.2 12.05 10.72C10.83 10.42 10.5 10.34 9.87 9.79C9.72 9.67 9.13 9.06 9.13 8.14C9.13 6.9 10.27 5.64 12.2 5.64C13.96 5.64 14.97 6.33 15.75 6.98L16.06 5.3C14.91 4.61 13.75 4.25 12.22 4.25C9.3 4.25 7.47 6.31 7.47 8.39C7.47 9.29 7.77 10.17 8.61 11.05C9.49 12 10.41 12.25 11.65 12.55C13.44 12.99 13.65 13.05 14.24 13.57C14.66 13.93 15.12 14.62 15.12 15.53C15.12 16.91 13.96 18.3 12.05 18.3C11.19 18.3 9.3 18.09 7.54 16.6L7.22 18.3C9.07 19.45 10.75 19.75 12.07 19.75C14.85 19.75 16.78 17.63 16.78 15.32Z"/>'),
 ('name', 'Write a name',
  '<path class="gl emb" d="M12.65 6.1V5.51L10.42 5.57L8.2 5.51V6.1C10.16 6.1 10.16 6.97 10.16 7.54V15.7L3.11 5.78C2.92 5.53 2.9 5.51 2.5 5.51H-0.71V6.1C-0.18 6.1 1.17 6.1 1.23 6.27C1.25 6.31 1.25 6.33 1.25 6.61V16.46C1.25 17.03 1.25 17.9 -0.71 17.9V18.49L1.51 18.43L3.73 18.49V17.9C1.78 17.9 1.78 17.03 1.78 16.46V6.54L10.08 18.26C10.23 18.49 10.33 18.49 10.42 18.49C10.69 18.49 10.69 18.36 10.69 17.96V7.54C10.69 6.97 10.69 6.1 12.65 6.1ZM24.71 18.49V18.01C23.65 18.01 23.48 17.92 23.23 17.25L19.96 8.82C19.85 8.51 19.75 8.4 19.54 8.4C19.28 8.4 19.24 8.51 19.14 8.76L16.09 16.61C15.88 17.12 15.55 17.98 14.36 18.01V18.49C14.77 18.45 15.32 18.43 15.74 18.43L17.32 18.49V18.01C16.52 17.98 16.46 17.33 16.46 17.16C16.46 17.01 16.46 16.97 17.11 15.28H21.04L21.46 16.34C21.61 16.72 21.86 17.37 21.86 17.48C21.86 18.01 21.18 18.01 20.85 18.01V18.49L22.87 18.43C23.51 18.43 24.25 18.47 24.71 18.49ZM20.85 14.8H17.3L19.09 10.22Z"/>'),
 ('italic', 'Write italic text',
  '<path class="gl emb" d="M13.24 18.77C13.24 18.52 13.07 18.52 12.65 18.52C11.35 18.52 11.35 18.37 11.35 18.14C11.35 18.14 11.35 18 11.43 17.68L14.27 6.34C14.44 5.67 14.56 5.48 16.01 5.48C16.45 5.48 16.66 5.48 16.66 5.08C16.66 4.83 16.45 4.83 16.37 4.83C15.53 4.83 14.65 4.89 13.79 4.89C12.92 4.89 12.02 4.83 11.16 4.83C11.01 4.83 10.76 4.83 10.76 5.21C10.76 5.48 10.91 5.48 11.33 5.48C12.04 5.48 12.65 5.48 12.65 5.84C12.65 5.9 12.61 6.17 12.59 6.24L9.73 17.66C9.56 18.35 9.38 18.52 7.97 18.52C7.53 18.52 7.34 18.52 7.34 18.9C7.34 19.13 7.49 19.17 7.63 19.17C8.47 19.17 9.35 19.11 10.21 19.11C11.08 19.11 11.98 19.17 12.82 19.17C12.97 19.17 13.24 19.17 13.24 18.77Z"/>'),
 ('bold', 'Write bold text',
  '<path class="gl emb" d="M19.5 15.3C19.5 13.28 17.69 11.87 15.19 11.68C17.46 11.29 18.83 10.03 18.83 8.41C18.83 6.48 17 4.8 13.62 4.8H4.5V5.78H6.77V18.22H4.5V19.2H14.25C17.73 19.2 19.5 17.36 19.5 15.3ZM15.84 8.41C15.84 10.03 14.84 11.33 12.8 11.33H9.52V5.78H13.34C15.46 5.78 15.84 7.44 15.84 8.41ZM16.41 15.28C16.41 15.53 16.41 18.22 13.39 18.22H9.52V12.08H13.6C14.02 12.08 15 12.08 15.72 12.99C16.41 13.87 16.41 15.04 16.41 15.28Z"/>'),
 ('typewriter', 'Use a typewriter font',
  '<path class="gl emb" d="M17.19 7.79V6.18C17.19 5.5 17.06 5.28 16.31 5.28H7.71C6.98 5.28 6.81 5.46 6.81 6.18V7.79C6.81 8.21 6.81 8.69 7.56 8.69C8.33 8.69 8.33 8.23 8.33 7.79V6.62H11.25V17.38H10.11C9.78 17.38 9.23 17.38 9.23 18.04C9.23 18.72 9.76 18.72 10.11 18.72H13.91C14.24 18.72 14.79 18.72 14.79 18.06C14.79 17.38 14.27 17.38 13.91 17.38H12.77V6.62H15.67V7.79C15.67 8.21 15.67 8.69 16.42 8.69C17.19 8.69 17.19 8.23 17.19 7.79Z"/>'),
 ('smallcaps', 'Use small capitals',
  '<path class="gl emb" d="M10.6 14.97C10.6 13.45 9.69 11.65 7.6 11.14L5.02 10.51C3.36 10.11 2.97 8.8 2.97 8.11C2.97 6.82 4.05 5.63 5.66 5.63C8.29 5.63 9.29 7.54 9.56 9.48C9.6 9.73 9.6 9.82 9.81 9.82C10.05 9.82 10.05 9.73 10.05 9.37V5.55C10.05 5.23 10.05 5.09 9.84 5.09C9.71 5.09 9.71 5.11 9.56 5.34L8.86 6.46C8.34 5.95 7.47 5.09 5.64 5.09C3.44 5.09 1.75 6.77 1.75 8.78C1.75 10 2.4 10.85 2.64 11.14C3.54 12.07 4.11 12.2 5.68 12.58C5.99 12.66 6.31 12.71 6.61 12.79C7.73 13.05 8.11 13.15 8.7 13.8C8.82 13.93 9.39 14.57 9.39 15.6C9.39 16.95 8.38 18.32 6.65 18.32C5.85 18.32 4.58 18.17 3.54 17.35C2.3 16.38 2.24 14.99 2.22 14.35C2.21 14.21 2.09 14.18 2 14.18C1.75 14.18 1.75 14.31 1.75 14.65V18.45C1.75 18.77 1.75 18.91 1.96 18.91C2.09 18.91 2.13 18.85 2.24 18.68L2.95 17.54C3.48 18.09 4.71 18.91 6.67 18.91C9.05 18.91 10.6 16.99 10.6 14.97ZM22.25 15.13C22.25 14.95 22.25 14.82 22 14.82C21.79 14.82 21.79 14.94 21.78 15.13C21.64 17.14 20.01 18.28 18.41 18.28C17.46 18.28 14.59 17.77 14.59 13.61C14.59 9.39 17.54 8.93 18.39 8.93C19.78 8.93 21.4 9.92 21.76 12.33C21.79 12.47 21.81 12.58 22 12.58C22.25 12.58 22.25 12.48 22.25 12.12V8.91C22.25 8.61 22.25 8.46 22.06 8.46C21.95 8.46 21.93 8.49 21.78 8.68L21.03 9.65C20.64 9.24 19.7 8.46 18.28 8.46C15.41 8.46 12.96 10.72 12.96 13.61C12.96 16.49 15.41 18.75 18.28 18.75C20.75 18.75 22.25 16.78 22.25 15.13Z"/>'),
 ('color', 'Select a foreground color',
  '<path class="paper o" d="M12 3.25c5.1 0 9.25 3.55 9.25 7.9 0 2.9-2.2 4.35-4.3 4.35h-1.7c-1.1 0-1.75.9-1.3 1.9.55 1.2.35 3.35-1.95 3.35-5.1 0-9.25-3.95-9.25-8.75S6.9 3.25 12 3.25z"/><circle class="gl" cx="7.5" cy="11" r="1.6"/><circle class="sec o thin" cx="10.5" cy="7" r="1.6"/><circle class="gl" cx="15.5" cy="7.25" r="1.6"/><circle class="sec o thin" cx="7.75" cy="15.75" r="1.6"/>'),
 ('macro', 'Insert a personal macro',
  '<path class="gl emb" d="M22.11 18.86V17.92H19.95V6.08H22.11V5.14H17.69C17.25 5.14 17.03 5.14 16.81 5.64L12.01 16.22L7.21 5.64C6.99 5.14 6.77 5.14 6.33 5.14H1.89V6.08H4.05V17.34C4.05 17.78 4.03 17.8 3.47 17.86C2.99 17.92 2.95 17.92 2.39 17.92H1.89V18.86C2.65 18.8 3.79 18.8 4.57 18.8C5.41 18.8 6.45 18.8 7.27 18.86V17.92H6.77C6.41 17.92 6.07 17.9 5.71 17.86C5.13 17.8 5.11 17.78 5.11 17.34V6.34H5.13L10.57 18.36C10.75 18.76 10.99 18.86 11.21 18.86C11.61 18.86 11.77 18.56 11.85 18.38L17.43 6.08H17.45V17.92H15.29V18.86C16.01 18.8 17.87 18.8 18.69 18.8C19.51 18.8 21.39 18.8 22.11 18.86Z"/>'),
 ('textual', 'Insert plain text',
  '<path class="gl emb" d="M19.14 9.5 18.72 4.55H5.28L4.86 9.5H5.41C5.72 5.96 6.05 5.23 9.37 5.23C9.77 5.23 10.34 5.23 10.56 5.28C11.02 5.37 11.02 5.61 11.02 6.12V17.71C11.02 18.46 11.02 18.77 8.71 18.77H7.83V19.45C8.73 19.38 10.98 19.38 11.99 19.38C13 19.38 15.27 19.38 16.17 19.45V18.77H15.29C12.98 18.77 12.98 18.46 12.98 17.71V6.12C12.98 5.68 12.98 5.37 13.38 5.28C13.62 5.23 14.21 5.23 14.63 5.23C17.95 5.23 18.28 5.96 18.59 9.5Z"/>'),
 ('math', 'Insert mathematics',
  '<path class="gl emb" d="M14.95 15.09C14.95 14.88 14.78 14.88 14.62 14.88C14.36 14.88 14.34 14.9 14.23 15.25C13.63 17.14 12.54 18 11.61 18C11.17 18 10.58 17.73 10.58 16.59C10.58 16.06 10.82 15.12 11 14.37L11.74 11.36C12.05 10.21 12.64 9 13.68 9C13.75 9 14.29 9 14.65 9.31C13.77 9.53 13.77 10.37 13.77 10.37C13.77 10.65 13.96 11.07 14.54 11.07C14.93 11.07 15.61 10.76 15.61 9.9C15.61 8.78 14.34 8.52 13.7 8.52C12.49 8.52 11.77 9.6 11.54 10.01C11.06 8.69 9.98 8.52 9.43 8.52C7.23 8.52 6.04 11.4 6.04 11.91C6.04 12.12 6.27 12.12 6.38 12.12C6.66 12.12 6.66 12.1 6.77 11.75C7.37 9.86 8.51 9 9.39 9C10.03 9 10.45 9.51 10.45 10.39C10.45 10.91 10.18 11.99 9.98 12.81L9.48 14.81C9.12 16.24 8.66 18 7.34 18C7.28 18 6.75 18 6.38 17.69C7.04 17.52 7.23 16.96 7.23 16.63C7.23 16.06 6.77 15.93 6.49 15.93C5.94 15.93 5.38 16.39 5.38 17.12C5.38 17.98 6.31 18.48 7.32 18.48C8.38 18.48 9.1 17.65 9.48 16.99C9.92 18.24 11 18.48 11.57 18.48C13.83 18.48 14.95 15.54 14.95 15.09Z"/><path class="gl emb" d="M20.43 8.15H20.06C20.02 8.39 19.91 9.04 19.77 9.15C19.68 9.22 18.84 9.22 18.68 9.22H16.66C17.81 8.19 18.2 7.89 18.86 7.37C19.67 6.72 20.43 6.04 20.43 4.99C20.43 3.66 19.27 2.85 17.86 2.85C16.49 2.85 15.57 3.8 15.57 4.82C15.57 5.38 16.04 5.43 16.15 5.43C16.42 5.43 16.73 5.25 16.73 4.85C16.73 4.65 16.66 4.27 16.09 4.27C16.43 3.49 17.18 3.24 17.69 3.24C18.79 3.24 19.36 4.1 19.36 4.99C19.36 5.95 18.68 6.71 18.33 7.1L15.68 9.72C15.57 9.82 15.57 9.84 15.57 10.15H20.1Z"/>'),
 ('table', 'Insert a table',
  '<rect class="paper o" x="3" y="4.5" width="18" height="15" rx="2"/><path class="sec" d="M3.8 5.3h16.4v3.45H3.8z"/><rect class="o" x="3" y="4.5" width="18" height="15" rx="2"/><path class="o thin" d="M3 8.75h18M3 12.25h18M3 15.75h18M9 4.5v15M15 4.5v15"/>'),
 ('image', 'Insert a picture',
  '<rect class="paper o" x="2.5" y="4.5" width="19" height="15" rx="2"/><path class="sec o thin" d="M3.3 17.5 8.5 11.5l4 4.25 2.75-2.75 5.5 5.5"/><circle class="o thin" cx="15.5" cy="9" r="1.75"/>'),
 ('link', 'Insert a link',
  '<g transform="rotate(-45 12 12)"><rect class="o" x="2.5" y="9" width="11" height="6" rx="3"/><rect class="o" x="10.5" y="9" width="11" height="6" rx="3"/></g>'),
 ('switch', 'Switching and folding',
  '<rect class="sec o" x="9" y="3" width="12" height="10.5" rx="1.5"/><rect class="sec o" x="6" y="6.25" width="12" height="10.5" rx="1.5"/><rect class="paper o" x="3" y="9.5" width="12" height="10.5" rx="1.5"/>'),
 ('animate', 'Animation',
  '<rect class="paper o" x="4.5" y="2.75" width="15" height="18.5" rx="1.5"/><path class="o thin" d="M8 2.75v18.5M16 2.75v18.5"/><path class="gl" d="M5.6 4.75h1.3v1.5H5.6zM5.6 8.75h1.3v1.5H5.6zM5.6 12.75h1.3v1.5H5.6zM5.6 16.75h1.3v1.5H5.6zM17.1 4.75h1.3v1.5h-1.3zM17.1 8.75h1.3v1.5h-1.3zM17.1 12.75h1.3v1.5h-1.3zM17.1 16.75h1.3v1.5h-1.3z"/><rect class="sec o thin" x="9.5" y="4.5" width="5" height="6.5" rx=".5"/><rect class="sec o thin" x="9.5" y="13" width="5" height="6.5" rx=".5"/>'),
 ('shell', 'Start an interactive session',
  '<rect class="paper o" x="2.5" y="4" width="19" height="16" rx="2.5"/><path class="o thin" d="M2.5 8h19"/><path class="o" d="M6.5 11.5 9 14l-2.5 2.5M11.5 16.5h5.5"/>'),
]
ICONS= ICONS + TEXT_ICONS

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
EMBOLDEN= 1.1          # outline that thickens the letters

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
  if "gl" in cl: fill= p["ink"]
  if "emb" in cl: stroke= p["ink"]; width= EMBOLDEN
  if "dots" in cl: stroke= p["ink"]; width= 1.4
  a= 'fill="%s"' % fill
  if stroke != "none": a += ' stroke="%s" stroke-width="%g"' % (stroke, width)
  if "dots" in cl: a += ' stroke-dasharray="0 2.2"'
  return a

def svg (body, p):
  body= re.sub (r'class="([^"]*)"', lambda m: attributes (m.group (1), p), body)
  return ('<?xml version="1.0" encoding="UTF-8"?>\n'
          '<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" '
          'viewBox="0 0 24 24">\n'
          ' <g stroke-linecap="round" stroke-linejoin="round">%s</g>\n'
          '</svg>\n' % body)

def png_directories (pix, name):
  modern= os.path.join (pix, "modern")
  found= []
  for size in sorted (os.listdir (modern)):
    for group in sorted (os.listdir (os.path.join (modern, size))):
      d= os.path.join (modern, size, group)
      if os.path.exists (os.path.join (d, "tm_%s.png" % name)): found.append (d)
  return found

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
      # PNG fallbacks, at the size of the directory where they already are
      src= os.path.join (pix, "light", "tm_%s.svg" % name)
      for d in png_directories (pix, name):
        base= int (os.path.basename (os.path.dirname (d)).split ("x")[0])
        for tag, k in (("", 1), ("_x2", 2), ("_x4", 4)):
          dest= os.path.join (d, "tm_%s%s.png" % (name, tag))
          size= str (base * k)
          subprocess.run (["rsvg-convert", "-w", size, "-h", size,
                           src, "-o", dest], check= True)

if __name__ == "__main__":
  main ()
