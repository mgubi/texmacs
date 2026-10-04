# Icon sets

TeXmacs has three icon sets. The choice is made in
**Preferences › General › Icon set** and takes effect after a restart.

| Set | Directory | Source |
|---|---|---|
| Classical (default) | `TeXmacs/misc/pixmaps/{light,dark}`, `pixmaps/modern`, `pixmaps/traditional` | the original icons, unchanged |
| Monochrome | `TeXmacs/misc/pixmaps/monochrome/{light,dark}` | `misc/icons/make-icons.py` |
| Neo-classical | `TeXmacs/misc/pixmaps/neoclassical/{light,dark}` | `misc/icons/neoclassical/make-neoclassical.py` |

Specimen sheets, with every icon of a set and its name on light and dark
pages: [classical.pdf](classical.pdf), [monochrome.pdf](monochrome.pdf),
[neoclassical.pdf](neoclassical.pdf).

## How a set is chosen

The icons are looked up in `TEXMACS_PIXMAP_PATH`: first in the `light` or
`dark` subdirectory of each directory of the path (according to the theme),
then in the directory itself, as SVG, then as PNG or XPM (see
`src/Plugins/Qt6/QTMIconManager.cpp`). The original construction of this
path, in `init_env_vars` (`src/System/Boot/init_texmacs.cpp`), is unchanged:
it gives the classical set. When the preference `icon set` is `monochrome`
or `neo-classical`, the directory of that set is put in front of the path.
An explicit `TEXMACS_PIXMAP_PATH` in the environment is left alone; to try a
set with it, repeat the default directories after the set:

    P=$TEXMACS_PATH/misc/pixmaps
    TEXMACS_PIXMAP_PATH=$P/neoclassical:$P:$P/modern/32x32/settings:$P/modern/32x32/table:$P/modern/24x24/main:$P/modern/20x20/mode:$P/modern/16x16/focus:$P/traditional/--x17

A set needs not contain every icon: a missing one is taken from the next
directories, that is from the classical set. The toolbars take their sizes
from the bitmaps of `pixmaps/modern`, which belong to the classical set.

TeXmacs keeps a persistent cache of which files exist
(`~/.TeXmacs/system/cache/stat_cache.scm`). If a set is tried while its
directory is incomplete, the missing files are remembered as missing and
later hidden; clearing the cache fixes it.

## The monochrome set

Drawn in the manner of the macOS symbols: filled objects with an outline, in
one tint (graphite by default) and its light tones, red kept for errors and
removals. Each icon is written once, in `misc/icons/make-icons.py`, as SVG
elements with classes (outline, paper, secondary fill, badge...), turned into
presentation attributes for the light and dark palettes, since the Qt SVG
renderer ignores style sheets. Letters are Latin Modern outlines, stored as
paths. The icons of the focus toolbar are drawn larger (they are shown at 16
pixels) and those of the main toolbar with slightly thinner lines.

    misc/icons/make-icons.py [names...]          # the set, in pixmaps/monochrome
    misc/icons/make-icons.py --tint=#RRGGBB      # another tint
    misc/icons/make-icons.py --colour --out=DIR  # a colour variant, elsewhere
    misc/icons/make-icons.py --png               # also PNG renderings

## The neo-classical set

The compositions and colours of the classical icons, modernized: a soft
palette, light vertical gradients, dark grey outlines of one weight, rounded
corners. It is built from three sources, in this order of priority:

1. icons redrawn by hand in the script (the main toolbar, the text toolbar,
   parts of the focus toolbar, the preference tabs and a few others);
2. the table icons, from the drawings of the monochrome set in the colours of
   this one;
3. all the other icons, converted from the original SVG files kept in
   `misc/icons/neoclassical/originals`: their shapes are kept, their colours
   mapped onto the palette (letters and symbols in solid ink, grey lines and
   borders in two darker tones, small red marks in a strong red), their
   fills shaded, and a dark version derived colour by colour. The flags are
   kept as they are.

The icons of the focus bar (those the classical set draws at 16 pixels) and
the preference tabs are flat, without gradients.

    misc/icons/neoclassical/make-neoclassical.py [names...]

The constants at the top of the script (`STROKE`, `SCALE`, `OUTLINE`,
`PALETTE`, `SHADING`, `JOINS`, `LINE`) set the overall style.

## Specimens and pictures

    misc/icons/make-sheets.py

writes `doc/icons/<set>.pdf`, the specimen sheets (vector: the SVG files of
the icons are nested as they are, their ids and style classes prefixed), and
`doc/icons/<set>-toolbars.png`, the main, text and focus toolbars on light and
dark, as shown in the README. It needs `rsvg-convert`; the generators need
Python 3, and `rsvg-convert` for PNG output.

Checking an icon with the renderer TeXmacs uses is worth it: the Qt SVG
renderer supports less than browsers or `rsvg-convert` (no filters, masks or
clip paths, no style sheets).
