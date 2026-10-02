# Typographic palettes

Alternatives to the standard grid of colours of the colour menus
(Format › Color, the backgrounds, the cells of the tables...), in the Vue
interface: the **Palette** list above the grid offers **Classical**, the
standard grid (the default), then the typographic palettes. The choice
changes the grid without closing the menu (the menu moves to stay in the
window if it changed size), and is kept (the preference `typographic
palette set`).

A set has families of hues, the columns (eight in the sets of TeXmacs, a
neutral one first), each in five tones, the rows, grouped by their use:

| Rows | Group | For |
|---|---|---|
| ink, deep | Text | text on white paper |
| medium | Accents | headings, emphasis, rules |
| soft, tint | Backgrounds | boxes, highlighted text, table cells, on which the inks stay legible |

Within a column the colours go together; across a row they have the same
weight. The sets of TeXmacs: *Muted*, *Classic print*, *Earth*, *Nordic*,
*Pastel* and *Solarized*.

## Sets of your own

In `~/.TeXmacs/progs/my-init-texmacs.scm`:

```scheme
;; five rows of colours (as many in each), from ink to tint
(define-typographic-palette "Corporate"
  '("#1a1a1a" "#0b3d91" "#8b0000")      ; ink
  '("#404040" "#1f5fbf" "#b22222")      ; deep
  '("#707070" "#3d7fe0" "#d9534f")      ; medium
  '("#c8c8c8" "#a9c8f5" "#f2b8b5")      ; soft
  '("#f2f2f2" "#e8f0fc" "#fbeceb"))     ; tint

;; or a colour per column, whose five tones are computed (their hue, at
;; the lightnesses 0.17 0.30 0.50 0.78 0.94, with the saturation scaled)
(define-typographic-palette-from-colors "Sea"
  (list "#5f6b73" "#c0504d" "#b07a45" "#c9a44a"
        "#4f8a6b" "#2f8f9d" "#3b6ea8" "#6c5fa8"))

;; with the lightnesses and the factors of the saturation of the tones
(define-typographic-palette-from-colors "Sea, lighter"
  (list "#5f6b73" "#2f8f9d" "#3b6ea8")
  (list 0.25 0.38 0.60 0.84 0.96)
  (list 0.7 0.7 0.8 0.8 0.9))
```

A set of the same name replaces the one there was (a set of TeXmacs too).
The colours are those of TeXmacs (`"#rrggbb"`, `"dark red"`...), except for
`define-typographic-palette-from-colors`, which computes with `"#rrggbb"`.
`(typographic-color-tones color lightnesses saturations)` gives the tones
of a colour, and `(typographic-palette-names)` the names in the list
(`"Classical"` first, then the sets); a set may not be called
`"Classical"`, which is the standard grid. The
grid has eight colours a line: the rows of a set of eight columns are its
lines (other widths wrap).

Code: `TeXmacs/progs/kernel/gui/menu-define.scm` (the sets, the menu);
tests: `src/Plugins/Vue/tests/typographic-palette.*` (a popup) and
`typographic-palette-bar.*` (Format › Colour from the menu bar).
