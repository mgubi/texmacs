# Third-party code in the Vue interface

## Clay

`clay.h` and `clay_renderer_SDL3.c` come from Clay, the layout library of
Nic Barker: <https://github.com/nicbarker/clay>, commit `e6cc36941ab2`
(the main branch of September 2026; the `// VERSION: 0.14` line of `clay.h`
is upstream's own). `clay_renderer_SDL3.c` is upstream's
`renderers/SDL3/clay_renderer_SDL3.c`.

Clay is under the zlib/libpng license, whose text is at the end of
`clay.h` and in `TeXmacs/misc/licenses/clay-zlib.txt`:

    Copyright (c) 2024 Nic Barker

Both files are altered. Each change is marked by a comment beginning with
`// TeXmacs:`:
- `clay.h`: the wheel scrolls the innermost container under the pointer
  that can scroll (in `Clay_UpdateScrollContainers`);
- `clay_renderer_SDL3.c`: custom elements are drawn by the Vue widgets
  (`vue_render`); no `SDL_main.h` and no SDL3_image.

The other files of this folder (`clay.c` included) are part of TeXmacs, under
the GNU General Public License version 3 or later.
