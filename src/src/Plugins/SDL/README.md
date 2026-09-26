# The SDL port

An experimental port which uses SDL3 for the windows and the events, the
Widkit toolkit of TeXmacs for the widgets (as the X11 port), and MuPDF for
the drawing.

    ./configure --with-gui=sdl --with-mupdf=/opt/homebrew --with-sdl3 ...
    make
    TEXMACS_PATH=$PWD/TeXmacs TeXmacs/bin/texmacs.bin

## How it works

- Each window has a backing store: an opaque MuPDF pixmap of the size of the
  window in device pixels, in which the widgets draw with `mupdf_renderer`.
  The repaint records what it changed, and `present` copies only those
  rectangles to the surface of the window (`SDL_ConvertPixels`, then
  `SDL_UpdateWindowSurfaceRects`). A scroll shifts the pixels of the backing
  store in place. There is no `SDL_Renderer`.
- The renderers draw at `retina_factor` pixels per point, taken from the pixel
  density of the primary display (`TEXMACS_SDL_DENSITY` overrides it). The
  pointer is converted with the density of its window.
- The loop sleeps in `SDL_WaitEventTimeout`, handles the waiting events in a
  burst (a motion superseded by the next one is dropped), lets the editors
  apply their changes, and repaints when the queue is empty, or at least every
  50 ms. While a window is dragged by its border, an event watch lays it out
  and repaints it from inside SDL's event pump, but only while the loop is
  waiting (`watch_may_run`).
- Keys: a keystroke which types text is delivered by its text event, which
  carries what the input method or a dead key composed; the others are keys
  (`C-x`, `M-s`, `return`...). An input method's composition comes as
  `pre-edit:<cursor>:<text>`.
- Mouse: the buttons are tracked from the events; the modifiers follow the Qt
  port (on macOS, control and option emulate the right and middle buttons).
  The wheel deltas accumulate into the `press-up`/`press-down` steps of the
  editor: a notch is one step, ten units of a trackpad make one.
- The system clipboard holds the primary selection (TeXmacs, HTML and plain
  text flavours); the other selections are kept internally.
- After a window is shown or resized, the whole window is invalidated, as the
  Expose events of X11 do: Widkit relies on them.

## Testing without a display

`TEXMACS_SDL_SCRIPT=<file>` replays the commands of the file (see the comment
in `sdl_gui.cpp`: `wait`, `window`, `move`, `click`, `wheel`, `key`, `text`,
`focus`, `resize`, `close`, `snapshot`). `snapshot <name>` saves the backing
store of the target window as `<TEXMACS_SDL_SNAPSHOT>/<name>.png`. A `window`
command which matches nothing lists the windows (popups are windows of their
own: a menu is `window Popup`).

## Limits

- The window positions and sizes given to TeXmacs are in points: on a display
  whose density differs from `retina_factor`, what is drawn is at the scale of
  the renderer and positions of popups may be off.
- Custom cursors (bitmaps) are not supported, except the invisible one.
- No file dialogs of the system, no drag and drop yet.
