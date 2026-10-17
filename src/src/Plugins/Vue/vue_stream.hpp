
/******************************************************************************
* MODULE     : vue_stream.hpp
* DESCRIPTION: The display of the Vue GUI as a stream of pixels and events
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef VUE_STREAM_H
#define VUE_STREAM_H

#include "string.hpp"
#include <SDL3/SDL.h>

// WASIX has no display: with TEXMACS_DISPLAY=stream, TeXmacs draws in the
// surface of a window of SDL's dummy video driver, sends what changed in it
// on its standard output and reads the events of the pointer and the
// keyboard on its standard input, to a host which shows the pixels (the page
// of the browser, misc/wasix/web; misc/wasix/host on the desktop). The
// protocol is in vue_stream.cpp and docs/wasix/README.md.

bool vue_stream_on ();
void vue_stream_redirect (); // at the start: the standard output is the stream
void vue_stream_open ();     // before SDL_Init (the dummy driver)
void vue_stream_present (SDL_Window* win, SDL_Surface* surf);
void vue_stream_pump ();     // the events of the host, as SDL events
void vue_stream_wait (int ms);
Uint32 vue_stream_buttons ();
void vue_stream_cursor (int system_cursor);
void vue_stream_title (string title);

#endif // VUE_STREAM_H
