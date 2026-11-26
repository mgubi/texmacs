/******************************************************************************
* MODULE     : vue_mac.hpp
* DESCRIPTION: The document of a window of the Vue port in the title bar of
*              macOS (vue_mac.mm, compiled on macOS only)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef VUE_MAC_HPP
#define VUE_MAC_HPP

// ns_window: the NSWindow of an SDL window (SDL_PROP_WINDOW_COCOA_WINDOW_POINTER).
// path: the file the window shows, in UTF-8, or NULL for none: AppKit then
// gives the title its proxy icon and its path menu (Cmd-click); edited: the
// document has unsaved changes (the dot in the close button)
void vue_mac_set_document (void* ns_window, const char* path, bool edited);

// what AppKit has (the represented URL and the edited flag), for the log of
// -debug-qt-widgets; the result is a static buffer
const char* vue_mac_document_state (void* ns_window);

// The scroll events of AppKit (issue #971): SDL gives the wheel events of
// a mouse and of a trackpad without hasPreciseScrollingDeltas nor the
// phases, which the Qt port reads. A local monitor of the scroll events,
// installed once by vue_mac_wheel_monitor, keeps those of the events which
// SDL turns into wheel events (one each, with the deltas x= -deltaX,
// y= deltaY, rounded away from zero without precise deltas); for the wheel
// event of SDL with deltas x, y, vue_mac_wheel_info finds the scroll event
// it came from and gives its precise flag, and whether its phase is
// cancelled (Qt does not scroll those); false when there is none (a wheel
// event made by a script, a monitor not installed)
void vue_mac_wheel_monitor ();
bool vue_mac_wheel_info (float x, float y, bool& precise, bool& cancelled);

#endif // VUE_MAC_HPP
