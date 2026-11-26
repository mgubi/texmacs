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
// a mouse and of a trackpad without hasPreciseScrollingDeltas, the
// scrolling deltas and the phases, which the Qt port reads. A local monitor
// of the scroll events (vue_mac_wheel_monitor, installed once) keeps the
// last one; vue_mac_wheel_attach, called by the event watch of SDL when it
// queues a wheel event (x, y, stamp), within the handling of that NSEvent,
// attaches it to the wheel event when its deltas are those of the event;
// vue_mac_wheel_take gives the record of the wheel event of that stamp when
// it is handled (false: none, a scripted event), and drops those of older
// events (see qt_wheel_queue in wheel_steps.hpp)
#include "wheel_steps.hpp"
void vue_mac_wheel_monitor ();
void vue_mac_wheel_attach (float x, float y, unsigned long long stamp);
bool vue_mac_wheel_take (unsigned long long stamp, qt_wheel_record& r);

#endif // VUE_MAC_HPP
