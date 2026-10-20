/******************************************************************************
* MODULE     : gui_test.hpp
* DESCRIPTION: Hooks of the graphical interface for scripted tests
* COPYRIGHT  : (C) 2026  The TeXmacs Upsilon developers
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GUI_TEST_H
#define GUI_TEST_H
#include "string.hpp"
#include "array.hpp"

/******************************************************************************
* The driver of the tests is in Scheme, (check gui-driver), with the line
* language of the scripts in (check gui-script); see docs/gui-scripting.md.
* A port implements the hooks it can; gui_test.cpp has the defaults, which
* say that the port has none.
******************************************************************************/

// the name of the port for its scripts ("vue"), "" if it has no hooks
string gui_test_port ();
// run a raw command of the port (click, key, window...), as a line of a
// script: 0 done, 1 done or skipped (no window), and the next command may
// follow in the same turn of the event loop (a choice of window), 2 unknown
// or impossible (the driver counts it as a failure)
int gui_test_raw (string line);
// the port has something to do (events, a transition, delayed commands
// which are due...): the driver waits for it
bool gui_test_busy ();
// the texts drawn last in the window of the script, as
// "x\ty\tw\th\tpath\ttext" (window points, from the top left of its
// content; the path of the bars and menus it is in, see gui-driver.scm)
array<string> gui_test_texts ();
// the titles of the windows shown
array<string> gui_test_windows ();
// call (gui-test-pump) at each turn of the event loop, while on; also
// tells the port that the script holds the state of the mouse buttons
void gui_test_pump (bool on);
// run a Scheme command at the next turn of the loop, as -x does
// (the same on all ports)
void gui_test_eval_later (string cmd);
// the port calls it as its event loop starts: TEXMACS_TEST_SCRIPT=<file>
// runs the script (the same on all ports)
void gui_test_start ();

// driving the Qt interface by the labels of its widgets (qt_test.cpp)
int gui_test_snapshot (string dir);
array<string> gui_test_buttons ();
bool gui_test_click (string label);
bool gui_test_menu (string path);
array<string> gui_test_menu_entries (string path);
void gui_test_type (string text);
void gui_test_click_later (int ms, string dir, string label);

#endif // defined GUI_TEST_H
