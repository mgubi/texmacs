/******************************************************************************
* MODULE     : gui_test.cpp
* DESCRIPTION: Hooks of the graphical interface for scripted tests
* COPYRIGHT  : (C) 2026  The TeXmacs Upsilon developers
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "gui_test.hpp"
#include "scheme.hpp"
#include "analyze.hpp"
#include "sys_utils.hpp"

void
gui_test_eval_later (string cmd) {
  exec_delayed (scheme_cmd (cmd));
}

void
gui_test_start () {
  // TEXMACS_TEST_SCRIPT=<file> (TEXMACS_VUE_SCRIPT, its former name, in
  // Vue) runs a script once the event loop runs
  string file= get_env ("TEXMACS_TEST_SCRIPT");
  if (N(file) == 0) file= get_env ("TEXMACS_VUE_SCRIPT");
  if (N(file) == 0) return;
  exec_delayed (scheme_cmd ("(begin (use-modules (check gui-script)) "
                            "(gui-script-start " * scm_quote (file) * "))"));
}

/******************************************************************************
* The defaults, for the ports which do not implement the hooks
******************************************************************************/

#if !defined (VUETEXMACS) && !defined (QTTEXMACS) && !defined (AQUATEXMACS)
string gui_test_port () { return ""; }
int gui_test_raw (string line) { (void) line; return 2; }
bool gui_test_busy () { return false; }
array<string> gui_test_texts () { return array<string> (); }
void gui_test_pump (bool on) { (void) on; }
#endif

#ifndef QTTEXMACS
int gui_test_snapshot (string dir) { (void) dir; return 0; }
array<string> gui_test_buttons () { return array<string> (); }
bool gui_test_click (string label) { (void) label; return false; }
bool gui_test_menu (string path) { (void) path; return false; }
array<string> gui_test_menu_entries (string path) {
  (void) path; return array<string> (); }
void gui_test_type (string text) { (void) text; }
void gui_test_click_later (int ms, string dir, string label) {
  (void) ms; (void) dir; (void) label; }
#endif
