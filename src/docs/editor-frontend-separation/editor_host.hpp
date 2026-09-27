
/******************************************************************************
* MODULE     : editor_host.hpp
* DESCRIPTION: What an editor asks of the window which shows it
*              (DRAFT for stage 2, see docs/editor-frontend-separation.md;
*              not compiled)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// Intended place: Edit/editor_host.hpp
//
// Today the editor reaches its window through server_rep (Texmacs/server.hpp)
// with the SERVER macro of Edit/editor.hpp, which makes this editor the
// current view, calls the server, and restores the previous view: the
// server then finds the window from the current view. It also calls
// concrete_window (), buffer_to_windows () and get_server () directly
// (edit_interface.cpp).
//
// An editor_host is given to each editor by the view which owns it
// (Texmacs/Data/new_view.cpp, attach_view), so the editor talks to its own
// window without changing the current view. A headless editor gets a host
// whose methods do nothing, or record the messages for tests.
//
// Only the methods used from Edit/ are listed; the rest of server_rep stays
// where it is, for Scheme and the application.

#ifndef EDITOR_HOST_H
#define EDITOR_HOST_H
#include "tree.hpp"
#include "command.hpp"

class editor_host_rep {
public:
  virtual ~editor_host_rep () {}

  // Messages and footers (set_message is used in 16 files of Edit/).
  virtual void set_message (tree left, tree right, bool temp= false) = 0;
  virtual void recall_message () = 0;
  virtual void set_left_footer (string s) = 0;
  virtual void set_right_footer (string s) = 0;

  // The bars of the window, as the editor switches them on resume and in
  // full screen mode (edit_interface.cpp).
  virtual void show_header (bool flag) = 0;
  virtual void show_footer (bool flag) = 0;
  virtual void menu_main (string menu) = 0;
  virtual void menu_icons (int which, string menu) = 0;
  virtual void side_tools (int which, string menu) = 0;
  virtual void bottom_tools (int which, string menu) = 0;
  virtual void full_screen_mode (bool on, bool edit) = 0;
  virtual bool in_full_screen_mode () = 0;
  virtual bool in_full_screen_edit_mode () = 0;

  // Every window showing the buffer of this editor (today
  // buffer_to_windows + concrete_window, to mark them modified).
  virtual void set_modified (bool flag) = 0;

  // Keyboard tables, which live in the server and are filled from Scheme.
  virtual bool   kbd_get_command (string s, string& help, command& cmd) = 0;
  virtual string kbd_post_rewrite (string l, bool var_flag= true) = 0;
  virtual tree   kbd_system_rewrite (string l) = 0;
  virtual void   get_keycomb (string& s, int& status, command& cmd,
                              string& shorthand, string& help) = 0;

  // Interaction which needs a dialog.
  virtual void interactive (object fun, scheme_tree p) = 0;

  virtual double get_default_zoom_factor () = 0;
};

#endif // defined EDITOR_HOST_H
