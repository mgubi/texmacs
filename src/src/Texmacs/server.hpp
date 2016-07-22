
/******************************************************************************
* MODULE     : server.hpp
* DESCRIPTION: Main current graphical interface for user applications
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef SERVER_H
#define SERVER_H
#include "url.hpp"
#include "widget.hpp"
#include "scheme.hpp"

class server_rep: public abstract_struct {
public:
  server_rep ();
  virtual ~server_rep ();
  virtual server_rep* get_server () = 0;

  /* Control global server parameters */
  virtual void   set_font_rules (scheme_tree rules) = 0;
  virtual bool   kbd_get_command (string s, string& help, command& cmd) = 0;
  virtual void   insert_kbd_wildcard (string key, string im,
				      bool post, bool l, bool r) = 0;
  virtual string kbd_pre_rewrite (string l) = 0;
  virtual string kbd_post_rewrite (string l, bool var_flag= true) = 0;
  virtual tree   kbd_system_rewrite (string l) = 0;
  virtual void   set_variant_keys (string var, string unvar) = 0;
  virtual void   get_keycomb (string& s, int& status,
			      command& cmd, string& sh, string& help) = 0;

  /* TeXmacs frames */
  virtual int  get_window_serial () = 0;
  virtual void set_window_property (scheme_tree what, scheme_tree val) = 0;
  virtual void set_bool_window_property (string what, bool val) = 0;
  virtual void set_int_window_property (string what, int val) = 0;
  virtual void set_string_window_property (string what, string val) = 0;
  virtual scheme_tree get_window_property (scheme_tree what) = 0;
  virtual bool get_bool_window_property (string what) = 0;
  virtual int get_int_window_property (string what) = 0;
  virtual string get_string_window_property (string what) = 0;

  virtual void show_header (bool flag) = 0;
  virtual void show_icon_bar (int which, bool flag) = 0;
  virtual void show_side_tools (int which, bool flag) = 0;
  virtual void show_bottom_tools (int which, bool flag) = 0;
  virtual bool visible_header () = 0;
  virtual bool visible_icon_bar (int which) = 0;
  virtual bool visible_side_tools (int which) = 0;
  virtual bool visible_bottom_tools (int which) = 0;
  virtual void menu_widget (string menu, widget& w) = 0;
  virtual void menu_main (string menu) = 0;
  virtual void menu_icons (int which, string menu) = 0;
  virtual void side_tools (int which, string menu) = 0;
  virtual void bottom_tools (int which, string menu) = 0;

  virtual void set_window_modified (bool flag) = 0;
  virtual void set_window_zoom_factor (double zoom) = 0;
  virtual double get_window_zoom_factor () = 0;
  virtual void set_scrollbars (int sb) = 0;
  virtual void get_visible (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void scroll_where (SI& x, SI& y) = 0;
  virtual void scroll_to (SI x, SI y) = 0;
  virtual void set_extents (SI x1, SI y1, SI x2, SI y2) = 0;
  virtual void get_extents (SI& x1, SI& y1, SI& x2, SI& y2) = 0;
  virtual void full_screen_mode (bool on, bool edit) = 0;
  virtual bool in_full_screen_mode () = 0;
  virtual bool in_full_screen_edit_mode () = 0;

  virtual void show_footer   (bool flag) = 0;
  virtual bool visible_footer () = 0;
  virtual void set_left_footer (string s) = 0;
  virtual void set_right_footer (string s) = 0;
  virtual void set_message (tree left, tree right, bool temp= false) = 0;
  virtual void recall_message () = 0;
  virtual void dialogue_start (string name, widget wid) = 0;
  virtual void dialogue_inquire (int i, string& arg) = 0;
  virtual void dialogue_end () = 0;
  virtual void choose_file (object fun, string title, string type,
			    string prompt, url name) = 0;
  virtual void interactive (object fun, scheme_tree p) = 0;
  virtual void keyboard_focus_on (string field) = 0;

  /* Miscellaneous routines */
  virtual void   style_clear_cache () = 0;
  virtual void   refresh () = 0;
  virtual void   interpose_handler () = 0;
  virtual void   wait_handler (string message, string arg) = 0;
  virtual void   set_script_status (int i) = 0;
  virtual void   set_printing_command (string s) = 0;
  virtual void   set_printer_page_type (string s) = 0;
  virtual string get_printer_page_type () = 0;
  virtual void   set_printer_dpi (string dpi) = 0;
  virtual void   set_default_zoom_factor (double zoom) = 0;
  virtual double get_default_zoom_factor () = 0;
  virtual void   inclusions_gc (string which= "*") = 0;
  virtual void   typeset_update (path p) = 0;
  virtual void   typeset_update_all () = 0;
  virtual bool   is_yes (string s) = 0;
  virtual void   quit () = 0;
  virtual void   shell (string s) = 0;
};

class server {
  ABSTRACT(server);
  server ();
};
ABSTRACT_CODE(server);


// the following are needed in the editor interface
server get_server ();
url  get_current_view ();
url  get_current_view_safe ();
void set_current_view (url u);
bool has_current_window ();

// the following are needed in the scheme interface (but not in editor)
bool in_rescue_mode ();
void gui_set_output_language (string lan);
void project_attach (string prj_name= "");
bool project_attached ();
url  project_get ();


#endif // defined SERVER_H
