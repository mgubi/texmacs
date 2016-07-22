
/******************************************************************************
* MODULE     : new_buffer.hpp
* DESCRIPTION: File related information for buffers
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NEW_BUFFER_H
#define NEW_BUFFER_H
#include "tree.hpp"
#include "hashmap.hpp"
#include "url.hpp"
#include "timer.hpp"


/******************************************************************************
* Low level types and routines
******************************************************************************/

class tm_buffer_rep;
typedef tm_buffer_rep* tm_buffer;
tm_buffer concrete_buffer (url name);
tm_buffer concrete_buffer_insist (url name);

/******************************************************************************
* High level routines
******************************************************************************/

array<url> get_all_buffers ();
url  make_new_buffer ();
void remove_buffer (url name);
int  number_buffers ();
url  get_current_buffer ();
url  get_current_buffer_safe ();
url  path_to_buffer (path p);
void rename_buffer (url name, url new_name);
url get_master_buffer (url name);
void set_master_buffer (url name, url master);
void set_title_buffer (url name, string title);
string get_title_buffer (url name);
void set_buffer_tree (url name, tree doc);
tree get_buffer_tree (url name);
void set_buffer_body (url name, tree body);
tree get_buffer_body (url name);
url new_buffer_in_new_window (url name, tree t, tree geom= "");
int  get_last_save_buffer (url name);
void set_last_save_buffer (url name, int t);
bool is_aux_buffer (url name);
double last_visited (url name);
bool buffer_modified (url name);
bool buffer_modified_since_autosave (url name);
void pretend_buffer_modified (url name);
void pretend_buffer_saved (url name);
void pretend_buffer_autosaved (url name);
void attach_buffer_notifier (url name);
bool buffer_has_name (url name);
bool buffer_import (url name, url src, string fm);
bool buffer_load (url name);
bool buffer_export (url name, url dest, string fm);
bool buffer_save (url name);
tree import_loaded_tree (string s, url u, string fm);
tree import_tree (url u, string fm);
bool export_tree (tree doc, url u, string fm);
tree load_style_tree (string package);

#endif // NEW_BUFFER_H
