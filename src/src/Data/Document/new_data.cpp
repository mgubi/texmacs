
/******************************************************************************
* MODULE     : new_data.cpp
* DESCRIPTION: Data attached to full buffers which are not necessarily
*              well represented by trees
* COPYRIGHT  : (C) 1999-2012  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "new_data.hpp"
#include "convert.hpp"
#include "merge_sort.hpp"
#include "vars.hpp"
#include "iterator.hpp"

// The variables of the initial environment which depend on the window of
// a view (its size, zoom and full screen mode): each view of a buffer has
// its own values, which are not saved (#528). The zoom is saved when the
// document asks for it (no-zoom), but it stays a variable of each view.
bool
is_view_init_variable (string var) {
  return var == PAGE_SCREEN_WIDTH || var == PAGE_SCREEN_HEIGHT ||
         var == ZOOM_FACTOR || var == "full-screen-mode";
}

tree
attach_data (tree body, new_data data, bool no_aux) {
  tree doc (DOCUMENT);
  doc << compound ("TeXmacs", TEXMACS_VERSION);
  if (data->project != "")
    doc << compound ("project", copy (data->project));
  if (data->style != tree (TUPLE))
    doc << compound ("style", copy (data->style));
  if (body != tree (DOCUMENT, ""))
    doc << compound ("body", body);
  if (N (data->init) != 0) {
    hashmap<string,tree> init= copy (data->init);
    bool keep_zoom= init->contains ("no-zoom");
    iterator<string> it= iterate (data->init);
    while (it->busy ()) {
      string var= it->next ();
      if (is_view_init_variable (var) && !(keep_zoom && var == ZOOM_FACTOR))
        init->reset (var);
    }
    doc << compound ("initial", make_collection (init));
  }
  if (N (data->fin) != 0)
    doc << compound ("final", make_collection (data->fin));
  if (N (data->att) != 0)
    doc << compound ("attachments", make_collection (data->att));
  if (!no_aux) {
    if (N (data->ref) != 0)
      doc << compound ("references", make_collection (data->ref));
    if (N (data->aux) != 0)
      doc << compound ("auxiliary", make_collection (data->aux));
  }
  //object arg1 (data->buffer_name);
  //object arg2 (body);
  //tree links= as_tree (call ("get-link-locations", arg1, arg2));
  //if (N (links) != 0)
  //  doc << compound ("links", links);
  return doc;  
}

tree
detach_data (tree doc, new_data& data) {
  data->project= extract (doc, "project");
  data->style  = extract (doc, "style");
  data->init   = hashmap<string,tree> (UNINIT, extract (doc, "initial"));
  data->fin    = hashmap<string,tree> (UNINIT, extract (doc, "final"));
  data->ref    = hashmap<string,tree> (UNINIT, extract (doc, "references"));
  data->aux    = hashmap<string,tree> (UNINIT, extract (doc, "auxiliary"));
  data->att    = hashmap<string,tree> (UNINIT, extract (doc, "attachments"));
  //tree links= extract (doc, "links");
  //if (N (links) != 0)
  //  (void) call ("register-link-locations", object (u), object (links));
  return extract (doc, "body");
}
