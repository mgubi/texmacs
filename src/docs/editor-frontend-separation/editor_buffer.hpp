
/******************************************************************************
* MODULE     : editor_buffer.hpp
* DESCRIPTION: The buffer as the editor sees it
*              (DRAFT for stage 2, see docs/editor-frontend-separation.md;
*              not compiled)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// Intended place: Edit/editor_buffer.hpp
//
// The editor holds a tm_buffer (Texmacs/tm_buffer.hpp), the application's
// buffer, which also knows its views (vws), its links (lns) and whether to
// notify Scheme. From it Edit/ uses only buf->buf (file information, 26
// uses), buf->data (document data, 42) and buf->prj (the project buffer,
// 33). This is the same split as abs_buffer_rep on the 2016 dev branch.
//
// tm_buffer_rep would derive from editor_buffer_rep, so no data moves and
// the application keeps one object per buffer, shared by all its views.
//
// Ownership stays with the application (Texmacs/Data/new_buffer.cpp):
// buffers are created, looked up by url and destroyed there, and outlive
// the editors which show them. An editor never creates or deletes a buffer;
// a headless user of the editor creates an editor_buffer_rep of its own
// and deletes it after the editor.

#ifndef EDITOR_BUFFER_H
#define EDITOR_BUFFER_H
#include "scheme.hpp"          // new_buffer.hpp uses object
#include "new_data.hpp"        // new_data: project, style, init, fin, ...
#include "Data/new_buffer.hpp" // new_buffer: name, master, fm, title, ...

class editor_buffer_rep {
public:
  new_buffer buf;             // file related information
  new_data data;              // data associated to the document
  editor_buffer_rep* prj;     // buffer which corresponds to the project
  path rp;                    // path to the document's root in the_et

  inline editor_buffer_rep (url name):
    buf (name), data (), prj (NULL), rp () {}
  virtual ~editor_buffer_rep () {}
};

#endif // defined EDITOR_BUFFER_H
