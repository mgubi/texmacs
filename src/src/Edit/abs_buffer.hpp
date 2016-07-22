//
//  abs_buffer.hpp
//  TeXmacs
//
//  Created by Massimiliano Gubinelli on 21/07/16.
//  Copyright © 2016 TeXmacs.org. All rights reserved.
//

#ifndef ABS_BUFFER_HPP
#define ABS_BUFFER_HPP

#include "new_document.hpp"
#include "new_data.hpp"

/******************************************************************************
 * file related information about buffers
 ******************************************************************************/

class new_buffer;
class new_buffer_rep: public concrete_struct {
public:
  url name;               // full name
  url master;             // base name for linking and navigation
  string fm;              // buffer format
  string title;           // buffer title (for menus)
  bool read_only;         // buffer is read only?
  bool secure;            // is the buffer secure?
  int last_save;          // last time that the buffer was saved
  time_t last_visit;      // time that the buffer was visited last
  
  inline new_buffer_rep (url name2):
  name (name2), master (name2),
  fm ("texmacs"), title (as_string (tail (name))),
  read_only (false), secure (is_secure (name2)),
  last_save (- (int) (((unsigned int) (-1)) >> 1)),
  last_visit (texmacs_time ()) {}
};

class new_buffer;
class new_buffer {
  CONCRETE(new_buffer);
  inline new_buffer (url name): rep (tm_new<new_buffer_rep> (name)) {}
};
//CONCRETE_CODE(new_buffer);

inline new_buffer::new_buffer (const new_buffer& x):
rep(x.rep) { INC_COUNT (this->rep); }
inline new_buffer::~new_buffer () { DEC_COUNT (this->rep); }
inline new_buffer_rep* new_buffer::operator -> () {
  return rep; }
inline new_buffer& new_buffer::operator = (new_buffer x) {
  INC_COUNT (x.rep); DEC_COUNT (this->rep);
  this->rep=x.rep; return *this; }

/******************************************************************************
 * The abstract buffer class
 ******************************************************************************/

class abs_buffer_rep;
typedef abs_buffer_rep *abs_buffer;

// abstract buffers

class abs_buffer_rep {
public:
  new_buffer buf;         // file related information
  new_data data;          // data associated to document
  abs_buffer prj;         // buffer which corresponds to the project
  path rp;                // path to the document's root in the_et

  inline abs_buffer_rep (url name):
  buf (name), data (), prj (NULL), rp (new_document ())  {}
  
  virtual ~abs_buffer_rep () { }
};


#endif /* ABS_BUFFER_HPP */
