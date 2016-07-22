//
//  abs_buffer.hpp
//  TeXmacs
//
//  Created by Massimiliano Gubinelli on 21/07/16.
//  Copyright © 2016 TeXmacs.org. All rights reserved.
//

#ifndef ABS_BUFFER_HPP
#define ABS_BUFFER_HPP

#include "new_data.hpp"
#include "Data/new_buffer.hpp"


extern tree the_et;
path new_document ();


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
