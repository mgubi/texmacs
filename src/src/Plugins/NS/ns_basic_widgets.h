
/******************************************************************************
* MODULE     : ns_basic_widgets.h
* DESCRIPTION: Basic widgets
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

class ns_text_widget_rep : public ns_widget_rep {
public:
  string str;
  color col;
  bool tsp;
  
  ns_text_widget_rep(string _s, color _col, bool _tsp) 
  : str(_s), col(_col), tsp(_tsp) {};  

  virtual TMMenuItem *as_menuitem();

};

class ns_image_widget_rep : public ns_widget_rep {
public:
  url image;
  
  ns_image_widget_rep(url _image) : image(_image) {};
  virtual TMMenuItem *as_menuitem();
};

class ns_balloon_widget_rep : public ns_widget_rep {
public:
  widget text, hint;
  
  ns_balloon_widget_rep(widget _text, widget _hint) : text(_text), hint(_hint) {};
  virtual TMMenuItem *as_menuitem();
};


class ns_input_text_widget_rep : public ns_widget_rep {
public:
  command cmd;
  string type;
  array<string> def;
  string text;
  
  ns_input_text_widget_rep(command _cmd, string _type, array<string> _def)
  : cmd(_cmd), type(_type), def(_def), text("") {};
};
