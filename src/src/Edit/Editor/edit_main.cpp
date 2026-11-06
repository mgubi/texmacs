
/******************************************************************************
* MODULE     : editor.cpp
* DESCRIPTION: routines for the editor
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_main.hpp"
#include "tm_buffer.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "printer.hpp"
#include "convert.hpp"
#include "connect.hpp"
#include "typesetter.hpp"

extern bool typeset_folds;   // Typeset/Bridge/bridge.cpp
#include "Html_math/html_math.hpp"
#include "drd_std.hpp"
#include "message.hpp"
#include <setjmp.h>
#include "image_files.hpp"
#include "iterator.hpp"
#include "Bridge/bridge.hpp"

#ifdef EXPERIMENTAL
#include "../../Style/Memorizer/clean_copy.hpp"
#endif

#ifdef USE_GS
#include "Ghostscript/gs_utilities.hpp"
#endif

#if defined(QTWKTEXMACS)
#include "Qtwk/qtwk_gui.hpp"
#include "Qt/qt_utilities.hpp"
#elif defined(QTTEXMACS)
#include "Qt/qt_gui.hpp"
#include "Qt/qt_utilities.hpp"
#endif

/******************************************************************************
* Constructors and destructor
******************************************************************************/

editor_rep::editor_rep ():
  simple_widget_rep (), cvw (NULL), mvw (NULL),
  drd (std_drd), et (the_et), rp () {
  cout << "TeXmacs] warning, this virtual constructor should never be called\n";
}

editor_rep::editor_rep (server_rep* sv2, tm_buffer buf2):
  simple_widget_rep (), sv (sv2), cvw (NULL), mvw (NULL), buf (buf2),
  drd (buf->buf->title, std_drd), et (the_et), rp (buf2->rp) {}

bool
editor_rep::is_current_editor () {
  editor ed= get_current_editor ();
  return ed.rep == (editor_rep*) this;
}

edit_main_rep::edit_main_rep (server_rep* sv, tm_buffer buf):
  editor_rep (sv, buf), props (UNKNOWN), ed_obs (edit_observer (this))
{
#ifdef EXPERIMENTAL
  cct= copy (subtree (et, rp));
  copy_ip (subtree (et, rp), cct);
#endif
  attach_observer (subtree (et, rp), ed_obs);
  notify_change (THE_TREE);
  tp= correct_cursor (et, rp * 0);
}

edit_main_rep::~edit_main_rep () {
  detach_observer (subtree (et, rp), ed_obs);
#ifdef EXPERIMENTAL
  mem= memorizer ();
#endif
}

editor
new_editor (server_rep* sv, tm_buffer buf) {
  return tm_new<edit_main_rep> (sv, buf);
}

#ifdef NO_FAST_ALLOC
template<> void
tm_delete<editor_rep> (editor_rep* ptr) {
  if (ptr == NULL) return;
  delete ptr;
}
#else
template<> void
tm_delete<editor_rep> (editor_rep* ptr) {
  if (ptr == NULL) return;
  void *mem= ptr->derived_this ();
  ptr -> ~editor_rep ();
  fast_delete (mem);
}
#endif

/******************************************************************************
* Properties
******************************************************************************/

void
edit_main_rep::set_property (scheme_tree what, scheme_tree val) {
  props (what)= val;
}

void
edit_main_rep::set_bool_property (string what, bool val) {
  props (what)= (val? string ("true"): string ("false"));
}

void
edit_main_rep::set_int_property (string what, int val) {
  props (what)= as_tree (val);
}

void
edit_main_rep::set_string_property (string what, string val) {
  props (what)= val;
}

scheme_tree
edit_main_rep::get_property (scheme_tree what) {
  return props [what];
}

bool
edit_main_rep::get_bool_property (string what) {
  return as_bool (props [what]);
}

int
edit_main_rep::get_int_property (string what) {
  return as_int (props [what]);
}

string
edit_main_rep::get_string_property (string what) {
  return as_string (props [what]);
}

/******************************************************************************
* Global routines
******************************************************************************/

void
edit_main_rep::clear_buffer () {
  assign (rp, tree (DOCUMENT, tree ("")));
}

void
edit_main_rep::new_window () {
}

void
edit_main_rep::clone_window () {
}

void
edit_main_rep::tex_buffer () {
}

url
edit_main_rep::get_name () {
  return buf->buf->name;
}

void
edit_main_rep::focus_on_this_editor () {
  focus_on_editor (this);
}

void
edit_main_rep::notify_page_change () {
  if (is_attached (this)) send_invalidate_all (this);
}

string
edit_main_rep::get_metadata (string kind) {
  string var= "global-" * kind;
  string val= get_init_string (var);
  if (val != "") return val;
  val= search_metadata (subtree (et, rp), kind);
  if (val != "") return val;
  if (kind == "title") return utf8_to_cork (as_string (tail (get_name ())));
#ifndef OS_MINGW
  if (kind == "author" &&
      !is_none (resolve_in_path ("whoami")) &&
      !is_none (resolve_in_path ("finger")) &&
      !is_none (resolve_in_path ("sed"))) {
    string val= var_eval_system ("finger `whoami` 2> /dev/null | sed -e '/Name/!d' -e 's/.*Name: //'");
    if (N(val) > 1) return utf8_to_cork (val);
  }
  if (kind == "pseudo" &&
      !is_none (resolve_in_path ("whoami"))) {
    string val= var_eval_system ("whoami");
    if (N(val) > 1) return utf8_to_cork (val);
  }
#endif
  return "";
}

/******************************************************************************
* Printing
******************************************************************************/

string printing_dpi ("600");
string printing_on ("a4");

// Is the PDF written by the MuPDF renderer? It is a prototype, so it is
// off unless it is asked for, by the preference or by the environment
// (see docs/pdf-output-with-mupdf.md).
bool
use_mupdf_pdf () {
#ifdef __EMSCRIPTEN__
  return true; // the only way to a PDF in the browser (no Ghostscript)
#elif defined(MUPDF_RENDERER)
  if (get_env ("TEXMACS_PDF_MUPDF") == "1") return true;
  return get_preference ("native pdf renderer", "default") == "mupdf";
#else
  return false;
#endif
}

bool
use_pdf () {
#ifdef PDF_RENDERER
  return get_preference ("native pdf", "on") == "on";
#else
  // without a PDF renderer the document goes out as PostScript and
  // Ghostscript makes the PDF; the MuPDF renderer writes it itself
  return use_mupdf_pdf ();
#endif
}

bool
use_ps () {
#ifdef PDF_RENDERER
  return get_preference ("native postscript", "on") == "on";
#else
  return true;
#endif
}

int
edit_main_rep::nr_pages () {
  string medium = env->get_string (PAGE_MEDIUM);
  if (medium == "paper") return N (eb[0]);
  typeset_prepare ();
  env->write (PAGE_MEDIUM, "paper");
  typeset_folds= false;
  box the_box= typeset_as_document (env, subtree (et, rp), reverse (rp));
  typeset_folds= true;
  env->write (PAGE_MEDIUM, medium);
  return N (the_box[0]);
}

bool
edit_main_rep::print_doc (url name, bool conform, int first, int last) {
  // true when the file was written (#705)
#ifdef USE_GS
  bool ps  = (suffix (name) == "ps");
  bool pdf = (suffix (name) == "pdf");
  url  orig= resolve (name, "");
  if (!use_pdf () && pdf)
    name= url_temp (".ps");
  if (!use_ps () && ps)
    name= url_temp (".pdf");
#endif
  
  string medium = env->get_string (PAGE_MEDIUM);
  if (conform && (medium != "paper")) conform= false;
    // FIXME: better command for conform printing

  // the hyperlinks are taken from the tables which the typesetting of the
  // buffer fills (see box_rep::display_links), and a buffer which was just
  // loaded (texmacs -c) has not been typeset yet
  typeset_forced ();

  typeset_preamble ();
    // FIXME: when printing several files via aux buffers,
    // it seems that the style can be corrupted.  Why?
  
  // Set environment variables for printing

  typeset_prepare ();
  env->write (DPI, printing_dpi);
  env->write (PAGE_SHOW_HF, "true");
  env->write (PAGE_SCREEN_MARGIN, "false");
  env->write (PAGE_BORDER, "none");
  if (is_func (env->read (BG_COLOR), _PATTERN))
    env->write (BG_COLOR, env->exec (env->read (BG_COLOR)));
  if (!conform) {
    env->write (PAGE_MEDIUM, "paper");
    env->write (PAGE_PRINTED, "true");
  }

  // Typeset pages for printing. The typesetter is kept until the pages
  // are drawn, not dropped as typeset_as_document drops it: the links of
  // the document (hlink and the like) are registered by the typesetter,
  // and a box finds its own when it is drawn (box_rep::display_links).
  // Dropped, they were found only when the editor had typeset the same
  // document on the screen before -- never in a batch export (texmacs -c).

  env->style_init_env ();
  env->update ();
  typesetter ttt= new_typesetter (env, subtree (et, rp), reverse (rp));
  typeset_folds= false;   // all the lines of program code are printed
  box the_box= ::typeset (ttt);
  typeset_folds= true;

  // Determine parameters for printer

  string page_type = env->page_real_type;
  double w         = env->page_real_width;
  double h         = env->page_real_height;
  double cm        = env->as_real_length (string ("1cm"));
  bool   landsc    = env->page_landscape;
  int    dpi       = as_int (printing_dpi);
  int    start     = max (0, first-1);
  int    end       = min (N(the_box[0]), last);
  int    pages     = end-start;
  if (conform) {
    page_type= "user";
    SI bw= the_box[0][0]->w();
    SI bh= the_box[0][0]->h();
    string bws= as_string (bw) * "tmpt";
    string bhs= as_string (bh) * "tmpt";
    w= env->as_length (bws);
    h= env->as_length (bhs);
  }
  
  // Print pages
  renderer ren= printer (name, dpi, pages, page_type, landsc, w/cm, h/cm);
  bool ok= ren->is_started ();
  
  if (ok) {
    int i;
    ren->set_metadata ("title", get_metadata ("title"));
    ren->set_metadata ("author", get_metadata ("author"));
    ren->set_metadata ("subject", get_metadata ("subject"));
    for (i=start; i<end; i++) {
      tree bg= env->read (BG_COLOR);
      ren->set_background (bg);
      if (bg != "white" && bg != "#ffffff")
        ren->clear_pattern (0, (SI) -h, (SI) w, 0);

      rectangles rs;
      the_box[0]->sx(i)= 0;
      the_box[0]->sy(i)= 0;
      the_box[0][i]->redraw (ren, path (0), rs);
      if (i<end-1) ren->next_page ();
    }
  }
  tm_delete (ren);
  delete_typesetter (ttt);

#ifdef USE_GS
  // through Ghostscript, the target is written by a conversion: an old
  // file under its name is removed first, so that it does not pass for the
  // new one when the conversion fails
  if ((!use_pdf () && pdf) || (!use_ps () && ps)) {
    if (ok && exists (orig)) ::remove (orig);
    ok= ok && !exists (orig);
  }
  if (!use_pdf () && pdf) {
    if (ok) gs_to_pdf (name, orig, landsc, h/cm, w/cm);
    ::remove (name);
    ok= ok && exists (orig);
  }
  if (!use_ps () && ps) {
    if (ok) gs_to_ps (name, orig, landsc, h/cm, w/cm);
    ::remove (name);
    ok= ok && exists (orig);
  }
  if (ok && (ps || pdf))
    if (get_preference ("texmacs->pdf:check", "off") == "on") {
# if QT_VERSION >= 0x060000
      system_wait ("Checking exported file '" *
		   as_string (tail (orig)) * "' for correctness");
# endif
      gs_check (orig);
# if QT_VERSION >= 0x060000
      system_wait ("");
# endif
    }
#endif
  return ok;
}

void
edit_main_rep::print_to_file (url name, string first, string last) {
  if (!print_doc (name, false, as_int (first), as_int (last)))
    set_message (concat ("Could not save ",
                         verbatim (as_string (name, URL_SYSTEM))),
                 "print to file");
  else set_message ("Done printing", "print to file");
}

void
edit_main_rep::print_buffer (string first, string last) {
  url target;
  target= use_pdf ()? url_temp (".pdf"): url_temp (".ps");
  if (!print_doc (target, false, as_int (first), as_int (last))) {
    // nothing to send to the printer (#705)
    set_message ("Could not print: the document could not be written",
                 "print buffer");
    return;
  }
  system (get_printing_cmd (), target);  // Send the document to the printer
  set_message ("Done printing", "print buffer");
  ::remove (target);
}

#ifdef THISISTHEPREVIOUSCODE_IJUSTLEFTITHEREINCASE
void
edit_main_rep::print_buffer (string first, string last) {
  // in Qt this is the main entry point to the printing subsystem.
  // the other routines (print_to_file, ...) are overriden since all fine tuning 
  // is made here via the Qt print dialog
  bool to_file, landscape;
  url name = url_none();
  string printer;
  string paper_type;
  if (qt_print (to_file, landscape, printer, name, first, last, paper_type)) {
      if (!to_file) name = url_temp (".ps");
      print_doc (name, false, as_int (first), as_int (last));
      if (!to_file) {
        string cmd = printing_cmd * " -P" * printer;
        system (cmd, name);  
        ::remove (name);
      }
  }
}
#endif

void
edit_main_rep::export_ps (url name, string first, string last) {
  if (!print_doc (name, true, as_int (first), as_int (last)))
    set_message (concat ("Could not save ",
                         verbatim (as_string (name, URL_SYSTEM))),
                 "export postscript");
}

static bool
display_style (tree t, bool display) {
  // in t, which starts with its with
  while (is_func (t, WITH) && N(t) >= 1) {
    for (int i=0; i+1 < N(t)-1; i+=2)
      if (t[i] == MATH_DISPLAY) display= (t[i+1] == "true");
    t= t[N(t)-1];
  }
  return display;
}

/******************************************************************************
* Snippets typeset apart, in the environment of the formulas of the HTML export
******************************************************************************/

static editor_rep*
recording_editor (int env_nr) {
  // the editor of the buffer whose expansion recorded environment env_nr
  array<edit_typeset_rep*> a= edit_typeset_rep::html_recorders;
  for (int i=0; i<N(a); i++)
    if (env_nr >= a[i]->html_first &&
        env_nr < a[i]->html_first + N(a[i]->html_envs))
      return a[i];
  return NULL;
}

void
edit_main_rep::snippet_env_begin (int env_nr) {
  // the prepared environment, or the one recorded by exec_html for a formula,
  // at the resolution of printing
  typeset_prepare ();
  if (recording_editor (env_nr) == (editor_rep*) this) {
    env->patch_env (html_base);
    env->patch_env (html_envs[env_nr - html_first]);
    // the tags which the export keeps for itself (tmhtml-expand.scm)
    env->write ("html-display",
                tree (MACRO, "x", compound ("equation*", tree (ARG, "x"))));
    env->write ("html-equations-base",
                tree (MACRO, "x", compound ("equations-base",
                                            tree (ARG, "x"))));
    env->write ("html-equation-lab",
                tree (MACRO, "x", "n",
                      compound ("equation-lab", tree (ARG, "x"),
                                tree (ARG, "n"))));
  }
  tree old_info_flag= env->read (INFO_FLAG);
  env->write (DPI, printing_dpi);
  if (is_compound (old_info_flag) || !ends (old_info_flag->label, "paper"))
    env->write (INFO_FLAG, "none");
  env->style_init_env ();
  env->update ();
}

void
edit_main_rep::snippet_env_end () {
  typeset_prepare ();
  env->style_init_env ();
  env->update ();
}

// What the typesetting of a snippet reads, from its environment, recorded
// as for a bridge (bridge_rep::typeset), and kept by snippet_record
struct snippet_inputs {
  unsigned long long state;
  tree zoom_at;
  env_id_set reads;
  array<string> keys;
  array<tree> values;
  array<int> vars;
  unsigned long long vals;
  edit_env_rep::read_record rec;
  snippet_inputs (edit_env env):
    state (env->derived_state ()), zoom_at (env->zoom_value ()), vals (0),
    rec (env.operator-> (), &reads, &keys, &values, &vars, &vals) {}
};

void
edit_main_rep::snippet_record (tree t, snippet_inputs& in) {
  // a snippet whose inputs are all known gets a number (html_snippet_made)
  bool known= in.rec.complete () && N(in.vars) <= INPUTS_MAX &&
              N(in.keys) <= LOOKUPS_MAX;
  in.rec.stop ();
  html_snippet_made= -1;
  if (!known) return;
  html_snippet s;
  s.id= html_snippet_count++;
  s.state= in.state;
  s.vars= in.vars;
  s.vals= in.vals;
  s.keys= in.keys;
  s.values= in.values;
  s.zoom= in.rec.zoom;
  s.zoom_at= in.zoom_at;
  // (a new array: that of the default value is shared by the others)
  if (!html_snippets->contains (t))
    html_snippets (t)= array<html_snippet> ();
  html_snippets (t) << s;
  html_snippet_made= s.id;
}

int
edit_main_rep::html_snippet_id (tree t, int env_nr) {
  // the number of a snippet typeset before from t, whose inputs have the
  // values which they have in environment env_nr, or -1
  editor_rep* ed= recording_editor (env_nr);
  if (ed != NULL && ed != (editor_rep*) this)
    return ed->html_snippet_id (t, env_nr);
  if (!html_snippets->contains (t)) return -1;
  array<html_snippet> a= html_snippets[t];
  snippet_env_begin (env_nr);
  unsigned long long state= env->derived_state ();
  int r= -1;
  for (int i=N(a)-1; i>=0 && r < 0; i--)  // (the last ones first)
    if (a[i].state == state && env->inputs_hash (a[i].vars, false) == a[i].vals
        && (!a[i].zoom || env->zoom_value () == a[i].zoom_at)) {
      unsigned long long stamp= 0;
      if (same_lookups (env, a[i].keys, a[i].values, stamp)) r= a[i].id;
    }
  snippet_env_end ();
  return r;
}

int
edit_main_rep::html_snippet_last () {
  return html_snippet_made;
}

scheme_tree
edit_main_rep::html_math_snippet (tree t, int env_nr, bool breakable) {
  // by the editor of the exported buffer, which need not be the current one
  editor_rep* ed= recording_editor (env_nr);
  if (ed != NULL && ed != (editor_rep*) this)
    return ed->html_math_snippet (t, env_nr, breakable);
  snippet_env_begin (env_nr);
  snippet_inputs in (env);
  int dpi= as_int (printing_dpi);
  SI  em = (SI) (env->get_int (FONT_BASE_SIZE) * (dpi / 72.0) * PIXEL);
  color inherit= env->pen->get_color ();
  // a displayed formula is not broken
  if (display_style (t, env->get_bool (MATH_DISPLAY))) breakable= false;
  // the targets of links are only asked with a repository, which keeps nothing:
  // the formula is not in the document, and its trees do not last
  link_repository old_link_env= env->link_env;
  link_repository link_env (true);
  link_env->record= false;
  env->link_env= link_env;
  array<box> bs;
  array<SI>  gaps;
  if (breakable) bs= typeset_as_segments (env, t, path (), 12*em, 4*em, gaps);
  else {
    bs << typeset_as_box (env, t, path ());
    gaps << ((SI) 0);
  }
  snippet_record (t, in);
  env->link_env= old_link_env;
  snippet_env_end ();
  scheme_tree r (TUPLE);
  for (int i=0; i<N(bs); i++) {
    box b= bs[i];
    renderer ren= html_math_renderer (em, inherit);
    rectangles rs;
    b->redraw (ren, path (0), rs);
    scheme_tree s= html_math_svg (ren, b->x1, b->y1, b->x2 + gaps[i], b->y2);
    tm_delete (ren);
    if (!is_atomic (s)) return tree (TUPLE);
    r << s;
  }
  return N(r) == 1? r[0]: r;
}

array<SI>
edit_main_rep::print_snippet (url name, tree t, bool conserve_preamble) {
  tree buft= subtree (et, rp);
  if (conserve_preamble)
    if (is_document (buft) && is_compound (buft[0], "hide-preamble"))
      t= tree (SURROUND, buft[0], "", t);

  typeset_prepare ();
  tree old_dpi= env->read (DPI);
  tree old_info_flag= env->read (INFO_FLAG);
  env->write (DPI, printing_dpi);
  if (is_compound (old_info_flag) || !ends (old_info_flag->label, "paper"))
    env->write (INFO_FLAG, "none");
  env->style_init_env ();
  env->update ();
  typeset_folds= false;
  box b= typeset_as_box (env, t, path ());
  typeset_folds= true;
  env->write (DPI, old_dpi);
  env->write (INFO_FLAG, old_info_flag);
  env->style_init_env ();
  env->update ();
  return print_snippet_box (name, b, env->get_int (FONT_BASE_SIZE));
}

array<SI>
edit_main_rep::print_snippet_in (url name, tree t, int env_nr) {
  // a formula of the HTML export, in its environment (see html_math_snippet)
  editor_rep* ed= recording_editor (env_nr);
  if (ed != NULL && ed != (editor_rep*) this)
    return ed->print_snippet_in (name, t, env_nr);
  snippet_env_begin (env_nr);
  snippet_inputs in (env);
  box b= typeset_as_box (env, t, path ());
  int base_size= env->get_int (FONT_BASE_SIZE);
  snippet_record (t, in);
  snippet_env_end ();
  return print_snippet_box (name, b, base_size);
}

array<SI>
edit_main_rep::print_snippet_box (url name, box b, int base_size) {
  // b, to the image name, and its extents, in the units of base_size
  string s= suffix (name);
  bool bitmap=
    (s == "png" || s == "jpg" || s == "jpeg" || s == "tif" || s == "tiff");
#if !defined (QTTEXMACS) && !defined (VUETEXMACS) && !defined (AQUATEXMACS)
  bitmap= false;
#endif
  bool ps= (s == "ps" || s == "eps");
  if (use_pdf ()) ps= (ps || s == "pdf");
  int dpi= as_int (printing_dpi);
  if (b->x4 - b->x3 >= 5*PIXEL && b->y4 - b->y3 >= 5*PIXEL) {
    if (bitmap) make_raster_image (name, b, 5.0);
    else if (ps) make_eps (name, b, dpi);
    else {
      url temp= url_temp (use_pdf ()? ".pdf": ".eps");
      make_eps (temp, b, dpi);
      ::remove (name);
      if (!call_scm_converter (temp, name)) {
        call_imagemagick_convert (temp, name);
        if (!exists (name))
          convert_error << "could not convert snippet " << temp
                        << " into :" << name << "\n";
      }
      ::remove (temp);
    }
  }
  array<SI> a;
  a << b->x3 << b->y3 << b->x4 << b->y4 << b->x1 << b->y1 << b->x2 << b->y2;
  a << ((SI) base_size) << ((SI) dpi);
  return a;
}

bool
edit_main_rep::graphics_file_to_clipboard (url name) {
#ifdef QTTEXMACS
  // false without a clipboard of the system (-headless, #708)
  return the_gui->put_graphics_on_clipboard (name);
#elif defined (AQUATEXMACS)
  bool ns_put_graphics_on_clipboard (url file);
  return ns_put_graphics_on_clipboard (name);
#elif defined (VUETEXMACS)
  bool vue_put_graphics_on_clipboard (url file);
  return vue_put_graphics_on_clipboard (name);
#else 
  return false;
#endif
}

/******************************************************************************
* Evaluation of expressions
******************************************************************************/

void
edit_main_rep::footer_eval (string s) {
  // s= unslash (s); // FIXME: dirty fix; should not be necessary
  s= tm_decode (s);
  string r= object_to_string (eval (s));
  set_message (verbatim (r), "evaluate expression");
}

tree
edit_main_rep::the_line () {
  path p= search_parent_upwards (DOCUMENT);
  return copy (subtree (et, p));
}

tree
edit_main_rep::the_root () {
  return et;
}

tree
edit_main_rep::the_buffer () {
  return subtree (et, rp);
}

bool
edit_main_rep::test_subtree (path p) {
  return has_subtree (et, p);
}

tree
edit_main_rep::the_subtree (path p) {
  return subtree (et, p);
}

path
edit_main_rep::the_buffer_path () {
  return copy (rp);
}

path
edit_main_rep::the_path () {
  return copy (tp);
}

path
edit_main_rep::the_shifted_path () {
  return shift (et, tp, 1);
}

path
the_editor_path () {
  return get_current_editor()->the_path ();
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
stretched_print (box b) {
  if (N(b) == 0) cout << b << " " << reverse (b->ip) << LF;
  else {
    tree t= (tree) b;
    if (is_tuple (t) && N(t) > 0) t= t[0];
    cout << t << " " << reverse (b->ip) << LF << INDENT;
    for (int i=0; i<N(b); i++)
      stretched_print (b[i]);
    cout << UNINDENT;
  }
}

void
edit_main_rep::show_tree () {
  stretched_print (et, true);
  // cout << et << "\n";
}

void
edit_main_rep::show_box () {
  stretched_print (eb);
}

// Read-only access to the typeset boxes, for tools that inspect how the
// document was typeset (the font inspector and the font report)
box
edit_main_rep::get_box_root () {
  return eb;
}

path
edit_main_rep::get_box_path_at (bool at_mouse) {
  // the box path of the cursor, or of the last position of the mouse, in
  // the root box; the empty path when there is none
  if (is_nil (eb)) return path ();
  bool found= false;
  path bp= at_mouse? eb->find_box_path (last_x, last_y, 0, false, found):
                     eb->find_box_path (tp, found);
  return found? bp: path ();
}

void
edit_main_rep::show_env () {
  cout << env << "\n";
}

void
edit_main_rep::show_path () {
  cout << tp << "\n";
}

void
edit_main_rep::show_cursor () {
  cout << "Principal cursor: "
       << cu->ox << ", " << cu->oy << " [" << cu->delta << "], "
       << cu->y1 << " : " << cu->y2 << ", " << cu->slope << "\n";
  cout << "Ghost cursor    : "
       << mv->ox << ", " << mv->oy << " [" << mv->delta << "], "
       << mv->y1 << " : " << mv->y2 << ", " << mv->slope << "\n";
}

void
edit_main_rep::show_selection () {
  selection sel; selection_get (sel);
  cout << "physical  selection: " << cur_sel << "\n";
  cout << "logical   selection: " << sel->start << " --- " << sel->end << "\n";
}

array<int>
edit_main_rep::get_selection_rectangles () {
  // the rectangles of the selection as it is drawn (x1 y1 x2 y2 each), for
  // the tests
  array<int> r;
  for (rectangles rs= selection_rects; !is_nil (rs); rs= rs->next) {
    rectangle q= rs->item;
    r << (int) q->x1 << (int) q->y1 << (int) q->x2 << (int) q->y2;
  }
  return r;
}

void
edit_main_rep::show_meminfo () {
  mem_info ();
}

void
edit_main_rep::edit_special () {
}

#ifdef UNCOMMENTED
void test_commute ();
void test_invert ();
#endif

void
edit_main_rep::edit_test () {
  cout << "Test !\n";
#ifdef UNCOMMENTED
  test_commute();
  test_invert();
#endif
}
