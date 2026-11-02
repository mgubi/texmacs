
/******************************************************************************
* MODULE     : new_buffer.cpp
* DESCRIPTION: Buffer management
* COPYRIGHT  : (C) 1999-2022  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_data.hpp"
#include "convert.hpp"
#include "file.hpp"
#include "web_files.hpp"
#include "tm_link.hpp"
#include "message.hpp"
#include "dictionary.hpp"
#include "new_document.hpp"
#include "merge_sort.hpp"
#include "iterator.hpp"
#include <time.h>

array<tm_buffer> bufs;

string propose_title (string old_title, url u, tree doc);

/******************************************************************************
* Check for changes in the buffer
******************************************************************************/

void
tm_buffer_rep::attach_notifier () {
  if (notify) return;
  string id= as_string (buf->name, URL_UNIX);
  tree& st (subtree (the_et, rp));
  call ("buffer-initialize", id, st, buf->name);
  lns= link_repository (true);
  lns->insert_locus (id, st, "buffer-notify");
  notify= true;
}

bool
tm_buffer_rep::needs_to_be_saved () {
  if (buf->read_only) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save ())
      return true;
  return false;
}

bool
tm_buffer_rep::needs_to_be_autosaved () {
  if (buf->read_only) return false;
  for (int i=0; i<N(vws); i++)
    if (vws[i]->ed->need_save (false))
      return true;
  return false;
}

/******************************************************************************
* Manipulation of buffer list
******************************************************************************/

void
insert_buffer (url name) {
  if (is_none (name)) return;
  if (!is_nil (concrete_buffer (name))) return;
  tm_buffer buf= tm_new<tm_buffer_rep> (name);
  bufs << buf;
}

void
remove_buffer (tm_buffer buf) {
  int nr, n= N(bufs);
  for (nr=0; nr<n; nr++)
    if (bufs[nr] == buf) {
      array<tm_view> vws= copy (buf->vws);
      for (int i=0; i<N(vws); i++)
        delete_view (abstract_view (vws[i]));
      if (n == 1 && number_of_servers () == 0)
        get_server () -> quit ();
      for (int i=nr; i<n-1; i++)
        bufs[i]= bufs[i+1];
      bufs->resize (n-1);
      tm_delete (buf);
      return;
    }
}

void
remove_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (!is_nil (buf)) remove_buffer (buf);
}

int
number_buffers () {
  return N(bufs);
}

array<url>
get_all_buffers () {
  array<url> r;
  for (int i=N(bufs)-1; i>=0; i--)
    r << bufs[i]->buf->name;
  return r;
}

tm_buffer
concrete_buffer (url name) {
  int i, n= N(bufs);
  for (i=0; i<n; i++)
    if (bufs[i]->buf->name == name)
      return bufs[i];
  return nil_buffer ();
}

tm_buffer
concrete_buffer_insist (url u) {
  tm_buffer buf= concrete_buffer (u);
  if (!is_nil (buf)) return buf;
  buffer_load (u);
  return concrete_buffer (u);
}

/******************************************************************************
* Buffer names
******************************************************************************/

url
get_current_buffer () {
  tm_view vw= concrete_view (get_current_view ());
  return vw->buf->buf->name;
}

url
get_current_buffer_safe () {
  url v= get_current_view_safe ();
  if (is_none (v)) return v;
  return concrete_view (v)->buf->buf->name;
}

url
path_to_buffer (path p) {
  int i;
  for (i=0; i<N(bufs); i++)
    if (bufs[i]->rp <= p)
      return bufs[i]->buf->name;
  return url_none ();
}

static void
set_window_urls (url name) {
  // tell the windows of the views of the buffer its url
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    tm_window win= concrete_window (view_to_window (vs[i]));
    if (win != NULL) win->set_window_url (name);
  }
}

void
rename_buffer (url name, url new_name) {
  if (new_name == name || is_nil (concrete_buffer (name))) return;
  kill_buffer (new_name);
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  notify_rename_before (name);
  buf->buf->name= new_name;
  buf->buf->master= new_name;
  array<url> vs= buffer_to_views (new_name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_change (THE_ENVIRONMENT);
  notify_rename_after (new_name);
  tree doc= subtree (the_et, buf->rp);
  string title= propose_title (buf->buf->title, new_name, doc);
  // the windows need the new url even when the title stays the same
  // (Save as to the same file name in another folder)
  if (buf->buf->title == title) set_window_urls (new_name);
  else set_title_buffer (new_name, title);
}

url
make_new_buffer () {
  int i=1;
  while (true) {
    url name= url_scratch ("no_name_", ".tm", i);
    if (is_nil (concrete_buffer (name))) {
      set_buffer_tree (name, tree (DOCUMENT));
      return name;
    }
    else i++;
  }
}

bool
buffer_has_name (url name) {
  return !is_scratch (name);
}

/******************************************************************************
* Buffer title
******************************************************************************/

string
propose_title (string old_title, url u, tree doc) {
  string name= as_string (tail (u));
  if (starts (name, "no_name_") && ends (name, ".tm")) {
    string no_name= translate ("No name");
    for (int i=0; i<N(no_name); i++)
      if (((unsigned char) (no_name[i])) >= (unsigned char) 128)
        { no_name= "No name"; break; }
    name= no_name * " [" * name (8, N(name) - 3) * "]";
  }
  if ((name == "") || (name == "."))
    name= as_string (tail (u * url_parent ()));
  if ((name == "") || (name == "."))
    name= as_string (u);
  if (is_rooted_tmfs (u))
    name= as_string (call ("tmfs-title", as_string (u), object (doc)));

  int i, j;
  for (j=1; true; j++) {
    bool flag= true;
    string ret (name);
    if (j>1) ret= name * " (" * as_string (j) * ")";
    if (ret == old_title) return ret;
    for (i=0; i<N(bufs); i++)
      if (bufs[i]->buf->title == ret) flag= false;
    if (flag) return ret;
  }
}

string
get_title_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  return buf->buf->title;
}

void
set_title_buffer (url name, string title) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->title == title) return;
  buf->buf->title= title;
  set_window_urls (name);
  update_window_names (name);
}

/******************************************************************************
* Setting and getting the buffer tree contents
******************************************************************************/

void
set_buffer_data (url name, new_data data) {
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++) {
    view_to_editor (vs[i]) -> set_data (data);
    view_to_editor (vs[i]) -> init_update ();
  }
}

void
set_buffer_tree (url name, tree doc) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) {
    insert_buffer (name);
    buf= concrete_buffer (name);
    tree body= detach_data (doc, buf->data);
    set_document (buf->rp, body);
    buf->buf->title= propose_title (buf->buf->title, name, body);
    if (buf->data->project != "") {
      url prj_name= head (name) * as_string (buf->data->project);
      buf->prj= concrete_buffer_insist (prj_name);
    }
  }
  else {
    string old_title= buf->buf->title;
    string old_project= buf->data->project->label;
    tree body= detach_data (doc, buf->data);
    assign (buf->rp, body);
    set_buffer_data (name, buf->data);
    buf->buf->title= propose_title (old_title, name, body);
    if (buf->data->project != "" && buf->data->project != old_project) {
      url prj_name= head (name) * as_string (buf->data->project);
      buf->prj= concrete_buffer_insist (prj_name);
    }
  }
  pretend_buffer_saved (name);
}

tree
get_buffer_tree (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  tree body= subtree (the_et, buf->rp);
  if (N(buf->vws) == 0) return attach_data (body, buf->data, true);
  // the editor keeps its own copies of the style and initial environment
  tm_view vw= concrete_view (get_recent_view (name));
  if (vw == NULL) return attach_data (body, buf->data, true);
  new_data data;
  data->project= buf->data->project;
  vw->ed->get_data (data);
  return attach_data (body, data, true);
}

tree
get_buffer_style (url name) {
  // the style of the buffer, in its most recent view (each editor keeps a
  // copy of its own, which change_style gives all the views: #535)
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  if (N(buf->vws) == 0) return copy (buf->data->style);
  tm_view vw= concrete_view (get_recent_view (name));
  if (vw == NULL) return copy (buf->data->style);
  return copy (vw->ed->get_style ());
}

void
set_buffer_style (url name, tree style) {
  // the style of the buffer, in its data and in all its views (the first
  // change_style gives it to all of them: #535)
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  buf->data->style= copy (style);
  for (int i=0; i<N(buf->vws); i++)
    buf->vws[i]->ed->change_style (style);
}

void
set_buffer_body (url name, tree body) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) {
    new_data data;
    set_buffer_tree (name, attach_data (body, data));
  }
  else {
    assign (buf->rp, body);
    pretend_buffer_saved (name);
  }
}

tree
get_buffer_body (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return "";
  return subtree (the_et, buf->rp);
}

/******************************************************************************
* Further information attached to buffers
******************************************************************************/

url
get_master_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return url_none ();
  return buf->buf->master;
}

void
set_master_buffer (url name, url master) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  if (buf->buf->master == master) return;
  buf->buf->master= master;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_change (THE_ENVIRONMENT);
}

void
set_last_save_buffer (url name, int t) {
  tm_buffer buf= concrete_buffer (name);
  if (!is_nil (buf)) buf->buf->last_save= t;
  //cout << "Set last save " << name << " -> " << t << "\n";
}

int
get_last_save_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) {
    //cout << "Get last save " << name << " -> *\n";
    return - (int) (((unsigned int) (-1)) >> 1);
  }
  //cout << "Get last save " << name << " -> " << buf->buf->last_save << "\n";
  return (int) buf->buf->last_save;
}

bool
is_aux_buffer (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->buf->master != buf->buf->name;
}

double
last_visited (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return (double) texmacs_time ();
  return (double) buf->buf->last_visit;
}

bool
buffer_modified (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->needs_to_be_saved ();
}

// A read-only buffer can be read, searched, selected and copied, but what
// one does in its editor does not change it: the changes made by a key, a
// mouse action, a drop or a menu command are cancelled at the end of the
// event (edit_modify_rep::end_editing), and there is no undo. The program
// still changes it (the messages of the console, debug-widgets.scm)
void
set_buffer_read_only (url name, bool flag) {
  tm_buffer buf= concrete_buffer (name);
  if (!is_nil (buf)) buf->buf->read_only= flag;
}

bool
is_buffer_read_only (url name) {
  tm_buffer buf= concrete_buffer (name);
  return !is_nil (buf) && buf->buf->read_only;
}

bool
buffer_modified_since_autosave (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return false;
  return buf->needs_to_be_autosaved ();
}

void
pretend_buffer_modified (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> require_save ();
}

void
pretend_buffer_saved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_save ();
  set_last_save_buffer (name, last_modified (name));
}

void
pretend_buffer_autosaved (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  array<url> vs= buffer_to_views (name);
  for (int i=0; i<N(vs); i++)
    view_to_editor (vs[i]) -> notify_save (false);
}

void
attach_buffer_notifier (url name) {
  tm_buffer buf= concrete_buffer (name);
  if (is_nil (buf)) return;
  buf->attach_notifier ();
}

/******************************************************************************
* Loading
******************************************************************************/

tree
attach_subformat (tree t, url u, string fm) {
  if ((fm == "texmacs") || (fm == "tmml") || (fm == "stm")) return t;
  if (!format_exists (fm)) return t;

  string s= suffix (u);
  string inferred_fm= suffix_to_format (s);
  if (!is_empty (inferred_fm) && inferred_fm != "generic") fm= inferred_fm;
  if (fm == "verbatim") return t;
  if (!prog_lang_exists (fm) &&
      fm != "mathemagix" &&
      fm != "scilab" &&
      fm != "scheme") return t;

  hashmap<string,tree> h (UNINIT, extract (t, "initial"));
  h (MODE)= "prog";
  h (PROG_LANGUAGE)= fm;
  tree t2= change_doc_attr (t, "initial", make_collection (h));
  return change_doc_attr (t2, "style", tree ("code"));
}

tree
import_loaded_tree (string s, url u, string fm) {
  set_file_focus (u);
  if (fm == "generic" && suffix (u) == "txt") fm= "verbatim";
  if (fm == "generic") fm= get_format (s, suffix (u));
  if (fm == "texmacs" && starts (s, "(document (TeXmacs")) fm= "stm";
  if (fm == "verbatim" && starts (s, "(document (TeXmacs")) fm= "stm";
  tree t= generic_to_tree (s, fm * "-document");
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return attach_subformat (t, u, fm);
}

tree
import_loaded_tree_from_object (object s, url u) {
  set_file_focus (u);
  tree t= stree_to_tree (s);
  tree links= extract (t, "links");
  if (N (links) != 0)
    (void) call ("register-link-locations", object (u), object (links));
  return attach_subformat (t, u, "stm-document");
}

tree
import_tree (url u, string fm) {
  url r= resolve (u, "fr");
  if (is_none (r)) {
    url b= get_current_buffer ();
    r= resolve (b * url_parent () * u);
  }
  string s;
  if (is_none (r) || load_string (r, s, false)) return "error";
  set_file_focus (r);
  return import_loaded_tree (s, r, fm);
}

bool
buffer_import (url name, url src, string fm) {
  tree t= import_tree (src, fm);
  if (t == "error") return true;
  set_buffer_tree (name, t);
  return false;
}

bool
buffer_load (url name) {
  string fm= file_format (name);
  return buffer_import (name, name, fm);
}

hashmap<string,tree> style_tree_cache ("");

tree
load_style_tree (string package) {
  if (style_tree_cache->contains (package))
    return style_tree_cache [package];
  url name= url_none ();
  url styp= "$TEXMACS_STYLE_PATH";
  if (ends (package, ".ts")) name= package;
  else name= styp * (package * ".ts");
  name= resolve (name);
  string doc_s;
  if (!load_string (name, doc_s, false)) {
    tree doc= texmacs_document_to_tree (doc_s);
    if (is_compound (doc)) doc= extract (doc, "body");
    style_tree_cache (package)= doc;
    return doc;
  }
  style_tree_cache (package)= "";
  return "";
}

tree
with_package_definitions (string package, tree body) {
  // FIXME: it would be more robust to execute the package
  // yet an alternative idea is to fetch the complete environment of
  // a style using get_style_env.
  tree w (WITH);
  tree doc= load_style_tree (package);
  if (!is_func (doc, DOCUMENT)) return body;
  for (int i=0; i<N(doc); i++)
    if (is_func (doc[i], ASSIGN, 2))
      w << doc[i][0] << doc[i][1];
  w << body;
  return w;
}

/******************************************************************************
* Saving
******************************************************************************/

bool
export_tree (tree doc, url u, string fm) {
  tree aux= doc;
  // NOTE: hook for encryption
  // On failure, never fall back to saving the document in clear
  tree init= extract (aux, "initial");
  if (fm == "texmacs")
    for (int i=0; i<N(init); i++)
      if (is_func (init[i], ASSOCIATE, 2) && init[i][0] == "encryption" &&
          init[i][1] == "gpg-passphrase") {
        object r= call ("tree-export-encrypted", u, aux);
        if (!is_tree (r)) return true;
        aux= as_tree (r);
        break;
      }
  // END hook
  if (fm == "generic") fm= "verbatim";
  // The converters write the images of the document next to their target
  bool has_target= (fm != "texmacs" && fm != "stm");
  object old_target;
  if (has_target) {
    // A converter passes the target on to programs, which need its full name
    url target= is_rooted_name (u)? u: resolve (u, "");
    old_target= call ("save-target-swap", object (target));
  }
  // A converter which fails (an error, or no conversion) writes no file;
  // an error inside a conversion made in C++ (LaTeX) is caught there and
  // gives an empty text, which is then no conversion either
  int errors= scheme_caught_errors ();
  object r= call ("texmacs->generic", aux, fm * "-document");
  if (has_target) call ("save-target-swap", old_target);
  if (!is_string (r)) return true;
  string s= as_string (r);
  if (s == "* error: unknown format *" ||
      s == "Error: bad format or data") return true;
  if (s == "" && scheme_caught_errors () > errors) return true;
  return save_string (u, s);
}

bool
buffer_export (url name, url dest, string fm) {
  if (is_nil (concrete_buffer (name))) return true;
  tm_view vw= concrete_view (get_recent_view (name));
  ASSERT (vw != NULL, "view expected");

  if (fm == "postscript" || fm == "pdf")
    // by what the printer says: a time stamp in seconds does not change
    // when the same file is written twice in a second (#705)
    return !vw->ed->print_doc (dest, false, 1, 1000000);

  tree body= subtree (the_et, vw->buf->rp);
  if (fm == "verbatim")
    body= vw->ed->exec_verbatim (body);
  if (fm == "html")
    body= vw->ed->exec_html (body);
  //if (fm == "latex")
  //body= vw->ed->exec_latex (body);

  vw->ed->get_data (vw->buf->data);
  tree doc= attach_data (body, vw->buf->data, !vw->ed->get_save_aux());

  if (fm == "latex")
    doc= change_doc_attr (doc, "view", as_string (abstract_view (vw)));

  object arg1 (vw->buf->buf->name);
  object arg2 (body);
  tree links= as_tree (call ("get-link-locations", arg1, arg2));
  if (N (links) != 0)
    doc << compound ("links", links);

  // The converters find the images linked by the document next to its source
  bool has_source= (fm != "texmacs" && fm != "stm");
  object old_source;
  if (has_source) old_source= call ("save-source-swap", object (name));
  bool r= export_tree (doc, dest, fm);
  if (has_source) call ("save-source-swap", old_source);
  return r;
}

// The macros of a document are expanded by the editor of a view on it; a
// document converted without a view, as from Scheme, is left as it is

tree
latex_expand (tree doc, url name) {
  tm_view vw= concrete_view (get_recent_view (name));
  if (vw == NULL) return doc;
  tree body= vw->ed->exec_latex (extract (doc, "body"));
  return change_doc_attr (doc, "body", body);
}

tree
latex_expand (tree doc) {
  tm_view vw= concrete_view (url (as_string (extract (doc, "view"))));
  if (vw == NULL) return remove_doc_attr (doc, "view");
  tree body= vw->ed->exec_latex (extract (doc, "body"));
  doc= change_doc_attr (doc, "body", body);
  return remove_doc_attr (doc, "view");
}

bool
buffer_save (url name) {
  string fm= file_format (name);
  if (fm == "generic") fm= "verbatim";
  bool r= buffer_export (name, name, fm);
  if (!r) {
    reset_inclusion (name);
    pretend_buffer_saved (name);
    array<url> ws = buffer_to_windows (name);
    for (int i=0; i<N(ws); i++)
      concrete_window (ws[i])->set_modified (false);
  }
  return r;
}

/******************************************************************************
* Loading inclusions
******************************************************************************/

static hashmap<string,tree> document_inclusions ("");
static hashmap<string,int> inclusions_busy (0);
// (changed with the cache: the typesetter looks the inclusions up again
// then, see same_lookups in Typeset/Bridge/bridge.cpp)
static unsigned long long inclusions_version= 1;
// when and how the file of an entry was read (time of the file, its identity,
// and the time of the reading), so that a file which was not touched since
// need not be read and parsed again (refresh_inclusions)
static hashmap<string,tree> inclusion_stats ("");

static tree
stat_tuple (url name) {
  int rd= (int) time (NULL);
  return tuple (as_string (last_modified (name, false)),
                file_identity (name), as_string (rd));
}

unsigned long long
inclusions_stamp () {
  return inclusions_version;
}

void
reset_inclusions () {
  document_inclusions = hashmap<string,tree> ("");
  inclusion_stats = hashmap<string,tree> ("");
  inclusions_version++;
}

void
reset_inclusion (url name) {
  string name_s= as_string (name);
  document_inclusions -> reset (name_s);
  inclusion_stats -> reset (name_s);
  inclusions_version++;
}

// The inclusions are read again (Tools > Update > Inclusions, Update All):
// the entries whose file has the same contents keep their tree (the
// typesetter compares the included documents by pointer, see same_lookups
// in Typeset/Bridge/bridge.cpp: the layout of an include is kept when its
// file did not change), the others get the new tree, and a file which
// cannot be read any more is dropped. Tells whether something changed.
bool
refresh_inclusions () {
  bool changed= false;
  array<string> names;
  iterator<string> it= iterate (document_inclusions);
  while (it->busy ()) names << it->next ();
  for (int i=0; i<N(names); i++) {
    string name_s= names[i];
    tree old_doc= document_inclusions [name_s];
    // (a file whose time, size, change time and inode are those of the reading, which is more
    // than two seconds later than the time of the file, is the same)
    url file= url_system (name_s);
    if (inclusion_stats->contains (name_s)) {
      tree st= inclusion_stats [name_s];
      int mt= last_modified (file, false);
      string id= file_identity (file);
      if (as_int (st[0]) == mt && N(id) > 0 && as_string (st[1]) == id &&
          as_int (st[2]) > mt + 2 && mt > 0)
        continue;
    }
    tree doc= extract_document (import_tree (url_system (name_s), "generic"));
    if (is_func (doc, _ERROR)) {
      document_inclusions -> reset (name_s);
      inclusion_stats -> reset (name_s);
      changed= true;
    }
    else {
      inclusion_stats (name_s)= stat_tuple (file);
      if (doc != old_doc) {
        document_inclusions (name_s)= doc;
        changed= true;
      }
    }
  }
  if (changed) inclusions_version++;
  return changed;
}

tree
load_inclusion (url name) {
  // url name= relative (base_file_name, file_name);
  string name_s= as_string (name);
  if (document_inclusions->contains (name_s))
    return document_inclusions [name_s];
  tree doc= extract_document (import_tree (name, "generic"));
  if (!is_func (doc, _ERROR)) {
    document_inclusions (name_s)= doc;
    inclusion_stats (name_s)= stat_tuple (name);
    inclusions_version++;
  }
  return doc;
}

tree
inclusion_value (string name_s) {
  if (document_inclusions->contains (name_s))
    return document_inclusions [name_s];
  return load_inclusion (url_system (name_s));
}

bool
inclusion_busy (url name) {
  // Is the inclusion of name currently being processed (cycle detection)?
  return inclusions_busy [as_string (name)] > 0;
}

void
inclusion_enter (url name) {
  inclusions_busy (as_string (name)) ++;
}

void
inclusion_leave (url name) {
  string name_s= as_string (name);
  if (inclusions_busy [name_s] <= 1) inclusions_busy->reset (name_s);
  else inclusions_busy (name_s) --;
}
