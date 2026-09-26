/******************************************************************************
* MODULE     : mupdf_attachments.cpp
* DESCRIPTION: The TeXmacs source of a PDF, taken back out of it, with MuPDF
* COPYRIGHT  : (C) 2023 Tangdouer (the linked files, from
*                  Plugins/Pdf/pdf_hummus_extract_attachment.cpp)
*              (C) 2026 Massimiliano Gubinelli (the extraction with MuPDF)
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// "Pdf with embedded document" (File -> Export / Import, tm-print.scm and
// file-menu.scm) puts the document and the files it links to -- images,
// included documents, styles of its own -- into the PDF as attachments, the
// document first, and takes them back out. pdf_hummus_*_attachment.cpp do it
// with PDFHummus; this is the same with MuPDF, for the builds without it
// (the embedding itself is mupdf_pdf_make_attachments, in
// mupdf_pdf_renderer.cpp). The files come out in the same order and under
// the same names, so that a PDF made by either is read by the other.
//
// Where this differs from Hummus: the attachments are written into a
// directory of their own, not next to the PDF -- the document of paper.pdf
// is paper.tm, and next to paper.pdf there is often a paper.tm already,
// which Hummus overwrote -- and the name an attachment has in the PDF is
// reduced to a file name, so that a PDF cannot write elsewhere ("../x").

#include "Pdf/pdf_hummus_extract_attachment.hpp"
#include "mupdf_renderer.hpp"   // mupdf_context
#include "analyze.hpp"
#include "converter.hpp"
#include "file.hpp"
#include "hashset.hpp"
#include "sys_utils.hpp"

#include <mupdf/fitz.h>
#include <mupdf/pdf.h>

/******************************************************************************
* The files a document links to (as pdf_hummus_extract_attachment.cpp)
******************************************************************************/

static hashset<string> internal_styles;

static void
declare_style (url u) {
  if (is_or (u)) {
    declare_style (u[1]);
    declare_style (u[2]);
  }
  else if (is_concat (u)) {
    string dir= upcase_first (as_string (u[1]));
    if (dir == "CVS" || dir == ".svn")
      ;
    else declare_style (u[2]);
  }
  else if (is_atomic (u)) {
    string s= as_string (u);
    if (ends (s, ".ts") && !starts (s, "source")) {
      internal_styles->insert (s (0, N (s) - 3));
      if (starts (s, "old-")) internal_styles->insert (s (4, N (s) - 3));
      if (starts (s, "old2-")) internal_styles->insert (s (5, N (s) - 3));
    }
  }
}

// Determine whether the style is a internal style
static bool
is_internal_style (string style) {
  if (N (internal_styles) == 0) {
    url sty_u= descendance ("$TEXMACS_PATH/styles");
    declare_style (sty_u);
    sty_u= descendance ("$TEXMACS_PATH/packages");
    declare_style (sty_u);
  }
  return internal_styles->contains (style);
}

// Pass in a image or include tree.
// return all include or image file url
static url
get_url_image_or_include_tree (tree t, url path) {
  if (is_atomic (t[0])) {
    url pre_url= url (get_label (t[0]));
    if (!exists (pre_url)) {
      pre_url= relative (path, pre_url);
      if (!exists (pre_url)) {
        if (DEBUG_CONVERT) debug_convert << pre_url << " do not exist" << LF;
      }
    }
    return pre_url;
  }
  else {
    if ((DEBUG_CONVERT) && is_func (t, INCLUDE))
      debug_convert << t << " include tree format wrong" << LF;
  }
  return url_none ();
}

// Pass in a tree with style label.
// return a actual ts file url
static url
get_actual_style_url (string style_name, url path) {
  url style_file;
  if (!is_internal_style (style_name)) {
    style_file= glue (url (style_name), ".ts");
    if (!exists (style_file)) {
      style_file= relative (path, style_file);
      if (!exists (style_file)) {
        if (DEBUG_CONVERT) debug_convert << style_file << "do not exist" << LF;
        style_file= url_none ();
      }
    }
  }
  return style_file;
}

// Pass in a style tree.
// return all external ts file url
static array<url>
get_url_style_tree (tree t, url path) {
  array<url> style_file;
  if (N (t) == 0) return style_file;
  if (get_label (t[0]) == "tuple") {
    for (int i= 0; i < N (t[0]); i++) {
      url style_url= get_actual_style_url (get_label (t[0][i]), path);
      if (!is_none (style_url)) style_file << style_url;
    }
  }
  else {
    if (!is_atomic (t[0])) {
      if (DEBUG_CONVERT)
        debug_convert << get_label (t[0]) << "is not atomic tree" << LF;
      return style_file;
    }
    url style_url= get_actual_style_url (get_label (t[0]), path);
    if (!is_none (style_url)) style_file << style_url;
  }
  return style_file;
}

array<url>
get_linked_file_paths (tree t, url path) {
  array<url> tm_and_linked_file;
  string     label= get_label (t);
  if (label == "image" || label == "include") {
    url incl_url= get_url_image_or_include_tree (t, path);
    if (incl_url != url ()) tm_and_linked_file << incl_url;
    return tm_and_linked_file;
  }
  if (label == "style") return get_url_style_tree (t, path);
  if (!is_atomic (t))
    for (int i= 0; i < N (t); i++)
      tm_and_linked_file << get_linked_file_paths (t[i], path);
  return tm_and_linked_file;
}

// Pass in an image or include tree and a path.
// change the url in tree to a url with the same path as the path.
static tree
replace_url_image_or_include_tree (tree t, url path) {
  if (get_label (t) != "image" && get_label (t) != "include") {
    if (DEBUG_CONVERT)
      debug_convert << get_label (t) << " is not image or include" << LF;
    return t;
  }
  if (is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is atomic" << LF;
    return t;
  }
  if (is_atomic (t[0])) {
    url pre_url= url (get_label (t[0]));
    if (!exists (pre_url)) {
      pre_url= relative (path, pre_url);
      if (!exists (pre_url)) {
        if (DEBUG_CONVERT) debug_convert << pre_url << " do not exist" << LF;
      }
    }
    string name= as_string (tail (pre_url));
    if (path != url ()) {
      name= as_string (relative (path, name));
    }
    t[0]->label= string (name);
  }
  else {
    if ((DEBUG_CONVERT) && is_func (t, INCLUDE))
      debug_convert << t << " include tree format wrong" << LF;
  }
  return t;
}

// Pass in an tree with style label and a path.
// change the label to a url with the same path as the path.
static tree
repalce_url_style (tree t, url path) {
  if (!is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is not atomic" << LF;
    return t;
  }
  string style_name= get_label (t);
  if (!is_internal_style (style_name)) {
    url style_url= url (style_name);
    style_url    = glue (style_url, ".ts");
    if (!exists (style_url)) {
      style_url= relative (path, style_url);
      if (!exists (style_url)) {
        if (DEBUG_CONVERT) debug_convert << style_url << "do not exist" << LF;
      }
    }
    string name= basename (style_url);
    if (path != url ()) {
      name= as_string (relative (path, name));
    }
    t->label= name;
  }
  return t;
}

// Pass in an style tree and a path.
// change the urls in style tree to a url with the same path as the path.
static tree
replace_url_style_tree (tree t, url path) {
  if (get_label (t) != "style") {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is not style" << LF;
    return t;
  }
  if (is_atomic (t)) {
    if (DEBUG_CONVERT) debug_convert << get_label (t) << " is atomic" << LF;
    return t;
  }
  if (is_tuple (t[0]))
    for (int i= 0; i < N (t[0]); i++)
      repalce_url_style (t[0][i], path);
  else repalce_url_style (t[0], path);
  return t;
}

tree
replace_with_relative_path (tree t, url path) {
  string label= get_label (t);
  if (label == "image" || label == "include") {
    replace_url_image_or_include_tree (t, path);
    return t;
  }
  if (label == "style") {
    replace_url_style_tree (t, path);
    return t;
  }
  if (!is_atomic (t))
    for (int i= 0; i < N (t); i++)
      replace_with_relative_path (t[i], path);
  return t;
}

/******************************************************************************
* Taking the attachments out
******************************************************************************/

// The attachments of a name tree, in order: its /Names, then its /Kids.
// C, since it runs inside an fz_try; what it finds goes in MuPDF memory.
struct found_attachments {
  int n, cap;
  char** name;       // the key, as its bytes (fz_malloc)
  int* len;          // their number
  pdf_obj** file;    // the embedded file stream (kept)
};

static void
found_add (fz_context* ctx, found_attachments* f, pdf_obj* key, pdf_obj* fs) {
  pdf_obj* ef= pdf_dict_get (ctx, fs, PDF_NAME(EF));
  pdf_obj* st= pdf_dict_get (ctx, ef, PDF_NAME(F));
  if (st == NULL) st= pdf_dict_get (ctx, ef, PDF_NAME(UF));
  if (st == NULL || !pdf_is_stream (ctx, st) || !pdf_is_string (ctx, key)) return;
  if (f->n == f->cap) {
    int cap= f->cap == 0 ? 8 : 2 * f->cap;
    f->name= (char**) fz_realloc (ctx, f->name, cap * sizeof (char*));
    f->len= (int*) fz_realloc (ctx, f->len, cap * sizeof (int));
    f->file= (pdf_obj**) fz_realloc (ctx, f->file, cap * sizeof (pdf_obj*));
    f->cap= cap;
  }
  int l= (int) pdf_to_str_len (ctx, key);
  char* s= (char*) fz_malloc (ctx, l + 1);
  memcpy (s, pdf_to_str_buf (ctx, key), l);
  s[l]= 0;
  f->name[f->n]= s;
  f->len[f->n]= l;
  f->file[f->n]= pdf_keep_obj (ctx, st);
  f->n++;
}

static void
walk_name_tree (fz_context* ctx, pdf_obj* node, found_attachments* f, int depth) {
  if (node == NULL || depth > 32) return;   // 32: against a loop in the tree
  pdf_obj* names= pdf_dict_get (ctx, node, PDF_NAME(Names));
  for (int i=0; i+1 < pdf_array_len (ctx, names); i += 2)
    found_add (ctx, f, pdf_array_get (ctx, names, i),
               pdf_array_get (ctx, names, i+1));
  pdf_obj* kids= pdf_dict_get (ctx, node, PDF_NAME(Kids));
  for (int i=0; i < pdf_array_len (ctx, kids); i++)
    walk_name_tree (ctx, pdf_array_get (ctx, kids, i), f, depth + 1);
}

// The file name an attachment is written under: the key, decoded (UTF-16
// after a byte order mark, as MuPDF writes a text string; UTF-8 or ASCII
// otherwise, as Hummus writes it), and reduced to its last component
static string
attachment_file_name (const char* s, int l) {
  string r;
  if (l >= 2 && ((unsigned char) s[0]) == 0xFE && ((unsigned char) s[1]) == 0xFF) {
    for (int i=2; i+1 < l; i += 2) {
      unsigned int c= (((unsigned char) s[i]) << 8) | ((unsigned char) s[i+1]);
      if (c >= 0xD800 && c < 0xDC00 && i+3 < l) {
        unsigned int d= (((unsigned char) s[i+2]) << 8) | ((unsigned char) s[i+3]);
        c= 0x10000 + ((c - 0xD800) << 10) + (d - 0xDC00);
        i += 2;
      }
      r << encode_as_utf8 (c);
    }
  }
  else r= string (s, l);
  int k= max (search_backwards ("/", r), search_backwards ("\\", r));
  if (k >= 0) r= r (k+1, N(r));
  if (r == "" || r == "." || r == "..") return "";
  for (int i=0; i<N(r); i++) if (((unsigned char) r[i]) < 32) return "";
  return r;
}

// a directory of its own for the attachments of one PDF
static url
attachment_directory () {
  static int count= 0;
  url dir= url_temp_dir () * url ("pdf-attachments-" * as_string (++count));
  if (!exists (dir)) mkdir (dir);
  return dir;
}

bool
extract_attachments_from_pdf (url pdf_path, list<url>& names) {
  fz_context* ctx= mupdf_context ();
  // made before fz_try: a throw is a longjmp, which skips destructors
  c_string path (concretize (pdf_path));
  pdf_document* doc= NULL;
  found_attachments f= { 0, 0, NULL, NULL, NULL };
  bool ok= false;
  fz_var (doc); fz_var (ok);
  fz_try (ctx) {
    doc= pdf_open_document (ctx, path);
    pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    pdf_obj* ef= pdf_dict_get (ctx, pdf_dict_get (ctx, root, PDF_NAME(Names)),
                               PDF_NAME(EmbeddedFiles));
    walk_name_tree (ctx, ef, &f, 0);
    ok= true;
  }
  fz_catch (ctx) {
    if (DEBUG_CONVERT)
      debug_convert << "MuPDF cannot read the attachments of " << pdf_path
                    << ": " << fz_caught_message (ctx) << LF;
    ok= false;
  }
  if (ok && f.n == 0) {
    if (DEBUG_CONVERT) debug_convert << pdf_path << " has no attachments" << LF;
    ok= false;
  }
  url dir= ok ? attachment_directory () : url_none ();
  for (int i=0; i<f.n; i++) {
    string nm= ok ? attachment_file_name (f.name[i], f.len[i]) : string ();
    if (N(nm) > 0) {
      url dest= dir * url (nm);
      c_string cdest (concretize (dest));
      fz_buffer* buf= NULL;
      bool saved= false;
      fz_var (buf); fz_var (saved);
      fz_try (ctx) {
        buf= pdf_load_stream (ctx, f.file[i]);
        fz_save_buffer (ctx, buf, cdest);
        saved= true;
      }
      fz_always (ctx) { fz_drop_buffer (ctx, buf); }
      fz_catch (ctx) {
        convert_warning << "MuPDF cannot take " << nm << " out of " << pdf_path
                        << ": " << fz_caught_message (ctx) << LF;
      }
      if (saved) names= names * dest;
      else ok= false;
    }
    fz_free (ctx, f.name[i]);
    pdf_drop_obj (ctx, f.file[i]);
  }
  fz_free (ctx, f.name); fz_free (ctx, f.len); fz_free (ctx, f.file);
  pdf_drop_document (ctx, doc);
  return ok && N(names) > 0;
}

bool
scm_extract_attachments (url pdf_path) {
  list<url> paths;
  return extract_attachments_from_pdf (pdf_path, paths);
}

// the document of the PDF: the first attachment, as Hummus puts it first
url
get_main_tm (url pdf_path) {
  list<url> paths;
  if (!extract_attachments_from_pdf (pdf_path, paths) || is_nil (paths))
    return url_none ();
  return paths[0];
}
