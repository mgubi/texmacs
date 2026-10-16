
/******************************************************************************
* MODULE     : tex_files.cpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tex_files.hpp"
#include "boot.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "analyze.hpp"
#include "tm_timer.hpp"
#include "data_cache.hpp"

static url the_tfm_path= url_none ();
static url the_pk_path = url_none ();
static url the_pfb_path= url_none ();

/******************************************************************************
* Finding a TeX font
******************************************************************************/

static string
kpsewhich (string name) {
  bench_start ("kpsewhich");
  string which= var_eval_system ("kpsewhich " * name);
  bench_cumul ("kpsewhich");
  return which;
}

// The TFM and Type 1 files of the TeX distribution, by name: read once
// from the databases of kpathsea (the files ls-R, which kpsewhich reads),
// and from the fonts of TEXMFHOME, which has none, in the order in which
// kpsewhich looks. kpsewhich, a program which takes some 70 ms to start,
// was run for each file which TeXmacs does not have (some hundred, for the
// sizes of the TeX fonts of a document, the first time it is shown).
// Without a database, kpsewhich is asked as before
static hashmap<string,string> tex_index ("");
static int tex_index_state= 0; // 0: not read yet, 1: read, -1: none

static bool
tex_indexed_name (string name) {
  return ends (name, ".tfm") || ends (name, ".pfb");
}

static void
tex_index_dir (url dir, int depth) {
  bool err= false;
  array<string> a= read_directory (dir, err);
  if (err) return;
  for (int i= 0; i < N(a); i++) {
    if (N(a[i]) == 0 || a[i][0] == '.') continue;
    url u= dir * url (a[i]);
    if (tex_indexed_name (a[i])) {
      if (!tex_index->contains (a[i])) tex_index (a[i])= as_string (u);
    }
    else if (depth < 8 && is_directory (u)) tex_index_dir (u, depth + 1);
  }
}

static void
tex_index_ls_r (string file) {
  string s;
  if (load_string (url_system (file), s, false)) return;
  string root= file (0, max (0, N(file) - 5)); // without "/ls-R"
  string dir= root;
  int i= 0, n= N(s);
  while (i < n) {
    int start= i;
    while (i < n && s[i] != '\n') i++;
    string line= s (start, i);
    i++;
    int l= N(line);
    if (l == 0 || line[0] == '%') continue;
    if (line[l-1] == ':') {
      // a directory: "./fonts/tfm/public/cm:", relative to the root
      string d= line (0, l-1);
      if (starts (d, "./")) d= d (2, N(d));
      else if (d == ".") d= "";
      dir= (N(d) == 0 || d[0] == '/')? (N(d) == 0? root: d): root * "/" * d;
    }
    else if (tex_indexed_name (line) && !tex_index->contains (line))
      tex_index (line)= dir * "/" * line;
  }
}

static bool
tex_index_ready () {
  if (tex_index_state == 0) {
    tex_index_state= -1;
    bench_start ("tex font index");
    string home= var_eval_system ("kpsewhich -var-value TEXMFHOME");
    url fonts= url_system (home) * url ("fonts");
    if (N(home) > 0 && is_directory (fonts)) tex_index_dir (fonts, 0);
    string all= var_eval_system ("kpsewhich -all ls-R");
    int found= 0, i= 0, n= N(all);
    while (i < n) {
      int start= i;
      while (i < n && all[i] != '\n') i++;
      string file= all (start, i);
      i++;
      if (N(file) > 0 && ends (file, "ls-R")) { tex_index_ls_r (file); found++; }
    }
    if (found > 0) tex_index_state= 1;
    bench_cumul ("tex font index");
  }
  return tex_index_state == 1;
}

// the file of the TeX distribution (as kpsewhich, see tex_index)
static string
tex_which (string name) {
  if (tex_indexed_name (name) && tex_index_ready ())
    return tex_index->contains (name)? tex_index[name]: string ("");
  return kpsewhich (name);
}

static url
resolve_tfm (url name) {
  url r= resolve (the_tfm_path * name);
  if (!is_none (r)) return r;
  if (get_setting ("KPSEWHICH") == "true") {
    string which= tex_which (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
  return r;
}

static url
resolve_pk (url name) {
  url r= resolve (the_pk_path * name);
  if (!is_none (r)) return r;
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pk fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= kpsewhich (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return r;
}

static url
resolve_pfb (url name) {
  url r= resolve (the_pfb_path * name);
  if (!is_none (r)) return r;
#ifndef OS_WIN32 // The kpsewhich from MikTeX is bugged for pfb fonts
  if (get_setting ("KPSEWHICH") == "true") {
    string which= tex_which (as_string (name));
    if ((which!="") && exists (url_system (which))) return url_system (which);
    // cout << "Missed " << name << "\n";
  }
#endif
  return r;
}

url
tfm_font_path () {
  return the_tfm_path;
}

/******************************************************************************
* Caching results
******************************************************************************/

url
resolve_tex (url name) {
  string s= as_string (name);
  // NOTE: a Type 1 font which is not there is remembered too (""), since
  // kpsewhich is asked for it, which takes some 70 ms (the TeX fonts at
  // the sizes which only have a Metafont form); the TFM and PK files are
  // not, since they may be made later (make_tex_tfm, make_tex_pk)
  bool pfb= ends (s, "pfb");
  if (is_cached ("font_cache.scm", s)) {
    string r= cache_get ("font_cache.scm", s) -> label;
    if (r == "" && pfb) return url_none ();
    url u= url_system (r);
    if (r != "" && exists (u)) return u;
    cache_reset ("font_cache.scm", s);
  }

  bench_start ("resolve tex");
  url u= url_none ();
  if (ends (s, "mf" )) {
    u= resolve_tfm (name);
#ifdef OS_WIN32
    if (is_none (u))
      u= resolve_tfm (replace (s, ".mf", ".tfm"));
#endif
  }
  if (ends (s, "tfm")) u= resolve_tfm (name);
  if (ends (s, "pk" )) u= resolve_pk  (name);
  if (ends (s, "pfb")) u= resolve_pfb (name);
  bench_cumul ("resolve tex");

  if (!is_none (u)) cache_set ("font_cache.scm", s, as_string (u));
  else if (pfb) cache_set ("font_cache.scm", s, "");
  //cout << "Resolve " << name << " -> " << u << "\n";
  return u;
}

bool
exists_in_tex (url u) {
  return !is_none (resolve_tex (u));
}

/******************************************************************************
* Automatically generate missing fonts
******************************************************************************/

void
make_tex_tfm (string name) {
  string s;
  int r= 0;
  if (get_setting ("MAKETFM") == "MakeTeXTFM") {
    s= "MakeTeXTFM " * name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKETFM") == "mktextfm") {
    url tfm_dir ("$TEXMACS_HOME_PATH/fonts/tfm");
    s= "mktextfm " *
      string ("--destdir ") * as_string (tfm_dir) * " " *
      name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
    string superfluous= name * ".600pk";
    if (ends (name, ".tfm")) superfluous= name (0, N(name)-4) * ".600pk";
    remove (tfm_dir * superfluous);
  }
  if (get_setting ("MAKETFM") == "maketfm"){
    if (name(N(name) - 4, N(name)) == ".tfm")
      name = name (0, N(name) - 4);
    s = "maketfm --dest-dir \"" * get_env("$TEXMACS_HOME_PATH")
      * "\\fonts\\tfm\" " * name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (r) cout << "TeXmacs] system command failed: " << s << "\n";
}

void
make_tex_pk (string name, int dpi, int design_dpi) {
  string s;
  int r= 0;
  if (get_setting ("MAKEPK") == "MakeTeXPK") {
    s="MakeTeXPK " * name * " " *
      as_string (dpi) * " " * as_string (design_dpi) * " " *
      as_string (dpi) * "/" * as_string (design_dpi) * " localfont";
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKEPK") == "mktexpk") {
    url pk_dir ("$TEXMACS_HOME_PATH/fonts/pk");
    s="mktexpk " *
      string ("--dpi ") * as_string (dpi) * " " *
      string ("--bdpi ") * as_string (design_dpi) * " " *
      string ("--mag ") * as_string (dpi) *"/"* as_string (design_dpi) * " " *
      string ("--destdir ") * as_string (pk_dir) * " " *
      name;
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (get_setting ("MAKEPK") == "makepk") {
#ifdef OS_WIN32
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "%//" * as_string(design_dpi);
#else
    s = "makepk --dest-dir \""
      * get_env("$TEXMACS_HOME_PATH") * "\\fonts\\pk\" "
      * name * " " * as_string(dpi) * " " * as_string(design_dpi)
      * " " * as_string(dpi) * "/" * as_string(design_dpi);
#endif
    if (DEBUG_VERBOSE) debug_fonts << "Executing " << s << "\n";
    r= system (s);
  }
  if (r) cout << "TeXmacs] system command failed: " << s << "\n";
}

/******************************************************************************
* Automatic determination of paths where TeX fonts might have been generated
******************************************************************************/

static url
get_kpsepath (string s) {
  // FIXME: adapt to WIN32
  if (get_setting ("KPSEPATH") != "true") return url_none ();
  string r= var_eval_system ("kpsepath " * s);
  if (N(r)==0) return url_none ();

  int i, start, end;
  url p= url_none ();
  for (i=0; i<N(r); i++) {
    while ((i<N(r)) && (r[i]=='!')) i++;
    start=i;
    while ((i<N(r)) && (r[i]!=':')) i++;
    end=i;
    while ((end>start) && (r[end-1]=='/')) end--;
    string dir= r (start, end);
    if (dir == ".") continue;
    p= search_sub_dirs (dir) | p;
  }
  return p;
}

void
reset_tfm_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string tfm= get_setting ("TFM");
  the_tfm_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/tfm") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/tfm") |
    "$TEX_TFM_PATH" |
    ((tfm == "" || tfm == "{}") ? url_none () : tfm);
  if ((get_setting ("MAKETFM") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_tfm_path= the_tfm_path | get_kpsepath ("tfm");
  the_tfm_path= expand (factor (the_tfm_path));
}

void
reset_pk_path (bool rehash) { (void) rehash;
  // if (rehash && (get_setting ("TEXHASH") == "true")) system ("texhash");
  string pk= get_setting ("PK");
  the_pk_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/pk") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/pk") |
    "$TEX_PK_PATH" |
    (pk == ""? url_none (): pk);
  if ((get_setting ("MAKEPK") != "false") ||
      (get_setting ("TEXHASH") == "true"))
    if (get_setting ("KPSEWHICH") != "true")
      the_pk_path= the_pk_path | get_kpsepath ("pk");
  the_pk_path= expand (factor (the_pk_path));
}

void
reset_pfb_path () {
  string pfb= get_setting ("PFB");
  the_pfb_path=
    url_here () |
    search_sub_dirs ("$TEXMACS_HOME_PATH/fonts/type1") |
    search_sub_dirs ("$TEXMACS_PATH/fonts/type1") |
    "$TEX_PFB_PATH" |
    (pfb == ""? url_none (): url_system (pfb));
  the_pfb_path= expand (factor (the_pfb_path));
}
