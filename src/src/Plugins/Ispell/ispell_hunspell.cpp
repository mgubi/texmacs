
/******************************************************************************
* MODULE     : ispell_hunspell.cpp
* DESCRIPTION: spell checking with the Hunspell library (the browser)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Ispell/ispell.hpp"

#if USE_HUNSPELL

// A page cannot run the spell checker as a program (ispell_exe.cpp), and
// TeXmacs asks it a word at a time, waiting for the answer: Hunspell is in
// the program itself (misc/wasm/get-hunspell.sh). The dictionary of a
// language is fetched the first time it is needed, from the packages of
// https://github.com/wooorm/dictionaries (dictionary-<code> on npm, through
// jsDelivr: Hunspell dictionaries in UTF-8), and kept in the home directory
// of the user ($TEXMACS_HOME_PATH/system/dictionaries), with the words which
// the user inserts (personal-<code>.txt).

#include "file.hpp"
#include "resource.hpp"
#include "convert.hpp"
#include "analyze.hpp"
#include "web_files.hpp"
#include <hunspell/hunspell.hxx>
#include <string>
#include <vector>

static string
hunspell_code (string lan) {
  if (lan == "english" || lan == "american") return "en";
  if (lan == "british") return "en-gb";
  if (lan == "bulgarian") return "bg";
  if (lan == "catalan") return "ca";
  if (lan == "croatian") return "hr";
  if (lan == "czech") return "cs";
  if (lan == "danish") return "da";
  if (lan == "dutch") return "nl";
  if (lan == "esperanto") return "eo";
  if (lan == "estonian") return "et";
  if (lan == "french") return "fr";
  if (lan == "german") return "de";
  if (lan == "greek") return "el";
  if (lan == "hungarian") return "hu";
  if (lan == "italian") return "it";
  if (lan == "korean") return "ko";
  if (lan == "latvian") return "lv";
  if (lan == "lithuanian") return "lt";
  if (lan == "norwegian") return "nb";
  if (lan == "polish") return "pl";
  if (lan == "portuguese") return "pt";
  if (lan == "romanian") return "ro";
  if (lan == "russian") return "ru";
  if (lan == "slovak") return "sk";
  if (lan == "slovene") return "sl";
  if (lan == "spanish") return "es";
  if (lan == "swedish") return "sv";
  if (lan == "turkish") return "tr";
  if (lan == "ukrainian") return "uk";
  return "";
}

static url
hunspell_dir () {
  return url ("$TEXMACS_HOME_PATH/system/dictionaries");
}

// the file of a dictionary (index.aff or index.dic of its package), fetched
// if it is not there yet; false if it cannot be
static bool
hunspell_fetch (string code, string ext, url& u) {
  u= hunspell_dir () * (code * "." * ext);
  if (exists (u)) return true;
  string ret;
  string addr= "https://cdn.jsdelivr.net/npm/dictionary-" * code *
               "/index." * ext;
  if (http_get (ret, addr, array<string> ()) != 0 || N(ret) == 0 ||
      starts (ret, "<")) // (an error page)
    return false;
  if (!exists (hunspell_dir ())) mkdir (hunspell_dir ());
  return !save_string (u, ret, false);
}

/******************************************************************************
* The spell checker of a language
******************************************************************************/

RESOURCE(ispeller);

struct ispeller_rep: rep<ispeller> {
  string    lan;
  string    code;
  Hunspell* speller;
  bool      unavailable;

public:
  ispeller_rep (string lan);
  ~ispeller_rep ();
  string start ();
  tree   check (string word);
  void   accept (string word);
  void   insert (string word);
};

RESOURCE_CODE(ispeller);

ispeller_rep::ispeller_rep (string lan2):
  rep<ispeller> (lan2), lan (lan2), code (hunspell_code (lan2)),
  speller (NULL), unavailable (false) {}

ispeller_rep::~ispeller_rep () {
  if (speller != NULL) delete speller;
}

static url
hunspell_personal (string code) {
  return hunspell_dir () * ("personal-" * code * ".txt");
}

string
ispeller_rep::start () {
  if (speller != NULL) return "ok";
  if (unavailable) return "Error: no dictionary for " * lan;
  url aff, dic;
  if (code == "" || !hunspell_fetch (code, "aff", aff) ||
      !hunspell_fetch (code, "dic", dic)) {
    unavailable= true;
    return "Error: no dictionary for " * lan;
  }
  c_string a (concretize (aff)), d (concretize (dic));
  speller= new Hunspell ((char*) a, (char*) d);
  // the words of the user
  string words;
  if (!load_string (hunspell_personal (code), words, false)) {
    array<string> l= tokenize (words, "\n");
    for (int i= 0; i < N(l); i++)
      if (l[i] != "") speller->add (std::string (&l[i][0], N(l[i])));
  }
  return "ok";
}

static std::string
std_utf8 (string word) {
  string s= cork_to_utf8 (word);
  return std::string (N(s) == 0? "": &s[0], N(s));
}

tree
ispeller_rep::check (string word) {
  if (speller == NULL) return "Error: unavailable";
  std::string w= std_utf8 (word);
  if (speller->spell (w)) return "ok";
  std::vector<std::string> l= speller->suggest (w);
  tree t (TUPLE, as_string ((int) l.size ()));
  for (size_t i= 0; i < l.size (); i++)
    t << utf8_to_cork (string (l[i].c_str (), (int) l[i].size ()));
  return t;
}

void
ispeller_rep::accept (string word) {
  if (speller != NULL) speller->add (std_utf8 (word));
}

void
ispeller_rep::insert (string word) {
  if (speller == NULL) return;
  speller->add (std_utf8 (word));
  string words;
  url u= hunspell_personal (code);
  if (load_string (u, words, false)) words= "";
  words << cork_to_utf8 (word) << "\n";
  save_string (u, words, false);
}

/******************************************************************************
* The interface of the spell checkers (ispell.hpp)
******************************************************************************/

static ispeller
get_ispeller (string lan) {
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  return sc;
}

string
ispell_start (string lan) {
  return get_ispeller (lan)->start ();
}

tree
ispell_check (string lan, string s) {
  ispeller sc= get_ispeller (lan);
  string r= sc->start ();
  if (r != "ok") return r;
  return sc->check (s);
}

void
ispell_accept (string lan, string s) {
  ispeller sc= ispeller (lan);
  if (!is_nil (sc)) sc->accept (s);
}

void
ispell_insert (string lan, string s) {
  ispeller sc= ispeller (lan);
  if (!is_nil (sc)) sc->insert (s);
}

void
ispell_done (string lan) {
  (void) lan;
}

#endif // USE_HUNSPELL
