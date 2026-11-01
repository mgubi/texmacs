
/******************************************************************************
* MODULE     : ispell.cpp
* DESCRIPTION: interface with the ispell spell checker
* COPYRIGHT  : (C) 2025  Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Ispell/ispell.hpp"
#include "file.hpp"
#include "resource.hpp"
#include "convert.hpp"
#include "locale.hpp"

#if USE_ASPELL
#include <aspell.h>

string ispell_encode (string lan, string s);
string ispell_decode (string lan, string s);

RESOURCE(ispeller);

struct ispeller_rep: rep<ispeller> {
  string         lan;
  AspellConfig* config;
  AspellSpeller* speller;
  bool           unavailable;
  string         error_msg;

public:
  ispeller_rep (string lan);
  ~ispeller_rep ();

  string start ();
  
  tree check (string word);
  void accept (string word);
  void insert (string word);
  void save_all ();
};

RESOURCE_CODE(ispeller);

ispeller_rep::ispeller_rep (string lan2): 
  rep<ispeller> (lan2), 
  lan (lan2), 
  config (NULL), 
  speller (NULL), 
  unavailable (false) {}

ispeller_rep::~ispeller_rep () {
  if (speller) delete_aspell_speller (speller);
  if (config) delete_aspell_config (config);
}

string
ispeller_rep::start () {
  if (speller) return "ok";
  if (unavailable) return error_msg;

  string locale = language_to_locale (lan);

  // a language which TeXmacs has no locale for (klingon: language_to_locale
  // gives en_US for all of them) has no dictionary: not English, as the
  // library would take, but an error, as the hunspell checker (and the
  // misc-modules suite) have it
  if (locale == "en_US" && lan != "english" && lan != "american") {
    error_msg = "Error: no dictionary for " * lan;
    std_error << error_msg << "\nThe corresponding text is not checked\n";
    unavailable = true;
    return error_msg;
  }

  config = new_aspell_config ();

  url aspell_path = url_system ("$TEXMACS_PATH/aspell-0.60");
  if (exists (aspell_path)) {
    cout << "Found aspell path: " << concretize(aspell_path) << "\n";
    aspell_config_replace (config, "data-dir", as_charp(concretize(aspell_path)));
    aspell_config_replace (config, "dict-dir", as_charp(concretize(aspell_path)));
  }
  
  if (locale != "") {
    char* c_locale = as_charp(locale);
    aspell_config_replace (config, "lang", c_locale);
  } else {
    aspell_config_replace (config, "lang", "en_US"); 
  }

  aspell_config_replace (config, "encoding", "utf-8");

  AspellCanHaveError* ret = new_aspell_speller (config);

  if (aspell_error (ret)) {
    error_msg = "Error: ";
    error_msg << aspell_error_message (ret);
    delete_aspell_can_have_error (ret);
    
    std_error << error_msg << "\nCannot spellcheck\n";
    unavailable = true;
    return error_msg;
  }

  speller = to_aspell_speller (ret);
  
  if (DEBUG_IO) debug_spell << "Aspell library initialized for " << lan << "\n";
  unavailable = false;
  return "ok";
}

tree
ispeller_rep::check (string word) {
  if (unavailable || !speller) return "Error: unavailable";

  string word_utf8 = ispell_encode (lan, word);
  int len = N(word_utf8);
  
  char* c_word = as_charp(word_utf8); 

  int correct = aspell_speller_check (speller, c_word, len);

  if (correct == -1) {
    return "Error: aspell check failed";
  }

  if (correct == 1) {
    return "ok"; 
  }

  // Same format as parse_ispell: number of suggestions, then suggestions
  tree t (TUPLE);
  
  const AspellWordList* suggestions = aspell_speller_suggest (speller, c_word, len);
  AspellStringEnumeration* elements = aspell_word_list_elements (suggestions);
  
  const char* suggestion;
  while ((suggestion = aspell_string_enumeration_next (elements)) != NULL) {
    t << ispell_decode (lan, string(suggestion));
  }
  
  delete_aspell_string_enumeration (elements);

  return tree (TUPLE, as_string (N(t))) * t;
}

void
ispeller_rep::accept (string word) {
  if (unavailable || !speller) return;
  string word_utf8 = ispell_encode (lan, word);
  char* c_word = as_charp(word_utf8);
  aspell_speller_add_to_session (speller, c_word, N(word_utf8));
}

void
ispeller_rep::insert (string word) {
  if (unavailable || !speller) return;
  string word_utf8 = ispell_encode (lan, word);
  char* c_word = as_charp(word_utf8);
  aspell_speller_add_to_personal (speller, c_word, N(word_utf8));
}

void
ispeller_rep::save_all () {
  if (unavailable || !speller) return;
  aspell_speller_save_all_word_lists (speller);
}

string
ispell_encode (string lan, string s) {
  (void) lan;
  return cork_to_utf8 (s);
}

string
ispell_decode (string lan, string s) {
  (void) lan;
  return utf8_to_cork (s);
}

static string
ispell_lib_start (string lan) {
  if (DEBUG_IO) debug_spell << "Start " << lan << "\n";
  ispeller sc= ispeller (lan);
  if (is_nil (sc)) sc= tm_new<ispeller_rep> (lan);
  string res = sc->start ();
  return res;
}

static tree
ispell_lib_check (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Check " << s << "\n";
  ispeller sc= ispeller (lan);
  
  if (is_nil (sc)) {
    string message= ispell_lib_start (lan);
    if (starts (message, "Error: ")) return message;
    sc = ispeller(lan);
  }
  
  if (is_nil(sc) || sc->unavailable) return "Error: unavailable";
  
  return sc->check(s);
}

static void
ispell_lib_accept (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Accept " << s << "\n";
  ispeller sc= ispeller (lan);
  if (!is_nil (sc)) sc->accept(s);
}

static void
ispell_lib_insert (string lan, string s) {
  if (DEBUG_IO) debug_spell << "Insert " << s << "\n";
  ispeller sc= ispeller (lan);
  if (!is_nil (sc)) sc->insert(s);
}

static void
ispell_lib_done (string lan) {
  if (DEBUG_IO) debug_spell << "End " << lan << "\n";
  ispeller sc= ispeller (lan);
  if (!is_nil (sc)) {
      sc->save_all();
  }
}

/******************************************************************************
* The library, and the external program for the languages it has no dictionary of
******************************************************************************/

// What the package has: the dictionaries of the library. A language they do
// not cover (Greek, Czech...) is spell checked by a Hunspell or an Aspell
// which the user has installed (ispell_exe.cpp, compiled under the names
// ispell_exe_*), as before the library; the error of the library is given
// when there is none.
#if !USE_HUNSPELL
string ispell_exe_start (string lan);
tree   ispell_exe_check (string lan, string s);
void   ispell_exe_accept (string lan, string s);
void   ispell_exe_insert (string lan, string s);
void   ispell_exe_done (string lan);

static hashmap<string,bool> ispell_external (false);
static hashmap<string,bool> ispell_no_external (false);   // tried, none

string
ispell_start (string lan) {
  if (ispell_external->contains (lan)) return ispell_exe_start (lan);
  string res= ispell_lib_start (lan);
  if (!starts (res, "Error: ")) return res;
  if (ispell_no_external->contains (lan)) return res;
  string ext= ispell_exe_start (lan);
  if (starts (ext, "Error: ")) {
    ispell_no_external (lan)= true;
    return res;
  }
  if (DEBUG_IO) debug_spell << "Using the external spell checker for " << lan << "\n";
  ispell_external (lan)= true;
  return ext;
}

tree
ispell_check (string lan, string s) {
  if (!ispell_external->contains (lan)) {
    tree t= ispell_lib_check (lan, s);
    if (!(is_atomic (t) && starts (as_string (t), "Error: "))) return t;
    string st= ispell_start (lan);   // the external program, if there is one
    if (!ispell_external->contains (lan)) return t;
  }
  return ispell_exe_check (lan, s);
}

void
ispell_accept (string lan, string s) {
  if (ispell_external->contains (lan)) ispell_exe_accept (lan, s);
  else ispell_lib_accept (lan, s);
}

void
ispell_insert (string lan, string s) {
  if (ispell_external->contains (lan)) ispell_exe_insert (lan, s);
  else ispell_lib_insert (lan, s);
}

void
ispell_done (string lan) {
  if (ispell_external->contains (lan)) ispell_exe_done (lan);
  else ispell_lib_done (lan);
}
#else
string ispell_start (string lan) { return ispell_lib_start (lan); }
tree   ispell_check (string lan, string s) { return ispell_lib_check (lan, s); }
void   ispell_accept (string lan, string s) { ispell_lib_accept (lan, s); }
void   ispell_insert (string lan, string s) { ispell_lib_insert (lan, s); }
void   ispell_done (string lan) { ispell_lib_done (lan); }
#endif

#endif