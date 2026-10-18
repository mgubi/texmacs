/******************************************************************************
* MODULE     : ts_highlight.cpp
* DESCRIPTION: syntax highlighting of programs by tree-sitter
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* A treesit_language_rep wraps the usual language of a program mode. As long
   as Scheme has not given it a highlighter (the tables of the language are
   generated at the first use, progs/treesit/ts-highlight.scm), it does what
   the usual language does. Then its colours come from the highlight query of
   tree-sitter, run on the syntax tree of the region (syntax_region.cpp): the
   document whose children are the lines of the program, found from a line
   by its ip. The colours are stored in the highlight observers of the lines,
   as for the packrat languages. A modification is only announced to the
   modified line; the other lines whose colours change (typing a quote or a
   comment start) are re-typeset afterwards, outside the typesetting, from
   Scheme (treesit-schedule-updates). */

#include "code_lines.hpp"
#include "ts_highlight.hpp"
#include "syntax_region.hpp"
#include "impl_language.hpp"
#include "observer.hpp"
#include "path.hpp"
#include "hashmap.hpp"
#include "hashset.hpp"

extern tree the_et;
int tree_sitter_query_language (int query);

struct ts_highlighter {
  int language;
  int query;
  array<int> colors;   // colour code of each capture
  int injections;      // the query of injections.scm, or -1
};

static hashmap<string,ts_highlighter*> highlighters (NULL);
// the names of the languages in the injections -> program languages
static hashmap<string,string> injection_names ("");
static hashset<string> injection_requests;
// changes when highlighters arrive after their tables were generated: the
// lines are coloured again (see treesit_language_rep::highlight)
static int highlight_epoch= 0;
static int highlight_codes= 1000000;
static array<tree> pending_updates;
static bool updates_scheduled= false;
static bool highlighting_on= true;

void
tree_sitter_set_highlighting (bool on) {
  // off: the languages behave as without tree-sitter (for the tests of the
  // other highlighters)
  highlighting_on= on;
}

void
tree_sitter_set_highlighter (string lan, int query, array<string> classes) {
  // the highlighter of the program language lan: a query, and the colour
  // class (as in initialize_color_encodings, "" for none) of each capture
  int language= tree_sitter_query_language (query);
  if (language < 0) return;
  ts_highlighter* h= tm_new<ts_highlighter> ();
  h->language= language;
  h->query= query;
  h->injections= -1;
  for (int i= 0; i < N(classes); i++)
    h->colors << (classes[i] == ""? 0: max (encode_color (classes[i]), 0));
  highlighters (lan)= h;
}

void
tree_sitter_set_injections (string lan, int query) {
  // the query of the injections of lan (after its highlighter is set)
  ts_highlighter* h= highlighters[lan];
  if (h != NULL && tree_sitter_query_language (query) == h->language)
    h->injections= query;
}

void
tree_sitter_set_injection_name (string name, string lan) {
  // name (in an injection) stands for the program language lan
  injection_names (name)= lan;
}

void
tree_sitter_reset_highlights () {
  // highlighters were installed late: colour the lines again
  highlight_epoch++;
}

static bool
resolve_injection (string name, int& language, int& query, array<int>& colors,
                   int& injections) {
  // the highlighter of the program language which name stands for; its
  // first use asks for it (it may arrive later, see the epoch)
  string lan= injection_names[name];
  if (lan == "") return false;
  ts_highlighter* h= highlighters[lan];
  if (h == NULL && !injection_requests->contains (lan)) {
    injection_requests->insert (lan);
    call ("treesit-highlighter-request", lan);
    h= highlighters[lan];
  }
  if (h == NULL) return false;
  language= h->language;
  query= h->query;
  colors= h->colors;
  injections= h->injections;
  return true;
}

int
tree_sitter_program_language (string lan) {
  // the language of tree-sitter of the program language lan, once its
  // highlighter is installed, else -1 (for the editing functions)
  if (!highlighting_on || highlighters[lan] == NULL) return -1;
  return highlighters[lan]->language;
}

array<tree>
tree_sitter_pending_updates () {
  // the lines whose colours changed without them being modified
  array<tree> r= pending_updates;
  pending_updates= array<tree> ();
  updates_scheduled= false;
  return r;
}

/******************************************************************************
* The language
******************************************************************************/

struct treesit_language_rep: language_rep {
  language_rep* base;
  bool requested;
  int epoch;
  treesit_language_rep (string s, language_rep* base2);
  text_property advance (tree t, int& pos);
  array<int> get_hyphens (string s) { return base->get_hyphens (s); }
  void hyphenate (string s, int after, string& l, string& r) {
    base->hyphenate (s, after, l, r); }
  string get_group (string s) { return base->get_group (s); }
  array<string> get_members (string s) { return base->get_members (s); }
  void highlight (tree t);
  string get_color (tree t, int start, int end);
  ts_highlighter* get_highlighter ();
};

treesit_language_rep::treesit_language_rep (string s, language_rep* base2):
  language_rep (s), base (base2), requested (false), epoch (highlight_epoch)
{
  // a code of its own for the highlight observers, far from those of packrat
  hl_lan= highlight_codes++;
}

ts_highlighter*
treesit_language_rep::get_highlighter () {
  if (!highlighting_on) return NULL;
  ts_highlighter* h= highlighters [lan_name];
  if (h == NULL && !requested) {
    requested= true;
    call ("treesit-highlighter-request", lan_name);
    h= highlighters [lan_name];
  }
  return h;
}

static void
set_line_colors (tree line, int hl, array<int> cols, bool notify) {
  // store the colours of a line; when notify, a line whose colours change
  // is scheduled to be typeset again
  if (!is_atomic (line)) {
    if (!has_highlight (line, hl)) attach_highlight (line, hl);
    return;
  }
  int n= N(line->label);
  if (N(cols) != n) return;
  if (has_highlight (line, hl)) {
    array<int> old= obtain_highlight (line, hl);
    if (old == cols) return;
    if (notify) pending_updates << line;
  }
  attach_highlight (line, hl, 0, 0, n);
  int i= 0;
  while (i < n) {
    int j= i + 1;
    while (j < n && cols[j] == cols[i]) j++;
    if (cols[i] != 0) attach_highlight (line, hl, cols[i], i, j);
    i= j;
  }
}

void
treesit_language_rep::highlight (tree t) {
  ts_highlighter* h= get_highlighter ();
  if (h == NULL) { base->highlight (t); return; }
  if (epoch != highlight_epoch) {
    // the colours of the lines are forgotten: a new code
    epoch= highlight_epoch;
    hl_lan= highlight_codes++;
  }
  if (has_highlight (t, hl_lan)) return;
  tree region;
  int row= -1;
  if (is_document (t)) region= t;
  else {
    path ip= obtain_ip (t);
    if (!is_nil (ip) && ip_attached (ip)) {
      tree parent= subtree (the_et, reverse (ip->next));
      int i= ip->item;
      if (is_document (parent) && i >= 0 && i < N(parent) &&
          strong_equal (parent[i], t)) {
        region= parent;
        row= i;
      }
    }
    if (row < 0) {
      if (!is_atomic (t)) { attach_highlight (t, hl_lan); return; }
      region= t;
    }
  }
  array<int> recoloured;
  array<array<int> > cols=
    syntax_region_highlight (region, h->language, h->query, h->colors,
                             h->injections, resolve_injection, highlight_epoch,
                             &recoloured);
  if (is_atomic (region)) {
    if (N(cols) == 1) set_line_colors (region, hl_lan, cols[0], false);
    return;
  }
  if (N(cols) != N(region)) return;
  if (!has_highlight (region, hl_lan)) {
    // the first time: all the lines
    for (int i= 0; i < N(region); i++)
      set_line_colors (region[i], hl_lan, cols[i], i != row);
    attach_highlight (region, hl_lan, 0, 0, 1);
  }
  else {
    // the lines coloured again, and the line asked for (a line without
    // colours of its own, as after an undo, gets them when it is typeset)
    for (int k= 0; k < N(recoloured); k++)
      if (recoloured[k] != row)
        set_line_colors (region[recoloured[k]], hl_lan, cols[recoloured[k]], true);
    if (row >= 0) set_line_colors (region[row], hl_lan, cols[row], false);
  }
  if (N(pending_updates) > 0 && !updates_scheduled) {
    updates_scheduled= true;
    call ("treesit-schedule-updates");
  }
}

text_property
treesit_language_rep::advance (tree t, int& pos) {
  // the token of the usual language, cut where the colour changes
  int start= pos;
  text_property tp= base->advance (t, pos);
  if (!highlighting_on || highlighters [lan_name] == NULL) return tp;
  array<int> cols= obtain_highlight (t, hl_lan);
  if (N(cols) == 0 || start >= N(cols)) return tp;
  for (int i= start + 1; i < pos && i < N(cols); i++)
    if (cols[i] != cols[start]) { pos= i; break; }
  return tp;
}

string
treesit_language_rep::get_color (tree t, int start, int end) {
  if (!highlighting_on || highlighters [lan_name] == NULL)
    return base->get_color (t, start, end);
  if (start >= end) return "";
  array<int> cols= obtain_highlight (t, hl_lan);
  if (start < N(cols) && cols[start] != 0)
    return decode_color (lan_name, cols[start]);
  return "";
}

/******************************************************************************
* Folds
******************************************************************************/

/* The folds of program code are those of the core (System/Language/
   code_lines.cpp), which the language servers fold too: a state of the
   hidden lines, which a modification of a line forgets. */

int
tree_sitter_fold_state (tree t) {
  return code_line_fold_state (t);
}

void
tree_sitter_set_fold_state (tree t, int state) {
  code_set_line_fold_state (t, state);
}

/******************************************************************************
* Interface
******************************************************************************/

static hashset<string> wrapped_languages;

static language_rep*
wrap_language (string lan, language_rep* base) {
  // (the wrapper of the program languages, see prog_language)
  if (!wrapped_languages->contains (lan)) return NULL;
  return tm_new<treesit_language_rep> (lan, base);
}

void
tree_sitter_wrap_language (string lan) {
  // lan may be highlighted by tree-sitter (called at the start for the
  // program languages with a grammar, see tm-program-languages.scm)
  wrapped_languages->insert (lan);
  set_prog_language_wrapper (wrap_language);
}

bool
tree_sitter_wraps (string lan) {
  // the program languages which tree-sitter may highlight
  return wrapped_languages->contains (lan);
}

language_rep*
tree_sitter_language (string lan, language_rep* base) {
  return tm_new<treesit_language_rep> (lan, base);
}

object
tree_sitter_line_colors (string lan, tree t) {
  // the coloured runs of a line, as (start end class), after highlighting it
  language l= prog_language (lan);
  l->highlight (t);
  static hashmap<int,string> names ("");
  if (N(names) == 0) {
    iterator<string> it= iterate (language_rep::color_encoding);
    while (it->busy ()) { string s= it->next (); names (encode_color (s))= s; }
  }
  array<object> r;
  if (!is_atomic (t)) return as_list_object (r);
  array<int> cols= obtain_highlight (t, l->hl_lan);
  int i= 0, n= N(cols);
  while (i < n) {
    int j= i + 1;
    while (j < n && cols[j] == cols[i]) j++;
    if (cols[i] != 0)
      r << list_object (object (i), object (j), object (names[cols[i]]));
    i= j;
  }
  return as_list_object (r);
}
