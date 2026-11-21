
/******************************************************************************
* MODULE     : verbatim.cpp
* DESCRIPTION: routines for converting between TeXmacs trees and text
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "converter.hpp"
#include "locale.hpp"
#include "wencoding.hpp"
#include "vars.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"

/******************************************************************************
* TeXmacs to verbatim
******************************************************************************/

static void print_verbatim (string& buf, tree t, bool wrap);
static string as_verbatim (tree t, bool wrap);

static void
print_verbatim_arg (string& buf, tree t, bool wrap) {
  string s= as_verbatim (t, wrap);
  if (tm_string_length (s) <= 1 || is_numeric (s) || is_iso_alpha (s))
    print_verbatim (buf, t, wrap);
  else {
    buf << "(";
    print_verbatim (buf, t, wrap);
    buf << ")";
  }
}

static int
get_width (string s) {
  if (occurs ("\n", s)) return 0;
  else return tm_string_length (s);
}

static void
print_verbatim_table (string& buf, tree t, bool wrap) {
  if (N(buf)>0 && buf[N(buf)-1] != '\n') buf << "\n";
  int i, nr= N(t), j, nc= 0;
  tree tab (TUPLE, nr);
  for (i=0; i<nr; i++) {
    tree row= t[i];
    if (is_func (row, CWITH)) row= row[N(row)-1];
    if (!is_func (row, ROW)) tab[i]= tree (TUPLE);
    else {
      nc= max (nc, N(row));
      tab[i]= tree (TUPLE, N(row));
      for (j=0; j<N(row); j++)
        tab[i][j]= as_verbatim (row[j], wrap);
    }
  }
  array<int> w (nc);
  for (j=0; j<nc; j++) w[j]= 0;
  for (i=0; i<nr; i++) {
    for (j=0; j<N(tab[i]); j++)
      w[j]= max (w[j], get_width(tab[i][j]->label));
  }
  for (i=0; i<nr; i++) {
    for (j=0; j<N(tab[i]); j++) {
      int spc= w[j] - get_width(tab[i][j]->label) + 1;
      buf << tab[i][j]->label;
      if (j != N(tab[i]) - 1)
        for (int k=0; k<spc; k++) buf << " ";
    }
    buf << "\n";
  }
}

static void
print_verbatim_string (string& buf, string s) {
  // in a string of the document, the bytes 0x09 and 0x0A are the Cork
  // glyphs of the macron and of the dot accent: they are written <#AF> and
  // <#2D9>, so that the tabs and newlines of the text are only those of its
  // structure, and the text is converted by cork_text_to_utf8 (#439)
  for (int i=0; i<N(s); i++)
    if (s[i] == '\t') buf << "<#AF>";
    else if (s[i] == '\n') buf << "<#2D9>";
    else buf << s[i];
}

static void
print_verbatim (string& buf, tree t, bool wrap) {
  if (is_atomic (t)) print_verbatim_string (buf, t->label);
  else switch (L(t)) {
    case SURROUND:
      print_verbatim (buf, t[0], wrap);
      print_verbatim (buf, t[2], wrap);
      print_verbatim (buf, t[1], wrap);
      break;
    case HSPACE:
    case SPACE:
      if (is_atomic (t[0]) && starts (t[0]->label, "-")) break;
    case HTAB:
      if (N(buf)>0 && buf[N(buf)-1] != '\n') buf << " ";
      break;
    case AROUND:
    case VAR_AROUND:
      print_verbatim (buf, t[0], wrap);
      print_verbatim (buf, t[1], wrap);
      print_verbatim (buf, t[2], wrap);
      break;
    case BIG_AROUND:
      print_verbatim (buf, t[0], wrap);
      print_verbatim (buf, t[1], wrap);
      break;
    case LEFT:
    case MID:
    case RIGHT:
    case BIG:
    case LPRIME:
    case RPRIME:
      print_verbatim (buf, t[0], wrap);
      break;
    case RSUB:
      buf << "_";
      print_verbatim_arg (buf, t[0], wrap);
      break;
    case RSUP:
      buf << "^";
      print_verbatim_arg (buf, t[0], wrap);
      break;
    case FRAC:
      print_verbatim_arg (buf, t[0], wrap);
      buf << "/";
      print_verbatim_arg (buf, t[1], wrap);
      break;
    case SQRT:
      if (N(t) == 1) {
        buf << "sqrt(";
        print_verbatim (buf, t[0], wrap);
        buf << ")";
      }
      else {
        print_verbatim_arg (buf, t[0], wrap);
        print_verbatim_arg (buf, tree (RSUP, tree (FRAC, "1", t[1])), wrap);
      }
      break;
    case WIDE:
      print_verbatim_arg (buf, t[0], wrap);
      print_verbatim (buf, t[1], wrap);
      break;
    case SYNTAX:
      print_verbatim_arg (buf, t[0], wrap);
      break;
    case TABLE:
      print_verbatim_table (buf, t, wrap);
      break;
    default:
      if (is_compound (t, "TeXmacs", 0))
        print_verbatim (buf, "TeXmacs", wrap);
      else {
        int i, n= N(t);
        for (i=0; i<n; i++)
          if (the_drd->is_accessible_child (t, i)) {
            if (is_document (t) && (i>0)) {
              if (wrap && N(buf)>0 && buf[N(buf)-1] != '\n') buf << "\n";
              buf << "\n";
            }
            tree w= std_drd->get_env_child (t, i, tree (ATTR));
            if (drd_env_read (w, MODE, "text") == "prog" ||
                drd_env_read (w, FONT_FAMILY, "rm") == "tt")
              print_verbatim (buf, t[i], false);
            else print_verbatim (buf, t[i], wrap);
          }
      }
      break;
    }
}

static string
as_verbatim (tree t, bool wrap) {
  if (!is_snippet (t)) {
    tree init= extract (t, "initial");
    hashmap<string,tree> h (UNINIT, init);
    if (h[MODE] == "prog" || h[FONT_FAMILY] == "tt") wrap= false;
    t= extract (t, "body");
  }
  string buf;
  print_verbatim (buf, t, wrap);
  if (wrap) {
    int i= 0, n= N(buf);
    while (i<n) {
      int pos= i;
      while (i<n && buf[i]!='\n') i++;
      array<string> a= tm_tokenize (buf (pos, i));
      int start= 0;
      //cout << "a= " << a << "\n";
      //cout << "l= " << N(a) << "\n";
      while (N(a)-start > 78) {
        //cout << "  start= " << start << "\n";
        int mid= start+78;
        while (mid>start && a[mid] != " ") mid--;
        if (mid <= start) break;
        if (mid<N(a)) {
          //cout << "  mid= " << mid << "\n";
          pos += N(tm_recompose (range (a, start, mid)));
          ASSERT (buf[pos] == ' ', "error in space synchronization");
          buf[pos]= '\n';
          start= mid+1;
          pos++;
        }
        else break;
      }
      if (i<n) i++;
    }
  }
  return buf;
}

string
unix_to_dos (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\n') r << "\r\n";
    else r << s[i];
  return r;
}

string
var_cork_to_utf8 (string s) {
  // the text made by as_verbatim: its newlines are those of the structure
  return cork_text_to_utf8 (s);
}

string
var_cork_to_sourcecode (string s) {
  return cork_text_to_sourcecode (s);
}

static string
cork_to_latin1 (string s) {
  string u= var_cork_to_utf8 (s), r;
  for (int i=0; i<N(u); ) {
    unsigned int code= decode_from_utf8 (u, i);
    if (code < 256) r << ((char) code);
    else r << '?';
  }
  return r;
}

static string
latin1_to_cork (string s) {
  string u;
  for (int i=0; i<N(s); i++)
    u << encode_as_utf8 ((unsigned int) (unsigned char) s[i]);
  return utf8_to_cork (u);
}

string
tree_to_verbatim (tree t, bool wrap, string enc) {
  if (enc == "default") enc= "auto";
  string buf= as_verbatim (t, wrap);
  if (enc == "auto")
    enc= get_locale_charset ();
  if (enc == "iso-8859-1" || enc == "ISO-8859-1") buf= cork_to_latin1 (buf);
  else if (enc == "SourceCode") buf= var_cork_to_sourcecode (buf);
  else if (enc != "cork" && enc != "Cork") buf= var_cork_to_utf8 (buf);
#ifdef OS_WIN32
  return unix_to_dos (buf);
#else
  return buf;
#endif
}

/******************************************************************************
* Verbatim to TeXmacs
******************************************************************************/

static string
encode (string s, string enc) {
  // @enc is resolved (see resolve_encoding): @s holds no newline
  if (enc == "utf-8") return utf8_to_cork (s);
  else if (enc == "iso-8859-1" || enc == "ISO-8859-1")
    return latin1_to_cork (s);
  else if (enc == "SourceCode") return sourcecode_to_cork (s);
  else if (enc == "universal") return s;
  else if (starts (enc, "iconv:")) return convert (s, enc (6, N(enc)), "Cork");
  else return tm_encode (s);
}

static string
resolve_encoding (string& s, string enc) {
  // the encoding "auto" is guessed on the whole text, as western_to_cork
  // does, and its byte order mark is removed
  if (enc != "auto") return enc;
  string charset= guess_wencoding (s);
  if (charset == "UTF-8-BOM") { s= s (3, N(s)); return "utf-8"; }
  if (charset == "UTF-8") return "utf-8";
  if (charset == "ISO-8859") {
    charset= language_to_local_ISO_charset (get_locale_language ());
    if (charset != "") return "iconv:" * charset;
  }
  if (looks_universal (s)) return "universal";
  return "verbatim";
}

static string
encode_line (string s, string enc) {
  // a line of the text in Cork: the tabs are expanded and a backspace erases
  // the character before it. This is done on the bytes of the text, before
  // the conversion to Cork, where the glyphs of the macron, of the dot accent
  // and of the breve are the bytes of a tab, a newline and a backspace (#866)
  string r;
  int i, n= N(s), start= 0;
  for (i=0; i<n; i++)
    if (s[i] == '\t' || s[i] == '\b') {
      r << encode (s (start, i), enc);
      start= i+1;
      if (s[i] == '\t') {
        int col= tm_string_length (r);
        do { r << " "; col++; } while ((col&7) != 0);
      }
      else if (N(r) > 0) {
        int pos= N(r);
        tm_char_backwards (r, pos);
        r->resize (pos);
      }
    }
  r << encode (s (start, n), enc);
  return r;
}

tree
verbatim_to_tree (string s, string enc) {
  // the text is split into lines before its conversion to Cork (see
  // encode_line)
  enc= resolve_encoding (s, enc);
  int i, j;
  for (i=0; i<N(s); i++)
    if (s[i]=='\n') {
      tree t (DOCUMENT);
      for (i=0, j=0; i<N(s); i++)
        if (s[i]=='\n') {
          t << encode_line (s (j, i), enc);
          j= i+1;
        }
      t << encode_line (s (j, i), enc);
      return t;
    }
  return encode_line (s, enc);
}

string
dos_to_unix (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\r' && i+1<n && s[i+1] == '\n') { r << "\n"; i++; }
    else r << s[i];
  return r;
}

string
mac_to_unix (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\r') r << "\n";
    else r << s[i];
  return r;
}

tree
verbatim_to_tree (string s, bool wrap, string enc) {
  if (enc == "default") enc= "auto";
  s= mac_to_unix (dos_to_unix (s));
  if (wrap) {
    string r;
    int i, n= N(s);
    for (i=0; i<n; ) {
      if (s[i] == '\n' || s[i] == ' ' || s[i] == '\t') {
        int lf= 0;
        while (i<n && (s[i] == '\n' || s[i] == ' ' || s[i] == '\t')) {
          if (s[i] == '\n') lf++;
          i++;
        }
        if (lf <= 1) r << " ";
        else r << "\n";
      }
      else r << s[i++];
    }
    s= r;
  }
  return verbatim_to_tree (s, enc);
}

tree
verbatim_document_to_tree (string s, bool wrap, string enc) {
  if (enc == "default") enc= "auto";
  tree t    = verbatim_to_tree (s, wrap, enc);
  tree init = tree (COLLECTION,
                    tree (ASSOCIATE, LANGUAGE, "verbatim"),
                    tree (ASSOCIATE, FONT_FAMILY, "tt"),
                    tree (ASSOCIATE, PAR_FIRST, "0cm"));
  return tree (DOCUMENT, compound ("body", t), compound ("initial", init));
}
