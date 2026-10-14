
/******************************************************************************
* MODULE     : locale.cpp
* DESCRIPTION: Locale related routines
* COPYRIGHT  : (C) 1999-2019  Joris van der Hoeven, Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "locale.hpp"
#include <time.h>

#ifndef OS_MINGW
#include <langinfo.h>
#ifndef X11TEXMACS
#include <locale>
#endif
#else
#include <winnls.h>
#endif

#include <iostream>

#define outline Core_outline
#define extend Core_extends
#ifdef OS_MACOS
#include <CoreFoundation/CFLocale.h>
#include <CoreFoundation/CFString.h>
#endif
#undef extend
#undef outline

#include "sys_utils.hpp"
#include "analyze.hpp"

#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#endif

/******************************************************************************
* Locales
******************************************************************************/

#ifdef OS_MINGW
const string
windows_locale_to_language () {
  static string language;

  if (N(language) == 0) {
    LANGID lid= GetUserDefaultUILanguage();
    switch(PRIMARYLANGID(lid)) {
    case LANG_BULGARIAN:  language= "bulgarian"; break;
    case LANG_CHINESE:	  language= "chinese"; break;
    case LANG_CHINESE_TRADITIONAL: language= "taiwanese"; break;
    case LANG_CROATIAN:   language= "croatian"; break;
    case LANG_CZECH:      language= "czech"; break;
    case LANG_DANISH:     language= "danish"; break;
    case LANG_DUTCH:      language= "dutch"; break;
    case LANG_ENGLISH:
      switch(SUBLANGID(lid)) {
      case SUBLANG_ENGLISH_UK: language= "british"; break;
      default:            language= "english"; break;
      }
      break;
    case LANG_FRENCH:     language= "french"; break;
    case LANG_GERMAN:     language= "german"; break;
    case LANG_GREEK:      language= "greek"; break;
    case LANG_HUNGARIAN:  language= "hungarian"; break;
    case LANG_ITALIAN:    language= "italian"; break;
    case LANG_JAPANESE:   language= "japanese"; break;
    case LANG_KOREAN:     language= "korean"; break;
    case LANG_POLISH:     language= "polish"; break;
    case LANG_PORTUGUESE: language= "portuguese"; break;
    case LANG_ROMANIAN:   language= "romanian"; break;
    case LANG_RUSSIAN:    language= "russian"; break;
    case LANG_SLOVAK:     language= "slovak"; break;
    case LANG_SLOVENIAN:  language= "slovene"; break;
    case LANG_SPANISH:    language= "spanish"; break;
    case LANG_SWEDISH:    language= "swedish"; break;
    case LANG_UKRAINIAN:  language= "ukrainian"; break;
    default:              language= "english"; break;
    }
  }
  return language;
}
#endif

#ifdef OS_MACOS
string
get_mac_language () {
  char mac_lang[50];
  CFLocaleRef locale= CFLocaleCopyCurrent ();
  CFTypeRef lang= CFLocaleGetValue (locale, kCFLocaleLanguageCode);
  CFStringGetCString ((CFStringRef) lang, mac_lang, sizeof (mac_lang), kCFStringEncodingUTF8);
  CFRelease (locale);
  return string (mac_lang);
}
#endif

string
locale_to_language (string s) {
  if (N(s) > 5) s= s (0, 5);
  if (s == "en_GB") return "british";
  if (s == "zh_TW") return "taiwanese";
  if (N(s) > 2) s= s (0, 2);
  if (s == "bg") return "bulgarian";
  if (s == "zh") return "chinese";
  if (s == "hr") return "croatian";
  if (s == "cs") return "czech";
  if (s == "da") return "danish";
  if (s == "nl") return "dutch";
  if (s == "en") return "english";
  if (s == "eo") return "esperanto";
  if (s == "fi") return "finnish";
  if (s == "fr") return "french";
  if (s == "de") return "german";
  if (s == "gr") return "greek";
  if (s == "hu") return "hungarian";
  if (s == "it") return "italian";
  if (s == "ja") return "japanese";
  if (s == "ko") return "korean";
  if (s == "pl") return "polish";
  if (s == "pt") return "portuguese";
  if (s == "ro") return "romanian";
  if (s == "ru") return "russian";
  if (s == "sk") return "slovak";
  if (s == "sl") return "slovene";
  if (s == "es") return "spanish";
  if (s == "sv") return "swedish";
  if (s == "uk") return "ukrainian";
  return "english";
}

string
language_to_locale (string s) {
  if (s == "american")   return "en_US";
  if (s == "british")    return "en_GB";
  if (s == "bulgarian")  return "bg_BG";
  if (s == "chinese")    return "zh_CN";
  if (s == "croatian")   return "hr_HR";
  if (s == "czech")      return "cs_CZ";
  if (s == "danish")     return "da_DK";
  if (s == "dutch")      return "nl_NL";
  if (s == "english")    return "en_US";
  if (s == "esperanto")  return "eo_EO";
  if (s == "finnish")    return "fi_FI";
  if (s == "french")     return "fr_FR";
  if (s == "german")     return "de_DE";
  if (s == "greek")      return "gr_GR";
  if (s == "hungarian")  return "hu_HU";
  if (s == "italian")    return "it_IT";
  if (s == "japanese")   return "ja_JP";
  if (s == "korean")     return "ko_KR";
  if (s == "polish")     return "pl_PL";
  if (s == "portuguese") return "pt_PT";
  if (s == "romanian")   return "ro_RO";
  if (s == "russian")    return "ru_RU";
  if (s == "slovak")     return "sk_SK";
  if (s == "slovene")    return "sl_SI";
  if (s == "spanish")    return "es_ES";
  if (s == "swedish")    return "sv_SV";
  if (s == "taiwanese")  return "zh_TW";
  if (s == "ukrainian")  return "uk_UA";
  return "en_US";
}

string
language_to_local_ISO_charset (string s) {
  if (s == "bulgarian")  return "ISO-8859-5";
  if (s == "chinese")    return "";
  if (s == "croatian")   return "ISO-8859-2";
  if (s == "czech")      return "ISO-8859-2";
  if (s == "greek")      return "ISO-8859-7";
  if (s == "hungarian")  return "ISO-8859-2";
  if (s == "japanese")   return "";
  if (s == "korean")     return "";
  if (s == "polish")     return "ISO-8859-2";
  if (s == "romanian")   return "ISO-8859-2";
  if (s == "russian")    return "ISO-8859-5";
  if (s == "slovak")     return "ISO-8859-2";
  if (s == "slovene")    return "ISO-8859-2";
  if (s == "taiwanese")  return "";
  if (s == "ukrainian")  return "ISO-8859-5";
  return "ISO-8859-1";
}

string
get_locale_language () {
#if OS_MINGW
  return windows_locale_to_language ();
#else
  string env_lan= get_env ("LC_ALL");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LC_MESSAGES");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("LANG");
  if (env_lan != "") return locale_to_language (env_lan);
  env_lan= get_env ("GDM_LANG");
  if (env_lan != "") return locale_to_language (env_lan);
#ifdef OS_MACOS
  return locale_to_language (get_mac_language ());
#endif
  return "english";
#endif
}

string
get_locale_charset () {
#ifdef OS_MINGW
  // in principle for now we use 8-bit codepage stuff in windows (at least for filenames);
  // return language_to_local_ISO_charset (get_locale_language ());
  return "UTF-8"; // do not change this!
  // otherwise there is a weird problem with page width shrinking on screen
#elif OS_MACOS
  return "UTF-8";
#elif X11TEXMACS
  return "UTF-8";
#elif OS_HAIKU
  return "UTF-8";
#elif OS_ANDROID
  return "UTF-8";
#else
  std::locale previous= std::locale::global (std::locale(""));
  string charset= string (nl_langinfo (CODESET));
  std::locale::global (previous);
  return charset;
#endif
}

std::locale
get_std_locale (string language) {
  {
    string loc= language_to_locale(language) * ".UTF-8";
    c_string _loc (loc);
    try {
      return std::locale (_loc);
    } catch (std::runtime_error&) {
      std_warning << "locale " << loc << " not found\n";
    }
  }

  {
    string loc= language_to_locale(language);
    loc[2] = '-';
    c_string _loc (loc);
    try {
      return std::locale (_loc);
    } catch (std::runtime_error&) {
      std_warning << "locale " << loc << " not found\n";
    }
  }

  std::locale loc= std::locale::classic(); 
  std::wcout.imbue(loc);
  string loc_name(loc.name().c_str());
  std_warning << "falling back to locale " << loc_name << "\n";
  return loc;
}

/******************************************************************************
* Getting a formatted date
******************************************************************************/

#ifdef QTTEXMACS
string
get_date (string lan, string fm) {
  return qt_get_date(lan, fm);
}

string
pretty_time (int t) {
  return qt_pretty_time (t);
}

string
pretty_date (int t, string fm) {
  return qt_pretty_date (t, fm);
}
#else

static bool
invalid_format (string s) {
  if (N(s) == 0) return true;
  for (int i=0; i<N(s); i++)
    if (!(is_alpha (s[i]) || is_numeric (s[i]) ||
	  s[i] == ' ' || s[i] == '%' || s[i] == '.' || s[i] == ',' ||
	  s[i] == '+' || s[i] == '-' || s[i] == ':'))
      return true;
  return false;
}

static string
system_date (string lan, string fm) {
  // the output of the date command for the format fm, in the language lan
  lan= language_to_locale (lan);
  string lvar= "LC_TIME";
  if (get_env (lvar) == "") lvar= "LC_ALL";
  if (get_env (lvar) == "") lvar= "LANG";
  string old= get_env (lvar);
  set_env (lvar, lan);
  // the errors and warnings (of the shell, of a missing locale) apart: with
  // system (cmd, out), which appends 2>&1, they were taken for the date
  string date, errors;
  int status= system ("date +\"" * fm * "\"", date, errors);
  while (N(date) > 0 && (date[N(date)-1] == '\n' || date[N(date)-1] == '\r'))
    date= date (0, N(date) - 1);
  if ((lan == "cz_CZ") || (lan == "hu_HU") || (lan == "pl_PL"))
    date= il2_to_cork (date);
  // if (lan == "ru_RU") date= iso_to_koi8 (date);
  set_env (lvar, old);
  if ((status != 0 || N(date) == 0) && N(fm) > 0) {
    // no date command (a browser has no processes, a system may lack
    // it): the C library, in its own locale, so the names are in English
    char buf[256];
    time_t ti;
    time (&ti);
    c_string _fm (fm);
    size_t len= strftime (buf, sizeof (buf), _fm, localtime (&ti));
    date= string (buf, (int) len);
  }
  return date;
}

static string
two_digits (int n) {
  return (n < 10? string ("0"): string ("")) * as_string (n);
}

static bool
has_am_pm (string fm) {
  // whether fm contains an AM/PM marker (a or A) outside quoted text
  bool quoted= false;
  for (int i=0; i<N(fm); i++)
    if (fm[i] == '\'') quoted= !quoted;
    else if (!quoted && (fm[i] == 'a' || fm[i] == 'A')) return true;
  return false;
}

static string
c_date (struct tm* tm, string fm) {
  // the date tm in the strftime format fm, by the C library, in its own
  // locale (the names are in English)
  char buf[256];
  c_string _fm (fm);
  size_t len= strftime (buf, sizeof (buf), _fm, tm);
  return string (buf, (int) len);
}

static string
pattern_date (string lan, string fm, time_t ti, bool now) {
  // the current date in the format fm, with the patterns of Qt (which
  // the Qt version of get_date uses): d dd ddd dddd for the day, M MM MMM
  // MMMM for the month, yy yyyy for the year, h hh H HH m mm s ss for the
  // time (h and hh on 12 hours when there is an AM/PM marker AP A ap a),
  // and 'text' quoted. The date is the one of ti, the current one when
  // now holds: its names of the months and days then come from date, in the
  // language lan (otherwise from the C library, in English); the format
  // itself is never passed to the shell.
  struct tm tm= *localtime (&ti);
  bool am_pm= has_am_pm (fm);
  string r;
  int i= 0, n= N(fm);
  while (i < n) {
    char c= fm[i];
    if (c == '\'') {
      // quoted text; two quotes are a quote, inside quoted text too
      i++;
      if (i < n && fm[i] == '\'') { r << '\''; i++; continue; }
      while (i < n) {
        if (fm[i] != '\'') r << fm[i++];
        else if (i+1 < n && fm[i+1] == '\'') { r << '\''; i += 2; }
        else break;
      }
      if (i < n) i++;
      continue;
    }
    int k= i;
    while (k < n && fm[k] == c) k++;
    int count= k - i;
    if (c == 'd') {
      if (count == 1) r << as_string (tm.tm_mday);
      else if (count == 2) r << two_digits (tm.tm_mday);
      else if (count == 3) r << (now? system_date (lan, "%a"): c_date (&tm, "%a"));
      else r << (now? system_date (lan, "%A"): c_date (&tm, "%A"));
    }
    else if (c == 'M') {
      if (count == 1) r << as_string (tm.tm_mon + 1);
      else if (count == 2) r << two_digits (tm.tm_mon + 1);
      else if (count == 3) r << (now? system_date (lan, "%b"): c_date (&tm, "%b"));
      else r << (now? system_date (lan, "%B"): c_date (&tm, "%B"));
    }
    else if (c == 'y' && count == 2) r << two_digits ((tm.tm_year + 1900) % 100);
    else if (c == 'y' && count == 4) r << as_string (tm.tm_year + 1900);
    else if ((c == 'h' || c == 'H' || c == 'm' || c == 's') && count <= 2) {
      int v= (c == 'm'? tm.tm_min: c == 's'? tm.tm_sec: tm.tm_hour);
      if (c == 'h' && am_pm) v= (v % 12 == 0? 12: v % 12);
      r << (count == 1? as_string (v): two_digits (v));
    }
    else if (c == 'A' || c == 'a') {
      // AP, A, ap or a
      bool up= (c == 'A');
      r << (tm.tm_hour < 12? (up? "AM": "am"): (up? "PM": "pm"));
      k= i + 1;
      if (k < n && fm[k] == (up? 'P': 'p')) k++;
    }
    else r << fm (i, k);
    i= k;
  }
  return r;
}

static string
long_date (string lan, time_t ti, bool now) {
  // the date ti in the long format of the language lan
  if (lan == "chinese" || lan == "japanese" ||
      lan == "korean" || lan == "taiwanese") {
    // numbers only: no date command is needed
    struct tm tm= *localtime (&ti);
    string y= as_string (tm.tm_year + 1900);
    string m= as_string (tm.tm_mon + 1);
    string d= as_string (tm.tm_mday);
    if (lan == "korean")
      return y * "<#b144> " * m * "<#c6d4> " * d * "<#c77c>";
    return y * "<#5e74>" * m * "<#6708>" * d * "<#65e5>";
  }
  string fm= "d MMMM yyyy";
  if ((lan == "british") || (lan == "english") || (lan == "american"))
    fm= "MMMM d, yyyy";
  else if (lan == "german")
    fm= "d. MMMM yyyy";
  return pattern_date (lan, fm, ti, now);
}

string
get_date (string lan, string fm) {
//#ifdef OS_MINGW
//  return win32::get_date(lan, fm);
  // as the Qt version: a strftime format if fm starts with %, the default
  // of the language if fm is empty, and Qt patterns otherwise
  if (N(fm) > 0 && fm[0] == '%' && !invalid_format (fm))
    return system_date (lan, fm);
  time_t ti;
  time (&ti);
  if (N(fm) == 0 || fm[0] == '%') return long_date (lan, ti, true);
  return pattern_date (lan, fm, ti, true);
}

// The date and time of t, as the Qt versions give them. They used to run
// "date -r t", which only BSD date reads as seconds: GNU date takes -r for
// a reference file, and gave an error message.

string
pretty_time (int t) {
  // as QDateTime::toString: Mon Oct 6 14:05:09 2026
  time_t ti= (time_t) t;
  return pattern_date ("english", "ddd MMM d HH:mm:ss yyyy", ti, false);
}

string
pretty_date (int t, string fm) {
  // as the Qt version: the short or the long date of the language (here
  // English, as the names), or the date in the Qt pattern fm
  time_t ti= (time_t) t;
  if (fm == "short") {
    struct tm tm= *localtime (&ti);
    return c_date (&tm, "%x");
  }
  if (fm == "") return long_date ("english", ti, false);
  return pattern_date ("english", fm, ti, false);
}
#endif

