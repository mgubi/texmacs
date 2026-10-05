
/******************************************************************************
* MODULE     : web_files.cpp
* DESCRIPTION: file handling via the web
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "file.hpp"
#include "web_files.hpp"
#include "sys_utils.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "scheme.hpp"

#ifdef QTTEXMACS
#include "qt_utilities.hpp"
#endif

#ifndef OS_MINGW
#include <fcntl.h>
#include <unistd.h>
#endif

#define MAX_CACHED 25
static int web_nr=0;
static array<tree> web_cache (MAX_CACHED);
static hashmap<tree,tree> web_cache_resolve ("");

/******************************************************************************
* Caching
******************************************************************************/

static url
get_cache (url name) {
  if (web_cache_resolve->contains (name->t)) {
    int i, j;
    tree tmp= web_cache_resolve [name->t];
    for (i=0; i<MAX_CACHED; i++)
      if (web_cache[i] == name->t) {
        // cout << name << " in cache as " << tmp << " at " << i << "\n";
        for (j=i; ((j+1) % MAX_CACHED) != web_nr; j= (j+1) % MAX_CACHED)
          web_cache[j]= web_cache[(j+1) % MAX_CACHED];
        web_cache[j]= name->t;
        break;
      }
    return as_url (tmp); // url_system (tmp);
  }
  return url_none ();
}

static url
set_cache (url name, url tmp) {
  web_cache_resolve->reset (web_cache [web_nr]);
  web_cache [web_nr]= name->t;
  web_cache_resolve (name->t)= tmp->t;
  web_nr= (web_nr+1) % MAX_CACHED;
  return tmp;
}

void
web_cache_invalidate (url name) {
  for (int i=0; i<MAX_CACHED; i++)
    if (web_cache[i] == name->t) {
      web_cache[i]= tree ("");
      web_cache_resolve->reset (name->t);
    }
}

/******************************************************************************
* Web files
******************************************************************************/

#if !defined(QTTEXMACS) || QT_VERSION < 0x060000
// not used
static string
web_encode (string s) {
  return tm_decode (s);
}

static string
fetch_tool () {
  static bool done= false;
  static string tool= "";
  if (done) return tool;
  if (tool == "" && exists_in_path ("wget")) tool= "wget";
  if (tool == "" && exists_in_path ("curl")) tool= "curl";
  done= true;
  return tool;
}
#endif

url
get_from_web (url name) {
  if (!is_rooted_web (name)) return url_none ();
  if (is_concat (name) && is_root (name[1], "doi")) {
    url u= url_root ("https") * (url ("www.doi.org") * name[2]);
    return get_from_web (u);
  }
  url res= get_cache (name);
  if (!is_none (res)) return res;

#if defined(QTTEXMACS) && QT_VERSION >= 0x060000
  url tmp= url_temp ();
  string tmp_s= concretize (tmp);
  string name_s= as_string (name);
  if (DEBUG_IO)
    debug_io << "get_from_web, downloading remote file "
	     << name_s << " into " << tmp_s << LF;
  qt_download_file (name_s, tmp_s);
#else
  string tool= fetch_tool ();
  if (tool == "") return url_none ();
  
  url tmp= url_temp ();
  string tmp_s= escape_sh (concretize (tmp));
  string cmd= "";
  
  if (tool == "wget") {
    cmd= "wget --header='User-Agent: TeXmacs-" TEXMACS_VERSION "' -q";
    cmd << " --no-check-certificate --tries=1";
    cmd << " -O " << tmp_s << " " << escape_sh (web_encode (as_string (name)));
  }
  
  if (tool == "curl") {
    cmd= "curl --user-agent TeXmacs-" TEXMACS_VERSION;
    cmd << " " << escape_sh (web_encode (as_string (name)));
    cmd << " --output " << tmp_s;
  }

  //cout << cmd << LF;
  system (cmd);
  //cout << "got " << name << " as " << tmp << LF;
#endif // QTTEXMACS, Qt >= 6.0

  if (file_size (tmp) <= 0) {
    remove (tmp);
    return url_none ();
  }
  else return set_cache (name, tmp);
}

/******************************************************************************
* Files from a hyperlink file system
******************************************************************************/

url
get_from_server (url u) {
  if (!is_rooted_tmfs (u)) return url_none ();
  url res= get_cache (u);
  if (!is_none (res)) return res;

  string name= as_string (u);
  if (ends (name, "~") || ends (name, "#")) {
    if (!is_rooted_tmfs (name)) return url_none ();
    if (!as_bool (call ("tmfs-can-autosave?", unglue (u, 1))))
      return url_none ();
  }
  string r= as_string (call ("tmfs-load", object (name)));
  if (r == "") return url_none ();
  url tmp= url_temp (string (".") * suffix (name));
  (void) save_string (tmp, r, true);

  //return set_cache (u, tmp);
  return tmp;
  // FIXME: certain files could be cached, but others not
  // for instance, files which are loaded in a delayed fashion
  // would always be cached as empty files, which is erroneous.
}

bool
save_to_server (url u, string s) {
  if (!is_rooted_tmfs (u)) return true;
  string name= as_string (u);
  (void) call ("tmfs-save", object (name), object (s));
  return false;
}

/******************************************************************************
* Ramdisc
******************************************************************************/

url
get_from_ramdisc (url u) {
  if (!is_ramdisc (u)) return url_none ();
  url res= get_cache (u);
  if (!is_none (res)) return (res);
  url tmp= url_temp (string (".") * suffix (u));
  save_string (tmp, u[1][2]->t->label);
  return set_cache (u, tmp);
}

/******************************************************************************
* Keeping secrets in HTTP headers off command lines and out of logs
******************************************************************************/

string
shell_quote (string s) {
  // quote s as a single word for a POSIX shell
  return "'" * replace (s, "'", "'\\''") * "'";
}

bool
http_secret_header (string name) {
  name= locase_all (name);
  return name == "authorization" || name == "proxy-authorization" ||
         occurs ("api-key", name) || occurs ("api_key", name) ||
         occurs ("token", name) || occurs ("secret", name);
}

array<string>
http_mask_headers (array<string> headers_attr) {
  array<string> r= copy (headers_attr);
  for (int i= 0; i+1 < N(r); i += 2)
    if (http_secret_header (r[i]) && r[i+1] != "") r[i+1]= "***";
  return r;
}

tree
http_mask_request (tree t) {
  // hide the secret header values of an (http_post url headers data) tree
  if (!is_compound (t, "http_post") || N(t) < 2 || !is_tuple (t[1])) return t;
  tree h= copy (t[1]);
  for (int i= 0; i+1 < N(h); i += 2)
    if (is_atomic (h[i]) && http_secret_header (h[i]->label) &&
        h[i+1] != "")
      h[i+1]= "***";
  tree r= copy (t);
  r[1]= h;
  return r;
}

static bool
save_private_string (url u, string s) {
  // like save_string, but the file is only readable by the user
#ifdef OS_MINGW
  return save_string (u, s);
#else
  c_string name (concretize (u));
  int fd= open (name, O_WRONLY | O_CREAT | O_EXCL, 0600);
  if (fd < 0) return true;
  bool err= ::write (fd, &s[0], N(s)) != N(s);
  return close (fd) != 0 || err;
#endif
}

string
curl_command (string args, array<string> headers_attr) {
  // Shell command for 'curl args', the HTTP headers being passed to curl
  // on its standard input from a temporary file of mode 600,
  // which is removed as soon as the shell has opened it.
  // The file is written when the command is built: if the command is
  // never run, the file stays in the temporary directory until it is
  // removed at exit, and the command can only be run once.
  string h;
  for (int i= 0; i+1 < N(headers_attr); i += 2) {
    string line= headers_attr[i] * ": " * headers_attr[i+1];
    h << replace (replace (line, "\r", ""), "\n", "") << "\n";
  }
  if (h == "") return "curl " * args;
  url tmp= url_temp (".txt");
  if (save_private_string (tmp, h)) {
    io_error << "curl_command, cannot write headers to "
             << as_string (tmp) << LF;
    return "";
  }
  string f= shell_quote (as_string (tmp));
  return "{ rm -f " * f * "; curl -H @- " * args * "; } < " * f;
}

/******************************************************************************
* HTTP requests
******************************************************************************/

#if !defined(QTTEXMACS) || AC_QT_MAJOR_VERSION < 6

static string
to_shell_command (string url, array<string> headers_attr, string data) {
  string args= "--silent --no-buffer -X POST " * shell_quote (url) * " \\\n";
  args << "  --data-binary " << shell_quote (data);
  string cmd= curl_command (args, headers_attr);
  if (DEBUG_IO)
    debug_io << "http_post, launching" << LF
	     << cmd << LF;
  return cmd;
}

static inline string
to_shell_command (string url, array<string> headers_attr, tree data) {
  return to_shell_command (url, headers_attr, tree_to_json (data));
}

static string
to_shell_command (string url, array<string> headers_attr, array<string> attr) {
  string args= "--silent --no-buffer -X POST " * shell_quote (url);
  for (int i= 0; i+1 < N(attr); i += 2) {
    args << " \\\n  --data-urlencode " << shell_quote (attr[i]);
    if (!ends (attr[i], "@")) args << "=";
    args << shell_quote (attr[i+1]);
  }
  string cmd= curl_command (args, headers_attr);
  if (DEBUG_IO)
    debug_io << "http_post, launching" << LF
	     << cmd << LF;
  return cmd;
}

int
http_post (string& ret, string url,
	   array<string> headers_attr, string data) {
  string cmd= to_shell_command (url, headers_attr, data);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

int
http_post_json (string& ret, string url,
		array<string> headers_attr, tree data) {
  string cmd= to_shell_command (url, headers_attr, data);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

int
http_post_query (string& ret, string url,
		 array<string> headers_attr, array<string> attr) {
  string cmd= to_shell_command (url, headers_attr, attr);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

bool
async_http_post (string url, array<string> headers_attr,
		 string data, object callback) {
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_json (string url, array<string> headers_attr,
		      tree data, object callback) {
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_query (string url, array<string> headers_attr,
		       array<string> attr, object callback) {
  string cmd= to_shell_command (url, headers_attr, attr);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_json (string url, array<string> headers_attr, tree data,
		      int& status, string& outbuf, string& errbuf,
		      bool& kill) {
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, status, outbuf, errbuf, kill);  
}

#endif

