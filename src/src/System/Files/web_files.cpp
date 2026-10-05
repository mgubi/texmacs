
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

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include "tm_timer.hpp"
#endif

#define MAX_CACHED 25
static int web_nr=0;
static array<tree> web_cache (MAX_CACHED);
static hashmap<tree,tree> web_cache_resolve ("");

/******************************************************************************
* The web from a page of the browser (the Emscripten build)
*
* A page runs no program: what wget and curl do elsewhere (the downloads, the
* requests of http_post and its variants) is done by the browser, with an
* XMLHttpRequest when the answer is needed at once and with fetch when it
* is not (async_http_post...). The site has to let the page read its answer
* (CORS: Access-Control-Allow-Origin), and the browser does not let a page
* set some headers (User-Agent...). Without XMLHttpRequest (the build for
* node) the programs are used, as elsewhere.
*
* A request is a slot of tmWebHttp, known by its number: it is done, with
* the status of the answer (0 when there was none: no network, CORS), or it
* is not yet (-2), and its bytes are then taken by TeXmacs. The answers of
* the requests made with fetch are taken by async_eval_pending, which the
* main loop calls, as it takes the output of the programs.
******************************************************************************/

#ifdef __EMSCRIPTEN__
EM_JS (int, web_http_available, (), {
  return typeof XMLHttpRequest !== 'undefined' ? 1 : 0;
});

// GET (body == 0 and len < 0) or POST; headers: "name\nvalue\n..."; a
// synchronous request is done when this returns
EM_JS (int, web_http_start, (const char* url, const char* headers,
                             const char* body, int len, int sync), {
  var web = Module.tmWebHttp || (Module.tmWebHttp = { next: 1, slots: {} });
  var id = web.next++, slot = { status: -2, bytes: null };
  web.slots[id] = slot;
  var u = UTF8ToString (url), h = UTF8ToString (headers).split ('\n'), hs = {};
  for (var i = 0; i + 1 < h.length; i += 2) if (h[i] !== '') hs[h[i]] = h[i+1];
  var post = len >= 0;
  if (post && !Object.keys (hs).some (function (k) { return k.toLowerCase () === 'content-type'; }))
    hs['Content-Type'] = 'application/x-www-form-urlencoded'; // as curl
  var data = post ? HEAPU8.slice (body, body + len) : null;
  function done (status, bytes) {
    slot.status = status; slot.bytes = bytes;
    // the loop of the page, which may sleep, takes the answer at once
    if (!sync && typeof _vue_web_wake !== 'undefined') _vue_web_wake ();
  }
  function failed (e) {
    if (e && e.name === 'AbortError') return; // stopped: nobody waits for it
    console.warn ('TeXmacs: no answer from ' + u + (e ? ': ' + e : '') +
                  (new URL (u, location.href).origin !== location.origin ?
                   ' (does the site allow it? CORS)' : ''));
    done (0, new Uint8Array (0));
  }
  if (sync) {
    try {
      var xhr = new XMLHttpRequest ();
      xhr.open (post ? 'POST' : 'GET', u, false);
      for (var k in hs) try { xhr.setRequestHeader (k, hs[k]); } catch (e) {}
      xhr.overrideMimeType ('text/plain; charset=x-user-defined'); // the bytes
      xhr.send (data);
      var s = xhr.responseText, b = new Uint8Array (s.length);
      for (var j = 0; j < s.length; j++) b[j] = s.charCodeAt (j) & 0xff;
      if (xhr.status === 0) failed (); else done (xhr.status, b);
    } catch (e) { failed (e); }
  }
  // the answer is read as it comes (an answer streamed by the AI engines):
  // its pieces so far are slot.parts, which a request link shows
  else {
    // it may be stopped (web_http_abort: a session interrupted)
    slot.controller = typeof AbortController !== 'undefined' ? new AbortController () : null;
    fetch (u, { method: post ? 'POST' : 'GET', headers: hs, body: data,
                signal: slot.controller ? slot.controller.signal : undefined })
      .then (function (r) {
        if (!r.body || !r.body.getReader)
          return r.arrayBuffer ().then (function (a) { done (r.status, new Uint8Array (a)); });
        var reader = r.body.getReader ();
        slot.parts = []; slot.length = 0;
        function pump () {
          return reader.read ().then (function (x) {
            if (x.done) {
              var b = new Uint8Array (slot.length), at = 0;
              slot.parts.forEach (function (p) { b.set (p, at); at += p.length; });
              slot.parts = null;
              done (r.status, b);
              return;
            }
            slot.parts.push (x.value);
            slot.length += x.value.length;
            if (typeof _vue_web_wake !== 'undefined') _vue_web_wake ();
            return pump ();
          });
        }
        return pump ();
      })
      .catch (failed);
  }
  return id;
});

// a request whose answer is no longer wanted: stopped (the server stops
// sending it, an AI engine stops writing it), and forgotten
EM_JS (void, web_http_abort, (int id), {
  var web = Module.tmWebHttp, slot = web && web.slots[id];
  if (!slot) return;
  if (slot.controller) try { slot.controller.abort (); } catch (e) {}
  delete web.slots[id];
});

// the status of the answer of request id, -2 while there is none yet
EM_JS (int, web_http_status, (int id), {
  var slot = Module.tmWebHttp && Module.tmWebHttp.slots[id];
  return slot ? slot.status : 0;
});

// what came so far of an answer which has not ended: its size, and its
// bytes copied to buf (which has that size)
EM_JS (int, web_http_partial_length, (int id), {
  var slot = Module.tmWebHttp && Module.tmWebHttp.slots[id];
  return slot && slot.parts ? slot.length : 0;
});
EM_JS (void, web_http_partial_take, (int id, char* buf), {
  var slot = Module.tmWebHttp && Module.tmWebHttp.slots[id];
  if (!slot || !slot.parts || !buf) return;
  var at = 0;
  slot.parts.forEach (function (p) { HEAPU8.set (p, buf + at); at += p.length; });
});

// the size of the answer, and its bytes copied to buf (which has that
// size); the request is then forgotten
EM_JS (int, web_http_length, (int id), {
  var slot = Module.tmWebHttp && Module.tmWebHttp.slots[id];
  return slot && slot.bytes ? slot.bytes.length : 0;
});
EM_JS (void, web_http_take, (int id, char* buf), {
  var slot = Module.tmWebHttp && Module.tmWebHttp.slots[id];
  if (slot && slot.bytes && buf) HEAPU8.set (slot.bytes, buf);
  if (Module.tmWebHttp) delete Module.tmWebHttp.slots[id];
});

static int
web_request (string url, array<string> headers_attr, string body, bool post,
             bool sync) {
  string h;
  for (int i= 0; i+1 < N(headers_attr); i += 2)
    h << headers_attr[i] << "\n" << headers_attr[i+1] << "\n";
  c_string u (url), hs (h);
  const char* b= (post && N(body) > 0) ? &(body[0]) : "";
  return web_http_start (u, hs, b, post ? N(body) : -1, sync ? 1 : 0);
}

// the answer of request id, when it has come (then forgotten)
static bool
web_answer (int id, int& status, string& out) {
  status= web_http_status (id);
  if (status == -2) return false;
  int n= web_http_length (id);
  string r (n);
  web_http_take (id, n > 0 ? &(r[0]) : NULL);
  out= r;
  return true;
}

// url encoding as curl --data-urlencode does it
static string
web_url_encode (string s) {
  string r;
  for (int i= 0; i < N(s); i++) {
    unsigned char c= (unsigned char) s[i];
    if (is_alpha (s[i]) || is_digit (s[i]) ||
        c == '-' || c == '.' || c == '_' || c == '~') r << s[i];
    else r << "%" << as_hexadecimal ((int) c, 2);
  }
  return r;
}

// the body of http_post_query: attr are pairs of a name and a value, or of
// "name@" and a file whose contents are the value (as for curl)
static string
web_query_body (array<string> attr) {
  string r;
  for (int i= 0; i+1 < N(attr); i += 2) {
    string name= attr[i], val= attr[i+1];
    if (ends (name, "@")) {
      name= name (0, N(name) - 1);
      string contents;
      if (!load_string (url_system (val), contents, false)) val= contents;
      else val= "";
    }
    if (N(r) > 0) r << "&";
    if (N(name) > 0) r << web_url_encode (name) << "=";
    r << web_url_encode (val);
  }
  return r;
}

// a request made now; 0 when it had an answer, as the shell commands
static int
web_post (string& ret, string url, array<string> headers_attr, string body) {
  int id= web_request (url, headers_attr, body, true, true), status;
  web_answer (id, status, ret);
  return status > 0 ? 0 : 1;
}

// the requests made with fetch whose answers are awaited
struct web_async_handle {
  int id;
  string url;
  object call_back;
  int* status; string* outbuf; string* errbuf;
};
static array<web_async_handle*> web_async_busy;

static bool
web_async_post (string url, array<string> headers_attr, string body,
                object call_back, int* status= NULL, string* outbuf= NULL,
                string* errbuf= NULL) {
  web_async_handle* h= tm_new<web_async_handle> ();
  h->id= web_request (url, headers_attr, body, true, false);
  h->url= url;
  h->call_back= call_back;
  h->status= status; h->outbuf= outbuf; h->errbuf= errbuf;
  web_async_busy << h;
  return false; // no error (async_eval_system returns true when it fails)
}

// the requests whose answer goes to outbuf (a request link which is
// interrupted, stopped, or asks again): stopped and forgotten, so that an
// answer which comes later does not go where the next one is awaited
void
web_async_cancel (string* outbuf) {
  for (int i= 0; i < N(web_async_busy); ) {
    web_async_handle* h= web_async_busy[i];
    if (h->outbuf != outbuf) { i++; continue; }
    web_http_abort (h->id);
    web_async_busy= append (range (web_async_busy, 0, i),
                            range (web_async_busy, i + 1, N(web_async_busy)));
    tm_delete<web_async_handle> (h);
  }
}

// the answers which came (called by async_eval_pending)
void
web_async_pending () {
  for (int i= 0; i < N(web_async_busy); ) {
    web_async_handle* h= web_async_busy[i];
    int st; string out;
    if (!web_answer (h->id, st, out)) {
      // a request link sees what came so far (its status is not changed:
      // it is still waiting)
      if (h->outbuf != NULL) {
        int n= web_http_partial_length (h->id);
        if (n != N(*(h->outbuf))) {
          string r (n);
          if (n > 0) web_http_partial_take (h->id, &(r[0]));
          *(h->outbuf)= r;
        }
      }
      i++; continue;
    }
    web_async_busy= append (range (web_async_busy, 0, i),
                            range (web_async_busy, i + 1, N(web_async_busy)));
    if (h->status == NULL) call (h->call_back, out);
    else {
      // no answer at all (no network, or the site does not let a page ask
      // it: CORS) is said on the channel of the errors of the link
      *(h->status)= 0; *(h->outbuf)= out;
      *(h->errbuf)= (st == 0 && N(out) == 0)
        ? "No answer from " * h->url *
          " (no network, or the site does not answer the requests of a web page)\n"
        : string ("");
    }
    tm_delete<web_async_handle> (h);
  }
}
#endif

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

#ifdef __EMSCRIPTEN__
  if (web_http_available ()) {
    // a failure is remembered for a while: loading a document asks for it
    // several times (does it exist, its format, the document), and each
    // request holds the page until the network answers
    static hashmap<string,int> failed (0);
    string key= as_string (name);
    int now= (int) (texmacs_time () / 1000);
    if (failed->contains (key) && now - failed[key] < 10) return url_none ();
    int id= web_request (key, array<string> (), "", false, true);
    int status; string bytes;
    web_answer (id, status, bytes);
    if (status < 200 || status >= 300 || N(bytes) == 0) {
      failed (key)= now;
      return url_none ();
    }
    failed->reset (key);
    url tmp= url_temp ();
    if (save_string (tmp, bytes, false)) return url_none ();
    return set_cache (name, tmp);
  }
#endif

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

  if (file_size (url_system (tmp_s)) <= 0) {
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
* HTTP requests
******************************************************************************/

#if !defined(QTTEXMACS) || AC_QT_MAJOR_VERSION < 6

static inline string
shell_quote (string s) {
  return "'" * replace (s, "'", "'\\''") * "'";
}

static string
to_shell_command (string url, array<string> headers_attr, string data) {
  string cmd= "curl --silent --no-buffer -X POST " * shell_quote (url) * "\\\n";
  for (int i= 0; i+1 < N(headers_attr); i += 2)
    cmd << "  -H "
	<< shell_quote (headers_attr[i] * ":" * headers_attr[i+1]) << "\\\n";
  cmd << "  --data-binary " << shell_quote (data) << "\\\n";
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
  string cmd= "curl --silent --no-buffer -X POST " * shell_quote (url) * " \\\n";
  for (int i= 0; i+1 < N(headers_attr); i += 2)
    cmd << "  -H "
	<< shell_quote (headers_attr[i] * ":" * headers_attr[i+1]) << "\\\n";
  for (int i= 0; i+1 < N(attr); i += 2) {
    cmd << "  --data-urlencode " << shell_quote (attr[i]);
    if (!ends (attr[i], "@")) cmd << "=";
    cmd << shell_quote (attr[i+1]) << "\\\n";
  }
  if (DEBUG_IO)
    debug_io << "http_post, launching" << LF
	     << cmd << LF;
  return cmd;
}

int
http_post (string& ret, string url,
	   array<string> headers_attr, string data) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ()) return web_post (ret, url, headers_attr, data);
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

int
http_post_json (string& ret, string url,
		array<string> headers_attr, tree data) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_post (ret, url, headers_attr, tree_to_json (data));
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

int
http_post_query (string& ret, string url,
		 array<string> headers_attr, array<string> attr) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_post (ret, url, headers_attr, web_query_body (attr));
#endif
  string cmd= to_shell_command (url, headers_attr, attr);
  int st= system (cmd, ret);
  if (st != 0)
    io_error << "http_post, cannot evaluate shell command: " << cmd << LF;
  return st;
}

bool
async_http_post (string url, array<string> headers_attr,
		 string data, object callback) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_async_post (url, headers_attr, data, callback);
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_json (string url, array<string> headers_attr,
		      tree data, object callback) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_async_post (url, headers_attr, tree_to_json (data), callback);
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_query (string url, array<string> headers_attr,
		       array<string> attr, object callback) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_async_post (url, headers_attr, web_query_body (attr), callback);
#endif
  string cmd= to_shell_command (url, headers_attr, attr);
  return async_eval_system (cmd, callback);
}

bool
async_http_post_json (string url, array<string> headers_attr, tree data,
		      int& status, string& outbuf, string& errbuf,
		      bool& kill) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ())
    return web_async_post (url, headers_attr, tree_to_json (data), object (),
                           &status, &outbuf, &errbuf);
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, status, outbuf, errbuf, kill);  
}

#endif

