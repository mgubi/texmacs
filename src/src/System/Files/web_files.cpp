
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
#ifndef OS_MINGW
#include <fcntl.h>
#include <unistd.h>
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
    cmd << curl_proxy_option (as_string (name));
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
* The proxy of a request (libcurl and the curl program; Qt and the browsers
* find it themselves): the preference "http proxy" ("host:port", or with a
* scheme, "socks5h://host:port"; "direct" for none), else the variables of
* the environment (http_proxy, https_proxy, all_proxy, no_proxy, which curl
* reads itself), else, on macOS, the settings of the system (System
* Settings > Network > Proxies), with their exceptions and their automatic
* configuration (a PAC file). "" when curl decides, "direct" for none.
******************************************************************************/

#if defined(OS_MACOS) && !defined(__EMSCRIPTEN__)
#include <CoreFoundation/CoreFoundation.h>
#include <CFNetwork/CFNetwork.h>

static string
cf_to_string (CFStringRef s) {
  if (s == NULL) return "";
  char buf[1024];
  if (!CFStringGetCString (s, buf, sizeof (buf), kCFStringEncodingUTF8))
    return "";
  return string (buf);
}

// the PAC script at u (fetched without proxy, kept for 5 minutes)
static string
macos_pac_script (string u) {
  static string last_url, last_script;
  static time_t last_time= 0;
  time_t now= time (NULL);
  if (u == last_url && now - last_time < 300) return last_script;
  string s;
  if (starts (u, "file://")) {
    if (load_string (url_system (u (7, N(u))), s, false)) s= "";
  }
  else {
    string cmd= "curl --silent --max-time 5 --noproxy '*' '" *
      replace (u, "'", "'\\''") * "'";
    if (system (cmd, s) != 0) s= "";
  }
  last_url= u; last_script= s; last_time= now;
  return s;
}

static string
macos_proxy_entry (CFDictionaryRef p, CFURLRef target, int depth);

static string
macos_proxy_list (CFArrayRef ps, CFURLRef target, int depth) {
  if (ps == NULL) return "";
  for (CFIndex i= 0; i < CFArrayGetCount (ps); i++) {
    CFDictionaryRef p= (CFDictionaryRef) CFArrayGetValueAtIndex (ps, i);
    string r= macos_proxy_entry (p, target, depth);
    if (r != "") return r;
  }
  return "";
}

static string
macos_proxy_entry (CFDictionaryRef p, CFURLRef target, int depth) {
  CFStringRef type= (CFStringRef) CFDictionaryGetValue (p, kCFProxyTypeKey);
  if (type == NULL) return "";
  if (CFEqual (type, kCFProxyTypeNone)) return "direct";
  if (CFEqual (type, kCFProxyTypeHTTP) || CFEqual (type, kCFProxyTypeHTTPS) ||
      CFEqual (type, kCFProxyTypeSOCKS)) {
    string host= cf_to_string ((CFStringRef)
      CFDictionaryGetValue (p, kCFProxyHostNameKey));
    CFNumberRef pn= (CFNumberRef) CFDictionaryGetValue (p, kCFProxyPortNumberKey);
    int port= 0;
    if (pn != NULL) CFNumberGetValue (pn, kCFNumberIntType, &port);
    if (host == "") return "";
    string r= CFEqual (type, kCFProxyTypeSOCKS)? string ("socks5h://"):
                                                 string ("http://");
    r << host;
    if (port > 0) r << ":" << as_string (port);
    return r;
  }
  if (depth > 0) return "";
  // an automatic configuration: its script tells the proxies of the url
  CFStringRef script= NULL;
  bool release= false;
  if (CFEqual (type, kCFProxyTypeAutoConfigurationJavaScript))
    script= (CFStringRef)
      CFDictionaryGetValue (p, kCFProxyAutoConfigurationJavaScriptKey);
  else if (CFEqual (type, kCFProxyTypeAutoConfigurationURL)) {
    CFURLRef pac= (CFURLRef)
      CFDictionaryGetValue (p, kCFProxyAutoConfigurationURLKey);
    if (pac == NULL) return "";
    string js= macos_pac_script (cf_to_string (CFURLGetString (pac)));
    if (js == "") return "";
    c_string cjs (js);
    script= CFStringCreateWithCString (NULL, (char*) cjs, kCFStringEncodingUTF8);
    release= true;
  }
  if (script == NULL) return "";
  CFErrorRef err= NULL;
  CFArrayRef ps= CFNetworkCopyProxiesForAutoConfigurationScript
                   (script, target, &err);
  if (release) CFRelease (script);
  if (err != NULL) CFRelease (err);
  string r= macos_proxy_list (ps, target, depth + 1);
  if (ps != NULL) CFRelease (ps);
  return r;
}

static string
macos_system_proxy (string u) {
  CFDictionaryRef settings= CFNetworkCopySystemProxySettings ();
  if (settings == NULL) return "";
  c_string cu (u);
  CFURLRef target= CFURLCreateWithBytes (NULL, (const UInt8*) (char*) cu,
                                         N(u), kCFStringEncodingUTF8, NULL);
  string r;
  if (target != NULL) {
    CFArrayRef ps= CFNetworkCopyProxiesForURL (target, settings);
    r= macos_proxy_list (ps, target, 0);
    if (ps != NULL) CFRelease (ps);
    CFRelease (target);
  }
  CFRelease (settings);
  return r;
}
#endif

static bool
proxy_in_environment () {
  const char* vars[]= { "http_proxy", "HTTP_PROXY", "https_proxy",
                        "HTTPS_PROXY", "all_proxy", "ALL_PROXY", NULL };
  for (int i= 0; vars[i] != NULL; i++) {
    const char* v= getenv (vars[i]);
    if (v != NULL && v[0] != '\0') return true;
  }
  return false;
}

string
http_proxy (string u) {
#ifdef __EMSCRIPTEN__
  (void) u;
  return "";
#else
  string p= get_preference ("http proxy", "");
  if (p == "default") p= "";
  if (p != "") return p;
  if (proxy_in_environment ()) return "";
#if defined(OS_MACOS)
  return macos_system_proxy (u);
#else
  (void) u;
  return "";
#endif
#endif
}

// the option of a curl command line for the proxy of u
string
curl_proxy_option (string u) {
  string p= http_proxy (u);
  if (p == "") return "";
  if (p == "direct") return " --noproxy '*'";
  return " --proxy '" * replace (p, "'", "'\\''") * "'";
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

/******************************************************************************
* The HTTP requests with libcurl (the builds without Qt 6, out of a browser)
*
* A request is an easy handle of libcurl. A synchronous one is performed at
* once. The others are in a multi handle which http_async_pending, called by
* the main loop at each turn (tm_server.cpp), drives without waiting: what
* comes is appended to the output of the request as it comes (the answers
* of the AI engines, streamed, which a request link shows as they grow),
* and the request ends with its status, or its callback is called with the
* answer. A request whose kill is set (its session was interrupted) is
* aborted, and its server stops sending; http_async_cancel forgets the
* requests of a request link which stops or is destroyed. The headers and
* the data stay in the memory of TeXmacs: no command line, no file. Without
* libcurl, the curl program does the same below (to_shell_command).
******************************************************************************/

#ifdef USE_LIBCURL
#include <curl/curl.h>

struct lc_request {
  CURL*       easy;
  curl_slist* headers;
  string      out;          // the answer, when it is not written to outbuf
  object      callback;
  bool        has_callback;
  int*        status;       // a request link: its status, output, errors
  string*     outbuf;
  string*     errbuf;
  bool*       kill;
  char        error[CURL_ERROR_SIZE];
  lc_request ():
    easy (NULL), headers (NULL), has_callback (false),
    status (NULL), outbuf (NULL), errbuf (NULL), kill (NULL) {
      error[0]= '\0'; }
};

static CURLM* lc_multi= NULL;
static array<lc_request*> lc_busy;

static size_t
lc_write (char* ptr, size_t size, size_t nmemb, void* data) {
  lc_request* r= (lc_request*) data;
  size_t n= size * nmemb;
  if (r->kill != NULL && *(r->kill)) return 0; // aborts the transfer
  if (r->outbuf != NULL) *(r->outbuf) << string (ptr, (int) n);
  else r->out << string (ptr, (int) n);
  return n;
}

static int
lc_progress (void* data, curl_off_t dt, curl_off_t dn,
             curl_off_t ut, curl_off_t un) {
  (void) dt; (void) dn; (void) ut; (void) un;
  lc_request* r= (lc_request*) data;
  return (r->kill != NULL && *(r->kill))? 1: 0;
}

static lc_request*
lc_make (string url, array<string> headers_attr, string body,
         bool post= true) {
  static bool initialized= false;
  if (!initialized) {
    curl_global_init (CURL_GLOBAL_DEFAULT);
    initialized= true;
  }
  lc_request* r= tm_new<lc_request> ();
  r->easy= curl_easy_init ();
  if (r->easy == NULL) { tm_delete (r); return NULL; }
  c_string u (url);
  curl_easy_setopt (r->easy, CURLOPT_URL, (char*) u);
  if (post) {
    curl_easy_setopt (r->easy, CURLOPT_POST, 1L);
    curl_easy_setopt (r->easy, CURLOPT_POSTFIELDSIZE, (long) N(body));
    c_string b (body);
    curl_easy_setopt (r->easy, CURLOPT_COPYPOSTFIELDS, (char*) b);
  }
  else curl_easy_setopt (r->easy, CURLOPT_HTTPGET, 1L);
  for (int i= 0; i+1 < N(headers_attr); i += 2) {
    c_string h (headers_attr[i] * ": " * headers_attr[i+1]);
    r->headers= curl_slist_append (r->headers, (char*) h);
  }
  if (r->headers != NULL)
    curl_easy_setopt (r->easy, CURLOPT_HTTPHEADER, r->headers);
  curl_easy_setopt (r->easy, CURLOPT_WRITEFUNCTION, lc_write);
  curl_easy_setopt (r->easy, CURLOPT_WRITEDATA, (void*) r);
  curl_easy_setopt (r->easy, CURLOPT_NOPROGRESS, 0L);
  curl_easy_setopt (r->easy, CURLOPT_XFERINFOFUNCTION, lc_progress);
  curl_easy_setopt (r->easy, CURLOPT_XFERINFODATA, (void*) r);
  curl_easy_setopt (r->easy, CURLOPT_ERRORBUFFER, r->error);
  curl_easy_setopt (r->easy, CURLOPT_FOLLOWLOCATION, 1L);
  curl_easy_setopt (r->easy, CURLOPT_NOSIGNAL, 1L);
  // (a server which cannot be reached does not hold TeXmacs for long)
  curl_easy_setopt (r->easy, CURLOPT_CONNECTTIMEOUT, 15L);
  curl_easy_setopt (r->easy, CURLOPT_USERAGENT, "TeXmacs");
  curl_easy_setopt (r->easy, CURLOPT_PRIVATE, (void*) r);
  string proxy= http_proxy (url);
  if (proxy == "direct") curl_easy_setopt (r->easy, CURLOPT_PROXY, "");
  else if (proxy != "") {
    c_string cp (proxy);
    curl_easy_setopt (r->easy, CURLOPT_PROXY, (char*) cp);
  }
  if (DEBUG_IO)
    debug_io << "http_post (libcurl), " << url
             << (proxy == ""? string (""): ", proxy " * proxy) << LF;
  return r;
}

static void
lc_free (lc_request* r) {
  if (r->headers != NULL) curl_slist_free_all (r->headers);
  if (r->easy != NULL) curl_easy_cleanup (r->easy);
  tm_delete (r);
}

static string
lc_error (lc_request* r, CURLcode code) {
  string m= (r->error[0] != '\0')? string (r->error):
                                     string (curl_easy_strerror (code));
  return m;
}

static int
lc_perform (string& ret, string url, array<string> headers_attr,
            string body, bool post= true) {
  lc_request* r= lc_make (url, headers_attr, body, post);
  if (r == NULL) return 1;
  CURLcode code= curl_easy_perform (r->easy);
  ret= r->out;
  if (code != CURLE_OK)
    io_error << "http request, " << url << ": " << lc_error (r, code) << LF;
  lc_free (r);
  return (code == CURLE_OK)? 0: 1;
}

static bool
lc_start (lc_request* r) {
  if (lc_multi == NULL) lc_multi= curl_multi_init ();
  if (lc_multi == NULL) { lc_free (r); return true; }
  curl_multi_add_handle (lc_multi, r->easy);
  lc_busy << r;
  http_async_pending (); // the transfer begins at once
  return false;
}

static void
lc_forget (int i) {
  lc_request* r= lc_busy[i];
  curl_multi_remove_handle (lc_multi, r->easy);
  lc_free (r);
  lc_busy= append (range (lc_busy, 0, i), range (lc_busy, i+1, N(lc_busy)));
}

static bool
lc_async (string url, array<string> headers_attr, string body,
          object callback) {
  lc_request* r= lc_make (url, headers_attr, body);
  if (r == NULL) return true;
  r->callback= callback;
  r->has_callback= true;
  return lc_start (r);
}

static bool
lc_async (string url, array<string> headers_attr, string body,
          int& status, string& outbuf, string& errbuf, bool& kill) {
  lc_request* r= lc_make (url, headers_attr, body);
  if (r == NULL) return true;
  r->status= &status; r->outbuf= &outbuf; r->errbuf= &errbuf; r->kill= &kill;
  return lc_start (r);
}

// the body of a post of a form, as curl --data-urlencode makes it (a name
// ending with @ takes the contents of the file which its value names)
static string
lc_form (array<string> attr) {
  string r;
  CURL* e= curl_easy_init ();
  for (int i= 0; i+1 < N(attr); i += 2) {
    string name= attr[i], val= attr[i+1];
    if (ends (name, "@")) {
      name= name (0, N(name) - 1);
      string contents;
      if (!load_string (url_system (val), contents, false)) val= contents;
    }
    c_string v (val);
    char* esc= curl_easy_escape (e, (char*) v, N(val));
    if (N(r) > 0) r << "&";
    r << name << "=" << string (esc);
    curl_free (esc);
  }
  curl_easy_cleanup (e);
  return r;
}

void
http_async_pending () {
  if (lc_multi == NULL || N(lc_busy) == 0) return;
  int running= 0;
  curl_multi_perform (lc_multi, &running);
  CURLMsg* msg;
  int left= 0;
  while ((msg= curl_multi_info_read (lc_multi, &left)) != NULL) {
    if (msg->msg != CURLMSG_DONE) continue;
    lc_request* r= NULL;
    curl_easy_getinfo (msg->easy_handle, CURLINFO_PRIVATE, (char**) &r);
    int i;
    for (i= 0; i < N(lc_busy); i++)
      if (lc_busy[i] == r) break;
    if (r == NULL || i == N(lc_busy)) continue;
    CURLcode code= msg->data.result;
    bool killed= (r->kill != NULL && *(r->kill));
    if (r->has_callback) {
      if (code != CURLE_OK)
        io_error << "http_post: " << lc_error (r, code) << LF;
      object cb= r->callback;
      string out= (code == CURLE_OK)? r->out: string ("");
      lc_forget (i);
      call (cb, out);
    }
    else {
      if (code != CURLE_OK && !killed && r->errbuf != NULL)
        *(r->errbuf) << lc_error (r, code) << "\n";
      if (r->status != NULL) *(r->status)= 0;
      lc_forget (i);
    }
  }
}

void
http_async_cancel (string* outbuf) {
  for (int i= 0; i < N(lc_busy); )
    if (lc_busy[i]->outbuf == outbuf) lc_forget (i);
    else i++;
}

#else

void http_async_pending () {}
void http_async_cancel (string* outbuf) { (void) outbuf; }

#endif // USE_LIBCURL

static string
to_shell_command (string url, array<string> headers_attr, string data) {
  // the headers (and the keys among them) go into a file, not onto the
  // command line (curl_command)
  string args= "--silent --no-buffer" * curl_proxy_option (url) *
    " -X POST " * shell_quote (url) * " \\\n";
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
  string args= "--silent --no-buffer" * curl_proxy_option (url) *
    " -X POST " * shell_quote (url);
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

// a GET request (without libcurl, the curl program, whose headers are
// given as for the posts, out of its command line: curl_command)
int
http_get (string& ret, string url, array<string> headers_attr) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ()) {
    int st;
    int id= web_request (url, headers_attr, "", false, true);
    if (!web_answer (id, st, ret)) return 1;
    return (st >= 200 && st < 300)? 0: 1;
  }
#endif
#ifdef USE_LIBCURL
  return lc_perform (ret, url, headers_attr, "", false);
#endif
  string cmd= curl_command ("--silent" * curl_proxy_option (url) * " " *
                            shell_quote (url), headers_attr);
  return system (cmd, ret);
}

int
http_post (string& ret, string url,
	   array<string> headers_attr, string data) {
#ifdef __EMSCRIPTEN__
  if (web_http_available ()) return web_post (ret, url, headers_attr, data);
#endif
#ifdef USE_LIBCURL
  return lc_perform (ret, url, headers_attr, data);
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
#ifdef USE_LIBCURL
  return lc_perform (ret, url, headers_attr, tree_to_json (data));
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
#ifdef USE_LIBCURL
  return lc_perform (ret, url, headers_attr, lc_form (attr));
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
#ifdef USE_LIBCURL
  return lc_async (url, headers_attr, data, callback);
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
#ifdef USE_LIBCURL
  return lc_async (url, headers_attr, tree_to_json (data), callback);
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
#ifdef USE_LIBCURL
  return lc_async (url, headers_attr, lc_form (attr), callback);
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
#ifdef USE_LIBCURL
  return lc_async (url, headers_attr, tree_to_json (data),
                   status, outbuf, errbuf, kill);
#endif
  string cmd= to_shell_command (url, headers_attr, data);
  return async_eval_system (cmd, status, outbuf, errbuf, kill);  
}

#endif

#if defined(QTTEXMACS) && AC_QT_MAJOR_VERSION >= 6
// (Qt makes the requests: QNetworkAccessManager, qt_http.cpp)
void http_async_pending () {}
void http_async_cancel (string* outbuf) { (void) outbuf; }
#endif
