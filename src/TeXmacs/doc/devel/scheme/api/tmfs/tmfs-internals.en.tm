<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Internals of the <TeXmacs> file system>

  <section|Overview>

  The <TeXmacs> file system (<verbatim|tmfs>) is a virtual file system whose
  documents do not live on disk, but are produced on demand by <scheme>
  code. Any <abbr|URL> starting with <verbatim|tmfs://> belongs to it. The
  implementation is split between two layers:

  <\itemize>
    <item>The <c++> file layer (<source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>,
    <source-link|System/Files/file.cpp|src/System/Files/file.cpp>, <source-link|System/Files/web_files.cpp|src/System/Files/web_files.cpp>)
    recognizes <verbatim|tmfs> <abbr|URL>s and, instead of accessing the disk,
    calls a small number of <scheme> entry points: <scm|tmfs-load>,
    <scm|tmfs-save>, <scm|tmfs-permission?>, <scm|tmfs-format>,
    <scm|tmfs-title> and <scm|tmfs-can-autosave?>.

    <item>The <scheme> module <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>
    implements these entry points by dispatching on the <em|class> of the
    <abbr|URL> (its first component) to <em|handlers> registered by other
    modules, with default behaviors for missing handlers. Some further
    operations (dates, removal, autosave, masters) are only used from
    <scheme>, mainly by <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>.
  </itemize>

  Since all file operations of <TeXmacs> go through the same <c++> routines
  (<cpp|load_string>, <cpp|save_string>, <cpp|is_of_type>, ...), a
  <verbatim|tmfs> document can be loaded into a buffer, saved, included,
  linked to or used as an image just like an ordinary file. The paths in
  this document are relative to <verbatim|src/src/> for <c++> files and to
  <verbatim|src/TeXmacs/progs/> for <scheme> files.

  <section|Syntax of <verbatim|tmfs> <abbr|URL>s>

  <subsection|Parsing on the <c++> side>

  <TeXmacs> represents <abbr|URL>s as trees (see <hlink|the <abbr|URL>
  system|../url.en.tm>). When a string is converted into a <abbr|URL>, the
  generic constructor <cpp|url_general> in <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>
  checks for known protocol prefixes. For <verbatim|tmfs://> it calls

  <\cpp-code>
    static url

    url_tmfs (string name) {

    \ \ url u= url_get_name (name);

    \ \ return url_root ("tmfs") * u;

    }
  </cpp-code>

  so that <verbatim|tmfs://help/normal/tm/doc/main/man-manual.en.tm> becomes
  the concatenation of the root <verbatim|tmfs> with the names
  <verbatim|help>, <verbatim|normal>, <verbatim|tm>, ... Nothing else is
  interpreted: query strings such as <verbatim|type=module&what=doc.apidoc>
  are simply parts of names. The following predicates are declared in
  <source-link|System/Classes/url.hpp|src/System/Classes/url.hpp>:

  <\description>
    <item*|<cpp|is_root_tmfs (url u)>>Whether <cpp|u> is the root
    <verbatim|tmfs> itself.

    <item*|<cpp|is_rooted_tmfs (url u)>>Whether <cpp|u> is a <verbatim|tmfs>
    <abbr|URL> (or an alternative of such <abbr|URL>s). Exported to
    <scheme> as <scm|url-rooted-tmfs?>.

    <item*|<cpp|is_rooted_tmfs (url u, string sub_protocol)>>Whether
    <cpp|u> is a <verbatim|tmfs> <abbr|URL> whose class is
    <cpp|sub_protocol>. Exported to <scheme> as
    <scm|url-rooted-tmfs-protocol?>. For instance,
    <source-link|Texmacs/Window/tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp> uses
    <cpp|is_rooted_tmfs (buf, "part")> to recognize partial documents.
  </description>

  <verbatim|tmfs> <abbr|URL>s count as rooted <abbr|URL>s: relative
  names can be completed against them (<cpp|complete> in
  <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp> accepts them in the same way as web
  <abbr|URL>s), which is how relative hyperlinks and images inside a
  generated document can work. When the directory of a <verbatim|tmfs> document
  is not meaningful, a <em|master> should be provided (see below).

  <subsection|Decomposition on the <scheme> side>

  On the <scheme> side, an <abbr|URL> (or its string form) is split by
  <scm|tmfs-decompose-name> into a class and a name:

  <\scm-code>
    (define-public (tmfs-decompose-name name)

    \ \ (if (url? name) (set! name (url-\<gtr\>unix name)))

    \ \ (if (string-starts? name "tmfs://") (set! name (string-drop name 7)))

    \ \ (with i (string-index name #\\/)

    \ \ \ \ (list (if i (substring name 0 i) "file")

    \ \ \ \ \ \ \ \ \ \ (if i (substring name (+ i 1) (string-length name)) name))))
  </scm-code>

  The <em|class> is the first component; the <em|name> is the rest of the
  string, which is passed verbatim to the handlers. If there is no slash,
  the class defaults to <verbatim|"file">. Classes are strings in the
  handler table, although <scm|lazy-tmfs-handler> takes symbols.

  How the name is structured is up to each handler. Two conventions are
  supported by helper functions in <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>:

  <\description>
    <item*|Queries>A name of the form <verbatim|var1=val1&var2=val2> is
    converted into an association list by <scm|query-\<gtr\>list> and back by
    <scm|list-\<gtr\>query>; <scm|query-ref> looks up a single variable. The
    only escaping performed is the replacement of colons by
    <verbatim|%3A>. Used by <verbatim|apidoc> and <verbatim|grep>.

    <item*|Paths>A name made of components separated by slashes is handled
    with <scm|tmfs-pair?>, <scm|tmfs-car>, <scm|tmfs-cdr>,
    <scm|tmfs-\<gtr\>list> and <scm|list-\<gtr\>tmfs>, which work on strings
    in the same way as <scm|car> and <scm|cdr> on lists. Most handlers use
    this convention, typically with a few leading parameters followed by an
    embedded file name.
  </description>

  <subsection|Embedding ordinary file names>

  Many handlers work on an existing file: the history or a revision of a
  file, a part of a document, the comments of a buffer, etc. The file is
  then embedded at the end of the name by <scm|url-\<gtr\>tmfs-string> and
  recovered with <scm|tmfs-string-\<gtr\>url>. The encoding prefixes the
  path with a pseudo protocol:

  <\description>
    <item*|<verbatim|tm/>>A file under <verbatim|$TEXMACS_PATH>, given
    relatively to it. For instance
    <verbatim|tm/doc/main/man-manual.en.tm>.

    <item*|<verbatim|file/>>An absolute file name without its leading slash,
    as in <verbatim|file/home/joe/paper.tm>. On <name|Windows> the colon after
    the drive letter is removed (<scm|strip-colon>), as in
    <verbatim|file/c/Users/joe/paper.tm>.

    <item*|<verbatim|here/>>A relative file name.

    <item*|<verbatim|http/>, <verbatim|https/>, <verbatim|ftp/>,
    <verbatim|tmfs/>>A web <abbr|URL> or another <verbatim|tmfs> <abbr|URL>,
    without the <verbatim|://>. For instance the <verbatim|tmfs> string of
    <verbatim|tmfs://help/normal/x.tm> is
    <verbatim|tmfs/help/normal/x.tm>.
  </description>

  Strings which do not start with one of these prefixes are interpreted by
  <scm|tmfs-string-\<gtr\>url> as paths relative to the root directory. A
  typical constructor thus reads

  <\scm-code>
    (tm-define (tmfs-url-commit root rev)

    \ \ (string-append "tmfs://commit/" rev "/" (url-\<gtr\>tmfs-string root)))
  </scm-code>

  and the corresponding load handler decomposes its name with
  <scm|(tmfs-car name)> (the revision) and
  <scm|(tmfs-string-\<gtr\>url (tmfs-cdr name))> (the file).

  <section|The <c++> file layer>

  <subsection|Testing and resolving>

  File tests are implemented by <cpp|is_of_type (url name, string filter)>
  in <source-link|System/Files/file.cpp|src/System/Files/file.cpp>, where <cpp|filter> is a string of
  letters. For <verbatim|tmfs> <abbr|URL>s:

  <\itemize>
    <item><verbatim|d> (directory), <verbatim|l> (link) and <verbatim|x>
    (executable) always fail.

    <item><verbatim|r> calls <scm|(tmfs-permission? name "read")> and
    <verbatim|w> calls <scm|(tmfs-permission? name "write")>.

    <item>All other letters, in particular <verbatim|f> (regular file) and
    <verbatim|c> (can be created), succeed.
  </itemize>

  Since <cpp|exists> is implemented as a resolution with the filter
  <verbatim|"r">, <scm|url-exists?> on a <verbatim|tmfs> <abbr|URL> is the
  same as asking for read permission; the load handler is not called.
  <cpp|file_size> returns <cpp|-1> and <cpp|last_modified> returns the
  smallest possible date for <verbatim|tmfs> <abbr|URL>s; on the <scheme>
  side, <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> redefines
  <scm|url-last-modified> and <scm|url-newer?> so that they use
  <scm|tmfs-date> instead. <cpp|file_format> calls <scm|tmfs-format>
  instead of guessing the format from the suffix.

  <subsection|Loading>

  Reading a file always goes through resolution and <em|concretization>,
  which maps a resolved <abbr|URL> to a local file name. For
  <verbatim|tmfs> <abbr|URL>s, <cpp|concretize_url> in
  <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp> calls <cpp|get_from_server> in
  <source-link|System/Files/web_files.cpp|src/System/Files/web_files.cpp>:

  <\cpp-code>
    url

    get_from_server (url u) {

    \ \ if (!is_rooted_tmfs (u)) return url_none ();

    \ \ url res= get_cache (u);

    \ \ if (!is_none (res)) return res;

    \;

    \ \ string name= as_string (u);

    \ \ if (ends (name, "~") \|\| ends (name, "#")) {

    \ \ \ \ if (!is_rooted_tmfs (name)) return url_none ();

    \ \ \ \ if (!as_bool (call ("tmfs-can-autosave?", unglue (u, 1))))

    \ \ \ \ \ \ return url_none ();

    \ \ }

    \ \ string r= as_string (call ("tmfs-load", object (name)));

    \ \ if (r == "") return url_none ();

    \ \ url tmp= url_temp (string (".") * suffix (name));

    \ \ (void) save_string (tmp, r, true);

    \ \ return tmp;

    }
  </cpp-code>

  In other words, the string returned by <scm|tmfs-load> is written to a
  fresh temporary file with the same suffix, and <cpp|load_string> then
  reads this file as usual. Several consequences follow:

  <\itemize>
    <item>An empty result means failure: the <abbr|URL> cannot be
    concretized and loading fails.

    <item>The result is not cached (the call to <cpp|set_cache> is commented
    out, because handlers which fill their buffers in a delayed fashion
    would be cached as empty files). The load handler is therefore called
    each time the document is read.

    <item>Any code which needs a local copy of a file (images, inclusions,
    <cpp|make_file> in <source-link|System/Files/make_file.cpp|src/System/Files/make_file.cpp> with the
    command <cpp|CMD_GET_FROM_SERVER>) works for <verbatim|tmfs> too. As a
    special case, <cpp|make_file> first looks for
    <verbatim|tmfs://artwork/...> files in
    <verbatim|$TEXMACS_HOME_PATH/misc>.
  </itemize>

  <subsection|Saving>

  <cpp|save_string> checks <cpp|is_rooted_tmfs> first and, for
  <verbatim|tmfs> <abbr|URL>s, calls <cpp|save_to_server>, which passes the
  <abbr|URL> and the string to <scm|tmfs-save>. <cpp|save_to_server> always
  reports success, so errors must be signalled by the save handler itself
  (for instance with <scm|set-message>). <cpp|append_string> is not
  supported and fails.

  <subsection|Summary of the calls from <c++> to <scheme>>

  <\description>
    <item*|<scm|(tmfs-load name)>>From <cpp|get_from_server>; returns the
    serialized document.

    <item*|<scm|(tmfs-save name s)>>From <cpp|save_to_server>.

    <item*|<scm|(tmfs-permission? name kind)>>From <cpp|is_of_type>, with
    <scm-arg|kind> equal to <verbatim|"read"> or <verbatim|"write">.

    <item*|<scm|(tmfs-format u)>>From <cpp|file_format>.

    <item*|<scm|(tmfs-can-autosave? u)>>From <cpp|get_from_server>, for
    autosave names ending with <verbatim|~> or <verbatim|#>.

    <item*|<scm|(tmfs-title name doc)>>From <cpp|propose_title> in
    <source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>.
  </description>

  <section|Loading and saving buffers>

  <subsection|Loading>

  The complete path followed when the user opens
  <verbatim|tmfs://help/normal/tm/doc/main/man-manual.en.tm> is as follows.

  <\enumerate>
    <item><scm|load-buffer> (<source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>)
    checks permissions with <scm|url-test?>: the tests <verbatim|"f"> and
    <verbatim|"r"> end up in <scm|tmfs-permission?>.

    <item><scm|buffer-load> calls <cpp|buffer_load> in
    <source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>, which determines the format with
    <cpp|file_format> (hence <scm|tmfs-format>, by default
    <verbatim|"stm">) and calls <cpp|import_tree>.

    <item><cpp|import_tree> resolves the <abbr|URL> with the filter
    <verbatim|"fr"> and calls <cpp|load_string>, which concretizes it via
    <cpp|get_from_server> and therefore <scm|tmfs-load>.

    <item><scm|tmfs-load> finds the load handler of the class and calls it
    on the name. If the handler returns a <scheme> tree rather than a string,
    it is serialized with <scm|object-\<gtr\>tmstring>, which yields the
    <verbatim|stm> format (<scheme> representation of <TeXmacs> trees).

    <item><cpp|import_loaded_tree> parses the string according to the format
    and the buffer is filled with <cpp|set_buffer_tree>. The buffer title is
    computed by <cpp|propose_title>, which calls <scm|tmfs-title>.

    <item>Finally, <scm|load-buffer-open> in <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> asks
    for <scm|(tmfs-master name)> and, if it differs from the name, makes it
    the master of the buffer with <scm|buffer-set-master>.
  </enumerate>

  Handlers which (re)generate documents on request are usually opened with
  <scm|load-document> or <scm|revert-buffer-revert>. The latter reloads the
  buffer even if it already exists, which is the natural way to refresh
  generated content such as a version history or a search result.

  <subsection|Saving>

  <scm|save-buffer> first checks with <scm|url-test?> that the <abbr|URL>
  is writable, which invokes the permission handler with
  <verbatim|"write">; without write permission it displays <verbatim|You do
  not have write access for ...>. Then <cpp|buffer_save> exports the buffer
  in the format returned by <cpp|file_format> and calls <cpp|save_string>.
  On the <scheme> side, <scm|tmfs-save> converts the string back into a
  <scheme> tree before calling the handler:

  <\scm-code>
    (define-public (tmfs-save u what)

    \ \ (with (class name) (tmfs-decompose-name u)

    \ \ \ \ (lazy-tmfs-force class)

    \ \ \ \ (cond ((ahash-ref tmfs-handler-table (cons class 'save)) =\<gtr\>

    \ \ \ \ \ \ \ \ \ \ \ (lambda (handler)

    \ \ \ \ \ \ \ \ \ \ \ \ \ (handler name (convert what "stm-document" "texmacs-stree"))))

    \ \ \ \ \ \ \ \ \ \ (else ((ahash-ref tmfs-handler-table (cons #t 'save)) u what)))))
  </scm-code>

  After a successful <cpp|save_string>, the buffer is marked as saved.

  <subsection|Asynchronous contents>

  A load handler must return immediately. Handlers whose contents come
  from a remote server, such as those of the remote file system in
  <source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm>, therefore send a request, return a
  placeholder document, and replace the contents of the buffer when the
  answer arrives:

  <\scm-code>
    (tm-define (buffer-set-stm u doc)

    \ \ (with t (tree-import-loaded-from-object doc u)

    \ \ \ \ (buffer-set u t)

    \ \ \ \ (buffer-pretend-saved u)))
  </scm-code>

  The same technique may be used for any computation which takes time.

  <section|Dispatching to handlers>

  <subsection|The handler table>

  Handlers are stored in the hash table <scm|tmfs-handler-table>, indexed
  by pairs <scm|(class . action)>, where <scm-arg|class> is a string (or
  <scm|#t> for the default handlers) and <scm-arg|action> one of the
  symbols <scm|load>, <scm|save>, <scm|autosave>, <scm|remove>,
  <scm|wrap>, <scm|date>, <scm|title>, <scm|permission?>, <scm|master> and
  <scm|format>. The function <scm|(tmfs-handler class action handle)> adds
  an entry; the macros <scm|tmfs-load-handler> and friends are shorthands
  which convert the class symbol into a string and build the
  <scm|lambda>:

  <\scm-code>
    (define-public-macro (tmfs-load-handler head . body)

    \ \ (with (type what) head

    \ \ \ \ `(tmfs-handler ,(symbol-\<gtr\>string type) 'load

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (lambda (,what) ,@body))))
  </scm-code>

  Each public operation first calls <scm|lazy-tmfs-force> on the class,
  which loads the module registered with <scm|lazy-tmfs-handler> the first
  time the class is used, and then looks up the handler.

  <subsection|Operations and their fallbacks>

  <\description>
    <item*|<scm|tmfs-load>>Calls the <scm|load> handler of the class, or the
    default one, which returns a document saying <verbatim|Invalid tmfs
    document.> A string result is returned as is; any other result is
    serialized with <scm|object-\<gtr\>tmstring>.

    <item*|<scm|tmfs-save>>Calls the <scm|save> handler with the name and the
    document as a <scheme> tree. The default handler does nothing.

    <item*|<scm|tmfs-title>>Calls the <scm|title> handler with the name and
    the document. Without a title handler, the title is the complete
    <abbr|URL>.

    <item*|<scm|tmfs-permission?>>Names ending with <verbatim|~> or
    <verbatim|#> (autosave files) are only accessible if the autosave
    handler accepts them. Otherwise the <scm|permission?> handler of the
    class decides. Without such a handler: if the <abbr|URL> wraps a file
    (see below), the permissions of that file are used; if the class has a
    load handler, only <verbatim|"read"> is granted; otherwise the default
    handler grants <verbatim|"read"> only.

    <item*|<scm|tmfs-master>>Calls the <scm|master> handler, or returns the
    <abbr|URL> itself.

    <item*|<scm|tmfs-format>>Calls the <scm|format> handler, or returns
    <verbatim|"stm">.

    <item*|<scm|tmfs-wrap>>Calls the <scm|wrap> handler, or returns
    <scm|#f>.

    <item*|<scm|tmfs-date>, <scm|tmfs-remove>>Call the handler of the class;
    the defaults apply <scm|url-last-modified> and <scm|url-remove> to the
    wrapped file, if any.

    <item*|<scm|tmfs-autosave>>Calls the <scm|autosave> handler with the name
    and a suffix (<verbatim|"~"> or <verbatim|"#">), which should return the
    <abbr|URL> where the autosave copy is stored, or <scm|#f> if autosaving
    is not supported. The default handler supports autosaving only for
    wrapped files.

    <item*|<scm|tmfs-remote?>>Returns <scm|#t> if the class has no load
    handler.
  </description>

  Notice that, except for <scm|load>, the default handlers (those
  registered for the class <scm|#t>) are called with the complete
  <abbr|URL> rather than the name. The default <scm|master> and
  <scm|format> handlers are never used: <scm|tmfs-master> and
  <scm|tmfs-format> directly return the <abbr|URL> and <verbatim|"stm">.

  <subsection|Wrapped files>

  A <em|wrap> handler declares that a <verbatim|tmfs> document is merely a
  different view on an ordinary file. For instance, the <verbatim|part>
  handler returns the file the part is taken from. The wrapped file is used
  by the default handlers for permissions, dates, removal and autosave, and
  by <scm|buffer-last-save> in <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>, which asks the
  buffer of the wrapped file, if it is open, for its last save time.

  <subsection|Autosave>

  <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> redefines <scm|url-autosave> so that, for
  <verbatim|tmfs> <abbr|URL>s, the autosave location is obtained from
  <scm|tmfs-autosave>. The remote file system, for example, stores the
  autosave copies of remote files in the local backup directory:

  <\scm-code>
    (tmfs-autosave-handler (remote-file name suf)

    \ \ (url-backup (url-glue name suf)))
  </scm-code>

  When recovering, <cpp|get_from_server> refuses to load
  <abbr|URL>s ending with <verbatim|~> or <verbatim|#> unless
  <scm|tmfs-can-autosave?> holds for the original <abbr|URL>.

  <section|Buffers, views and windows>

  <subsection|Names of buffers>

  Buffers are identified by <abbr|URL>s: an ordinary file buffer is named by
  its file <abbr|URL>, a generated buffer by its <verbatim|tmfs>
  <abbr|URL>. New documents which have not been saved yet get a
  <verbatim|no_name_...tm> name in <verbatim|$TEXMACS_HOME_PATH/texts/scratch>
  (see <cpp|is_scratch> in <source-link|System/Files/file.cpp|src/System/Files/file.cpp>). There is no
  special <verbatim|tmfs://buffer/...> scheme. The buffer layer itself is
  described in <hlink|the <TeXmacs> server|../../../source/server.en.tm> and
  in <hlink|the buffer <abbr|API>|../../buffer/buffer-api.en.tm>.

  The title of a buffer is computed by <cpp|propose_title> in
  <source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>. For <verbatim|tmfs> buffers it is
  the result of <scm|tmfs-title>; if several buffers would get the same
  title, a number is appended, as in <verbatim|Help - Title (2)>.

  <subsection|Views and windows>

  Views and windows are also identified by <verbatim|tmfs> <abbr|URL>s, but
  these <abbr|URL>s are mere identifiers: there are no handlers for them and
  they cannot be loaded.

  <\itemize>
    <item>A view is named
    <verbatim|tmfs://view/<em|nr>/<em|buffer>> by <cpp|abstract_view> in
    <source-link|Texmacs/Data/new_view.cpp|src/Texmacs/Data/new_view.cpp>, where <em|nr> is a counter
    specific to the buffer and <em|buffer> encodes the name of the buffer:
    <verbatim|here/> followed by the path for relative names,
    <verbatim|default> followed by the absolute path for local files, and
    otherwise the protocol, a slash and the rest of the <abbr|URL>. For
    instance the first view of <verbatim|/home/joe/paper.tm> is
    <verbatim|tmfs://view/1/default/home/joe/paper.tm>. <cpp|concrete_view>
    decodes such names.

    <item>A window is named <verbatim|tmfs://window/<em|nr>> by
    <cpp|create_window_id> in <source-link|Texmacs/Data/new_window.cpp|src/Texmacs/Data/new_window.cpp>, with a
    global counter starting at <verbatim|1>.
  </itemize>

  From <scheme>, these identifiers are manipulated with the functions of
  the <hlink|view <abbr|API>|../../buffer/view-api.en.tm> and the
  <hlink|window <abbr|API>|../../buffer/window-api.en.tm>. This is why the
  classes <verbatim|view> and <verbatim|window> must not be used for
  handlers.

  <subsection|Masters>

  Every buffer has a <em|master> <abbr|URL>, returned by
  <scm|buffer-get-master> (or <scm|buffer-master> for the current buffer),
  which is used instead of the buffer name to resolve relative links and to
  navigate. Normally the master is the buffer itself; <scm|load-buffer-open>
  replaces it by the result of <scm|tmfs-master> for <verbatim|tmfs>
  buffers. Some generators set the master explicitly instead; for example
  <scm|version-show-history> in <source-link|version/version-tmfs.scm|TeXmacs/progs/version/version-tmfs.scm> makes the
  history buffer point to the file whose history is shown. The <c++>
  predicate <cpp|is_aux_buffer> (<source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>)
  considers a buffer as auxiliary as soon as its master differs from its
  name.

  <section|Auxiliary buffers>

  Many dialogs and side tools of <TeXmacs> edit small documents in buffers
  of the class <verbatim|aux>, defined in
  <source-link|kernel/texmacs/tm-file-system.scm|TeXmacs/progs/kernel/texmacs/tm-file-system.scm>. Their contents are not
  generated, but stored in the <scheme> table <scm|aux-buffers>, and their
  masters in <scm|aux-masters>:

  <\scm-code>
    (tmfs-load-handler (aux name)

    \ \ (or (ahash-ref aux-buffers name)

    \ \ \ \ \ \ `(document

    \ \ \ \ \ \ \ \ \ (TeXmacs ,(texmacs-version))

    \ \ \ \ \ \ \ \ \ (style (tuple "generic"))

    \ \ \ \ \ \ \ \ \ (body (document "")))))

    \;

    (tmfs-title-handler (aux name doc)

    \ \ name)

    \;

    (tmfs-master-handler (aux name)

    \ \ (or (ahash-ref aux-masters name)

    \ \ \ \ \ \ (unix-\<gtr\>url (string-append "tmfs://aux/" name))))
  </scm-code>

  <scm|(aux-name <scm-arg|aux>)> returns the <abbr|URL>
  <verbatim|tmfs://aux/> followed by the string <scm-arg|aux>,
  <scm|aux-set-document> and <scm|aux-set-master> set the contents and the
  master, and <scm|open-auxiliary> in <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> combines them
  and switches to the auxiliary buffer; the macro <scm|with-aux> evaluates
  some code with an auxiliary buffer holding a given file. The title of an
  auxiliary buffer is its name.

  Examples of auxiliary buffers are <verbatim|tmfs://aux/search> and
  <verbatim|tmfs://aux/replace> (<source-link|generic/search-widgets.scm|TeXmacs/progs/generic/search-widgets.scm>),
  <verbatim|tmfs://aux/spell> (<source-link|generic/spell-widgets.scm|TeXmacs/progs/generic/spell-widgets.scm>),
  <verbatim|tmfs://aux/latex-source> (<source-link|convert/latex/tmtex-widgets.scm|TeXmacs/progs/convert/latex/tmtex-widgets.scm>)
  and <verbatim|tmfs://aux/macro-editor> (<source-link|source/macro-widgets.scm|TeXmacs/progs/source/macro-widgets.scm>).
  On the <c++> side, <cpp|embedded_name> in
  <source-link|Texmacs/Window/tm_window.cpp|src/Texmacs/Window/tm_window.cpp> names the buffers of embedded
  editor widgets <verbatim|tmfs://aux/TeXmacs-input-<em|nr>>, and
  <cpp|edit_interface_rep::is_embedded_widget> recognizes such widgets by
  the prefix <verbatim|tmfs://aux/>. When the current buffer is auxiliary
  (but not a <verbatim|part> buffer), interactive commands ask their
  questions in a popup rather than in the footer
  (<source-link|Texmacs/Window/tm_dialogue.cpp|src/Texmacs/Window/tm_dialogue.cpp>).

  <section|Importation of foreign formats>

  When a file is imported from a format different from its natural one,
  <source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> opens it as
  <verbatim|tmfs://import/<em|format>/<em|file>>, where <em|file> is the
  output of <scm|url-\<gtr\>tmfs-string>. The <verbatim|import> handler
  converts the file with <scm|tree-import> and completes the result with
  <scm|tmfs-document>; its title is the file name followed by the format.

  <section|Remote file systems>

  The client/server infrastructure of <TeXmacs> exposes files stored on a
  <TeXmacs> server as <verbatim|tmfs://remote-file/...> and
  <verbatim|tmfs://remote-dir/...> <abbr|URL>s (see the
  <hlink|catalogue|tmfs-handlers.en.tm>). The client handlers in
  <source-link|client/client-tmfs.scm|TeXmacs/progs/client/client-tmfs.scm> forward loading and saving to
  <scheme> services such as <scm|remote-file-load> and
  <scm|remote-file-save>, implemented on the server by
  <source-link|server/server-tmfs.scm|TeXmacs/progs/server/server-tmfs.scm>, which stores the files in a repository
  under <verbatim|$TEXMACS_HOME_PATH/server> and keeps their metadata in a
  database. The server side itself does not define any <verbatim|tmfs>
  handler. These mechanisms are described in <hlink|collaborative
  editing|../../../source/collaboration.en.tm>.

  <section|Pitfalls>

  <\itemize>
    <item>When calling <scm|tmfs-handler> directly, the class must be a
    string: <scm|(tmfs-handler "foo" 'load ...)>. A symbol would never be
    found.

    <item>Handlers receive the name <em|without> the class and without
    <verbatim|tmfs://>; <scm|tmfs-car> and <scm|tmfs-cdr> return <scm|#f>
    when there is no slash in their argument.

    <item>A load handler returning the empty string makes loading fail.
    Return an empty document instead.

    <item>The load handler is called each time the document is read, not
    only when a buffer is created; make it fast, or cache results yourself.

    <item>Without a permission handler, the document is read-only; the
    save handler is then never called.

    <item>Errors during saving are not propagated to the <c++> layer.

    <item><scm|lazy-tmfs-handler> only registers the listed classes. The
    handlers <verbatim|remote-dir>, <verbatim|history> or <verbatim|git>,
    for example, are not registered lazily and are only available once
    their modules have been loaded for another reason.
  </itemize>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
