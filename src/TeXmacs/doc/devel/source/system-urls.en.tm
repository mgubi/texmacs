<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|URLs, resolution and concretization>

  This page describes the <c++> class <cpp|url> of
  <verbatim|System/Classes/url.hpp> and <verbatim|url.cpp>. The user level
  view and the <scheme> routines are documented in <hlink|the URL
  system|../scheme/api/url.en.tm>; the treatment of <verbatim|tmfs>
  <abbr|URL>s in <hlink|internals of the <TeXmacs> file
  system|../scheme/api/tmfs/tmfs-internals.en.tm>.

  <section|Representation>

  <\explain>
    <cpp|class url><explain-synopsis|a name, a path or a pattern>
  <|explain>
    A <cpp|url> is a reference counted handle (<cpp|CONCRETE>) on a
    <cpp|url_rep>, whose only field is a <cpp|tree t>. Two <abbr|URL>s are
    equal if their trees are equal; <cpp|u[i]> returns the <cpp|i>-th
    child of the tree as a <abbr|URL> and <cpp|as_tree> and <cpp|as_url>
    convert in both directions. The tree has one of the following forms:

    <\description>
      <item*|an atomic string>One component of a name, such as
      <verbatim|"doc"> or <verbatim|"a.tm">. The special atoms
      <verbatim|"."> (<cpp|url_here>), <verbatim|".."> (<cpp|url_parent>)
      and <verbatim|"..."> (<cpp|url_ancestor>) denote the current
      directory, the parent directory and any ancestor directory; the empty
      atom marks a trailing slash.

      <item*|<verbatim|(concat <em|u1> <em|u2>)>>The concatenation
      <verbatim|<em|u1>/<em|u2>>. Concatenations are kept right nested:
      <verbatim|a/b/c> is <verbatim|(concat a (concat b c))>.

      <item*|<verbatim|(or <em|u1> <em|u2>)>>A search path: the first of
      <cpp|u1> and <cpp|u2> which exists, or all of them, depending on the
      operation. Also right nested.

      <item*|<verbatim|(root <em|protocol>)>>The root of a name space:
      <verbatim|default> (the local file system), <verbatim|file>,
      <verbatim|http>, <verbatim|https>, <verbatim|ftp>, <verbatim|doi>,
      <verbatim|mailto>, <verbatim|tmfs>, <verbatim|blank> (a name starting
      with <verbatim|//>) and, on <name|Android>, <verbatim|content>.
      <verbatim|(root ramdisc <em|contents>)> is a <em|ramdisc>, a
      pseudo-file whose contents are stored in the <abbr|URL> itself.

      <item*|<verbatim|(wildcard)>>Any sequence of components
      (printed <verbatim|**>).

      <item*|<verbatim|(wildcard <em|pattern>)>>One component matching a
      pattern with <verbatim|*>.

      <item*|<verbatim|(none)>>The empty <abbr|URL> (<cpp|url_none>,
      printed <verbatim|{}>), which is also the result of failed
      operations. The default constructor <cpp|url ()> returns it.
    </description>

    So <verbatim|/usr/share> is <verbatim|(concat (root default) (concat
    usr share))> and <verbatim|https://www.texmacs.org/x> is
    <verbatim|(concat (root https) (concat www.texmacs.org x))>.
  </explain>

  The predicates of <verbatim|url.hpp> test these forms
  (<cpp|is_none>, <cpp|is_atomic>, <cpp|is_concat>, <cpp|is_or>,
  <cpp|is_root>, <cpp|is_wildcard>, ...) and the root of a name
  (<cpp|is_rooted>, <cpp|is_rooted_web>, <cpp|is_rooted_tmfs>,
  <cpp|is_rooted_blank>, <cpp|is_ramdisc>). <cpp|is_name> is true for
  rootless names without paths or wildcards and <cpp|is_rooted_name> for a
  root followed by such a name; the latter is the form of a <em|resolved>
  <abbr|URL>.

  <section|Parsing>

  <abbr|URL>s are parsed from strings by <cpp|url_general (name, type)>,
  where <cpp|type> is one of

  <\description>
    <item*|<cpp|URL_SYSTEM>>The conventions of the operating system:
    components are separated by <verbatim|/> (and also by
    <verbatim|\\> on <name|Windows>), and search paths by <verbatim|:>
    (<verbatim|;> on <name|Windows>). Used by <cpp|url_system>, for names
    which come from the system (command line, environment variables, file
    dialogs).

    <item*|<cpp|URL_UNIX>>Unix conventions on all systems. Used by
    <cpp|url_unix> and by the constructors <cpp|url (const char*)>,
    <cpp|url (string)> and <cpp|url (dir, name)>, that is, for all names
    written in the source code.

    <item*|<cpp|URL_STANDARD>>Web conventions (<cpp|url_standard>).

    <item*|<cpp|URL_CLEAN_UNIX>>Like <cpp|URL_UNIX>, but without the
    heuristics described below.
  </description>

  In the system and Unix formats, a component <verbatim|~> is replaced by
  the value of <verbatim|$HOME> and a component <verbatim|$<em|VAR>> by
  the value of the environment variable, which is itself parsed in the
  system format (so a variable holding a search path yields an
  <verbatim|or>). An unset or empty variable yields <cpp|url_none>, which
  makes the whole <abbr|URL> empty. Components containing <verbatim|*>
  become wildcards.

  <cpp|url_general> first recognizes explicit protocols
  (<verbatim|local:>, <verbatim|file://>, <verbatim|http://>,
  <verbatim|https://>, <verbatim|ftp://>, <verbatim|doi:>,
  <verbatim|mailto:>, <verbatim|tmfs://>, <verbatim|//> and, on
  <name|Android>, <verbatim|content://>). It then applies heuristics, in
  this order: a string containing the path separator is a search path; a
  string starting with <verbatim|/> (on <name|Windows>: a drive letter or
  <verbatim|\\\\>) is an absolute local name; on <name|Windows>,
  <verbatim|/c/...> in the Unix format is the drive <verbatim|c:>; except
  in the clean Unix format, names starting with <verbatim|www.> or
  <verbatim|ftp.> are web addresses. Anything else is a relative name.

  <section|Printing>

  <cpp|as_string (u, type)> prints a <abbr|URL> in one of the formats
  above (the default is the system format). Components of non-local roots
  are always printed with <verbatim|/>; non-trivial subexpressions are
  put between braces, so that <verbatim|a/{b:c}> denotes
  <verbatim|a/b:a/c>. The shortcuts <cpp|as_system_string>,
  <cpp|as_unix_string> and <cpp|as_standard_string> fix the format;
  <cpp|operator \<less\>\<less\>> on a <cpp|tm_ostream> uses the system
  format. On <name|Windows>, local names are printed with drive letters
  (<verbatim|c:\\...>) or as <abbr|UNC> names (<verbatim|\\\\host\\...>).

  <section|Operations>

  <\description>
    <item*|Concatenation>
    <cpp|u1 * u2> (also with a string or <cpp|const char*> on the right,
    parsed in the Unix format). If <cpp|u2> is rooted, the result is
    <cpp|u2>, except that a local absolute name or a <verbatim|//> name
    after a web <abbr|URL> stays on the same host and protocol. A trailing
    <verbatim|..> removes the last component; the parent of a root, of a
    web host or (on <name|Windows>) of a drive is itself. <verbatim|.> is
    neutral and <cpp|url_none> is absorbing.

    <item*|Search paths><cpp|u1 \| u2>, which removes immediate
    duplicates; an empty side (<cpp|url_none>) is simply dropped. <cpp|expand> distributes concatenations
    over <verbatim|or> and replaces <verbatim|...> by the list of all
    ancestors; <cpp|factor> is its inverse and also sorts; <cpp|sort>
    sorts the alternatives.

    <item*|Components><cpp|head> (the directory, <cpp|u * "..">),
    <cpp|tail> (the last component), <cpp|suffix> (lower case, without a
    trailing <verbatim|~> or <verbatim|#>, empty for names without a dot
    or starting with one), <cpp|basename>, <cpp|glue> and <cpp|unglue>
    (add or remove characters at the end of the last component),
    <cpp|unblank> (remove a trailing slash).

    <item*|Relative names><cpp|relative (base, u)> is <cpp|head (base) *
    u>; <cpp|delta (base, u)> computes a relative name such that
    <cpp|relative (base, delta (base, u)) == u> when both are on the same
    root and host, and returns <cpp|u> itself otherwise.

    <item*|Roots><cpp|get_root>, <cpp|unroot>, <cpp|reroot (u,
    protocol)>.

    <item*|Descent and security><cpp|descends (u, base)> tests whether
    <cpp|u> lies below one of the alternatives of <cpp|base>.
    <cpp|is_secure (u)> tests whether <cpp|u> lies below
    <verbatim|$TEXMACS_SECURE_PATH>, which the boot sets to its previous
    value followed by <verbatim|$TEXMACS_PATH> and
    <verbatim|$TEXMACS_HOME_PATH> (see <hlink|paths, directories and
    settings|system-boot.en.tm>). In documents from secure locations,
    the typesetter executes scripts (such as <markup|extern>) without
    first asking the <scheme> predicate <scm|secure?>
    (<verbatim|Typeset/Env/env_exec.cpp>); the environment computes this
    flag from the file name of the document (<cpp|edit_env_rep::secure>).
    <cpp|new_buffer_rep::secure> is initialized in the same way but not
    read anywhere.
  </description>

  <section|Resolution>

  An <abbr|URL> with search paths, wildcards or a relative name denotes a
  set of candidate resources. <em|Resolution> finds the existing ones:

  <\description>
    <item*|<cpp|complete (u, filter)>>All candidates which pass the
    <cpp|filter>, as an <verbatim|or> of rooted names. Relative names are
    interpreted with respect to <verbatim|$PWD> (or <verbatim|$HOME> if
    <verbatim|PWD> is not set).

    <item*|<cpp|resolve (u, filter)>>The first candidate only, or
    <cpp|url_none>. The default filter is <verbatim|"fr">: a readable
    regular file. Alternatives of an <verbatim|or> are tried in order, so
    the order of a search path is the order of precedence.

    <item*|<cpp|exists (u)>>The same as <cpp|resolve (u, "r")> being
    non-empty; <cpp|has_permission (u, filter)> is the general version.

    <item*|<cpp|resolve_in_path (u)>, <cpp|exists_in_path (u)>>Look for an
    executable program, with the shell command <verbatim|which> if it works
    on the system (<cpp|use_which>, determined at boot time), and
    otherwise in <verbatim|$PATH> (also in <verbatim|$TEXMACS_PATH/bin> on
    <name|Windows> and <name|Android>; on <name|Windows>,
    <cpp|exists_in_path> tries the suffixes <verbatim|.bat>,
    <verbatim|.exe> and <verbatim|.com>).

    <item*|<cpp|descendance (u)>, <cpp|subdirectories (u)>>All
    subdirectories of the directories of a path, used for the style and
    package menus and for the style search paths.
  </description>

  The filter is a string of letters which are all tested by
  <cpp|is_of_type> (<hlink|files and caches|system-files.en.tm>):
  <verbatim|f> regular file, <verbatim|d> directory, <verbatim|l> symbolic
  link, <verbatim|r> readable, <verbatim|w> writable, <verbatim|x>
  executable. The empty filter accepts everything, so that <cpp|resolve
  (u, "")> just builds the first candidate name; this is how
  <cpp|save_string> chooses where to write a file which does not exist
  yet. Local candidates are returned with the <verbatim|default> root even
  if they were written with <verbatim|file://>.

  Wildcards can only be expanded for local files (anything else fails
  with an error): <verbatim|**> matches any sequence of directories,
  including the empty one, and a pattern matches one directory entry.
  Directory listings come from <cpp|read_directory> and are therefore
  cached for the system directories.

  <section|Concretization>

  A resolved <abbr|URL> is turned into a name which the operating system
  understands by

  <\description>
    <item*|<cpp|concretize_url (u)>>Local names (roots
    <verbatim|default>, <verbatim|file>, <verbatim|blank>) are rerooted to
    <verbatim|default>; web <abbr|URL>s are downloaded with
    <cpp|get_from_web>, <verbatim|tmfs> <abbr|URL>s are fetched from their
    <scheme> handler with <cpp|get_from_server>, ramdiscs are written to a
    temporary file with <cpp|get_from_ramdisc>; <verbatim|.> and
    <verbatim|..> become <verbatim|$PWD> and its parent. Anything else
    yields <cpp|url_none>.

    <item*|<cpp|concretize (u, quiet)>>The same, printed as a system
    string; a warning is printed on failure unless <cpp|quiet> is set.

    <item*|<cpp|materialize (u, filter)>>Resolution followed by
    concretization.

    <item*|<cpp|sys_concretize (u)>>(<verbatim|file.hpp>) The concretized
    name quoted for the shell, used by the <cpp|system (cmd, u, ...)>
    helpers.
  </description>

  For remote resources the result is a temporary file in the session's
  temporary directory, so concretized names must be used at once and not
  stored. The three fetch functions live in
  <verbatim|System/Files/web_files.cpp>:

  <\description>
    <item*|<cpp|get_from_web (u)>><verbatim|doi:> names are first
    rewritten to <verbatim|https://www.doi.org/...>. With <name|Qt> 6 the
    file is downloaded with <cpp|qt_download_file>; otherwise with
    <verbatim|wget> or, if it is not installed, <verbatim|curl>. An empty
    or missing result means failure.

    <item*|<cpp|get_from_server (u)>>Calls <scm|tmfs-load> and saves the
    result in a temporary file; see the <verbatim|tmfs> internals.

    <item*|<cpp|get_from_ramdisc (u)>>Writes the contents stored in the
    <abbr|URL> to a temporary file.
  </description>

  Web and ramdisc results are kept in a small in-memory cache which maps
  the <abbr|URL> to its temporary file and remembers the 25 most recently
  used entries; <cpp|web_cache_invalidate (u)> forgets one entry, so that
  the next access downloads the file again. <verbatim|tmfs> results are not
  cached. The cache is not saved between sessions, but generated files
  which should survive a session, such as downloaded images, go through
  <cpp|make_file> (<hlink|files and caches|system-files.en.tm>).

  <section|Pitfalls>

  <\itemize>
    <item>Environment variables and <verbatim|~> are expanded when the
    <abbr|URL> is <em|constructed>, not when it is resolved. A static
    <cpp|url> initialized before the boot has set <verbatim|TEXMACS_PATH>
    and the other variables is empty or wrong. Conversely,
    <cpp|url ("$TEXMACS_PATH/...")> in a function picks up the current
    value each time it is called.

    <item>In the Unix format, used by all string constants and by
    <cpp|url (string)>, a colon separates search paths. A relative name
    containing a colon is therefore parsed as a path: use
    <cpp|url_system> or build the name component by component.

    <item>Testing a web <abbr|URL> (<cpp|exists>, <cpp|is_of_type>)
    downloads the whole file.

    <item>Without <name|Qt> 6, web files are downloaded with
    <verbatim|wget --no-check-certificate> when <verbatim|wget> is
    available, so the <abbr|TLS> certificate of the server is not checked
    (the <verbatim|curl> command lines, including those of the
    <abbr|HTTP> requests in <hlink|programs, web requests, messages and
    timing|system-utils.en.tm>, do not disable the check).

    <item><verbatim|file://<em|host>/<em|path>> is concretized as the local
    name <verbatim|/<em|host>/<em|path>>; host names in <verbatim|file>
    <abbr|URL>s are not supported.

    <item>The in-memory web cache does not check that its temporary file
    still exists. <cpp|make_file> <em|moves> the temporary file of a
    downloaded resource into <verbatim|system/make>, after which the cache
    entry points to a missing file: a later <cpp|load_string> or
    <cpp|concretize> of the same web <abbr|URL> in the same session fails
    instead of downloading it again.

    <item><cpp|concretize> of a single wildcard component returns the raw
    pattern instead of failing.
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
