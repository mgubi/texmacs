<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|How the plug-in system works internally>

  <section|Introduction>

  This chapter describes the implementation of the plug-in system, for
  developers who want to write non-trivial plug-ins or who need to debug or
  modify the plug-in machinery itself. It complements the user-level
  description in the previous sections and the chapter about
  <hlink|interfacing <TeXmacs> with other
  programs|../interface/interface.en.tm>.

  The implementation is spread over the following files (<scheme> files are
  given relative to <verbatim|src/TeXmacs/progs>, <c++> files relative to
  <verbatim|src/src>):

  <\description>
    <item*|<source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>>Discovery of the plug-in
    directories (<cpp|plugin_list>) and extension of the search paths with
    the subdirectories of all plug-ins (<cpp|plugin_path>).

    <item*|<source-link|kernel/texmacs/tm-plugins.scm|TeXmacs/progs/kernel/texmacs/tm-plugins.scm>>The
    <scm|plugin-configure> macro, the plug-in cache, lazy initialization,
    the tables of connections, sessions and scripting languages, and remote
    plug-ins.

    <item*|<source-link|utils/plugins/plugin-eval.scm|TeXmacs/progs/utils/plugins/plugin-eval.scm>>Queues of pending
    evaluations, the notification call-backs from the <c++> side, silent
    (background) evaluations and <scm|plugin-eval>.

    <item*|<source-link|utils/plugins/plugin-cmd.scm|TeXmacs/progs/utils/plugins/plugin-cmd.scm>>Serialization of the
    input, formatting of special commands, and the tables for
    tab-completion, input completeness tests and numeric evaluation.

    <item*|<source-link|utils/plugins/plugin-convert.scm|TeXmacs/progs/utils/plugins/plugin-convert.scm>>Conversion of
    mathematical input into strings (<scm|plugin-input-converters>).

    <item*|<source-link|dynamic/session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm>,
    <source-link|dynamic/session-menu.scm|TeXmacs/progs/dynamic/session-menu.scm>>Shell sessions.

    <item*|<source-link|dynamic/scripts-edit.scm|TeXmacs/progs/dynamic/scripts-edit.scm>>Evaluation of scripts inside
    documents.

    <item*|<verbatim|System/Link/>>The <c++> side of connections:
    <source-link|connection.cpp|src/System/Link/connection.cpp> (the <cpp|connection> resource),
    <source-link|tm_link.hpp|src/System/Link/tm_link.hpp> (the abstract <cpp|tm_link_rep> class and the
    control characters), <source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp>,
    <source-link|dyn_link.cpp|src/System/Link/dyn_link.cpp>, <source-link|cmdline_link.cpp|src/System/Link/cmdline_link.cpp> and
    <source-link|request_link.cpp|src/System/Link/request_link.cpp>. Under <name|Windows> (and for <name|Qt>
    builds with <cpp|QTPIPES>), pipes are implemented by
    <source-link|Plugins/Qt/qt_pipe_link.cpp|src/Plugins/Qt/qt_pipe_link.cpp> instead of
    <source-link|pipe_link.cpp|src/System/Link/pipe_link.cpp>.

    <item*|<source-link|Data/Convert/Generic/input.cpp|src/Data/Convert/Generic/input.cpp>>The parser for the
    output of plug-ins (<cpp|texmacs_input_rep>).
  </description>

  <section|Discovery of plug-ins>

  <subsection|The list of plug-ins>

  The <scheme> function <scm|(plugin-list)> is implemented by the <c++>
  function <cpp|plugin_list> in <source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>. It
  returns the sorted list of the names of all subdirectories of

  <\verbatim-code>
    $TEXMACS_PATH/plugins

    /etc/TeXmacs/plugins

    $TEXMACS_HOME_PATH/plugins

    /usr/share/TeXmacs/plugins
  </verbatim-code>

  Duplicates are removed, as well as entries ending with <verbatim|.txt> or
  <verbatim|.md> (such as the <verbatim|README.md> file of the plug-in
  collection). If a plug-in named <verbatim|jupyter> is present, it is moved
  to the front of the list, so that it is initialized before all other
  plug-ins: the <verbatim|jupyter> plug-in overloads <scm|alt-launcher> in
  order to redirect the launchers of other plug-ins.

  In the source tree, the plug-ins which are shipped with <TeXmacs> live in
  <verbatim|src/plugins>; the build system copies them into
  <verbatim|TeXmacs/plugins> (which is not under version control), and the
  installation procedure installs them into <verbatim|$TEXMACS_PATH/plugins>.
  The small example plug-ins of the chapter about interfaces live in
  <verbatim|$TEXMACS_PATH/examples/plugins> and are <em|not> in the plug-in
  search path: they have to be copied into one of the above directories.

  <subsection|Search paths>

  At startup, <cpp|init_env_vars> extends a number of environment variables
  with the corresponding subdirectories of <em|all> plug-ins, using the
  helper <cpp|plugin_path>:

  <\cpp-code>
    static url

    plugin_path (string which) {

    \ \ url base= "$TEXMACS_HOME_PATH:/etc/TeXmacs:$TEXMACS_PATH:/usr/share/TeXmacs";

    \ \ url search= base * "plugins" * url_wildcard ("*") * which;

    \ \ return expand (complete (search, "r"));

    }
  </cpp-code>

  The following table summarizes which subdirectories of a plug-in are
  taken into account:

  <descriptive-table|<tformat|<cwith|1|1|1|-1|cell-font-series|bold>|<table|<row|<cell|Subdirectory>|<cell|Added
    to>|<cell|Purpose>>|<row|<cell|<verbatim|progs>>|<cell|<verbatim|GUILE_LOAD_PATH>>|<cell|<scheme>
    modules and <verbatim|init-<em|name>.scm>>>|<row|<cell|<verbatim|bin>>|<cell|<verbatim|PATH>
    (appended)>|<cell|helper executables>>|<row|<cell|<verbatim|lib>>|<cell|<verbatim|LD_LIBRARY_PATH>>|<cell|shared
    libraries for <scm|:link>>>|<row|<cell|<verbatim|styles>>|<cell|<verbatim|TEXMACS_STYLE_ROOT>>|<cell|style
    files>>|<row|<cell|<verbatim|packages>>|<cell|<verbatim|TEXMACS_PACKAGE_ROOT>>|<cell|style
    packages>>|<row|<cell|<verbatim|texts>>|<cell|<verbatim|TEXMACS_TEXT_ROOT>>|<cell|text
    files>>|<row|<cell|<verbatim|doc>>|<cell|<verbatim|TEXMACS_DOC_PATH>>|<cell|documentation>>|<row|<cell|<verbatim|misc/patterns>>|<cell|<verbatim|TEXMACS_PATTERN_PATH>>|<cell|background
    patterns>>|<row|<cell|<verbatim|misc/pixmaps>>|<cell|<verbatim|TEXMACS_PIXMAP_PATH>>|<cell|icons>>|<row|<cell|<verbatim|misc/themes>>|<cell|<verbatim|TEXMACS_THEME_PATH>>|<cell|themes>>|<row|<cell|<verbatim|langs/natural/dic>>|<cell|<verbatim|TEXMACS_DIC_PATH>>|<cell|dictionaries>>>>>

  The style and package roots are searched recursively (through
  <cpp|search_sub_dirs>), so that a package
  <verbatim|<em|myplugin>/packages/session/<em|myplugin>.ts> is found. Since
  wildcards are expanded only for existing directories and only once at
  startup, <TeXmacs> has to be restarted after installing a new plug-in or
  after creating a new subdirectory (such as a <verbatim|bin> directory
  produced by a compilation).

  Since the <verbatim|progs> directories of all plug-ins are in the load
  path, <scheme> modules of plug-ins are simply named after their file
  names. For instance, the module in
  <source-link|plugins/python/progs/python-menus.scm|plugins/python/progs/python-menus.scm> is declared as
  <scm|(texmacs-module (python-menus) ...)> and loaded using
  <scm|(import-from (python-menus))>.

  <section|Lazy loading of plug-ins>

  <subsection|Initialization at boot time>

  Near the end of <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>, all plug-ins are scheduled
  for initialization:

  <\scm-code>
    (for-each lazy-plugin-initialize (plugin-list))
  </scm-code>

  The function <scm|lazy-plugin-initialize> (in
  <source-link|kernel/texmacs/tm-plugins.scm|TeXmacs/progs/kernel/texmacs/tm-plugins.scm>) marks the plug-in in the table
  <scm|plugin-initialize-todo> and postpones the actual initialization using
  <scm|(delayed (:idle 1000) (plugin-initialize name))>, that is, until
  <TeXmacs> has been idle for one second. This keeps the startup time low,
  even when many plug-ins are installed.

  The function <scm|(plugin-initialize <scm-arg|name>)> first loads the
  plug-in cache (<scm|plugin-load-setup>), and then loads the file
  <verbatim|plugins/<em|name>/progs/init-<em|name>.scm>, which is searched
  in <verbatim|$TEXMACS_HOME_PATH> and <verbatim|$TEXMACS_PATH> (in this
  order). When the last plug-in has been initialized, the cache is saved
  (<scm|plugin-save-setup>) if it had to be rebuilt.

  <\remark>
    The initialization file is only searched in <verbatim|$TEXMACS_HOME_PATH>
    and <verbatim|$TEXMACS_PATH>, although <cpp|plugin_list> and
    <cpp|plugin_path> also look into <verbatim|/etc/TeXmacs/plugins> and
    <verbatim|/usr/share/TeXmacs/plugins>. For a plug-in installed in one of
    the latter directories, the search paths are extended, but its
    initialization file is not loaded.
  </remark>

  <subsection|Forcing the initialization>

  Whenever some code needs complete information about the available
  plug-ins, it calls <scm|(lazy-plugin-force)>, which immediately
  initializes all plug-ins which are still pending. This is done
  automatically by <scm|connection-defined?>, <scm|connection-info>,
  <scm|connection-variants>, <scm|session-list>, <scm|scripts-list>,
  <scm|connection-get-handlers> and <scm|write-local-plugin-info>, as well
  as by the menus which list sessions and scripting languages. For
  instance, opening the <menu|Insert|Session> menu before the idle timer has
  fired loads all plug-ins at once.

  Consequently, the initialization file should be fast and should not do
  more than necessary. A good practice is to keep
  <verbatim|init-<em|myplugin>.scm> small, and to load the real
  implementation lazily from a <scm|(when (supports-<em|myplugin>?) ...)>
  block, using <scm|lazy-menu>, <scm|lazy-keyboard>, <scm|lazy-define>,
  <scm|lazy-input-converter> or <scm|import-from>.

  <section|The plug-in cache>

  Testing whether a plug-in is supported usually requires to search the
  <verbatim|PATH> for executables and sometimes to run them (for instance in
  order to determine their version). The results of these tests are
  therefore cached in the file

  <\verbatim-code>
    $TEXMACS_HOME_PATH/system/cache/plugin_cache.scm
  </verbatim-code>

  The file contains a list with three elements: the original value of the
  <verbatim|PATH> when the cache was created (<scm|get-original-path>), the
  table <scm|plugin-data-table> as an association list, and a table which
  associates to each directory of the <verbatim|PATH> its last modification
  time. The table <scm|plugin-data-table> maps the name of each plug-in to
  the result of its <scm|:require> or <scm|:versions> option; it also
  contains the unevaluated <scm|:prioritary> expressions under keys of the
  form <scm|(<em|name> :prioritary)>.

  When loading the cache, <scm|plugin-load-setup> checks, using
  <scm|path-up-to-date?>, that the <verbatim|PATH> did not change and that
  none of its directories has been modified since. If this is the case, the
  global variable <scm|reconfigure-flag?> is set to <scm|#f>: during the
  subsequent evaluation of <scm|plugin-configure>, the options
  <scm|:require>, <scm|:versions> and <scm|:setup> are skipped and the
  cached values are used. Otherwise the cache is rebuilt.

  The cache can be invalidated explicitly:

  <\explain>
    <scm|(reinit-plugin-cache)><explain-synopsis|re-detect all plug-ins>
  <|explain>
    Clear all connection tables, set <scm|reconfigure-flag?> and re-run the
    initialization files of all plug-ins. This is what
    <menu|Tools|Update|Plugins> and <menu|Insert|Session|Redetect> do. It is
    also called after changing the manual path to plug-in binaries
    (<scm|set-manual-path>) or an <abbr|API> key (<scm|set-manual-key>).
  </explain>

  <\explain>
    <scm|(reinit-plugin-single <scm-arg|name>)><explain-synopsis|re-detect
    one plug-in>
  <|explain>
    Same as above, but only for the plug-in <scm-arg|name>. The
    <verbatim|jupyter> plug-in uses this when the user toggles whether a
    language should be run via <name|Jupyter>.
  </explain>

  Removing the cache file by hand has the same effect as
  <scm|reinit-plugin-cache> at the next startup. This is a useful debugging
  step when a plug-in is unexpectedly (not) detected, for instance after
  installing a helper application in a directory whose modification time
  was not changed.

  <section|Connection tables and predicates>

  Processing the options of <scm|plugin-configure> fills a number of hash
  tables in <source-link|tm-plugins.scm|TeXmacs/progs/kernel/texmacs/tm-plugins.scm>: <scm|connection-variant> maps pairs
  <scm|(<em|name> <em|variant>)> to a description of the launcher, such as
  <scm|(tuple "pipe" <em|shell-cmd>)>, <scm|(tuple "dynlink" <em|lib>
  <em|symbol> <em|init>)>, <scm|(tuple "cmdline" <em|cmd-fun>
  <em|result-fun>)> or <scm|(tuple "request" <em|request-fun>
  <em|result-fun>)>; <scm|connection-varlist> contains the list of variants
  of each plug-in; <scm|connection-session> and <scm|connection-scripts> map
  plug-in names to their menu names; <scm|connection-handler> contains the
  channel handlers. The following functions give access to this
  information:

  <\explain>
    <scm|(connection-defined? <scm-arg|name>)>

    <scm|(connection-list)>

    <scm|(connection-variants <scm-arg|name>)><explain-synopsis|declared
    connections>
  <|explain>
    Test whether a connection with the given name was declared (locally or
    remotely), return the sorted list of all declared connections, resp.
    the list of variants of a connection. The function
    <scm|local-connection-variants> only returns the local variants.
  </explain>

  <\explain>
    <scm|(connection-info <scm-arg|name>
    <scm-arg|session>)><explain-synopsis|launcher for a session>
  <|explain>
    Return the launcher description for the session <scm-arg|session> of
    the plug-in <scm-arg|name>. If the session name contains a colon, then
    only the part before the colon is used as the variant, so that several
    sessions <verbatim|<em|variant>:1>, <verbatim|<em|variant>:2>,
    <abbr|etc.> may use the same launcher. If there is no launcher for the
    given variant, the first variant is used. This function is called from
    <c++> by <cpp|connection_start>.
  </explain>

  <\explain>
    <scm|(session-list)>, <scm|(session-defined? <scm-arg|name>)>,
    <scm|(session-name <scm-arg|name>)>

    <scm|(scripts-list)>, <scm|(scripts-defined? <scm-arg|name>)>,
    <scm|(scripts-name <scm-arg|name>)><explain-synopsis|sessions and
    scripting languages>
  <|explain>
    Plug-ins declared with <scm|:session> resp. <scm|:scripts>, and their
    menu names. The tables initially contain an entry for the built-in
    <verbatim|scheme> language (notice that <scm|reinit-plugin-cache> clears
    the tables without restoring this entry).
  </explain>

  The capabilities of a plug-in are tested with the following predicates,
  which take the name of the plug-in as argument:

  <\description>
    <item*|<scm|plugin-supports-completions?>>The plug-in was configured
    with <scm|(:tab-completion #t)> (<source-link|plugin-cmd.scm|TeXmacs/progs/utils/plugins/plugin-cmd.scm>).

    <item*|<scm|plugin-supports-input-done?>>The plug-in was configured
    with <scm|(:test-input-done #t)> (<source-link|plugin-cmd.scm|TeXmacs/progs/utils/plugins/plugin-cmd.scm>).

    <item*|<scm|plugin-supports-math-input-ref>>The plug-in declared
    mathematical input converters using <scm|plugin-input-converters>
    (<source-link|plugin-convert.scm|TeXmacs/progs/utils/plugins/plugin-convert.scm>).
  </description>

  Inside a session, the predicates <scm|(session-supports-completions?)>
  and <scm|(session-supports-input-done?)> (in
  <source-link|dynamic/session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm>) combine these tests with the current
  value of <verbatim|prog-language> and the status of the connection. The
  function <scm|(plugin-approx-command-ref <scm-arg|name>)> returns the
  name of the function which should be used for numeric evaluation in
  scripts (it is set by <scm|plugin-approx-command-set!>, for instance to
  <verbatim|"float"> by the <verbatim|maxima> plug-in).

  <section|Life cycle of a connection>

  <subsection|From the session to the <c++> side>

  When the user evaluates an input field in a session, the following
  sequence of calls takes place (all <scheme> functions are defined in
  <source-link|dynamic/session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm> or
  <source-link|utils/plugins/plugin-eval.scm|TeXmacs/progs/utils/plugins/plugin-eval.scm>):

  <\enumerate>
    <item><scm|session-feed> preprocesses the input (conversion of
    mathematical input if the option <scm|:math-input> is present), marks
    the output as busy and calls <scm|plugin-feed>.

    <item><scm|plugin-feed> appends the request to the queue of pending
    evaluations for the pair (language, session), together with four
    call-backs (<em|do>, <em|notify>, <em|next> and <em|cancel>). If the
    queue was empty, <scm|plugin-do> is called.

    <item>If the connection is not alive yet (status 0), <scm|plugin-do>
    first inserts a special start request, which calls <scm|plugin-start>
    and hence the <c++> function <cpp|connection_start>. Otherwise the
    <em|do> call-back of the first request is called, which eventually
    calls <scm|plugin-write>.

    <item><scm|plugin-write> calls <scm|connection-write>, which is
    implemented by <cpp|connection_write> in
    <source-link|System/Link/connection.cpp|src/System/Link/connection.cpp>. This function serializes the tree
    by calling back the <scheme> function <scm|plugin-serialize> and writes
    the resulting string to the link.
  </enumerate>

  For the <verbatim|scheme> language, <scm|plugin-write> does not use any
  link, but directly evaluates the input with <scm|scheme-eval>.

  <subsection|The <c++> side>

  The <c++> class <cpp|connection_rep> is a resource indexed by the string
  <verbatim|<em|name>-<em|session>>. Its main fields are the underlying
  <cpp|tm_link>, a status and two parsers <cpp|tm_out> and <cpp|tm_err>
  of type <cpp|texmacs_input> for the standard output and the standard error
  of the application. The function <cpp|connection_start> calls
  <scm|connection-info> and creates a link of the appropriate type:

  <\cpp-code>
    if (is_tuple (t, "pipe", 1)) {

    \ \ tm_link ln= make_pipe_link (t[1]-\<gtr\>label);

    \ \ con= tm_new\<less\>connection_rep\<gtr\> (name, session, ln);

    }

    else if (is_tuple (t, "dynlink", 3)) {

    \ \ tm_link ln=

    \ \ \ \ make_dynamic_link (t[1]-\<gtr\>label, t[2]-\<gtr\>label,
    t[3]-\<gtr\>label, session);

    \ \ con= tm_new\<less\>connection_rep\<gtr\> (name, session, ln);

    }
  </cpp-code>

  (<verbatim|"cmdline"> and <verbatim|"request"> tuples are handled in a
  similar way.) The status of a connection is one of the constants defined
  in <source-link|System/Link/tm_link.hpp|src/System/Link/tm_link.hpp>:

  <\cpp-code>
    #define CONNECTION_DEAD \ \ \ 0

    #define CONNECTION_DYING \ \ 1

    #define WAITING_FOR_INPUT \ 2

    #define WAITING_FOR_OUTPUT 3
  </cpp-code>

  A pipe link (<cpp|pipe_link_rep>) forks a child process which executes the
  launcher using <verbatim|/bin/sh -c>, with its standard input, output and
  error redirected to three pipes. The child is put into its own process
  group (<cpp|setsid>). Socket notifiers are attached to the output and
  error pipes, so that <cpp|connection_rep::listen> is called whenever data
  arrives. This function feeds the data character by character to the
  parsers, and then notifies the <scheme> side of the new output on the
  standard channels and on the channels declared with <scm|:handler>:

  <\cpp-code>
    read (LINK_ERR);

    connection_notify (this, "error", tm_err-\<gtr\>get ("error"));

    read (LINK_OUT);

    connection_notify (this, "output", tm_out-\<gtr\>get ("output"));

    connection_notify (this, "prompt", tm_out-\<gtr\>get ("prompt"));

    connection_notify (this, "input", tm_out-\<gtr\>get ("input"));
  </cpp-code>

  Each of these calls invokes the <scheme> function <scm|(connection-notify
  <scm-arg|lan> <scm-arg|ses> <scm-arg|channel> <scm-arg|tree>)>, which
  forwards the tree to the <em|notify> call-back of the pending request (for
  sessions: <scm|session-notify>, which inserts the output, error output,
  prompt or default input into the session). Changes of the status are
  reported through <scm|connection-notify-status>: status 2 (waiting for
  input) means that the current request is complete and the next one is
  started by <scm|plugin-next>; status 0 means that the application died,
  in which case all pending requests are cancelled.

  The <menu|Interrupt execution> entry of the session menus (also
  available as an icon) calls <scm|plugin-interrupt>, which sends
  <verbatim|SIGINT> to the process group of the application (this is not
  implemented under <name|Windows>) and cancels the pending requests. The
  <menu|Close session> entry calls <scm|plugin-stop>, which sends
  <verbatim|SIGTERM> to the process group, followed by <verbatim|SIGKILL>
  two seconds later.

  <section|The pipe protocol>

  <subsection|Control characters>

  The following control characters are defined in
  <source-link|System/Link/tm_link.hpp|src/System/Link/tm_link.hpp>:

  <descriptive-table|<tformat|<cwith|1|1|1|-1|cell-font-series|bold>|<table|<row|<cell|Name>|<cell|Code>|<cell|Meaning>>|<row|<cell|<verbatim|DATA_ABORT>>|<cell|1>|<cell|discard
    the verbatim text of the current block>>|<row|<cell|<verbatim|DATA_BEGIN>>|<cell|2>|<cell|start
    a block>>|<row|<cell|<verbatim|DATA_END>>|<cell|5>|<cell|end a
    block>>|<row|<cell|<verbatim|DATA_COMMAND>>|<cell|16>|<cell|prefix of
    special commands sent <em|to> the application>>|<row|<cell|<verbatim|DATA_ESCAPE>>|<cell|27>|<cell|take
    the next character literally>>>>>

  All output of the application is structured in blocks of the form
  <verbatim|DATA_BEGIN <em|format>: <em|message> DATA_END> (without the
  spaces) or <verbatim|DATA_BEGIN <em|channel># <em|message> DATA_END>.
  Blocks may be nested. The parser <cpp|texmacs_input_rep::put> maintains a
  stack of (format, channel) pairs; as soon as the <verbatim|DATA_END> which
  closes the outermost block has been read, the application is considered
  to be waiting for input. Characters outside any block are interpreted in
  the <verbatim|verbatim> format on the default channel. A
  <verbatim|DATA_ESCAPE> character causes the next character to be taken
  literally, which allows to use the control characters inside messages. A
  <verbatim|DATA_ABORT> character at the very beginning of a
  <verbatim|verbatim> or <verbatim|utf8> block suppresses the verbatim text
  of that block; according to a comment in the source code, this allows
  tab-completion with a naive read-eval loop which echoes its input.

  <subsection|Output formats>

  The format of a block determines how its message is converted into a
  <TeXmacs> tree (see <cpp|texmacs_input_rep::get_mode> and the various
  <cpp|*_flush> methods in <source-link|Data/Convert/Generic/input.cpp|src/Data/Convert/Generic/input.cpp>):

  <\description>
    <item*|<verbatim|verbatim>>Plain text, converted with
    <cpp|verbatim_to_tree> using automatic detection of the encoding. The
    text is flushed at each newline, which allows for gradual output.
    Blocks in other formats may be nested inside a <verbatim|verbatim>
    block.

    <item*|<verbatim|utf8>>Like <verbatim|verbatim>, but the text is
    explicitly converted from <abbr|UTF-8>. This is the format used by
    <verbatim|flush_verbatim> in the <verbatim|tmpy> <name|Python> package.

    <item*|<verbatim|latex>>A <LaTeX> snippet, converted using the
    <verbatim|latex-snippet> converter. Use <verbatim|$...$> for formulas.
    The text is flushed at empty lines.

    <item*|<verbatim|html>>An <name|HTML> snippet, converted using the
    <verbatim|html-snippet> converter and wrapped into an
    <markup|html-text> tag.

    <item*|<verbatim|scheme>>A <TeXmacs> tree in <scheme> syntax, such as
    <verbatim|(frac "a" "b")>. This is also the format of the answers to
    special commands (tab-completion, <scm|input-done?>).

    <item*|<verbatim|math>>A mathematical expression in prefix <scheme>
    notation, such as <verbatim|(+ (* 2 x) 1)>, which is converted into
    presentation markup by <scm|cas-\<gtr\>stree> (<source-link|utils/cas/cas-out.scm|TeXmacs/progs/utils/cas/cas-out.scm>)
    and inserted in math mode.

    <item*|<verbatim|ps>>Encapsulated <name|PostScript>, inserted as an
    image. The message may start with lines <verbatim|width=<em|w>> and
    <verbatim|height=<em|h>>; the default width is given by the preference
    <verbatim|plugins:embedded postscript width> (<verbatim|0.7par> by
    default).

    <item*|<verbatim|file>>The name of an image file, optionally followed by
    parameters, as in <verbatim|/tmp/plot.png?width=400px&height=300px>.
    Supported suffixes are <verbatim|png>, <verbatim|eps>, <verbatim|pdf>
    and <verbatim|svg>; the width may be given in <verbatim|pt>,
    <verbatim|px> or <verbatim|par> and the height in <verbatim|pt>,
    <verbatim|px> or <verbatim|pag>. The contents of the file are embedded
    in the document. Errors (such as a missing file) are reported as
    verbatim output.

    <item*|<verbatim|command>>A <scheme> command, which is evaluated as
    <verbatim|(begin <em|message>)> as soon as the block is closed. See
    <hlink|sending commands to <TeXmacs>|../interface/interface-commands.en.tm>.

    <item*|<verbatim|channel>>Legacy syntax for switching channels:
    <verbatim|DATA_BEGIN channel:prompt DATA_END> redirects the remainder of
    the enclosing block to the <verbatim|prompt> channel. It is equivalent
    to, but less convenient than, the <verbatim|prompt#> syntax.

    <item*|Other data formats>If <verbatim|<em|format>> is a data format
    known to <TeXmacs> (<scm|(format? <em|format>)>), then the message is
    converted using the converter from <verbatim|<em|format>-snippet> to
    <verbatim|texmacs-tree>. This allows plug-ins to emit output in formats
    which they define themselves (see below).
  </description>

  Blocks in an unknown format are treated as <verbatim|verbatim>. The
  formats <verbatim|cmdline-<em|name>> and <verbatim|request-<em|name>> are
  used internally for the <scm|:cmdline> and <scm|:request> connection
  types.

  <subsection|Channels>

  The channel of a block determines where its output goes. The channels
  which are understood by sessions are <verbatim|output> (the default for
  the standard output), <verbatim|prompt> (the prompt for the next input),
  <verbatim|input> (a default value for the next input) and
  <verbatim|error>. All output on the <em|standard error> of the
  application goes to the <verbatim|error> channel and is displayed as
  error output (in an <markup|errput> tag); it may again be structured
  using <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks, as done by
  <verbatim|flush_err> in <source-link|tmpy/protocol.py|plugins/tmpy/protocol.py>. Blocks on other
  channels are only delivered if a handler has been declared with the
  <scm|:handler> option; notice that the <verbatim|error> channel of the
  <em|standard output> is not displayed unless a handler is declared.

  <section|Input to the application>

  <subsection|Serialization>

  Before being sent to the application, the input tree is converted to a
  string by <scm|(plugin-serialize <scm-arg|lan> <scm-arg|t>)> (in
  <source-link|plugin-cmd.scm|TeXmacs/progs/utils/plugins/plugin-cmd.scm>), which calls the serializer declared with
  <scm|:serializer>, or <scm|verbatim-serialize> by default. The following
  building blocks are available for custom serializers:

  <\explain>
    <scm|(pre-serialize <scm-arg|lan> <scm-arg|t>)><explain-synopsis|preprocessing>
  <|explain>
    Remove a <markup|document> with a single child and convert
    <markup|math> content into a string using the mathematical input
    converters of the plug-in <scm-arg|lan>.
  </explain>

  <\explain>
    <scm|(verbatim-serialize <scm-arg|lan> <scm-arg|t>)><explain-synopsis|default
    serializer>
  <|explain>
    Apply <scm|pre-serialize>, convert the result to plain text using
    <scm|texmacs-\<gtr\>code> (in the locale charset), replace newlines and
    tabs by spaces, remove all other control characters and append a
    newline. In particular, multi-line input is sent as a single line; a
    plug-in which needs to preserve newlines must provide its own
    serializer (the <verbatim|python> plug-in, for instance, sends the input
    with its newlines followed by a line <verbatim|\<less\>EOF\<gtr\>>).
  </explain>

  <\explain>
    <scm|(generic-serialize <scm-arg|lan> <scm-arg|t>)><explain-synopsis|serialize
    as a verbatim block>
  <|explain>
    Send the input as a <verbatim|DATA_BEGIN verbatim: ... DATA_END> block,
    with the control characters escaped (<scm|escape-generic>). Newlines are
    preserved.
  </explain>

  <subsection|Special commands>

  Some requests are not ordinary input, but commands for the application,
  such as tab-completion requests. The string of such a command is
  formatted by <scm|(format-command <scm-arg|lan> <scm-arg|s>)>, which calls
  the function declared with <scm|:commander>, or by default prefixes the
  string with <verbatim|DATA_COMMAND> and appends a newline. The <scheme>
  function <scm|(plugin-command <scm-arg|lan> <scm-arg|ses> <scm-arg|cmd>
  <scm-arg|return> <scm-arg|opts>)> sends such a command asynchronously and
  passes the answer (usually a tree in the <verbatim|scheme> format) to the
  call-back <scm-arg|return>. Two commands are currently sent by <TeXmacs>:

  <\description>
    <item*|<verbatim|(complete <em|input-string>
    <em|cursor-position>)>>Sent by <scm|kbd-variant> (the <key|tab> key)
    in a session of a plug-in with <scm|:tab-completion>. The string is
    computed by <cpp|edit_interface_rep::session_complete_command> in
    <source-link|Edit/Interface/edit_complete.cpp|src/Edit/Interface/edit_complete.cpp>, using
    <scm|verbatim-serialize> (not the custom serializer of the plug-in).
    The answer should be a tuple <verbatim|(tuple <em|root>
    <em|completion-1> ...)>, which is passed to <scm|custom-complete>. See
    <hlink|tab-completion|../interface/interface-tab.en.tm>.

    <item*|<verbatim|(input-done? <em|input-string>)>>Sent by
    <scm|kbd-enter> when the user presses <key|return> in a session of a
    plug-in with <scm|:test-input-done>. The input is serialized with the
    serializer of the plug-in. The answer <verbatim|#f> inserts a newline,
    any other answer evaluates the input.
  </description>

  <subsection|Mathematical input>

  When mathematical input is enabled in a session (<menu|Mathematical
  input> in the input options menu of the session, which calls
  <scm|toggle-session-math-input>; this entry is only shown for plug-ins
  with input converters), the input field is a formula and the
  option <scm|:math-input> is passed to <scm|session-feed>. The function
  <scm|plugin-preprocess> then converts the formula into a string using
  <scm|(plugin-math-input (list 'tuple <scm-arg|lan>
  <scm-arg|formula>))>, provided that the plug-in declared input converters.
  The default for new sessions can be set with the boolean preference
  <verbatim|<em|lan>-math-input> (and similarly
  <verbatim|<em|lan>-text-input> for textual input), as read by
  <scm|session-math-input?> and <scm|session-text-input?>.

  Input converters are declared with <scm|plugin-input-converters> (see
  <hlink|mathematical and customized input|../interface/interface-input.en.tm>)
  and are usually put in a separate module which is loaded on demand:

  <\scm-code>
    (when (supports-maxima?)

    \ \ (lazy-input-converter (maxima-input) maxima))
  </scm-code>

  Rules which are not defined by the plug-in fall back to the rules of the
  <verbatim|generic> converter at the end of
  <source-link|utils/plugins/plugin-convert.scm|TeXmacs/progs/utils/plugins/plugin-convert.scm>, which for instance rewrites
  fractions as <verbatim|(a/b)>, <verbatim|\<less\>alpha\<gtr\>> as
  <verbatim|alpha> and matrices as <verbatim|[a, b; c, d]>.

  <section|Evaluation outside sessions>

  <subsection|Background and synchronous evaluation>

  <\explain>
    <scm|(plugin-eval <scm-arg|name> <scm-arg|session> <scm-arg|t>
    . <scm-arg|opts>)><explain-synopsis|synchronous evaluation>
  <|explain>
    Evaluate <scm-arg|t> (a <TeXmacs> tree in <scheme> form) using the
    session <scm-arg|session> of the plug-in <scm-arg|name>, and return the
    output in <scheme> form. The connection is started if necessary. This
    function is synchronous: it calls the <c++> function
    <cpp|connection_eval>, which blocks until the application is waiting
    for input again. The output is simplified with
    <scm|plugin-output-simplify>; the option <scm|:math-input> enables the
    conversion of mathematical input. The function is defined in the module
    <verbatim|(utils plugins plugin-eval)>.
  </explain>

  <\explain>
    <scm|(silent-feed <scm-arg|lan> <scm-arg|ses> <scm-arg|in>
    <scm-arg|return> <scm-arg|opts>)>

    <scm|(silent-feed* <scm-arg|lan> <scm-arg|ses> <scm-arg|in>
    <scm-arg|return> <scm-arg|opts>)><explain-synopsis|asynchronous
    evaluation>
  <|explain>
    Queue an asynchronous evaluation of <scm-arg|in> without any visible
    session. For <scm|silent-feed>, the call-back <scm-arg|return> receives
    a pair of the output and the error output, or one of the keywords
    <scm|:dead> and <scm|:interrupted>. For <scm|silent-feed*>, it receives
    a single tree: the output, the error output in red, or a
    <markup|script-dead> or <markup|script-interrupted> tag.
  </explain>

  <subsection|Scripts in documents>

  A plug-in configured with <scm|:scripts> can be selected as the scripting
  language of a document (<menu|Document|Scripts>), which sets the
  environment variable <verbatim|prog-scripts>. Formulas or selections can
  then be evaluated in place (<scm|script-eval>), numerically evaluated (<scm|script-approx>, which
  inserts the function set with <scm|plugin-approx-command-set!>), or
  turned into executable fields (<markup|script-input>). All these
  operations end up in <scm|(script-eval-at <scm-arg|where> <scm-arg|lan>
  <scm-arg|session> <scm-arg|in> . <scm-arg|opts>)> and <scm|script-feed>
  in <source-link|dynamic/scripts-edit.scm|TeXmacs/progs/dynamic/scripts-edit.scm>, which use <scm|silent-feed*> and
  replace the tree <scm-arg|where> by the result. The rendering of
  <markup|script-input> fields can be customized per language through a
  macro <markup|<em|lan>-script-input>.

  <subsection|Secure evaluation in style files>

  A <scheme> function which is declared with <scm|(:secure #t)> can be
  called from the <markup|extern> primitive in style files. By combining
  this with <scm|plugin-eval>, a plug-in can provide new markup whose
  rendering is computed by the extern application. See the <verbatim|secure>
  plug-in in the section on <hlink|background
  evaluations|../interface/interface-background.en.tm>.

  <section|Customizing the user interface>

  <subsection|Menus, icons and keyboard>

  The following hooks are provided for plug-ins:

  <\description>
    <item*|<scm|plugin-menu>>This menu is declared empty in
    <source-link|texmacs/menus/main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm> and linked into the main menu
    bar. A plug-in adds a top-level menu using a conditional binding, as
    done by the <verbatim|maxima> plug-in:

    <\scm-code>
      (menu-bind plugin-menu

      \ \ (:require (or (in-maxima?) (and (not-in-session?)
      (maxima-scripts?))))

      \ \ (=\<gtr\> "Maxima" (link maxima-menu)))
    </scm-code>

    <item*|<scm|session-help-icons>>Icons which are displayed in the help
    part of the toolbar inside sessions (see
    <source-link|plugins/python/progs/python-menus.scm|plugins/python/progs/python-menus.scm>).

    <item*|<scm|plugin-icons>>This menu is also declared in
    <source-link|main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm>, but it is not linked into any toolbar in the
    current version.

    <item*|<scm|plugin-preferences-widget>>The contents of the preferences
    of the plug-in, for plug-ins configured with <scm|:preferences>.
  </description>

  Keyboard shortcuts which are specific to a plug-in are defined with
  <scm|kbd-map> and the automatically defined modes, for instance
  <scm|(kbd-map (:mode in-maxima?) (:mode in-math?) ("$" "$"))>.

  <subsection|Style packages and rendering of sessions>

  When a session or a script input field for the language
  <verbatim|<em|lan>> is inserted, the style package
  <verbatim|<em|lan>.ts> is automatically added to the document, provided
  that it can be found in <verbatim|$TEXMACS_STYLE_PATH> (for instance in
  <verbatim|<em|lan>/packages/session/<em|lan>.ts>). The generic session
  markup in <source-link|packages/compute/session.ts|TeXmacs/packages/compute/session.ts> dispatches on the
  current programming language: if the macros
  <markup|<em|lan>-session>, <markup|<em|lan>-input>,
  <markup|<em|lan>-output>, <markup|<em|lan>-errput> or
  <markup|<em|lan>-textput> are defined, they are used instead of
  <markup|generic-session>, <markup|generic-input>, <abbr|etc.> The colors
  of prompts and inputs are controlled by the environment variables
  <verbatim|generic-prompt-color> and <verbatim|generic-input-color>, which
  a package may redefine. The default prompt, as long as the application
  did not send one, is <verbatim|<em|Lan>] > (<scm|plugin-prompt>).

  <section|Data formats and converters>

  A plug-in may also extend the set of file formats of <TeXmacs>, for
  instance in order to import and export source files of its language. A
  format is declared with <scm|define-format> and the conversions with
  <scm|converter> (see <source-link|kernel/texmacs/tm-convert.scm|TeXmacs/progs/kernel/texmacs/tm-convert.scm>). For
  example, <source-link|plugins/python/progs/python-format.scm|plugins/python/progs/python-format.scm> contains

  <\scm-code>
    (texmacs-module (python-format))

    \;

    (define-format python

    \ \ (:name "Python source code")

    \ \ (:suffix "py"))

    \;

    (converter texmacs-tree python-document

    \ \ (:function texmacs-\<gtr\>python))

    \;

    (converter python-snippet texmacs-tree

    \ \ (:function python-snippet-\<gtr\>texmacs))
  </scm-code>

  together with the corresponding <verbatim|python-document> and
  <verbatim|texmacs-tree>-to-<verbatim|python-snippet> converters. Such
  modules are loaded lazily with <scm|(lazy-format (<em|module>)
  <em|format> ...)>, which imports the module after two seconds of idle time
  or as soon as the list of formats is needed. For the plug-ins which are
  shipped with <TeXmacs>, these declarations are made in
  <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>; a third-party plug-in can put the
  <scm|lazy-format> declaration in its own initialization file. Once a
  format <verbatim|<em|fm>> with a converter from
  <verbatim|<em|fm>-snippet> to <verbatim|texmacs-tree> is defined, the
  application may also use <verbatim|<em|fm>> as an output format in the
  pipe protocol.

  <section|Remote plug-ins>

  <TeXmacs> can also run plug-ins which are installed on another machine,
  through <verbatim|ssh>. The command <scm|(detect-remote-plugins
  <scm-arg|server>)>, available through <menu|Insert|Session|Remote>, runs

  <\shell-code>
    ssh <em|server> "texmacs -s -x \\"(write-local-plugin-info)\\" -q"
  </shell-code>

  on the remote server. The function <scm|write-local-plugin-info> writes
  the remote <verbatim|PATH>, the list of pipe launchers and the session and
  scripting language tables. This information is stored in
  <verbatim|$TEXMACS_HOME_PATH/system/remote-plugins.scm>, and each remote
  launcher <verbatim|<em|cmd>> becomes a local launcher of the form
  <verbatim|ssh <em|server> "export PATH=...; <em|cmd>"> for the variant
  <verbatim|<em|server>/<em|variant>>. Only pipe launchers can be used
  remotely. The remote environment has to be prepared in the
  <verbatim|~/.bashrc> of the remote account.

  <section|Debugging plug-ins>

  <\itemize>
    <item>Run the helper application by hand in a terminal, with exactly
    the command given to <scm|:launch>, and check its output. The control
    characters <verbatim|DATA_BEGIN> and <verbatim|DATA_END> are usually
    displayed as <verbatim|^B> and <verbatim|^E>. Make sure that the output
    is flushed after each <verbatim|DATA_END>; otherwise <TeXmacs> waits
    forever.

    <item>Start <TeXmacs> with the option <verbatim|-debug-io>, or enable
    <menu|Debug|io> (the <menu|Debug> menu appears after enabling
    <menu|Tools|Debugging tool>). All data exchanged with pipes is then
    printed on the terminal, with the control characters displayed as
    <verbatim|[BEGIN]>, <verbatim|[END]>, <verbatim|[ESCAPE]>,
    <verbatim|[COMMAND]> and <verbatim|[ABORT]>, and the input preceded by
    <verbatim|[INPUT]>. With <menu|Debug|auto>, the launch commands and the
    results of dynamic linking are printed as well.

    <item>If a plug-in is not detected, check the <scm|:require> condition
    in a <scheme> session, for instance <scm|(url-exists-in-path?
    "maxima")>, check <scm|(supports-maxima?)> and use
    <menu|Tools|Update|Plugins> (or remove
    <verbatim|$TEXMACS_HOME_PATH/system/cache/plugin_cache.scm>) to force a
    new detection. Remember that the <verbatim|bin> directory of a plug-in
    is only added to the <verbatim|PATH> if it existed at startup.

    <item>Errors in the initialization file and unknown configuration
    options are reported in the terminal from which <TeXmacs> was started;
    unknown options produce the message <verbatim|warning: unsupported tm-configure
    option>.

    <item>Use the standard error of the application for diagnostics: it is
    displayed as error output in the session, and does not interfere with
    the <verbatim|DATA_BEGIN>-<verbatim|DATA_END> structure of the standard
    output.

    <item>Remember that the initialization of plug-ins is lazy: code in
    <verbatim|init-<em|myplugin>.scm> is only executed about one second
    after startup, or earlier if <scm|lazy-plugin-force> is called.
  </itemize>

  <section|Known limitations>

  The following peculiarities of the current implementation are worth
  knowing when writing a plug-in:

  <\itemize>
    <item>The <scm|:socket> option is accepted by <scm|plugin-configure>,
    but <cpp|connection_start> does not create socket links, so it is not
    functional.

    <item>In <cpp|connection_rep::start>, the startup output of a dynamic
    link is only processed immediately when the <em|plug-in> is called
    <verbatim|dynlink> (the test is on the name of the plug-in, not on the
    type of the link).

    <item>Dynamic linking requires <cpp|TM_DYNAMIC_LINKING> to be defined at
    compile time. The <name|autotools> configuration defines it (as
    <cpp|dlopen>) through <verbatim|misc/m4/dlopen.m4>; otherwise
    <cpp|symbol_install> returns <verbatim|"Dynamic linking not
    implemented">.

    <item>The <scm|:prioritary> option is read from the plug-in cache
    before the cache has been loaded, and therefore seems to have no
    effect.

    <item>Plug-ins in <verbatim|/etc/TeXmacs/plugins> and
    <verbatim|/usr/share/TeXmacs/plugins> are listed, but their
    initialization files are not loaded (see above).
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
