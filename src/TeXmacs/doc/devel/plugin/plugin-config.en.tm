<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Summary of the configuration options for plug-ins>

  <section|The <scm|plugin-configure> macro>

  As explained before, the <scheme> configuration file
  <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm> of a plug-in with
  name <verbatim|<em|myplugin>> should contain an instruction of the type

  <\scm-code>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scm-code>

  The macro <scm|plugin-configure> is defined in
  <verbatim|kernel/texmacs/tm-plugins.scm>. Each option is a list whose
  first element is a keyword; the options are processed <em|in order> by
  <scm|plugin-configure-cmd>. After each option, the processing stops as
  soon as the plug-in is known to be unsupported (that is, as soon as the
  entry of the plug-in in the internal <scm|plugin-data-table> is false).
  In practice this means that options which follow a failing <scm|:require>
  are ignored, whereas options which precede it (such as <scm|:winpath>)
  are always executed.

  The list of options is <em|quasi-quoted>. The five options
  <scm|:require>, <scm|:versions>, <scm|:preferences>, <scm|:setup> and
  <scm|:initialize> are special: their argument is automatically wrapped
  into a <scm|lambda> by <scm|plugin-configure-sub>, so that it is
  evaluated at the moment the option is processed (or not at all). For all
  other options the arguments are taken literally, so computed values and
  <scheme> functions have to be inserted with an explicit unquote, as in
  <scm|(:launch ,(python-launcher))> or <scm|(:serializer
  ,python-serialize)>. It is also possible to splice a computed list of
  options, as the <verbatim|maxima> plug-in does with <scm|,@(maxima-launchers)>.

  <section|The plug-in cache>

  Several options interact with the <em|plug-in cache>, which is stored in
  <verbatim|$TEXMACS_HOME_PATH/system/cache/plugin_cache.scm>. When the
  cache is valid, the global flag <scm|reconfigure-flag?> is false and the
  (potentially expensive) tests of <scm|:require>, <scm|:versions> and
  <scm|:setup> are <em|not> evaluated: their cached results are used
  instead. The cache is rebuilt (and <scm|reconfigure-flag?> set to true)
  when it does not exist yet, when the <verbatim|PATH> changed, when one of
  the directories of the <verbatim|PATH> has been modified since the last
  run, or when the user explicitly asks for it using
  <menu|Tools|Update|Plugins> or <menu|Insert|Session|Redetect> (both call
  <scm|reinit-plugin-cache>). See the <hlink|section on
  internals|plugin-internals.en.tm> for more details.

  <section|Available options>

  Here follows the complete list of the options which are recognized by
  <scm|plugin-configure-cmd>. Any other option triggers the message
  <verbatim|warning: unsupported tm-configure option> on the standard
  output, and is otherwise ignored.

  <subsection|Detection and set-up>

  <\explain>
    <scm|(:require <scm-arg|condition>)><explain-synopsis|sanity check>
  <|explain>
    This option specifies a sanity <scm-arg|condition> which needs to be
    satisfied by the plug-in. Usually, it is checked that certain binaries
    or libraries are present on your system, for instance using
    <scm|(url-exists-in-path? "maxima")>. If the condition evaluates to
    <scm|#f>, then <TeXmacs> will continue as whether your plug-in did not
    exist: the remaining options are ignored and
    <scm|(supports-<em|myplugin>?)> returns <scm|#f>. The condition is only
    evaluated when the plug-in cache is rebuilt; otherwise the cached result
    is used. If an alternative launcher is installed for the plug-in (see
    <scm|:launch> below), then the condition is not evaluated and the
    plug-in is considered to be supported.
  </explain>

  <\explain>
    <scm|(:versions <scm-arg|version-cmd>)><explain-synopsis|detect
    available versions>
  <|explain>
    This option is similar to <scm|:require>, but <scm-arg|version-cmd>
    should evaluate to a list of available versions of the helper
    application (or <scm|#f> if there is none). The result is stored in the
    cache and can be retrieved later using <scm|(plugin-versions
    "<em|myplugin>")>, which always returns a list. Like <scm|:require>,
    the expression is only evaluated when the cache is rebuilt. The
    <verbatim|maxima> plug-in uses this in order to define one launcher for
    each installed version of <name|Maxima>.
  </explain>

  <\explain>
    <scm|(:setup <scm-arg|cmd>)><explain-synopsis|one-time set-up>
  <|explain>
    The command <scm-arg|cmd> is only executed when the plug-in cache is
    being rebuilt, that is, at the first run, after a change of the
    <verbatim|PATH> or of one of its directories, or after
    <menu|Tools|Update|Plugins>. It can be used for set-up work whose result
    remains valid as long as the environment does not change.
  </explain>

  <\explain>
    <scm|(:initialize <scm-arg|cmd>)><explain-synopsis|initialization code>
  <|explain>
    The command <scm-arg|cmd> is executed each time the configuration is
    processed (provided that the plug-in is supported). In most plug-ins,
    the same effect is obtained by putting code inside a <scm|(when
    (supports-<em|myplugin>?) ...)> block after the <scm|plugin-configure>
    instruction.
  </explain>

  <\explain>
    <scm|(:prioritary <scm-arg|expr>)><explain-synopsis|initialize early>
  <|explain>
    The unevaluated expression <scm-arg|expr> is stored in the plug-in cache.
    It is evaluated by <scm|lazy-plugin-initialize>: when it is true, the
    initialization file of the plug-in is loaded immediately at startup
    instead of after one second of idle time. No plug-in of the
    distribution currently uses this option. Notice that
    <scm|lazy-plugin-initialize> is called before the plug-in cache is
    loaded from disk, so that the option does not seem to have an effect
    in the current implementation.
  </explain>

  <\explain>
    <scm|(:preferences <scm-arg|condition>)><explain-synopsis|the plug-in has
    preferences>
  <|explain>
    If <scm-arg|condition> evaluates to a true value, then the plug-in is
    listed by <scm|(plugins-with-preferences)>, and hence in the dialogue
    opened by <menu|Insert|Session|Preferences>. The actual contents of
    the preferences widget are provided by overloading the widget
    <scm|plugin-preferences-widget> for the plug-in, as in the
    <verbatim|python> plug-in:

    <\scm-code>
      (tm-widget (plugin-preferences-widget name)

      \ \ (:require (== name "python"))

      \ \ (aligned

      \ \ \ \ (meti (hlist // (text "Run via Jupyter"))

      \ \ \ \ \ \ (toggle (run-via-jupyter "python" answer)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ (run-via-jupyter? "python")))))
    </scm-code>
  </explain>

  <\explain>
    <scm|(:winpath <scm-arg|package-path> <scm-arg|inner-bin-path>)>

    <scm|(:macpath <scm-arg|package-path>
    <scm-arg|inner-bin-path>)><explain-synopsis|search helper binaries>
  <|explain>
    Specify where to search for the helper application under <name|Windows>
    (<scm|:winpath>) resp. <name|macOS> (<scm|:macpath>); the option has no
    effect on other systems. The <scm-arg|package-path> is the usual
    installation directory of the application, relative to a list of
    standard roots, and may contain wildcards. The <scm-arg|inner-bin-path>
    is the place where to look for the binary, relative to the
    <scm-arg|package-path>. All existing directories which match are
    appended to the <verbatim|PATH>. Under <name|Windows>, the roots are
    <verbatim|C:\\.>, <verbatim|C:\\Program File*>,
    <verbatim|$HOME\\AppData\\Local> and
    <verbatim|$HOME\\AppData\\Local\\Programs>; under <name|macOS> they are
    <verbatim|/Applications> and <verbatim|$HOME/Applications>. For
    instance, the <verbatim|pari> plug-in uses <scm|(:macpath "Pari*"
    "Contents/Resources/bin")>.

    The same functionality is also available as the ordinary functions
    <scm|(plugin-add-windows-path <scm-arg|rad> <scm-arg|rel>
    <scm-arg|after?>)> and <scm|(plugin-add-macos-path <scm-arg|rad>
    <scm-arg|rel> <scm-arg|after?>)>, which may be called before
    <scm|plugin-configure> and which allow to prepend (<scm-arg|after?>
    false) instead of append the directories.
  </explain>

  <subsection|Connection types>

  The following options declare how <TeXmacs> communicates with the helper
  application. Each of them may take an optional first argument
  <scm-arg|variant>, a string which names a <em|variant> of the plug-in
  (for instance a version number); the default variant is called
  <verbatim|"default">. When a plug-in has several variants, the
  <menu|Insert|Session> menu displays a submenu with all variants.

  <\explain>
    <scm|(:launch <scm-arg|shell-cmd>)>

    <scm|(:launch <scm-arg|variant> <scm-arg|shell-cmd>)><explain-synopsis|communication
    through pipes>
  <|explain>
    This option specifies that the plug-in is able to evaluate expressions
    over a pipe, using a helper application which is launched using the
    shell command <scm-arg|shell-cmd>. The command is executed using
    <verbatim|/bin/sh -c> on <name|Unix> systems. The protocol used on the
    pipe is described in the chapter about <hlink|writing
    interfaces|../interface/interface.en.tm>. If an alternative launcher is
    installed for the plug-in, that is, if the overloadable function
    <scm|(alt-launcher <scm-arg|name>)> returns a command instead of
    <scm|#f>, then this command replaces <scm-arg|shell-cmd>. The
    <verbatim|jupyter> plug-in uses this mechanism in order to run other
    plug-ins through a <name|Jupyter> kernel.
  </explain>

  <\explain>
    <scm|(:link <scm-arg|lib-name> <scm-arg|export-symbol>
    <scm-arg|options>)>

    <scm|(:link <scm-arg|variant> <scm-arg|lib-name> <scm-arg|export-symbol>
    <scm-arg|options>)><explain-synopsis|dynamic linking>
  <|explain>
    This option is similar to <scm|:launch>, except that the extern
    application is now linked dynamically as a shared library. The library
    <scm-arg|lib-name> is searched in <verbatim|$LD_LIBRARY_PATH>, the
    symbol <scm-arg|export-symbol> should be a structure of type
    <cpp|package_exports_1> and the string <scm-arg|options> is passed to
    its installation routine. For more information, see the section about
    <hlink|dynamic linking|../interface/interface-dynlibs.en.tm>. Dynamic
    linking is only available when <TeXmacs> has been compiled with
    <cpp|TM_DYNAMIC_LINKING> and never under <name|Windows>.
  </explain>

  <\explain>
    <scm|(:cmdline ,<scm-arg|cmd-fun> ,<scm-arg|result-fun>)><explain-synopsis|one
    process per evaluation>
  <|explain>
    With this connection type, a new shell command is launched for each
    evaluation. For each input string <scm-arg|in>, <TeXmacs> calls
    <scm|(<scm-arg|cmd-fun> <scm-arg|name> <scm-arg|session> <scm-arg|in>)>,
    which should return the shell command to be executed (newlines in the
    input are replaced by spaces and <verbatim|2\<gtr\> /dev/null> is
    appended to the command). When the process terminates, its complete
    standard output <scm-arg|res> is passed to <scm|(<scm-arg|result-fun>
    <scm-arg|name> <scm-arg|session> <scm-arg|res>)>, which should return
    the <TeXmacs> tree to be inserted as output. This connection type is
    used by the <name|AI> plug-ins (<verbatim|chatgpt>, <verbatim|gemini>,
    <verbatim|ollama>, <abbr|etc.>), which are all declared in
    <verbatim|plugins/ai/progs/init-ai.scm>.
  </explain>

  <\explain>
    <scm|(:request ,<scm-arg|request-fun>
    ,<scm-arg|result-fun>)><explain-synopsis|evaluation through network
    requests>
  <|explain>
    This is similar to <scm|:cmdline>, but <scm-arg|request-fun> returns a
    string with a <scheme> expression describing a network request instead
    of a shell command. The only request which is currently understood by
    <verbatim|System/Link/request_link.cpp> is of the form
    <verbatim|(http_post <em|url> (tuple <em|header-1> ...) <em|data>)>,
    which performs an asynchronous <abbr|HTTP> <verbatim|POST> of <abbr|JSON>
    data. The answer is again passed to <scm-arg|result-fun>. This
    connection type is used by the <verbatim|albert> plug-in.
  </explain>

  <\explain>
    <scm|(:socket <scm-arg|host> <scm-arg|port>)>

    <scm|(:socket <scm-arg|variant> <scm-arg|host>
    <scm-arg|port>)><explain-synopsis|sockets (not operational)>
  <|explain>
    This option is accepted by <scm|plugin-configure> and registers a
    connection of type <verbatim|"socket">. However, the function
    <cpp|connection_start> in <verbatim|System/Link/connection.cpp> only
    knows how to start connections of the types <verbatim|"pipe">,
    <verbatim|"dynlink">, <verbatim|"cmdline"> and <verbatim|"request">, so
    this option cannot be used in practice.
  </explain>

  <subsection|Sessions and scripts>

  <\explain>
    <scm|(:session <scm-arg|menu-name>)><explain-synopsis|shell sessions>
  <|explain>
    This option indicates that the plug-in supports an evaluator for
    interactive shell sessions. An item <scm-arg|menu-name> will be inserted
    in the <menu|Insert|Session> menu in order to launch such sessions. The
    <scm-arg|menu-name> is also used as the \Phuman\Q name of the plug-in by
    <scm|plugin-\<gtr\>name> and <scm|session-name>.
  </explain>

  <\explain>
    <scm|(:scripts <scm-arg|menu-name>)><explain-synopsis|scripting language>
  <|explain>
    This option indicates that the plug-in may be used as a scripting
    language inside documents. The plug-in then appears under the name
    <scm-arg|menu-name> in the <menu|Document|Scripts> menu, in the
    <menu|Insert|Scripts> submenu (in text mode) and in the list of
    scripting languages in the preferences.
  </explain>

  <\explain>
    <scm|(:serializer ,<scm-arg|fun>)><explain-synopsis|input serialization>
  <|explain>
    If the plug-in can be used as an evaluator, then this option specifies
    the <scheme> function <scm-arg|fun> which is used in order to transform
    <TeXmacs> trees to strings. The function is called as
    <scm|(<scm-arg|fun> <scm-arg|lan> <scm-arg|t>)>, where <scm-arg|lan> is
    the name of the plug-in and <scm-arg|t> the input in <scheme> form, and
    it should return the string which is sent to the application (usually
    terminated by a newline). The default serializer is
    <scm|verbatim-serialize>; see <hlink|mathematical and customized
    input|../interface/interface-input.en.tm>.
  </explain>

  <\explain>
    <scm|(:commander ,<scm-arg|fun>)><explain-synopsis|formatting of special
    commands>
  <|explain>
    This option is similar to the <scm|:serializer> option, except that it
    is used to transform special commands (such as tab-completion requests
    and <scm|(input-done? ...)> queries) to strings. The function is called
    with a single argument, the command string. By default, the command is
    prefixed by the <verbatim|DATA_COMMAND> character (ASCII 16) and
    terminated by a newline.
  </explain>

  <\explain>
    <scm|(:tab-completion #t)><explain-synopsis|tab-completion>
  <|explain>
    This option indicates that the plug-in supports <hlink|tab-completion|../interface/interface-tab.en.tm>.
  </explain>

  <\explain>
    <scm|(:test-input-done #t)><explain-synopsis|multi-line input>
  <|explain>
    This option indicates that the plug-in provides a routine for testing
    whether the input is complete; see <hlink|miscellaneous
    features|../interface/interface-misc.en.tm>.
  </explain>

  <\explain>
    <scm|(:handler <scm-arg|channel> <scm-arg|fun-name>)><explain-synopsis|custom
    output channels>
  <|explain>
    Declare that output of the application to the channel
    <scm-arg|channel> (a string) should be passed to the <scheme> function
    with name <scm-arg|fun-name>. Notice that <scm-arg|fun-name> is a
    <em|symbol> (no unquote): the function is looked up by name and called
    with the output document as its only argument. See the
    <verbatim|handler> example plug-in, which uses <scm|(:handler "error"
    handler-error-handler)>.
  </explain>

  <\explain>
    <scm|(:filter-in <scm-arg|x>)><explain-synopsis|obsolete>
  <|explain>
    This option is still accepted for compatibility, but it is ignored.
  </explain>

  <section|Automatically defined predicates>

  It should be noticed that the configuration of the plug-in
  <verbatim|<em|myplugin>> automatically creates a few predicates:

  <\description>
    <item*|<scm|(supports-<em|myplugin>?)>>Test whether the plug-in is fully
    operational (all requirements are met), or whether a remote plug-in with
    the same name has been declared.

    <item*|<scm|(in-<em|myplugin>?)>>Test whether <verbatim|<em|myplugin>>
    is the current programming language, that is, whether the environment
    variable <verbatim|prog-language> equals <verbatim|"<em|myplugin>">. This
    is typically the case inside a session of the plug-in. This predicate
    is defined as a <TeXmacs> mode (<scm|in-<em|myplugin>%>), so that it can
    be used in <scm|(:mode ...)> clauses of keyboard and menu definitions.

    <item*|<scm|(<em|myplugin>-scripts?)>>Test whether
    <verbatim|<em|myplugin>> is the current scripting language, that is,
    whether the environment variable <verbatim|prog-scripts> equals
    <verbatim|"<em|myplugin>">. This predicate is also defined as a mode.
  </description>

  <section|Several connections in one plug-in>

  A single initialization file may contain several <scm|plugin-configure>
  instructions for different names. For instance, the file
  <verbatim|plugins/ai/progs/init-ai.scm> declares the connections
  <verbatim|chatgpt>, <verbatim|gemini>, <verbatim|ollama>,
  <verbatim|open-mistral-7b> and <verbatim|albert>, and the
  <verbatim|jupyter> plug-in generates one <scm|plugin-configure>
  instruction for each <name|Jupyter> kernel found on the system. Moreover,
  <scm|plugin-configure> may be called again later for an existing plug-in
  in order to complete its configuration; for instance, the
  <verbatim|complete> example plug-in sends the command
  <scm|(plugin-configure complete (:tab-completion #t))> to <TeXmacs> in its
  startup banner.

  <tmdoc-copyright|1998--2013|Joris van der Hoeven>

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
