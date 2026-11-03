<TeXmacs|1.0.7.21>

<style|tmdoc>

<\body>
  <tmdoc-title|When and how to use <scheme>>

  You may invoke <scheme> programs from <TeXmacs> in different ways,
  depending on whether you want to customize some aspects of <TeXmacs>, to
  extend the editor with new functionality, to make your markup more dynamic,
  and so on. In this section, we list the major ways to invoke <scheme>
  routines.

  <paragraph*|User provided initialization files>

  In order to customize the basic aspects of <TeXmacs>, you may provide one
  or both of the initialization files

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/progs/my-init-texmacs.scm<new-line>
    \ \ \ ~/.TeXmacs/progs/my-init-buffer.scm
  </verbatim>

  The file <verbatim|my-init-texmacs.scm> is loaded when booting <TeXmacs>
  and <verbatim|my-init-buffer.scm> is executed each time a view of a
  document is created, after the system file
  <verbatim|$TEXMACS_PATH/progs/init-buffer.scm>. Both files can be opened
  with <menu|Developer|Open my-init-texmacs.scm> <abbr|resp.>
  <menu|Developer|Open my-init-buffer.scm> (the <menu|Developer> menu is
  shown when the developer tool is enabled in <menu|Tools>).

  Usually, the file <verbatim|my-init-texmacs.scm> contains personal keyboard
  bindings and menus. For instance, when putting the following piece of code
  in this file, you define the keyboard shortcuts <key|T h .> and <key|P r o
  p .> for starting a new theorem <abbr|resp.> proposition (and similar
  shortcuts for definitions and lemmas):

  <\scm-code>
    (kbd-map

    \ \ ("D e f ." (make 'definition))

    \ \ ("L e m ." (make 'lemma))

    \ \ ("P r o p ." (make 'proposition))

    \ \ ("T h ." (make 'theorem)))
  </scm-code>

  Similarly, the following commands extend the standard <menu|Insert> menu
  with a special section for the insertion of greetings:

  <\scm-code>
    (import-from (generic insert-menu))

    (menu-bind insert-menu

    \ \ (former)

    \ \ ---

    \ \ (-\<gtr\> "Opening"

    \ \ \ \ \ \ ("Dear Sir" (insert "Dear Sir,"))

    \ \ \ \ \ \ ("Dear Madam" (insert "Dear Madam,")))

    \ \ (-\<gtr\> "Closing"

    \ \ \ \ \ \ ("Yours sincerely" (insert "Yours sincerely,"))

    \ \ \ \ \ \ ("Greetings" (insert "Greetings,"))))
  </scm-code>

  The <scm|import-from> line is needed because the standard menu is defined
  lazily: at boot time, <scm|insert-menu> is only a stub, and the module
  <verbatim|(generic insert-menu)>, loaded later when the editor is idle,
  would define it again and replace your version. Loading the module first
  makes <scm|(former)> refer to the standard menu, and your definition comes
  after it.

  The customization of the <hlink|keyboard|../utils/utils-keyboard.en.tm> and
  <hlink|menus|../utils/utils-menus.en.tm> is described in more detail in the
  chapter about the <TeXmacs> extensions of <scheme>. Notice also that,
  because of the <hlink|lazy loading mechanism|overview-lazyness.en.tm>, you
  can not always assume that the standard key-bindings and menus are loaded
  before <verbatim|my-init-texmacs.scm>. This implies that some care is
  needed in the case of <hlink|redefinitions|overview-lazyness.en.tm#redefinitions>.

  The file <verbatim|my-init-buffer.scm> can for instance be used in order to
  automatically select a certain style when starting a new document:

  <\scm-code>
    (if (not (buffer-has-name? (current-buffer)))

    \ \ \ \ (begin

    \ \ \ \ \ \ (init-style "article")

    \ \ \ \ \ \ (buffer-pretend-saved (current-buffer))))
  </scm-code>

  Notice that the \Pno name\Q check is important: when omitted, the styles of
  existing documents would also be changed to <tmstyle|article>. The function
  <scm|buffer-pretend-saved> is used in order to avoid <TeXmacs> to complain
  about unsaved documents when leaving <TeXmacs> without changing the
  document.

  Another typical use of <verbatim|my-init-buffer.scm> is when you mainly
  want to use <TeXmacs> as a front-end to another system. For instance, the
  following code will force <TeXmacs> to automatically launch a <name|Maxima>
  session for every newly opened document:

  <\scm-code>
    (if (not (buffer-has-name? (current-buffer)))

    \ \ \ \ (make-session "maxima" (url-\<gtr\>string (current-buffer))))
  </scm-code>

  Using <scm|(url-\<gtr\>string (current-buffer))> as the second argument of
  <scm|make-session> ensures that a different session will be opened for
  every new buffer. If you want all buffers to share a common instance of
  <name|Maxima>, then you should use <scm|"default"> instead, for the second
  argument.

  <paragraph*|User provided plug-ins>

  The above technique of <scheme> initialization files is sufficient for
  personal customizations of <TeXmacs>, but not very convenient if you want
  to share extensions with other users. A more portable way to extend the
  editor is therefore to regroup your <scheme> programs into a <em|plug-in>.

  The simplest way to write a plug-in <verbatim|<em|name>> with some
  additional <scheme> functionality is to create two directories and a file

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/plugins/<em|name><new-line>
    \ \ \ ~/.TeXmacs/plugins/<em|name>/progs<new-line>
    \ \ \ ~/.TeXmacs/plugins/<em|name>/progs/init-<em|name>.scm
  </verbatim>

  Furthermore, the file <verbatim|init-<em|name>.scm> should a piece of
  configuration code of the form

  <\scm-code>
    (plugin-configure <em|name>

    \ \ (:require #t))
  </scm-code>

  Any other <scheme> code present in <verbatim|init-<em|name>.scm> will then
  be executed when the plug-in is booted, that is, shortly after <TeXmacs> is
  started up. By using the additional <scm|(:prioritary #t)> option, you may
  force the plug-in to be loaded earlier during the boot procedure.

  Of course, the plug-in mechanism is more interesting when the plug-in
  contains more than a few customization routines. In general, a plug-in may
  also contain additional style files or packages, scripts for launching
  extern binaries, additional icons and internationalization files, and so
  on. Furthermore, <scheme> extensions are usually regrouped into <scheme>
  modules in the directory

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/plugins/<em|name>/progs
  </verbatim>

  The initialization file <verbatim|init-<em|name>.scm> should then be kept
  as short as possible so as to save boot time: it usually only contains
  <hlink|lazy declarations|overview-lazyness.en.tm> which allow <TeXmacs> to
  load the appropriate modules only when needed.

  For more information about how to write plug-ins, we refer to the
  <hlink|corresponding chapter|../../plugin/plugins.en.tm>.

  <paragraph*|Interactive invocation of <scheme> commands>

  In order to rapidly test the effect of <scheme> commands, it is convenient
  to execute them directly from within the editor. <TeXmacs> provides two
  mechanisms for doing this: directly type the command on the footer using
  the <shortcut|(interactive footer-eval)> shortcut, or start a <scheme>
  session using <menu|Insert|Session|Scheme>.

  The first mechanism is useful when you do not want to alter the document or
  when the current cursor position is important for the command you wish to
  execute. For instance, the command <verbatim|(inside? 'theorem)> to test
  whether the cursor is inside a theorem usually makes no sense when you are
  inside a session.

  <scheme> sessions are useful when the results of the <scheme> commands do
  not fit on the footer, or when you want to keep your session inside a
  document for later use. Some typical commands you might want to use inside
  a <scheme> session are as follows (try positioning your cursor inside the
  session and execute them):

  <\session|scheme|default>
    <\input|Scheme] >
      (define (square x) (* x x))
    </input>

    <\input|Scheme] >
      (square 1111111)
    </input>

    <\input|Scheme] >
      (kbd-map ("h i ." (insert "Hi there!")))
    </input>

    <\input|Scheme] >
      ;; now try typing "hi."
    </input>
  </session>

  <paragraph*|Command-line options for executing <scheme> commands>

  <TeXmacs> also provides several command-line options for the execution of
  <scheme> commands. This is useful when you want to use <TeXmacs> as a batch
  processor. The <scheme>-related options are the following:

  <\description-long>
    <item*|<with|font-series|medium|<verbatim|-x <em|cmd>>>>Executes the
    scheme command <verbatim|<em|cmd>> when booting has completed. For
    instance,

    <\shell-code>
      texmacs -x "(display \\"Hi there\\\\n\\")"
    </shell-code>

    causes <TeXmacs> to print \PHi there\Q when starting up. Notice that the
    <verbatim|-x> option may be used several times.

    <item*|<with|font-series|medium|<verbatim|-q>>>This option causes
    <TeXmacs> to quit. It is usually used after a <verbatim|-x> option. For
    instance,

    <\shell-code>
      texmacs text.tm -x "(print)" -q
    </shell-code>

    will cause <TeXmacs> to load the file <verbatim|text.tm>, to print it,
    and quit.

    <item*|<with|font-series|medium|<verbatim|-c <em|in> <em|out>>>>This
    options may be used to convert the input file <verbatim|<em|in>> into the
    output file <verbatim|<em|out>>. The suffixes of <verbatim|<em|in>> and
    <verbatim|<em|out>> determine their file formats. It runs the commands
    <scm|load-buffer> and <scm|export-buffer>, and is usually followed by
    <verbatim|-q>.

    <item*|<with|font-series|medium|<verbatim|-headless>>>Runs <TeXmacs>
    without opening a window (also <verbatim|-H>), for instance for
    conversions with <verbatim|-c> or commands with <verbatim|-x>.

    <item*|<with|font-series|medium|<verbatim|-i <em|file>>>>Uses
    <verbatim|<em|file>> instead of the standard initialization file of
    <scheme> (also <verbatim|-initialize>).

    <item*|<with|font-series|medium|<verbatim|-b <em|file>>>>Uses
    <verbatim|<em|file>> instead of the system file
    <verbatim|$TEXMACS_PATH/progs/init-buffer.scm>, which is executed for
    each new view (also <verbatim|-initialize-buffer>).

    <item*|<with|font-series|medium|<verbatim|-test-suite
    <em|dir>>>>Typesets the documents of the directory
    <verbatim|<em|dir>> into <verbatim|<em|dir>-check>, compares them with
    the reference results in <verbatim|<em|dir>-ref> (made with
    <verbatim|-reference-suite <em|dir>>) and quits.
  </description-long>

  <paragraph*|Invoking <scheme> scripts from <TeXmacs> markup>

  <label|markup-scripts><TeXmacs> provides two major tags for invoking
  <scheme> scripts from within the markup:

  <\description-long>
    <item*|<with|font-series|medium|<explain-macro|action|text|script>>>This
    tag works like a hyperlink with body <src-arg|text>, but such that the
    <scheme> command <src-arg|script> is invoked when clicking on the
    <src-arg|text>. For instance, when clicking <action|here|(lambda ()
    (system "xterm"))>, you will launch an<nbsp><verbatim|xterm>.

    <item*|<with|font-series|medium|<explain-macro|extern|fun|arg-1|...|arg-n>>>This
    tag is used in order to implement macros whose body is written in
    <scheme> rather than the<nbsp><TeXmacs> macro language. The first
    argument <src-arg|fun> is a scheme function with <src-arg|n> arguments.
    During the typesetting phase, <TeXmacs> passes the arguments
    <src-arg|arg-1> until <src-arg|arg-n> to<nbsp><src-arg|fun>, and the
    result will be typeset. For instance, the code

    <\tm-fragment>
      <inactive*|<extern|(lambda (x) `(concat "Hallo " ,x))|Piet>>
    </tm-fragment>

    produces the output \P<extern|(lambda (x) `(concat "Hallo " ,x))|Piet>\Q.
    Notice that the argument \PPiet\Q remains editable.
  </description-long>

  It should be noticed that the direct invocation of <scheme> scripts from
  within documents carries as risk: an evil person might send you a document
  with a script which attempts to erase your hard disk (for instance). For
  this reason, <TeXmacs> implements a way to test whether scripts can be
  considered secure or not. For instance, when clicking <action|here|(lambda
  () (system "xterm"))> (so as to launch an <verbatim|xterm>), the editor
  will prompt you by default in order to confirm whether you wish to execute
  this script. The desired level of security can be specified with the
  <menu|Security> field in the <menu|Other> tab of the preferences dialog
  (<menu|Edit|Preferences>). When writing your own <scheme> extensions
  to <TeXmacs>, it is also possible to define routines as being secure.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>