<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|The module system and lazy definitions>

  As explained above, each <scheme> file inside <TeXmacs> or one of its
  plug-ins corresponds to a <em|<TeXmacs> module>. The individual <TeXmacs>
  modules are usually grouped together into an internal or external module,
  which corresponds to a directory on your hard disk.

  Any <TeXmacs> module should start with an instruction of the form

  <\scm-code>
    (texmacs-module <em|name>

    \ \ (:use <em|submodule-1> ... <em|submodule-n>))
  </scm-code>

  The <verbatim|<em|name>> of the module is a list which corresponds to the
  location of the corresponding file. More precisely, <TeXmacs> searches for
  its modules in the directory <verbatim|$TEXMACS_PATH/progs>, in the
  directories of <verbatim|$GUILE_LOAD_PATH>, in your personal directory
  <verbatim|$TEXMACS_HOME_PATH/progs> (that is,
  <verbatim|~/.TeXmacs/progs>), and in the <verbatim|progs> subdirectories
  of the plug-ins (this path is used by <name|S7> too). For instance, the module <verbatim|(math math-edit)>
  corresponds to the file

  <verbatim| \ \ \ $TEXMACS_PATH/progs/math/math-edit.scm>

  The option <scm|(:inherit <em|module-1> ... <em|module-n>)> can be used
  instead of <scm|:use> in order to import the given modules and re-export
  all their public symbols. Outside a module declaration, the same effects
  are obtained with <scm|(use-modules <em|module-1> ... <em|module-n>)>
  <abbr|resp.> <scm|(inherit-modules <em|module-1> ... <em|module-n>)>.

  The user should explicitly specify all submodules on which the module
  depends, except those modules which are loaded by default, <abbr|i.e.> the
  language extensions in the directory

  <verbatim| \ \ \ $TEXMACS_PATH/progs/kernel>

  (loaded by <source-link|init-kernel.scm|TeXmacs/progs/init-kernel.scm>).
  The utilities in <verbatim|$TEXMACS_PATH/progs/utils/library> are not all
  loaded at boot time: use for instance <scm|(:use (utils library
  cursor))>.

  All symbols which are defined inside the module using <scm|define> or
  <scm|define-macro> are only visible within the module itself. In order to
  make the symbol publicly visible you should use <scm|tm-define> or
  <scm|tm-define-macro>, or <scm|define-public>, <scm|define-public-macro>
  and <scm|provide-public> (the latter only defines the symbol when it is not
  yet defined). Because of implementation details for the
  <hlink|contextual overloading system|overview-overloading.en.tm>, a symbol
  declared with <scm|tm-define> becomes visible inside all other modules.
  For <scm|define-public>, this depends on the implementation of <scheme>:
  with <name|S7>, the symbol is published for all modules
  (<source-link|boot-s7.scm|TeXmacs/progs/kernel/boot/boot-s7.scm>), while
  with <name|Guile> it is an ordinary export of the module, only visible in
  the modules which <scm|:use> it. Code which works with <name|S7> without a
  <scm|:use> may thus fail with <name|Guile>: always import the modules you
  depend on.

  Because the number of <TeXmacs> modules and plug-ins keeps on growing, it
  is inefficient to load all modules when booting. Instead, initialization
  files are assumed to declare the provided functionality in a <em|lazy> way:
  whenever the functionality is explicitly needed, <TeXmacs> is triggered to
  load the corresponding modules (if this was not already done). In addition,
  <TeXmacs> may load some of these modules during spare time, when the
  computer is waiting for user input. Indeed, this helps increasing the
  reactivity of <TeXmacs> at the first use of the functionality.

  For instance, assume that you defined a large new editing function
  <scm|foo-action> inside the module <scm|(foo-edit)>. Then your
  initialization file <verbatim|init-foo.scm> would typically contain a line

  <\scm-code>
    (lazy-define (foo-edit) foo-action)
  </scm-code>

  Similarly, lazy keyboard shortcuts and menus for <verbatim|foo> might be
  defined using

  <\scm-code>
    (lazy-keyboard (foo-kbd) in-foo-mode?)

    (lazy-menu (foo-menu) foo-menu)
  </scm-code>

  Note that <scm|lazy-define> only creates its stub when the function is not
  yet defined, and that <scm|(lazy-define-force <scm-arg|name>)> loads the
  modules which promised <scm-arg|name>. The initialization files also use
  the following lazy declarations:

  <\itemize>
    <item><scm|(lazy-format <scm-arg|module> <scm-arg|format-1> ...)>: the
    module defines the given file formats and their converters; it is
    loaded when the editor is idle, or when a format is needed;

    <item><scm|(lazy-tmfs-handler <scm-arg|module> <scm-arg|class-1> ...)>:
    the module defines the handlers of <verbatim|tmfs> urls of the given
    classes (see <hlink|the <TeXmacs> file
    system|../api/tmfs/tmfs.en.tm>), loaded at the first access to such a url;

    <item><scm|(lazy-tool <scm-arg|module> <scm-arg|tool-1> ...)>: the module
    defines the given tools of the side panels;

    <item><scm|(lazy-initialize <scm-arg|module> <scm-arg|pred?>)>: the module
    is loaded as soon as the predicate holds, for instance
    <scm|(lazy-initialize (math math-menu) (in-math?))>;

    <item><scm|(lazy-language <scm-arg|module> <scm-arg|language-1> ...)>:
    the module defines the given formal languages.
  </itemize>

  For more concrete examples, we recommend the user to take a look at the
  standard initialization file <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>.

  <label|redefinitions>On the negative side, the mechanism for lazy loading
  has the important consequence that you can no longer make assumptions on
  when a particular module is loaded. For instance, when you attempt to
  redefine a keyboard shortcut in your personal initialization file, it may
  happen that the standard definition is loaded after your \Predefinition\Q.
  In that case, your redefinition remains without consequence.

  For this reason, <TeXmacs> also provides the instruction <scm|import-from>
  to force a particular module to be loaded. Similarly, the commands
  <scm|(lazy-keyboard-force #t)>, <scm|lazy-plugin-force>, <abbr|etc.> may
  be used to force all lazy keyboard definitions <abbr|resp.> plug-ins to be
  loaded (without argument, <scm|lazy-keyboard-force> only loads the
  keyboard definitions of the modes which are currently active).
  In other words, the use of laziness forces to make implicit dependencies
  between modules more explicit.

  In the case when you want to redefine keyboard shortcuts, the
  <hlink|contextual overloading system|overview-overloading.en.tm> gives you
  an even more fine-grained control. The rule is simple: among the
  definitions whose conditions (such as the mode) hold, the most recent one
  wins; there is no ordering by specificity of the modes. For instance,
  assume that the keyboard shortcut <key|x x x> has been defined twice, both
  in general and in math mode. After calling <scm|(lazy-keyboard-force #t)>,
  a new general definition of the shortcut overrides both of them, also in
  math mode. To keep the special behaviour in math mode, define it again
  after your general definition:

  <\scm-code>
    (lazy-keyboard-force #t)

    (kbd-map ("x x x" <em|action>))

    (kbd-map (:mode in-math?) ("x x x" <em|math-action>))
  </scm-code>

  Conversely, a definition loaded after yours, for instance by a module
  which had not been loaded yet, overrides your definition wherever its
  conditions hold.

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