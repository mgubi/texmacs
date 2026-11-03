<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Miscellaneous style-sheet primitives>

  <\explain>
    <explain-macro|extern|scheme-foo|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|apply
    extern typesetting macro>
  <|explain>
    This primitive allows the user to implement macros in <scheme>. The
    primitive applies the <scheme> function or macro <src-arg|scheme-foo> (a
    string, which is evaluated) to the arguments <src-arg|arg-1> until
    <src-arg|arg-n>. The result of the <scheme> function is converted back
    into a <TeXmacs> tree, which is evaluated and typeset. For instance, the
    code <inactive*|<extern|(lambda (name) `(concat "hi " ,name))|dude>>
    yields ``<extern|(lambda (name) `(concat "hi " ,name))|dude>''.

    The arguments <src-arg|arg-1> until <src-arg|arg-n> are evaluated and
    then passed as trees to <src-arg|scheme-foo>. When defining a macro which
    relies on extern scheme code, it is therefore recommended to pass the
    macro arguments using the <markup|quote-arg> primitive:

    <\tm-fragment>
      <inactive*|<assign|inc-div|<macro|x|y|<style-with|src-compact|none|<extern|(lambda
      (x y) `(frac ,x (concat "1+" ,y)))|<quote-arg|x>|<quote-arg|y>>>>>>
    </tm-fragment>

    It has been foreseen that the accessibility of the macro arguments
    <src-arg|x> and <src-arg|y> is preserved for this kind of definitions.
    However, since <TeXmacs> does not heuristically analyze your <scheme>
    code, you will have to manually set the <abbr|D.R.D.> properties using
    <markup|drd-props>.

    Notice also that the <scheme> function <src-arg|scheme-foo> should only
    rely on secure scheme functions (and not on functions like
    <verbatim|system> which may erase your hard disk). User implemented
    <scheme> functions in plug-ins may be defined to be secure using the
    <verbatim|:secure> option. Alternatively, the user may define all
    <scheme> routines to be secure in <menu|Edit|Preferences|Security|Accept
    all scripts>.
  </explain>

  <\explain>
    <explain-macro|use-package|package-1|<math|\<cdots\>>|package-n><explain-synopsis|load
    style packages>
  <|explain>
    Load the style packages <src-arg|package-1> until <src-arg|package-n>
    and execute their definitions in the current environment. The package
    names are literal strings; they are looked up (with the suffix
    <verbatim|.ts>) in the style path and in the directory of the current
    document. This primitive is mainly used in style files, in order to
    build upon other packages.
  </explain>

  <\explain>
    <explain-macro|use-module|module-1|<math|\<cdots\>>|module-n><explain-synopsis|load
    <scheme> modules>
  <|explain>
    Load <scheme> modules on which a style file depends. Each argument is
    evaluated and is either a module name like <verbatim|(dynamic
    fold-edit)>, which is loaded with <scm|use-modules>, or the name of a
    plug-in, which is initialized. The loaded modules are recorded in the
    <src-var|the-modules> environment variable.
  </explain>

  <\explain>
    <explain-macro|with-package|package|body><explain-synopsis|local
    package>
  <|explain>
    Typeset <src-arg|body> in an environment in which the definitions of
    the style <src-arg|package> have been loaded. Contrary to
    <markup|use-package>, the definitions are local to <src-arg|body>: the
    tag is rewritten into a <markup|with> which sets all variables assigned
    at the top level of the package (only plain <markup|assign> statements
    are taken into account).
  </explain>

  <\explain>
    <explain-macro|write|aux|content><explain-synopsis|write auxiliary
    information>
  <|explain>
    Adds <src-arg|content> to the auxiliary section <src-arg|aux> of the
    document. This tag is used for instance by <inactive*|<nocite|citekey>>
    to add entries to the automatically generated bibliography at the end of
    the document, without inserting a citation in the text.
  </explain>

  <\explain>
    <explain-macro|flag|content|color>

    <explain-macro|flag|content|color|var><explain-synopsis|display an
    informative flag>
  <|explain>
    This tag is used to in order to inform the user about information which
    is present in the document, but not visible when printed out. <TeXmacs>
    displays such informative flags for labels, formatting directives such as
    page breaks, and so on. In <menu|Document|Informative flags>, the user
    may specify how the informative flags should be rendered.

    The two-argument variant displays an informative flag with a given
    <src-arg|content> and <src-arg|color>. The <src-arg|content> is only
    rendered when selecting <menu|Document|Informative flags|Detailed>. For
    instance, <inactive*|<flag|warning|red>> is rendered as
    <flag|warning|red>. The optional <src-arg|var> argument may be used in
    order to specify that the flag should only be visible if the macro
    argument <src-arg|var> corresponds to an accessible part of the document.
    For instance, <TeXmacs> automatically generates labels for section titles
    (so as to include them in the table of contents), but it is undesirable
    to display informative flags for such labels.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>