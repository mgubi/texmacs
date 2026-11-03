<TeXmacs|1.0.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Organization of style files and packages>

  Each standard <TeXmacs> style file or package is based on a potentially
  finite number of subpackages. From an abstract point of view, this
  organization may be represented by a labeled tree. For instance, the tree
  which corresponds to the <tmstyle|article> style is represented below:

  <\big-figure|<tree|<tmstyle|article>|<tree|<tmpackage|std>|<stack|<tformat|<table|<row|<cell|<tmpackage|std-counter>>>|<row|<cell|<tmpackage|std-markup>>>|<row|<cell|<tmpackage|std-utils>>>|<row|<cell|<tmpackage|std-symbol>>>|<row|<cell|<tmpackage|std-math>>>|<row|<cell|<tmpackage|std-list>>>|<row|<cell|<tmpackage|std-automatic>>>|<row|<cell|<tmpackage|std-pattern>>>|<row|<cell|<tmpackage|std-fold>>>|<row|<cell|<tmpackage|std-graphics>>>|<row|<cell|<tmpackage|std-security>>>|<row|<cell|<tmpackage|std-animate>>>|<row|<cell|<tmpackage|session>>>|<row|<cell|<tmpackage|scripts>>>|<row|<cell|<tmpackage|calc>>>|<row|<cell|<tmpackage|program>>>>>>>|<tree|<tmpackage|env>|<stack|<tformat|<table|<row|<cell|<tmpackage|env-base>>>|<row|<cell|<tmpackage|env-math>>>|<row|<cell|<tmpackage|env-theorem>>>|<row|<cell|<tmpackage|env-float>>>|<row|<cell|<tmpackage|env-program>>>>>>>|<tree|<tmpackage|title-generic>|<tmpackage|title-base>>|<tmpackage|header-article>|<tree|<tmpackage|section-article>|<tmpackage|section-base>>>>
    The tree with the packages from which the <tmstyle|article> style has
    been built up. In order to save space, we have regrouped the numerous
    children of <tmpackage|std> and <tmpackage|env> in vertical lists.
  </big-figure>

  Most of the style packages correspond to a <abbr|d.t.d.> (data type
  definition) which contains the ``abstract interface'' of the package,
  <abbr|i.e.> the exported tags. For instance, the package
  <tmpackage|std-markup> corresponds to the <abbr|d.t.d.> <tmdtd|std-markup>.
  Sometimes however, several style packages match the same <abbr|d.t.d.>. For
  instance, both <tmpackage|header-article> and <tmpackage|header-book> match
  the <abbr|d.t.d.> <tmdtd|header>, since they merely implement different
  ways to render the same tags.

  When building your own style files or packages, you may use the
  <markup|use-package> primitive in order to include other packages. For
  instance, the <tmstyle|article> style essentially consists of the line

  <\tm-fragment>
    <inactive*|<use-package|std|env|title-generic|header-article|section-article>>
  </tm-fragment>

  More precisely, the <markup|use-package> primitive sequentially includes
  the style packages corresponding to its arguments. The packages are
  searched in the directory of the current file and in
  <verbatim|$TEXMACS_STYLE_PATH>, which by default contains all
  subdirectories of <verbatim|~/.TeXmacs/styles>,
  <verbatim|~/.TeXmacs/packages>, <verbatim|$TEXMACS_PATH/styles>,
  <verbatim|$TEXMACS_PATH/packages> and of the <verbatim|styles> and
  <verbatim|packages> directories of the installed plug-ins. Furthermore
  rendering information for the source code like <markup|style-with> tags
  are discarded before evaluation of the files. Similarly, the
  <markup|use-module> primitive may be used to load a <scheme> module on
  which the package depends, as in <inactive*|<use-module|(database
  title-markup)>>.

  <\remark>
    We strongly recommend the user to take a look at some of the standard
    style files and packages which can be found in

    <\verbatim>
      \ \ \ \ $TEXMACS_PATH/styles

      \ \ \ \ $TEXMACS_PATH/packages
    </verbatim>

    When loading using <shortcut|(interactive load-buffer)>, these paths are in the standard load
    path. For instance, if you want to take a look at the
    <tmpackage|std-markup> package, then it suffices to type <shortcut|(interactive load-buffer)>,
    followed by the file name <source-link|std-markup.ts|TeXmacs/packages/standard/std-markup.ts> and
    <shortcut|(kbd-return)>.
  </remark>

  <\remark>
    It is also possible to customize the presentation of the source code of
    the style files and packages themselves, by using other packages in
    addition to <tmstyle|source> or by using another major style file based
    on <tmstyle|source>. In that case, the extra markup provided by such
    packages may be used for presentation purposes of the source code, but it
    is not exported when using your package in another file.
  </remark>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>