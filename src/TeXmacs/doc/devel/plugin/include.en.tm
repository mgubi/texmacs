<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Supporting your system inside <TeXmacs>>

  Assume that you have successfully written a first interface with <TeXmacs>
  as explained in the previous section. Then it is time now to include
  support for your system in the standard <TeXmacs> distribution, after
  which further improvements can be made.

  It is quite easy to adapt your interface in such a way that it can be
  directly integrated into <TeXmacs>. The idea is to create a directory

  <\verbatim-code>
    $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim-code>

  where <verbatim|myplugin> is the name of your plug-in. Here we recall
  that <verbatim|$TEXMACS_HOME_PATH> is bound to <verbatim|~/.TeXmacs>, by
  default. In the directory <verbatim|$TEXMACS_PATH/plugins> you can find
  all standard plug-ins, which are shipped with your <TeXmacs> distribution
  (in the source code of <TeXmacs>, they can be found in
  <verbatim|src/plugins>). These provide good examples which you may
  imitate.

  The above <verbatim|myplugin> directory should contain a similar
  subdirectory structure as <verbatim|$TEXMACS_PATH> itself, but you may
  omit directories which you do not actually use. In any case, you need to
  provide a file <verbatim|progs/init-myplugin.scm> which describes how to
  initialize your plug-in. Usually, this file contains a <scheme>
  instruction of the following form:

  <\scm-code>
    (plugin-configure myplugin

    \ \ (:require (url-exists-in-path? "myplugin"))

    \ \ (:launch "myplugin --texmacs")

    \ \ (:session "Myplugin"))
  </scm-code>

  The first instruction is a predicate which checks whether your plug-in
  can be used on a particular system. Usually, it tests whether a certain
  program is available in the path. The remainder of the instructions is
  only executed if the requirement is fulfilled. The <scm|:launch>
  instruction specifies that your plug-in should be launched using the
  given shell command, which is usually of the form <verbatim|myplugin
  --texmacs>. The <scm|:session> instruction makes shell sessions available
  for your plug-in from the menu <menu|Insert|Session|Myplugin>.

  By default, the input is sent to your program as a single line of plain
  text, and the output of your program is interpreted according to the
  formats specified in its <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks
  (<verbatim|verbatim>, <verbatim|latex>, <verbatim|scheme>,
  <verbatim|html>, <verbatim|ps> and others). The input format can be
  customized using the <scm|:serializer> option and the mathematical input
  converters; notice that an option <verbatim|:format> for specifying the
  input and output formats, which existed in early versions of <TeXmacs>,
  is no longer supported. The complete list of options is given in the
  <hlink|summary of configuration options|plugin-config.en.tm>.

  If everything works well, and you wish to make it possible for others to
  use your system inside the official <TeXmacs> distribution, then contact
  the <TeXmacs> developers.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
