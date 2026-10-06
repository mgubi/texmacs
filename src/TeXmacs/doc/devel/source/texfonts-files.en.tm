<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Finding and generating <TeX> font files>

  This page describes how <TeXmacs> locates the files of a <TeX> font and how
  it generates missing ones with the tools of a <TeX> distribution. The code
  is in <source-link|Plugins/Metafont/tex_init.cpp|src/Plugins/Metafont/tex_init.cpp>,
  <source-link|Plugins/Metafont/tex_files.cpp|src/Plugins/Metafont/tex_files.cpp> and, for the error cache,
  <source-link|Plugins/Metafont/load_tex.cpp|src/Plugins/Metafont/load_tex.cpp>.

  <section|Settings detected at the first run>

  When <TeXmacs> sets up a new home directory (<cpp|setup_texmacs> in
  <source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>, run when
  <verbatim|$TEXMACS_HOME_PATH/system/settings.scm> does not exist, and
  after <verbatim|-setup>), it calls <cpp|setup_tex>, which removes
  <verbatim|$TEXMACS_HOME_PATH/fonts/font-index.scm> and records the
  following values in the settings (<cpp|set_setting>), which are saved in
  <verbatim|settings.scm>:

  <\description>
    <item*|<verbatim|KPSEPATH>, <verbatim|KPSEWHICH>,
    <verbatim|TEXHASH>><verbatim|"true"> or <verbatim|"false">, according to
    whether the programs <verbatim|kpsepath>, <verbatim|kpsewhich> and
    <verbatim|texhash> are in the <verbatim|PATH>
    (<cpp|init_helper_binaries>).

    <item*|<verbatim|MAKETFM>>The program used to generate missing metrics:
    the first of <verbatim|mktextfm>, <verbatim|MakeTeXTFM> and
    <verbatim|maketfm> which is found, or <verbatim|"false">.

    <item*|<verbatim|MAKEPK>>Likewise for bitmap glyphs: <verbatim|mktexpk>,
    <verbatim|MakeTeXPK>, <verbatim|makepk> or <verbatim|"false">.

    <item*|<verbatim|DPI>>Always <verbatim|"600">; it is passed to the
    <name|PK> generator as the base resolution of the <name|Metafont> mode.

    <item*|<verbatim|TFM>, <verbatim|PK>, <verbatim|PFB>>Search paths
    obtained by looking for <verbatim|tfm>, <verbatim|pk> and
    <verbatim|pfb>/<verbatim|type1> subdirectories of a few traditional
    <TeX> font directories (<verbatim|/usr/share/texmf/fonts>,
    <verbatim|/usr/lib/texmf/fonts>, <verbatim|/var/texfonts>,
    <verbatim|/opt/local/share/texmf-texlive-dist/fonts>, ...; on
    <name|Windows>, the subdirectories of <verbatim|$TEX_HOME/fonts>), see
    <cpp|init_heuristic_tex_paths>.
  </description>

  These values are not detected again at later runs. Installing a <TeX>
  distribution after the first run of <TeXmacs> therefore has no effect on
  font generation until the settings are reset (for instance with
  <verbatim|texmacs -setup>, see <hlink|the main program|server-startup.en.tm>).

  <section|Search paths>

  At every start, after the settings have been loaded, <cpp|init_tex> calls
  <cpp|reset_tfm_path>, <cpp|reset_pk_path> and <cpp|reset_pfb_path>, which
  build the three static search paths of <source-link|tex_files.cpp|src/Plugins/Metafont/tex_files.cpp>. For the
  metrics, the path is, in this order,

  <\enumerate>
    <item>the current directory;

    <item>all subdirectories of <verbatim|$TEXMACS_HOME_PATH/fonts/tfm>
    (where generated metrics are stored);

    <item>all subdirectories of <verbatim|$TEXMACS_PATH/fonts/tfm> (the
    metrics shipped with <TeXmacs>);

    <item>the environment variable <verbatim|$TEX_TFM_PATH>;

    <item>the <verbatim|TFM> setting;

    <item>if fonts may be generated (<verbatim|MAKETFM> is not
    <verbatim|"false">, or <verbatim|TEXHASH> is <verbatim|"true">) and
    <verbatim|kpsewhich> is <em|not> available, the directories printed by
    <verbatim|kpsepath tfm>, if the <verbatim|KPSEPATH> setting allows it.
  </enumerate>

  The <name|PK> path is built in the same way from
  <verbatim|fonts/pk>, <verbatim|$TEX_PK_PATH>, the <verbatim|PK> setting and
  <verbatim|kpsepath pk>. The <name|Type 1> path uses
  <source-link|fonts/type1|TeXmacs/fonts/type1>, <verbatim|$TEX_PFB_PATH> and the <verbatim|PFB>
  setting, without <verbatim|kpsepath>.

  <section|Looking up a file>

  <cpp|resolve_tex (url name)> finds a file by name; the kind of file is
  given by its suffix:

  <\enumerate>
    <item>If the name is in the persistent cache <verbatim|font_cache.scm>
    (see <hlink|the system layer|system-files.en.tm>) and the cached file
    still exists, it is returned; a stale entry is removed.

    <item>Names ending in <verbatim|tfm> or <verbatim|mf> are looked up in
    the metric path (on <name|Windows>, a <verbatim|.mf> name is also tried
    with the suffix <verbatim|.tfm>), names ending in <verbatim|pk> in the
    <name|PK> path and names ending in <verbatim|pfb> in the <name|Type 1>
    path.

    <item>If this fails and the <verbatim|KPSEWHICH> setting is
    <verbatim|"true">, the name is passed to <verbatim|kpsewhich> (except
    for <name|PK> and <name|Type 1> names on <name|Windows>, where the
    <name|MiKTeX> version of <verbatim|kpsewhich> is considered unreliable
    for these files).

    <item>A result which was found is stored in <verbatim|font_cache.scm>.
    Failures are not cached here.
  </enumerate>

  <cpp|exists_in_tex (u)> is <cpp|!is_none (resolve_tex (u))>. The
  <name|Type 1> files are usually not located through <cpp|resolve_tex>
  directly, but through <cpp|tt_font_find> (<source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>),
  which tries <verbatim|<em|name>.pfb> first (with <cpp|resolve_tex>) and then
  the <name|TrueType> and <name|OpenType> suffixes; see
  <hlink|Type 1 substitution|texfonts-formats.en.tm>. Unlike
  <cpp|resolve_tex>, <cpp|tt_font_find> also caches failures, as an empty
  entry <verbatim|ttf:<em|name>> in <verbatim|font_cache.scm>.

  <section|Generating missing files>

  <cpp|make_tex_tfm (name)> runs the program given by the <verbatim|MAKETFM>
  setting:

  <\description>
    <item*|<verbatim|mktextfm>><verbatim|mktextfm --destdir
    $TEXMACS_HOME_PATH/fonts/tfm <em|name>>, after which the
    <verbatim|<em|name>.600pk> file which <verbatim|mktextfm> creates as a
    side effect is removed again.

    <item*|<verbatim|MakeTeXTFM>><verbatim|MakeTeXTFM <em|name>>, which
    stores the result in the <TeX> tree.

    <item*|<verbatim|maketfm>>The <name|Windows> tool, called with
    <verbatim|--dest-dir> and the name without its suffix.
  </description>

  <cpp|make_tex_pk (name, dpi, design_dpi)> runs the <verbatim|MAKEPK>
  program; with <verbatim|mktexpk> the command is

  <\verbatim-code>
    mktexpk --dpi <em|dpi> --bdpi <em|design_dpi> --mag <em|dpi>/<em|design_dpi>

    \ \ \ \ \ \ \ \ --destdir $TEXMACS_HOME_PATH/fonts/pk <em|name>
  </verbatim-code>

  where <em|design_dpi> is the <verbatim|DPI> setting. Both functions run
  the command with <cpp|system>, print a message if it fails, and are
  called between <cpp|system_wait ("Generating font file", <em|name>)> and
  <cpp|system_wait ("")>, which show a \Pplease wait\Q indicator. After a
  successful generation, the caller looks the file up again, and if it is
  still not found, it rebuilds the search path (<cpp|reset_tfm_path> or
  <cpp|reset_pk_path>) and tries once more, since the generator may have
  created a new directory.

  <section|The error cache>

  Generating a font is slow and, when the font does not exist at all,
  pointless. <source-link|load_tex.cpp|src/Plugins/Metafont/load_tex.cpp> therefore records failures as empty
  marker files <verbatim|$TEXMACS_HOME_PATH/fonts/error/<em|file-name>>:

  <\itemize>
    <item>Before generating a metric or a <name|PK> file, <cpp|try_tfm> and
    <cpp|try_pk> check for such a marker and give up immediately if it
    exists.

    <item>A metric marker is written when generation was attempted and the
    file could still not be found.

    <item>A <name|PK> marker is written whenever the file could not be
    found, also when generation is disabled (<verbatim|MAKEPK> is
    <verbatim|"false">).
  </itemize>

  The markers are removed by the options <verbatim|-setup> and
  <verbatim|-delete-font-cache>, by the reset which follows a crash during
  startup (the boot lock), by an upgrade to a new version
  (<cpp|init_upgrade>), on <name|macOS> when <TeXmacs> is started with the
  <key|Alt> key pressed, and automatically when the directories
  <source-link|fonts/type1|TeXmacs/fonts/type1> or <source-link|fonts/truetype|TeXmacs/fonts/truetype> of
  <verbatim|$TEXMACS_PATH> or <verbatim|$TEXMACS_HOME_PATH> have changed
  (<source-link|System/Misc/data_cache.cpp|src/System/Misc/data_cache.cpp>).

  <section|What the cache remembers>

  Besides file locations, <verbatim|font_cache.scm> contains three kinds
  of entries related to <TeX> fonts:

  <\description>
    <item*|<verbatim|tfm:<em|family><em|size>>>The size of the metric which
    was actually used for a request at another size (for instance the
    design size 10 for a request at a size which has no metric of its own, see
    <hlink|the size search|texfonts-formats.en.tm>), so that the next
    request goes straight to that file.

    <item*|<verbatim|tt:<em|family><em|size>>>The name of the <name|Type 1>
    (or <name|TrueType>) font chosen for a family and size
    (<cpp|tt_find_name>).

    <item*|<verbatim|ttf:<em|name>>>The file of an outline font, or the
    empty string if there is none.
  </description>

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
