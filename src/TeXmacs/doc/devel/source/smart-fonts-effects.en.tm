<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Font effects>

  <section|The <src-var|font-effects> environment variable>

  The environment variable <src-var|font-effects> (C++ name
  <cpp|FONT_EFFECTS>, declared in <verbatim|Data/Drd/vars.cpp>, default
  value the empty string in <verbatim|Typeset/Env/env_default.cpp>)
  contains a comma separated list of effects of the form
  <verbatim|<em|effect>=<em|value>>. It belongs to the font variables
  (<cpp|Env_Font>), so changing it triggers
  <cpp|edit_env_rep::update_font>, which applies the effects on top of the
  smart font:

  <\cpp-code>
    string eff= get_string (FONT_EFFECTS);

    if (N(eff) != 0) fn= apply_effects (fn, eff);
  </cpp-code>

  The effects are therefore applied to the whole smart font, including all
  its subfonts (fallback fonts, virtual and emulated symbols). The user
  level documentation of the variable is in the <hlink|font environment
  variables|../format/environment/env-font.en.tm>.

  At the markup level, the effects are used by the macros of
  <verbatim|packages/standard/std-markup.ts>, which append an effect to the
  current list with <markup|add-font-effect>:

  <\verbatim-code>
    \<less\>assign\|add-font-effect\|\<less\>macro\|prop\|val\|body\|\<less\>with\|font-effects\|\<less\>merge\|\<less\>value\|font-effects\<gtr\>\|\<less\>if\|\<less\>equal\|\<less\>value\|font-effects\<gtr\>\|\<gtr\>\|\|,\<gtr\>\|\<less\>arg\|prop\<gtr\>\|=\|\<less\>arg\|val\<gtr\>\<gtr\>\|\<less\>arg\|body\<gtr\>\<gtr\>\<gtr\>\<gtr\>

    \<less\>assign\|embold\|\<less\>macro\|body\|\<less\>add-font-effect\|bold\|\<less\>value\|embold-strength\<gtr\>\|\<less\>arg\|body\<gtr\>\<gtr\>\<gtr\>\<gtr\>
  </verbatim-code>

  The macros <markup|embold>, <markup|embbb>, <markup|slanted>,
  <markup|hmagnified>, <markup|vmagnified>, <markup|condensed>,
  <markup|extended>, <markup|monospaced>, <markup|degraded>,
  <markup|distorted>, <markup|gnawed>, <markup|blurred> and
  <markup|enhanced> take their parameters from style variables such as
  <src-var|embold-strength> (2), <src-var|slanted-slope> (0.25) or
  <src-var|condensed-factor> (0.8). Most of them are available from the
  <menu|Font effects> submenus of the text properties menus
  (<scm|text-font-effects-menu> in <verbatim|progs/generic/format-menu.scm>).

  <section|<cpp|apply_effects>>

  <\explain>
    <cpp|font apply_effects (font fn, string effects)><explain-synopsis|wrap
    a font with effects>
  <|explain>
    Splits <src-arg|effects> at the commas and each item at the equal sign,
    and successively wraps <src-arg|fn> with the corresponding emulated
    font. Unknown effects and items without a value are ignored. The order
    of the list is the order of application.
  </explain>

  The recognized effects, with the clamping of their values, are
  (<verbatim|Graphics/Fonts/smart_font.cpp>):

  <\description>
    <item*|<verbatim|bold=<em|e>>><math|e\<in\>[1,5]>.
    <cpp|poor_bold_font (fn, fat, fat)> with
    <math|fat=(e-1)\<cdot\>wline/wfn>.

    <item*|<verbatim|bbb=<em|e>>><math|e\<in\>[1,5]>.
    <cpp|poor_bbb_font (fn, penw, penh, fat)> with pen dimensions
    <math|wline/wfn> and <math|fat=(e-1)\<cdot\>wline/wfn>.

    <item*|<verbatim|slant=<em|s>>><math|s\<in\>[-2,2]>.
    <cpp|poor_italic_font (fn, s)>.

    <item*|<verbatim|hmagnify=<em|f>>, <verbatim|vmagnify=<em|f>>><math|f\<in\>[0.1,10]>.
    <cpp|poor_stretched_font> horizontally or vertically.

    <item*|<verbatim|hextended=<em|f>>><math|f\<in\>[0.1,10]>.
    <cpp|poor_extended_font (fn, f)> (stroke preserving; used for both
    <markup|condensed> and <markup|extended>). A <verbatim|vextended>
    effect is present in the code but commented out, so it is currently
    ignored.

    <item*|<verbatim|mono=<em|f>>><math|f\<in\>[0.1,10]>.
    <cpp|poor_mono_font (fn, f, f)>.

    <item*|<verbatim|degraded=<em|threshold>;<em|frequency>>>Defaults 0.666
    and 1.0, clamped to <math|[0.01,0.99]> and <math|[0.1,10]>.
    <cpp|poor_distorted_font> with kind <verbatim|(degraded ...)>.

    <item*|<verbatim|distorted=<em|strength>;<em|frequency>>,
    <verbatim|gnawed=<em|strength>;<em|frequency>>>Defaults 1.0 and
    1.0, strength clamped to <math|[0.1,9.9]>. <cpp|poor_distorted_font>
    with kind <verbatim|(distorted ...)> resp. <verbatim|(gnawed ...)>.

    <item*|<verbatim|blurred=<em|radius>>>A length; if its unit is
    <verbatim|pt> it is divided by the font size, otherwise the number is
    taken as a fraction of the font size; clamped to <math|[0.01,1]>.
    <cpp|poor_effected_font> with kind <verbatim|(blurred <em|r>)>.

    <item*|<verbatim|enhanced=<em|radius>;<em|shadow>;<em|sunny>>>An
    engraved look: two blurred copies of the font, shifted diagonally in
    opposite directions and recolored with the colors <em|shadow> (default
    black) and <em|sunny> (default white), are superposed under the
    original font:

    <\cpp-code>
      font fn1= recolored_font (blurred1, shadow);

      ...

      font fn2= recolored_font (blurred2, sunny);

      array\<less\>font\<gtr\> a; a \<less\>\<less\> fn1 \<less\>\<less\> fn2 \<less\>\<less\> fn;

      fn= superposed_font (a, 2);
    </cpp-code>
  </description>

  Parameters inside one effect are separated by semicolons, since the comma
  separates the effects. The values are parsed by the static helpers
  <cpp|get_double_parameter>, <cpp|get_string_parameter> and
  <cpp|get_length_parameter>.

  <section|Interaction with zooming and rendering>

  Each effect font implements <cpp|magnify> by applying the same effect to
  the magnified base font, so that the effects survive the zooming in
  <cpp|font_rep::draw>. The parameters are expressed relative to the
  design size (<cpp|wfn>), hence the appearance does not depend on the
  zoom.

  As explained for <hlink|emulated fonts|smart-fonts-emulated.en.tm>,
  slanting, stretching and monospacing are drawn with renderer
  transformations on printers, whereas bold, blackboard bold, extended,
  distorted and blurred glyphs are bitmaps. On the screen all of them use
  transformed glyph tables through <cpp|index_glyph>, except monospacing.

  <section|Adding a new effect>

  <\enumerate>
    <item>Implement the glyph transformation, typically as a function on
    <cpp|glyph> and on <cpp|font_glyphs> (and on <cpp|font_metric> if the
    metrics change) in <verbatim|Graphics/Bitmap_fonts/>, and declare it in
    <verbatim|bitmap_font.hpp>.

    <item>Either add a new kind to an existing wrapper
    (<cpp|poor_distorted_font_rep> and <cpp|poor_effected_font_rep> take a
    <cpp|tree> describing the effect, so adding a new tuple there is the
    least intrusive way), or write a new <verbatim|poor_<em|name>.cpp> by
    copying the simplest existing wrapper (<verbatim|poor_distorted.cpp>):
    delegate everything to <cpp|base>, transform the tables in
    <cpp|index_glyph> and <cpp|get_glyph>, and draw with
    <cpp|ren-\<gtr\>draw (c, fng, x, y)>. Give the font a unique resource
    name which includes all parameters, and implement <cpp|magnify>.
    Declare the constructor in <verbatim|Graphics/Fonts/font.hpp>.

    <item>Add a branch to <cpp|apply_effects>, with clamping of the
    parameters.

    <item>Optionally add a macro in <verbatim|std-markup.ts> based on
    <markup|add-font-effect>, an entry in <scm|text-font-effects-menu>, and
    document the effect in <verbatim|doc/devel/format/environment/env-font.en.tm>.
  </enumerate>

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
