<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Adding converters>

  New data formats and converters between them are declared using the
  macros <scm|define-format> and <scm|converter>, which are defined in
  <source-link|kernel/texmacs/tm-convert.scm|TeXmacs/progs/kernel/texmacs/tm-convert.scm>. <TeXmacs> maintains a graph of
  all declared converters and automatically combines them when no direct
  converter exists between two formats. Formats which can be converted from
  or into <TeXmacs> automatically appear in the <menu|File|Import> and
  <menu|File|Export> menus. Typical examples of declarations can be found in
  the files <verbatim|convert/*/init-*.scm>, such as
  <source-link|convert/html/init-html.scm|TeXmacs/progs/convert/html/init-html.scm>. The internals of the <LaTeX>
  converters are described in <hlink|the section on
  conversions|../../source/conversions.en.tm>.

  <paragraph|Formats>

  Each format <scm-arg|fm> gives rise to several representations, which are
  distinguished by suffixes: <verbatim|<scm-arg|fm>-file> (a <abbr|URL>),
  <verbatim|<scm-arg|fm>-document> (a string with the complete document),
  <verbatim|<scm-arg|fm>-snippet> (a string with a document fragment) and
  <verbatim|<scm-arg|fm>-stree> (a parsed <scheme> tree). For <TeXmacs>
  itself, the relevant formats are <verbatim|texmacs-file>,
  <verbatim|texmacs-document>, <verbatim|texmacs-snippet>,
  <verbatim|texmacs-stree> and <verbatim|texmacs-tree>.

  <\explain>
    <scm|(define-format <scm-arg|name> <scm-arg|option> ...)><explain-synopsis|declare
    a new data format>
  <|explain>
    Declare a new format <scm-arg|name>, together with converters between
    <verbatim|<scm-arg|name>-file> and <verbatim|<scm-arg|name>-document>.
    The following options are supported:

    <\description>
      <item*|<scm|(:name <scm-arg|string>)>>The name of the format as it
      appears in menus.

      <item*|<scm|(:suffix <scm-arg|suffix> ...)>>The file suffixes of the
      format; the first one is the default suffix.

      <item*|<scm|(:recognize <scm-arg|pred?>)>>A predicate on strings which
      recognizes documents in this format (used when the suffix does not
      determine the format). With the variant <scm|:must-recognize>, a
      matching suffix is not sufficient: files which are not recognized by
      the predicate are opened as verbatim text.

      <item*|<scm|(:hidden)>>Do not show the format in menus.
    </description>

    Other options (such as <scm|:option>, which occurs in some format
    declarations) are currently ignored by <scm|define-format>; options
    should be attached to converters instead.
  </explain>

  <paragraph|Converters>

  <\explain>
    <scm|(converter <scm-arg|from> <scm-arg|to> <scm-arg|option>
    ...)><explain-synopsis|declare a converter>
  <|explain>
    Declare a converter from the format <scm-arg|from> to the format
    <scm-arg|to> (both given as symbols or strings with the suffixes
    mentioned above). The following options are supported:

    <\description>
      <item*|<scm|(:function <scm-arg|fun>)>>The conversion is done by
      applying <scm-arg|fun> to the input.

      <item*|<scm|(:function-with-options
      <scm-arg|fun>)>><scm-arg|fun> takes a second argument: an association
      list of conversion options.

      <item*|<scm|(:shell <scm-arg|prog> <scm-arg|arg> ...)>>The conversion
      is done by an external program <scm-arg|prog>. The special arguments
      <scm|from> and <scm|to> are replaced by the names of the input and
      output files. The converter is removed if <scm-arg|prog> cannot be
      found in the path.

      <item*|<scm|(:require <scm-arg|cond>)>>The converter is only defined
      if <scm-arg|cond> evaluates to true. This option (possibly preceded by
      <scm|:penalty>) must come first. It allows for alternative
      implementations of the same converter depending on the availability
      of external tools: the last valid declaration is retained.

      <item*|<scm|(:penalty <scm-arg|x>)>>The cost of the converter (by
      default <math|1.0>), which is used when searching for the cheapest
      chain of converters.

      <item*|<scm|(:option <scm-arg|name> <scm-arg|default>)>>Declare an
      option of the converter, which is stored as a user preference
      <scm-arg|name> with the given <scm-arg|default> value and passed to
      the converter function in the option list.
    </description>

    For instance:

    <\scm-code>
      (define-format blablah

      \ \ (:name "Blablah")

      \ \ (:suffix "bla"))

      \;

      (converter blablah-file latex-file

      \ \ (:require (url-exists-in-path? "bla2tex"))

      \ \ (:shell "bla2tex" from "\<gtr\>" to))
    </scm-code>
  </explain>

  <paragraph|Using converters>

  <\explain>
    <scm|(convert <scm-arg|what> <scm-arg|from> <scm-arg|to> <scm-arg|option>
    ...)><explain-synopsis|convert data>
  <|explain>
    Convert <scm-arg|what> from the format <scm-arg|from> to the format
    <scm-arg|to>, using the cheapest chain of declared converters. Returns
    <scm|#f> if no such chain exists. For instance, <scm|(convert
    "\<less\>b\<gtr\>x\<less\>/b\<gtr\>" "html-snippet" "texmacs-stree")>
    parses a piece of <name|Html>. The variant <scm|(convert-to-file
    <scm-arg|what> <scm-arg|from> <scm-arg|to> <scm-arg|dest>)> writes the
    result to the file <scm-arg|dest>.
  </explain>

  <\explain>
    <scm|(converter-search <scm-arg|from> <scm-arg|to>)>

    <scm|(converters-from <scm-arg|from> ...)>

    <scm|(converters-to <scm-arg|to> ...)><explain-synopsis|inspect the
    converter graph>
  <|explain>
    Return the cheapest chain of formats leading from <scm-arg|from> to
    <scm-arg|to> (or <scm|#f>), <abbr|resp.> the lists of formats which can
    be reached from, or converted to, the given formats.
  </explain>

  <\explain>
    <scm|(format-from-suffix <scm-arg|suffix>)>

    <scm|(format-get-name <scm-arg|fm>)>

    <scm|(format-default-suffix <scm-arg|fm>)><explain-synopsis|information
    about formats>
  <|explain>
    Determine the format corresponding to a file suffix, the name of a
    format as shown in menus, <abbr|resp.> the default suffix of a format.
  </explain>

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
