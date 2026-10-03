<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Foundations: data types, strings, <scheme> and the system layer>

  These chapters describe the layers on which everything else is built:
  the basic data types of the kernel (reference counted strings, arrays,
  lists, hash maps and, above all, trees and paths), the handling of
  characters and encodings, the embedded <scheme> interpreter with the glue
  which exports <c++> routines to it, and the system layer, which deals
  with files, <abbr|URL>s, caches, the boot sequence and the differences
  between platforms. A developer who works on any other part of the
  program needs at least the first chapter.

  <\traverse>
    <branch|Basic data types|types.en.tm>

    <branch|Strings, characters and encodings|strings.en.tm>

    <branch|The <scheme> interpreter and the <c++>/<scheme>
    glue|scheme-bridge.en.tm>

    <branch|The system layer: files, URLs, caches and platform
    support|system.en.tm>
  </traverse>

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
