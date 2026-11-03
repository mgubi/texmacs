<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Layout.> <verbatim|hlist> and <verbatim|vlist> put items side by side or one below the other; <verbatim|glue> (<verbatim|hext>, <verbatim|vext>, width, height) adds space which may stretch:

  <hlist|<action-button*|Left|(gui-message "Left")>|<glue|true|false|0px|0px>|<action-button*|Right|(gui-message "Right")>>

  <verbatim|tiled> places its items on a grid of the given number of columns:

  <tiled|3|<action-button*|1|(gui-message "1")>|<action-button*|2|(gui-message "2")>|<action-button*|3|(gui-message "3")>|<action-button*|4|(gui-message "4")>|<action-button*|5|(gui-message "5")>|<action-button*|6|(gui-message "6")>>

  <verbatim|align-tiled> aligns labels and their fields in two columns:

  <align-tiled|2|Width:|<input-field|string|(gui-message "width " answer)|6em|10cm>|Height:|<input-field|string|(gui-message "height " answer)|6em|5cm>>

  Styles of text:

  <title-style|A title>

  <section-style|A section>

  <subsection-style|A subsection>

  <plain-style|Plain text>

  <discrete-style|Discrete text>

</body>

<initial|<\collection>
</collection>>
