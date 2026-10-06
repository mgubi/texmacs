<TeXmacs|2.1.4>

<style|<tuple|generic|gui-bright>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Themes.> The same widget with the bright theme (<verbatim|gui-bright>); the preference <verbatim|gui theme> chooses the theme of the dialogs.

  <top-widget|<vlist|<title-style|Settings>|<align-tiled|2|Size:|<input-field|string|(gui-message "size " answer)|6em|10pt>|Font:|<choice-list|(gui-message "font " answer)|Roman|Roman|Sans|Mono>>|<hlist|<form-checkbox|grid|true|(gui-message name " " answer)> Grid|<glue|true|false|0px|0px>|<action-button*|Cancel|(gui-message "Cancel")>|<action-button*|Ok|(gui-message "Ok")>>>>

</body>

<initial|<\collection>
</collection>>
