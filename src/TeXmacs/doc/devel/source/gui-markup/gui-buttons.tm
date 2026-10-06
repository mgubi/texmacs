<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Buttons.> Each button runs its command when the mouse button is released; the command shows a message in the footer. Hover and press to see the three looks of a button (<verbatim|normal>, <verbatim|hover>, <verbatim|pressed>), which <verbatim|dynamic-case> chooses.

  <action-button*|Hello|(gui-message "Hello was pressed")> <action-button*|Goodbye|(gui-message "Goodbye was pressed")> <action-button*|<icon|tm_new.xpm>|(gui-message "The icon was pressed")>

  <verbatim|action-button> fills the width of the line:

  <action-button|A wide button|(gui-message "The wide button was pressed")>

  Menu buttons, as in a menu (<verbatim|menu-button> inside <verbatim|vlist>), and with <verbatim|with-explicit-buttons>:

  <vlist|<menu-button|Open...|(gui-message "Open")>|<menu-button|Save|(gui-message "Save")>|<menu-button|Close|(gui-message "Close")>>

  <with-explicit-buttons|<hlist|<menu-button|One|(gui-message "One")>|<menu-button|Two|(gui-message "Two")>>>

</body>

<initial|<\collection>
</collection>>
