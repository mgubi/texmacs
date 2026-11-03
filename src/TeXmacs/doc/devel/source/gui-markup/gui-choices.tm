<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Choice lists.> <verbatim|choice-list> takes a command, the current item and the items; a click selects an item and runs the command with <verbatim|answer>.

  <choice-list|(gui-message "chosen: " answer)|Green|Red|Green|Blue>

  <verbatim|check-list> keeps several items (a <verbatim|tuple>):

  <check-list|(gui-message "checked: " answer)|<tuple|Apples>|Apples|Pears|Plums>

  In a popup (<verbatim|input-popup>), as an enumeration of a dialog: click on the field to open the list.

  <input-popup|generic|(gui-message "size: " answer)|8em|10pt|<choice-list|(gui-message "size: " answer)|10pt|8pt|10pt|12pt|14pt>>

</body>

<initial|<\collection>
</collection>>
