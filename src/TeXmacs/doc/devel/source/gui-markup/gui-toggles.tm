<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Toggles.> A click changes the value of the toggle in the document, then runs its command with <verbatim|answer> (the new value) and, for <verbatim|form-checkbox>, <verbatim|name>.

  <form-checkbox|bold|false|(gui-message name " is now " answer)> Bold <space|2em> <form-checkbox|italic|true|(gui-message name " is now " answer)> Italic

  A <verbatim|toggle-button> (made by the interpreter of widgets) changes its look, but its command does not run (see the loose ends of the chapter):

  <toggle-button|false|(gui-message "toggled to " answer)> Underline

</body>

<initial|<\collection>
</collection>>
