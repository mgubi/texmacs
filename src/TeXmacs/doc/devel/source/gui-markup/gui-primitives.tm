<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|The primitives.> <verbatim|dynamic-case> shows one of its branches according to the messages which its box receives:

  <dynamic-case|mouse-over|<with|color|dark red|The mouse is over me>|any|<with|color|dark blue|Move the mouse over me>>

  <verbatim|relay> sends the events of its box to a Scheme function (here <verbatim|gui-show-event>, which shows them in the footer); click, move or press keys in it:

  <relay|<with|color|dark green|An area which relays its events>|gui-show-event|relay>

  A broadcast message (here <verbatim|shift>, sent by the button) changes all the boxes which wait for it:

  <action-button*|Toggle shift|(emu-toggle-modifier "shift")> <dynamic-case|shift|<strong|SHIFT IS ON>|any|shift is off>

</body>

<initial|<\collection>
</collection>>
