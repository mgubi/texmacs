<TeXmacs|2.1.4>

<style|<tuple|generic|gui-keyboard>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|A virtual keyboard.> Put the cursor at the end of the line below, then click on the keys: they type there (<verbatim|simple-key> and <verbatim|extended-key> call <verbatim|emu-key>). <verbatim|shift> is a <verbatim|mod-key>: it is toggled by <verbatim|emu-toggle-modifier>, which broadcasts <verbatim|shift> or <verbatim|no-shift> to all the boxes, and the keys show their state without being typeset again.

  Type here: 

  <keyboard|<tformat|<table|<row|<cell|<simple-key|a><simple-key|b><simple-key|c><simple-key|d><extended-key|<math|\<leftarrow\>>|(emu-key "backspace")|1.5><mod-key|shift|1.5>>>>>>

</body>

<initial|<\collection>
</collection>>
