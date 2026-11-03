<TeXmacs|2.1.4>

<style|<tuple|generic|gui-button>>

<\body>
  <use-module|(doc gui-markup-examples)>

  <strong|Input fields.> <verbatim|input-field> runs its command with the text of the field when <verbatim|Return> is pressed in it:

  <input-field|string|(gui-message "you typed: " answer)|20em|edit me, then press Return>

  The fields of forms run their command at each key, with <verbatim|name> and <verbatim|answer>:

  <align-tiled|2|Name:|<form-input-text|name|string|(gui-message name " = " answer)|15em|>|E-mail:|<form-input-text|email|string|(gui-message name " = " answer)|15em|>>

  <form-text-area|notes|string|(gui-message name " has " answer)|20em|Some notes>

</body>

<initial|<\collection>
</collection>>
