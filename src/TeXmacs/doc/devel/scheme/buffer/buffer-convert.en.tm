<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Links between buffers, views and windows>

  A buffer may have several views, and each view is displayed in at most
  one window, which displays exactly one view. The following routines,
  documented on the previous pages, convert between the three kinds of
  objects; those which may fail return <scm|#f>:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|cell-bborder|1ln>|<table|<row|<cell|From>|<cell|To>|<cell|Routine>>|<row|<cell|buffer>|<cell|views>|<cell|<scm|(buffer-\<gtr\>views
  <scm-arg|buf>)>>>|<row|<cell|buffer>|<cell|windows>|<cell|<scm|(buffer-\<gtr\>windows
  <scm-arg|buf>)>, <scm|(buffer-\<gtr\>window
  <scm-arg|buf>)>>>|<row|<cell|view>|<cell|buffer>|<cell|<scm|(view-\<gtr\>buffer
  <scm-arg|vw>)>>>|<row|<cell|view>|<cell|window>|<cell|<scm|(view-\<gtr\>window
  <scm-arg|vw>)>>>|<row|<cell|window>|<cell|view>|<cell|<scm|(window-\<gtr\>view
  <scm-arg|win>)>>>|<row|<cell|window>|<cell|buffer>|<cell|<scm|(window-\<gtr\>buffer
  <scm-arg|win>)>>>|<row|<cell|>|<cell|current
  objects>|<cell|<scm|(current-buffer)>, <scm|(current-view)>,
  <scm|(current-window)>>>>>>>
    Conversions between buffers, views and windows.
  </big-table>

  The current buffer is always the buffer of the current view, and the
  current window the window of the current view, if any. Changing one of
  them thus changes the others:

  <\itemize>
    <item><scm|(switch-to-buffer <scm-arg|buf>)> displays in the current
    window a view on <scm-arg|buf> which is not displayed elsewhere (a new
    one is made if needed), and makes it the current view;

    <item><scm|(window-set-buffer <scm-arg|win> <scm-arg|buf>)> and
    <scm|(window-set-view <scm-arg|win> <scm-arg|vw> <scm-arg|focus?>)> do
    the same for another window;

    <item><scm|(buffer-focus <scm-arg|buf>)> and <scm|(with-buffer
    <scm-arg|buf> ...)> make a view on <scm-arg|buf> the current view
    without changing what the windows display, which is how routines act on
    a buffer which is not displayed;

    <item><scm|(switch-to-window <scm-arg|win>)> and <scm|(window-focus
    <scm-arg|win>)> make the view of <scm-arg|win> the current one.
  </itemize>

  A view which is not displayed in any window still holds an editor, so
  that the document can be typeset and modified (for instance by
  <scm|with-buffer>); <scm|(view-delete <scm-arg|vw>)> removes it. Closing a
  buffer deletes all its views, and the windows which displayed them switch
  to another buffer. See <hlink|the server: buffers, views and
  windows|../../source/server.en.tm> for the <c++> side.

  <tmdoc-copyright|2012\U2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
