<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <scheme> interface to buffers, views and windows>

  Almost everything described in this chapter is exported to <scheme>, and
  most of the user visible behaviour (which questions are asked when a
  document is loaded, saved or closed, where a new document appears) is
  programmed there. This page relates the three levels:

  <\enumerate>
    <item>the <c++> routines of <verbatim|Texmacs/Data/>, of the server and
    of the editor;

    <item>the <em|glue> routines, which export them under <scheme> names
    and are generated from the files <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>,
    <source-link|build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm> and <source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>
    of <verbatim|src/src/Scheme/Glue/> (see <hlink|the <scheme>
    interpreter and the glue|scheme-bridge.en.tm>);

    <item>the <scheme> library and the user level commands, in
    <source-link|kernel/library/base.scm|TeXmacs/progs/kernel/library/base.scm>,
    <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm> and
    <source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm> (relative to
    <verbatim|src/TeXmacs/progs/>).
  </enumerate>

  The user level documentation of the <scheme> functions is in
  <hlink|the <scheme> buffer API|../scheme/buffer/scheme-buffer.en.tm>,
  which describes <hlink|buffers|../scheme/buffer/buffer-api.en.tm>,
  <hlink|views|../scheme/buffer/view-api.en.tm> and
  <hlink|windows|../scheme/buffer/window-api.en.tm>.

  <section|Three receivers>

  The three glue files differ by the object on which the exported routines
  are called:

  <\description>
    <item*|<source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>>Free functions, without a
    receiver. Among those are all buffer, view, window and project routines
    of <verbatim|Texmacs/Data/> (the block starting with the comment
    <verbatim|;; buffers> in that file). They take buffer names, view
    and window <abbr|URL>s as arguments and therefore work on any buffer.

    <item*|<source-link|build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm>>Routines called as
    <cpp|get_server()-\<gtr\>...>. Those which concern windows (properties,
    bars, zoom, footer, dialogs) act on the window of the <em|current
    view>.

    <item*|<source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>>Routines called as
    <cpp|get_current_editor()-\<gtr\>...>: all editing commands, the
    cursor, the selection, the undo history and the environment at the
    cursor. They always act on the <em|current view>.
  </description>

  To apply a routine of the last two kinds to another buffer or window,
  change the current view temporarily with <scm|with-buffer> or
  <scm|with-window>; see <hlink|working in the context of another
  view|server-views.en.tm> for the limitations of these macros.

  <section|Correspondence between <scheme> and <c++>>

  The correspondence is mostly one to one, with the <scheme> name obtained
  by replacing underscores by dashes. The noteworthy exceptions are listed
  in parentheses.

  <\description>
    <item*|Buffers><scm|buffer-list> (<cpp|get_all_buffers>),
    <scm|current-buffer-url> (<cpp|get_current_buffer_safe>),
    <scm|path-to-buffer> (<cpp|path_to_buffer>), <scm|buffer-new>
    (<cpp|make_new_buffer>), <scm|buffer-rename> (<cpp|rename_buffer>),
    <scm|buffer-set> and <scm|buffer-get> (<cpp|set_buffer_tree>,
    <cpp|get_buffer_tree>), <scm|buffer-set-body> and <scm|buffer-get-body>,
    <scm|buffer-set-master> and <scm|buffer-get-master>,
    <scm|buffer-set-title> and <scm|buffer-get-title>,
    <scm|buffer-last-save>, <scm|buffer-last-visited>,
    <scm|buffer-modified?> (<cpp|buffer_modified>),
    <scm|buffer-modified-since-autosave?>, <scm|buffer-pretend-modified>,
    <scm|buffer-pretend-saved>, <scm|buffer-pretend-autosaved>,
    <scm|buffer-attach-notifier>, <scm|buffer-has-name?>, <scm|buffer-aux?>
    (<cpp|is_aux_buffer>), <scm|buffer-embedded?>
    (<cpp|is_embedded_buffer>), <scm|buffer-import>, <scm|buffer-load>,
    <scm|buffer-export>, <scm|buffer-save>, <scm|buffer-focus>
    (<cpp|focus_on_buffer>), <scm|buffer-focus*>
    (<cpp|var_focus_on_buffer>) and <scm|cpp-buffer-close>
    (<cpp|kill_buffer>).

    <item*|Views><scm|view-list> (<cpp|get_all_views>),
    <scm|buffer-\<gtr\>views>, <scm|current-view-url>,
    <scm|window-\<gtr\>view>, <scm|view-\<gtr\>buffer>,
    <scm|view-\<gtr\>window-url>, <scm|view-new> (<cpp|get_new_view>),
    <scm|view-passive> (<cpp|get_passive_view>), <scm|view-recent>
    (<cpp|get_recent_view>), <scm|view-delete> (<cpp|delete_view>),
    <scm|window-set-view> (<cpp|window_set_view>), <scm|switch-to-buffer>
    (<cpp|switch_to_buffer>) and <scm|set-drd> (<cpp|set_current_drd>).

    <item*|Windows><scm|window-list> (<cpp|windows_list>),
    <scm|windows-number> (<cpp|get_nr_windows>, which counts all toolkit
    windows), <scm|current-window>, <scm|buffer-\<gtr\>windows>,
    <scm|window-to-buffer>, <scm|window-set-buffer>, <scm|window-focus>,
    <scm|switch-to-window>, <scm|new-buffer> (<cpp|create_buffer ()>),
    <scm|open-buffer-in-window> (<cpp|new_buffer_in_new_window>),
    <scm|open-window>, <scm|clone-window>, <scm|kill-window> and
    <scm|kill-current-window-and-buffer>.

    <item*|Projects><scm|project-attach>, <scm|project-detach> (which is
    <cpp|project_attach> with the default empty name),
    <scm|project-attached?> and <scm|project-get>.

    <item*|Alternative windows>The family <scm|alt-window-handle>,
    <scm|alt-window-create-quit>, <scm|alt-window-create-plain>,
    <scm|alt-window-create-popup>, <scm|alt-window-create-tooltip>,
    <scm|alt-window-delete>, <scm|alt-window-show>, <scm|alt-window-hide>,
    <scm|alt-window-get-size>, <scm|alt-window-set-size>,
    <scm|alt-window-get-position>, <scm|alt-window-set-position> and
    <scm|alt-window-search>, which wraps the functions at the end of
    <source-link|tm_window.hpp|src/Texmacs/tm_window.hpp>; these use integer handles, except
    <scm|alt-window-search>, which maps a buffer name to the list of handles
    of its windows; see <hlink|alternative windows|server-windows.en.tm>.

    <item*|Server and current window>(<source-link|build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm>)
    <scm|window-get-serial>, <scm|window-set-property>,
    <scm|window-get-property>, <scm|show-header>, <scm|show-icon-bar>,
    <scm|show-side-tools>, <scm|show-bottom-tools>, <scm|show-footer>,
    <scm|full-screen-mode>, <scm|set-window-zoom-factor>,
    <scm|set-message>, <scm|recall-message>, <scm|cpp-choose-file>,
    <scm|tm-interactive>, <scm|insert-kbd-wildcard>,
    <scm|set-variant-keys>, <scm|kbd-system-rewrite>,
    <scm|update-all-buffers>, <scm|quit-TeXmacs>, ...

    <item*|Current editor>(<source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>)
    <scm|root-tree>, <scm|buffer-path>, <scm|buffer-tree>,
    <scm|cursor-path>, <scm|key-press>, <scm|mouse-any>,
    <scm|get-input-mode>, <scm|get-env>, <scm|go-to-path>, the selection
    routines (<scm|selection-active-any?>, <scm|selection-get-start>,
    ...), the undo routines (<scm|start-editing>, <scm|end-editing>,
    <scm|archive-state>, <scm|mark-start>, <scm|mark-end>,
    <scm|mark-cancel>, <scm|undo>, <scm|redo>, <scm|clear-undo-history>),
    <scm|notify-change>, <scm|idle-time>, <scm|update-menus>, ...
  </description>

  <section|Convenience wrappers>

  Several glue routines return <cpp|url_none ()> or the empty <abbr|URL>
  when there is no answer. The library <source-link|kernel/library/base.scm|TeXmacs/progs/kernel/library/base.scm>
  defines wrappers which return <scm|#f> in that case, and which most
  <scheme> code uses: <scm|current-buffer>, <scm|current-view>,
  <scm|window-\<gtr\>buffer>, <scm|view-\<gtr\>window>,
  <scm|path-\<gtr\>buffer>, <scm|buffer-\<gtr\>window>, as well as
  <scm|buffer-exists?>, <scm|buffer-\<gtr\>tree>,
  <scm|tree-\<gtr\>buffer> and <scm|buffer-master>.

  <section|User level commands>

  The commands bound to menus and keyboard shortcuts are rarely glue
  routines themselves. They are <scheme> functions which add
  confirmations, autosave handling, recent file lists and the like on top
  of the glue:

  <\description>
    <item*|Loading><scm|load-buffer>, <scm|load-buffer-in-new-window>,
    <scm|load-browse-buffer>, <scm|open-buffer> (with a file chooser),
    <scm|revert-buffer>, <scm|open-auxiliary> (<source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>);
    see <hlink|loading|server-buffers.en.tm>.

    <item*|Saving><scm|save-buffer>, <scm|save-buffer-as>,
    <scm|export-buffer>, <scm|autosave-buffer>, <scm|autosave-all>
    (<source-link|tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>); see <hlink|saving and
    exporting|server-buffers.en.tm>.

    <item*|Closing><scm|safely-kill-buffer>, <scm|safely-kill-window>,
    <scm|safely-quit-TeXmacs>, <scm|buffer-close>
    (<source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>); see <hlink|closing a
    buffer|server-buffers.en.tm> and <hlink|closing
    windows|server-windows.en.tm>.

    <item*|Policy><scm|new-document>, <scm|new-document*>,
    <scm|close-document>, <scm|close-document*> (<source-link|tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>)
    choose between the buffer and the window versions of these commands
    according to the <verbatim|buffer management> preference
    (<scm|window-per-buffer?>).
  </description>

  New code should normally call the user level commands when it acts on
  behalf of the user, and the glue routines (or the <source-link|base.scm|TeXmacs/progs/kernel/library/base.scm>
  wrappers) when it manipulates buffers programmatically, for instance to
  build an auxiliary document in the background without showing it.

  <section|Exporting a new routine>

  A new routine of <verbatim|Texmacs/Data/> is exported by declaring it in
  the corresponding header (<source-link|new_buffer.hpp|src/Texmacs/Data/new_buffer.hpp>,
  <source-link|new_view.hpp|src/Texmacs/Data/new_view.hpp>, <source-link|new_window.hpp|src/Texmacs/Data/new_window.hpp>) and adding a line
  such as

  <\scm-code>
    (buffer-has-name? buffer_has_name (bool url))
  </scm-code>

  to the buffer block of <source-link|build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>. A new editing
  command is declared as a pure virtual method of <cpp|editor_rep>,
  implemented in the appropriate <verbatim|edit_*_rep> class (see
  <hlink|the editor classes|server-editor.en.tm>) and added to
  <source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>; a new routine of the server is added
  to <cpp|server_rep>, implemented in one of the three server classes
  and added to <source-link|build-glue-server.scm|src/Scheme/Glue/build-glue-server.scm>. In all cases the glue
  has to be regenerated (see <hlink|the <c++>/<scheme>
  glue|scheme-bridge.en.tm>).

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
