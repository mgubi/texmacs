
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-server.scm
;; DESCRIPTION : Building basic glue for the server
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public licence and comes WITHOUT
;; ANY WARRENTY WHATSOEVER. See the file $TEXMACS_PATH/LICENCE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(output-copyright "build-glue-server.scm")

(build
  "get_server()->"
  "initialize_glue_server"

  (set-input-language set_input_language (void string))
  (get-input-language get_input_language (string))
  (set-output-language set_output_language (void string))
  (get-output-language get_output_language (string))
  (insert-kbd-wildcard insert_kbd_wildcard (void string string bool bool bool))
  (set-variant-keys set_variant_keys (void string string))
  (kbd-pre-rewrite kbd_pre_rewrite (string string))
  (kbd-post-rewrite kbd_post_rewrite (string string))
  (set-font-rules set_font_rules (void scheme_tree))

  (window-set-property set_window_property (void scheme_tree scheme_tree))
  (window-get-property get_window_property (scheme_tree scheme_tree))
  (show-header show_header (void bool))
  (show-icon-bar show_icon_bar (void int bool))
  (show-footer show_footer (void bool))
  (visible-header? visible_header (bool))
  (visible-icon-bar? visible_icon_bar (bool int))
  (visible-footer? visible_footer (bool))
  (full-screen-mode full_screen_mode (void bool))
  (full-screen? in_full_screen_mode (bool))
  (set-shrinking-factor set_shrinking_factor (void int))
  (get-shrinking-factor get_shrinking_factor (int))

  (exec-delayed exec_delayed (void string))
  (exec-delayed-cmd exec_delayed (void command))
  (shell shell (void string))
  (dialogue-end dialogue_end (void))
  (choose-file choose_file (void string string scheme_tree))

  (has-view? has_view (bool))
  (buffer-unsaved? buffer_unsaved (bool))
  (exists-unsaved-buffer? exists_unsaved_buffer (bool))
  (pretend-save-buffer pretend_save_buffer (void))
  (get-name-buffer get_name_buffer (url))
  (set-name-buffer set_name_buffer (void url))
  (new-buffer new_buffer (void))
  (switch-to-buffer switch_to_buffer (void url))
  (switch-to-active-buffer switch_to_active_buffer (void url))
  (revert-buffer revert_buffer (void))
  (kill-buffer kill_buffer (void))
  (open-window open_window (void))
  (clone-window clone_window (void))
  (kill-window kill_window (void))
  (set-maximal-undo-depth set_max_undo_depth (void int))
  (get-maximal-undo-depth get_max_undo_depth (int))
  (no-name? no_name (bool))
  (help-buffer? help_buffer (bool))
  (set-buffer revert_buffer (void url texmacs_tree))
  (set-aux-buffer set_aux_buffer (void string url texmacs_tree))
  (set-help-buffer set_help_buffer (void url texmacs_tree))
  (browse-help browse_help (void int))

  (project-attach project_attach (void string))
  (project-detach project_attach (void))
  (project-attached? project_attached (bool))

  (texmacs-load-tree load_tree (texmacs_tree url string))
  (texmacs-load-buffer load_buffer (void url string int bool))
  (texmacs-save-buffer save_buffer (void url string))
  (delayed-autosave delayed_autosave (void))
  
  (color get_color (int string))
  (style-clear-cache style_clear_cache (void))
  (set-script-status set_script_status (void int))
  (set-printing-command set_printing_command (void string))
  (set-printer-paper-type set_printer_page_type (void string))
  (set-printer-dpi set_printer_dpi (void string))
  (set-default-shrinking-factor set_default_shrinking_factor (void int))
  (get-default-shrinking-factor get_default_shrinking_factor (int))
  (get-nr-windows get_nr_windows (int))
  (postscript-gc postscript_gc (void))
  (inclusions-gc inclusions_gc (void))
  (translate translate (string string string string))
  (yes? is_yes (bool string))
  (quit-TeXmacs quit (void))
  (package-evaluate evaluate (texmacs_tree string string texmacs_tree)))
