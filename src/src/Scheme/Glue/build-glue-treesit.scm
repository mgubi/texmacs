
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue-treesit.scm
;; DESCRIPTION : Building the glue of tree-sitter (Plugins/TreeSitter)
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (compiled only when the feature is part of the build, see
;; doc/code-features.md and glue.cpp)

(output-copyright "build-glue-treesit.scm")

(build
  ""
  "initialize_glue_treesit"

  (tree-sitter-language-new tree_sitter_language_new (int string))
  (tree-sitter-language-load tree_sitter_language_load (int url))
  (tree-sitter-parse-sexp tree_sitter_parse_sexp (string int string))
  (tree-sitter-query-new tree_sitter_query_new (int int string))
  (tree-sitter-query-capture-names tree_sitter_query_capture_names (array_string int))
  (tree-sitter-query-captures tree_sitter_query_captures (array_int int string))
  (syntax-region-sexp syntax_region_sexp (string tree int))
  (syntax-region-node-at syntax_region_node_at (object tree int int int))
  (syntax-region-node-range syntax_region_node_range (object tree int int int int int))
  (syntax-region-errors syntax_region_errors (object tree int))
  (syntax-region-last-nodes syntax_region_last_nodes (object tree int))
  (syntax-region-captures syntax_region_captures (object tree int int))
  (syntax-region-statistics syntax_region_statistics (array_int tree int))
  (syntax-region-forget syntax_region_forget (void tree))
  (syntax-region-indent syntax_region_indent (int tree int int int int))
  (syntax-region-errors-with syntax_region_errors_with (int tree int int int string))
  (syntax-region-node-move syntax_region_node_move (array_int tree int int int int))
  (syntax-region-node-info syntax_region_node_info (array_int tree int int int))
  (tree-sitter-set-highlighter tree_sitter_set_highlighter (void string int array_string))
  (tree-sitter-set-injections tree_sitter_set_injections (void string int))
  (tree-sitter-set-injection-name tree_sitter_set_injection_name (void string string))
  (tree-sitter-reset-highlights tree_sitter_reset_highlights (void))
  (tree-sitter-fold-state tree_sitter_fold_state (int tree))
  (tree-sitter-set-fold-state tree_sitter_set_fold_state (void tree int))
  (tree-sitter-pending-updates tree_sitter_pending_updates (array_tree))
  (tree-sitter-line-colors tree_sitter_line_colors (object string tree))
  (tree-sitter-set-highlighting tree_sitter_set_highlighting (void bool))
  (tree-sitter-wrap-language tree_sitter_wrap_language (void string))
  (tree-sitter-program-language tree_sitter_program_language (int string))
  (tree-sitter-set-scanner tree_sitter_set_scanner (bool int object))
  (tree-sitter-unpack tree_sitter_unpack (bool url url))
  (tree-sitter-lexer-lookahead tree_sitter_lexer_lookahead (int))
  (tree-sitter-lexer-advance tree_sitter_lexer_advance (void bool))
  (tree-sitter-lexer-mark-end tree_sitter_lexer_mark_end (void))
  (tree-sitter-lexer-set-result tree_sitter_lexer_set_result (void int))
  (tree-sitter-lexer-column tree_sitter_lexer_column (int))
  (tree-sitter-lexer-eof? tree_sitter_lexer_eof (bool))
  (tree-sitter-lexer-advance-while tree_sitter_lexer_advance_while (int string bool))
  (tree-sitter-lexer-advance-until tree_sitter_lexer_advance_until (int string bool))
  (tree-sitter-lexer-skip-spaces tree_sitter_lexer_skip_spaces (int bool bool))
  (tree-sitter-tables-start tree_sitter_tables_start (string string))
  (tree-sitter-tables-finish tree_sitter_tables_finish (string int string))
  (tree-sitter-tables-export tree_sitter_tables_export (string int string)))
