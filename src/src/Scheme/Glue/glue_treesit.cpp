
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-treesit.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

tmscm
tmg_tree_sitter_language_new (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-language-new");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_language_new (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_language_load (tmscm arg1) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tree-sitter-language-load");

  url in1= tmscm_to_url (arg1);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_language_load (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_parse_sexp (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-parse-sexp");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-parse-sexp");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= tree_sitter_parse_sexp (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_sitter_query_new (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-query-new");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-query-new");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_query_new (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_query_capture_names (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-query-capture-names");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  array_string out= tree_sitter_query_capture_names (in1);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tree_sitter_query_captures (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-query-captures");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-query-captures");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  array_int out= tree_sitter_query_captures (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_syntax_region_sexp (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-sexp");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-sexp");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  string out= syntax_region_sexp (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_syntax_region_node_at (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-node-at");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-node-at");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-node-at");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-node-at");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  object out= syntax_region_node_at (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_syntax_region_node_range (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5, tmscm arg6) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-node-range");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-node-range");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-node-range");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-node-range");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "syntax-region-node-range");
  TMSCM_ASSERT_INT (arg6, TMSCM_ARG6, "syntax-region-node-range");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);
  int in5= tmscm_to_int (arg5);
  int in6= tmscm_to_int (arg6);

  // TMSCM_DEFER_INTS;
  object out= syntax_region_node_range (in1, in2, in3, in4, in5, in6);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_syntax_region_errors (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-errors");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-errors");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  object out= syntax_region_errors (in1, in2);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_syntax_region_last_nodes (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-last-nodes");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-last-nodes");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  object out= syntax_region_last_nodes (in1, in2);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_syntax_region_captures (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-captures");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-captures");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-captures");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);

  // TMSCM_DEFER_INTS;
  object out= syntax_region_captures (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_syntax_region_statistics (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-statistics");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-statistics");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  array_int out= syntax_region_statistics (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_syntax_region_forget (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-forget");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  syntax_region_forget (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_syntax_region_indent (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-indent");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-indent");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-indent");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-indent");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "syntax-region-indent");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  int out= syntax_region_indent (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_syntax_region_errors_with (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-errors-with");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-errors-with");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-errors-with");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-errors-with");
  TMSCM_ASSERT_STRING (arg5, TMSCM_ARG5, "syntax-region-errors-with");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);
  string in5= tmscm_to_string (arg5);

  // TMSCM_DEFER_INTS;
  int out= syntax_region_errors_with (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_syntax_region_node_move (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4, tmscm arg5) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-node-move");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-node-move");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-node-move");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-node-move");
  TMSCM_ASSERT_INT (arg5, TMSCM_ARG5, "syntax-region-node-move");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);
  int in5= tmscm_to_int (arg5);

  // TMSCM_DEFER_INTS;
  array_int out= syntax_region_node_move (in1, in2, in3, in4, in5);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_syntax_region_node_info (tmscm arg1, tmscm arg2, tmscm arg3, tmscm arg4) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "syntax-region-node-info");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "syntax-region-node-info");
  TMSCM_ASSERT_INT (arg3, TMSCM_ARG3, "syntax-region-node-info");
  TMSCM_ASSERT_INT (arg4, TMSCM_ARG4, "syntax-region-node-info");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);
  int in3= tmscm_to_int (arg3);
  int in4= tmscm_to_int (arg4);

  // TMSCM_DEFER_INTS;
  array_int out= syntax_region_node_info (in1, in2, in3, in4);
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_set_highlighter (tmscm arg1, tmscm arg2, tmscm arg3) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-set-highlighter");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-sitter-set-highlighter");
  TMSCM_ASSERT_ARRAY_STRING (arg3, TMSCM_ARG3, "tree-sitter-set-highlighter");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);
  array_string in3= tmscm_to_array_string (arg3);

  // TMSCM_DEFER_INTS;
  tree_sitter_set_highlighter (in1, in2, in3);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_set_injections (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-set-injections");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-sitter-set-injections");

  string in1= tmscm_to_string (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree_sitter_set_injections (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_set_injection_name (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-set-injection-name");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-set-injection-name");

  string in1= tmscm_to_string (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  tree_sitter_set_injection_name (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_reset_highlights () {
  // TMSCM_DEFER_INTS;
  tree_sitter_reset_highlights ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_fold_state (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-sitter-fold-state");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_fold_state (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_set_fold_state (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "tree-sitter-set-fold-state");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "tree-sitter-set-fold-state");

  tree in1= tmscm_to_tree (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  tree_sitter_set_fold_state (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_pending_updates () {
  // TMSCM_DEFER_INTS;
  array_tree out= tree_sitter_pending_updates ();
  // TMSCM_ALLOW_INTS;

  return array_tree_to_tmscm (out);
}

tmscm
tmg_tree_sitter_line_colors (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-line-colors");
  TMSCM_ASSERT_TREE (arg2, TMSCM_ARG2, "tree-sitter-line-colors");

  string in1= tmscm_to_string (arg1);
  tree in2= tmscm_to_tree (arg2);

  // TMSCM_DEFER_INTS;
  object out= tree_sitter_line_colors (in1, in2);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_tree_sitter_line_tokens (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-line-tokens");
  TMSCM_ASSERT_TREE (arg2, TMSCM_ARG2, "tree-sitter-line-tokens");

  string in1= tmscm_to_string (arg1);
  tree in2= tmscm_to_tree (arg2);

  // TMSCM_DEFER_INTS;
  array_string out= tree_sitter_line_tokens (in1, in2);
  // TMSCM_ALLOW_INTS;

  return array_string_to_tmscm (out);
}

tmscm
tmg_tree_sitter_set_highlighting (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "tree-sitter-set-highlighting");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  tree_sitter_set_highlighting (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_wrap_language (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-wrap-language");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  tree_sitter_wrap_language (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_program_language (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-program-language");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_program_language (in1);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_set_scanner (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-set-scanner");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "tree-sitter-set-scanner");

  int in1= tmscm_to_int (arg1);
  object in2= tmscm_to_object (arg2);

  // TMSCM_DEFER_INTS;
  bool out= tree_sitter_set_scanner (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_sitter_unpack (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_URL (arg1, TMSCM_ARG1, "tree-sitter-unpack");
  TMSCM_ASSERT_URL (arg2, TMSCM_ARG2, "tree-sitter-unpack");

  url in1= tmscm_to_url (arg1);
  url in2= tmscm_to_url (arg2);

  // TMSCM_DEFER_INTS;
  bool out= tree_sitter_unpack (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_lookahead () {
  // TMSCM_DEFER_INTS;
  int out= tree_sitter_lexer_lookahead ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_advance (tmscm arg1) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "tree-sitter-lexer-advance");

  bool in1= tmscm_to_bool (arg1);

  // TMSCM_DEFER_INTS;
  tree_sitter_lexer_advance (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_lexer_mark_end () {
  // TMSCM_DEFER_INTS;
  tree_sitter_lexer_mark_end ();
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_lexer_set_result (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-lexer-set-result");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  tree_sitter_lexer_set_result (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_tree_sitter_lexer_column () {
  // TMSCM_DEFER_INTS;
  int out= tree_sitter_lexer_column ();
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_eofP () {
  // TMSCM_DEFER_INTS;
  bool out= tree_sitter_lexer_eof ();
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_advance_while (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-lexer-advance-while");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tree-sitter-lexer-advance-while");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_lexer_advance_while (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_advance_until (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-lexer-advance-until");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tree-sitter-lexer-advance-until");

  string in1= tmscm_to_string (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_lexer_advance_until (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_lexer_skip_spaces (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_BOOL (arg1, TMSCM_ARG1, "tree-sitter-lexer-skip-spaces");
  TMSCM_ASSERT_BOOL (arg2, TMSCM_ARG2, "tree-sitter-lexer-skip-spaces");

  bool in1= tmscm_to_bool (arg1);
  bool in2= tmscm_to_bool (arg2);

  // TMSCM_DEFER_INTS;
  int out= tree_sitter_lexer_skip_spaces (in1, in2);
  // TMSCM_ALLOW_INTS;

  return int_to_tmscm (out);
}

tmscm
tmg_tree_sitter_tables_start (tmscm arg1) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "tree-sitter-tables-start");

  string in1= tmscm_to_string (arg1);

  // TMSCM_DEFER_INTS;
  string out= tree_sitter_tables_start (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_sitter_tables_finish (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-tables-finish");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-tables-finish");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= tree_sitter_tables_finish (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

tmscm
tmg_tree_sitter_tables_export (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "tree-sitter-tables-export");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "tree-sitter-tables-export");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  string out= tree_sitter_tables_export (in1, in2);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

void
initialize_glue_treesit () {
  tmscm_install_procedure ("tree-sitter-language-new",  tmg_tree_sitter_language_new, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-language-load",  tmg_tree_sitter_language_load, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-parse-sexp",  tmg_tree_sitter_parse_sexp, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-query-new",  tmg_tree_sitter_query_new, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-query-capture-names",  tmg_tree_sitter_query_capture_names, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-query-captures",  tmg_tree_sitter_query_captures, 2, 0, 0);
  tmscm_install_procedure ("syntax-region-sexp",  tmg_syntax_region_sexp, 2, 0, 0);
  tmscm_install_procedure ("syntax-region-node-at",  tmg_syntax_region_node_at, 4, 0, 0);
  tmscm_install_procedure ("syntax-region-node-range",  tmg_syntax_region_node_range, 6, 0, 0);
  tmscm_install_procedure ("syntax-region-errors",  tmg_syntax_region_errors, 2, 0, 0);
  tmscm_install_procedure ("syntax-region-last-nodes",  tmg_syntax_region_last_nodes, 2, 0, 0);
  tmscm_install_procedure ("syntax-region-captures",  tmg_syntax_region_captures, 3, 0, 0);
  tmscm_install_procedure ("syntax-region-statistics",  tmg_syntax_region_statistics, 2, 0, 0);
  tmscm_install_procedure ("syntax-region-forget",  tmg_syntax_region_forget, 1, 0, 0);
  tmscm_install_procedure ("syntax-region-indent",  tmg_syntax_region_indent, 5, 0, 0);
  tmscm_install_procedure ("syntax-region-errors-with",  tmg_syntax_region_errors_with, 5, 0, 0);
  tmscm_install_procedure ("syntax-region-node-move",  tmg_syntax_region_node_move, 5, 0, 0);
  tmscm_install_procedure ("syntax-region-node-info",  tmg_syntax_region_node_info, 4, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-highlighter",  tmg_tree_sitter_set_highlighter, 3, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-injections",  tmg_tree_sitter_set_injections, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-injection-name",  tmg_tree_sitter_set_injection_name, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-reset-highlights",  tmg_tree_sitter_reset_highlights, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-fold-state",  tmg_tree_sitter_fold_state, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-fold-state",  tmg_tree_sitter_set_fold_state, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-pending-updates",  tmg_tree_sitter_pending_updates, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-line-colors",  tmg_tree_sitter_line_colors, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-line-tokens",  tmg_tree_sitter_line_tokens, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-highlighting",  tmg_tree_sitter_set_highlighting, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-wrap-language",  tmg_tree_sitter_wrap_language, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-program-language",  tmg_tree_sitter_program_language, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-set-scanner",  tmg_tree_sitter_set_scanner, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-unpack",  tmg_tree_sitter_unpack, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-lookahead",  tmg_tree_sitter_lexer_lookahead, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-advance",  tmg_tree_sitter_lexer_advance, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-mark-end",  tmg_tree_sitter_lexer_mark_end, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-set-result",  tmg_tree_sitter_lexer_set_result, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-column",  tmg_tree_sitter_lexer_column, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-eof?",  tmg_tree_sitter_lexer_eofP, 0, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-advance-while",  tmg_tree_sitter_lexer_advance_while, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-advance-until",  tmg_tree_sitter_lexer_advance_until, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-lexer-skip-spaces",  tmg_tree_sitter_lexer_skip_spaces, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-tables-start",  tmg_tree_sitter_tables_start, 1, 0, 0);
  tmscm_install_procedure ("tree-sitter-tables-finish",  tmg_tree_sitter_tables_finish, 2, 0, 0);
  tmscm_install_procedure ("tree-sitter-tables-export",  tmg_tree_sitter_tables_export, 2, 0, 0);
}
