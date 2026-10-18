
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-lsp.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

tmscm
tmg_lsp_start (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_STRING (arg1, TMSCM_ARG1, "lsp-start");
  TMSCM_ASSERT_OBJECT (arg2, TMSCM_ARG2, "lsp-start");

  string in1= tmscm_to_string (arg1);
  object in2= tmscm_to_object (arg2);

  // TMSCM_DEFER_INTS;
  object out= lsp_start (in1, in2);
  // TMSCM_ALLOW_INTS;

  return object_to_tmscm (out);
}

tmscm
tmg_lsp_send (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "lsp-send");
  TMSCM_ASSERT_STRING (arg2, TMSCM_ARG2, "lsp-send");

  int in1= tmscm_to_int (arg1);
  string in2= tmscm_to_string (arg2);

  // TMSCM_DEFER_INTS;
  bool out= lsp_send (in1, in2);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_lsp_listen (tmscm arg1, tmscm arg2) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "lsp-listen");
  TMSCM_ASSERT_INT (arg2, TMSCM_ARG2, "lsp-listen");

  int in1= tmscm_to_int (arg1);
  int in2= tmscm_to_int (arg2);

  // TMSCM_DEFER_INTS;
  lsp_listen (in1, in2);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_lsp_aliveP (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "lsp-alive?");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  bool out= lsp_alive (in1);
  // TMSCM_ALLOW_INTS;

  return bool_to_tmscm (out);
}

tmscm
tmg_lsp_stop (tmscm arg1) {
  TMSCM_ASSERT_INT (arg1, TMSCM_ARG1, "lsp-stop");

  int in1= tmscm_to_int (arg1);

  // TMSCM_DEFER_INTS;
  lsp_stop (in1);
  // TMSCM_ALLOW_INTS;

  return TMSCM_UNSPECIFIED;
}

tmscm
tmg_lsp_document_text (tmscm arg1) {
  TMSCM_ASSERT_TREE (arg1, TMSCM_ARG1, "lsp-document-text");

  tree in1= tmscm_to_tree (arg1);

  // TMSCM_DEFER_INTS;
  string out= lsp_document_text (in1);
  // TMSCM_ALLOW_INTS;

  return string_to_tmscm (out);
}

void
initialize_glue_lsp () {
  tmscm_install_procedure ("lsp-start",  tmg_lsp_start, 2, 0, 0);
  tmscm_install_procedure ("lsp-send",  tmg_lsp_send, 2, 0, 0);
  tmscm_install_procedure ("lsp-listen",  tmg_lsp_listen, 2, 0, 0);
  tmscm_install_procedure ("lsp-alive?",  tmg_lsp_aliveP, 1, 0, 0);
  tmscm_install_procedure ("lsp-stop",  tmg_lsp_stop, 1, 0, 0);
  tmscm_install_procedure ("lsp-document-text",  tmg_lsp_document_text, 1, 0, 0);
}
