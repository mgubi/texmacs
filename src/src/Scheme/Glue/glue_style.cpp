
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from build-glue-style.scm. Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/

tmscm
tmg_style_rewrite_buffer () {
  // TMSCM_DEFER_INTS;
  tree out= get_current_editor()->style_rewrite ();
  // TMSCM_ALLOW_INTS;

  return tree_to_tmscm (out);
}

tmscm
tmg_style_rewrite_statistics () {
  // TMSCM_DEFER_INTS;
  array_int out= get_current_editor()->style_rewrite_statistics ();
  // TMSCM_ALLOW_INTS;

  return array_int_to_tmscm (out);
}

void
initialize_glue_style () {
  tmscm_install_procedure ("style-rewrite-buffer",  tmg_style_rewrite_buffer, 0, 0, 0);
  tmscm_install_procedure ("style-rewrite-statistics",  tmg_style_rewrite_statistics, 0, 0, 0);
}
