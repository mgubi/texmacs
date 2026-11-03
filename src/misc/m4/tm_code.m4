
#--------------------------------------------------------------------
#
# MODULE      : tm_code.m4
# DESCRIPTION : the features of program code which a build may leave out
# COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
#
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
#
#--------------------------------------------------------------------

# tree-sitter (Plugins/TreeSitter, progs/treesit) and the language servers
# (System/Link/lsp_link.cpp, progs/lsp) are optional providers of the
# features of program code (src/docs/code-features.md): TeXmacs works without
# one of them, or both.

AC_DEFUN([TM_CODE_FEATURES],[
  AC_ARG_ENABLE(tree-sitter,
  AS_HELP_STRING([--disable-tree-sitter],
  [without the syntax trees of tree-sitter for program code]),
  [], [enable_tree_sitter="yes"])
  if test "$enable_tree_sitter" = "no"; then
    AC_MSG_RESULT([disabling tree-sitter])
    AC_SUBST(CONFIG_TREE_SITTER,[""])
  else
    AC_MSG_RESULT([enabling tree-sitter])
    AC_DEFINE(USE_TREE_SITTER, 1, [Use the syntax trees of tree-sitter])
    AC_SUBST(CONFIG_TREE_SITTER,["TreeSitter"])
  fi

  AC_ARG_ENABLE(lsp,
  AS_HELP_STRING([--disable-lsp],
  [without the language servers for program code]),
  [], [enable_lsp="yes"])
  if test "$enable_lsp" = "no"; then
    AC_MSG_RESULT([disabling language servers])
  else
    AC_MSG_RESULT([enabling language servers])
    AC_DEFINE(USE_LSP, 1, [Use language servers])
  fi
])
