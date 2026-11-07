; The indentation of Scheme as utils/misc/tm-keywords.scm and
; prog/scheme-edit.scm compute it, for syntax_region_indent
; (Plugins/TreeSitter/syntax_region.cpp): every list and vector is a
; node of Lisp indentation, and the lists whose first element is a
; keyword get its arity. Generated from progs/tm-mode.el by
; misc/tree-sitter/gen-scheme-indents.py: do not edit.

(list) @indent.lisp
(vector) @indent.lisp

((list . (symbol) @_op) @indent.lisp
 (#any-of? @_op
  "begin" "cond" "else" "values" "define-preferences" "menu-dynamic"
  "conserve-focus" "attach-macro" "case-lambda" "kbd-map" "kbd-wildcards"
  "kbd-commands" "kbd-symbols" "define-grammar" "define-regexp-grammar"
  "drd-rule" "logic-rules" "assume" "texmacs-modes" "user-delayed" "delayed"
  "on-entry" "on-exit" "widget-delayed" "with-wallet" "association-tile"
  "bar" "concat" "dense-bar" "dense-tile" "document" "header-bar" "sequence"
  "short-bar" "short-tile" "minibar" "wrap-selection-any"
  "wrap-selection-small" "try-modification" "try-correct" "tabs" "icon-tabs"
  "padded" "centered" "aligned" "bottom-buttons" "scrollable" "hlist"
  "vlist" "hsplit" "vsplit" "explicit-buttons" "horizontal" "vertical"
  "$begin" "$cond" ":use" ":inherit" "$tmdoc" "$tmdoc-title" "$para"
  "$itemize" "$enumerate" "$description" "$description-aligned"
  "$description-long" "$tm-fragment")
 (#set! indent.arity "0"))

((list . (symbol) @_op) @indent.lisp
 (#any-of? @_op
  "let" "let*" "lambda" "with-result" "and-let*" "setup-append-if" "while"
  "for" "repeat" "when" "unless" "assuming" "mini" "tile" "plugin-configure"
  "define-preference-names" "with-focus-after" "logic-group" "logic-table"
  "logic-dispatcher" "with-aux" "with-action" "with-module" "with-cursor"
  "with-buffer" "with-window" "with-author" "with-server" "with-database"
  "with-database*" "with-time" "with-time-stamp" "with-limit"
  "with-encoding" "with-indexing" "with-user" "with-extra-fields"
  "with-remote-context" "with-identifier-context" "speech-symbols"
  "user-ask" "tab" "icon-tab" "section-tab" "form" "item" "meti"
  "refreshable" "division" "$when" "$let" "$let*" "$for" "$refreshable"
  "$division" "tmfs-load-handler" "tmfs-save-handler"
  "tmfs-autosave-handler" "tmfs-remove-handler" "tmfs-wrap-handler"
  "tmfs-date-handler" "tmfs-title-handler" "tmfs-permission-handler"
  "tmfs-master-handler" "tmfs-format-handler" "push-focus" "pull-focus"
  "define" "define-public" "define-macro" "define-public-macro"
  "texmacs-module" "provide-public" "define-group" "tm-define"
  "tm-define-macro" "lazy-body-macro" "tm-property" "request-handler"
  "tm-menu" "define-menu" "tm-widget" "define-widget" "tm-generate"
  "tm-tool" "tm-tool*" "tm-build" "tm-build-macro" "tm-build-widget"
  "menu-bind" "smart-table" "define-table" "extend-table"
  "define-collection" "extend-collection" "tm-service" "tm-call-back"
  "define-format" "define-language" "define-graphics" "format" "interactive"
  "$describe-item" "$link" "$tmdoc-link" "$folded-documentation"
  "$unfolded-documentation" "$explain")
 (#set! indent.arity "1"))

((list . (symbol) @_op) @indent.lisp
 (#any-of? @_op
  "with" "with-define" "with-global" "and-with" "with-innermost" "receive"
  "with-environment" "with-environment*" "converter" "with-cache"
  "speech-map" "speech-map-wildcard" "speech-adjust" "speech-reduce"
  "speech-collection" "user-confirm" "user-url" "section-tabs" "resize"
  "cached" "$with" "client-remote-eval" "server-remote-eval")
 (#set! indent.arity "2"))

((list . (symbol) @_op) @indent.lisp
 (#any-of? @_op
  "ahash-with" "canvas-input" "with-remote-get-attributes"
  "with-remote-get-entry" "with-remote-create-entry" "with-remote-search"
  "with-remote-search-user" "with-remote-get-user-pseudo"
  "with-remote-get-user-name" "with-remote-identifier")
 (#set! indent.arity "3"))
