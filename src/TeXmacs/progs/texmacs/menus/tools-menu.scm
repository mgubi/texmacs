
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tools-menu.scm
;; DESCRIPTION : the tools menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus tools-menu)
  (:use (texmacs texmacs tm-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats, languages, and AI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (clipboard-preference-menu cvs fun)
  (with l (cvs "texmacs-file" "-file" #f)
    (for (fm l)
      (with name (format-get-name fm)
        ((eval name) (fun fm))))))

(tm-define (clipboard-import-preference-menu)
  (clipboard-preference-menu converters-to-special clipboard-set-import))
(tm-define (clipboard-export-preference-menu)
  (clipboard-preference-menu converters-from-special clipboard-set-export))

(menu-bind ai-translate-menu
  (for (lan supported-languages)
    ((eval (upcase-first lan))
     (ai-translate lan (get-preference "ai")))))

(tm-menu (tools-equation-editor-menu)
  ("Enable" (begin 
              (toggle-preference "equation-editor")
              (reinit-plugin-cache))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agents on a shared file (docs/mcp-server.md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (agents-here?)
  (and (shared-file? (current-buffer))
       (nnull? (shared-file-agents (current-buffer)))))

(define (pending-proposals)
  (list-filter (or (shared-proposals (current-buffer)) '())
               (lambda (e) (== (cadr e) "pending"))))

(define (short-agent id)
  ;; "agent.1234.xxxx.1" -> "agent 1234"
  (let ((l (string-tokenize-by-char id #\.)))
    (if (and (pair? l) (pair? (cdr l)) (== (car l) "agent"))
        (string-append "agent " (cadr l))
        (if (and (pair? l) (pair? (cdr l))) (string-append "program " (car l)) id))))

(define (unmarked-agents)
  ;; the participants taken for agents although they do not say so
  (list-filter (shared-file-agents (current-buffer))
               (lambda (id) (not (shared-agent-id? id)))))

(tm-menu (agents-menu)
  ((check "Propose edits (I accept or reject them)" "v"
          (== (shared-agent-mode (current-buffer)) 'propose))
   (shared-set-agent-mode! (current-buffer) 'propose))
  ((check "Let them edit directly" "v"
          (== (shared-agent-mode (current-buffer)) 'direct))
   (shared-set-agent-mode! (current-buffer) 'direct))
  ((check "Read only (refuse their edits)" "v"
          (== (shared-agent-mode (current-buffer)) 'read-only))
   (shared-set-agent-mode! (current-buffer) 'read-only))
  ---
  (for (id (unmarked-agents))
    (when #t
      ((eval (string-append "Treat " (short-agent id) " as a person"))
       (shared-treat-as-person! (current-buffer) id #t))))
  ---
  (for (e (pending-proposals))
    (-> (eval (string-append "Proposal " (car e) " (" (short-agent (cadddr e)) "): " (list-ref e 4)))
        ("Accept" (shared-proposal-resolve! (current-buffer) (car e) 'new))
        ("Reject" (shared-proposal-resolve! (current-buffer) (car e) 'old))))
  (when (nnull? (pending-proposals))
    ("Accept all"
     (for-each (lambda (e) (shared-proposal-resolve! (current-buffer) (car e) 'new))
               (pending-proposals)))
    ("Reject all"
     (for-each (lambda (e) (shared-proposal-resolve! (current-buffer) (car e) 'old))
               (pending-proposals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Tools menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind tools-menu
  (-> "Macros"
      (link source-macros-menu))
  (-> "Keyboard"
      ("Edit keyboard shortcuts" (open-shortcuts-editor "" "")))
  (-> "Speech"
      ("Off" (reset-preference "speech"))
      ---
      ("English" (set-preference "speech" "english"))
      ("French" (set-preference "speech" "french")))
  ---
  (-> "Update"
      ("Help" (help-caches-update))
      ("Inclusions" (inclusions-gc))
      ("Pictures" (picture-gc))
      ("Plugins" (reinit-plugin-cache))
      ("Styles" (style-clear-cache)))
  (if (url-exists-in-path? "pdflatex")
      (-> "LaTeX"
          (link tmtex-menu)))
  (-> "References"
      (link ref-menu))
  (if (defined? 'lsp-start)
      (-> "Language servers"
          (link lsp-menu)))
  (if supports-email?
      (-> "Email"
          ("Open mailbox" (email-open-mailbox))
          ("Retrieve email" (begin (email-pop) (email-open-inbox)))
          ---
          ("Pop server settings" (interactive email-settings))))
  (-> "Project"
      (link project-manage-menu))
  (if (agents-here?)
      (-> "Agents"
          (link agents-menu)))
  (-> "Statistics"
      ("Count characters" (show-character-count))
      ("Count words" (show-word-count))
      ("Count lines" (show-line-count)))
  ---
  (-> "AI engine"
      ("Off" (reset-preference "ai"))
      ---
      (when (has-albert?)
        ("Albert" (set-preference "ai" "albert")))
      (when (has-chatgpt?)
        ("Chat GPT" (set-preference "ai" "chatgpt")))
      (when (has-claude?)
        ("Claude" (set-preference "ai" "claude")))
      (when (has-gemini?)
        ("Gemini" (set-preference "ai" "gemini")))
      (when (has-ollama?)
        ("Ollama" (set-preference "ai" "ollama")))
      (when (has-open-mistral-7b?)
        ("Mistral" (set-preference "ai" "open-mistral-7b")))
      (when (has-openrouter?)
        ("OpenRouter" (set-preference "ai" "openrouter"))))
  (when (and (cpp-has-preference? "ai")
             (selection-active-any?))
    ("Correct" (ai-correct (get-preference "ai")))
    (-> "Translate"
        (link ai-translate-menu))
    ("Ask about the selection" (ai-ask-about-selection (get-preference "ai"))))
  (when (and (cpp-has-preference? "ai")
             (not (selection-active-any?)))
    ("Ask about the document" (ai-ask-about-document (get-preference "ai"))))
  (-> "External AI"
      (when (selection-active-any?)
        ("Copy" (ai-copy))
        ("Cut" (ai-cut)))
      ("Paste" (ai-paste)))
  ---
  ("Create web site" (open-website-builder))
  ;;(-> "Web"
  ;;    ("Create web site" (tmweb-interactive-build))
  ;;    ("Update web site" (tmweb-interactive-update)))
  (-> "Fonts"
      ("Scan disk for fonts" (scan-disk-for-fonts))
      ("Clear font cache" (clear-font-cache))
      ---
      ("Font inspector" (open-font-inspector)))
  (-> "Equation editor" (link tools-equation-editor-menu))
  (-> "Miscellaneous"
      ("Clear undo history" (clear-undo-history))
      ("Save auxiliary data" (toggle-save-aux))
      ("Show key presses" (toggle-show-kbd))
      ---
      (-> "Import selections as"
          (link clipboard-import-preference-menu))
      (-> "Export selections as"
          (link clipboard-export-preference-menu)))
  ---
  ("Database tool" (toggle-preference "database tool"))
  ("Debugging tool" (toggle-preference "debugging tool"))
  ("Developer tool" (toggle-preference "developer tool"))
  ("Linking tool" (toggle-preference "linking tool"))
  ("Presentation tool" (toggle-preference "presentation tool"))
  ("Remote tool" (toggle-preference "remote tool"))
  ("Source macros tool" (toggle-preference "source tool"))
  (-> "Versioning tool"
      ("Automatic" (set-versioning-tool "auto"))
      ("Always" (set-versioning-tool "on"))
      ("Never" (set-versioning-tool "off"))
      ---
      ;; NOTE: also here, since the Version menu is not shown by default
      ;; for documents outside repositories
      (when (and (not (url-rooted-web? (current-buffer)))
                 (not (url-rooted-tmfs? (current-buffer)))
                 (url-exists? (current-buffer))
                 (not (versioning-directory (current-buffer))))
        ("Create Git repository" (git-interactive-init (current-buffer))))
      ("Clone Git repository" (git-interactive-clone))))
