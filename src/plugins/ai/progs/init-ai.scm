
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-ai.scm
;; DESCRIPTION : placeholder for various AI plugins
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ollama command line tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ollama-models)
  (let* ((ret (eval-system "ollama list"))
         (lines** (string-decompose ret "\n"))
         (lines* (if (null? lines**) lines** (cdr lines**)))
         (lines (if (and (nnull? lines*) (== (cAr lines*) ""))
                    (cDr lines*) lines*))
         (models (map (lambda (l) (car (string-decompose l " "))) lines)))
    (sort models string<=?)))

(tm-define (ollama-model-variants model)
  (with models (ollama-models)
    (append-map (lambda (m)
                  (if (string-starts? m model) (list m) (list)))
                models)))

(tm-define (ollama-default-model)
  (let* ((l (ollama-models))
         (llama (ollama-model-variants "llama")))
    (cond ((nnull? llama) (car llama))
          ((nnull? l) (car l))
          (else "llama3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("ollama server" "localhost" noop)
  ("ollama port" "11434" noop)
  ("ollama model" "default" noop)
  ("ollama-text-input" "on" noop)
  ("chatgpt-text-input" "on" noop)
  ("gemini-text-input" "on" noop)
  ("open-mistral-7b-text-input" "on" noop)
  ("albert api key" "" noop)
  ("albert-text-input" "on" noop)
  ("albert ai-agents corrector" "default" noop)
  ("albert ai-agents interlocutor" "default" noop)
  ("albert ai-agents translator" "default" noop)
  ("albert model" "openweight-large" noop))

(with key (getenv "ALBERT_API_KEY")
  (when key (set-preference "albert api key" key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API keys: in the wallet when it is on, else a preference, else the
;; environment. A key given while the wallet is on goes to the wallet (and
;; the preference is emptied), so that it is only kept encrypted; the plug-ins
;; are set up again when a key changes or the wallet is turned on, since
;; which engines are there depends on their keys.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-key-entry engine) (list "ai" engine "api key"))

(define (ai-wallet-key engine)
  (and (supports-wallet?) (wallet-on?)
       (with k (wallet-get (ai-key-entry engine))
         (and (string? k) (!= k "") k))))

(tm-define (ai-api-key engine env)
  (or (ai-wallet-key engine)
      (with p (get-preference (string-append engine " api key"))
        (and (string? p) (!= p "") p))
      (with e (getenv env)
        (and e (!= e "") e))
      ""))

(define ai-in-wallet "(in the wallet)")

(tm-define (ai-api-key-shown engine)
  (if (ai-wallet-key engine) ai-in-wallet
      (get-preference (string-append engine " api key"))))

(tm-define (ai-set-api-key engine key)
  (when (!= key ai-in-wallet)
    (if (and (supports-wallet?) (wallet-on?))
        (begin
          (if (== key "")
              (wallet-delete (ai-key-entry engine))
              (wallet-set (ai-key-entry engine) key))
          (set-preference (string-append engine " api key") ""))
        (set-preference (string-append engine " api key") key))
    (reinit-plugin-single "ai")))

(when (supports-wallet?)
  (wallet-add-on-hook (lambda () (reinit-plugin-single "ai"))))

(tm-define (ai-models)
  (list "chatgpt" "gemini" "open-mistral-7b" "albert" "ollama"))

(tm-define (albert-variants)
  (list "openweight-large" "openweight-medium" "openweight-small" ""))

(tm-widget (plugin-preferences-widget name)
  (:require (in? name (ai-models)))
  (assuming (== name "ollama")
    (aligned
      (item (text "Ollama server")
        (enum (set-preference "ollama server" answer) '("localhost" "")
              (get-preference "ollama server") "16em"))
      (item (text "Ollama port")
        (enum (set-preference "ollama port" answer) '("11434" "")
              (get-preference "ollama port") "16em"))
      (item (text "Ollama model")
        (enum (set-preference "ollama model" answer) (ollama-models)
              (get-preference "ollama model") "16em")))
    === === ===)
  (assuming (== name "albert")
    (with model (string-append name " model")
      (aligned
	(item (text "API key")
          (enum (ai-set-api-key "albert" answer)
                (list (ai-api-key-shown "albert")
		      (or (getenv "ALBERT_API_KEY") ""))
		(ai-api-key-shown "albert") "11em"))
        (item (text model)
          (enum (set-preference model answer)
		(albert-variants)
                (get-preference model) "11em"))
        (assuming (nnull? (ai-agents-correctors))
	  (item (text "Corrector agent")
	    (enum (set-preference "albert ai-agents corrector" answer)
		  (cons "default" (ai-agents-correctors))
		  (get-preference "albert ai-agents corrector") "11em")))
        (assuming (nnull? (ai-agents-interlocutors))
	  (item (text "Interlocutor agent")
	    (enum (set-preference "albert ai-agents interlocutor" answer)
		  (cons "default" (ai-agents-interlocutors))
		  (get-preference "albert ai-agents interlocutor") "11em")))
	(assuming (nnull? (ai-agents-translators))
	  (item (text "Translator agent")
	    (enum (set-preference "albert ai-agents translator" answer)
		  (cons "default" (ai-agents-translators))
		  (get-preference "albert ai-agents translator") "11em")))
	(item (text "Chat history size")
	  (enum (set-preference "albert chat history size" answer)
		'("10" "5" "4" "3" "2" "1" "0" "")
		(get-preference "albert chat history size") "6em"))))
    === === ===)
  (with textual-input (string-append name "-text-input")
    (aligned
      (meti (hlist // (text "Textual input"))
	(toggle (set-boolean-preference textual-input answer)
		(get-boolean-preference textual-input))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-chatgpt?)
  (and (url-exists-in-path? "openai")
       (getenv "OPENAI_API_KEY")
       (!= (getenv "OPENAI_API_KEY") "")))

(plugin-configure chatgpt
  (:require (has-chatgpt?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "ChatGPT")
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gemini
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-gemini?)
  (and (getenv "GEMINI_API_KEY")
       (!= (getenv "GEMINI_API_KEY") "")))

(plugin-configure gemini
  (:require (has-gemini?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Gemini")
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ollama
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-ollama?)
  (url-exists-in-path? "ollama"))

(plugin-configure ollama
  (:require (has-ollama?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Ollama")
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mistral
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-open-mistral-7b?)
  (and (getenv "MISTRAL_API_KEY")
       (!= (getenv "MISTRAL_API_KEY") "")))

(plugin-configure open-mistral-7b
  (:require (has-open-mistral-7b?))
  (:cmdline ,ai-cmdline ,ai-result)
  (:preferences #t)
  (:session "Mistral 7B")
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Albert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-ai-agents-interlocutor ai)
  (with pref (string-append ai " ai-agents interlocutor")
    (for (s (cons "default" (ai-agents-interlocutors)))
      ((check (eval s) "v" (== (get-preference pref) s))
       (set-preference pref s)))))

(define (focus-session-language*)
  (string-downcase (focus-session-language)))

(tm-menu (focus-extra-icons t)
  (:require (in? (focus-session-language*) (list "albert"))) ;;(ai-models)))
  (dynamic (former t))
  (mini #t
    //
    (=> (eval (get-preference (string-append (focus-session-language*)
					     " ai-agents interlocutor")))
        (dynamic (focus-ai-agents-interlocutor (focus-session-language*))))))

(tm-define (has-albert?)
  (!= (ai-api-key "albert" "ALBERT_API_KEY") ""))

(plugin-configure albert
  (:require (has-albert?))
  (:request ,ai-request ,ai-result)
  (:preferences #t)
  (:session "Albert")
  (:serializer ,ai-serialize))
