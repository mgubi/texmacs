
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

;; in a web browser there is no ollama program to ask: the models are those
;; which its server gave (ai-update-models, a button of the preferences)
(tm-define (ollama-models)
  (if (defined? 'web-javascript) (ai-models-downloaded "ollama")
      (ollama-models-listed)))

(define (ollama-models-listed)
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
  ("claude-text-input" "on" noop)
  ("chatgpt model" "gpt-5-mini" noop)
  ("gemini model" "gemini-2.5-flash" noop)
  ("open-mistral-7b model" "mistral-small-latest" noop)
  ("claude model" "claude-sonnet-5-5" noop)
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

;; the key of the preferences ("default" when there is none)
(define (ai-preference-key engine)
  (with p (get-preference (string-append engine " api key"))
    (and (string? p) (!= p "") (!= p "default") p)))

(tm-define (ai-api-key engine env)
  (or (ai-wallet-key engine)
      (ai-preference-key engine)
      (with e (getenv env)
        (and e (!= e "") e))
      ""))

(define ai-in-wallet "(in the wallet)")

(tm-define (ai-api-key-shown engine)
  (if (ai-wallet-key engine) ai-in-wallet
      (or (ai-preference-key engine) "")))

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
  (list "chatgpt" "claude" "gemini" "open-mistral-7b" "albert" "ollama"))

;; the engines which are asked with a key, its environment variable, and the
;; models proposed in the preferences ("" for another one)
(define ai-keyed-engines
  '(("chatgpt" "OPENAI_API_KEY" "gpt-5-mini" "gpt-5" "gpt-5-nano" "")
    ("claude" "ANTHROPIC_API_KEY"
     "claude-sonnet-5-5" "claude-opus-5-5" "claude-haiku-4-5" "")
    ("gemini" "GEMINI_API_KEY"
     "gemini-2.5-flash" "gemini-2.5-pro" "gemini-2.5-flash-lite" "")
    ("open-mistral-7b" "MISTRAL_API_KEY" "mistral-small-latest"
     "mistral-medium-latest" "mistral-large-latest" "open-mistral-7b" "")))

(define (ai-key-env name)
  (with e (assoc name ai-keyed-engines) (if e (cadr e) "")))

;; the models proposed: those which the engine gave last (ai-update-models),
;; else a few known ones; "" for another one
(define (ai-model-variants name)
  (with l (ai-models-downloaded name)
    (if (nnull? l) (append l (list ""))
        (with e (assoc name ai-keyed-engines) (if e (cddr e) (list ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The models which an engine has, asked to it (with its key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-quote-js s)
  (string-append "\"" (string-replace (string-replace s "\\" "\\\\")
                                       "\"" "\\\"") "\""))

(define (ai-quote-shell s)
  (string-append "'" (string-replace s "'" "'\\''") "'"))

;; a GET request, its answer as text ("" when there is none): by the browser
;; in a web browser (a synchronous request), else by curl
(define (ai-http-get url headers)
  (if (defined? 'web-javascript)
      (web-javascript
       (string-append
        "(function () { var x = new XMLHttpRequest (); "
        "x.open ('GET', " (ai-quote-js url) ", false); "
        (apply string-append
               (map (lambda (h)
                      (string-append "x.setRequestHeader ("
                                     (ai-quote-js (car h)) ", "
                                     (ai-quote-js (cdr h)) "); "))
                    headers))
        "try { x.send (); } catch (e) { return ''; } "
        "return x.responseText; }) ()"))
      (eval-system
       (string-append
        "curl --silent "
        (apply string-append
               (map (lambda (h)
                      (string-append "-H " (ai-quote-shell
                                            (string-append (car h) ": " (cdr h)))
                                     " "))
                    headers))
        (ai-quote-shell url)))))

;; a value of a JSON object (as json->tree reads it)
(define (json-ref t key)
  (and (pair? t) (== (car t) 'attr)
       (let loop ((l (cdr t)))
         (cond ((or (null? l) (null? (cdr l))) #f)
               ((== (car l) key) (cadr l))
               (else (loop (cddr l)))))))

(define (json-items t)
  (if (and (pair? t) (== (car t) 'tuple)) (cdr t) (list)))

(define (json-string t) (if (string? t) t ""))

(define (ai-models-request name)
  (with key (ai-api-key name (ai-key-env name))
    (cond ((== name "chatgpt")
           (list "https://api.openai.com/v1/models"
                 (list (cons "Authorization" (string-append "Bearer " key)))))
          ((== name "claude")
           (list "https://api.anthropic.com/v1/models?limit=100"
                 (list (cons "x-api-key" key)
                       (cons "anthropic-version" "2023-06-01")
                       (cons "anthropic-dangerous-direct-browser-access"
                             "true"))))
          ((== name "gemini")
           (list "https://generativelanguage.googleapis.com/v1beta/models?pageSize=1000"
                 (list (cons "x-goog-api-key" key))))
          ((== name "open-mistral-7b")
           (list "https://api.mistral.ai/v1/models"
                 (list (cons "Authorization" (string-append "Bearer " key)))))
          ((== name "albert")
           (list "https://albert.api.etalab.gouv.fr/v1/models"
                 (list (cons "Authorization"
                             (string-append "Bearer "
                                            (ai-api-key "albert"
                                                        "ALBERT_API_KEY"))))))
          ((== name "ollama")
           (list (string-append "http://" (get-preference "ollama server")
                                ":" (get-preference "ollama port")
                                "/api/tags")
                 (list)))
          (else #f))))

;; the models of the answer which can chat
(define (ai-models-of name t)
  (cond ((== name "gemini")
         (map (lambda (m)
                (with n (json-string (json-ref m "name"))
                  (if (string-starts? n "models/") (string-drop n 7) n)))
              (list-filter
               (json-items (json-ref t "models"))
               (lambda (m)
                 (in? "generateContent"
                      (json-items (json-ref m "supportedGenerationMethods")))))))
        ((== name "ollama")
         (map (lambda (m) (json-string (json-ref m "name")))
              (json-items (json-ref t "models"))))
        (else
         (with l (map (lambda (m) (cons (json-string (json-ref m "id")) m))
                      (json-items (json-ref t "data")))
           (map car
                (list-filter
                 l
                 (lambda (p)
                   (let ((id (car p)) (m (cdr p)))
                     (cond ((== id "") #f)
                           ((== name "chatgpt")
                            (and (or (string-starts? id "gpt-")
                                     (and (string-starts? id "o")
                                          (> (string-length id) 1)
                                          (char-numeric? (string-ref id 1))))
                                 (not (list-or
                                       (map (lambda (w) (string-contains? id w))
                                            '("audio" "realtime" "tts"
                                              "transcribe" "image" "search"
                                              "embedding" "instruct"))))))
                           ((== name "open-mistral-7b")
                            (with c (json-ref m "capabilities")
                              (or (not c)
                                  (== (json-ref c "completion_chat") "true"))))
                           (else #t))))))))))

(define (ai-models-downloaded name)
  (with s (get-preference (string-append name " models"))
    (if (or (not (string? s)) (== s "") (== s "default")) (list)
        (string-decompose s " "))))

;; asks the engine its models, keeps them in the preferences; the number of
;; models, or a text which says why there are none
(tm-define (ai-update-models name)
  (with r (ai-models-request name)
    (if (not r) "this engine gives no list of its models"
        (let* ((ans (ai-http-get (car r) (cadr r)))
               (t (if (== ans "") #f
                      (catch #t (lambda () (tree->stree (json->tree ans)))
                        (lambda args #f))))
               (l (if t (sort (ai-models-of name t) string<=?) (list))))
          (cond ((nnull? l)
                 (set-preference (string-append name " models")
                                 (string-recompose l " "))
                 (length l))
                ((== ans "") "no answer (network, key, or the site refuses the page)")
                ((and t (json-ref t "error"))
                 (with e (json-ref t "error")
                   (or (and (pair? e) (json-string (json-ref e "message")))
                       (json-string e))))
                (else "no models in the answer"))))))

;; the model which a session of the engine asks (as ai.cpp chooses it)
(define (ai-session-model name)
  (let* ((pref (cond ((== name "ollama") "ollama model")
                     (else (string-append name " model"))))
         (m (get-preference pref))
         (known (with e (assoc name ai-keyed-engines)
                  (and e (pair? (cddr e)) (caddr e)))))
    (cond ((and (string? m) (!= m "") (!= m "default")) m)
          ((== name "ollama") (ollama-default-model))
          ((== name "albert") name)
          (known known)
          (else ""))))

;; the first line of a session: the engine and its model
(define (ai-banner lan)
  (with m (ai-session-model lan)
    `(document
       (concat (strong ,(session-name lan))
               ,(if (== m "") "" `(concat ", model " (verbatim ,m)))))))

(for-each (lambda (name) (set-request-banner! name ai-banner)) (ai-models))

(define (ai-update-models-message name)
  (with r (ai-update-models name)
    (set-message (if (number? r)
                     (string-append (number->string r) " models")
                     (string-append "No models: " r))
                 (string-append "Models of " (session-name name)))
    (refresh-now "ai-model-list")))

(define (ai-has-key? name)
  (!= (ai-api-key name (ai-key-env name)) ""))

(tm-define (albert-variants)
  (list "openweight-large" "openweight-medium" "openweight-small" ""))

(tm-widget (plugin-preferences-widget name)
  (:require (in? name (ai-models)))
  (assuming (assoc name ai-keyed-engines)
    (with model (string-append name " model")
      (aligned
        (item (text "API key")
          (enum (ai-set-api-key name answer)
                (list (ai-api-key-shown name) "")
                (ai-api-key-shown name) "16em"))
        (item (text "Model")
          (refreshable "ai-model-list"
            (enum (set-preference model answer)
                  (ai-model-variants name)
                  (get-preference model) "16em")))
        (item (text "")
          (explicit-buttons
            ("Update the list of models"
             (ai-update-models-message name))))))
    === === ===)
  (assuming (== name "ollama")
    (aligned
      (item (text "Ollama server")
        (enum (set-preference "ollama server" answer) '("localhost" "")
              (get-preference "ollama server") "16em"))
      (item (text "Ollama port")
        (enum (set-preference "ollama port" answer) '("11434" "")
              (get-preference "ollama port") "16em"))
      (item (text "Ollama model")
        (refreshable "ai-model-list"
          (enum (set-preference "ollama model" answer)
                (append (ollama-models) (list ""))
                (get-preference "ollama model") "16em")))
      (item (text "")
        (explicit-buttons
          ("Update the list of models"
           (ai-update-models-message "ollama")))))
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

;; All the engines are asked by HTTP requests (ai.cpp), with a key of the
;; wallet, of the preferences or of the environment (ai-api-key)

(tm-define (has-chatgpt?)
  (ai-has-key? "chatgpt"))

(plugin-configure chatgpt
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "ChatGPT")
  (:require (has-chatgpt?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gemini
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Claude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-claude?)
  (ai-has-key? "claude"))

(plugin-configure claude
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Claude")
  (:require (has-claude?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

(tm-define (has-gemini?)
  (ai-has-key? "gemini"))

(plugin-configure gemini
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Gemini")
  (:require (has-gemini?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ollama
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in a web browser Ollama is asked where the preferences say (localhost by
;; default), if it lets the page do it (OLLAMA_ORIGINS)
(tm-define (has-ollama?)
  (or (defined? 'web-javascript)
      (url-exists-in-path? "ollama")))

(plugin-configure ollama
  (:require (has-ollama?))
  (:request ,ai-request ,ai-result)
  (:preferences #t)
  (:session "Ollama")
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mistral
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-open-mistral-7b?)
  (ai-has-key? "open-mistral-7b"))

(plugin-configure open-mistral-7b
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Mistral")
  (:require (has-open-mistral-7b?))
  (:request ,ai-request ,ai-result)
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
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Albert")
  (:require (has-albert?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))
