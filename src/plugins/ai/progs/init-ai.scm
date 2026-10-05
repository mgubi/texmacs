
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
  ("openrouter-text-input" "on" noop)
  ("ai raw answer" "on" noop)
  ("chatgpt model" "gpt-5-mini" noop)
  ("gemini model" "gemini-2.5-flash" noop)
  ("open-mistral-7b model" "mistral-small-latest" noop)
  ("claude model" "claude-sonnet-5-5" noop)
  ("openrouter model" "openrouter/auto" noop)
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

(define (ai-store-api-key engine key)
  (if (and (supports-wallet?) (wallet-on?))
      (begin
        (if (== key "")
            (wallet-delete (ai-key-entry engine))
            (wallet-set (ai-key-entry engine) key))
        (set-preference (string-append engine " api key") ""))
      (set-preference (string-append engine " api key") key))
  (reinit-plugin-single "ai"))

;; a wallet which is there but closed: it may hold the keys
(define (ai-wallet-closed?)
  (and (supports-wallet?) (wallet-initialized?) (wallet-off?)))

;; a key given while the wallet is closed: it is opened first, to keep the
;; key there (if it is not opened, the key is kept in the preferences)
(tm-define (ai-set-api-key engine key)
  (when (!= key ai-in-wallet)
    (if (and (!= key "") (ai-wallet-closed?))
        (wallet-dialogue-turn-on
         (lambda (r)
           (when (!= r "Ok")
             (set-message "The key is kept in the preferences, not encrypted"
                          (string-append "Key of " (session-name engine))))
           (ai-store-api-key engine key)))
        (ai-store-api-key engine key))))

(when (supports-wallet?)
  (wallet-add-on-hook (lambda () (reinit-plugin-single "ai"))))

(tm-define (ai-models)
  (list "chatgpt" "claude" "gemini" "open-mistral-7b" "openrouter" "albert"
        "ollama"))

;; the engines which are asked with a key, its environment variable, and the
;; models proposed in the preferences ("" for another one)
(define ai-keyed-engines
  '(("chatgpt" "OPENAI_API_KEY" "gpt-5-mini" "gpt-5" "gpt-5-nano" "")
    ("claude" "ANTHROPIC_API_KEY"
     "claude-sonnet-5-5" "claude-opus-5-5" "claude-haiku-4-5" "")
    ("gemini" "GEMINI_API_KEY"
     "gemini-2.5-flash" "gemini-2.5-pro" "gemini-2.5-flash-lite" "")
    ("open-mistral-7b" "MISTRAL_API_KEY" "mistral-small-latest"
     "mistral-medium-latest" "mistral-large-latest" "open-mistral-7b" "")
    ;; the models of many providers, named provider/model; openrouter/auto
    ;; chooses one for each question
    ("openrouter" "OPENROUTER_API_KEY" "openrouter/auto" "openai/gpt-5-mini"
     "anthropic/claude-sonnet-4.5" "google/gemini-2.5-flash"
     "deepseek/deepseek-chat" "google/gemini-2.5-flash-image" "")))

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

;; a GET request, its answer as text ("" when there is none): http-get
;; (web_files.cpp), made by the browser in a web browser, by Qt, by libcurl
;; or else by the curl program; the headers (the key) stay out of any
;; command line but with the curl program
(define (ai-http-get url headers)
  (http-get url (append-map (lambda (h) (list (car h) (cdr h))) headers)))

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
          ((== name "openrouter")
           (list "https://openrouter.ai/api/v1/models"
                 (list (cons "Authorization" (string-append "Bearer " key)))))
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
                           ((and (== name "chatgpt")
                                 (or (string-starts? id "gpt-image")
                                     (string-starts? id "dall-e-3")))
                            #t) ; they draw pictures
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
                           ((== name "openrouter")
                            ;; those which write, or draw
                            (with o (json-items
                                     (json-ref (json-ref m "architecture")
                                               "output_modalities"))
                              (or (null? o) (in? "text" o) (in? "image" o))))
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
               (l (if t (sort (ai-models-of name t) string<=?) (list)))
               ;; (the model which chooses one, not in the list)
               (l (if (and (== name "openrouter") (nnull? l))
                      (cons "openrouter/auto" l) l)))
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
(define (ai-default-model name)
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

;; The model of a session: in the document, as (with "ai-model" m session)
;; around it, else the one of the preferences of the engine (which the
;; sessions without a model of their own, and the folds, ask)
(define (ai-session-with s)
  ;; the with which holds the variables of the session s (its last child)
  (with w (and s (tree-up s))
    (and w (tree-is? w 'with) (odd? (tree-arity w))
         (== (tree-index s) (- (tree-arity w) 1))
         w)))

(define (ai-session-var-index w var)
  (let loop ((i 0))
    (cond ((>= (+ i 1) (tree-arity w)) #f)
          ((and (tree-atomic? (tree-ref w i))
                (== (tree->string (tree-ref w i)) var)) i)
          (else (loop (+ i 2))))))

;; a variable of a session (ai-model, ai-document), #f when it has none
(define (ai-session-var s var)
  (let* ((w (ai-session-with s))
         (i (and w (ai-session-var-index w var))))
    (and i (tree-atomic? (tree-ref w (+ i 1)))
         (tree->string (tree-ref w (+ i 1))))))

(define (ai-set-session-var s var val)
  (let* ((w (ai-session-with s))
         (i (and w (ai-session-var-index w var))))
    (cond (i (tree-assign (tree-ref w (+ i 1)) val))
          (w (tree-insert! w (- (tree-arity w) 1) (list var val)))
          (else (tree-insert-node! s 2 `(with ,var ,val))))))

(define (ai-tree-model s name)
  (or (ai-session-var s "ai-model") (ai-default-model name)))

;; the session of the engine at the cursor, if any
(define (ai-cursor-session name)
  (with s (tree-innermost 'session)
    (and s (tree-atomic? (tree-ref s 0))
         (== (tree->string (tree-ref s 0)) name)
         s)))

(define (ai-session-model name)
  (ai-tree-model (ai-cursor-session name) name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The instructions of the engines (their system prompt): a text file of the
;; user for each engine, which the preferences open, else the default one,
;; which tells how to write LaTeX which TeXmacs takes well
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ai-default-instructions-text
  (string-append
   "You are a chatbot inside GNU TeXmacs, a scientific editor. Your answer "
   "is converted from LaTeX into a TeXmacs document: it is not compiled by "
   "LaTeX, so only its structure and its mathematics count.\n"
   "\n"
   "Write the answer as one complete LaTeX document: \\documentclass{article}, "
   "the preamble, \\begin{document}, the answer, \\end{document}. No Markdown, "
   "and no code fences around the document.\n"
   "\n"
   "- Structure: \\section*, \\subsection*, \\paragraph, the environments "
   "itemize, enumerate and description (without options in brackets), "
   "tabular, quote, and verbatim for code.\n"
   "- Mathematics: $...$, \\[...\\], and the environments of amsmath "
   "(equation, align, gather, cases, pmatrix...), with the symbols of "
   "amssymb.\n"
   "- Text: \\emph, \\textbf, \\textit, \\texttt, \\footnote, \\href.\n"
   "- Leave out what only matters for printing: the packages geometry, "
   "fontenc, inputenc, lmodern, microtype, enumitem; \\setlength, \\vspace, "
   "\\hfill, \\newpage, minipage, the placement of figures.\n"
   "- Pictures: in TikZ, a tikzpicture (or tikzcd, circuitikz), with "
   "pgfplots for the graphs of functions and data; put \\usepackage{pgfplots}, "
   "\\usetikzlibrary, \\usepgfplotslibrary and \\pgfplotsset in the preamble. "
   "The packages for pictures are pgfplots, tikz-cd, circuitikz and chemfig. "
   "Or in SVG: a complete svg element, with its xmlns attribute, inside "
   "\\begin{verbatim}...\\end{verbatim}.\n"
   "- Images in PNG or JPEG (an artistic rendition, a painting, a "
   "photograph): if you can make images, make one; it is shown with the "
   "answer. An image you have as data goes in "
   "\\includegraphics{data:image/png;base64,...} (or image/jpeg). Do not "
   "write the data of an image which you did not make.\n"
   "- Do not define new commands, and use no other packages than those "
   "above.\n"
   "- Answer in the language of the question.\n"))

(tm-define (ai-default-instructions name)
  ai-default-instructions-text)

(define (ai-instructions-file name)
  (string-append "$TEXMACS_HOME_PATH/system/ai/" name "-instructions.txt"))

;; the instructions of an engine (text in UTF-8), for ai.cpp
(tm-define (ai-instructions name)
  (with u (ai-instructions-file name)
    (if (url-exists? u) (string-load u) (ai-default-instructions name))))

;; the file of the instructions, in a new tab (made from the default ones
;; the first time): saving it changes them
(tm-define (ai-edit-instructions name)
  (with u (ai-instructions-file name)
    (when (not (url-exists? u))
      (with d "$TEXMACS_HOME_PATH/system/ai"
        (when (not (url-exists? d)) (system-mkdir d)))
      (string-save (ai-default-instructions name) u))
    (load-buffer-in-new-window u)))

(tm-define (ai-reset-instructions name)
  (with u (ai-instructions-file name)
    (when (url-exists? u) (system-remove u))
    (set-message "The default instructions are used" (session-name name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The conversation of a session, sent with a question as its context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ai-io-tags
  '(unfolded-io folded-io unfolded-io-text folded-io-text
    unfolded-io-math folded-io-math))

(define (ai-context-size name)
  (with s (get-preference (string-append name " context size"))
    (if (and (string? s) (string->number s)) (string->number s) 10)))

;; the text of a field (as the input is sent: LaTeX, in UTF-8); an answer
;; is in text mode (ai_latex_output), which would be \text{...}
(define (ai-unwrap x)
  (cond ((and (tm-func? x 'document 1)) (ai-unwrap (cadr x)))
        ((and (tm-func? x 'with 3) (== (cadr x) "mode") (== (caddr x) "text"))
         (ai-unwrap (cadddr x)))
        (else x)))

;; the answer as it came, if it is kept in the field (ai.cpp, ai_raw_fold)
(define (ai-raw-text x)
  (cond ((not (pair? x)) #f)
        ((and (tm-func? x 'with 3) (== (cadr x) "ai-raw"))
         (with code (select (cadddr x) '(:* verbatim-code 0))
           (and (nnull? code)
                (with d (car code)
                  ;; (each line converted: a newline is a character of Cork;
                  ;; as source code, which keeps ... as it is)
                  (string-recompose
                   (map (lambda (l) (if (string? l) (cork->sourcecode l) ""))
                        (if (tm-func? d 'document) (cdr d) (list d)))
                   "\n")))))
        (else (list-or (map ai-raw-text (cdr x))))))

(define (ai-field-text name t)
  (let* ((x (tm->stree t))
         (raw (ai-raw-text x)))
    (or raw
        (with s (ai-serialize name (ai-unwrap x))
          (if (string? s) s "")))))

;; The questions and answers of the fields of the session above the one
;; which is evaluated, the last ones (ai-context-size), as a list (question
;; answer ...); #f when the evaluation is not in a session (a fold): the
;; engine then has the last exchanges kept in memory (ai.cpp)
;; the field of the session which is evaluated (its output is where the
;; answer goes), #f for a fold
(define (ai-pending-field name chat)
  (let* ((l (pending-ref name chat))
         (item (and (nnull? l) (car l)))
         (p (and item (>= (length item) 4) (third item))))
    (and p (not (tree? p))
         (let ((out (catch #t (lambda () (tree-pointer->tree p))
                      (lambda args #f))))
           (and (tree? out) (tree-up out))))))

;; The model of a request: that of the session of the field which is
;; evaluated (ai-request sets it while ai.cpp makes the request, and
;; ai_model_name asks it), else the one of the preferences
(define ai-model-of-request "")
(tm-define (ai-model-override) ai-model-of-request)

(tm-define (ai-request-prepare name chat)
  (let* ((field (ai-pending-field name chat))
         (doc (and field (tree-up field)))
         (s (and doc (tree-up doc)))
         (s (and s (tree-is? s 'session) s)))
    (set! ai-model-of-request (or (and s (ai-session-var s "ai-model")) ""))
    (set! ai-document-of-request
          (if (and s (== (ai-session-var s "ai-document") "true"))
              (ai-document-latex s) ""))))

(tm-define (ai-request-done)
  (set! ai-model-of-request "")
  (set! ai-document-of-request ""))

;; The document as the context of a session (its variable ai-document): the
;; document which holds the session, as LaTeX, without the sessions of the
;; chatbots; ai.cpp gives it after the instructions (to Claude as a block of
;; its system prompt which it caches)
(define ai-document-of-request "")
(tm-define (ai-document-override) ai-document-of-request)

(define ai-document-max 400000) ; characters

(define (ai-buffer-body t)
  (if (or (not (tree-up t)) (tree-is-buffer? t)) t (ai-buffer-body (tree-up t))))

(define (ai-session-stree? x)
  (or (and (pair? x) (== (car x) 'session) (>= (length x) 2)
           (in? (cadr x) (ai-models)))
      (and (pair? x) (== (car x) 'with) (ai-session-stree? (cAr x)))))

(define (ai-strip-sessions x)
  (cond ((not (pair? x)) x)
        ((ai-session-stree? x) "")
        ((== (car x) 'document)
         (with l (map ai-strip-sessions
                      (list-filter (cdr x) (lambda (y) (not (ai-session-stree? y)))))
           (cons 'document (if (null? l) (list "") l))))
        (else (cons (car x) (map ai-strip-sessions (cdr x))))))

(define (ai-document-latex s)
  (let* ((body (ai-buffer-body s))
         (x (ai-strip-sessions (tree->stree body)))
         (l (catch #t
              (lambda () (convert (stree->tree x) "texmacs-tree" "latex-snippet"))
              (lambda args ""))))
    (if (> (string-length l) ai-document-max)
        (string-append (substring l 0 ai-document-max) "\n[...]")
        l)))

(tm-define (ai-session-document? lan)
  (with s (ai-cursor-session lan)
    (and s (== (ai-session-var s "ai-document") "true"))))

(tm-define (ai-toggle-session-document lan)
  (with s (ai-cursor-session lan)
    (when s
      (with on? (== (ai-session-var s "ai-document") "true")
        (ai-set-session-var s "ai-document" (if on? "false" "true"))
        (set-message (if on? "The questions are sent without the document"
                         "The questions are sent with the document")
                     (session-name lan))))))

(tm-define (ai-session-context name chat)
  (let* ((field (ai-pending-field name chat))
         (doc (and field (tree-up field))))
    (if (not field) #f
        (let ()
          (if (not (and doc (tm-func? doc 'document))) #f
              (let* ((i (tree-index field))
                     (pairs
                      (append-map
                       (lambda (j)
                         (with f (tree-ref doc j)
                           (if (not (tree-in? f ai-io-tags)) (list)
                               (let ((q (ai-field-text name (tree-ref f 1)))
                                     (a (ai-field-text name (tree-ref f 2))))
                                 (if (or (== q "") (== a "")
                                         (string-starts? a "Error:"))
                                     (list)
                                     (list (list q a)))))))
                       (.. 0 i)))
                     (n (ai-context-size name))
                     (kept (if (> (length pairs) n)
                               (list-tail pairs (- (length pairs) n))
                               pairs)))
                (apply append kept)))))))

;; the first line of a session: the engine and its model
(define (ai-banner lan)
  (when (and (or (assoc lan ai-keyed-engines) (== lan "albert"))
             (not (ai-has-key? lan)))
    (ai-ask-key lan))
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

(define (ai-key-env* name)
  (if (== name "albert") "ALBERT_API_KEY" (ai-key-env name)))

(define (ai-has-key? name)
  (!= (ai-api-key name (ai-key-env* name)) ""))

;; An engine without a key is there all the same (in the submenu AI of
;; Insert > Session): a session of it asks for the key when it starts, and
;; when a question is asked without one (ai-key-missing in ai-batch.scm):
;; the wallet is opened if it is closed (it may hold the key), else the
;; preferences of the engine, where the key is given.
(define (ai-available? name) #t)

(define ai-asking-key? #f)

(define (ai-ask-key name)
  (when (not ai-asking-key?)
    (set! ai-asking-key? #t)
    (delayed
      (:idle 10)
      (set! ai-asking-key? #f)
      (if (ai-wallet-closed?)
          (wallet-dialogue-turn-on
           (lambda (r)
             (when (not (ai-has-key? name))
               (open-plugin-preferences name))))
          (open-plugin-preferences name)))))

;; the message of a question asked without a key (and the key asked for),
;; #f when there is one
(tm-define (ai-key-missing name)
  (and (or (assoc name ai-keyed-engines) (== name "albert"))
       (not (ai-has-key? name))
       (begin
         (ai-ask-key name)
         (string-append
          "No API key for " (session-name name) ": "
          (if (ai-wallet-closed?)
              "open the wallet, which may hold it"
              "give it in the preferences of the session")
          ", then ask again."))))

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
             (ai-update-models-message name))))
        (item (text "Context")
          (enum (set-preference (string-append name " context size") answer)
                '("10" "5" "20" "50" "0" "")
                (number->string (ai-context-size name)) "5em"))
        (item (text "Instructions")
          (explicit-buttons
            ("Edit" (ai-edit-instructions name)) // //
            ("Default" (ai-reset-instructions name))))))
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
           (ai-update-models-message "ollama"))))
      (item (text "Instructions")
        (explicit-buttons
          ("Edit" (ai-edit-instructions "ollama")) // //
          ("Default" (ai-reset-instructions "ollama")))))
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
		(get-preference "albert chat history size") "6em"))
        (item (text "Instructions")
          (explicit-buttons
            ("Edit" (ai-edit-instructions "albert")) // //
            ("Default" (ai-reset-instructions "albert"))))))
    === === ===)
  (with textual-input (string-append name "-text-input")
    (aligned
      (meti (hlist // (text "Textual input"))
	(toggle (set-boolean-preference textual-input answer)
		(get-boolean-preference textual-input)))
      (meti (hlist // (text "Show the answer as it came"))
	(toggle (set-boolean-preference "ai raw answer" answer)
		(get-boolean-preference "ai raw answer"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ChatGPT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All the engines are asked by HTTP requests (ai.cpp), with a key of the
;; wallet, of the preferences or of the environment (ai-api-key)

(tm-define (has-chatgpt?)
  (ai-available? "chatgpt"))

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
;; OpenRouter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-openrouter?)
  (ai-available? "openrouter"))

(plugin-configure openrouter
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "OpenRouter")
  (:require (has-openrouter?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Claude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-claude?)
  (ai-available? "claude"))

(plugin-configure claude
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Claude")
  (:require (has-claude?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

(tm-define (has-gemini?)
  (ai-available? "gemini"))

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
  (ai-available? "open-mistral-7b"))

(plugin-configure open-mistral-7b
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Mistral")
  (:require (has-open-mistral-7b?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The model, in the focus bar of a session: a menu which shows it and
;; changes it (the preference of the engine, which the next questions ask)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-focus-models lan)
  (let* ((l (list-filter (cond ((== lan "ollama") (ollama-models))
                               ((== lan "albert") (albert-variants))
                               (else (ai-model-variants lan)))
                         (lambda (m) (and (string? m) (!= m "")))))
         (m (ai-session-model lan)))
    ;; with the current one, which may not be in the list
    (if (or (== m "") (in? m l)) l (cons m l))))

;; the model in the first line of a session (ai-banner), while it has no
;; answer yet: after one, it stays the model with which it began (each
;; answer says the model which gave it, in the fold of the answer as it
;; came)
(define (ai-update-banner s m)
  (let* ((body (tree-ref s 2))
         (n (if (tree-is? body 'document) (tree-arity body) 0)))
    (when (and (> n 0) (tree-is? (tree-ref body 0) 'output)
               (list-and (map (lambda (i)
                                (not (tree-in? (tree-ref body i) ai-io-tags)))
                              (.. 1 n))))
      (with v (tree-search (tree-ref body 0) (lambda (t) (tree-is? t 'verbatim)))
        (when (and (pair? v) (tree-atomic? (tree-ref (car v) 0)))
          (tree-assign (tree-ref (car v) 0) m))))))

(tm-define (ai-set-session-model lan m)
  ;; the session at the cursor, else the default model of the engine
  (with s (ai-cursor-session lan)
    (if (not s)
        (set-preference (string-append lan " model") m)
        (begin
          (ai-update-banner s m)
          (ai-set-session-var s "ai-model" m))))
  (set-message (string-append "The next questions ask " m)
               (string-append "Model of " (session-name lan)))
  (refresh-now "ai-model-list"))

;; provider/model: the provider of a model of OpenRouter
(define (ai-model-provider m)
  (with i (string-index m #\/)
    (if i (substring m 0 i) "")))

;; the model chosen, and with start? a new session of it, which asks it
(define (ai-choose-model lan m start?)
  (when start? (make-session lan "default"))
  (ai-set-session-model lan m))

(tm-menu (focus-ai-model-items lan l start?)
  (for (m l)
    ((check (eval m) "v" (== (ai-session-model lan) m))
     (ai-choose-model lan m start?))))

(tm-menu (ai-model-choices lan start?)
  (with l (ai-focus-models lan)
    (if (<= (length l) 30)
        (dynamic (focus-ai-model-items lan l start?)))
    (if (> (length l) 30)
        ;; many models (OpenRouter): by provider
        (for (p (list-remove-duplicates (map ai-model-provider l)))
          (-> (eval (if (== p "") "Others" p))
              (dynamic (focus-ai-model-items
                        lan (list-filter l (lambda (m)
                                             (== (ai-model-provider m) p)))
                        start?))))))
  ---
  ("Other model"
   (interactive (lambda (m) (when (!= m "") (ai-choose-model lan m start?)))
     (list "Model" "string" (ai-session-model lan)))))

(tm-menu (focus-ai-model-menu lan)
  (dynamic (ai-model-choices lan #f))
  (if (and (!= lan "albert") (ai-models-request lan))
      ("Update the list of models" (ai-update-models-message lan)))
  ---
  ((check "Send the document as context" "v" (ai-session-document? lan))
   (ai-toggle-session-document lan))
  ("Preferences" (open-plugin-preferences lan)))

(tm-menu (focus-ai-icons lan)
  (mini #t
    (if (== lan "albert")
      //
      (=> (eval (get-preference "albert ai-agents interlocutor"))
          (dynamic (focus-ai-agents-interlocutor lan))))
    //
    (=> (balloon (eval (ai-session-model lan)) "Model of the session")
        (dynamic (focus-ai-model-menu lan)))))

;; (not an overloading of focus-extra-icons: the plug-in is loaded again
;; when a key is given, and each loading would add its icons)
(for-each (lambda (name) (set-session-focus-menu! name focus-ai-icons))
          (ai-models))

;; Ask about the selection: a session of the engine after the paragraph of
;; the selection, whose input holds the selection, for the question which is
;; typed after it
(tm-define (ai-ask-about-selection lan)
  (when (selection-active-any?)
    (with t (tree-copy (selection-tree))
      (selection-cancel)
      (go-end-paragraph)
      (insert-return)
      (make-session lan "default")
      (insert t)
      (insert-return))))

;; Ask about the document: a session at the cursor which sends the document
(tm-define (ai-ask-about-document lan)
  (make-session lan "default")
  (with s (ai-cursor-session lan)
    (when s (ai-set-session-var s "ai-document" "true"))))

;; the chatbots in a submenu AI of Insert > Session, each a submenu of its
;; models, which starts a session of the one chosen
(tm-menu (ai-insert-session-menu lan)
  (-> (eval (session-name lan))
      (dynamic (ai-model-choices lan #t))))

(for-each (lambda (name)
            (set-session-group! name "AI")
            (set-session-insert-menu! name ai-insert-session-menu))
          (ai-models))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Albert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-ai-agents-interlocutor ai)
  (with pref (string-append ai " ai-agents interlocutor")
    (for (s (cons "default" (ai-agents-interlocutors)))
      ((check (eval s) "v" (== (get-preference pref) s))
       (set-preference pref s)))))


(tm-define (has-albert?)
  (ai-available? "albert"))

(plugin-configure albert
  ;; before :require, so that its key can be given in its preferences
  (:preferences #t)
  (:session "Albert")
  (:require (has-albert?))
  (:request ,ai-request ,ai-result)
  (:serializer ,ai-serialize))
