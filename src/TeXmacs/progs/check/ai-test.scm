
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-test.scm
;; DESCRIPTION : Tests of the conversion of the answers of the chatbots
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The answers of the AI engines (ai.cpp, ai_latex_output) as they come from
;; their APIs, converted without a network: the JSON of OpenAI (also that of
;; Mistral, OpenRouter, Ollama), of Claude and of Gemini, a stream of
;; server-sent events, an error of a provider, pictures in SVG and PNG, and
;; the fold of the answer as it came, which names the model. The TikZ
;; pictures are left out: their fold asks the TikZ plug-in for the picture.
;; Also the files attached to the questions: their text, what keeps them
;; from being sent, and the requests which carry them (made, not sent).

(texmacs-module (check ai-test)
  (:use (check check-lib)))

(define (st x) (if (tree? x) (tree->stree x) x))

;; a preference with a value during thunk, in memory only: it is not saved
;; (set-preference would save the preferences at once), and comes back to
;; what it was also when thunk fails
(define (with-preference which val thunk)
  (let ((had? (cpp-has-preference? which))
        (old (cpp-get-preference which "default")))
    (dynamic-wind
      (lambda () (cpp-set-preference which val))
      thunk
      (lambda ()
        (if had? (cpp-set-preference which old)
            (cpp-reset-preference which))))))

(define (answer s engine)
  (st (cpp-ai-latex-output s engine "check")))

(define (openai text)
  (string-append "{\"model\": \"gpt-x\", \"choices\": [{\"message\": "
                 "{\"content\": " (tree->json text) "}}]}"))

(define (claude text)
  (string-append "{\"model\": \"claude-x\", \"content\": [{\"type\": \"text\", "
                 "\"text\": " (tree->json text) "}]}"))

(define (gemini text)
  (string-append "{\"modelVersion\": \"gemini-x\", \"candidates\": [{\"content\": "
                 "{\"parts\": [{\"text\": " (tree->json text) "}]}}]}"))

(define latex-answer
  (string-append "\\documentclass{article}\n\\begin{document}\n"
                 "\\section*{A}\n$\\nu \\ni x$ and \\emph{b}.\n"
                 "\\end{document}"))

(define latex-tree
  '(with "mode" "text"
     (document (section* "A")
               (concat (math "<nu><ni>x") " and " (em "b") "."))))

;; a PNG of one pixel
(define png
  (string-append "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42m"
                 "Nk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="))

(define (find-tag t tag)
  (cond ((and (pair? t) (== (car t) tag)) t)
        ((pair? t) (list-or (map (lambda (x) (find-tag x tag)) (cdr t))))
        (else #f)))

(define (test-engines)
  (check-group "the answers of the engines")
  ;; the same LaTeX document, in the JSON of each API
  (check= (answer (openai latex-answer) "chatgpt") latex-tree)
  (check= (answer (claude latex-answer) "claude") latex-tree)
  (check= (answer (gemini latex-answer) "gemini") latex-tree)
  (check= (answer (openai latex-answer) "openrouter") latex-tree)
  ;; an answer in plain text, in the text font
  (check= (answer (openai "Just text.") "openrouter")
          '(with "mode" "text" "Just text.")))

(define (test-stream)
  (check-group "streamed answers")
  ;; server-sent events; OpenRouter sends comments while it waits
  (check= (answer (string-append
                   ": OPENROUTER PROCESSING\n\n"
                   "data: {\"model\": \"m\", \"choices\": [{\"delta\": "
                   "{\"content\": \"Hello \"}}]}\n\n"
                   "data: {\"choices\": [{\"delta\": "
                   "{\"content\": \"world.\"}}]}\n\n"
                   "data: [DONE]\n\n")
                  "openrouter")
          '(with "mode" "text" "Hello world.")))

(define (test-errors)
  (check-group "errors")
  ;; the error of the provider of the model, which OpenRouter passes on
  (check= (answer (string-append
                   "{\"error\": {\"message\": \"Provider returned error\", "
                   "\"code\": 429, \"metadata\": {\"raw\": "
                   "\"rate-limited upstream\", \"provider_name\": "
                   "\"Google\"}}}")
                  "openrouter")
          "Error: Provider returned error: rate-limited upstream (Google)")
  (check= (answer "{\"error\": {\"message\": \"invalid key\"}}" "chatgpt")
          "Error: invalid key"))

(define (reasoning-fold text)
  `(with "ai-reasoning" "true"
     (folded (with "font-shape" "italic" "The reasoning")
             (with "color" "dark grey" "font-size" "0.84"
               (document ,text)))))

(define (usage-line data text)
  `(with "ai-usage" ,data (with "color" "dark grey" "font-size" "0.84" ,text)))

(define (test-reasoning)
  (check-group "reasoning and tokens")
  ;; OpenRouter: the reasoning, the text, then the tokens and the cost
  (check= (answer (string-append
                   "data: {\"choices\": [{\"delta\": "
                   "{\"reasoning\": \"Let me think.\"}}]}\n\n"
                   "data: {\"choices\": [{\"delta\": "
                   "{\"content\": \"Yes.\"}}]}\n\n"
                   "data: {\"choices\": [], \"usage\": {\"prompt_tokens\": 10, "
                   "\"completion_tokens\": 5, \"completion_tokens_details\": "
                   "{\"reasoning_tokens\": 2}, \"cost\": 0.5}}\n\n"
                   "data: [DONE]\n\n")
                  "openrouter")
          `(document ,(reasoning-fold "Let me think.")
                     (with "mode" "text" "Yes.")
                     ,(usage-line "10 5 0 2 0.5"
                                  "10 tokens in, 5 out (2 for the reasoning), $0.5000")))
  ;; Claude: its thinking, and its tokens in two events (the input with the
  ;; part read from the cache)
  (check= (answer (string-append
                   "event: message_start\n"
                   "data: {\"type\": \"message_start\", \"message\": "
                   "{\"model\": \"claude-x\", \"usage\": {\"input_tokens\": 3, "
                   "\"cache_read_input_tokens\": 100, \"output_tokens\": 1}}}\n\n"
                   "data: {\"type\": \"content_block_delta\", \"delta\": "
                   "{\"type\": \"thinking_delta\", \"thinking\": \"Hmm.\"}}\n\n"
                   "data: {\"type\": \"content_block_delta\", \"delta\": "
                   "{\"type\": \"text_delta\", \"text\": \"Ok.\"}}\n\n"
                   "data: {\"type\": \"message_delta\", \"delta\": "
                   "{\"stop_reason\": \"end_turn\"}, \"usage\": "
                   "{\"output_tokens\": 7}}\n\n")
                  "claude")
          `(document ,(reasoning-fold "Hmm.")
                     (with "mode" "text" "Ok.")
                     ,(usage-line "103 7 100 0 -1"
                                  "103 tokens in (100 cached), 7 out")))
  ;; the cost of Claude, estimated from the prices of OpenRouter, once they
  ;; are known (they are never fetched by the conversion: the list of the
  ;; suite is set here)
  (cpp-ai-set-prices
   (string-append "{\"data\": [{\"id\": \"anthropic/claude-x\", "
                  "\"pricing\": {\"prompt\": \"0.01\", "
                  "\"completion\": \"0.1\", "
                  "\"input_cache_read\": \"0.001\"}}]}"))
  (check= (answer (string-append
                   "data: {\"type\": \"message_start\", \"message\": "
                   "{\"model\": \"claude-x\", \"usage\": {\"input_tokens\": 3, "
                   "\"cache_read_input_tokens\": 100, \"output_tokens\": 7}}}\n\n"
                   "data: {\"type\": \"content_block_delta\", \"delta\": "
                   "{\"type\": \"text_delta\", \"text\": \"Ok.\"}}\n\n")
                  "claude")
          `(document (with "mode" "text" "Ok.")
                     ,(usage-line "103 7 100 0 0.83 estimated"
                                  "103 tokens in (100 cached), 7 out, about $0.8300")))
  (cpp-ai-set-prices "{\"data\": []}")
  ;; a model which thinks in its text (<think>, with Ollama)
  (check= (answer (openai "<think>Hmm.</think>\n\nAnswer.") "ollama")
          `(document ,(reasoning-fold "Hmm.") (with "mode" "text" "Answer.")))
  (check= (ai-reasoning "claude") "default")
  ;; the executable folds of the chatbots (tools/ai/ai-folds.scm)
  (check-true (ai-fold? (stree->tree '(script-input "claude" "default" "" ""))))
  (check-false (ai-fold? (stree->tree '(script-input "scheme" "default" "" "")))))

(define (test-pictures)
  (check-group "pictures")
  ;; an SVG in the text
  (with t (answer (openai (string-append
                           "Look: <svg xmlns=\"http://www.w3.org/2000/svg\" "
                           "width=\"10\" height=\"10\"><rect width=\"10\" "
                           "height=\"10\"/></svg> done"))
                  "openrouter")
    (with img (find-tag t 'image)
      (check-true (pair? img))
      (check-true (and img (string-ends? (caddr (cadr img)) ".svg")))))
  ;; a PNG as a data URL (as the image models give it)
  (with t (answer (openai (string-append "A picture:\n\n![](data:image/png;"
                                         "base64," png ")"))
                  "openrouter")
    (with img (find-tag t 'image)
      (check-true (pair? img))
      (check-true (and img (string-ends? (caddr (cadr img)) ".png")))
      (check-true (and img (string-starts? (cadr (cadr (cadr img)))
                                           "\x89PNG"))))))

(define (test-raw-fold)
  (check-group "the answer as it came")
  (with-preference "ai raw answer" "on"
    (lambda ()
      (check= (answer (openai "Hi.") "openrouter")
              '(document
                 (with "mode" "text" "Hi.")
                 (with "ai-raw" "true"
                   (folded (with "font-shape" "italic"
                             "The answer of gpt-x as it came")
                           (verbatim-code (document "Hi.")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The files attached to the questions (tools/ai/ai-attachments.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has? what s)
  (and (string? s) (>= (string-search-forwards what 0 s) 0)))

(define (tm-file name)
  (url->system (url-concretize (string-append "$TEXMACS_PATH/" name))))

(define attach-dir (url-append (url-temp-dir) "ai-test-attach"))
(define (attach-file name) (url->system (url-append attach-dir name)))

;; a session asked by the test: one exchange above, the question asked; a
;; PDF file with the exchange and a PNG file with the question
(define attach-session? #f)
(define attach-files '())

(tm-define (ai-session-context name chat)
  (:require attach-session?)
  (list "Q1" "A1"))

(tm-define (ai-session-attachments name chat)
  (:require attach-session?)
  attach-files)

;; the JSON of the request of an engine for the question "Describe"
(define (attach-request engine)
  (set! attach-session? #t)
  (with r (catch #t
            (lambda ()
              (with x (string->object (cpp-ai-latex-request "Describe" engine "check"))
                (tree->json (stree->tree (cAr x)))))
            (lambda args (object->string args)))
    (set! attach-session? #f)
    r))

(define (test-attachments)
  (check-group "attachments")
  (when (not (url-exists? attach-dir)) (system-mkdir attach-dir))
  (string-save "The secret word is avocado." (url-append attach-dir "notes.txt"))
  (string-save "PK" (url-append attach-dir "n.zip"))
  (let ((txt (attach-file "notes.txt"))
        (png (tm-file "misc/images/texmacs-20.png"))
        (pdf (tm-file "misc/pixmaps/unknown.pdf")))
    ;; the kinds of files
    (check= (map ai-attachment-kind (list txt png pdf "a.tm" "a.zip"))
            '("text" "image" "pdf" "texmacs" #f))
    ;; a text file comes after the question, the others not in its text
    (check= (ai-serialize "claude"
                          (stree->tree `(document (concat "Read " (ai-attachment "notes.txt" ,txt)))))
            (string-append "Read \n\nThe attached file notes.txt:\n\n```\n"
                           "The secret word is avocado.\n```"))
    (check= (ai-serialize "claude"
                          (stree->tree `(concat "Look " (ai-attachment "p.png" ,png))))
            "Look ")
    (check= (ai-binary-attachments
             (ai-stree-attachments `(concat "a" (ai-attachment "t" ,txt)
                                            (ai-attachment "p.png" ,png))))
            (list (string-append "image/png\tp.png\t" png)))
    ;; what is not sent
    (check= (ai-attachments-problem "claude" (list (list "p.png" png) (list "u.pdf" pdf)))
            #f)
    (check= (has? "not there" (ai-attachments-problem "claude" (list (list "x.png" (attach-file "x.png"))))) #t)
    (check= (has? "PDF" (ai-attachments-problem "ollama" (list (list "u.pdf" pdf)))) #t)
    (check= (has? "image" (ai-attachments-problem "albert" (list (list "p.png" png)))) #t)
    (check= (has? "neither" (ai-attachments-problem "claude" (list (list "n.zip" (attach-file "n.zip"))))) #t)
    ;; the requests: the PDF file with the first question, the PNG file
    ;; with the one asked, each as its API takes it
    (lazy-plugin-force)
    (set! attach-files
          (list (string-append "0\tapplication/pdf\tunknown.pdf\t" pdf)
                (string-append "1\timage/png\ttexmacs-20.png\t" png)))
    (with c (attach-request "claude")
      (check= (has? "\"type\": \"document\"" c) #t)
      (check= (has? "\"media_type\": \"application/pdf\"" c) #t)
      (check= (has? "\"media_type\": \"image/png\"" c) #t)
      (check= (has? "\"data\": \"iVBORw0KGgo" c) #t)
      ;; (base64 without line breaks)
      (check= (not (has? "\\n" (substring c (string-search-forwards "iVBORw0KGgo" 0 c)
                                          (string-length c)))) #t))
    (with c (attach-request "chatgpt")
      (check= (has? "\"file_data\": \"data:application/pdf;base64,JVBERi" c) #t)
      (check= (has? "\"url\": \"data:image/png;base64,iVBORw0KGgo" c) #t))
    (with c (attach-request "openrouter")
      (check= (has? "\"filename\": \"unknown.pdf\"" c) #t)
      (check= (has? "\"url\": \"data:image/png;base64,iVBORw0KGgo" c) #t))
    (with c (attach-request "open-mistral-7b")
      (check= (has? "\"image_url\": \"data:image/png;base64,iVBORw0KGgo" c) #t))
    (with c (attach-request "gemini")
      (check= (has? "\"mimeType\": \"application/pdf\"" c) #t)
      (check= (has? "\"mimeType\": \"image/png\"" c) #t))
    ;; without files: the messages as before
    (set! attach-files '())
    (with c (attach-request "claude")
      (check= (has? "\"content\": \"Describe\"" c) #t)
      (check= (not (has? "iVBORw0KGgo" c)) #t)))
  (system-remove (url-append attach-dir "notes.txt"))
  (system-remove (url-append attach-dir "n.zip")))

(tm-define (ai-test-failures)
  (check-suite "ai")
  ;; no prices: the conversion reads none from the home, and never asks the
  ;; network for them (ai_price_list)
  (cpp-ai-set-prices "{\"data\": []}")
  (with-preference "ai raw answer" "off"
    (lambda ()
      (test-engines)
      (test-stream)
      (test-errors)
      (test-pictures)
      (test-reasoning)))
  (test-raw-fold)
  (test-attachments)
  ;; the prices are read again from the home when they are needed
  (cpp-ai-set-prices "")
  (check-end))
