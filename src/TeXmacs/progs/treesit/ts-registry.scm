
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-registry.scm
;; DESCRIPTION : the tree-sitter languages of TeXmacs and their cached tables
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The grammars live in $TEXMACS_PATH/misc/tree-sitter/<lan>/grammar.json.
;; Their tables are generated at the first use of the language and cached in
;; $TEXMACS_HOME_PATH/system/cache/tree-sitter/<lan>-<hash>.tsdyn, the hash
;; being that of the grammar and of the version of the generator; the C
;; runtime (Plugins/TreeSitter) loads them from there.
;;
;; The generation of a large grammar takes seconds (C: about 35 under S7),
;; so it runs in a second TeXmacs, without window, while the present
;; highlighter keeps working: treesit-language returns #f until the tables
;; are ready.

(texmacs-module (treesit ts-registry)
  (:use (treesit ts-base) (treesit ts-language) (treesit ts-export)
        (treesit ts-scanner-cpp) (treesit ts-scanner-python)
        (treesit ts-scanner-javascript) (treesit ts-scanner-r)
        (treesit ts-scanner-julia) (treesit ts-scanner-bash)
        (treesit ts-scanner-rust) (treesit ts-scanner-lua)
        (treesit ts-scanner-lean)))

;; to be changed when the generator or the exported data change
(define-public treesit-generator-version "2")

(define-public treesit-languages
  '("json" "c" "scheme" "cpp" "python" "java" "javascript" "r" "julia"
    "bash" "go" "rust" "lua" "lean"))

;; the external scanners (ts-scanner-*.scm), for the languages with one
(define treesit-scanners
  (list (cons "cpp" ts-cpp-scanner) (cons "python" ts-python-scanner)
        (cons "javascript" ts-javascript-scanner) (cons "r" ts-r-scanner)
        (cons "julia" ts-julia-scanner) (cons "bash" ts-bash-scanner)
        (cons "rust" ts-rust-scanner) (cons "lua" ts-lua-scanner)
        (cons "lean" ts-lean-scanner)))

(define-public (treesit-supported? lan)
  (and (defined? 'tree-sitter-language-load)
       (member lan treesit-languages)
       (url-exists? (treesit-grammar-url lan))
       #t))

(define-public (treesit-grammar-url lan)
  (url-concretize
   (string-append "$TEXMACS_PATH/misc/tree-sitter/" lan "/grammar.json")))

(define (cache-dir)
  (url-concretize "$TEXMACS_HOME_PATH/system/cache/tree-sitter"))

(define (text-hash s)
  ;; two hashes of 24 bits, in hexadecimal
  (let loop ((i 0) (a 5381) (b 0))
    (if (= i (string-length s))
        (string-append (number->string a 16) "-" (number->string b 16))
        (let ((c (char->integer (string-ref s i))))
          (loop (+ i 1)
                (modulo (+ (* a 33) c) 16777213)
                (modulo (+ (* b 65599) c) 16777199))))))

(define-public (treesit-cache-url lan)
  ;; the file of the tables of the present grammar of lan
  (let ((grammar (string-load (treesit-grammar-url lan))))
    (url-append (cache-dir)
                (string-append lan "-" (text-hash (string-append treesit-generator-version
                                                                 grammar))
                               ".tsdyn"))))

(define-public (treesit-generate-cache lan)
  ;; generate the tables of lan and write them in the cache; the file is
  ;; written under another name and then renamed, so that a reader never
  ;; sees half of it
  (let* ((file (treesit-cache-url lan))
         (tmp (url-glue file ".part"))
         (s (ts-generate-tsdyn (string-load (treesit-grammar-url lan)))))
    (system-mkdir (cache-dir))
    (string-save s tmp)
    (system-move tmp file)
    file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loaded languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define loaded (make-ahash-table))       ; lan -> index in the C runtime
(define generating (make-ahash-table))   ; lan -> list of continuations

(define-public (treesit-language lan)
  ;; the index of lan in the C runtime if its tables are ready, else #f;
  ;; when they are not in the cache, they start to be generated
  (cond ((ahash-ref loaded lan) => identity)
        ((not (treesit-supported? lan)) #f)
        ((url-exists? (treesit-cache-url lan))
         (let ((id (tree-sitter-language-load (treesit-cache-url lan)))
               (scanner (assoc-ref treesit-scanners lan)))
           (and (>= id 0)
                (or (not scanner) (tree-sitter-set-scanner id (scanner)))
                (begin (ahash-set! loaded lan id) id))))
        (else (treesit-prepare lan (lambda (id) (noop))) #f)))

(define (generate-command lan)
  (list (texmacs-binary) "-headless" "-x"
        (string-append "(begin (use-modules (treesit ts-registry))"
                       " (treesit-generate-cache \"" lan "\") (exit 0))")))

(define (texmacs-binary)
  (let* ((tp (url->system (get-texmacs-path)))
         (bin (string-append tp "/bin/texmacs.bin"))
         (mac (string-append tp "/../../../MacOS/TeXmacs")))
    (cond ((url-exists? (system->url bin)) bin)
          ((url-exists? (system->url mac)) mac)
          (else "texmacs"))))

(define-public (treesit-prepare lan cont)
  ;; make the tables of lan in the background, then call cont with the
  ;; index of the language (or #f if the generation failed)
  (cond ((treesit-language-loaded lan) => cont)
        ((not (treesit-supported? lan)) (cont #f))
        ((url-exists? (treesit-cache-url lan)) (cont (treesit-language lan)))
        ((ahash-ref generating lan)
         (ahash-set! generating lan (cons cont (ahash-ref generating lan))))
        (else
         (ahash-set! generating lan (list cont))
         (set-message (string-append "Preparing the syntax of " lan "...") "")
         (let ((done (lambda (r)
                       (let ((conts (ahash-ref generating lan))
                             (id (and (url-exists? (treesit-cache-url lan))
                                      (treesit-language lan))))
                         (ahash-remove! generating lan)
                         (set-message "" "")
                         (for-each (lambda (c) (c id)) conts)))))
           (when (== (async-evaluate-system (generate-command lan) "" done) 0)
             (done (list -1 "" "")))))))

(define-public (treesit-language-loaded lan) (ahash-ref loaded lan))
