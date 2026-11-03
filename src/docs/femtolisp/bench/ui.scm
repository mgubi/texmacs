;; The interactive work of the editor: the expansion of all menus, with their
;; submenus, and typing, in text and in math. Each task runs once cold, then
;; three times warm. Usage: texmacs.bin -x '(load "docs/femtolisp/bench/ui.scm")'
;; Output: BENCH <task> cold <ms> warm <median ms> [items <n>].

(define (median3 l) (cadr (sort l <)))
(define (bench name thunk)
  (let* ((t0 (texmacs-time))
         (n (thunk))
         (cold (- (texmacs-time) t0))
         (warm (map (lambda (i)
                      (let ((t1 (texmacs-time))) (thunk) (- (texmacs-time) t1)))
                    '(1 2 3))))
    (display* "BENCH " name " cold " cold " warm " (median3 warm)
              (if (number? n) (string-append " items " (number->string n)) "")
              "\n")))

;; menus: expand the menu bar, the toolbars and the context menu, then each
;; submenu which they link to, as opening every menu would; returns the
;; number of expanded menus
(define ui-roots
  '((vertical (link texmacs-menu))
    (horizontal (link texmacs-main-icons))
    (horizontal (link texmacs-mode-icons))
    (horizontal (link texmacs-extra-icons))
    (horizontal (dynamic (texmacs-focus-icons)))
    (vertical (link texmacs-popup-menu))))

(define (ui-links x acc)
  (cond ((not (pair? x)) acc)
        ((and (eq? (car x) 'link) (pair? (cdr x)) (symbol? (cadr x)))
         (cons (cadr x) acc))
        (else (ui-links (cdr x) (ui-links (car x) acc)))))

(define (expand-all-menus)
  (let ((seen (make-ahash-table)) (count 0))
    (define (expand m)
      (set! count (+ count 1))
      (with r (catch #t (lambda () (menu-expand m)) (lambda e '()))
        (for-each (lambda (name)
                    (when (not (ahash-ref seen name))
                      (ahash-set! seen name #t)
                      (expand (list 'vertical (list 'link name)))))
                  (ui-links r '()))))
    (for-each expand ui-roots)
    count))

;; editing: one key press as the event loop does it, then typesetting
(define (key k)
  (archive-state)
  (start-editing)
  (key-press k)
  (end-editing)
  (update-forced))

(define text-keys
  (map char->string
       (string->list "The quick brown fox jumps over the lazy dog, again. ")))

(define math-keys
  '("a" "^" "2" "right" "+" "b" "_" "1" "right" "=" "\\" "f" "r" "a" "c"
    "return" "x" "down" "y" "right" "space" "s" "i" "n" "space" "x" "space"))

(define (ui-buffer doc)
  (let ((u (new-buffer)))
    (buffer-set-body u (stree->tree doc))
    (update-forced)
    u))

(define (close-ui-buffer u)
  (buffer-pretend-saved u)
  (buffer-close u))

(define (in-text thunk)
  (let ((u (ui-buffer '(document "Some text."))))
    (go-end)
    (with r (thunk) (close-ui-buffer u) r)))

(define (in-math thunk)
  (let ((u (ui-buffer '(document (concat "Text " (math "x+y"))))))
    (go-to (append (buffer-path) '(0 1 0 1)))
    (with r (thunk) (close-ui-buffer u) r)))

(define (type keys n)
  (do ((i 0 (+ i 1))) ((= i n)) (for-each key keys)))

(define (load-and-typeset)
  (let ((u (url-unix "$TEXMACS_PATH" "doc/about/changes/change-log.en.tm")))
    (load-buffer u)
    (update-forced)
    (close-ui-buffer u)))

(exec-delayed
  (lambda ()
    (bench "menus-text" (lambda () (in-text expand-all-menus)))
    (bench "menus-math" (lambda () (in-math expand-all-menus)))
    (bench "typing-text-520-keys" (lambda () (in-text (lambda () (type text-keys 10)))))
    (bench "typing-math-270-keys" (lambda () (in-math (lambda () (type math-keys 10)))))
    (bench "open-change-log" load-and-typeset)
    (quit-TeXmacs)))
