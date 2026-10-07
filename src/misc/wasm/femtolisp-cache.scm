;; The cache of compiled files of femtolisp shipped with the page
;; (misc/wasm/Makefile, femtolisp-cache): run by the program for node, it
;; loads what TeXmacs loads at its start and when the menus are opened (all
;; the menus, with their submenus, in text and in math), whose compiled code
;; then goes to $TEXMACS_HOME_PATH/system/cache/femtolisp
(define (cache-menu-links x acc)
  (cond ((not (pair? x)) acc)
        ((and (eq? (car x) 'link) (pair? (cdr x)) (symbol? (cadr x)))
         (cons (cadr x) acc))
        (else (cache-menu-links (cdr x) (cache-menu-links (car x) acc)))))

(define (cache-expand-menus)
  (let ((seen (make-ahash-table)))
    (define (expand m)
      (with r (catch #t (lambda () (menu-expand m)) (lambda e '()))
        (for-each (lambda (name)
                    (when (not (ahash-ref seen name))
                      (ahash-set! seen name #t)
                      (expand (list 'vertical (list 'link name)))))
                  (cache-menu-links r '()))))
    (for-each expand
              '((vertical (link texmacs-menu))
                (horizontal (link texmacs-main-icons))
                (horizontal (link texmacs-mode-icons))
                (horizontal (link texmacs-extra-icons))
                (horizontal (dynamic (texmacs-focus-icons)))
                (vertical (link texmacs-popup-menu))))))

(let ((u (new-buffer)))
  (buffer-set-body u (stree->tree '(document (concat "Text " (math "x+y")))))
  (cache-expand-menus)
  (go-to (append (buffer-path) '(0 1 0 1)))
  (cache-expand-menus)
  (buffer-pretend-saved u))
