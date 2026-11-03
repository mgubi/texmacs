
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : debug-widgets.scm
;; DESCRIPTION : Debugging widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (debug debug-widgets)
  (:use (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message-type m)
  (cond ((string-ends? m "-error") (string-drop-right m 6))
        ((string-ends? m "-warning") (string-drop-right m 8))
        ((string-ends? m "-bench") (string-drop-right m 6))
        ((string-starts? m "debug-") (string-drop m 6))
        (else m)))

(define (list-message-types kind)
  (let* ((l (tree-children (get-debug-messages kind 100)))
         (t (make-ahash-table)))
    (for (m l)
      (ahash-set! t (message-type (tree->string (tree-ref m 0))) #t))
    (sort (ahash-set->list t) string<=?)))

(define (message-among? m selected)
  (let* ((k (tm-ref m 0))
         (s (tm-ref m 1)))
    (and (tree-atomic? k)
         (tree-atomic? s)
         (in? (message-type (tree->string k)) selected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build a document with the list of all messages of a certain kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-message m)
  (let* ((k (tm->stree (tm-ref m 0)))
         (s (utf8->cork (tm->stree (tm-ref m 1))))
         (t (tm->stree (tm-ref m 2))))
    (cond ((and (!= t "") (== (get-preference "console details") "detailed"))
           `(document ,(build-message `(tuple ,(tm-ref m 0) ,(tm-ref m 1) ""))
                      (indent (small ,t))))
          ((string-ends? k "-error")
           `(with "color" "#e02020" (concat "Error: " ,s)))
          ((string-ends? k "-warning")
           `(with "color" "dark magenta" (concat "Warning: " ,s)))
          ((string-ends? k "-bench")
           `(with "color" "dark blue" ,s))
          (else s))))

(define (selected-messages kind selected)
  (let* ((n (or (string->number (get-preference "console size")) 100))
         (all-ms (tree-children (get-debug-messages kind n))))
    (map tm->stree (list-filter all-ms (cut message-among? <> selected)))))

(define (messages->document ms)
  `(document
     (with "language" "verbatim" "font-family" "tt" "par-par-sep" "0fn"
           (document
             ,@(if (null? ms) (list "") (map build-message ms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main console widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define console-active? (make-ahash-table))
(tm-define console-categories (make-ahash-table))

;; The messages are shown in read-only editors (texmacs-input), so that they
;; can be selected, copied and searched as in a document: one embedded
;; buffer per tab, named after the console and the category ("all" for the
;; tab of all messages), which lives as long as the window (the window
;; closes it, see texmacs_input_widget). A new message is added to the
;; buffers which show it, at the end, without typesetting them anew: the
;; selection and the scrolling position stay as they were.

(define console-buffers (make-ahash-table)) ; buffer -> (kind cat)
(define console-shown (make-ahash-table))   ; buffer -> messages shown

(define (console-buffer kind cat)
  (string->url
    (string-append "tmfs://aux/console-"
                   (string-replace (string-downcase kind) " " "-")
                   "-" (or cat "all"))))

(define (console-selection kind cat)
  (if cat (list cat) (ahash-ref console-categories kind)))

(define (console-buffer? u)
  (and (url? u) (ahash-ref console-buffers (url->unix u)) #t))

(define (console-document kind cat)
  (let* ((u (console-buffer kind cat))
         (ms (selected-messages kind (console-selection kind cat))))
    (ahash-set! console-buffers (url->unix u) (list kind cat))
    (ahash-set! console-shown (url->unix u) ms)
    (messages->document ms)))

(define (console-messages-tree u)
  ;; the document of the messages, inside the with of messages->document
  (let* ((body (buffer-get-body u))
         (w (and (tree-is? body 'document) (> (tree-arity body) 0)
                 (tree-ref body 0))))
    (and w (tree-is? w 'with)
         (with d (tree-ref w (- (tree-arity w) 1))
           (and (tree-is? d 'document) d)))))

(define (list-drop-common old new)
  ;; the number k of old messages gone when @new continues @old without
  ;; its first k elements, or #f
  (let loop ((k 0) (l old))
    (cond ((null? l) (and (null? old) 0))
          ((list-starts? new l) k)
          (else (loop (+ k 1) (cdr l))))))

(define (list-starts? l prefix)
  (cond ((null? prefix) #t)
        ((null? l) #f)
        ((== (car l) (car prefix)) (list-starts? (cdr l) (cdr prefix)))
        (else #f)))

(define (console-reset u)
  (with info (ahash-ref console-buffers (url->unix u))
    (when (and info (buffer-exists? u))
      (let* ((doc (console-document (car info) (cadr info)))
             (d (console-messages-tree u)))
        ;; (the messages only: a new body left the cursor of a console
        ;; being used outside of it, and the next change failed on it)
        (if d (tree-assign! d (cAr (cAr doc)))
            (buffer-set-body u doc))
        (console-forget-history u)))))

;; A read-only buffer has no undo: the history of a console would only keep
;; every message added or removed, as long as it is open. It is cleared
;; after the changes, unless the console is the current buffer: an event of
;; its own may be under way, and clearing first confirms what the event
;; changed, which the end of the event is to cancel (console-recheck clears
;; it then, once the event is over)
(define (console-forget-history u . opt-force?)
  (when (or (!= (current-buffer) u) (and (nnull? opt-force?) (car opt-force?)))
    (with-buffer u (clear-undo-history))))

;; The messages shown are those given to the console, one per message,
;; unless a message came during an event of the console itself (one of its
;; keys raised a warning): it was added, then cancelled with what the event
;; changed (read only), with the removal of the first one when the console
;; was full. The number, the first and the last message tell
(define (console-shows? d i m)
  (== (tree->stree (tree-ref d i)) (tree->stree (stree->tree (build-message m)))))

(define (console-consistent? u)
  (let* ((d (console-messages-tree u))
         (old (or (ahash-ref console-shown (url->unix u)) '())))
    (or (null? old)
        (and d (== (tree-arity d) (length old))
             (console-shows? d 0 (car old))
             (console-shows? d (- (tree-arity d) 1) (cAr old))))))

(define (console-recheck u)
  ;; (delayed: no event is under way)
  (when (and (buffer-exists? u) (ahash-ref console-buffers (url->unix u)))
    (when (not (console-consistent? u))
      (console-reset u))
    (console-forget-history u #t)))

;; A change of the document cancels its selection: the selection of a
;; console is kept, its paths moved by the number k of messages removed
;; before it (with-buffer: the console need not be the current buffer, as
;; the one which opened by itself, without the focus)
(define (console-selection-paths u d)
  (with-buffer u
    (and (selection-active-any?)
         (list (tree->path d) (selection-get-start) (selection-get-end)))))

(define (console-restore-selection sel k)
  (and-with sel sel
    (let* ((p (car sel)) (n (length p)))
      (define (move q)
        (and (list-starts? q p) (> (length q) n)
             (>= (- (list-ref q n) k) 0)
             (append p (list (- (list-ref q n) k)) (list-tail q (+ n 1)))))
      (let* ((s (move (cadr sel))) (e (move (caddr sel))))
        (when (and s e) (selection-set s e))))))

(define (console-restore-selection-in u sel k)
  (when sel (with-buffer u (console-restore-selection sel k))))

(define (console-update u)
  ;; add the new messages at the end and remove those which are gone
  (let* ((info (ahash-ref console-buffers (url->unix u)))
         (kind (car info))
         (old (or (ahash-ref console-shown (url->unix u)) '()))
         (new (selected-messages kind (console-selection kind (cadr info))))
         (k (list-drop-common old new))
         (d (console-messages-tree u))
         (end? (console-at-end? u)))
    (cond ((== old new) (noop))
          ((or (not k) (not d) (null? old) (not (console-consistent? u)))
           (console-reset u))
          (else
            (let* ((added (list-tail new (- (length old) k)))
                   (sel (console-selection-paths u d)))
              (when (> k 0) (tree-remove! d 0 k))
              (when (nnull? added)
                (tree-insert! d (tree-arity d) (map build-message added)))
              (ahash-set! console-shown (url->unix u) new)
              (console-restore-selection-in u sel k)
              (console-forget-history u))))
    ;; a console whose cursor is at the end follows it, as a log
    (when end? (console-go-end u))
    ;; (the console in use: what was added may be cancelled with the event
    ;; under way, see console-consistent?)
    (when (== (current-buffer) u)
      (delayed
        (:idle 10)
        (console-recheck u)))))

(define (console-at-end? u)
  (with-buffer u
    (and (not (selection-active-any?))
         (== (cursor-path) (path-end (root-tree) (tree->path (buffer-tree)))))))

(define (console-go-end u)
  (with-buffer u (go-end)))

(define (console-buffer-list)
  (list-filter (map unix->url (ahash-set->list console-buffers))
               buffer-exists?))

(define (console-buffers-of kind)
  (list-filter (console-buffer-list)
               (lambda (u)
                 (== (car (ahash-ref console-buffers (url->unix u))) kind))))

(define (reset-console-buffers)
  (for (u (console-buffer-list))
    (console-reset u)))

(define (update-console-buffers)
  (for (u (console-buffer-list))
    (console-update u)))

;; the preferences of the consoles show the messages anew
(define (refresh-console . x)
  (reset-console-buffers)
  (noop))

(define-preferences
  ("console details" "normal" refresh-console)
  ("console size" "100" refresh-console))

(define (forget-console kind)
  ;; (the window closes the buffers of its editors)
  (for (key (ahash-set->list console-buffers))
    (with info (ahash-ref console-buffers key)
      (when (== (car info) kind)
        (ahash-remove! console-buffers key)
        (ahash-remove! console-shown key)))))

(tm-widget ((console-widget kind))
  (responsive-tabs
    (responsive-tab (text "All")
      (texmacs-input (console-document kind #f) '(style "generic")
                     (console-buffer kind #f)))
    (loop (cat (ahash-ref console-categories kind))
      (responsive-tab (text (upcase-first cat))
        (texmacs-input (console-document kind cat) '(style "generic")
                       (console-buffer kind cat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic updates of consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define console-updating? #f)
(tm-define console-errors? #f)
(tm-define console-warnings? #f)

(tm-define (update-consoles)
  ;; the new messages are added to the buffers of the tabs (the tabs are
  ;; those of the categories when the console was opened: the loop of the
  ;; widget is not done again when it is refreshed, and remaking the
  ;; editors would only reset them)
  (for (kind (ahash-set->list console-active?))
    (ahash-set! console-categories kind (list-message-types kind)))
  (update-console-buffers)
  (when (and (or (and console-errors?
                      (get-boolean-preference "open console on errors"))
                 (and console-warnings?
                      (get-boolean-preference "open console on warnings")))
             (not (ahash-ref console-active? "Error messages")))
    ;; (opened by itself: the keys typed in the document stay there)
    (delayed
      (:idle 1)
      (open-console "Error messages" #t)))
  (set! console-updating? #f)
  (set! console-errors? #f)
  (set! console-warnings? #f))

(tm-define (notify-debug-message channel)
  (when (string-ends? channel "-error")
    (set! console-errors? #t))
  (when (string-ends? channel "-warning")
    (set! console-warnings? #t))
  (when (not console-updating?)
    (set! console-updating? #t)
    (update-consoles)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-console kind . opt-passive?)
  (when (not (ahash-ref console-active? kind))
    (ahash-set! console-active? kind #t)
    (ahash-set! console-categories kind (list-message-types kind))
    (when (and (nnull? opt-passive?) (car opt-passive?))
      (gui-set-next-window-without-focus))
    (top-window (console-widget kind) kind
                (lambda x
                  (ahash-remove! console-active? kind)
                  (forget-console kind)))
    ;; read only: what one does in a console does not change it (see
    ;; set_buffer_read_only), the messages are added by the program
    (for (u (console-buffers-of kind))
      (buffer-set-read-only u #t))
    ;; the consoles begin at the end, the latest messages, and follow them
    ;; (at the next round, once the window is made; not (:idle 1): the idle
    ;; time of an editor is 0 while it has not the keyboard focus, so that
    ;; the end waited for a window of TeXmacs to get it, and the console
    ;; opened at its top when it did not, #661)
    (delayed
      (:pause 1)
      (for (u (console-buffers-of kind))
        (console-go-end u)))))

(tm-define (open-debug-console)
  (open-console "Debugging console"))

(tm-define (open-error-messages)
  (open-console "Error messages"))
