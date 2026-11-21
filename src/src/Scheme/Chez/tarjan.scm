;;; tarjan.scm -- Tarjan's strongly connected components algorithm.
;;;
;;; Computes strongly connected components (SCCs) and returns them in
;;; topological dependency order (leaves first). Also optionally provides
;;; an O(1) node-to-component index mapping for condensation DAG operations.
;;;
;;; Pure Chez Scheme; zero dependencies.

(library (tarjan)
  (export tarjan-scc
          tarjan-scc/comp-of)
  (import (chezscheme))

  ;; Returns (values COMPONENTS COMP-OF).
  ;; - COMPONENTS is a list of components in dependency order (leaf first:
  ;;   a component appears after every component it depends on).
  ;; - COMP-OF is a hashtable mapping each node to its component index in
  ;;   COMPONENTS (0-based integer).
  ;;
  ;; OPTS can specify:
  ;;   - A 0-arg hashtable constructor, e.g. make-eq-hashtable (the default)
  ;;   - Or two arguments: hash-fn and equiv-fn, e.g. equal-hash and equal?
  (define (tarjan-scc/comp-of nodes get-neighbors . opts)
    (let* ((make-ht (cond
                      ((null? opts) make-eq-hashtable)
                      ((and (pair? opts) (procedure? (car opts)) (null? (cdr opts)))
                       (car opts))
                      ((and (pair? opts) (pair? (cdr opts)))
                       (lambda () (make-hashtable (car opts) (cadr opts))))
                      (else make-eq-hashtable)))
           (index (make-ht))
           (low (make-ht))
           (on-stack (make-ht))
           (comp-of (make-ht))
           (stack '())
           (counter 0)
           (comps '())
           (comp-count 0))

      (define (pop-until! v)
        (let loop ((acc '()))
          (let ((w (car stack)))
            (set! stack (cdr stack))
            (hashtable-set! on-stack w #f)
            (if (equal? w v)
                (cons w acc)
                (loop (cons w acc))))))

      (define (strongconnect! v)
        (hashtable-set! index v counter)
        (hashtable-set! low v counter)
        (set! counter (+ counter 1))
        (set! stack (cons v stack))
        (hashtable-set! on-stack v #t)
        (for-each
         (lambda (w)
           (cond
            ((not (hashtable-ref index w #f))
             (strongconnect! w)
             (hashtable-set! low v
                            (min (hashtable-ref low v 0)
                                 (hashtable-ref low w 0))))
            ((hashtable-ref on-stack w #f)
             (hashtable-set! low v
                            (min (hashtable-ref low v 0)
                                 (hashtable-ref index w 0))))))
         (get-neighbors v))
        (when (= (hashtable-ref low v 0) (hashtable-ref index v 0))
          (let ((c (pop-until! v))
                (c-idx comp-count))
            (set! comp-count (+ comp-count 1))
            (for-each (lambda (n) (hashtable-set! comp-of n c-idx)) c)
            (set! comps (cons c comps)))))

      (let ((node-list (if (vector? nodes) (vector->list nodes) nodes)))
        (for-each (lambda (v) (unless (hashtable-ref index v #f) (strongconnect! v)))
                  node-list))
      (values (reverse comps) comp-of)))

  ;; Returns COMPONENTS in dependency order.
  (define (tarjan-scc nodes get-neighbors . opts)
    (let-values (((comps _) (apply tarjan-scc/comp-of nodes get-neighbors opts)))
      comps))
)

;; If loaded into a top-level interaction environment, import the library
;; bindings so they are available immediately.
(eval-when (load eval)
  (import (tarjan)))
