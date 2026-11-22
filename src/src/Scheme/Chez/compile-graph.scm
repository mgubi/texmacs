;;; compile-graph.scm -- Dependency graph for the Chez TeXmacs build.
;;;
;;; Builds an explicit DAG from the (compiler-db) records: adjacency in both
;;; directions, Tarjan strongly connected components, a deterministic
;;; topological order, and the parallel waves a scheduler can dispatch.
;;;
;;; Pure Chez Scheme; no TeXmacs runtime, no external libraries.

(library (compile-graph)
  (export compile-graph?
          compile-graph-nodes
          compile-graph-prereqs
          compile-graph-dependents
          make-compile-graph-from-modules
          make-compile-graph-from-db
          compile-graph-subgraph
          tarjan-scc
          tarjan-scc/comp-of
          compile-graph-scc
          compile-graph-cycles
          compile-graph-topological-sort
          compile-graph-waves
          compile-graph-macro-nodes
          compile-graph-macro-free?)
  (import (chezscheme) (tarjan) (compiler-db))

  ;;; -------------------------------------------------------------------------
  ;;; Record
  ;;; -------------------------------------------------------------------------

  ;; Only dependencies that are themselves nodes are kept: a module may name a
  ;; library outside the build (ice-9 rdelim, for one), and that must not turn
  ;; into a phantom node or a phantom cycle.
  (define-record-type (compile-graph %make-compile-graph compile-graph?)
    (fields
      (immutable nodes)         ; sorted list of module mod-names
      (immutable prereqs)       ; mod-name -> sorted (mod-name ...) inside the graph
      (immutable dependents)    ; mod-name -> sorted (mod-name ...) inside the graph
      (immutable macros))       ; mod-name -> #t when the module defines macros
    (nongenerative compile-graph-v1))

  ;;; -------------------------------------------------------------------------
  ;;; Spec ordering
  ;;; -------------------------------------------------------------------------

  (define (mod-name<? a b)
    (cond ((null? a) (pair? b))
          ((null? b) #f)
          ((eq? (car a) (car b)) (mod-name<? (cdr a) (cdr b)))
          (else
           (string<? (symbol->string (car a))
                     (symbol->string (car b))))))

  (define (sort-mod-names mod-names)
    (sort mod-name<? mod-names))

  ;;; -------------------------------------------------------------------------
  ;;; Construction
  ;;; -------------------------------------------------------------------------

  ;; Keep only the edges whose target is in SPECS, sorted. Used by both the
  ;; constructor and the induced subgraph.
  (define (restrict prereqs mod-names)
    (let ((inside (make-hashtable equal-hash equal?))
          (out (make-hashtable equal-hash equal?)))
      (for-each (lambda (s) (hashtable-set! inside s #t)) mod-names)
      (for-each
       (lambda (s)
         (hashtable-set! out s
                        (sort-mod-names
                         (filter (lambda (d) (hashtable-ref inside d #f))
                                 (hashtable-ref prereqs s '())))))
       mod-names)
      out))

  (define (build-graph nodes prereqs macros)
    (let ((dependents (make-hashtable equal-hash equal?)))
      (for-each (lambda (n) (hashtable-set! dependents n '())) nodes)
      (for-each
       (lambda (n)
         (for-each (lambda (p)
                     (hashtable-update! dependents p (lambda (ds) (cons n ds)) '()))
                   (hashtable-ref prereqs n '())))
       nodes)
      (for-each (lambda (n)
                  (hashtable-set! dependents n
                                 (sort-mod-names (hashtable-ref dependents n '()))))
                nodes)
      (%make-compile-graph nodes prereqs dependents macros)))

  (define (make-compile-graph-from-modules infos)
    (let ((nodes (sort-mod-names (map module-info-name infos)))
          (raw (make-hashtable equal-hash equal?))
          (macros (make-hashtable equal-hash equal?)))
      (for-each
       (lambda (info)
         (hashtable-set! raw (module-info-name info) (module-info-deps info))
         (unless (null? (module-info-global-macros info))
           (hashtable-set! macros (module-info-name info) #t)))
       infos)
      (build-graph nodes (restrict raw nodes) macros)))

  (define (make-compile-graph-from-db db)
    (make-compile-graph-from-modules (compiler-db-modules db)))

  ;; Induced subgraph: keep only the named mod-names and their internal edges.
  ;; Unknown mod-names are ignored rather than invented.
  (define (compile-graph-subgraph g mod-names)
    (let* ((all (compile-graph-prereqs g))
           (keep (sort-mod-names (filter (lambda (s) (hashtable-ref all s #f)) mod-names))))
      (build-graph keep (restrict all keep) (compile-graph-macros g))))

  ;;; -------------------------------------------------------------------------
  ;;; Tarjan's strongly connected components
  ;;; -------------------------------------------------------------------------

  ;; Returns the components in dependency order: a component appears only after
  ;; every component it depends on. That is already a valid build order, which
  ;; is why compile-graph-topological-sort can return it unchanged.
  (define (compile-graph-scc g)
    (tarjan-scc (compile-graph-nodes g)
                (lambda (v) (hashtable-ref (compile-graph-prereqs g) v '()))
                equal-hash equal?))

  (define (compile-graph-cycles g)
    (let ((prereqs (compile-graph-prereqs g)))
      (filter (lambda (c)
                (or (> (length c) 1)
                    ;; A one-node component is a cycle only if it depends on
                    ;; itself; the scan drops self dependencies, so this is
                    ;; here for graphs built by hand.
                    (member (car c) (hashtable-ref prereqs (car c) '()))))
              (compile-graph-scc g))))

  ;; Each element is one build step: a single module, or an SCC that has to be
  ;; compiled as a cluster because its members depend on each other.
  (define (compile-graph-topological-sort g)
    (compile-graph-scc g))

  ;;; -------------------------------------------------------------------------
  ;;; Parallel waves
  ;;; -------------------------------------------------------------------------

  ;; Wave n holds every module whose prerequisites all live in waves < n.
  ;; Members of one SCC always land in the same wave. Components arrive in
  ;; dependency order, so a component's level only reads levels already set.
  (define (compile-graph-waves g)
    (let ((prereqs (compile-graph-prereqs g))
          (level (make-hashtable equal-hash equal?))
          (buckets '()))                     ; (level . mod-names), unordered

      ;; Prerequisites inside the component itself do not raise its level.
      (define (level-of c v)
        (fold-left
         (lambda (acc p)
           (if (member p c)
               acc
               (max acc (+ 1 (hashtable-ref level p 0)))))
         0
         (hashtable-ref prereqs v '())))

      (for-each
       (lambda (c)
         (let ((lv (apply max 0 (map (lambda (v) (level-of c v)) c))))
           (for-each (lambda (v) (hashtable-set! level v lv)) c)
           (let ((bucket (assv lv buckets)))
             (if bucket
                 (set-cdr! bucket (append c (cdr bucket)))
                 (set! buckets (cons (cons lv c) buckets))))))
       (compile-graph-scc g))

      (map (lambda (b) (sort-mod-names (cdr b)))
           (sort (lambda (a b) (< (car a) (car b))) buckets))))

  ;;; -------------------------------------------------------------------------
  ;;; Macro safety
  ;;; -------------------------------------------------------------------------

  (define (compile-graph-macro-nodes g)
    (sort-mod-names
     (vector->list (hashtable-keys (compile-graph-macros g)))))

  ;; A macro-free set can be expanded without racing on another worker's
  ;; compile-time definitions.
  (define (compile-graph-macro-free? g . opt-mod-names)
    (let ((mod-names (if (null? opt-mod-names)
                     (compile-graph-nodes g)
                     (car opt-mod-names)))
          (macros (compile-graph-macros g)))
      (not (exists (lambda (s) (hashtable-ref macros s #f)) mod-names))))
)
