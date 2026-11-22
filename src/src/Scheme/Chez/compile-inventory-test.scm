;;; compile-inventory-test.scm -- Tests for compile-inventory.scm.
;;;
;;; Run from the repository root or from src/Scheme/Chez:
;;;   scheme -q compile-inventory-test.scm
;;;
;;; Exits with status 0 when every check passes, 1 otherwise.
;;;
;;; The inventory decides what the build compiles, so the checks here are about
;;; the shape of the data rather than its behaviour: tiers numbered 1..7 in
;;; order, *source-files* the exact concatenation of them, no duplicates, and
;;; (when the source tree is reachable) every whitelisted path really existing.

(define (first-existing paths)
  (cond ((null? paths) #f)
        ((file-exists? (car paths)) (car paths))
        (else (first-existing (cdr paths)))))

(define inventory-path
  (or (first-existing '("compile-inventory.scm"
                        "src/Scheme/Chez/compile-inventory.scm"
                        "texmacs/src/Scheme/Chez/compile-inventory.scm"))
      (error 'compile-inventory-test "cannot find compile-inventory.scm")))

(load inventory-path)
(import (compile-inventory))

(define checks 0)
(define failures 0)

(define (check label expected actual)
  (set! checks (+ checks 1))
  (if (equal? expected actual)
      (begin
        (display "PASS ") (display label) (newline))
      (begin
        (set! failures (+ failures 1))
        (display "FAIL ") (display label)
        (display " expected ") (write expected)
        (display " got ") (write actual)
        (newline))))

(define (check-true label actual)
  (check label #t (and actual #t)))

;; Chez's (chezscheme) has no delete-duplicates in every build this runs on.
(define (unique-count l)
  (length (let loop ((rest l) (seen '()))
            (cond ((null? rest) seen)
                  ((member (car rest) seen) (loop (cdr rest) seen))
                  (else (loop (cdr rest) (cons (car rest) seen)))))))

;;; Interface -----------------------------------------------------------------

(check-true "kernel list exported" (pair? *kernel-files*))
(check-true "tier table exported" (pair? inventory-tiers))
(check "seven tiers" 7 (length inventory-tiers))
(check "internal helper not leaked" #f (top-level-bound? '*tier-table*))

;;; Tier numbering ------------------------------------------------------------

(check "tiers numbered 1..7 in order" '(1 2 3 4 5 6 7)
       (map inventory-tier-number inventory-tiers))

(check "tier names are distinct" 7
       (unique-count (map inventory-tier-name inventory-tiers)))

(check "tier titles are distinct" 7
       (unique-count (map inventory-tier-title inventory-tiers)))

(check "no empty tier" '()
       (filter (lambda (tier) (null? (inventory-tier-files tier))) inventory-tiers))

;; The driver prints these banners and the graph report these labels; pinning
;; them here means a rename cannot silently change build output.
(check "progress banners"
       '("Compiling TeXmacs Kernel Modules"
         "Compiling Tier 2 (Application Core) Modules"
         "Compiling Tier 3 (Core Editing) Modules"
         "Compiling Tier 4 (Desktop GUI & Menus) Modules"
         "Compiling Tier 5 (Document Converters) Modules"
         "Compiling Tier 6 (Dynamic & Interactive Sessions) Modules"
         "Compiling Tier 7 (Extended Subsystems & Plugins) Modules")
       (map inventory-tier-title inventory-tiers))

(check "graph report labels"
       '("kernel" "tier2" "tier3" "tier4"
         "tier5-convert" "tier6-dynamic" "tier7-extended")
       (map inventory-tier-name inventory-tiers))

;;; Composition ---------------------------------------------------------------

(check "*source-files* is the tier concatenation" #t
       (equal? *source-files*
               (apply append (map inventory-tier-files inventory-tiers))))

(check "module count matches the tier sum" (length *source-files*)
       (apply + (map (lambda (tier) (length (inventory-tier-files tier)))
                     inventory-tiers)))

(check "no duplicate paths" (length *source-files*)
       (unique-count *source-files*))

(check "kernel is the first tier" *kernel-files*
       (inventory-tier-files (car inventory-tiers)))

;; The bootstrap order inside tier 1 is load-bearing: chez-compat first.
(check "kernel starts at chez-compat" "kernel/boot/chez-compat.scm"
       (car *kernel-files*))

;;; Path shape ----------------------------------------------------------------

(define (string-has? s needle)
  (let ((n (string-length s)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) needle) #t)
            (else (loop (+ i 1)))))))

(define (relative-scm-path? p)
  (and (string? p)
       (> (string-length p) 4)
       (string=? (substring p (- (string-length p) 4) (string-length p)) ".scm")
       (not (char=? (string-ref p 0) #\/))
       (not (string-has? p ".."))
       (not (string-has? p "\\"))))

(check "every entry is a relative .scm path" '()
       (filter (lambda (p) (not (relative-scm-path? p))) *source-files*))

;;; Live tree -----------------------------------------------------------------

(define progs
  (let* ((root (or (getenv "TEXMACS_PATH") ""))
         (p (first-existing (list (string-append root "/progs")
                                  "TeXmacs/progs"
                                  "texmacs/TeXmacs/progs"
                                  ;; running from src/Scheme/Chez
                                  "../../../TeXmacs/progs"))))
    (and p (file-exists? p) p)))

(if progs
    (check "every whitelisted module exists" '()
           (filter (lambda (p) (not (file-exists? (string-append progs "/" p))))
                   *source-files*))
    (begin
      (display "SKIP live tree checks: set TEXMACS_PATH or run from the repository root")
      (newline)))

;;; Summary -------------------------------------------------------------------

(display "------------------------------------------------------------") (newline)
(display checks) (display " checks, ")
(display failures) (display " failures") (newline)
(exit (if (= failures 0) 0 1))
