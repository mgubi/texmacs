
;; Tests of the Git support which need the event loop (asynchronous commands)
;; or open documents.  Run with doc/tests/run-git-tests.sh --gui, which uses
;; the offscreen Qt platform, so that no windows are shown.

(use-modules (version version-menu))

(define failures 0)
(define (check name ok?)
  (display* (if ok? "ok   " "FAIL ") name "\n")
  (when (not ok?) (set! failures (+ failures 1))))

(define T (getenv "GIT_TEST_DIR"))
(define A (system->url (string-append T "/remote/clone a")))
(define B (system->url (string-append T "/remote/clone b")))
(define DA (url-append A "doc.tm"))
(define DB (url-append B "doc.tm"))
(define C (system->url (string-append T "/conflict/paper.tm")))
(define C2 (system->url (string-append T "/conflict2/paper.tm")))

(define (doc text)
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  "
                 text "\n</body>\n"))

(define (finish)
  (display* "FAILURES: " failures "\n")
  (quit-TeXmacs))

;; Conflict resolution with the structured comparison
(define (test-conflict)
  (check "conflicted" (== (git-file-state C) 'conflicted))
  (load-buffer C)
  (git-resolve-conflict C)
  (check "one difference"
         (== (length (tree-search (buffer-get C)
                                  (lambda (t) (tree-is? t 'version-both))))
             1))
  (version-first-difference)
  (version-retain 1)
  (git-mark-resolved C)
  (check "resolved" (== (git-file-state C) 'staged))
  (check "their text" (string-contains? (string-load C) "as they wrote it"))
  (test-automatic-merge))

;; Changes to different words of a same line, merged automatically
(define (test-automatic-merge)
  (check "line conflict for git" (== (git-file-state C2) 'conflicted))
  (load-buffer C2)
  (git-resolve-conflict C2)
  (check "no remaining difference"
         (null? (tree-search (buffer-get C2)
                             (lambda (t) (tree-in? t '(version-old
                                                       version-new
                                                       version-both))))))
  (check "both changes merged"
         (== (tree->stree (buffer-get-body C2))
             '(document "The slow brown fox leaps." "Second.")))
  (git-mark-resolved C2)
  (check "merge resolved" (== (git-file-state C2) 'staged))
  (finish))

;; Asynchronous remote commands and reloading of open documents
(define (test-remote)
  (string-save (doc "Version one.") DA)
  (git-stage DA)
  (git-commit-staged A "one")
  (git-push A
    (lambda (r)
      (check "push with new upstream" (git-ok? r))
      (check "upstream" (== (git-status-ref (git-status A) 'upstream)
                            "origin/main"))
      (git-pull B
        (lambda (r)
          (check "pull" (git-ok? r))
          (load-buffer DB)
          (string-save (doc "Version two.") DA)
          (git-stage DA)
          (git-commit-staged A "two")
          (git-push A
            (lambda (r)
              (git-fetch B
                (lambda (r)
                  (check "behind after fetch"
                         (== (git-status-ref (git-status B) 'behind) 1))
                  (git-pull B
                    (lambda (r)
                      (check "open document reloaded"
                             (== (tree->stree (buffer-get-body DB))
                                 '(document "Version two.")))
                      (check "not busy" (not (git-busy? B)))
                      (test-conflict)))))))))))
  (check "busy while pushing" (git-busy? A)))

(test-remote)
