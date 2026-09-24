
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-base.scm
;; DESCRIPTION : low level interface with the Git executable
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All calls to Git go through git-run below, which executes the command
;; without a shell (no quoting problems), in the root of the working tree,
;; and returns the exit code together with the standard output and error.
;; Only machine readable output formats of Git are parsed.

(texmacs-module (version git-base))

(define-preferences
  ("git executable" "git" noop)
  ("git log length" "250" noop)
  ("git large file size" "10" noop)
  ("git sign" "off" noop)
  ("git pull mode" "fast-forward" noop)
  ("git recent repositories" "" noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nul-char (list->string (list (integer->char 0))))
(define unit-sep (list->string (list (integer->char 31))))
(define record-sep (list->string (list (integer->char 30))))
(define code-sep (list->string (list (integer->char 1))))

(tm-define (git-split s sep)
  (:synopsis "Split @s at each occurrence of @sep, dropping a trailing @sep")
  (let loop ((start 0) (acc '()))
    (with pos (string-search-forwards sep start s)
      (cond ((>= pos 0)
             (loop (+ pos (string-length sep))
                   (cons (substring s start pos) acc)))
            ((< start (string-length s))
             (reverse (cons (substring s start (string-length s)) acc)))
            (else (reverse acc))))))

(define (git-chomp s)
  (if (string-ends? s "\n") (git-chomp (string-drop-right s 1)) s))

(tm-define (git-log-length)
  (with n (string->number (get-preference "git log length"))
    (if (and n (> n 0)) n 250)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Working trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-root u)
  (:synopsis "Root directory of the Git working tree which contains @u")
  ;; NOTE: .git is a directory for ordinary repositories,
  ;; but a file for linked worktrees and submodules
  (and (url-rooted? u)
       (not (url-rooted-tmfs? u))
       (let loop ((dir (if (url-directory? u) u (url-head u))))
         (cond ((url-exists? (url-append dir ".git")) dir)
               ((== (url-head dir) dir) #f)
               (else (loop (url-head dir)))))))

(tm-define (git-relative root u)
  (:synopsis "Path of @u relative to the working tree @root, with slashes")
  (url->unix (url-delta (url-append root "dummy") u)))

(tm-define (git-absolute root path)
  (:synopsis "Url of the file with relative @path in the working tree @root")
  (url-append root (unix->url path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define git-environment-set? #f)
(define git-history '())
(define git-history-max 50)

(define (git-set-environment)
  (when (not git-environment-set?)
    (set! git-environment-set? #t)
    ;; Never block on a password prompt for which there is no terminal
    (system-setenv "GIT_TERMINAL_PROMPT" "0")
    ;; Do not take the index lock merely for displaying the status
    (system-setenv "GIT_OPTIONAL_LOCKS" "0")))

(tm-define (git-arguments root args)
  (append (list (get-preference "git executable")
                "-C" (url->system root)
                "-c" "core.quotepath=off"
                "-c" "color.ui=false"
                ;; file names are never patterns
                "--literal-pathspecs")
          args))

(define (git-remember root args ret)
  (set! git-history
        (cons (list (texmacs-time) (url->system root) args ret) git-history))
  (when (> (length git-history) git-history-max)
    (set! git-history (sublist git-history 0 git-history-max))))

(tm-define (git-command-history)
  (:synopsis "List of (time root arguments result) for recent Git commands")
  git-history)

(define (git-result ret)
  (with code (string->number (car ret))
    (list (or code -1) (cadr ret) (caddr ret))))

(define (spawn-supported?)
  ;; NOTE: evaluate-system is not available for the X11 version
  (not (x-gui?)))

(tm-define (git-shell-quote s)
  (:synopsis "Quote @s for a POSIX shell")
  (string-append "'" (string-replace s "'" "'\\''") "'"))

(define (git-shell-run cmd input)
  ;; Fallback via the shell, when evaluate-system is not available
  (let* ((in (url-temp))
         (err (url-temp))
         (sh (string-append "(" (string-recompose (map git-shell-quote cmd) " ")
                            " < " (git-shell-quote (url->system in))
                            " 2> " (git-shell-quote (url->system err))
                            "; printf '\\001%d' $?)")))
    (string-save (or input "") in)
    (let* ((out (eval-system sh))
           (pos (string-search-backwards code-sep (string-length out) out))
           (msg (if (url-exists? err) (string-load err) "")))
      (system-remove in)
      (system-remove err)
      (if (< pos 0)
          (list "-1" out msg)
          (list (substring out (+ pos 1) (string-length out))
                (substring out 0 pos)
                msg)))))

(tm-define (git-run-with-input root input . args)
  (:synopsis "Run Git with @args in @root, sending @input to its stdin")
  ;; NOTE: widgets evaluate their contents eagerly, so that root may be #f
  (if (not root)
      (list -1 "" "not in a Git working tree")
      (git-run-in root input args)))

(define (git-run-in root input args)
  (git-set-environment)
  (let* ((cmd (git-arguments root args))
         (ret (cond ((not (spawn-supported?)) (git-shell-run cmd input))
                    (input (evaluate-system cmd '(0) (list input) '(1 2)))
                    (else (evaluate-system cmd '() '() '(1 2)))))
         (r (git-result ret)))
    (git-remember root args r)
    r))

(tm-define (git-run root . args)
  (:synopsis "Run Git with @args in @root and return (code stdout stderr)")
  (apply git-run-with-input (cons* root #f args)))

;; Asynchronous commands, for those which may take a long time.
;; At most one asynchronous command runs per working tree.

(define git-busy-table (make-ahash-table))

(tm-define (git-busy? root)
  (:synopsis "Is an asynchronous Git command running for @root?")
  (nnot (ahash-ref git-busy-table (url->system root))))

(tm-define (git-run-async root args input cont)
  (:synopsis "Run Git with @args in @root and call @cont with the result")
  (git-set-environment)
  (let* ((key (url->system root))
         (cmd (git-arguments root args))
         (done (lambda (r)
                 (ahash-remove! git-busy-table key)
                 (git-remember root args r)
                 (cont r))))
    (cond ((ahash-ref git-busy-table key)
           (cont (list -1 "" "Another Git command is still running")))
          ((not (spawn-supported?))
           (done (git-result (git-shell-run cmd input))))
          (else
            (with id (async-evaluate-system cmd (or input "") done)
              (if (== id 0)
                  (done (list -1 "" "Could not start Git"))
                  (ahash-set! git-busy-table key id)))))))

(tm-define (git-cancel root)
  (:synopsis "Terminate the asynchronous Git command running for @root")
  (and-with id (ahash-ref git-busy-table (url->system root))
    (async-evaluate-cancel id)))

(tm-define (git-ok? ret) (== (car ret) 0))
(tm-define (git-out ret) (cadr ret))
(tm-define (git-err ret) (caddr ret))

(tm-define (git-message ret)
  (:synopsis "Most informative line of the output of a Git command")
  (with l (list-filter (git-split (string-append (git-out ret) "\n"
                                                 (git-err ret)) "\n")
                       (lambda (s) (!= (tm-string-trim-both s) "")))
    (cond ((null? l) "")
          ((git-ok? ret) (cAr l))
          (else (car l)))))

(tm-define (git-output root . args)
  (:synopsis "Standard output of Git with @args in @root, or #f on failure")
  (with ret (apply git-run (cons root args))
    (and (git-ok? ret) (git-out ret))))

(define git-available-cache (make-ahash-table))

(tm-define (git-available?)
  (:synopsis "Can the Git executable be run?")
  (with exe (get-preference "git executable")
    (when (not (ahash-ref git-available-cache exe))
      (ahash-set! git-available-cache exe
                  (with ret (git-run-with-input (system->url "$HOME") #f
                                                "--version")
                    (if (git-ok? ret) 'yes 'no))))
    (== (ahash-ref git-available-cache exe) 'yes)))

;; Revisions and names of branches, tags, stashes or remotes may come from
;; untrusted sources (links in documents); they must never be taken for
;; options by Git.

(tm-define (git-safe-name? s)
  (:synopsis "Can @s be passed to Git as a revision or a name?")
  (and (string? s) (!= s "") (not (string-starts? s "-"))
       (not (string-index s #\newline))
       (not (string-index s (integer->char 0)))))

(tm-define (git-safe-names? l)
  (list-and (map git-safe-name? l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status of the working tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The status of a working tree is obtained from
;;   git status --porcelain=v2 -z --branch --untracked-files=all
;; and represented as an association list with the keys
;;   head (branch name or "(detached)"), oid, upstream, ahead, behind
;;   entries: a list of entries (kind xy path orig), where kind is one of
;;     ordinary, renamed, unmerged, untracked and xy is the two-character
;;     index/worktree status code of Git (with "." for unchanged).
;; The status is cached for a short time, since the menus ask for it often.

(define git-status-table (make-ahash-table))
(define git-tracked-table (make-ahash-table))
(define git-status-delay 2000)

(define (git-fields s n)
  ;; the n space separated fields of s, followed by the remainder of s
  (string-tokenize-by-char-n s #\space n))

(define (git-parse-branch s st)
  (let* ((l (string-tokenize-by-char-n (string-drop s 2) #\space 1))
         (key (car l))
         (val (if (null? (cdr l)) "" (cadr l))))
    (cond ((== key "branch.oid") (acons 'oid val st))
          ((== key "branch.head") (acons 'head val st))
          ((== key "branch.upstream") (acons 'upstream val st))
          ((== key "branch.ab")
           (with ab (string-tokenize-by-char val #\space)
             (acons 'ahead (string->number (string-drop (car ab) 1))
                    (acons 'behind (string->number (string-drop (cadr ab) 1))
                           st))))
          (else st))))

(define (git-parse-status s)
  (let loop ((l (git-split s nul-char)) (st '()) (entries '()))
    (if (null? l)
        (acons 'entries (reverse entries) st)
        (with r (car l)
          (cond ((string-starts? r "# ")
                 (loop (cdr l) (git-parse-branch r st) entries))
                ((string-starts? r "1 ")
                 (with f (git-fields r 8)
                   (loop (cdr l) st
                         (cons (list 'ordinary (second f) (cAr f) #f)
                               entries))))
                ((and (string-starts? r "2 ") (nnull? (cdr l)))
                 (with f (git-fields r 9)
                   (loop (cddr l) st
                         (cons (list 'renamed (second f) (cAr f) (cadr l))
                               entries))))
                ((string-starts? r "u ")
                 (with f (git-fields r 10)
                   (loop (cdr l) st
                         (cons (list 'unmerged (second f) (cAr f) #f)
                               entries))))
                ((string-starts? r "? ")
                 (loop (cdr l) st
                       (cons (list 'untracked "??" (string-drop r 2) #f)
                             entries)))
                (else (loop (cdr l) st entries)))))))

(tm-define (git-invalidate root)
  (:synopsis "Forget cached information about the working tree @root")
  (when root
    (ahash-remove! git-status-table (url->system root))
    (ahash-remove! git-tracked-table (url->system root))))

(tm-define (git-status root)
  (:synopsis "Status of the working tree @root (or #f)")
  (and root (git-status-in root)))

(define (git-status-in root)
  (let* ((key (url->system root))
         (old (ahash-ref git-status-table key)))
    (if (and old (< (- (texmacs-time) (car old)) git-status-delay))
        (cdr old)
        (with ret (git-run root "status" "--porcelain=v2" "-z" "--branch"
                           "--untracked-files=all")
          (with st (and (git-ok? ret) (git-parse-status (git-out ret)))
            (ahash-set! git-status-table key (cons (texmacs-time) st))
            (ahash-remove! git-tracked-table key)
            st)))))

(tm-define (git-status-cached root)
  (:synopsis "The last known status of @root, without running Git")
  (with old (and root (ahash-ref git-status-table (url->system root)))
    (and old (cdr old))))

(tm-define (git-status-ref st key)
  (and st (assoc-ref st key)))

(tm-define (git-entry-kind e) (first e))
(tm-define (git-entry-index e) (string-ref (second e) 0))
(tm-define (git-entry-worktree e) (string-ref (second e) 1))
(tm-define (git-entry-path e) (third e))
(tm-define (git-entry-orig e) (fourth e))

(tm-define (git-entry-staged? e)
  (and (in? (git-entry-kind e) '(ordinary renamed))
       (!= (git-entry-index e) #\.)))

(tm-define (git-entry-unstaged? e)
  (and (in? (git-entry-kind e) '(ordinary renamed))
       (!= (git-entry-worktree e) #\.)))

(tm-define (git-entry-untracked? e) (== (git-entry-kind e) 'untracked))
(tm-define (git-entry-conflicted? e) (== (git-entry-kind e) 'unmerged))

(tm-define (git-status-entries root)
  (or (git-status-ref (git-status root) 'entries) '()))

(tm-define (git-file-entry root u)
  (with path (git-relative root u)
    (list-find (git-status-entries root)
               (lambda (e) (== (git-entry-path e) path)))))

(define (git-tracked? root u)
  (let* ((key (url->system root))
         (tab (or (ahash-ref git-tracked-table key)
                  (with t (make-ahash-table)
                    (ahash-set! git-tracked-table key t)
                    t)))
         (path (git-relative root u)))
    (with old (ahash-ref tab path)
      (if old (== old 'yes)
          (with ret (git-run root "ls-files" "--error-unmatch" "--" path)
            (ahash-set! tab path (if (git-ok? ret) 'yes 'no))
            (git-ok? ret))))))

(tm-define (git-file-state u)
  (:synopsis "State of the file @u in its Git working tree")
  ;; One of untracked, unmodified, modified (only in the working tree),
  ;; staged (all changes staged), partial (staged and unstaged changes),
  ;; added (new file in the index), deleted, conflicted or #f
  (and-with root (git-root u)
    (with e (git-file-entry root u)
      (cond ((not e)
             (if (git-tracked? root u) 'unmodified 'untracked))
            ((git-entry-untracked? e) 'untracked)
            ((git-entry-conflicted? e) 'conflicted)
            ((or (== (git-entry-index e) #\D)
                 (== (git-entry-worktree e) #\D)) 'deleted)
            ((and (git-entry-staged? e) (git-entry-unstaged? e)) 'partial)
            ((== (git-entry-index e) #\A) 'added)
            ((git-entry-staged? e) 'staged)
            (else 'modified)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A commit is represented by a list (hash parents author date subject),
;; where parents is a list of hashes.  Strings are in the utf8 encoding.

(define git-log-format
  (string-append "--format=" "%x1e%H%x1f%P%x1f%an%x1f%ad%x1f%s"))

(define git-date-format "--date=format:%Y-%m-%d %H:%M")

(define (git-parse-commit r)
  ;; NOTE: with -z, the file names are separated by null characters
  (let* ((lines (with l (git-split r "\n") (if (null? l) (list "") l)))
         (header (with l (git-split (car lines) nul-char)
                   (if (null? l) "" (car l))))
         (fields (git-split header unit-sep))
         (names (append-map (cut git-split <> nul-char) (cdr lines)))
         (files (list-filter names (lambda (s) (!= s "")))))
    (and (>= (length fields) 5)
         (list (first fields)
               (list-filter (string-tokenize-by-char (second fields) #\space)
                            (lambda (h) (!= h "")))
               (third fields)
               (fourth fields)
               (fifth fields)
               files))))

(define (git-parse-log s)
  (list-filter (map git-parse-commit (git-split s record-sep)) identity))

(tm-define (git-commit-hash c) (first c))
(tm-define (git-commit-parents c) (second c))
(tm-define (git-commit-author c) (third c))
(tm-define (git-commit-date c) (fourth c))
(tm-define (git-commit-subject c) (fifth c))
(tm-define (git-commit-files c) (sixth c))

(tm-define (git-log root skip count . revs)
  (:synopsis "The @count commits of @root after the first @skip ones")
  (with out (and (git-safe-names? revs)
                 (apply git-output
                   (append (list root "log" git-log-format git-date-format
                                 (string-append "--skip=" (number->string skip))
                                 (string-append "--max-count="
                                                (number->string count)))
                           revs)))
    (if out (git-parse-log out) '())))

(tm-define (git-graph root count)
  (:synopsis "The graph of the @count last commits of all branches")
  ;; List of lines (graph-prefix commit), where commit is #f for lines
  ;; which only continue the graph
  (with out (git-output root "log" "--graph" "--all" "--date-order"
                        git-date-format
                        "--format=%x1e%H%x1f%P%x1f%an%x1f%ad%x1f%s%x1f%D"
                        (string-append "--max-count=" (number->string count)))
    (if (not out) '()
        (map (lambda (line)
               (with pos (string-search-forwards record-sep 0 line)
                 (if (< pos 0)
                     (list line #f)
                     (list (substring line 0 pos)
                           (with f (git-split (substring line (+ pos 1)
                                                         (string-length line))
                                              unit-sep)
                             (and (>= (length f) 5) f))))))
             (list-filter (git-split out "\n") (lambda (l) (!= l "")))))))

(tm-define (git-file-log u)
  (:synopsis "The commits which modified @u, following renames")
  ;; The last element of each commit is the list with the name of the file
  ;; at that commit
  (and-with root (git-root u)
    (and-with out (git-output root "log" "--follow" "--name-only" "-z"
                              git-log-format git-date-format
                              (string-append "--max-count="
                                             (number->string (git-log-length)))
                              "--" (git-relative root u))
      (git-parse-log out))))

(tm-define (git-commit-info root rev)
  (:synopsis "Information about the commit @rev in @root, or #f")
  (with l (git-log root 0 1 rev)
    (and (nnull? l) (car l))))

(tm-define (git-commit-message root rev)
  (:synopsis "Full message of the commit @rev")
  (or (and (git-safe-name? rev)
           (git-output root "show" "--no-patch" "--format=%B" rev))
      ""))

(tm-define (git-numstat root rev . parent)
  (:synopsis "List of (added removed path) for the changes of @rev")
  ;; Binary files have #f for added and removed
  (with out (cond ((not (git-safe-names? (cons rev parent))) #f)
                  ((null? parent)
                   (git-output root "show" "--numstat" "--format=" "-z" rev))
                  (else
                   (git-output root "diff" "--numstat" "-z"
                               (car parent) rev)))
    (if (not out) '()
        (let loop ((l (git-split out nul-char)) (acc '()))
          (cond ((null? l) (reverse acc))
                ((== (car l) "") (loop (cdr l) acc))
                (else
                  (with f (string-tokenize-by-char-n (car l) #\tab 2)
                    (if (and (== (length f) 3) (== (third f) "")
                             (>= (length l) 3))
                        ;; rename: the old and new names follow
                        (loop (cdddr l)
                              (cons (list (string->number (first f))
                                          (string->number (second f))
                                          (third l))
                                    acc))
                        (loop (cdr l)
                              (cons (list (string->number (first f))
                                          (string->number (second f))
                                          (if (>= (length f) 3) (third f) ""))
                                    acc))))))))))

(tm-define (git-show-file root rev path)
  (:synopsis "Contents of the file with @path at the revision @rev")
  ;; NOTE: rev may be empty or :1, :2, :3 for the stages in the index
  (or (and (or (== rev "") (git-safe-name? rev))
           (git-output root "show" (string-append rev ":" path)))
      ""))

(tm-define (git-rev-parse root rev)
  (and (git-safe-name? rev)
       (and-with out (git-output root "rev-parse" "--verify" "--quiet" rev)
         (git-chomp out))))

(tm-define (git-merging? root)
  (:synopsis "Is a merge in progress in @root?")
  (nnot (git-rev-parse root "MERGE_HEAD")))

(tm-define (git-merge-message root)
  (:synopsis "The prepared message for the merge in progress, or #f")
  (and-with out (git-output root "rev-parse" "--git-path" "MERGE_MSG")
    (with f (with u (unix->url (git-chomp out))
              (if (url-rooted? u) u (url-append root u)))
      (and (url-exists? f)
           (list-filter (git-split (string-load f) "\n")
                        (lambda (l) (not (string-starts? l "#"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Branches, tags and remotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A branch is a list (name current? upstream track date subject)

(define (git-parse-ref r)
  (with f (git-split r unit-sep)
    (and (>= (length f) 6)
         (list (first f) (== (second f) "*") (third f)
               (fourth f) (fifth f) (sixth f)))))

(define (git-refs root pattern)
  (with out (git-output root "for-each-ref" "--sort=-committerdate"
                        (string-append
                         "--format=%(refname:short)%1f%(HEAD)%1f"
                         "%(upstream:short)%1f%(upstream:track)%1f"
                         "%(committerdate:format:%Y-%m-%d %H:%M)%1f"
                         "%(contents:subject)")
                        pattern)
    (if (not out) '()
        (list-filter (map git-parse-ref (git-split out "\n")) identity))))

(tm-define (git-branches root) (git-refs root "refs/heads"))
(tm-define (git-remote-branches root) (git-refs root "refs/remotes"))
(tm-define (git-tags root) (git-refs root "refs/tags"))

(tm-define (git-branch-name b) (first b))
(tm-define (git-branch-current? b) (second b))
(tm-define (git-branch-upstream b) (third b))
(tm-define (git-branch-track b) (fourth b))

(tm-define (git-current-branch root)
  (with h (git-status-ref (git-status root) 'head)
    (and h (!= h "(detached)") h)))

(tm-define (git-remotes root)
  (with out (git-output root "remote")
    (if out (list-filter (git-split out "\n") (lambda (s) (!= s ""))) '())))

(tm-define (git-remote-url root name)
  (and (git-safe-name? name)
       (and-with out (git-output root "remote" "get-url" name)
         (git-chomp out))))

(tm-define (git-push-remote root)
  (:synopsis "The remote to which the current branch is pushed by default")
  (let* ((up (git-status-ref (git-status root) 'upstream))
         (l (git-remotes root)))
    (cond ((and up (string-index up #\/))
           (car (string-tokenize-by-char up #\/)))
          ((in? "origin" l) "origin")
          ((nnull? l) (car l))
          (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signatures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-signing?)
  (== (get-preference "git sign") "on"))

(tm-define (git-toggle-signing)
  (:synopsis "Toggle the signature of commits and tags with GnuPG")
  (:check-mark "v" git-signing?)
  (set-preference "git sign" (if (git-signing?) "off" "on")))

(tm-define (git-test-pull-mode? m)
  (== (get-preference "git pull mode") m))

(tm-define (git-set-pull-mode m)
  (:synopsis "Set the way in which remote changes are pulled")
  (:check-mark "*" git-test-pull-mode?)
  (set-preference "git pull mode" m))

(tm-define (git-commit-options)
  (:synopsis "Additional options for all commits")
  (if (git-signing?) (list "--gpg-sign") '()))

(tm-define (git-signature root rev)
  (:synopsis "Description of the signature of the commit @rev, or #f")
  ;; See the %G? placeholder of git log
  (and-with out (and (git-safe-name? rev)
                     (git-output root "show" "--no-patch"
                                 "--format=%G?%x1f%GS%x1f%GK" rev))
    (with f (git-split (git-chomp out) unit-sep)
      (and (== (length f) 3)
           (with c (car f)
             (and (!= c "N")
                  (string-append
                   (cond ((== c "G") "good signature")
                         ((== c "U") "good signature, unknown validity")
                         ((== c "X") "good signature, expired")
                         ((== c "Y") "good signature, expired key")
                         ((== c "R") "good signature, revoked key")
                         ((== c "E") "signature cannot be checked")
                         ((== c "B") "BAD signature")
                         (else "signature"))
                   (if (== (cadr f) "") ""
                       (string-append " by " (cadr f)))
                   (if (== (caddr f) "") ""
                       (string-append " (key " (caddr f) ")")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indicator in the footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The footer is updated very often, so the indicator only uses the cached
;; status; if there is none yet, then the status is computed once idle.

(define git-footer-pending (make-ahash-table))

(define (footer-schedule root)
  (with key (url->system root)
    (when (not (ahash-ref git-footer-pending key))
      (ahash-set! git-footer-pending key #t)
      (delayed
        (:idle 500)
        (git-status root)
        (ahash-remove! git-footer-pending key)))))

(define (footer-count n singular plural)
  (string-append (number->string n) " " (if (== n 1) singular plural)))

(tm-define (git-footer-text root)
  (:synopsis "Short description of the state of @root for the footer")
  (with st (git-status-cached root)
    (if (not st)
        (begin (footer-schedule root) #f)
        (let* ((head (or (git-status-ref st 'head) "?"))
               (l (or (git-status-ref st 'entries) '()))
               (n (length l))
               (c (length (list-filter l git-entry-conflicted?)))
               (ahead (or (git-status-ref st 'ahead) 0))
               (behind (or (git-status-ref st 'behind) 0)))
          (string-recompose
           (append (list (string-append "Git " (utf8->cork head)))
                   (cond ((> c 0) (list (footer-count c "conflict"
                                                      "conflicts")))
                         ((> n 0) (list (footer-count n "change" "changes")))
                         (else (list "saved")))
                   (if (> ahead 0) (list (string-append
                                          "<#2191>" (number->string ahead)))
                       '())
                   (if (> behind 0) (list (string-append
                                           "<#2193>" (number->string behind)))
                       '())
                   (if (git-busy? root) (list "working...") '()))
           " <#B7> ")))))

(tm-define (git-footer t)
  (:synopsis "Add the state of the Git working tree to the footer @t")
  (let* ((u (current-buffer))
         (root (and u (git-root u)))
         (s (and root (git-footer-text root))))
    (if (not s) t
        (stree->tree `(concat ,(tree->stree t) (hspace "2em")
                              (with "color" "dark grey" ,s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Large files and recent repositories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-large-file? u)
  (:synopsis "Is @u too large for being conveniently versioned?")
  (with mb (or (string->number (get-preference "git large file size")) 10)
    (and (url-exists? u) (not (url-directory? u))
         (> (url-size u) (* mb 1024 1024)))))

(tm-define (git-recent-repositories)
  (:synopsis "Recently used working trees (system paths)")
  (list-filter (git-split (get-preference "git recent repositories") "\n")
               (lambda (s) (and (!= s "")
                                (url-exists? (url-append (system->url s)
                                                         ".git"))))))

(tm-define (git-remember-repository root)
  (let* ((s (url->system root))
         (l (cons s (list-filter (git-recent-repositories)
                                 (lambda (x) (!= x s))))))
    (set-preference "git recent repositories"
                    (string-recompose (sublist l 0 (min 10 (length l)))
                                      "\n"))))

(tm-define (git-stashes root)
  (:synopsis "List of (name subject) for the stashes of @root")
  (with out (git-output root "stash" "list" "--format=%gd%x1f%s")
    (if (not out) '()
        (list-filter (map (cut git-split <> unit-sep) (git-split out "\n"))
                     (lambda (x) (== (length x) 2))))))
