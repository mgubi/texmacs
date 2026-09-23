
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-drivers.scm
;; DESCRIPTION : structured merging of TeXmacs documents from within Git
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git can be told to merge TeXmacs documents with TeXmacs itself:
;;   .gitattributes:  *.tm merge=texmacs
;;   git config:      merge.texmacs.driver "<texmacs> -headless -x
;;                      '(git-merge-driver \"%O\" \"%A\" \"%B\")'"
;; The driver writes the structured three way merge into the file of our
;; version, and exits with a non zero code if conflicts remain.  The
;; conflicts are then marked up as differences, which can be resolved
;; in TeXmacs, instead of the usual textual conflict markers.

(texmacs-module (version git-drivers)
  (:use (version version-merge)
        (version git-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The merge driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (driver-url name)
  (with u (system->url name)
    (if (url-rooted? u) u (url-append (url-pwd) u))))

(define (load-document name)
  (with t (tree->stree (tree-import (driver-url name) "texmacs"))
    (and (document-body t) t)))

(tm-define (git-merge-files base ours theirs)
  (:synopsis "Merge @theirs into @ours with ancestor @base; #f on failure")
  ;; Returns the number of remaining conflicts
  (let* ((o (load-document base))
         (a (load-document ours))
         (b (load-document theirs)))
    (and o a b
         (let* ((m (merge-versions (document-body o) (document-body a)
                                   (document-body b)))
                (doc (document-set-body a m)))
           (and (not (tree-export (stree->tree doc) (driver-url ours)
                                  "texmacs"))
                (merge-conflicts m))))))

(tm-define (git-merge-driver base ours theirs)
  (:synopsis "Entry point for the merge driver of Git")
  (with n (catch #t
            (lambda () (git-merge-files base ours theirs))
            (lambda args #f))
    (quit-TeXmacs-code (if (== n 0) 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing the driver in a repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-executable)
  (let* ((tp (url->system (get-texmacs-path)))
         (bin (string-append tp "/bin/texmacs.bin"))
         (mac (string-append tp "/../../../MacOS/TeXmacs")))
    (cond ((url-exists? (system->url bin)) bin)
          ((url-exists? (system->url mac)) mac)
          (else "texmacs"))))

(tm-define (git-merge-driver-command)
  (:synopsis "The shell command which Git should use for merging documents")
  (string-append "TEXMACS_PATH="
                 (git-shell-quote (url->system (get-texmacs-path))) " "
                 (git-shell-quote (texmacs-executable))
                 " -headless -x "
                 (git-shell-quote "(git-merge-driver \"%O\" \"%A\" \"%B\")")
                 " > /dev/null 2>&1"))

(define attributes-line "*.tm merge=texmacs")

(define (attributes-file root) (url-append root ".gitattributes"))

(tm-define (git-merge-driver-installed? root)
  (:synopsis "Does @root merge TeXmacs documents with TeXmacs?")
  (and (git-output root "config" "--get" "merge.texmacs.driver")
       (with f (attributes-file root)
         (and (url-exists? f)
              (in? attributes-line (git-split (string-load f) "\n"))))))

(tm-define (git-install-merge-driver root)
  (:synopsis "Make Git merge TeXmacs documents in @root with TeXmacs")
  ;; NOTE: the driver itself is configured locally, since it depends on
  ;; the installation; .gitattributes can be shared with collaborators,
  ;; for whom Git falls back to its textual merge
  (git-run root "config" "merge.texmacs.name" "TeXmacs structured merge")
  (git-run root "config" "merge.texmacs.driver" (git-merge-driver-command))
  (let* ((f (attributes-file root))
         (s (if (url-exists? f) (string-load f) "")))
    (when (nin? attributes-line (git-split s "\n"))
      (string-save (string-append s
                                  (if (or (== s "") (string-ends? s "\n"))
                                      "" "\n")
                                  attributes-line "\n")
                   f)))
  (git-invalidate root)
  (set-message "TeXmacs documents will be merged structurally" "Git"))
