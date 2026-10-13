
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-femtolisp.scm
;; DESCRIPTION : femtolisp-specific start of the initialization
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The initialization file of TeXmacs on femtolisp (scheme_init_file in
;; femtolisp_tm.cpp), loaded with the load of femtolisp. It loads standard
;; Scheme and the module system, the kernel's compatibility module, then the
;; common init-kernel.scm and init-texmacs.scm.

(define boot-start (texmacs-time))

(define developer-mode? #f)

(load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/r5rs-femtolisp.scm"))
(load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot-femtolisp.scm"))

(inherit-modules (kernel boot compat-femtolisp))

;; The common initialization
(load (url-concretize "$TEXMACS_PATH/progs/init-kernel.scm"))
(load (url-concretize "$TEXMACS_PATH/progs/init-texmacs.scm"))
