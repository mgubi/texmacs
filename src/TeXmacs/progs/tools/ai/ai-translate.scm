
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-translate.scm
;; DESCRIPTION : asynchroneous AI-based language translation
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (tools ai ai-translate)
  (:use (utils library process)
        (version version-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-translation-agent src-lan dest-lan)
  (string-append "Please translate the following HTML snippet from "
                 src-lan " into " dest-lan ", without explanations: "))

(define ((translate-processed-one serial tp st model next) out)
  ;;(when out
  ;;  (display* "======================================================\n")
  ;;  (display* out "\n")
  ;;  (display* "------------------------------------------------------\n")
  ;;  (display* (car (cpp-ai-get-body (cpp-ai-output out model))) "\n")
  ;;  (display* "======================================================\n"))
  (cond ((not (process-active? serial)) (noop))
        ((and (tm? out) (tree-pointer->tree tp))
         (let* ((t (tree-pointer->tree tp))
                (st* (tm->stree t))
                (out* (cpp-ai-output out model))
                (out** (cpp-ai-get-body out*))
                (new-t (decompress-html (car out**) 1)))
           (when (== st st*)
             (tree-set! t new-t)
             (tree-correct-upwards t)))))
  (tree-pointer-detach tp)
  (next))

;; an empty answer (a failed request) leaves the paragraph as it is
(define ((translate-nonempty fun) out)
  (fun (and (!= out "") out)))

(define ((translate-process-one dest-lan) serial tp st next)
  (let* ((t (tree-pointer->tree tp))
         ;; the codes of this paragraph, freed once its answer is there
         (table (compress-table-new "translate"))
         (t* (with-compress-table table (lambda () (compress-tree t))))
         (in (compressed->html t* 1))
         (src-lan (tree-get-env t "language"))
         (model (get-preference "ai"))
         (agent (ai-translation-agent src-lan dest-lan))
         (return (compress-table-callback table
                   (translate-nonempty
                     (translate-processed-one serial tp st model next)))))
    (cond ((not (compressed-contains-text? t*))
           (return #f))
          ((not (process-running? 'translate))
           (return #f))
          (else
            (with cmd (cpp-ai-command in model agent)
              ;;(display* "Eval] " cmd "\n")
              ;; (true: nothing launched, and return will not be called)
              (when (cpp-ai-async-eval-command cmd return)
                (compress-table-free table)))))))

(define (make-translate-process lan)
  (make-process (translate-process-one lan) 'translate))

(tm-define (ai-translate* lan)
  ((make-translate-process lan)))

(tm-define (ai-abort-translate)
  (process-deactivate 'translate))
