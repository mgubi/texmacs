;;;; finalizer

;; from https://github.com/macdavid313/chez-finalize
;; commit 39689a14733c461cae3467172041ab32789534fd

;; MIT License
;;
;; Copyright (c) 2019 David Gu
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (finalize)
  (export finalize set-collect-limit!)
  (import (chezscheme))
  
  (define %finalizers (make-weak-eq-hashtable))

  (define %collect-limit 0)
  
  (define garbage-pool (make-guardian))

  (define (set-collect-limit! limit)
    (unless (and (fixnum? limit) (fx>=? limit 1))
      (assertion-violationf 'set-%collect-limit "Invalid limit: ~a" limit))
    (set! %collect-limit limit))

  (define (finalize object free)
    (unless (and (procedure? free)
                 (logbit? 1 (procedure-arity-mask free)))
      (assertion-violationf 'finalize "~a is not a unary procedure" free))
    (unless (hashtable-ref %finalizers object #f)
      ;; when the entry doesn't exist,
      ;; send the object to guardian and store the free function
      (garbage-pool object)
      (hashtable-set! %finalizers object free))
    object)

  (collect-request-handler
   (lambda ()
     (collect)
     (do ([x (garbage-pool) (garbage-pool)]
          [i 0 (fx1+ i)])
         ((or (not x)
              (and (not (fxzero? %collect-limit))
                   (fx=? i %collect-limit))))
       (let ([free (hashtable-ref %finalizers x #f)])
         (when free (free x))))))

  ) ;; end of library
