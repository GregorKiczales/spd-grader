#lang racket/base

(provide %%eval-test-args-and-results
         %%check-validity
         %%check-thoroughness)


;;
;; The purpose of the code in this file is to reduce how much repeated code is
;; generated and pushed over to the sandbox to run. It's possible this will have
;; an observable effect on performance, but even if not, it's a plus in terms of
;; simplifying the runtime architecture of the grader.
;;


;; (listof (_ -> (listof Any))) -> (listof (listof Any)|'error)
(define (%%eval-test-args-and-results thunks)
  (for/list ([thunk thunks])
    (with-handlers ([exn:fail? (lambda (e) 'error)])
      (thunk (void)))))

;; produce uids (index numbers) of tests that fail at least once
(define (%%check-validity lo-args-with-result checker-names checker-thunks)
  (let loop ([lo-args-with-result lo-args-with-result]
             [test-uid 0])
    (cond [(null? lo-args-with-result) '()]
          [else
           (mergeq (check-validity-thunks (car lo-args-with-result) checker-names checker-thunks)
                   (loop (cdr lo-args-with-result) (add1 test-uid)))])))

;; produce names of checks that are satisfied at least once
(define (%%check-thoroughness lo-args aa-check-names pa-check-names aa-check-thunks pa-check-thunks)
  (mergeq (check-thoroughness-thunks (list lo-args) aa-check-names aa-check-thunks)
          (let loop ([lo-args lo-args])
            (cond [(null? lo-args) '()]
                  [else
                   (mergeq (check-thoroughness-thunks (car lo-args) pa-check-names pa-check-thunks)
                           (loop (cdr lo-args)))]))))


(define (check-validity-thunks args-with-result test-uid thunks)
  (let loop ([thunks thunks])
    (cond [(null? thunks) '()]
          [else
           (if (not (apply (car thunks) args-with-result))
               (cons-if-not-memq test-uid
                                 (loop (cdr thunks)))
               (loop (cdr thunks)))])))

(define (check-thoroughness-thunks args names thunks)
  (let loop ([names names]
             [thunks thunks])
    (cond [(null? names) '()]
          [else
           (if (apply (car thunks) args)
               (cons-if-not-memq (car names)
                                 (loop (cdr names) (cdr thunks)))
               (loop (cdr names) (cdr thunks)))])))



(define (mergeq l1 l2)
  (cond [(null? l1) l2]
        [else
         (cons-if (not (memq (car l1) l2))
                  (car l1)
                  (mergeq (cdr l1) l2))]))

(define (cons-if q x lox)
  (if q (cons x lox) lox))

(define (cons-if-not-memq x lox)
  (cons-if (not (memq x lox)) x lox))


