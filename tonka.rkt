#lang racket/base

(require racket/function)

(provide %%check-validity
         %%check-argument-thoroughness
         %%check-faulty-functions

         %%call-thunks-with-handler
         
         %%equal-sets?
         %%check-random)


;;
;; The purpose of the code in this file is to reduce how much repeated code is
;; generated and pushed over to the sandbox to run. It's possible this will have
;; an observable effect on performance, but even if not, it's a plus in terms of
;; simplifying the runtime architecture of the grader.
;;
;; Note the generated code in the runtime architecture has to run in BSL, so we
;; are it is not allowed to define zero-argument functions, and it is not allowed
;; to use lambda.
;;


;; produce (listof #t|#f|error) indicating whether each check is valid
(define (%%check-validity aar-thunks all-criteria)
  (for/list ([args-and-result (%%call-thunks-with-handler aar-thunks)])
    (if (eqv? args-and-result 'error)
        'error
        (with-handlers ([exn:fail? (lambda (e) 'error)])
          (apply all-criteria (append (car args-and-result) (cdr args-and-result)))))))

;; produce names of checks that are satisfied at least once
(define (%%check-argument-thoroughness lo-args aa-check-names pa-check-names aa-check-thunks pa-check-thunks)
  (append (if (null? aa-check-names)
              '()
              (check-thoroughness-thunks (list lo-args) aa-check-names aa-check-thunks))
          (if (null? pa-check-names)
              '()
              (let loop ([lo-args lo-args])
                (cond [(null? lo-args) '()]
                      [else
                       (mergeq (check-thoroughness-thunks (car lo-args) pa-check-names pa-check-thunks)
                               (loop (cdr lo-args)))])))))

(define (check-thoroughness-thunks args names thunks)
  (let loop ([names names]
             [thunks thunks])
    (cond [(null? names) '()]
          [else
           (if (apply (car thunks) args)
               (cons-if-not-memq (car names)
                                 (loop (cdr names) (cdr thunks)))
               (loop (cdr names) (cdr thunks)))])))

(define (%%check-faulty-functions all-tests faulty-fns)
  (for/list ([fn faulty-fns])
    (with-handlers ([exn:fail? (lambda (e) 'error)])
      (all-tests fn))))



;; (listof (_ -> (listof Any))) -> (listof (listof Any)|'error)
(define (%%call-thunks-with-handler thunks)
  (for/list ([thunk thunks])
    (with-handlers ([exn:fail? (lambda (e) 'error)])
      (thunk (void)))))



(define (%%equal-sets? s1 s2) 
  (and (andmap (curryr member s2) s1)
       (andmap (curryr member s1) s2)
       #t))

(define (%%check-random thunk1 thunk2)
  (let ([rng (make-pseudo-random-generator)]
        [k (modulo (current-milliseconds) (sub1 (expt 2 31)))])
    (equal? (begin (current-pseudo-random-generator rng)
                   (random-seed k)
                   (thunk1 (void)))
            (begin (current-pseudo-random-generator rng)
                   (random-seed k)
                   (thunk2 (void))))))




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


