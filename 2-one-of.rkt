#lang racket

(require racket/list
         "walker.rkt"
         "grader.rkt"
         "utils.rkt")

(provide grade-2-one-of check-2-one-of)

(define-syntax (grade-2-one-of stx)
  (syntax-case stx (cond)
      [(_ n (p ...) (cond sp ...))
       #'(grade-2-one-of-fn 'n '(p ...) '(sp ...))]))

(define (grade-2-one-of-fn n params sol-pairs)
  (assert-context--@htdf)
  (let* ([htdf     (car (context))]
         [defns    (htdf-defns htdf)]
         [defn     (and (pair? defns)
                        (> (length defns) (sub1 n))
                        (list-ref defns (sub1 n)))])

    (check-2-one-of defn params sol-pairs)))

(define (check-2-one-of defn params sol-pairs)
  (ensure-parameter-names params)
  (let* ([cnd (get-cond defn)]
         [sub-pairs (and cnd (cdr cnd))])
    (weights (.1 .2 *)
      (grade-template-origin (2-one-of))
      (rubric-item 'template
                   1
                   (and (pair? sub-pairs) (= (length sub-pairs) (length sol-pairs)))
                   "cond reduced to ~a cases" (length sol-pairs))
      (if (not (pair? sub-pairs))
          (rubric-item 'template 1 #f "~a properly simplified questions" (length sol-pairs))
          (combine-scores
            (weights* 1.0 '(*)
              (for/list ([n  (in-naturals 0)]
                         [sol-pair sol-pairs])
                (rubric-item 'template
                             1
                             (and (> (length sub-pairs) n)
                                  (qequal? (car (list-ref sub-pairs n))
                                           (car sol-pair)
                                           (= n (sub1 (length sol-pairs)))))
                             "~a question is properly simplified"
                             (number->ordinal (add1 n))))))))))

(define (get-cond defn)
  (if (and (pair? (caddr defn))
           (eqv? (caaddr defn) 'local))
      (return-cond (caddr (caddr defn)))
      (return-cond        (caddr defn))))

(define (return-cond x)
  (and (pair? x)
       (eqv? (car x) 'cond)
       x))


(define (qequal? sub sol last?)
  (or (and last? (eqv? sub 'else))
      (equal? sub sol)
      (and (pair? sub)
           (pair? sol)
           (member (car sub) '(string=? = equal?))
           (eqv? (car sub) (car sol))
           (equal? (cdr sub) (reverse (cdr sol))))))
