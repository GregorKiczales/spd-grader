#lang racket

(require racket/list
         "walker.rkt"
         "grader.rkt"
         "utils.rkt")

(provide grade-2-one-of check-2-one-of)

(define-syntax (grade-2-one-of stx)
  (syntax-case stx (cond)
    [(_ n (p ...) (cond [q a] ...))
     #'(grade-2-one-of* 'n '(p ...) '(q ...) '(a ...))]))

(define (grade-2-one-of* n sol-params sol-questions sol-answers)
  (assert-context--@htdf)
  (let* ([htdf     (car (context))]
         [defns    (htdf-defns htdf)]
         [defn     (and (pair? defns)
                        (> (length defns) (sub1 n))
                        (list-ref defns (sub1 n)))])

    (check-2-one-of htdf defn sol-params sol-questions sol-answers)))

(define (check-2-one-of htdf defn sol-params sol-qs sol-as)  
  (header "2-one-of:"
    (ensuring
     (begin
       (ensure defn                             "(@htdf ~a) must define single top-level function" (cadr htdf))
       (ensure (pair? (caddr defn))             "~a function definition must be more than stub" (cadr htdf))
       (ensure (eqv?  (car (caddr defn)) 'cond) "~a function definition body must be a cond expression" (cadr htdf))

       (let* ([cnd         (get-cond defn)]
              [sub-params  (fn-defn-parameters defn)]
              [sub-pairs   (cdr cnd)]
              [sub-qs      (map car sub-pairs)]
              [sub-sub-qs  (get-sub-qs sub-qs)]
              [sub-as      (map cadr sub-pairs)]

              [sol-qs      (rename-params sub-params sol-params sol-qs (fn-defn-name defn))]
              [sol-sub-qs  (get-sub-qs sol-qs)]

              [invalid-sub-qs (filter (lambda (pq) (not (memf (lambda (opq) (qequal? pq opq)) sol-sub-qs))) sub-sub-qs)])


         (weights (.1 .40 .25 .25)
           (grade-template-origin (2-one-of))
           (rubric-item 'template-intact
                        (null? invalid-sub-qs)
                        "All sub-expressions in cond questions are well formed~a"
                        (cond [(null? invalid-sub-qs) ""]
                              [(null? (cdr invalid-sub-qs))
                               (format " - ~a is not formed according to template rules" (car invalid-sub-qs))]
                              [else
                               (format " - ~a are not formed according to template rules" (list->text invalid-sub-qs))]))
           (rubric-item 'template-intact
                        (and (pair? sub-pairs) (= (length sub-pairs) (length sol-qs)))
                        "cond expression has ~a cases (question/answer pairs)" (length sol-qs))
           (combine-scores
            (weights* 1.0 '(*)
              (for/list ([n     (in-naturals 0)]
                         [sol-q (drop-right sol-qs 1)]);leave else behind
                (rubric-item 'template-intact
                             (and (pair? sub-qs)
                                  (> (length sub-qs) n)
                                  (qequal? (list-ref sub-qs n)
                                           sol-q))
                             "~a question is properly simplified"
                             (number->ordinal (add1 n))))))))))))

(define (rename-params sub-params sol-params qs fn-name)
  (let loop ((sub-params sub-params)
             (sol-params sol-params)
             (qs         qs))
    (cond [(and (null? sub-params) (null? sol-params)) qs]
          [(null? sub-params) (rubric-item 'template "~a has too few parameters" fn-name)]
          [(null? sub-params) (rubric-item 'template "~a has too few parameters" fn-name)]
          [else
           (loop (cdr sub-params)
                 (cdr sol-params)
                 (subst (car sub-params) (car sol-params) qs))])))

;; no need to use walker?
(define (get-sub-qs qs)
  (define (for-one-q q)
    (cond [(not (pair? q)) (list q)]
          [(member (car q) '(and or not)) (map get-sub-qs (cdr q))]
          [else (list q)]))

  (foldr append '() (map for-one-q qs)))


(define (get-cond defn)
  (if (and (pair? (caddr defn))
           (eqv? (caaddr defn) 'local))
      (produce-cond (caddr (caddr defn)))
      (produce-cond        (caddr defn))))

(define (produce-cond x)
  (and (pair? x)
       (eqv? (car x) 'cond)
       x))

(define (qequal? sub sol)
  (or (equal? sub sol)
      (and (pair? sub)
           (pair? sol)
           (member (car sub) '(string=? = equal?))
           (eqv? (car sub) (car sol))
           (equal? (cdr sub) (reverse (cdr sol))))))
