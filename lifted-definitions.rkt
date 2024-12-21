#lang racket

(require "utils.rkt"
         "grader.rkt")

(provide grade-lifted-definitions)


(define-syntax (grade-lifted-definitions stx)
  (syntax-case stx ()
    [(_ [defn ...] call lifted ...)
     #'(check-lifted-definitions '(defn ...) 'call '(lifted ...))]))


(define (check-lifted-definitions defns call lifted)
  (let* ([sexps (problem-sexps (car (context)))]
         [sexps (cond [(and (>= (length sexps) (+ (length defns) 2))
                            (equal?      (take sexps (length defns))  defns)
                            (equal? (car (drop sexps (length defns))) call))
                       (drop sexps (+ (length defns) 1))]
                      
                      [(and (>= (length sexps) (+ (length defns) 1))
                            (equal? (take sexps (length defns)) defns))
                       (drop sexps (length defns))]

                      [(and (>= (length sexps) 2)
                            (equal? (car sexps) call))
                       (drop sexps 1)]
                      [else sexps])])

    (combine-scores
     (weights* 1.0 '(*)
       (let loop
           ([sol-defns      lifted]   ;go through this
            [sub-defns      sexps]    ;consuming this perhaps out of order
            [original-names '()])     ;building up this
         (if (null? sol-defns)
             '()
             (let* ([sol-defn      (car sol-defns)]
                    [lifted-name   (defn-name sol-defn)]
                    [original-name (drop_n lifted-name)]
                    [sub-defn      (findf (lambda (d)
                                            (cond [(fn-defn? sol-defn)
                                                   (and (fn-defn? d)
                                                        (name-base? (fn-defn-name d) original-name))]
                                                  [(const-defn? sol-defn)
                                                   (and (const-defn? d)
                                                        (name-base? (const-defn-name d) original-name))]
                                                  [else #f]))
                                          sub-defns)]
                    [nth           (number->ordinal (add1 (occurrences original-name original-names)))])
               (cons (grade-prerequisite 'other (format "~a lifting of ~a exists" nth original-name) sub-defn
                       (header (format "~a lifting of ~a" nth original-name)
                         (if (fn-defn? sub-defn)
                             (weights (*)
                               (rubric-item 'other (has_?  (fn-defn-name       sub-defn) original-name)    "new name has _ after original name")
                               (rubric-item 'other (equal? (fn-defn-parameters sub-defn) (fn-defn-parameters sol-defn)) "parameters")
                               (rubric-item 'other (equal? (fn-defn-body       sub-defn) (fn-defn-body sol-defn)) "body"))
                             (weights (*)
                               (rubric-item 'other (has_?  (const-defn-name       sub-defn) original-name)    "new name has _ after original name")
                               (rubric-item 'other (equal? (const-defn-value-expr sub-defn) (const-defn-value-expr sol-defn)) "body")))))
                     (loop (cdr sol-defns)
                           (remove sub-defn sub-defns)
                           (cons original-name original-names))))))))))


(define (drop_n x)
  (let* ([str (symbol->string x)]
         [n (index-of (string->list str) #\_ equal?)])
    (string->symbol (substring str 0 n))))


(define (name-base? x s)
  (let ([x-str (symbol->string x)]
        [s-str (symbol->string s)])
    (and (>= (string-length x-str) (string-length s-str))
         (string=? (substring x-str 0 (string-length s-str)) s-str))))

(define (has_? x s)
  (let* [(x-string (symbol->string x))
         (name_ (string-append (symbol->string s) "_"))
         (len_  (string-length name_))]

    (and (>= (string-length x-string) len_)
         (string=? (substring x-string (sub1 len_) len_) "_"))))

(define (occurrences x lox)
  (foldr (lambda (a b)
           (+ (if (equal? a x) 1 0) b))
         0 lox))
