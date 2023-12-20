#lang racket

(require "utils.rkt"
         "grader.rkt")

(provide grade-lifted-definitions)


(define-syntax (grade-lifted-definitions stx)
  (syntax-case stx ()
    [(_ defn0 call0 defn ...)
     #'(check-lifted-definitions 'defn0 'call0 '(defn ...))]))


(define (check-lifted-definitions defn0 call0 sol-defns)
  (let* ([sexps (problem-sexps (car (context)))]
         [sexps (and (>= (length sexps) 2)
                     ;; some problems don't have the starter uncommented in the file
                     (cond [(and (equal? (car  sexps) defn0)
                                 (equal? (cadr sexps) call0))
                            (cddr sexps)]
                           [(or (equal? (car  sexps) defn0)
                                (equal? (cadr sexps) call0))
                            (cdr sexps)]
                           [else sexps]))])

    (combine-scores
     (weights* 1.0 '(*)
       (let loop
           ([sol-defns      sol-defns] ;go through this
            [sub-defns      sexps]     ;consuming this perhaps out of order
            [original-names '()])      ;building up this
         (if (null? sol-defns)
             '()
             (let* ([sol-defn      (car sol-defns)]
                    [lifted-name   (caadr sol-defn)]
                    [original-name (drop_n lifted-name)]
                    [sub-defn      (findf (lambda (d)
                                            (and (fn-defn? d)
                                                 (name-base? (caadr d) original-name)))
                                          sub-defns)]
                    [nth           (number->ordinal (add1 (occurrences original-name original-names)))])
               (cons (grade-prerequisite 'other (format "Lifting of ~a exists" original-name) sub-defn
                       (header (format "~a lifting of ~a" nth original-name)
                         (weights (*)
                           (rubric-item 'other (has_?  (caadr sub-defn) original-name)    "new name has _ after original name")
                           (rubric-item 'other (equal? (cdadr sub-defn) (cdadr sol-defn)) "parameters")
                           (rubric-item 'other (equal? (caddr sub-defn) (caddr sol-defn)) "body"))))
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
