#lang racket

(require "grader.rkt"
         "walker.rkt")

(provide grade-encapsulated-template-fns

         grade-nr-intact grade-mr-intact grade-nh-intact
         grade-try-catch grade-no-try-catch
         grade-questions-intact
         grade-prohibited-calls)


#|

Standard directions are:


|#


(define-syntax (grade-encapsulated-template-fns stx)
  (syntax-case stx ()
    [(_ (fn-name ...) body-expr ...)
     #'(let* ([defns (htdf-defns (car (context)))]
              [defn  (and (pair? defns) (car defns))]
              [body  (and (list? defn) (= (length defn) 3) (caddr defn))]
              
              [local-defns (and (list? body) (= (length body) 3) (eqv? (car body) 'local) (cadr body))]
                 
              [all-locals? (= (length local-defns) (length '(fn-name ...)))] ;!!!

              [fn-name (find-defn 'fn-name local-defns)] ...)
         body-expr ...)]))

(define-syntax (guard-template-fn-grading stx)
  (syntax-case stx ()
    [(_ fn-name topic what body)
     #'(cond [(not fn-name)
              (score #f topic 1 0 '() (list (message #f "~a: not graded because template function deleted or renamed." what)))]
             [(not (filled? fn-name))
              (score #f topic 1 0 '() (list (message #f "~a: not graded because ... still present in function definition." what)))]
             [else
              body])]))


(define-syntax (grade-nr-intact stx)
  (syntax-case stx ()
    [(_ fn-name)
     #'(let ([what (format "~a - natural recursion intact" 'fn-name)])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (calls? fn-name 'fn-name)
                                                 what)))]))

(define-syntax (grade-mr-intact stx)
  (syntax-case stx ()
    [(_ fn-name called-fn ...)
     #'(let ([what (format "~a - mutual recursion~a intact"
                           'fn-name 
                           (if (null? (cdr '(called-fn ...))) "" "s"))])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (calls-all? fn-name '(called-fn ...))
                                                 what)))]))

(define-syntax (grade-nh-intact stx)
  (syntax-case stx ()
    [(_ fn-name called-fn ...)
     #'(let ([what (format "~a - natural helper~a intact"
                           'fn-name
                           (if (null? (cdr '(called-fn ...))) "" "s"))])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (calls-all? fn-name '(called-fn ...))
                                                 what)))]))

(define-syntax (grade-try-catch stx)
  (syntax-case stx ()
    [(_ fn-name)
     #'(let ([what (format  "~a - no try-catch blended in" 'fn-name)])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (and fn-name
                                                      (filled? fn-name)
                                                      (has-try-catch? fn-name))
                                                 what)))]))
                   

(define-syntax (grade-no-try-catch stx)
  (syntax-case stx ()
    [(_ fn-name)
     #'(let ([what (format  "~a - try-catch blended in" 'fn-name)])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (and fn-name
                                                      (filled? fn-name)
                                                      (not (has-try-catch? fn-name)))
                                                 what)))]))

(define-syntax (grade-questions-intact stx)
  (syntax-case stx ()    
    [(_ fn-name qa-pair ...)
     #'(let ([what (format "~a - cond questions intact" 'fn-name)])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (and fn-name
                                                      (filled? fn-name)
                                                      (let [(cond-expr (get-cond fn-name))]
                                                        (list? cond-expr)
                                                        (equal? (map car (cdr cond-expr))
                                                                (map car '(qa-pair ...)))))
                                                 what)))]))

(define-syntax (grade-prohibited-calls stx)
  (syntax-case stx ()
    [(_ fn-name bad-fns-to-call)
     #'(let ([what (format "~a - does not call any of ~a" 'fn-name 'bad-fns-to-call)])
         (guard-template-fn-grading fn-name 'template-intact what
                                    (rubric-item 'template-intact 1
                                                 (and (filled? fn-name)
                                                      (not (ormap (lambda (bad) (calls? fn-name bad)) 'bad-fns-to-call)))
                                                 what)))]))



(define (find-defn name defns)
  (findf (lambda (defn) (eqv? (caadr defn) name)) defns))

;; !!! move this to walker and make it call walk- directly so it produces true as soon as it finds name
(define (calls? defn name)
  (and defn
       (filled? defn)
       (member name (called-fn-names defn))))

;; !!! ditto
(define (calls-all? defn names)
  (andmap (lambda (name) (calls? defn name)) names))

(define (get-cond defn) ;!!! make this work in presence of try-catch
  (and (pair? defn)
       (= (length defn) 3)
       (pair? (caddr defn))
       (eqv? (caaddr defn) 'cond)
       (caddr defn)))
  
