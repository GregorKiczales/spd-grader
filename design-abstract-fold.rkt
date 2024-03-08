#lang racket

(require racket/list
         "grader.rkt"
         "utils.rkt"
         "check-signature-by-constraints.rkt")

(provide grade-design-abstract-fold check-design-abstract-fold)


#|

Standard directions are:

Complete the design of the following abstract fold function for Challenge.
Note that we have already given you the actual function definition and the
template tag. You must complete the design with a signature, purpose, and
the two following check-expects:

  - uses the fold function to produce a copy of T10
  - uses the fold function to count the total number of tasks in C4,
    which is 5

Be VERY CAREFUL WRITING THE SIGNATURE. The autograder is very picky about
these problems. If you skip the type of one parameter then the types of all
following parameters will probably be marked wrong. On the other hand an
incorrect type typically does not affect anything after it. So work very
carefully to first setup the number of parameters the function has, and be
sure your final answer has types for that many parameters. HINT, there are 7.

This problem will be autograded.  NOTE that all of the following are required.
Violating one or more will cause your solution to receive 0 marks.

  - Files must not have any errors when the Check Syntax button is pressed.
    Press Check Syntax and Run often, and correct any errors early.

  - You MUST NOT edit the provided fold-challenge function definition or
    the template tag.


NOTE TO PROBLEM WRITERS: We always provide the actual fold-function itself.
This solves problems about order of arguments.

|#




(define-syntax (grade-design-abstract-fold stx)
  (syntax-case stx (@template-origin)
    [(_ fn1 sig (#:copy-test d1 d2) (#:count-test d3 v3 d4 v4) (@template-origin . origins) (define (fn2 . args) . body))
     #'(recovery-point grade-design-abstract-fold
         (unless (eqv? 'fn1 'fn2)
           (error 'grade-design-abstract-fold "fn name and name in function definition are not the same"))
         (grade-design-abstract-fold* 'fn1 'args (cdr 'sig) 'd1 'd2 'd3 'd4 'v3 'v4 '(@template-origin . origins) '(define (fn2 . args) . body)))]))

(define (grade-design-abstract-fold* fn args sig d1 d2 d3 d4 v3 v4 totag defn)
  (let* ([htdf   `(@htdf ,fn)]
         [sigs   (htdf-sigs htdf)]
         [tests  (htdf-checks htdf)]
         [totag2 (and (pair? (htdf-template-origins htdf)) (car (htdf-template-origins htdf)))]
         [defn2  (and (htdf-defns htdf) (last (htdf-defns htdf)))])
    (check-design-abstract-fold     fn args sig d1 d2 d3 d4 v3 v4 totag defn sigs tests totag2 defn2)))
  
(define (check-design-abstract-fold fn args sig d1 d2 d3 d4 v3 v4 totag defn sigs tests totag2 defn2)
;  (let* ([htdf   (car (context))]
;         [sigs   (htdf-sigs htdf)]
;         [tests  (htdf-checks htdf)]
;         [totag2 (and (pair? (htdf-template-origins htdf)) (car (htdf-template-origins htdf)))]
;         [defn2  (and (htdf-defns htdf) (last (htdf-defns htdf)))])
    (weights (.50 *)
        
      (if (not (pair? sigs))
          (rubric-item 'signature #f "Abstract function signature")
          (check-signature-by-constraints (and sigs (car sigs))
                                          (make-constraints-from-signature sig)))
      
      ;; grade-prerequisite without excess indenting
      (if (not (equal? defn defn2))
          (score-it 'submitted-tests 1 0 #f "Copy test: incorrect (fold function has been edited).")
          (rubric-item 'submitted-tests
                       (with-handlers ([void (lambda (e) #f)])
                         (ormap (lambda (ce)
                                  (let ([lhs (cadr ce)]
                                        [rhs (caddr ce)])
                                    (and (calling-evaluator #f `(equal? ,rhs ,d1))
                                         (calling-evaluator #f `(equal? ,lhs ,d1))
                                         (calling-evaluator #f `(equal? ,(subst d2 d1 lhs) ,d2)))))
                                tests))
                       "Copy test"))

      (if (not (equal? defn defn2))
          (score-it 'submitted-tests 1 0 #f "Count test: incorrect (fold function has been edited).")
          (rubric-item 'submitted-tests                
                       (with-handlers ([void (lambda (e) #f)])
                         (ormap (lambda (ce)
                                  (let ([lhs (cadr ce)]
                                        [rhs (caddr ce)])
                                    (and (calling-evaluator #f `(equal? ,rhs ,v3))
                                         (calling-evaluator #f `(equal? ,lhs ,v3))
                                         (calling-evaluator #f `(equal? ,(subst d4 d3 lhs) ,v4)))))
                                tests))
                       "Count test")))) ;)

;; !!! this subst uses equal? not eqv? consider changing the one in utils and testing
(define (subst new old in)
  (cond [(equal? in old) new]
        [(cons? in) (cons (subst new old (car in)) (subst new old (cdr in)))]
        [else in]))
