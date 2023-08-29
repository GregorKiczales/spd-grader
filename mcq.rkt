#lang racket

(require racket/list
         "walker.rkt"
         "grader.rkt"
         "utils.rkt")

(provide grade-mcq check-mcq)

#|

Standard direction are:

This is a multiple choice problem on reference arrows.  It asks you to label
reference arrows and answer a question on whether the self/mutual-reference
is well-formed.


NOTE: This problem will be autograded, and ALL OF THE FOLLOWING ARE ESSENTIAL
      IN YOUR SOLUTION.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - You MUST NOT EDIT the type comments.
 
 - You MUST NOT edit the code at the end of the file marked do not edit.
 
 - This file includes special check-expects at the bottom that will check
   whether your answer is well-formed when you run the file. If one or more
   answers are not well-formed the test will fail with a message that
   describes what needs to be fixed.
 
 - Run the file EVERY time you edit an answer.
 
 - Note that initially the tests WILL FAIL, because you need to edit your
   answers in for the tests to pass.
 
 - Your submission WILL BE GRADED IF THERE ARE FAILING TESTS.  A failing test
   just means that specific answer is not well-formed.
 
 - BUT your submission will not be graded if running it produces red errors.

Consider the following type comments.  You will notice that every reference to
a type name is followed by a letter in square brackets, e.g. [A], [B], etc. That
label exists to give a name to the reference arrow that starts at that point.  
So, the arrow from Bar in the first line of the type comments to Bar in the
third line of the type comments is called A.

In this problem you must do two things:

  - for each arrow indicate whether it is a reference, self-reference or
    mutual reference

  - indicate whether the self and mutual reference relationships
    in these types are all well-formed




(define-struct foo (a b))
(define-struct baz (a b c))
(define-struct bee (a b))
(define-struct bix (a b))
;;
;; Foo is (make-foo Bar[A] Foo[B] Bix[C])
;;
;; Bar is one of:
;;  - false
;;  - (cons Baz[D] Bar[E])
;;
;; Baz is - (make-baz Number String Bee[F])
;;
;; Bee is one of:
;;  - false
;;  - (make-bee Number Foo[G])
;;
;; Bix is (make-bix String Number)
;;


 To answer this question, please edit each of the following constant
 definitions correctly. Remember that your answer MUST conform to the
 labeled arrows.


(define ARROW-A "???") ;replace ??? with one of R, SR or MR
(define ARROW-B "???") ;replace ??? with one of R, SR or MR
(define ARROW-C "???") ;replace ??? with one of R, SR or MR
(define ARROW-D "???") ;replace ??? with one of R, SR or MR
(define ARROW-E "???") ;replace ??? with one of R, SR or MR
(define ARROW-F "???") ;replace ??? with one of R, SR or MR
(define ARROW-G "???") ;replace ??? with one of R, SR or MR

;; Now answer the following question by editing the PART-2 constant:
;;
;; Are all the self-reference and mutual-reference relationships in these
;; type comments well-formed?

(define PART-2 "???") ;replace ??? with yes or no

|#

(define-syntax (grade-mcq stx)
  (syntax-case stx ()
    [(_ case ...)
     (let-values ([(weights cases) (parse-mcq-cases stx #'(case ...))])
       #`(check-mcq '#,weights '#,cases))]))

(define-for-syntax (parse-mcq-cases stx raw)
  (let loop ([stxs (syntax-e raw)]
             [first? #t]
             [seen-weight? #f]
             [weights '()]
             [cases '()])
    (if (null? stxs)
        (values (reverse weights) (reverse cases))
        (let ([d (syntax->datum (car stxs))])
          (if (weight-and-check? d)
              (if (or first? seen-weight?)
                  (loop (cdr stxs) #f #t (cons (car d) weights) (cons (cadr d) cases))
                  (raise-syntax-error #f "Expecting check without weight." stx (car stxs)))
              (if (or first? (not seen-weight?))
                  (loop (cdr stxs) #f #f '() (cons d cases))
                  (raise-syntax-error #f "Expecting check with weight." stx (car stxs))))))))

(define-for-syntax (weight-and-check? d)
  (and (= (length d) 2)
       (or (number? (car d))
           (eqv? (car d) '*))))
  

(define (check-mcq weights cases)
  (let ([subs (map (lambda (c)
                     (list (cadr c)
                           (calling-evaluator #f (cadr c))))
                   cases)])
    
    (define (check-equal case)
      (let* ([var  (cadr case)]
             [sub  (cadr (assq var subs))]
             [sols (cddr case)]
             [correct? (member sub sols)])
        (produce-score correct? var sols #f sub)))
       
    (define (check-exclusive case)
      (let* ([var  (cadr case)]
             [sub  (cadr (assq var subs))]
             [sols (cddr case)]
             
             [exclude-subs
              (for/list ([c cases]
                         [s subs])
                (if (and (eqv? (car c) 'exclusive?)
                         (not (eqv? (cadr c) var)))
                    s
                    (void)))]
             [correct?
              (and (member sub sols)
                   (not (member sub exclude-subs)))])
        (produce-score correct? var sols #t sub)))

    (define (produce-score correct? var sols exclusive? sub)
      (score #f 'other 1 (if correct? 1 0) '()
             (list (if correct?
                       (message #f "~a: correct."         (answer-string var sols exclusive?))
                       (message #f "~a: ~s is incorrect." (answer-string var sols exclusive?) sub)))))
    
    (define (answer-string var sols exclusive?)      
      (format "~a should be ~a~a"
              var
              (if (pair? (cdr sols))
                  (format "one of ~s" sols)
                  (format "~s" (car sols)))
              (if exclusive?
                  " and only used once"
                  "")))
      
    (combine-scores
     (weights* 1.0
               (if (null? weights)
                   (make-list (length cases) '*)
                   weights)

       (for/list ([case cases])
         (match (car case)
           ('equal?     (check-equal     case))
           ('exclusive? (check-exclusive case))
           ('yes?       (check-equal     `(equal? ,(cadr case) "yes")))
           ('no?        (check-equal     `(equal? ,(cadr case) "no")))))))))
                

