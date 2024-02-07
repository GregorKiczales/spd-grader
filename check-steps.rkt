#lang racket

(require racket/exn
         spd-grader/grader
         spd-grader/utils)

(provide grade-steps check-steps steps step)

;;
;; Standard directions are:
;;

;;
;; Given the definition(s) below, write the step-by-step evaluation of
;; the expression that follows it(them).
;;
;; Be sure not to comment out the definitions, not to comment out or delete
;; the original expression, show every intermediate evaluation step, and the
;; final result.  The steps MUST NOT be in comments.
;;
;; Stepping questions like this one have ONLY ONE CORRECT ANSWER.  The goal
;; of this problem is to assess whether you have learned the exact BSL step
;; by step evaluation rules; not whether you can figure out the final result
;; of an expression.
;;

(define-syntax (grade-steps stx)
  (syntax-case stx ()
    [(_ sexp ...)
     #'(with-handlers ([void (lambda (e)
                               (rubric-item 'evaluation
                                            #f
                                            "Error grading problem ~s"
                                            (exn->string e)))])
         (assert-context--@problem)
         (check-steps (problem-sexps (car (context))) '(sexp ...)))]))

(define (check-steps sub sol-starter)
  (parameterize ([print-boolean-long-form #t])
    (let* ([sol-defs  (post-read-convert (filter defn? sol-starter))] ;*sl code in grader that we need
           [sol-expr  (post-read-convert (last sol-starter))]         ;to compare to code in handin
           [defs (eval-defs sol-defs)]
           [ndefs (length sol-defs)]
           
           [sol (print-convert (steps sol-expr defs))]) ;named like below

      (ensure (and (>= (length sub) ndefs)
                   (equal? (take sub ndefs) sol-defs))
              "Must not edit, comment out, or move definitions from starter.")
      (ensure (and (>= (length sub) (add1 ndefs))
                   (equal? (car (drop sub ndefs)) sol-expr))
              "Must not edit, comment out, or move first expression from starter.")
      
      ;; sub(mitted) is (listof expr)  student answer
      ;; sol(ution)  is (listof expr)  computed steps (resets)
      ;; possible    is possible marks (resets)
      ;; correct     is correct marks
      ;; skipped     is number skipped
      ;; extra       is number extra
      ;; msgs        is (listof Message) describing each step
      ;; n           is the step number in sub
      (let loop ([sub      (cdr (drop sub ndefs))] ;starting expr doesn't count
                 [sol      (cdr sol)]              ;
                 [possible (sub1 (length sol))]    ;
                 [correct  0]
                 [skipped  0]
                 [extra    0]
                 #;#;
                 [msgs     '()]
                 [n        1]
                 )
        (cond [(and (empty? sub) (empty? sol))
               (let ([%    (/ correct possible)]
                     [poss (pluralize possible "correct step")]
                     [corr (pluralize correct  "correct step")]
                     [skip (pluralize skipped  "skipped step")]
                     [extr (pluralize extra    "extra step")])
                 (cond [(= correct possible) (score-it 'eval-etc 1 % #f "Stepping problem: correct - ~a." corr)]
                       [(zero? extra)        (score-it 'eval-etc 1 % #f "Stepping problem: incorrect - ~a of ~a, ~a." correct poss skip)]
                       [(zero? skipped)      (score-it 'eval-etc 1 % #f "Stepping problem: incorrect - ~a of ~a, ~a." correct poss extr)]
                       [else                 (score-it 'eval-etc 1 % #f "Stepping problem: incorrect - ~a of ~a, ~a, and ~a." correct poss skip extr)]))]
              [(empty? sub)
               (loop '()
                     (cdr sol)
                     possible
                     correct
                     (add1 skipped)
                     extra
                     #;#;
                     (cons (message #f "skipped solution step:  ~s" (car sol)) msgs)
                     (add1 n))]
              [(empty? sol)
               (loop (cdr sub)
                     '()
                     possible
                     correct
                     skipped
                     (add1 extra)
                     #;#;
                     (cons (message #f "extra submission step:  ~s" (car sub)) msgs)
                     (add1 n))]
              
              [(equal? (car sub) (car sol))
               (loop (cdr sub)
                     (cdr sol)
                     possible
                     (add1 correct)
                     skipped
                     extra
                     #;#;
                     (cons (message #f "correct:  ~s" (car sol)) msgs)
                     (add1 n))]
              [(member (car sub) (cdr sol))
               ;; skipping to a later but correct step
               (loop sub
                     (member (car sub) (cdr sol)) ;(cdr sol)
                     possible
                     correct
                     (+ skipped (index-of sol (car sub)))
                     extra
                     #;#;
                     (cons (message #f "skipped solution step:  ~s" (car sol)) msgs)
                     n)]
              [else
               ;; jumping to a new incorrect step
               (let* ([new-steps    (print-convert (steps (car sub) defs))]
                      [new-possible (+ (- possible (length sol)) (length new-steps))])
                 (loop (cdr sub)
                       (cdr new-steps)
                       (max possible new-possible)
                       correct
                       skipped
                       extra
                       #;#;
                       (cons (message #f "jumped to incorrect new step:  ~s" (car sub)) msgs)
                       (add1 n)))])))))
    
    
    
;; produce list of step by step evaluation including expr
(define (steps expr defs)
  (let loop ([last void]
             [this expr])
    (if (equal? (print-convert last) (print-convert this));!!!???
        '()
        (cons this
              (loop this (step this defs))))))


(define (step expr defs)
  (cond [(pair? expr)
         (case (car expr)
           [(if)    (step-if  expr defs)]
           [(and)   (step-and expr defs)]
           [(or)    (step-or expr defs)]
           [(cond)  (step-cond expr defs)]
           [(local) (step-local expr defs)]
           [else
            (if (andmap value? (cdr expr))
                (step-call expr defs)
                (cons (car expr)
                      (step-next-rand (cdr expr) defs)))])]
        
        [(value? expr) expr]
        [(symbol? expr) (step-constant expr defs)]
        [else
         (raise-student-error "Can't step ~a" expr)]))

(define (step-if expr defs)
  (cond [(not (value? (cadr expr))) `(if ,(step (cadr expr) defs)
                                         ,(caddr expr)
                                         ,(cadddr expr))]
        [(true-value? (cadr expr))  (caddr expr)]
        [else                       (cadddr expr)]))

(define (step-and expr defs)
  (let/cc k
    (k (cons 'and
             (let loop ([rands (cdr expr)])
               (cond [(null? rands)              (k 'true)]
                     [(false-value? (car rands)) (k 'false)]
                     [(true-value?  (car rands)) (cons (car rands) (loop (cdr rands)))]
                     [else                       (cons (step (car rands) defs) (cdr rands))]))))))

(define (step-or expr defs)
  (let/cc k
    (k (cons 'or
             (let loop ([rands (cdr expr)])
               (cond [(null? rands)              (k 'false)]
                     [(true-value?  (car rands)) (k 'true)]
                     [(false-value? (car rands)) (cons (car rands) (loop (cdr rands)))]
                     [else                       (cons (step (car rands) defs) (cdr rands))]))))))

(define (step-cond expr defs)
  (let/cc k
    (k (cons 'cond
             (let loop ([rands (cdr expr)])
               (cond [(null? rands) (raise-student-error "No cond question evaluated to true.")]
                     [(or (true-value? (caar rands)) (eq? (caar rands) 'else)) (k (cadar rands))]
                     [(false-value?  (caar rands)) (cdr rands)]
                     [else (cons (cons (step (caar rands) defs) (cdar rands))
                                 (cdr rands))]))))))

(define (step-local expr top-level-defs)
  (error* "not yet implemented"))


;; precondition: all rands have been reduced to values
(define (step-call expr defs)
  (let ([def (assoc (car expr) defs)])
    (if def
        (foldr subst               ;!!! whoa this isn't doing lexical scoping!
               (caddr (cadr def))
               (cadr (cadr def))
               (cdr expr))
        ;(print-convert
         (calling-evaluator #t expr);)
         )))


(define (step-constant expr defs)
  (let ([def (assoc expr defs)])
    (if def
        (cadr def)
        (raise-student-error "No value for ~a" expr))))

(define (step-next-rand rands defs)
  (cond [(empty? rands) '()]
        [else
         (if (value? (car rands))
             (cons (car rands)
                   (step-next-rand (cdr rands) defs))
             (cons (step (car rands) defs)
                   (cdr rands)))]))
	
(define (step-through expr defs)
  (if (value? expr)
      expr
      (step-through (step expr defs) defs)))

(define (value? x) ;really is self-evaluating?
  (or (number? x)
      (string? x)
      (member x '(true false #t #f empty))
      (cons-list? x)
      (and (pair? x) (eqv? (car x) 'quote))
      (struct? x)
      (object? x))) ;all that's left is an image or not a value


(define (cons-list? x)
  (or (eqv? x 'empty)
      (and (pair? x)
           (or (and (eqv? (car x) 'cons)
                    (value? (cadr x))
                    (cons-list? (caddr x)))
               (and (eqv? (car x) 'list)
                    (andmap value? (cdr x)))))))

(define (true-value? x)  (member x '(#t true)))
(define (false-value? x) (member x '(#f false)))


(define (eval-defs defs)
  (foldl (lambda (def env)
           (append env
                   (if (pair? (cadr def))
                       (list (list (caadr def) (list 'lambda (cdadr def) (caddr def))))
                       (list (list (cadr  def) (step-through (caddr def) env))))))
         '()
         (filter (compose not struct-defn?) defs)))


  

(define (subst o n x)
  (cond [(pair? x) (cons (subst o n (car x)) (subst o n (cdr x)))]
        [(equal? x o) n]
        [else x]))
