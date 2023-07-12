#lang racket

(module+ test
  (require rackunit))

(require racket/list
         "walker.rkt"
         "grader.rkt"
         "utils.rkt")


(provide grade-use-bia-fn check-use-bia-fn)


#|

Standard directions are:

Complete the design of the function below by writing the template origin tag
and the function definition.  

<start optional>
The function consumes a list of integers and produces true if one of the 
integers is both positive and odd.

As a reminder, ISL includes a function called odd? which should be useful
in this problem.  If you are unsure about how it works, try a few examples
in the interactions window.

For maximum credit your function definition must have:

  - exactly two calls to foldr and two calls to map, OR
  - exactly two calls to foldr
<end optional>

NOTE: This problem will be autograded, and ALL OF THE FOLLOWING ARE ESSENTIAL
      IN YOUR SOLUTION.  Failure to follow these requirements may result in
      receiving zero marks for this problem.

 - The function you design MUST BE CALLED <NAME>.
 - You MUST NOT COMMENT out any @ metadata tags.
 - You MUST NOT EDIT the provided tests.
 - You MUST NOT EDIT any part of the file above the first line marked with ***.
 - You MUST NOT EDIT any part of the file below the second line marked with ***. (OPTIONAL)
 - You MUST FOLLOW all applicable design rules.
 - The file MUST NOT have any errors when the Check Syntax button is pressed.

 - The function definition MUST call one or more built-in abstract functions.

 - You must define a single top-level function with the given name. You are
   permitted to define helpers, but they must be defined within the the
   top-level function using local.

 - The function definition and any helper functions you design MUST NOT be
   recursive.

 - The result of the function must directly be the result of one of the
   built-in abstract functions. So, for example, the following would not
   be a valid function body:

       (define (foo x)
         (empty? (filter ...)))

   This would be a valid function body:

       (define (foo x)
         (local [(define (helper y) (foldr ... ... ...))]
           (helper ...)))



--------------

     (grade-use-bia-fn any-positive-odd?
       (@signature (listof Integer) -> Boolean)

       [(1 ormap _ _)    ;ormap solution gets 100%
        (.4 foldr _ _)]  ;foldr solution gets  40%
                         ;it wouldn't make sense, but (.8 (filter _ (map _ _))) would 
                         ;means a composition of filter and map gets 80%
       ;; these are the tests in the starter file
       ;; grade-use-bia-fn makes sure they are not edited
       #:supplied-tests
       (check-expect (any-positive-odd? empty) false)
       (check-expect (any-positive-odd? (list 1 3 2)) true)
       (check-expect (any-positive-odd? (list -11 -3 2)) false)
       ;; these are additional tests, they form the basis for
       ;; the correctness score
       #:additional-tests
       (check-expect (any-positive-odd? (list -11 -3 2 13 4)) true))

|#

(define-syntax (grade-use-bia-fn stx)
  (syntax-case stx ()
    [(_ n sig [(max-mark comp) ...] arg ...)
     #'(begin
         (let-values ([(supplied additional) (parse-kw-args `(arg ...))])
           (when (null? additional)
             (raise-argument-error 'grade-use-bia-fn "must supply at least one #:additional test" '()))
           (grade-htdf n
             (check-use-bia-fn `n `sig `((max-mark comp) ...) supplied additional))))]))

(define (parse-kw-args args0)
  ;; (@template-origin fsm list-as-stream)
  (let loop ([args args0]
             [supplied '()]
             [additional '()]
             [state 'init])
    (cond [(null? args) (values (reverse supplied) (reverse additional))]
          [(and (memq state '(init))
                (eqv? (car args) '#:supplied-tests))   (loop (cdr args) '() '() 'supplied)]
          [(and (memq state '(init supplied))
                (eqv? (car args) '#:additional-tests)) (loop (cdr args) supplied  '() 'additional)]
          [(eqv? state 'supplied)                      (loop (cdr args) (cons (car args) supplied) '() state)]
          [(eqv? state 'additional)                    (loop (cdr args) supplied (cons (car args) additional) state)]
          [else
           (error 'grade-use-bia-fn "bad syntax for args to grade-use-bia-fn")])))


(define (check-use-bia-fn fn-name sig max-marks-and-comps supplied additional)
  (assert-context--@htdf)
  (let* ([sexps (map elt-sexp (elts))]
         [htdf (car (context))]
         [defns (htdf-defns htdf)]
         [defn  (car defns)]
         [to-tags (htdf-template-origins htdf)]
         [to-tag  (and (pair? to-tags) (car to-tags))]
         [all-bia-fns (remove-duplicates   ;allow graders to specify abs-fns we don't know like map2
                       (append (apply append (map all-fns (map cadr max-marks-and-comps))) 
                               BIA-FNS))]
         ;; specially allow and/or to be used in the grade-use-bia-fn composition/combinations
         [called-bia-fns (special-calls (caddr defn) all-bia-fns)])

    (ensure (unchanged? (list sig) sexps) "must not edit signature in starter file")
    (ensure (unchanged? supplied sexps)   "must not edit tests in starter file")
    (ensure (pair? (caddr defn))          "fn definition must be more than stub")
    (ensure (= (length defns) 1)          "must define single top-level function")
    (ensure (not (recursive? defn))       "must not be recursive")
    (ensure (not (empty? called-bia-fns)) "must call one or more built-in abstract functions")
    
    (let* ([comp (infer-bia-fn-composition (datum->syntax #f defn) all-bia-fns)]
           [entry                     (lookup comp max-marks-and-comps)]
           [entry-is-best?            (and entry (equal? entry (first max-marks-and-comps)))]
           [ti-score                  (if entry (car entry) 0)]
           [ti-score-reduction-string (and entry (~r (* 100 (- 1 (car entry))) #:precision 1 #:notation 'positional))]
           [best-is-comp?             (not (equal? (cdadar max-marks-and-comps) '(_ _)))]
           [to-score
            (cond [(not (pair? to-tag)) 0]
                  [best-is-comp?
                   (+ (if (member 'fn-composition  to-tag) .5 0)
                      (if (member 'use-abstract-fn to-tag) .5 0))]
                  [else
                   (if (member 'use-abstract-fn to-tag) 1 0)])])

      (header (format "Use built-in abstract function -")
        (weights (10/100 45/100 45/100)
          (score-it 'template-origin 1 to-score #f
                    (format "@template-origin tag: ~a."
                            (cond [(= to-score 0) "incorrect"]
                                  [(= to-score 1)   "correct"]
                                  [else "partially correct"])))
          (score-it 'template-intact 1 ti-score #f
                    (format "choice of abstract function~a: ~a"
                            (if best-is-comp? "s" "")
                            (cond [(= ti-score 0) "incorrect"]
                                  [(= ti-score 1)   "correct"]
                                  [else "partially correct"])))
          (check-additional-tests fn-name additional))))))


(define (special-calls f0 all-bia-fns)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn-defn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(call)   (if (member (syntax->datum (car e)) all-bia-fns)
                               (cons (syntax->datum (car e)) (recur))
                               (recur))]
                 [(and or) (if (member kind all-bia-fns)
                               (cons kind (recur))
                               (recur))]
                 [(if cond define local local-define local-body lambda) (recur)]))))

(define (lookup comp max-marks-and-comps)
  (cond [(empty? max-marks-and-comps) #f]
        [else
         (if (match-abs-compositions? (cadar max-marks-and-comps) comp)
             (car max-marks-and-comps)
             (lookup comp (cdr max-marks-and-comps)))]))



(define (match-abs-compositions? sol sub)
  (cond [(and (null? sol) (null? sub)) #t]
        [(or  (null? sol) (null? sub)) #f]
        [(symbol? sol) (or (eqv? sol '_) (eqv? sol sub))]
        [(symbol? sub) #f]
        [else
         (and (match-abs-compositions? (car sol) (car sub))
              (match-abs-compositions? (cdr sol) (cdr sub)))]))

(define (all-fns comp)
  (cond [(eqv? comp '_) '()]
        [(symbol? comp) (list comp)]
        [else
         (foldr (lambda (x y)
                  (append (all-fns x) y))
                '()
                comp)]))
  


;;
;; preconditions:
;;  - no functions are recursive (we raise ensure violation)
;;  - no BIAF calls are guarded by if/cond (ensure violation???)
;;  - every call to a function has same type arguments, so once
;;    we type the result w/ one call we have it for all calls
;;
;;  --> the type of any tail expression is the type of the function
;;
;; depth-first traversal of tree, when we reach function calls,
;; if we have not already done an apply do it, then remember
;; the type of the function
;;
;;  bia-result-type is one of:
;;
;;  'any                        not a biaf result
;;  '(foldr any any)            foldr
;;  '(foldr any (map any any))  foldr o  map
;;  (map (foldr any any)        map fn produces foldr result        with foldr in the fn
;;       (build-list any any))  composed with build-list
;;  
;;

(define (infer-bia-fn-composition stx0 [bia-fns BIA-FNS])

  ;; the walker env binds from identifier to defining syntax
  ;; the shadow env binds from identifier to type

  ;; walker doesn't let walk-fn extend env (!!! maybe change this)
  ;; so we have to maintain a parallel environment to handle binding of fn
  ;; parameters on the first call to each function. These are "limited"
  ;; mutation tables, no key value changes and all keys are distinct syntax values
  ;; !!! these need to be parameters
  (define fn-types  (make-hasheq))
  (define var-types (make-hasheq))

  (define (p kind stx e ctx env in-fn-defn recur)
    (walker-case kind
      ;; simple things
      [(value constant null free) (list 'any)]
      
      [(if cond local) (list (last (recur)))]

      [(and or)
       (if (memq kind bia-fns)
           (list (compose-abs-fn-result kind (recur)))
           (list (last (recur))))]
      
      [(local-body) (recur)]
      
      [(lambda) (recur)] ; this should not produce the result of (recur)!!! but maybe we can exploit that for now
      
      [(define) (recur)]

      ;; building up from there
      [(bound) (list (hash-ref var-types (cdr (assq e env)) 'any))]
      
      [(local-define)
       (if (symbol? (syntax-e (cadr e)))
           (let ([typ (car (recur))])
             (hash-set! var-types stx typ)
             (list typ))
           (let* ([d (syntax->datum stx)]
                  [typ (car (walk-form (datum->syntax #f `(lambda ,(cadr d) ,(caddr d))) env p append))])
             (hash-set! var-types stx typ)
             (list typ)))]
      
      [(call)
       (let* ([called-fn-name (syntax->datum (car e))]
              [called-fn-defn
               ;; !!! also has to look in shadow env
               (and (assq called-fn-name env)
                    (cdr (assq called-fn-name env)))])
         (cond [(member called-fn-name bia-fns)
                (list (compose-abs-fn-result called-fn-name (cdr (recur))))]
               [(false? called-fn-defn) (list 'any)]
               ;; The first time we call a fn we check walk it with the arg types to compute
               ;; its result type. On any subsequent calls it has the same result type.
               [(hash-ref fn-types called-fn-defn #f) => identity]
               [else
                (let* ([param-stxs (cdr (syntax-e (cadr (syntax-e called-fn-defn))))]
                       [param-es (map syntax-e param-stxs)]
                       [body (caddr (syntax-e called-fn-defn))]
                       [arg-types (cdr (recur))])
                  (for ([stx param-stxs]
                        [typ arg-types])
                    (hash-set! var-types stx typ))
                  (let ([result-type 
                         (walk-form body
                                    (append (map cons param-es param-stxs) env)
                                    p)])
                    (hash-set! fn-types called-fn-defn result-type)
                    result-type))]))]))
  
  (car (walk-form stx0 '() p append)))

(define (compose-abs-fn-result biafn arg-types)
  (case biafn
    [(foldr foldl) `(,biafn ,(car arg-types) ,(caddr arg-types))] ;!!! is this really worth it?
    [else          `(,biafn ,@arg-types)]))


(module+ test

  (check-true (match-abs-compositions? '(foldr _ _) '(foldr any any)))

  (check-true (match-abs-compositions? '(foldr _ _)
                                       '(foldr any (filter any (map any any)))))

  (check-true (match-abs-compositions? '(foldr _ (_ _ (map _ _)))
                                       '(foldr any (filter any (map any any)))))
  
  (check-true (match-abs-compositions? '(map (foldr _ _) (build-list _ _))
                                       '(map (foldr any any) (build-list any any))))


  (check-true (match-abs-compositions? '(map (foldr _ _) _)
                                       '(map (foldr any any) (build-list any any))))

  (check-true (match-abs-compositions? '(map _ (filter _ _))
                                       '(map any (filter any any))))

  
  (check-true (match-abs-compositions?
               '(map (foldr _ _) _)
               (infer-bia-fn-composition #'(define (largest lolon)
                                             (map (lambda (lon)
                                                    (foldr max 0 lon))
                                                  lolon)))))


  (check-true (match-abs-compositions?
               '(map _ _)
               (infer-bia-fn-composition #'(define (largest lolon)
                                             (map (lambda (lon)
                                                    (foldr max 0 lon))
                                                  lolon)))))
  
  (check-true (match-abs-compositions?
               '(_ (foldr _ _) _)
               (infer-bia-fn-composition #'(define (largest lolon)
                                             (map (lambda (lon)
                                                    (foldr max 0 lon))
                                                  lolon)))))

  (check-false (match-abs-compositions?
               '(foldr _ _)
               (infer-bia-fn-composition #'(define (largest lolon)
                                             (map (lambda (lon)
                                                    (foldr max 0 lon))
                                                  lolon)))))


  
  (check-equal? (infer-bia-fn-composition #'(foldr + 0 empty))
                '(foldr any any))

  
  
  (check-equal? (infer-bia-fn-composition #'(foldr + 0 (map sqr empty)))
                '(foldr any (map any any)))
  
  (check-equal? (infer-bia-fn-composition #'(foldr + 0 (filter foo (map sqr empty))))
                '(foldr any (filter any (map any any))))
  
  (check-equal? (infer-bia-fn-composition #'(build-list (foldr  + 0 empty) sqr))
                '(build-list (foldr any any) any))
  
  
  (check-equal? (infer-bia-fn-composition #'(define (foo x) (foldr + 0 x)))
                '(foldr any any))
  
  (check-equal? (infer-bia-fn-composition #'(define (foo x) (foldr + 0 (map sqr x))))
                '(foldr any (map any any)))
  
  (check-equal? (infer-bia-fn-composition #'(define (foo x) (foldr + 0 (filter foo (map sqr x)))))
                '(foldr any (filter any (map any any))))
  
  
  (check-equal? (infer-bia-fn-composition #'(local [(@template-origin Number)
                                                    (define (foo x) x)]
                                              (foldr + 0 empty)))
                '(foldr any any))
  
  (check-equal? (infer-bia-fn-composition #'(local [(define (foo x) x)]
                                              (foldr + 0 (map sqr empty))))
                '(foldr any (map any any)))
  
  
  (check-equal? (infer-bia-fn-composition #'(local [(define (f2 l2) (map sqr l2))]
                                              (f2 empty)))
                '(map any any))
  
  
  (check-equal? (infer-bia-fn-composition #'(local [(define (f1 x) x)]
                                              (f1 (foldr + 0 empty))))
                '(foldr any any))
  
  
  
  (check-equal? (infer-bia-fn-composition #'(local [(define (f1 l1) (foldr + 0 l1))
                                                    (define (f2 l2) (map sqr l2))]
                                              (f1 (f2 empty))))
                '(foldr any (map any any)))
  
  (check-equal? (infer-bia-fn-composition #'(local [(define (f1 l1) (foldr + 0 l1))
                                                    (define (f2 l2) (map sqr l2))]
                                              (local ([define x (f2 empty)]
                                                      [define y (f1 x)])
                                                y)))
                '(foldr any (map any any)))
  
  
  (check-equal? (infer-bia-fn-composition #'(define (pyramid n i)
                                              (foldr above empty-image
                                                     (build-list (add1 n)
                                                                 (lambda (y)
                                                                   (foldr beside empty-image
                                                                          (build-list y
                                                                                      (lambda (x) i))))))))
                '(foldr any
                        (build-list any
                                    (foldr any (build-list any any)))))

  (check-equal? (infer-bia-fn-composition #'(define (titles-with->=rating num lob)
                                              (local [(define (select-book b)
                                                        (>= (book-rating b) num))
                                                      (define (name-book b)
                                                        (book-title b))]
                                                
                                                (map name-book (filter select-book lob)))))
                '(map any
                      (filter any
                              any)))

  (check-equal? (infer-bia-fn-composition #'(define (in-this-not-that x this that)
                                              (and (ormap (lambda (t) (= x t)) this)
                                                   (andmap (lambda (t) (not (= x t))) that)))
                                          (cons 'and BIA-FNS))
                '(and (ormap any any) (andmap any any)))

  
  (check-equal? (infer-bia-fn-composition #'(define (explode-pyramid s)
                                              (local [(define (get-1 n) (substring s n (add1 n)))
                                                      (define (row n) (build-list (add1 n) get-1))]
                                                (row (sub1 (string-length s))))))
                '(build-list any any))

  (check-equal? (infer-bia-fn-composition #'(define (explode-pyramid s)
                                              (local [(define (get-1 n) (substring s n (add1 n)))]
                                                (build-list (string-length s)
                                                            (lambda (n) (build-list (add1 n) get-1))))))
                '(build-list any (build-list any any)))

  (check-equal? (infer-bia-fn-composition #'(define (explode-pyramid s)
                                              (local [(define (get-1 n) (substring s n (add1 n)))
                                                      (define (row n) (build-list (add1 n) get-1))]
                                                (build-list (string-length s) row))))
                '(build-list any (build-list any any)))

  
  (check-equal? (infer-bia-fn-composition #'(define (explode-pyramid s)
                                              (local [(define (grow n) (build-list (add1 n) (λ (x) (string-ith s x))))]
                                                (build-list (string-length s) grow))))
                '(build-list any (build-list any any)))


  ) ;!!!
