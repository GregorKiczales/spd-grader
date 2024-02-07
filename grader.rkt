#lang racket

(require racket/bool
         racket/function
         racket/list
         racket/sequence
         racket/string
         racket/exn
         "defs.rkt"
         "reloadable.rkt"
         "sandbox.rkt"
         "file-structure.rkt"
         "score.rkt"
         "utils.rkt"
         "walker.rkt")
 
(provide (all-defined-out)
         (except-out (all-from-out "defs.rkt") 
                     CURRENT-TERM
                     GRADERS-DIR)
         (all-from-out "file-structure.rkt")
         (all-from-out "score.rkt"))


(define INTERNAL-DATA-LINE "Internal use only below here:")

(define LINE-LENGTH-LIMIT 80)

(define-for-syntax TOPICS '(other
                            eval-etc
                            signature
                            test-validity test-thoroughness 
                            template-origin
                            template
                            template-intact
                            submitted-tests additional-tests))

(define            TOPICS '(other
                            eval-etc
                            starter-intact
                            signature
                            test-validity test-thoroughness 
                            template-origin
                            template
                            template-intact
                            submitted-tests additional-tests))

(define EARLY-REPORT-TOPICS '(eval-etc starter-intact signature test-validity test-thoroughness))

(define EVAL-LIMITS '(15 128)) ; 15 seconds shallow time, 128 MB  !!!


;; Error handling strategy:  THIS IS OUT OF DATE!!!
;;
;; By the time the grader code starts running the code has passed checked-syntax and run. So it is well formed,
;; and the only runtime errors occur inside of check-expects. This means:
;; 
;;  - The grader can assume code is well formed, syntacticly and in terms of bindings. This
;;    greatly simplifies the picking about of code into different forms and expressions.
;;
;;    --> Any errors in grader code that syntactically analyzes the submission are errors in
;;        the grader itself.  These are handled on a per-problem basis by grade-problem, with
;;        the problem producing a mark of 1, but logging the issue so we can spot it easily.
;;
;;  - BUT this is not true for all aspects of tag structure. In particular problem and htdf tags
;;    may be missing. function definitions may be misnamed etc.
;;
;;    --> These kinds of errors are represented by exn:fail:student-error and are handled on
;;        a per problem basis in grade-problem. grade-submission also handles them.
;; 
;;  - The grader cannot assume valid tests will run without errors.
;;
;;    --> Any errors in evaluation of teaching language code should be treated as errors in the
;;        student code.  They might not be, if for example we inject faulty code. But any errors
;;        in how the grader constructs such code are likely to be found during testing, or fail
;;        on a large enough number of submissions that we will notice them. These are represented
;;        by exn:fail:eval-error, and handled in grade-problem, with that problem producing a
;;        mark of 0. These are quite common, so they are not logged. They will be easy to find
;;        in grading reports though.
;;
;; One additional kind of error exists to allow writing (ensure (test x) "...") inside of graders.
;; These are represented as exn:fail:ensure-violation and are handled in grade-problem.
;;

(define-syntax (per-problem-error-handling stx) ;used in grade-problem 
  (syntax-case stx ()
    [(_ n exp ...)
     #'(with-handlers ([exn:fail?
                        (lambda (e)
                          (cond [(exn:fail:student-error? e)
                                 (weights (*)
                                   (score-it 'other 1 0 #f (exn->string e)))]
                                [(exn:fail:ensure-violation? e)
                                 (weights (*)
                                   (score-it 'other 1 0 #f "Grading of problem halted - ~a." (exn->string e)))]
                                [(exn:fail:eval-error? e)
                                 (weights (*)
                                   (if (exn:fail:eval-error-student? e)
                                       (score-it 'other 1 0 #f "Error evaluating submitted code - ~a" (exn->string e))
                                       (score-it 'other 1 0 #f "Error evaluating grader code that calls submitted code - ~a" (exn->string e))))]
                                [else
                                 ((logger) (format "Internal grader system error ~a: ~a" n (exn->string e)))
                                 (weights (*) (score-it 'other 1 1 #f "Internal grader system error, student given 100% on this problem. This does not necessarily mean the submission is correct. Grade may change when grader system error is fixed."))]))])
         exp ...)]))

(define-syntax (recovery-point stx)
  (syntax-case stx ()
    [(_ w item ...)            ;!!! why is there no with-handlers here???
     #'(begin item ...)]))

(define-syntax (calling-evaluator stx)
  (syntax-case stx ()
    [(_ student? exp)
     #'(with-handlers ([exn:fail?
                        (lambda (e)
                          (raise (exn:fail:eval-error (exn->string e) (current-continuation-marks) student?)))])
         ((evaluator) exp))]))




(define (ensure tst fmt-ctl . fmt-args)
  (when (not tst)
    (raise (exn:fail:ensure-violation (apply format fmt-ctl fmt-args) (current-continuation-marks)))))

(define (ensure-parameter-names names)
  (assert-context--@htdf)
  (let* ([htdf  (car (context))]
         [defns (htdf-defns htdf)]
         [defn  (and (pair? defns) (car defns))]
         [fn-name (and defn (caadr defn))]
         [params  (and defn (cdadr defn))])
    (ensure (equal? params names)
            "Parameter names for ~a must be: ~a. Edit function design and resubmit."
            fn-name
            names)))

(define (ensure-unchanged sol-sexps [fmt-ctl #f] . fmt-args)
  (let ([sub-sexps (if (null? (context))
                       (map elt-sexp (elts))
                       (tag-sexps (car (context))))]
        [msg
         (if fmt-ctl
             (apply format fmt-ctl fmt-args)
             (let loop ([sol-sexps sol-sexps]
                        [sig? #f]
                        [ces? #f]
                        [tos? #f]
                        [others? #f])
               (if (null? sol-sexps)
                   (cond [others?              "must not edit elements of starter file that instructions say not to change"]
                         [(and sig? ces? tos?) "must not edit signature, tests or template-origin in starter file"]
                         [(and sig? ces?)      "must not edit signature or tests in starter file"]
                         [sig?                 "must not edit signature in starter file"]
                         [ces?                 "must not edit tests in starter-file"]
                         [tos?                 "must not edit template-origins in starter-file"])
                   (cond [(@signature?       (car sol-sexps)) (loop (rest sol-sexps) #t   ces? tos? others?)]
                         [(check?            (car sol-sexps)) (loop (rest sol-sexps) sig? #t   tos? others?)]
                         [(@template-origin? (car sol-sexps)) (loop (rest sol-sexps) sig? #t   #t   others?)]
                         [else                                (loop (rest sol-sexps) sig? ces? tos? #t)]))))])
    
    (ensure (unchanged? sol-sexps sub-sexps) msg)))

;; true if every element of sol appears in original order in sub
;; additions anywhere in sub are ok
(define (unchanged? sol sub) ;;!!! make consistent w/ other fns
  (let loop ([sol sol]
             [sub sub])
    (cond [(empty? sol) #t]
          [(empty? sub) #f]
          [else
           (if (equal? (post-read-convert (car sol)) (car sub))
               (loop   (cdr sol) (cdr sub))
               (loop        sol  (cdr sub)))])))


(define (autograde-file filename [verb? #t] [earl? #f] [rpt (current-output-port)] [logr displayln])
  (let ([grader
         (with-handlers ([exn:fail? (lambda (exn)
                                      (logr (format "Error: problem loading grader for ~a - ~a" filename (exn->string exn))))])
           (find-grader filename))])
    (autograde-file-with-grader filename grader verb? earl? rpt logr)))

(define (autograde-file-with-grader filename grader [verb? #t] [earl? #f] [rpt (current-output-port)] [logr displayln])               
  ;; until we get going any error is a grader framework error and we just log it
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (logr (format "Error: framework error for submission ~a - ~a"
                                   filename
                                   (exn->string exn))))])
      (call-with-input-file* filename
	(lambda (in)
          (read-line in)
          (read-line in)
          (let ([lang (line->language-level (read-line in))])
            (parameterize ([sandbox-input                #f]
                           [sandbox-output               #f]
                           [sandbox-error-output         #f]
                           [sandbox-propagate-exceptions #t]
                           [error-value->string-handler (lambda (v s)
                                                          ((current-value-printer) v))]
                          ;[list-abbreviation-enabled
                          ; (not (or (equal? lang '(special beginner))         ;!!! doesn't work because
                          ;          (equal? lang '(special beginner-abbr))))] ;!!! line->language-level overrides w/ intermediate
                           [sandbox-eval-limits         EVAL-LIMITS])

              (let ([abort-tag (make-continuation-prompt-tag)])
                
                (define (handle-resource-error exn)
                  (when (verbose-error-logging?)
                    (logr (format "Error: running submission ~a used too much ~a." filename (exn:fail:resource-resource exn))))
                  (display-overall-grade 0
                                         (format "Submission used too much ~a" (exn:fail:resource-resource exn))
                                         rpt)
                  (abort-current-continuation abort-tag (lambda () (void))))

                (define (handle-submission-error exn)
                  (when (verbose-error-logging?)
                    (logr (format "Error: running submission ~a - ~a" filename (exn->string exn))))
                  ;; deliberately don't display error so students debug themselves
                  ;; rather than just try to debug from autograder reports
                  (display-overall-grade 0
                                         (format "Error running submission ~a." (exn->string exn))
                                         rpt)
                  (abort-current-continuation abort-tag (lambda () (void))))

                (define (handle-framework-error exn)
                  ((logger) (format "Error: Framework error for submission ~a - ~a"
                                    filename
                                    (exn->string exn)))
                  (display-overall-grade 100
                                         (format "Autograder framework error. ~a"
                                                 (exn->string exn))
                                         rpt)
                  (abort-current-continuation abort-tag (lambda () (void)))) ;!!! added 1/31/24
                
                (call-with-continuation-prompt 
                 (lambda ()
                   (let ([e
                          ;; this region is errors when originally running the submission
                          (with-handlers ([exn:fail:resource? handle-resource-error]
                                          [exn:fail? handle-submission-error])                            
                           (make-evaluator lang
                                           in
                                           #:requires '(spd-grader/tonka) ;grader runtime for playing in sandbox '(spd/tags)
                                           
                                           #:allow-for-load (list
                                                             ;; hard-coded for our server
                                                             "/etc/ssl/certs/ca-certificates.crt"
                                                             "/etc/ssl/cert.pem"
                                                             "/home/c/cs-110/.racket/racket-prefs.rktd"
                                                             ;; try to do the right thing for local testing
                                                             (find-system-path 'pref-file)
                                                             (let-values ([(base name is-dir) (split-path (find-system-path 'pref-file))])
                                                               (build-path base "_LOCKracket-prefs.rktd")))
                                           #:allow-read  (list "/etc/ssl/cert.pem"
                                                               "/usr/lib/ssl/cert.pem"
                                                               "/usr/lib/ssl/certs")))])
                     ;; from here till we get inside grade-submission any errors are framework errors
                     (with-handlers ([exn:fail? handle-framework-error ])
                       (let ([s
                              (parameterize ([evaluator 
                                              (lambda (x)
                                                ;; these are errors when the grader makes additional calls to the evaluator
                                                ;; we abort on resource errors and let others through
                                                (with-handlers ([exn:fail:resource? handle-resource-error])
                                                  (e (if (string? x) `(identity ,x) x))))]
                                             [logger    logr]
                                             [context   '()]
                                             [elts      #f])
                                (elts (fn->elts filename))
                                (grader))])

                         ;; we now have a score, time to render it
                      (parameterize ([verbose? verb?]
                                     [early? earl?])
                        (cond [earl?
                               (displayln/f "\n\nAssignment submitted for regrading before end of cooldown - only signature, test
validity, and test thoroughness results are reported. No grade information is reported.\n\n" rpt)
                               (display-score s rpt #f)
                               (score-m s)]
                              [else
                               (display-overall-grade (inexact->exact (round (* 100 (score-m s)))) "" rpt)
			       (displayln/f "\n\n" rpt)
                               (display-score s rpt #t)
                               (score-m s)]))))))
                 abort-tag)))))
        
	#:mode 'text)))



(define (default-grader)
  (grade-submission 
    (weights (1)
      (rubric-item 'other #t "No custom grader exists for this starter."))))


;; grade-* syntax

(define-syntax (grade-submission stx)
  (syntax-case stx ()
    [(_ item ...)
     #`(per-problem-error-handling "prior to (@problem 1)"
         (send-definitions '#,(filter stx-define? (syntax->list #'(item ...))))
         (header "Overall submission: "
                 (weights (*) #,@(filter stx-not-define? (syntax->list #'(item ...))))))]))


(define (send-definitions defines)
  (let [(names (map (lambda (def)
                      (if (list? (cadr def))
                          (caadr def)
                          (cadr def)))
                    defines))]
    (calling-evaluator #f `(define %%fns (local ,defines (list ,@names))))
    (for ([name names]
          [i (in-naturals 0)])
      (calling-evaluator #f `(define ,name (list-ref %%fns ,i))))))


(define-for-syntax (stx-define? stx)
  (syntax-case stx (define define-struct)
    [(define (id ...) body) #t]
    [(define id body) #t]
    [(define-struct id . args) #t]
    [_ #f]))

(define-for-syntax (stx-not-define? stx)
  (syntax-case stx (define define-struct)
    [(define (id ...) body) #f]
    [(define id body) #f]
    [(define-struct id . args) #f]
    [_ #t]))



(define-syntax (grade-problem stx)
  (syntax-case stx ()
    [(_ n item ...)
     #'(begin
         (assert-context--top-level)
         (header (format "~a: " `(@problem ,n))
                 (per-problem-error-handling `(@problem ,n)
                   (parameterize ([context (cons (get-problem* n) (context))])
                     (weights (*) item ...)))))]))


(define (not-graded)
  (score-it 'other 0 0 #f "This item is not graded."))


(define-syntax (grade-sexps stx)
  (syntax-case stx ()
    [(_ desc sexps)
     #'(check-sexps desc (problem-sexps (car (context))) `sexps)]))


(define (check-sexps desc sub sol)
  (rubric-item 'other (equal? sub sol) desc))



(define-syntax (grade-prerequisite stx)
  (syntax-case stx ()
    [(_ type str test item ...)
     #'(if (not test)
           (rubric-item 'type #f (format "Prerequisite: ~a" str))
           (weights (*) item ...))]))
  


(define-syntax (grade-htdd stx)
  (syntax-case stx ()
    [(_ n item ...)
     #'(recovery-point grade-htdd
         (assert-context--@problem)
         (parameterize ([context (cons (get-htdd* 'n) (context))])
           (header (format "~a: " (car (context)))
                   (weights (*) item ...))))]))

(define-syntax (grade-htdf stx)
  (syntax-case stx ()
    [(_ n item ...)
     (if (identifier? #'n)
         #'(begin ;recovery-point n !!!
             (assert-context--@problem)
             (parameterize ([context (cons (get-htdf* `n) (context))])
               (header (format "~a: " (car (context)))
                       (let* ([defns (htdf-defns (car (context)))]
                              [n (and (pair? defns) (car defns))])
                         (weights (*) item ...)))))
         
         #'(begin ;recovery-point n !!!
             (assert-context--@problem)
             (parameterize ([context (cons (get-htdf* `n) (context))])
               (header (format "~a: " (car (context)))
                 (weights (*) item ...)))))]))

(define-syntax (grade-bb-handler stx)
  (syntax-case stx ()
    [(_ (option fn) item ...)
     #'(begin
         (let ([fn (find-bb-handler 'option)])
           (grade-htdf ,fn item ...)))]))

(define (find-bb-handler option)
  (let* ([htdf '(@htdf main)]
         [defns (htdf-defns htdf)]
         [defn  (and (pair? defns) (car defns))]
         [alist (and defn (cddr (caddr defn)))]

         [entry (and alist
                     (if (memq option '(on-draw to-draw))
                         (or (assq 'on-draw alist)
                             (assq 'to-draw alist))
                         (assq option alist)))])

    (if entry
        (cadr entry)
        (rubric-item "big-bang ~a handler - could not find ~a option to big-bang" option option))))


  
(define-syntax (grade-signature stx)
  (syntax-case stx ()
    [(_   sol) #'(grade-signature-1 1 `sol)]
    [(_ n sol) #'(grade-signature-1 n `sol)]
    [(_ n sol sol2 ...) #'(score-max (grade-signature-1 n `sol) 
                                     (grade-signature-1 n `sol2)
                                     ...)]))

(define (grade-signature-1 n sol)
  (recovery-point grade-signature
    (assert-context--@htdf)
    (let* ([htdf (car (context))]
           [sigs (htdf-sigs htdf)])
      (if (< (length sigs) n)
          (score-it 'signature 1 0 #f "Signature: not found.")
          (check-signature (subst 'false #f (list-ref sigs (sub1 n)))
                           (subst 'false #f (cons '@signature sol)))))))






;; !!!! move to tests.rkt

(define-syntax (grade-submitted-tests stx)
  (syntax-case stx ()
    [(_)       #'(grade-submitted-tests*  1 0)]
    [(_   min) #'(grade-submitted-tests*  1 `min)]
    [(_ n min) #'(grade-submitted-tests* `n `min)]))

(define (grade-submitted-tests* n min)
  (assert-context--@htdf)
  ;(check-bounds n (length (htdf-names (car (context)))) "function definition")
  (let* ([htdf    (car (context))]
         [fn-name (if (symbol? n) n (list-ref (htdf-names htdf) (sub1 n)))]
         [defns   (htdf-defns htdf)]
         [defn    (and (pair? defns) (car defns))]
         [body    (and defn (caddr defn))]
         [stub?   (or (not (list? body))
                      (equal? body 'empty)
                      (equal? body '()))]
         [tests (get-function-tests fn-name)])
    (grade-prerequisite 'submitted-tests "submitted-tests: function definition must be more than a stub" (not stub?)
      (check-submitted-tests fn-name tests))))
                                   

(define-syntax (grade-additional-tests stx)
  (syntax-case stx ()
    [(_ n test ...)
     #'(grade-additional-tests* `n `(test ...))]))

(define (grade-additional-tests* n tests)
  (let* ([htdf    (car (context))]
         [fn-name (if (symbol? n) n (list-ref (htdf-names htdf) (sub1 n)))]
         [defns   (htdf-defns htdf)]
         [defn    (and (pair? defns) (car defns))]
         [body    (and defn (caddr defn))]
         [stub?   (or (not (list? body))
                      (equal? body 'empty)
                      (equal? body '()))])
    (grade-prerequisite 'additional-tests "additional-tests: function definition must be more than a stub" (not stub?)
      (check-additional-tests fn-name tests))))


(define-syntax (grade-tests-validity stx)
  (syntax-case stx ()
    [(_ (p ...) r check ...)
     #'(grade-tests-validity* (list `p ... `r) `(check ...))]))

(define (grade-tests-validity* params checks)
  (assert-context--@htdf)
  (let* ([htdf    (car (context))]
         [fn-name (list-ref (htdf-names htdf) 0)]
         [tests   (get-function-tests fn-name)])
    (check-validity fn-name tests params checks)))


(define-syntax (grade-argument-thoroughness stx)
  (syntax-case stx (all-args per-args)
    [(_ lop
        (all-args (aaparam)     aacheck ...)
        (per-args (paparam ...) pacheck ...))
     #'(grade-argument-thoroughness* `lop `aaparam (list `aacheck ...) (list `paparam ...) (list `pacheck ...))]
    
    [(_ lop
        (per-args (paparam ...) pacheck ...)
        (all-args (aaparam)     aacheck ...))
     #'(grade-argument-thoroughness* `lop `aaparam (list `aacheck ...) (list `paparam ...) (list `pacheck ...))]
    
    [(_ lop (all-args (aaparam) aacheck ...))
     #'(grade-argument-thoroughness lop (all-args (aaparam) aacheck ...) (per-args (_)            ))]
    [(_ lop (per-args (paparam ...) pacheck ...))
     #'(grade-argument-thoroughness lop (all-args (_)                  ) (per-args (paparam ...) pacheck ...))]
    [(_ lop)
     #'(grade-argument-thoroughness lop (all-args (_)                  ) (per-args (_)            ))]))

(define (grade-argument-thoroughness* lop aaparam aachecks paparams pachecks)
  (assert-context--@htdf)
  (let* ([htdf      (car (context))]
         [fn-name   (car (htdf-names htdf))]
         [tests     (get-function-tests fn-name)])
    (check-argument-thoroughness fn-name tests lop aaparam aachecks paparams pachecks)))

                                      
;deprecated
(define-syntax (grade-tests-argument-thoroughness stx)
  (syntax-case stx ()
    [(_ (p ...) check ...)
     #'(grade-argument-thoroughness () (per-args (p ...) check ...))]))


(define-syntax (grade-thoroughness-by-faulty-functions stx)
  (syntax-case stx ()
    [(_ n defn ...) #'(grade-thoroughness-by-faulty-functions* `n (list `defn ...))]))

(define (grade-thoroughness-by-faulty-functions* n defns)
  (assert-context--@htdf)
  (let* ([htdf      (car (context))]
         [fn-name   (car (htdf-names htdf))]
         [tests     (get-function-tests fn-name)])
    (check-faulty-functions fn-name tests defns)))


(define (check-submitted-tests  fn-name tests) (check-tests fn-name tests 'submitted-tests  "Submitted"  "submitted"))
(define (check-additional-tests fn-name tests) (check-tests fn-name tests 'additional-tests "Additional" "autograder supplied additional"))

(define (check-tests fn-name tests topic Camel lower)
  (cond [(< (length tests) 2) (score-it topic 1 0 #f "~a tests: incorrect - at least 2 tests are required." Camel)]
        [else
         (let* ([names (map (lambda (x) (gensym)) tests)]
                [results
                 (calling-evaluator #f    ;#f because %%call-thunks-with-handler
                                    ;;    ;wraps actual student code evaluation
                   `(%%call-thunks-with-handler
                     (local ,(for/list ([name names]
                                        [test tests])
                               `(define (,name _) ,(xform-test test)))
                       (list ,@names))))]
                

                [ntests (length tests)]
                [npass  (count (lambda (x) (eqv? x #t)) results)]
                [nfail  (count (lambda (x) (eqv? x #f)) results)]
                [nerror (count (lambda (x) (eqv? x 'error)) results)]
                
                [%      (/ (- npass nerror) ntests)])
           
           (cond [(= npass ntests)   (score-it topic 1 1 #f "~a tests: correct." Camel)]
                 [(= nfail ntests)   (score-it topic 1 0 #f "~a tests: incorrect - all ~a tests failed." Camel lower)]
                 [(=       nerror 0) (score-it topic 1 % #f "~a tests: incorrect - ~a failed." Camel
                                               (pluralize nfail (format "~a test" lower)))]
                 [else               (score-it topic 1 % #f "~a tests: incorrect - ~a failed and ~a caused an error." Camel
                                               (pluralize (- ntests npass nerror) (format "~a test" lower))
                                               (pluralize nerror (format "~a test" lower)))]))]))


(define (check-validity fn-name tests params checks)
  (cond [(< (length tests) 2) (score-it 'test-validity 1 0 #f "Test validity (matches problem statement): incorrect - at least 2 tests are required.")]
        [else 
         (let* ([tests         (filter not-check-satisfied? tests)]
                [test-names    (map (lambda (x) (gensym "test")) tests)]
                [criteria-names (map (lambda (x) (gensym "cond"))  checks)]         
                  
                [results
                 (calling-evaluator #f
                  `(local [,@(for/list ([c    tests]
                                        [name test-names])
                               `(define (,name _)
                                  (list ,(subst 'list fn-name (cadr c)) ;...trying to make local work with 1985 hygiene !!! do it right w/ walker
                                        ,(caddr c))))

                           (define (%%all-checks ,@params)
                             (and ,@checks))]
                     
                     (%%check-validity (list ,@test-names) %%all-checks)))]

                [ntests (length tests)]
                [npass  (count (lambda (x) (eqv? x #t)) results)]
                [nfail  (count (lambda (x) (eqv? x #f)) results)]
                [nerror (count (lambda (x) (eqv? x 'error)) results)]

                [% (/ npass ntests)])

             (cond [(= % 1)        (score-it 'test-validity 1 1 #f "Test validity (matches problem statement): correct.")]
                   [(zero? nerror) (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests is invalid." nfail ntests)]
                   [(zero? nfail)  (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests caused an error." nerror ntests)]
                   [else           (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests is invalid, and ~a caused an error." nfail ntests (pluralize nerror "test"))]))]))

(define (check-argument-thoroughness fn-name tests lop aa-param aa-checks pa-params pa-checks)
  (cond [(< (length tests) 2) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (test argument coverage): incorrect - at least 2 tests are required.")]
        [else
         (let* ([tests (filter not-check-satisfied? tests)]
                [lo-args-and-result  (get-lo-args-and-result fn-name tests)]
                [lo-args  (map car (filter pair? lo-args-and-result))]
                 
                [aa-check-names (map (lambda (x) (gensym)) aa-checks)]
                [pa-check-names (map (lambda (x) (gensym)) pa-checks)]

                [equal-positions (if (null? lo-args) ;all the tests might have errored
                                     '()
                                     (filter (lambda (p) (not (member p lop))) (test-args-equal-positions lo-args)))])

           (grade-prerequisite 'test-thoroughness
               "A set of tests must not have the same argument for any given parameter"
               (null? equal-positions)               
             (let* ([passes
                     (calling-evaluator #f
                       `(%%check-argument-thoroughness ',lo-args
                                                       ',aa-check-names
                                                       ',pa-check-names                                                             
                                                       (local ,(for/list ([name aa-check-names]
                                                                          [check aa-checks])
                                                                 `(define (,name ,aa-param) ,check))
                                                         (list ,@aa-check-names))
                                                       (local ,(for/list ([name pa-check-names]
                                                                          [check pa-checks])
                                                                 `(define (,name ,@pa-params) ,check))
                                                         (list ,@pa-check-names))))]
                    
                    [nchecks (+ (length aa-check-names) (length pa-check-names))]
                    [nerror  (count (lambda (x) (eqv? x 'error)) lo-args-and-result)]
                    [npass   (length passes)]
                    
                    [% (if (and (null? aa-checks) (null? pa-checks));happens when used to only check non-duplicate args
                           1
                           (/ (max 0 (- npass nerror)) nchecks))])
               
               (cond [(> nerror 0) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (test argument coverage): incorrect - one or more tests caused an error.")]
                     [(= % 1)      (score-it 'test-thoroughness 1 1 #f "Test thoroughness (test argument coverage): correct.")]
                     [else         (score-it 'test-thoroughness 1 % #f "Test thoroughness (test argument coverage): incorrect - missing one or more cases.")]))))]))


(define (test-args-equal-positions lo-args)
  (let* ([nargs (length (car lo-args))])
    (map add1
         (filter (lambda (i)
                   (let ([args-at-p (for/list ([args lo-args]) (list-ref args i))])
                     (andmap (lambda (x) (equal? x (car args-at-p))) (cdr args-at-p))))
                 (sequence->list (in-range nargs))))))



(define (check-faulty-functions fn-name tests defns)
  (cond [(< (length tests) 2) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (known fault detection): incorrect - at least 2 tests are required.")]
        [else
         (let* ([results
                 (calling-evaluator #f
                   `(local [(define (%%all-tests ,fn-name)           ;#t if at least one test fails
                              (not (and ,@(for/list ([test tests])
                                            (xform-test test)))))]
                      (%%check-faulty-functions
                       %%all-tests
                       (list ,@(for/list ([defn defns])
                                 `(local [,defn] ,fn-name))))))]     ;remember we can't use lambda
                
                [ndefns    (length defns)]
                [ndetected (count (lambda (x) (eqv? x #t)) results)]
                [nmissed   (count (lambda (x) (eqv? x #f)) results)]
                [nerror    (count (lambda (x) (eqv? x 'error)) results)]
                
                [%         (/ ndetected ndefns)])
           
           (cond [(= % 1)        (score-it 'test-validity 1 1 #f "Test thoroughness (known fault detection): correct.")]
                 [(zero? nerror) (score-it 'test-validity 1 % #f "Test thoroughness (known fault detection): incorrect - no submitted test failed for ~a of ~a known faulty functions." nmissed ndefns)]
                 [else           (score-it 'test-validity 1 % #f "Test thoroughness (known fault detection): incorrect - no submitted test failed for ~a of ~a known faulty functions, and ~a tests caused an error." nmissed ndefns nerror)]))]))

;; -> (listof (list (listof Any) Any)|'error)
(define (get-lo-args-and-result fn-name checks)
  (let* ([checks   (filter not-check-satisfied? checks)]
         [fn-names (map (lambda (ce) (gensym)) checks)])
    (if (empty? checks)
        '()
        (calling-evaluator #f
         `(%%call-thunks-with-handler
           (local ,(for/list ([c     checks]
                              [fn-nm fn-names])
                     `(define (,fn-nm _)
                        (list ,(subst 'list fn-name (cadr c)) ;...trying to make local work with 1985 hygiene !!! do it right w/ walker
                              ,(caddr c))))
             (list ,@fn-names)))))))

(define (xform-test test-expr)
  (if (pair? test-expr)
      (case (car test-expr)
        [(check-expect)     `(equal?        ,@(cdr test-expr))]
        [(check-member)     `(member?       ,@(cdr test-expr))]
        [(check-equal-sets) `(%%equal-sets? ,@(cdr test-expr))]
        
        [(check-within)     `(<= (magnitude (- ,(cadr test-expr) (caddr test-expr))) (cadddr ,test-expr))]
        [(check-satisfied)  `(,(caddr test-expr) ,(cadr test-expr))]

        [(check-random)
         `(local [(define (thunk1 _) ,(cadr  test-expr))
                  (define (thunk2 _) ,(caddr test-expr))]
            (%%check-random thunk1 thunk2))]
        [else test-expr])
      test-expr))



  

(define-syntax (grade-template-origin stx)  ;!!! -origin
  (syntax-case stx ()
    [(_ sol)   #'(grade-template-origin 1 sol)]
    [(_ n sol)
     #'(recovery-point grade-template-origin
         (assert-context--@htdf)
	 (let* ([htdf    (car (context))]
		[to-tags (htdf-template-origins htdf)])
           (if (< (length to-tags) n)
               (score-it 'template-origin 1 0 #f "Template origin tag: could not find ~a template origin tag." (number->ordinal n))
               (check-template-origin (list-ref to-tags (sub1 n)) `sol))))]))

;(define-syntax (grade-exact-problem-body stx)) ;deprecated, see pset-01-grader.rkt

(define-syntax (grade-refactoring stx)
  (syntax-case stx ()
    [(_ n
        [sol-sig ...]
        [sol-templ ...]
        min
        test ...)
     #'(recovery-point grade-refactoring
         (assert-context--@htdf)
         (weights (.15 .2 .15 *)
           (score-max (grade-signature n sol-sig) ...)
           (score-max (grade-template-origin n sol-templ) ...) 
           (grade-submitted-tests n min)
           (grade-additional-tests n test ...)))]))







(define-syntax (grade-constants-use stx)
  (syntax-case stx ()
    [(_ constants) #'(grade-constants-use 1 constants)]
    [(_ n constants)
     #'(recovery-point grade-constants-use
         (assert-context--@htdf)
         (let* ([htdf (car (context))]
                [defns (htdf-defns htdf)])
           (check-bounds n (length defns) "function definition")
           (check-constants-use (list-ref defns (sub1 n)) 'constants (list-ref htdf n) false)))]))

(define-syntax (grade-tests-constants-use stx)
  (syntax-case stx ()
    [(_ n constants)
     #'(recovery-point grade-tests-constants-use
	(assert-context--@htdf)
        (let* ([htdf (car (context))]
               [tests (htdf-checks htdf)])
          (check-constants-use tests 'constants (list-ref htdf n) true)))]))


(define (check-constants-use defn constants htdf tests?)
  (let* ([free-defn (free defn)]
         [constants-not-used (filter (lambda (x) (not (member x free-defn))) constants)]
         [% (/ (- (length constants) (length constants-not-used))
               (length constants))])
    (score-it 'submitted-tests 1 % #f
              (format "~a ~a: ~a."
                      (if tests? "Tests use" (format "Function body uses"))
                      (pluralize (length constants) "required constant")
                      (cond [(= % 1) "correct"]
                            [(> % 0) "partially correct"]
                            [else    "incorrect"])))))







(define-syntax (grade-htdd-coherence stx)
  (syntax-case stx ()
    [(_)
     #'(recovery-point grade-htdd-coherence
         (assert-context--@htdd)
         (check-htdd-coherence (car (context))))]))











; !!!
(define (ensure-htdd-coherence htdd) true)

;; post condition:
;;  recipe elements in proper order
;;  same number of fn-names, signatures, template origins, templates, defines.
;;  same order of fn-names and defines
;;  number of arguments in signature, checks, and defines match
(define (ensure-htdf-coherence htdf)
  (define fn-names (cdr htdf))
  (define num-fns  (length fn-names))
  (define sexps    (htdf-sexps htdf))

  (define (fail ctl . args)
    (ensure #f (string-append "in ~s, htdf recipe elements are inconsistent and/or incomplete - " ctl) htdf args))

  (define (next p s . args)
    (cond [(null? sexps)         (fail (format "~a is missing from the htdf design." (apply format s args)))]
	  [(not (p (car sexps))) (fail (format "at current position in htdf recipe, ~a should be a ~a"
					      (car sexps)
					      (apply format s args)))]
	  [else
	   (begin0 (car sexps) (set! sexps (cdr sexps)))]))

  (define sigs   (build-list num-fns (lambda (n) (next @signature? "@signature tag"))))
  (define checks (let scan ([res empty])
		   (if (or (empty? sexps) (not (check? (car sexps))))
		       (if (and (< (length res) 2)
				(not (and (pair? sexps) (equal? (car sexps) '(@template-origin htdw-main)))))
			   (fail "after @signature there must be at least two tests unless the template origin is htdw-main")
			   (reverse res))
		       (scan (begin0 (cons (car sexps) res)
				     (set! sexps (cdr sexps)))))))
  (define to-tag&defn-pairs
    (map (lambda (nm)
	   (list (next @template-origin? "a @template-origin tag for ~a"       nm)
		 (next fn-defn?          "a function definition for ~a" nm)))
	 fn-names))

  (for ([name fn-names]
	[num  (in-naturals 1)]
	[sig  sigs]
	[pair to-tag&defn-pairs])
    (let* ([to-tag (car pair)]
	   [defn (cadr pair)]
	   [nargs (length (signature-args sig))])
      (unless (eqv? name (caadr defn))
	(if (pair? (cdr fn-names))
	    (fail "function definitions not in same order as in @htdf tag")
	    (fail "function definition does not match name in @htdf tag")))
      (unless (= (length (cdadr defn)) nargs)
	(fail "~a definition and @signature do not have the same number of arguments" (number->ordinal num)))
      (for ([check checks])
        (when (eqv? (caadr check) name)
	  (unless (= (length (cdadr check)) nargs)
	    (fail "in ~a, number of operands does not match ~a signature" check (number->ordinal num)))))))

  #t)

;; (ensure-tag-contains-elements '(@htdw *)
;;                               '((define WIDTH 100)))

;; (env ...)
;;
(define-syntax (grade-top-level-expression stx)
  (syntax-case stx ()
    [(_ must-use-free defns ... value-expr)
     #'(recovery-point grade-top-level-expression
	 (assert-context--@problem)
         (check-top-level-expression `must-use-free `(defns ...) `value-expr))]))

(define (check-top-level-expression must-use-free sol-defns sol-expr)
  (let* ([problem (car (context))]
         
         [sol-ndefns   (length sol-defns)]
         [sol-free     (free sol-expr)]
         [sol-values   (constants sol-expr)] ;!!! rename constant to values or something
         
         [sub          (problem-sexps problem)]
         [sub-defns    (and (pair? sub) (filter defn? sub))]
         [sub-expr     (and (pair? sub) (not (defn? (last sub))) (last sub))]

         [sub-ndefns   (and sub-defns (length sub-defns))]
         [sub-free     (and sub-expr (free sub-expr))]
         [sub-values   (and sub-expr (constants sub-expr))]

         [scores
          (list
           (if (zero? sol-ndefns)
               (rubric-item 'other (or (not sub-ndefns) (zero? sub-ndefns)) "Does not add any definitions")
               (rubric-item 'other
                            (and (= sub-ndefns sol-ndefns)
                                 (equal? (take sub sol-ndefns) sol-defns))
                            "Does not comment out, edit or add to the ~a supplied in starter file"
                            (pluralize sol-ndefns "definition")))
           
           (if sub-expr ;(zero? sol-ndefns)
               (rubric-item 'other sub-expr "Has a single top-level expression")
               (rubric-item 'other
                            sub-expr
                            "Has a single top-level expression following the ~a supplied in the starter file"
                            (pluralize sol-ndefns "definition")))
           
           (and (pair? must-use-free)
                (rubric-item 'other
                             (and sub-free (andmap (lambda (s) (member s sub-free)) must-use-free))
                             "Expression uses required CONSTANTs"))
           
           (rubric-item 'eval-etc
                        (and sub-expr (calling-evaluator #f `(equal? ,sub-expr ,sol-expr)))
                        "Expression evaluates to correct value"))])

    (grade-prerequisite 'eval-etc "has an added top-level expression" sub-expr
      (header "Top-level expression:"
        (combine-scores
         (if (member #f scores)
             (weights* 1.0 '(.15 .15     *) (remove #f scores))
             (weights* 1.0 '(.10 .10 .10 *) scores)))))))






(define (grade-not-recursive [n 1])
  (let* ([defn    (list-ref (htdf-defns (car (context))) (sub1 n))]
         [fn-name (cdadr defn)]
         [not-rec? (not (recursive? defn))])
    (rubric-item 'template not-rec? "Must not use any part of a recursive template")))

(define (grade-tail-recursive [n 1] [local-fn-names #f])
  (let* ([htdf   (car (context))]
         [defns  (and htdf (htdf-defns htdf))]
         [defn   (and (>= (length defns) n) (list-ref defns (sub1 n)))]
         [body   (and (fn-defn? defn) (caddr defn))]
         
         [local-defns (and (list? body)
                           (= (length body) 3)
                           (eqv? (car body) 'local)
                           (filter fn-defn? (cadr body)))]
         [local-names (or local-fn-names (and local-defns (map caadr local-defns)))]
         [local-calls (and local-defns
                           (filter (lambda (c)
                                     (member (call-called-fn-name c) local-names))
                                   (foldr append '() (map calls2 local-defns))))])


    (if (or (not local-calls) (null? local-calls))
        (rubric-item 'template-intact #f "Local expression with function definitions present")
        (header "Tail recursive: "
                (combine-scores
                 (weights* 1.0 '(1 *)
                   (cons (rubric-item 'template-intact
                                      (andmap (lambda (c) (eqv? (call-ctx c) 'tail)) local-calls)
                                      "All recursive or mutually recursive calls must be in tail position") 
                         (for/list ([c local-calls])
                           (score-it 'template-intact 0 0 (eqv? (call-ctx c) 'tail)
                                     "Call must be in tail position ~a"
                                     (syntax->datum (call-stx c)))))))))))

(define (check-tail-recursive [n 1] [local-names #f])
  (let* ([htdf   (car (context))]
         [defns  (and htdf (htdf-defns htdf))]
         [defn   (and (>= (length defns) n) (list-ref defns (sub1 n)))]
         [local-defns (and defn (filter fn-defn? (cdr (defines defn))))])

    (if (or (not local-defns) (null? local-defns))

        (let* ([name   (and defn (caadr defn))]
               [calls  (and defn (filter (lambda (c)
                                           (eqv? (call-called-fn-name c) name))
                                         (calls2 defn)))])
          (andmap (lambda (c) (eqv? (call-ctx c) 'tail)) calls))
        
        (let* ([names  (or local-names (and local-defns (map caadr local-defns)))]                   ;these three variables values correspond
               [defns  (map (lambda (n) (findf (compose (curry eqv? n) caadr) local-defns)) names)]  ;locally defined functions
               [lcalls (map (lambda (defn)                                                           ;for each defn, calls to other locally defined fns
                              (filter (lambda (call) (memq (call-called-fn-name call) names))
                                      (calls2 defn)))
                            defns)])

          ;; all this should use stx objects!!!

          (define (get-defn   name) (list-ref defns  (index-of names name)))
          (define (get-lcalls name) (list-ref lcalls (index-of names name)))

          ;; is there a cycle back to this call?
          (define (cycles? call [path '()])
            (cond [(and (pair? path) (eqv? call (last path))) true] 
                  [(member call path) false]
                  [else
                   (ormap (curryr cycles? (cons call path))
                          (get-lcalls (call-called-fn-name call)))]))

          (andmap (lambda (calls)
                    (andmap (lambda (call)
                              (or (eqv? (call-ctx call) 'tail)                 ;fastest check first
                                  (not (cycles? call))))
                            calls))
                  lcalls)))))

(define (check-recursive? [n 1] [local-names #f])
  (let* ([htdf   (car (context))]
         [defns  (and htdf (htdf-defns htdf))]
         [defn   (and (>= (length defns) n) (list-ref defns (sub1 n)))]
         [local-defns (and defn (filter fn-defn? (cdr (defines defn))))])

    (if (or (not local-defns) (null? local-defns))

        (let* ([name   (and defn (caadr defn))]
               [calls  (and defn (filter (lambda (c)
                                           (eqv? (call-called-fn-name c) name))
                                         (calls2 defn)))])
          (not (null? calls)))
        
        (let* ([names  (or local-names (and local-defns (map caadr local-defns)))]                   ;these three variables values correspond
               [defns  (map (lambda (n) (findf (compose (curry eqv? n) caadr) local-defns)) names)]  ;locally defined functions
               [lcalls (map (lambda (defn)                                                           ;for each defn, calls to other locally defined fns
                              (filter (lambda (call) (memq (call-called-fn-name call) names))
                                      (calls2 defn)))
                            defns)])

          ;; all this should use stx objects!!!

          (define (get-defn   name) (list-ref defns  (index-of names name)))
          (define (get-lcalls name) (list-ref lcalls (index-of names name)))

          ;; is there a cycle back to this call?
          (define (cycles? call [path '()])
            (cond [(and (pair? path) (eqv? call (last path))) true] 
                  [(member call path) false]
                  [else
                   (ormap (curryr cycles? (cons call path))
                          (get-lcalls (call-called-fn-name call)))]))

          (ormap (lambda (calls)
                   (ormap cycles? calls))
                 lcalls)))))

(define (check-not-stub [n 1])
  (let* ([htdf   (car (context))]
         [defns  (and htdf (htdf-defns htdf))]
         [defn   (and (>= (length defns) n) (list-ref defns (sub1 n)))])
    (pair? (caddr defn))))


  



;; check-* stuff




(define (check-signature sub sol)
  (let ([sub-args   (signature-args   (cdr sub))]
        [sub-result (signature-result (cdr sub))]
        [sub-fail?  (signature-fail?  (cdr sub))]
	
        [sol-args   (signature-args   (cdr sol))]
        [sol-result (signature-result (cdr sol))]
        [sol-fail?  (signature-fail?  (cdr sol))])

    (header "Signature:"
     (combine-scores
      (weights* 1.0
                '(*)
                (let loop ([sub-args sub-args]
                           [sol-args sol-args])
                  (if (empty? sol-args)
                      (list (rubric-item 'signature 
                                         (and (equal? sub-result sol-result)
                                              (equal? sub-fail?  sol-fail?))
                                         "result type"))
                      (cons (rubric-item 'signature 
                                         (and (pair? sub-args)
                                              (equal? (car sub-args) (car sol-args)))
                                         "argument type")
                            (loop (and (pair? sub-args) (cdr sub-args)) (cdr sol-args))))))))))
    

(define (check-template-origin sub sol)

  (define last-char-index (compose sub1 string-length))

  (define (last-char-*? s)
    (char=? (string-ref s (last-char-index s)) #\*))

  (define (optional? sexp)
    (and (symbol? sexp)
         (last-char-*? (symbol->string sexp))))
  
  (define not-optional? (compose not optional?))
         
  (define (remove-end-* sexp)
    (if (optional? sexp)
        (let* ([s (symbol->string sexp)])
          (string->symbol (substring s 0 (last-char-index s))))
        sexp))  
  
  (let* ([sub (cdr sub)] ;drop @template-origin
         [required         (filter not-optional? sol)]
         [allowed-explicit (map remove-end-* sol)]
         [allowed          (if (memq 'accumulator allowed-explicit)
                               (cons 'encapsulated allowed-explicit)
                               allowed-explicit)]
         [present-not-allowed  (filter (compose not (curryr member allowed)) sub)])
    (header "Template origin:"
      (combine-scores
       (weights* 1.0
                 '(*)
                 (append (for/list ([req required])
                           (if (member req sub)             
                               (score-it 'template-origin 1 1 #f "~a: correct." req)
                               (score-it 'template-origin 1 0 #f "~a: incorrect (missing)." req)))
                         (for/list ([pna present-not-allowed])
                           (score-it 'template-origin 1 0 #f "~a: incorrect (not allowed)." pna))))))))





;; Produce t/f if evaluatable w/o errors.
(define (check-evaluatable? expr)
  (with-handlers ([exn:fail? (lambda (e) #f)]) (calling-evaluator #t expr) #t))

(define (check-htdd-coherence htdd)
  (score #f 'other 1 1 '() '()))









;; !!!!! DOM "utils.rkt"








;; Utility functions




;; used to enforce proper nesting of grader-* forms in autograder files

(define (assert-context--top-level)  (assert-context empty?      "at top-level inside grade-submission."))
(define (assert-context--@problem)   (assert-context '@problem   "immediately inside grade-problem."))
(define (assert-context--@htdd)      (assert-context '@htdd      "immediately inside grade-htdd."))
(define (assert-context--@htdf)      (assert-context '@htdf      "immediately inside grade-htdf."))
(define (assert-context--@htdf-main) (assert-context @htdf-main? "immediately inside grade-htdf."))

(define (assert-context guard str)
  (unless (if (symbol? guard)
	      (and (pair? (context)) (eqv? (caar (context)) guard))
	      (guard (context)))
    (error 'assert-context "Must be ~a, instead context is ~a." str (context))))

;; !!! moves or goes away
(define (check-bounds n max kind)
  (unless (valid-index (sub1 n) max) (raise-student-error "could not find the ~a ~a" (number->ordinal n) kind)))







;; compiling graders and/or keeping a table of already loaded graders
;; does not make any appreciable performance difference, so to keep
;; things simple we just reload the grader each time

(define (find-grader fn)
  (let ([tag (find-assignment-tag fn)])
    (if (not tag)
        default-grader
        (let* ([elts (string-split (symbol->string (cadr tag)) "/")]
               [dir (apply build-path GRADERS-DIR (drop-right elts 1))]
               #;
               [compiled (build-path dir
                                     "compiled"
                                     (string-append (car (take-right elts 1)) "-grader_rkt.zo"))]
               [source   (build-path dir
                                     (string-append (car (take-right elts 1)) "-grader.rkt"))])
          (auto-reload-procedure source 'grader)))))

(define (has-grader? p)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let ([tag (find-assignment-tag p)])
      (and tag
           (let* ([elts (string-split (symbol->string (cadr tag)) "/")]
                  [fn (build-path (apply build-path GRADERS-DIR (drop-right elts 1))
                                  (string-append (car (take-right elts 1)) "-grader.rkt"))])
             (file-exists? fn))))))
  

(define (find-assignment-tag fn) (let-values ([(assgn-tag cwl-tag) (find-assgn-cwl-tags fn)]) assgn-tag))
(define (find-cwl-tag        fn) (let-values ([(assgn-tag cwl-tag) (find-assgn-cwl-tags fn)]) cwl-tag))

(define (find-assgn-cwl-tags fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ([line (read-line)]
                 [assgn-tag #f]
                 [cwl-tag   #f])
        (cond [(and assgn-tag cwl-tag) (values assgn-tag cwl-tag)]
              [(eof-object? line)      (values assgn-tag cwl-tag)]
              [else
               (let ([line (string-downcase line)])
                 (cond [(and (not assgn-tag)
                             (regexp-match #rx"^\\(@assignment .*\\)" line))
                        (loop (read-line) (read-from-string line) #f)]
                       [(and (not cwl-tag)
                             (regexp-match #rx"^\\(@cwl .*\\)" line))
                        (loop (read-line) assgn-tag (read-from-string line))]
                       [else
                        (loop (read-line) assgn-tag cwl-tag)]))])))))


