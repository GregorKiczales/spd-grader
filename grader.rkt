#lang racket

(require racket/bool
         racket/function
         racket/list
         racket/sequence
         racket/string
         "defs.rkt"
         "reloadable.rkt"
         "sandbox.rkt"
         "score.rkt"
         "utils.rkt"
         "walker.rkt")
 
(provide (all-defined-out)
         (except-out (all-from-out "defs.rkt") 
                     CURRENT-TERM
                     GRADERS-DIR)
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
(struct exn:fail:student-error    exn:fail:user ())         ;student errors like missing tags
(struct exn:fail:ensure-violation exn:fail:user ())         ;student errors like changing tests or signatures
(struct exn:fail:eval-error       exn:fail:user (student?)) ;error evaluating teaching language code student? false if grader code

(define-syntax (per-problem-error-handling stx) ;used in grade-problem 
  (syntax-case stx ()
    [(_ n exp ...)
     #'(with-handlers ([exn:fail?
                        (lambda (e)
                          (cond [(exn:fail:student-error? e)
                                 (weights (*)
                                          (score-it 'other 1 0 #f (exn-message e)))]
                                [(exn:fail:ensure-violation? e)
                                 (weights (*)
                                          (score-it 'other 1 0 #f "Grading of problem halted - ~a." (exn-message e)))]
                                [(exn:fail:eval-error? e)
                                 (weights (*)
                                          (if (exn:fail:eval-error-student? e)
                                              (score-it 'other 1 0 #f "Error evaluating submitted code - ~a" (exn-message e))
                                              (score-it 'other 1 0 #f "Error evaluating grader code that calls submitted code - ~a" (exn-message e))))]
                                [else
                                 ((logger) (format "Internal grader system error ~a: ~a" n (exn-message e)))
                                 (weights (*) (score-it 'other 1 1 #f "Internal grader system error, student given 100% on this problem. This does not necessarily mean the submission is correct. Grade may change when grader system error is fixed."))]))])
         exp ...)]))

(define-syntax (calling-evaluator stx)
  (syntax-case stx ()
    [(_ student? exp)
     #'(with-handlers ([exn:fail?
                        (lambda (e)
                          (raise (exn:fail:eval-error (exn-message e) (current-continuation-marks) student?)))])
         ((evaluator) exp))]))


(define (raise-student-error msg . v)
  (raise (exn:fail:student-error  (apply format msg v) (current-continuation-marks))))


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




;; !!!! temporary

(define-syntax (recovery-point stx)
  (syntax-case stx ()
    [(_ w item ...)
     #'(begin item ...)]))

(define (autograde-file filename [verb? #t] [earl? #f] [rpt (current-output-port)] [logr displayln])
  (let ([grader
         (with-handlers ([exn:fail? (lambda (exn)
                                      (logr (format "Error: problem loading grader for ~a - ~a" filename (if (exn? exn) (exn-message exn) exn))))])
           (find-grader filename))])
    (autograde-file-with-grader filename grader verb? earl? rpt logr)))

(define (autograde-file-with-grader filename grader [verb? #t] [earl? #f] [rpt (current-output-port)] [logr displayln])               
  ;; until we get going any error is a grader framework error and we just log it
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (logr (format "Error: framework error for submission ~a - ~a"
                                   filename
                                   (if (exn? exn) (exn-message exn) exn))))])
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
                    (logr (format "Error: running submission ~a - ~a" filename (exn-message exn))))
                  ;; deliberately don't display error so students debug themselves
                  ;; rather than just try to debug from autograder reports
                  (display-overall-grade 0
                                         (format "Error running submission ~a." (exn-message exn))
                                         rpt)
                  (abort-current-continuation abort-tag (lambda () (void))))

                (define (handle-framework-error exn)
                  ((logger) (format "Error: Framework error for submission ~a - ~a"
                                    filename
                                    (if (exn? exn) (exn-message exn) exn)))
                  (display-overall-grade 100
                                         (format "Autograder framework error. ~a"
                                                 (if (exn? exn) (exn-message exn) exn))
                                         rpt))
                
                (call-with-continuation-prompt 
                 (lambda ()
                   (let ([e
                          ;; this handles errors in initial running of submission
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
                                                (with-handlers ([exn:fail:resource? handle-resource-error])
                                                  (e (if (string? x) `(identity ,x) x))))]
                                             [logger    logr]
                                             [context   '()]
                                             [elts      #f])
                                (elts (fn->elts filename))
                                (grader))])

                         ;; we now have a score report, time to render it
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
                       (let* ([defns (htdf-defns (car (context)))] ;!!!(list '@htdf 'n))]
                              [n (and (pair? defns) (car defns))])
                         (weights (*) item ...)))))
         
         #'(begin ;recovery-point n !!!
             (assert-context--@problem)
             (parameterize ([context (cons (get-htdf* `n) (context))])
               (header (format "~a: " (car (context)))
                 (weights (*) item ...)))))]))

(define-syntax (grade-bb-handler stx) ;this may be too specialized for this file
  (syntax-case stx ()
    [(_ name fn item ...)
     #'(if (false? fn)
           (rubric-item 'other #f "couldn't find ~a handler function" 'name)
           (recovery-point grade-bb-handler
             (assert-context--@problem)
             (parameterize ([context (cons (get-htdf* fn) (context))])
               (header (format "big-bang ~a handler:" 'name)
                       (weights (*) item ...)))))]))

(define (grade-htdf* fn thnk)  ;!!! try to make this go away w/ check-htdf, but used in pset 4
  (recovery-point grade-htdf*
   (assert-context--@problem)
   (parameterize ([context (cons (get-htdf* fn) (context))])
     (header (format "~a: " (car (context)))
             (thnk)))))
  
(define-syntax (grade-signature stx)
  (syntax-case stx ()
    [(_ sol)   #'(grade-signature 1 sol)]
    [(_ n sol) #'(grade-signature-1 n `sol)]
    [(_ n sol sol2 ...) #'(score-max (grade-signature n sol)
                                     (grade-signature n sol2) ...)]))

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
    [(_)       #'(grade-submitted-tests 1 0)]
    [(_   min) #'(grade-submitted-tests 1 min)]
    [(_ n min)
     #'(begin 
         (assert-context--@htdf)
        ;(check-bounds n (length (htdf-names (car (context)))) "function definition")
         (let* ([htdf    (car (context))]
		[fn-name (if (symbol? n) n (list-ref (htdf-names htdf) (sub1 n)))]
                [tests (get-function-tests fn-name)])
           (check-submitted-tests fn-name tests)))]))
                                   

(define-syntax (grade-additional-tests stx)
  (syntax-case stx ()
    [(_ n test ...)
     #'(begin 
         (assert-context--@htdf)
         (let* ([htdf    (car (context))]
		[fn-name (if (symbol? n) n (list-ref (htdf-names htdf) (sub1 n)))])
           (check-additional-tests fn-name `(test ...))))]))


(define-syntax (grade-tests-validity stx)
  (syntax-case stx ()
    [(_ (p ...) r check ...)
     #'(begin
         (assert-context--@htdf)
         (let* ([htdf    (car (context))]
                [fn-name (list-ref (htdf-names htdf) 0)]
                [tests   (get-function-tests fn-name)])
           (check-tests-validity fn-name tests (append (list 'p ...) (list 'r)) `(check ...))))]))



(define-syntax (grade-argument-thoroughness stx)
  (syntax-case stx (all-args per-args)
    [(_ lop
        (all-args (aaparam)     aacheck ...)
        (per-args (paparam ...)   check ...))
     #'(grade-argument-thoroughness* `lop `aaparam (list `aacheck ...) (list `paparam ...) (list `check ...))]
    
    [(_ lop
        (all-args (aaparam) aacheck ...))
     #'(grade-argument-thoroughness lop (all-args (aaparam) aacheck ...) (per-args (_)              ))]
    [(_ lop (per-args (paparam ...) pacheck ...))
     #'(grade-argument-thoroughness lop (all-args (_)                  ) (per-args (paparam ...) pacheck ...))]
    [(_ lop)
     #'(grade-argument-thoroughness lop (all-args (_)                  ) (per-args (_)              ))]))

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
    [(_ n defn ...)
     #'(begin
         (assert-context--@htdf)
         (let* ([htdf      (car (context))]
                [fn-name   (car (htdf-names htdf))]
                [tests     (get-function-tests fn-name)])
           (check-thoroughness fn-name tests (list `defn ...))))]))



;; fn-name is passed for consistency w/ other check*test* fns
(define (check-submitted-tests  fn-name tests) (check-tests fn-name tests 2 'submitted-tests  "Submitted"  "submitted"))
(define (check-additional-tests fn-name tests) (check-tests fn-name tests 0 'additional-tests "Additional" "autograder additional"))

(define (check-tests fn-name tests min topic Camel lower)
  (cond [(< (length tests) min) (score-it topic 1 0 #f "~a tests: incorrect - at least ~a test~a required." Camel min (plural min))]
        [(= (length tests) 0)   (score-it topic 1 1 #f "~a tests: correct." Camel)]
        [else
         (let* ([results (map eval-test tests)]
                [ntests (length tests)]
                [npass  (count (curry eqv? #t)     results)]
                [nerror (count (curry eqv? 'error) results)]
                [%      (/ npass ntests)])
           (cond [(= npass ntests)   (score-it topic 1 1 #f "~a tests: correct." Camel)]
                 [(= npass nerror 0) (score-it topic 1 0 #f "~a tests: incorrect - all ~a tests failed." Camel lower)]
                 [(=       nerror 0) (score-it topic 1 % #f "~a tests: incorrect - ~a ~a tests failed." Camel (- ntests npass) lower)]
                 [else               (score-it topic 1 % #f "~a tests: incorrect - ~a ~a tests failed, and ~a caused errors." Camel (- ntests npass nerror) lower nerror)]))]))


;; rename to check-validity !!!
(define (check-tests-validity fn-name tests params checks)
  (let ([ce-s (filter check-expect? tests)])
    (cond [(= (length ce-s) 0) (score-it 'test-validity 1 0 #f "Test validity (matches problem statement): incorrect - at least 1 test is required.")]
          [else 
           (let* ([lo-args-and-result  (get-lo-args-and-result fn-name ce-s)]
                  [lo-args-with-result (map (lambda (2-list) (append (car 2-list) (cdr 2-list)))
                                            (filter pair? lo-args-and-result))]
                  [lo-error            (filter (curry eqv? 'error) lo-args-and-result)]
                  [checker-names  (map (lambda (c) (gensym "check")) checks)]
                  
                  [fails 
                   (calling-evaluator #f
                    `(%%check-validity ',lo-args-with-result
                                       ',checker-names
                                       (local ,(for/list ([name checker-names]
                                                          [check checks])
                                                 `(define (,name ,@params) ,check))
                                         (list ,@checker-names))))]

                  [ntests (length ce-s)]
                  [nerror (length lo-error)]
                  [nfail  (length fails)]
                  [npass  (- ntests nfail nerror)]
                  
                  [% (/ (max 0 npass) ntests)])
        
             (cond [(= % 1)        (score-it 'test-validity 1 1 #f "Test validity (matches problem statement): correct.")]
                   [(zero? nerror) (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests is invalid." nfail ntests)]
                   [(empty? fails) (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests caused an error." nerror ntests)]
                   [else           (score-it 'test-validity 1 % #f "Test validity (matches problem statement): incorrect - ~a of ~a tests is invalid, and ~a caused an error." nfail ntests (pluralize nerror "test"))]))])))

(define (check-argument-thoroughness fn-name tests lop aa-param aa-checks pa-params pa-checks) '()
  (let ([ce-s (filter check-expect? tests)])
    (cond [(= (length ce-s) 0) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (test argument coverage): incorrect - at least 1 test is required.")]
          [else
           (let* ([lo-args-and-result  (get-lo-args-and-result fn-name ce-s)]
                  [lo-args  (map car (filter pair? lo-args-and-result))]
                  [lo-error          (filter (curry eqv? 'error) lo-args-and-result)]
                 
                  [aa-check-names (map (lambda (x) (gensym)) aa-checks)]   ; (take check-numbers (length aa-checks))]
                  [pa-check-names (map (lambda (x) (gensym)) pa-checks)]  ; [pa-check-numbers (drop check-numbers (length aa-checks))]
                 
                  [equal-positions (filter (lambda (p) (not (member p lop))) (test-args-equal-positions lo-args))])
             
             (if (not (null? equal-positions))
                 (score #f 'test-thoroughness 1 0
                        '()
                        (cons (message #f "Test thoroughness (test argument coverage): incorrect -")
                              (for/list ([p equal-positions])
                                (message #f " In every test the value of argument ~a is the same." p))))

                 (let* ([passes
                         (calling-evaluator #f
                           `(%%check-thoroughness ',lo-args
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
                        [nerror  (length lo-error)]
                        [npass   (length passes)]
                        [nfail   (- nchecks npass)]
                        
                        [% (if (and (null? aa-checks) (null? pa-checks));happens when used to only check non-duplicate args
                               1
                               (/ (max 0 (- npass nerror)) nchecks))])
                     
                   (cond [(> nerror 0) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (test argument coverage): incorrect - one or more tests caused an error.")]
                         [(= % 1)      (score-it 'test-thoroughness 1 1 #f "Test thoroughness (test argument coverage): correct.")]
                         [else         (score-it 'test-thoroughness 1 % #f "Test thoroughness (test argument coverage): incorrect - missing one or more cases.")]))))])))


(define (test-args-equal-positions lo-args)
  (let* ([nargs (length (car lo-args))])
    (map add1
         (filter (lambda (i)
                   (let ([args-at-p (for/list ([args lo-args]) (list-ref args i))])
                     (andmap (lambda (x) (equal? x (car args-at-p))) (cdr args-at-p))))
                 (sequence->list (in-range nargs))))))



(define (check-thoroughness fn-name tests defns)
  (cond [(= (length tests) 0) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (known fault detection): incorrect - at least 1 test is required.")]
        [else
         (let ([xformed-tests (map xform-test tests)])
           (with-handlers ([exn:fail? (lambda (e) (score-it 'test-thoroughness 1 0 #f "Test thoroughness (known fault detection): incorrect - one or more tests caused an error."))])
             (let ([results
                    ;; build the code for all faulty functions together
                    ;; so that we only cross into the sandbox once
                    (calling-evaluator #f
                                       `(list
                                         ,@(for/list ([defn defns])
                                             `(local [,defn]
                                                (not (and ,@xformed-tests))))))]) ;true if at least one test fails


               (if (andmap identity results)
                   (score-it 'test-thoroughness 1 1 #f "Test thoroughness (known fault detection): correct.")
                   (score-it 'test-thoroughness 1 (/ (count identity results) (length defns)) #f "Test thoroughness (known fault detection): incorrect - failed to detect known likely faults.")))))]))

;; -> (listof (list (listof Any) Any)|'error)
(define (get-lo-args-and-result fn-name ce-s)
  (if (empty? ce-s)
      '()
      (let ([fn-names (map (lambda (ce) (gensym)) ce-s)])
        ((evaluator)
         `(%%eval-test-args-and-results
           (local ,(for/list ([ce ce-s]
                              [fn-nm fn-names])
                     ;; We may be running in BSL, so no zero-arg
                     ;; functions and no lambda are allowed.
                     `(define (,fn-nm _)
                        (list ,(subst 'list fn-name (cadr ce)) ;...trying to make local work with 1985 hygiene !!! do it right w/ walker
                              ,(caddr ce))))
             (list ,@fn-names)))))))




  

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
         [sub-defns    (filter defn? sub)]
         [sub-expr     (and (not (defn? (last sub))) (last sub))]

         [sub-ndefns   (length sub-defns)]
         [sub-free     (free sub-expr)]
         [sub-values   (constants sub-expr)]

         [scores
          (list
           (if (zero? sol-ndefns)
               (rubric-item 'other (zero? sub-ndefns) "Does not add any definitions")
               (rubric-item 'other
                            (and (= sub-ndefns sol-ndefns)
                                 (equal? (take sub sol-ndefns) sol-defns))
                            "Does not comment out, edit or add to the ~a supplied in starter file"
                            (pluralize sol-ndefns "definition")))
           
           (if (zero? sol-ndefns)
               (rubric-item 'other sub-expr "Has a single top-level expression")
               (rubric-item 'other
                            sub-expr
                            "Has a single top-level expression following the ~a supplied in the starter file"
                            (pluralize sol-ndefns "definition")))
           
           (and (pair? must-use-free)
                (rubric-item 'other
                             (andmap (lambda (s) (member s sub-free)) must-use-free)
                             "Expression uses required CONSTANTs"))
           
           (rubric-item 'eval-etc
                        (calling-evaluator #f `(equal? ,sub-expr ,sol-expr))
                        "Expression evaluates to correct value"))])

    (header "Top-level expression:"
      (combine-scores
       (if (member #f scores)
           (weights* 1.0 '(.15 .15     *) (remove #f scores))
           (weights* 1.0 '(.10 .10 .10 *) scores))))))


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

;; get- utility functions

(define (tag-sexps t)
  (map elt-sexp (filter (compose (curry member t) elt-tags) (elts))))


(define (get-problem* n)
  (or (get-by-pred (lambda (x) (and (@problem? x) (= n (problem-num x)))))
      (raise-student-error  "Could not find (@problem ~a)." n)))



(define (get-htdf* n) (get-by-name-or-index '@htdf @htdf? n (lambda (x) (member n (htdf-names x)))))
(define (get-htdd* n) (get-by-name-or-index '@htdd @htdd? n (lambda (x) (member n (htdd-names x)))))
(define (get-htdw* n) (get-by-name-or-index '@htdw @htdw? n (lambda (x) (eqv? n (htdw-ws x)))))

(define (get-by-name-or-index kind type? n name?)
  (cond [(and (symbol? n)
	      (eqv? #\$ (string-ref (symbol->string n) 0)))
	 (or (get-by-pred type?)
	     (raise-student-error "could not find ~a tag." kind))]
	[(symbol? n)
	 (or (get-by-pred (lambda (x) (and (type? x) (name? x))))
	     (raise-student-error "could not find (~a ~a)." kind n))]
	[(number? n)
	 (or (get-by-index type? n)
	     (raise-student-error "could not find ~a number ~a" kind n))]))

(define (get-by-pred p)
  (let loop ([sexps (if (null? (context)) (map elt-sexp (elts)) (tag-sexps (car (context))))])
    (cond [(empty? sexps) #f]
          [(p (car sexps)) (car sexps)]
          [else
           (loop (rest sexps))])))

(define (get-by-index p n)
  (let loop ([i 0]                        ;!!! below was just (elts)
	     [sexps (if (null? (context)) (map elt-sexp (elts)) (tag-sexps (car (context))))])
    (cond [(empty? sexps)                 #f]
	  [(and (p (car sexps)) (= i n))  (car sexps)]
	  [     (p (car sexps))           (loop (add1 i) (rest sexps))]
	  [else                           (loop       i  (rest sexps))])))

(define (get-all-tests)
  (filter check? (map elt-sexp (elts))))

(define (get-function-tests fn-name)
  (filter (lambda (t)
	    (and (check? t)
		 (let ([test-actual (cadr t)])
		   (and (pair? test-actual)
			(or (eqv? (car test-actual) fn-name)
			    (and (eqv? (car test-actual) 'local)
				 (eqv? (caaddr test-actual) fn-name)))))))
	  (map elt-sexp (elts))))
  



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




;; -> true | false | error
(define (eval-test test-expr)
  (with-handlers ([exn:fail? (lambda (e) 'error)])
    (case (car test-expr)
      [(check-expect)     (equal? (calling-evaluator #t (cadr test-expr))
                                  (calling-evaluator #t (caddr test-expr)))]
      [(check-member)     (and (member (calling-evaluator #t (cadr test-expr))
                                       (calling-evaluator #t (caddr test-expr)))
                               #t)]
      [(check-equal-sets) (equal-sets? (calling-evaluator #t (cadr test-expr))
                                       (calling-evaluator #t (caddr test-expr)))]
      
      [(check-within)     (<= (magnitude (- (calling-evaluator #t (cadr test-expr))
                                            (calling-evaluator #t (caddr test-expr))))
                              (calling-evaluator #t (cadddr test-expr)))]
      [(check-satisfied)  (and (calling-evaluator #t `(,(caddr test-expr) ,(cadr test-expr)))
                               true)]
      
      [(check-random)
       (let ([rng (make-pseudo-random-generator)]
             [k (modulo (current-milliseconds) (sub1 (expt 2 31)))])
         (equal? (begin (call-in-sandbox-context (evaluator)
                                                 (thunk (current-pseudo-random-generator rng)
                                                        (random-seed k)))
                        (calling-evaluator #t (cadr test-expr)))
                 (begin (call-in-sandbox-context (evaluator)
                                                 (thunk (current-pseudo-random-generator rng)
                                                        (random-seed k)))
                        (calling-evaluator #t (caddr test-expr)))))]

      [else
       (let ([result (calling-evaluator #t test-expr)])
         (if (boolean? result)
             result
             true))])))

(define (xform-test test-expr)
  (if (pair? test-expr)
      (case (car test-expr)
        [(check-expect)     `(equal?  ,@(cdr test-expr))]
        [(check-member)     `(member? ,@(cdr test-expr))]
        [(check-equal-sets) `(equal-sets? ,(cdr test-expr))]
        
        [(check-within)     `(<= (magnitude (- ,(cadr test-expr) (caddr test-expr))) (cadddr ,test-expr))]
        [(check-satisfied)  `(,(caddr test-expr) ,(cadr test-expr))]
        #;
        [(and (pair? test-expr) (eq? (car test-expr) 'check-random))
           (let ([rng (make-pseudo-random-generator)]
                 [k (modulo (current-milliseconds) (sub1 (expt 2 31)))])
             (equal? (begin (call-in-sandbox-context (evaluator)
                                                     (thunk (current-pseudo-random-generator rng)
                                                            (random-seed k)))
                            (calling-evaluator #t (cadr test-expr)))
                     (begin (call-in-sandbox-context (evaluator)
                                                     (thunk (current-pseudo-random-generator rng)
                                                            (random-seed k)))
                            (calling-evaluator #t (caddr test-expr)))))]
        [else test-expr])
      test-expr))


;; Produce t/f if evaluatable w/o errors.
(define (check-evaluatable? expr)
  (with-handlers ([exn:fail? (lambda (e) #f)]) (calling-evaluator #t expr) #t))

(define (check-htdd-coherence htdd)
  (score #f 'other 1 1 '() '()))



;; Helper for check-*
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ TOPIC PREFIX [Q V T] ...)
     #'(rubric TOPIC PREFIX [Q V T] ... [else "correct"])]
    
    [(_ TOPIC PREFIX [Q V T] ... [else Te])
     #'(let* ([max-score (+ V ...)]
              [items (list (list V (lambda () Q) T) ...)]
              [applied (filter (lambda (i) (not ((cadr i)))) items)])
         (if (empty? applied)
             (score-it 'TOPIC 1 1 #f "~a: ~a." PREFIX Te)
             (score #f
                    'TOPIC
                    1
                    (/ (- max-score (foldl + 0 (map car applied))) max-score)
                    '()
                    (cons (message #f "~a: incorrect." PREFIX); #f Te)
                          (map (lambda (item) (message #t (format "~a: ~a." PREFIX (caddr item))))
                               applied)))))]))






;; !!!!! DOM "utils.rkt"

(define CHECK-FORMS
  '(check-expect check-random check-satisfied check-within check-error check-member-of check-range))


(struct elt (tags sexp) #:transparent)


(define (fn->elts fn)
  (parse-elts (read-sexps fn)))

(define (read-sexps fn)
  (call-with-input-file fn
    (lambda (p)
      (parameterize ([read-accept-reader #t]
		     [read-case-sensitive #t]
                     [read-decimal-as-inexact #f] ;to match teaching languages
		     [current-input-port p])
	(read-line (current-input-port))
	(read-line (current-input-port))
	(read-line (current-input-port))
	
	(let loop ([se (read)])
	  (if (eof-object? se)
	      '()
	      (cons (post-read-convert se) (loop (read)))))))))

(define (read-syntaxes fn)
  (call-with-input-file fn
    (lambda (p)
      (parameterize ([read-accept-reader #t]
		     [read-case-sensitive #t]
                     [read-decimal-as-inexact #f] ;to match teaching languages
		     [current-input-port p])

        (cdr (syntax->list (cadddr (syntax->list (read-syntax fn p)))))))))
  




(define (parse-elts lose)
  ;;
  ;; The grade-xxx forms enforce a nesting of graders as follows
  ;;  grade-problem (must be at top-level)
  ;;   grade-htdd
  ;;     grade-dd-rules-and-template
  ;;     ...
  ;;   grade-htdf
  ;;     grade-signature
  ;;     grade-tests-validity
  ;;     ...
  ;;
  ;; So we parse the appearance of tags in the file as matching that structure.
  ;;
  (define (clear-context @t context)
    (cond [(eq? @t    '@problem)      (remove-all '(@problem @htdf @htdd) context)]
          [(member @t '(@htdd @htdf)) (remove-all '(@htdd @htdf) context)]
          [(eq? @t    '@htdw)         (remove-all '(@htdw) context)]))

  (define (remove-all tags context)
    (cond [(null? context) context]
          [else
           (if (and (pair? (car context))
                    (member (caar context) tags))
               (remove-all tags (cdr context))
               (cons (car context)
                     (remove-all tags (cdr context))))]))
           

  (let loop ([lose lose]
	     [context '()])
    (if (null? lose)
	'()
	(let ([sexp (first lose)])
	  (cond [(and (pair? sexp)
		      (member (first sexp) '(@problem @htdw @htdd @htdf)))
		 (let ([new-context (cons sexp (clear-context (first sexp) context))])
		   (cons (elt (cdr new-context) sexp)
			 (loop (cdr lose)
			       new-context)))]
                [(and (pair? sexp)
                      (eqv? (first sexp) '@signature))
                 (cons (elt context (subst 'false #f sexp)) ;!!! make this go away, false is a symbol in this case not a value
                       (loop (cdr lose)
                             context))]
		[else
		 (cons (elt context sexp)
		       (loop (cdr lose)
			     context))])))))



(define problem-sexps tag-sexps)  ;some tags have elements after them
(define htdw-sexps    tag-sexps)
(define htdd-sexps    tag-sexps)
(define htdf-sexps    tag-sexps)

(define problem-num             cadr)    ;data in the actual tag
(define htdw-ws                 cadr)   
(define htdf-names              cdr)    
(define htdd-names              cdr)
(define template-origin-origins cdr)
(define template-defn           cadr)
(define dd-template-rules-rules cdr)

;; !!! the remove '@signature is because sometimes this is called on the tag and
;; !!! sometimes it is called on just the signature. sigh.
(define (signature-args   sig) (takef (remove '@signature sig) (lambda (x) (not (eqv? x '->))))) ;!!!cdr added because sig includes @signature
(define (signature-result sig) (cadr (member '-> sig)))
(define (signature-fail?  sig) (equal? (cddr (member '-> sig)) '(or false)))

(define (problem-htdfs         t) (filter @htdf?              (tag-sexps t)))

(define (htdf-sigs             t) (filter @signature?         (tag-sexps t)))
(define (htdf-checks           t) (filter check?              (tag-sexps t)))
(define (htdf-template-origins t) (filter @template-origin?   (tag-sexps t)))
(define (htdf-templates        t) (filter @template?          (tag-sexps t)))
(define (htdf-defns            t) (filter fn-defn?            (tag-sexps t)))

(define (htdd-constants        t) (filter const-defn?         (tag-sexps t)))
(define (htdd-rules            t) (filter @dd-template-rules? (tag-sexps t)))
(define (htdd-templates        t) (filter fn-defn?            (tag-sexps t)))


(define (@assignment?          x) (and (pair? x) (eq? (car x) '@assignment)))
(define (@cwl?                 x) (and (pair? x) (eq? (car x) '@cwl)))

(define (@problem?             x) (and (pair? x) (eq? (car x) '@problem)))

(define (@htdw?                x) (and (pair? x) (eq? (car x) '@htdw)))
(define (@htdd?                x) (and (pair? x) (eq? (car x) '@htdd)))
(define (@htdf?                x) (and (pair? x) (eq? (car x) '@htdf)))

(define (@signature?           x) (and (pair? x) (eq? (car x) '@signature)))

(define (@dd-template-rules?   x) (and (pair? x) (eq? (car x) '@dd-template-rules)))
(define (@template-origin?     x) (and (pair? x) (eq? (car x) '@template-origin)))
(define (@template?            x) (and (pair? x) (eq? (car x) '@template)))

(define (require?              x) (and (pair? x) (eq?   (car x) 'require)))
(define (check?                x) (and (pair? x) (member (car x) CHECK-FORMS)))
(define (fn-defn?              x) (and (pair? x) (eq? (car x) 'define) (pair? (cadr x)) (pair? (cddr x))))
(define (const-defn?           x) (and (pair? x) (eq? (car x) 'define) (symbol? (cadr x))))
(define (struct-defn?          x) (and (pair? x) (eq? (car x) 'define-struct)))
(define (defn?                 x) (or (fn-defn? x) (const-defn? x) (struct-defn? x)))

(define (check-expect?         x) (and (pair? x) (eq? (car x) 'check-expect)))

(define fn-defn-name       caadr)
(define fn-defn-parameters cdadr)
(define fn-defn-body       caddr)










;; Utility functions




;; used to enforce proper nesting of grader-* forms in autograder files

(define (assert-context--top-level) (assert-context empty?    "at top-level inside grade-submission."))
(define (assert-context--@problem)  (assert-context '@problem "immediately inside grade-problem."))
(define (assert-context--@htdd)     (assert-context '@htdd    "immediately inside grade-htdd."))
(define (assert-context--@htdf)     (assert-context '@htdf    "immediately inside grade-htdf."))

(define (assert-context guard str)
  (unless (if (symbol? guard)
	      (and (pair? (context)) (eqv? (caar (context)) guard))
	      (guard (context)))
    (error "Must be ~a, instead context is ~a." str (context))))

;; !!! moves or goes away
(define (check-bounds n max kind)
  (unless (valid-index (sub1 n) max) (raise-student-error "could not find the ~a ~a" (number->ordinal n) kind)))





(define (display-overall-grade n msg rpt)
  (displayln/f (format "~%~%~%AUTOGRADING GRADE:   ~a    (out of 100)~%~%~a" n msg) rpt))



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


;; CONSTRAINT: file is well-formed with @assignment before @cwl
;; !!! this is slowing down running the grader over the solution bank because it runs through
;; !!! entire files looking for cwl tags that aren't there
(define (find-assgn-cwl-tags fn)
  (with-input-from-file fn
    (lambda ()
      (let loop [(line (read-line))
                 (assgn-tag #f)
                 (cwl-tag   #f)]
        (cond [(and assgn-tag cwl-tag) (values assgn-tag cwl-tag)]
              [(eof-object? line)
               (cond [(not assgn-tag)
                      (raise-argument-error 'find-assgn-cwl-tags
                                            "File is missing @assignment tag" fn)]
                     [(not cwl-tag)
                      ;; The solution files don't have cwl tags, so we let this by.  Note that
                      ;; the precondition above means we shouldn't be in this boat anyways.
                      (values assgn-tag #f)])]
              [(and (not assgn-tag)
                    (regexp-match #rx"^\\(@assignment .*\\)" line))
               (loop (read-line) (read-from-string line) #f)]
              [(and (not cwl-tag)
                    (regexp-match #rx"^\\(@cwl .*\\)" line))
               (values assgn-tag (read-from-string line))]
              [else
               (loop (read-line) assgn-tag cwl-tag)])))))


