#lang racket

(provide (all-defined-out))

;; first two have same definition in handin-server/defs.rkt
(define CURRENT-TERM (build-path (find-system-path 'home-dir) "current-term")) 
(define GRADERS-DIR  (build-path CURRENT-TERM "graders"))

(unless (directory-exists? CURRENT-TERM)
  (error 'config "handin server directory does not exist: ~e" CURRENT-TERM))



(define INTERNAL-DATA-LINE "Internal use only below here:")

(define LINE-LENGTH-LIMIT 80)

(define-for-syntax TOPICS '(other
                            eval-etc
                            dd-template-rules
                            dd-template
                            signature
                            test-validity test-thoroughness 
                            template-origin
                            template
                            template-intact
                            submitted-tests additional-tests))

(define            TOPICS '(other
                            eval-etc
                            dd-template-rules
                            dd-template
                            starter-intact
                            signature
                            test-validity test-thoroughness 
                            template-origin
                            template
                            template-intact
                            submitted-tests additional-tests))

(define EARLY-REPORT-TOPICS '(eval-etc starter-intact signature test-validity test-thoroughness))


;; these affect production of score

(define evaluator (make-parameter (lambda (x) (error "No sandbox evaluator setup yet."))))
(define logger    (make-parameter (lambda (x) (error "No grading logger setup yet."))))
(define elts      (make-parameter #f)) ;(listof Elt) the dom for the submission file
(define context   (make-parameter #f)) ;(listof tag) tag context stack, ie. ((@htdf foo) (@problem 1))

;; these affect displaying of score

(define verbose?  (make-parameter #f)) ;produce verbose or concise reports
(define early?    (make-parameter #f)) ;was this before the resubmit deadline


(define current-value-printer     (make-parameter #f))
;(define list-abbreviation-enabled (make-parameter #f))
(define verbose-error-logging?  (make-parameter #t))


(struct exn:fail:student-error    exn:fail:user ())         ;student errors like missing tags
(struct exn:fail:ensure-violation exn:fail:user ())         ;student errors like changing tests or signatures
(struct exn:fail:eval-error       exn:fail:user (student?)) ;error evaluating teaching language code student? false if grader code


(define (raise-student-error msg . v)
  (raise (exn:fail:student-error  (apply format msg v) (current-continuation-marks))))

(define (error* fmt . args)
  (error (apply format fmt args)))
