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


;; these affect production of score

(define evaluator (make-parameter #f)) ;the submission evaluator
(define logger    (make-parameter #f)) ;logger for internal errors (goes to staff)
(define elts      (make-parameter #f)) ;(listof Elt) the dom for the submission file
(define context   (make-parameter #f)) ;(listof tag) tag context stack, ie. ((@htdf foo) (@problem 1))

;; these affect displaying of score

(define verbose?  (make-parameter #f)) ;produce verbose or concise reports
(define early?    (make-parameter #f)) ;was this before the resubmit deadline


(define current-value-printer     (make-parameter #f))
;(define list-abbreviation-enabled (make-parameter #f))
(define verbose-error-logging?  (make-parameter #t))

