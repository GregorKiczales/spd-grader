#lang racket

(require spd/tags
         spd/walker
         syntax/stx)


;; A simple matcher with pattern variables.  No backtracking, and pvars can only appear in the
;; solution, not the submission.
;;
;; PVARS have global scope within a grade-submission.
;;
;; when do the $names get converted for error messages?
;; does a failed match unwind bindings?

;; (list ($name (void)|val)
;; interp. an environment binding pvars to their value
(define env      (make-parameter #f))

(define (match sub0 sol0)

  (define (match sub sol)
    (cond [(pair? sub)
	   (and (pair? sol)
		(match (car sub) (car sol))
		(match (cdr sub) (cdr sol)))]
	  [(symbol? sub)
	   (and (symbol? sol)
		(or (eqv? sub sol)  ;simple, fast
		    (and (pvar? sol) (match-pvar? sub sol))))]
	  [else
	   (eqv? sub sol)]))

  ;; !!! maybe bindings are never void?
  (define (match-pvar? sub pvar)  ;sol might not be
    (let ([binding (assq pvar (env))])
      (if binding
	  (if (void? (cadr binding))
	      (env (cons (list pvar sub) (remove binding (env) eqv?)))
	      (eqv? sub (cadr binding)))
	  (env (cons (list pvar sub) (env))))))

  (define (pvar? s)
    (and (symbol? s)
	 (char=? (string-ref (symbol->string s) 0) #\$))) ;ugh, this conses a new string every time

  (match sub0 sol0))

#|

(define (ensure-contains tag sol0)  
  (let ([env (make-parameter '())]   ;(($foo val) ...)
	[tag* (cond [(pvar? (cadr tag))
		     (env (cons (list (cadr tag) (void)) (env)))
		     tag)
		    [else tag])])
    
|#

(env '())
(not (match 'foo 'bar))
(match 'foo 'foo)
(match '(foo) '(foo))
(match '(foo) '($V))


(match '(foo bar foo) '($V bar $V))

(not (match '(foo bar bar) '($V bar $V)))


(match '(foo (bar foo) foo) '($V (bar $V) $V))
