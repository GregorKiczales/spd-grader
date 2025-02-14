#lang racket/base

(require racket/function
         racket/list
         "defs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(struct elt (tags stx sexp) #:transparent)

(define CHECK-FORMS
  '(check-expect check-random check-satisfied check-within check-error check-member-of check-range))


(define (sexps)     (map elt-sexp (elts))) ;acts like a parameter

(define (get-defns) (htdf-defns (car (context))))
(define (get-defn)  (let ([defns (get-defns)]) (and (pair? defns) (car defns))))

(define (sexp->stx sexp)
  (let loop ([elts (elts)])
    (cond [(null? elts) #f]
          [(equal? (elt-sexp (car elts)) sexp) (elt-stx (car elts))]
          [else
           (loop (cdr elts))])))

(define (get-problem* n)
  (or (get-by-pred (lambda (x) (and (@problem? x) (= n (problem-num x)))))
      (raise-student-error  "Could not find (@problem ~a)." n)))


(define (tag-sexps t)
  (map elt-sexp (filter (compose (curry member t) elt-tags) (elts))))

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
  (let loop ([sexps (if (null? (context)) (sexps) (tag-sexps (car (context))))])
    (cond [(null? sexps) #f]
          [(p (car sexps)) (car sexps)]
          [else
           (loop (cdr sexps))])))

(define (get-by-index p n)
  (let loop ([i 0]                        
	     [sexps (if (null? (context)) (sexps) (tag-sexps (car (context))))])
    (cond [(null? sexps)                 #f]
	  [(and (p (car sexps)) (= i n))  (car sexps)]
	  [     (p (car sexps))           (loop (add1 i) (cdr sexps))]
	  [else                           (loop       i  (cdr sexps))])))

(define (get-all-tests)
  (filter check? (sexps)))

;;
;; The new check-expect swallows bad syntax errors, so we can't assume all
;; the expressions in the check are well-formed. 
(define (get-function-tests fn-name)
  (filter (lambda (t)
	    (and (check? t)
		 (let ([test-actual (cadr t)])
		   (and (pair? test-actual)
			(or (eqv? (car test-actual) fn-name)
			    (and (eqv? (car test-actual) 'local)
                                 (> (length test-actual) 2)
                                 (pair? (caddr test-actual))
				 (eqv? (caaddr test-actual) fn-name)))))))
	  (sexps)))

(define (read-syntaxes fn)
  (with-handlers [(exn:fail? (lambda (e) '()))]
    (call-with-input-file fn
      (lambda (p)
        (parameterize ([read-accept-reader #t]
                       [read-case-sensitive #t]
                       [read-decimal-as-inexact #f] ;to match teaching languages
                       [current-input-port p])
          (port-count-lines! p) ;line numbers will include 3 hidden lines.
          
          (cdr (syntax->list (cadddr (syntax->list (read-syntax fn p))))))))))



(define (parse-elts lo-stx lo-line)
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
           

  (let loop ([lo-stx lo-stx]
	     [context '()])
    (if (null? lo-stx)
	'()
	(let* ([stx (car lo-stx)]
               [sexp (post-read-convert (syntax->datum stx))])
	  (cond [(and (pair? sexp)
		      (member (car sexp) '(@problem @htdw @htdd @htdf)))
		 (let ([new-context (cons sexp (clear-context (car sexp) context))])
		   (cons (elt (cdr new-context) stx sexp)
			 (loop (cdr lo-stx)
			       new-context)))]
                [(and (pair? sexp)
                      (eqv? (car sexp) '@signature))
                 (cons (elt context stx (subst 'false #f sexp)) ;!!! make this go away, false is a symbol in this case not a value
                       (loop (cdr lo-stx)
                             context))]
		[else
		 (cons (elt context stx sexp)
		       (loop (cdr lo-stx)
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
(define (signature-args   sig) (takef (remove '@signature sig) (lambda (x) (not (eqv? x '->)))))
(define (signature-result sig) (cadr (member '-> sig)))
(define (signature-fail?  sig) (equal? (cddr (member '-> sig)) '(or false)))

(define (problem-htdfs         t) (filter @htdf?               (tag-sexps t)))

(define (htdf-sigs             t) (filter @signature?          (tag-sexps t)))
(define (htdf-checks           t) (filter check?               (tag-sexps t)))
(define (htdf-template-origins t) (filter @template-origin?    (tag-sexps t)))
(define (htdf-templates        t) (filter @template?           (tag-sexps t)))
(define (htdf-defns            t) (filter fn-defn?             (tag-sexps t)))

(define (htdd-constants        t) (filter const-defn?          (tag-sexps t)))
(define (htdd-rules            t) (filter @dd-template-rules?  (tag-sexps t)))
(define (htdd-templates        t) (filter fn-defn?             (tag-sexps t)))


(define (@assignment?          x) (and (pair? x) (eq? (car x)  '@assignment)))
(define (@cwl?                 x) (and (pair? x) (eq? (car x)  '@cwl)))

(define (@problem?             x) (and (pair? x) (eq? (car x)  '@problem)))

(define (@htdw?                x) (and (pair? x) (eq? (car x)  '@htdw)))
(define (@htdd?                x) (and (pair? x) (eq? (car x)  '@htdd)))
(define (@htdf?                x) (and (pair? x) (eq? (car x)  '@htdf)))
(define (@htdf-main?           x) (and (pair? x) (equal? x     '(@htdf main))))

(define (@signature?           x) (and (pair? x) (eq? (car x)  '@signature)))

(define (@dd-template-rules?   x) (and (pair? x) (eq? (car x)  '@dd-template-rules)))
(define (@template-origin?     x) (and (pair? x) (eq? (car x)  '@template-origin)))
(define (@template?            x) (and (pair? x) (eq? (car x)  '@template)))

(define (require?              x) (and (pair? x) (eq? (car x) 'require)))
(define (fn-defn?              x) (and (pair? x) (eq? (car x) 'define) (pair? (cadr x)) (pair? (cddr x))))
(define (const-defn?           x) (and (pair? x) (eq? (car x) 'define) (symbol? (cadr x))))
(define (struct-defn?          x) (and (pair? x) (eq? (car x) 'define-struct)))
(define (defn?                 x) (or (fn-defn? x) (const-defn? x) (struct-defn? x)))

(define (check?                x) (and (pair? x) (member (car x) CHECK-FORMS)))
(define (check-expect?         x) (and (pair? x) (eq? (car x) 'check-expect)))
(define (not-check-satisfied?  x) (or (not (pair? x)) (not (eqv? (car x) 'check-satisfied))))
(define (cond-expr?            x) (and (pair? x) (eqv? (car x) 'cond) (pair? (cdr x))))

(define fn-defn-name       caadr)
(define fn-defn-parameters cdadr)
(define fn-defn-body       caddr)

(define const-defn-name       cadr)
(define const-defn-value-expr caddr)

(define (defn-name x)
  (if (fn-defn? x)
      (fn-defn-name x)
      (const-defn-name x)))



