#lang racket/base

(require racket/function
         racket/pretty
         racket/string
         racket/list
         racket/port
         racket/struct
         (prefix-in pc: mzlib/pconvert)
         "defs.rkt");!!! this file probably shouldn't be called utils

(provide (all-defined-out))




;; value printed form canonicalization
;; The goal is to keep values canonicalized to their ISL expression form.
;;
;; This has effect in two places:
;;   comparing source s-expressions  (unchanged-starter-elements, use-bia-fn...)
;;   comparing result s-expressions  (in the stepper)
;;
;; Actual value comparisons all happen in the sandbox, so they are properly printed-form agnostic.
;;
;;  #t, #f  -> true, false
;;  '()     -> empty        when it appears in the car of a list (aka appears in value/expression)
;;  structures to (make-foo ...)

;; called in read-elts,  ensure-unchanged-starter-elements
(define (post-read-convert x)
  (cond [(pair? x) (cons (if (equal? (car x) ''())
                             'empty
                             (post-read-convert (car x)))
			 (post-read-convert (cdr x)))]
        [(eqv? x #t)    'true]
	[(eqv? x #f)    'false]
	[(eqv? x 'time) 'time*] ;!!! are these still needed?
	[(eqv? x 'sort) 'sort*] ;!!!
        [else x]))



;; 
;; called to convert values to the teaching language expressions that evaluate to them
;;
(define (print-convert x)
  
  (define (pc-helper v)
    (parameterize ([pc:booleans-as-true/false #t]
                   [pc:abbreviate-cons-as-list #t];!!!(list-abbreviation-enabled)]
                   [pc:constructor-style-printing #t]
                   [pc:add-make-prefix-to-constructor #t])
      (pc:print-convert v)))
  
  (cond [(pair? x) (cons (if (eqv? (car x) ''())
                             'empty
                             (print-convert (car x)))
			 (print-convert (cdr x)))]
        [(struct? x) (let* ([constructor (car (pc-helper x))] ;has one extra field and has converted
                            ;;                                ;nested structs incorrectly, but knows
                            ;;                                ;how to get the constructor name
                            [fields (struct->list x)])
                       (cons constructor
                             (map print-convert
                                  (butlast fields))))]
        [(eqv? x #t)  'true]
	[(eqv? x #f)  'false]
        [else x]))



(define (default-value-printer v)
  (parameterize ([pretty-print-show-inexactness #t]
                 [pretty-print-.-symbol-without-bars #t]
                 [pretty-print-exact-as-decimal #t]
                 [pretty-print-columns 'infinity]
                 [read-case-sensitive #t])
    (let ([p (open-output-string)])
      (regexp-replace #rx"\n$" (get-output-string p) ""))))

(current-value-printer default-value-printer)




(define (line->language-level line)
  (cond [(string-contains? line "htdp-beginner-reader")            '(special intermediate) #;'(special beginner)]   ;so we can define fns.
        [(string-contains? line "htdp-beginner-abbr-reader")       '(special intermediate) #;'(special beginner-abbr)]
        [(string-contains? line "htdp-intermediate-reader")        '(special intermediate)]
        [(string-contains? line "htdp-intermediate-lambda-reader") '(special intermediate-lambda)]
        [(string-contains? line "htdp-advanced-reader")            '(special advanced)]
        [else
	 (error* "~s is not a valid reader line." line)]))



(define (display/f datum [out (current-output-port)] . more)
  (for ([p (cons out more)])
    (display datum out)
    (flush-output out)))

(define (displayln/f datum [out (current-output-port)] . more)
  (for ([p (cons out more)])
    (displayln datum out)
    (flush-output out)))



;; !!! consider changing eqv? to equal? and making it the first clause
(define (subst new old in)
  (cond [(pair? in) (cons (subst new old (car in)) (subst new old (cdr in)))]
        [(eqv? in old) new]
        [else in]))

(define (substf fn x)
  (cond [(pair? x)  (cons (substf fn (car x)) (substf fn (cdr x)))]
        [else (fn x)]))

(define (butlast lst)
  (take lst (sub1 (length lst))))

(define (within? a b delta)
  (< (abs (- a b)) (abs delta)))

(define (round* value decimals)
  (/ (round (* (expt 10 decimals) value)) (expt 10 decimals)))

(define (number->ordinal n)
  (unless (and (not (negative? n)) (integer? n)) (error* "Can't convert the number ~a to an ordinal." n))
  (let ([nstr (number->string n)])
    (cond [(and (= 1 (modulo n 10)) (not (= n 11))) (string-append nstr "st")]
          [(and (= 2 (modulo n 10)) (not (= n 12))) (string-append nstr "nd")]
          [(and (= 3 (modulo n 10)) (not (= n 13))) (string-append nstr "rd")]
          [else (string-append nstr "th")])))

(define (number->ordinal* n)
  (if (= n 1)
      ""
      (format "~a " (number->ordinal n))))

(define (plural n)
  (if (list? n)
      (plural (length n))
      (if (or (= n 0) (> n 1)) "s" "")))

(define (pluralize n str)
  (format "~a ~a~a" n str (plural n)))

(define (is/are n)
  (if (list? n)
      (is/are (length n))
      (if (or (= n 0) (> n 1)) "are" "is")))

(define (naturals->text lon)
  (define (scan lon)
    (cond [(empty? (rest lon)) (format ", and ~a" (car lon))]
          [else (format ", ~a~a" (car lon) (scan (rest lon)))]))

  (cond [(empty? lon) ""]
        [(empty? (rest lon)) (format " ~a" (car lon))]
        [else
         (format "s ~a~a" (car lon) (scan (rest lon)))]))


(define (list->text lox)
  (define (scan lox)
    (cond [(empty? (rest lox)) (format ", and ~a" (car lox))]
          [else (format ", ~a~a" (car lox) (scan (rest lox)))]))

  (cond [(empty? lox) ""]
        [(empty? (cdr  lox)) (format " ~a" (car lox))]
        [(empty? (cddr lox)) (format " ~a and ~a" (car lox) (cadr lox))]
        [else
         (format "~a~a" (car lox) (scan (rest lox)))]))


(define (valid-index index list-length)
  (and (<= 0 index) (< index list-length)))


(define (read-from-string str)
  (with-input-from-string str read))


;; Set utility functions

(define (equal-sets? s1 s2) 
  (and (andmap (curryr member s2) s1)
       (andmap (curryr member s1) s2)
       #t))

(define (set-subset? s1 s2)
  (andmap (curryr member s2) s1))

(define (set-superset? s1 s2)
  (ormap (lambda (o) (not (member o s2))) s1))

(define (contains-duplicates? lst)
  (cond [(empty? lst) #f]
	[else
	 (if (member (first lst) (rest lst))
             #t
	     (contains-duplicates? (rest lst)))]))
