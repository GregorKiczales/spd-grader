#lang racket

(require (only-in 2htdp/image image?)
         "defs.rkt"
         spd/constants)

(provide (all-defined-out))

(define    ATOMIC-TYPES '(Number Integer Natural String 1String Boolean Image Color Scene))
(define PRIMITIVE-TYPES (append ATOMIC-TYPES '(KeyEvent MouseEvent)))

;;
;; Type is one of:
;;   "distinct string" empty false true  (symbols not values)
;;   Number Integer Natural String Image Boolean
;;   (one-of t...)
;;   (compound (t...)       ;field types
;;             symbol       ;maker
;;             symbol       ;predicate
;;             (symbol...)) ;selectors
;;   (self-ref fn-for-<type-name>)
;;   (ref fn-for-<type-name>)
;;
;; Also note these constraints
;;
;;  - one-of must be at top-level only
;;  - compound cannot be nested inside compound
;;  - allow distinct type for testing
;;
;; Together these mean that while the type type is recursive, no actual data of the type type can really
;; be arbitrary sized, or for that matter more than 3 deep (one-of -> compound -> atomic*,ref,self-ref).
;;

;; Types

(define (type? x)
  (or (atomic-d?  x)
      (atomic-nd? x)
      (one-of?    x)
      (compound?  x)
      (self-ref?  x)
      (ref?       x)
      (mref?      x)))

(define (atomic?    x) (or (atomic-d? x) (atomic-nd? x)))
(define (atomic-d?  x) (and (or (string? x) (memq x '(true false empty))) #t))
(define (atomic-nd? x) (and (memq x ATOMIC-TYPES) #t))
(define (one-of?    x) (and (list? x) (eqv? (car x) 'one-of)))
(define (compound?  x) (and (list? x) (eqv? (car x) 'compound) (= (length x) 5)))
(define (self-ref?  x) (and (list? x) (eqv? (car x) 'self-ref) (= (length x) 2)))
(define (ref?       x) (and (list? x) (eqv? (car x) 'ref)      (= (length x) 2)))
(define (mref?      x) (and (list? x) (eqv? (car x) 'mref)     (= (length x) 2)))

(define (make-listof-type x fn-for-lox)
  `(one-of empty
           (compound (,x (self-ref ,fn-for-lox)) cons cons? (first rest))))


(define one-of-subclasses  cdr)

(define compound-fts       cadr)
(define compound-maker     caddr)
(define compound-predicate cadddr)
(define compound-selectors (lambda (x) (car (cddddr x))))

(define self-ref-fn        cadr)
(define ref-fn             cadr)
(define mref-fn            cadr)

(define (rule-kind t)
  (cond [(atomic-d? t)  'atomic-distinct]
        [(atomic-nd? t) 'atomic-non-distinct]
        [(one-of? t)    'one-of]
        [(compound? t)  'compound]
        [(self-ref? t)  'self-ref]
        [(ref? t)       'ref]
        [(mref? t)      'mref]        
        [else
         (error* "unrecognized type ~a" t)]))


;; -> raise internal error if type is malformed
;;
;; In terms of static types, types look mutually recursive. But our well-formedness rules are far
;; more restrictive:
;;   one-of   can contain compound and atomic*
;;   compound can contain ref* and atomic*
;; nothing else contains anything
;;
(define (ensure-type-is-well-formed type)
  
  (define (check ty in-one-of? in-compound?)
    (cond [(atomic-d?  ty) (or       in-one-of?       in-compound?)] ;allowed in compound for things like (cons "L" Path)
          [(atomic-nd? ty) #t]          
	  [(one-of?    ty) (and (not in-one-of?) (not in-compound?) (check-one-of   ty))]
	  [(compound?  ty) (and                  (not in-compound?) (check-compound ty in-one-of?))]

          [(self-ref?  ty) (and      in-one-of?       in-compound?)] ; these three are syntactically leaves
          [(ref?       ty)                            in-compound?]  ; so they enclose nothing
          [(mref?      ty)                            in-compound?]  ;
	  [else
	   (error* "unrecognized type ~a" ty)]))
  
  (define (check-one-of   ty)            (andmap (lambda (t) (check t #t         #f)) (one-of-subclasses ty)))
  (define (check-compound ty in-one-of?) (andmap (lambda (t) (check t in-one-of? #t)) (compound-fts      ty)))
  
  (unless (check type #f #f)
    (error* "Type is not well formed: ~a" type)))
