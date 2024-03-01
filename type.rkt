#lang racket

(require "defs.rkt"
         spd/constants)

(provide (all-defined-out))

#|

(define Cat  (typedef Cat fn-for-cat String))

(define Pos  (typedef Pos fn-for-pos (compound (Integer Integer) make-pos pos? (pos-x pos-y))))

(define Tree (typedef Tree fn-for-tree (compound (String Tree) make-tree tree? (pos-name pos-subs))))

(define ListOfTree (make-listof-type 'ListOfTree 'fn-for-lot Tree)) ;

(define ListOfTree (typedef ListOfTree fn-for-lot (listof Tree)))

|#

(struct %typedef (name fn-for-t type))

(define typdef?             %typdef)

(define typedef-name        %typdef-name)
(define typedef-fn-for-t    %typedef-fn-for-t)
(define typedef-type        %typedef-type)

(define-syntax (typedef stx)
  (syntax-case stx ()
    [(_ name fn-for-t type) #'(%typedef-type 'name 'fn-for-t type)]))
  


(struct type ()
  #:methods gen:custom-write [(define write-proc (lambda (t p m) (type-print t p m)))])

(struct %atomic-nd type (name))
(struct %atomic-d  type (value))
(struct %one-of    type (subclasses))
(struct %compound  type (field-types constructor predicate selectors))

(struct %sref (type-name fn-for-t))  ;these are not types in and of themselves
(struct  %ref (type-name fn-for-t))  ;they just serve to render explicit what
(struct %mref (type-name fn-for-t))  ;kind of reference the bare name represents




(define (atomic? x)
  (or (atomic-nd? x) (atomic-d? x)))

(define atomic-nd?           %atomic-nd?)
(define atomic-d?            %atomic-d?)
(define one-of?              %one-of?)
(define compound?            %compound?)
(define sref?                %sref?)
(define  ref?                %ref?)
(define mref?                %mref?)

(define atomic-nd-name       %atomic-nd-name)
(define atomic-d-value       %atomic-d-value)
(define one-of-subclasses    %one-of-subclasses)
(define compound-field-types %compound-field-types)
(define compound-constructor %compound-constructor)
(define compound-predicate   %compound-predicate)
(define compound-selectors   %compound-selectors)
(define sref-type-name       %sref-type-name)
(define sref-fn-for-t        %sref-fn-for-t)
(define  ref-type-name       %ref-type-name)
(define  ref-fn-for-t        %ref-fn-for-t)
(define mref-type-name       %mref-type-name)
(define mref-fn-for-t        %mref-fn-for-t)

(define (atomic-nd name)  (%atomic-nd name))
(define (atomic-d  value) (%atomic-d  value))

(define (one-of . subclasses)
  (%one-of (parse-referred-to-types subclasses #t)))

(define-syntax (compound stx)
  (syntax-case stx ()
    [(_ (ft ...) constructor predicate (sel ...))
     #'(%compound (parse-referred-to-types (list ft ...) #f)
                  'constructor 'predicate
                  '(sel ...))]))

(define-syntax (sref stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%sref 'type-name 'fn-for-t)]))
(define-syntax (ref  stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%ref  'type-name 'fn-for-t)]))
(define-syntax (mref stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%mref 'type-name 'fn-for-t)]))

(define (parse-referred-to-types lox allow-bare?)
  (map (lambda (x)
         (cond [(string? x)                       (atomic-d x)]
               [(memq x (list false empty true))  (atomic-d x)]
               [(or (sref? x) (ref? x) (mref? x))           x]
               [(primitive-type? x)                         x]
               [(and allow-bare? (type? x))                 x]
               [(type? x)
                (error* "Bare reference to a type ~s." x)]
               [else
                (error* "Bad one-of subclass or compound field type ~s." x)]))
       lox))


(define (make-listof-type lo-type-name fn-for-lot type-name [fn-for-t (void)])
  (%one-of (list (atomic-d empty)
                 (%compound (list (if (primitive-type? type-name) type-name (%ref type-name fn-for-t))
                                  (%sref lo-type-name fn-for-lot))
                           'cons 'cons?
                           '(first rest)))))

(define (rule-kind t)
  (cond [(atomic-d? t)  'atomic-distinct]
        [(atomic-nd? t) 'atomic-non-distinct]
        [(one-of? t)    'one-of]
        [(compound? t)  'compound]
        [(sref? t)      'self-ref]
        [(ref?  t)      'ref]
        [(mref? t)      'mref]
        [else
         (error* "unrecognized type expr ~a" t)]))

(define (type-print type port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else display #;(lambda (p port) (print p port mode))])])
    (when mode (write-string "<Type " port))
    (recur (type->type-expr type) port)
    (when mode (write-string ">" port))))

(define (type->type-expr t)
  (cond [(atomic-nd? t) (atomic-nd-name t)]
        [(atomic-d?  t) (atomic-d-value t)]
        [(one-of?    t) `(one-of                    ,@(map type->type-expr (one-of-subclasses t)))]
        [(compound?  t) `(,(compound-constructor t) ,@(map type->type-expr (compound-field-types t)))]

        [(sref?      t) `(sref ,(sref-type-name t) ,(sref-fn-for-t t))]
        [(ref?       t) `(ref  ,(ref-type-name  t) ,(ref-fn-for-t  t))]
        [(mref?      t) `(mref ,(mref-type-name t) ,(mref-fn-for-t t))]))



(define Number  (atomic-nd 'Number))
(define Integer (atomic-nd 'Integer))
(define Natural (atomic-nd 'Natural))
(define String  (atomic-nd 'String))
(define 1String (atomic-nd '1String))
(define Boolean (atomic-nd 'Boolean))
(define Image   (atomic-nd 'Image))
(define Color   (atomic-nd 'Color))
(define Scene   (atomic-nd 'Scene))

(define KeyEvent   (one-of))   ;it's either a empty one of or
(define MouseEvent (one-of))   ;an atomic-nd which seems worse

(define PRIMITIVE-TYPES (list Number Integer Natural String 1String Boolean Image Color Scene KeyEvent MouseEvent))

(define (primitive-type? x) (member x PRIMITIVE-TYPES))

(define ListOfNumber  (make-listof-type 'ListOfNumber  'fn-for-lon  Number))
(define ListOfInteger (make-listof-type 'ListOfInteger 'fn-for-loi  Integer))
(define ListOfNatural (make-listof-type 'ListOfNatural 'fn-for-lon  Natural))
(define ListOfString  (make-listof-type 'ListOfString  'fn-for-los  String))
(define ListOf1String (make-listof-type 'ListOf1String 'fn-for-lo1s 1String))
(define ListOfBoolean (make-listof-type 'ListOfBoolean 'fn-for-lob  Boolean))
(define ListOfImage   (make-listof-type 'ListOfImage   'fn-for-loi  Image))
(define ListOfColor   (make-listof-type 'ListOfColor   'fn-for-loc  Color))
(define ListOfScene   (make-listof-type 'ListOfScene   'fn-for-los  Scene))


;; -> raise internal error if type is malformed
;;
;; In terms of static types, types look mutually recursive. But our well-formedness rules are far
;; more restrictive:
;;   one-of   can only contain compound and atomic*
;;   compound can only contain ref* and atomic*
;; nothing else contains anything
;;
(define (ensure-type-is-well-formed type)

  (define (check ty in-one-of? in-compound?)
    (cond [(atomic-nd? ty) #t]
          [(atomic-d?  ty) (or       in-one-of?       in-compound?)] ;allowed in compound for things like (cons "L" Path)
	  [(one-of?    ty) (and (not in-one-of?) (not in-compound?) (check-one-of   ty))]
	  [(compound?  ty) (and                  (not in-compound?) (check-compound ty in-one-of?))]

          [(sref?      ty)                            in-compound?]  ;these three are syntactically leaves
          [(ref?       ty)                            in-compound?]  ;so they enclose nothing
          [(mref?      ty)                            in-compound?]  ;
	  [else
	   (error* "unrecognized type ~a" ty)]))

  (define (check-one-of   ty)            (andmap (lambda (t) (check t #t         #f)) (one-of-subclasses    ty)))
  (define (check-compound ty in-one-of?) (andmap (lambda (t) (check t in-one-of? #t)) (compound-field-types ty)))

  (unless (check type #f #f)
    (error* "Type is not well formed: ~a" type)))
