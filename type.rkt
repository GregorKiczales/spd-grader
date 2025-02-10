#lang racket

(require "defs.rkt"
         spd/constants)

(provide (all-defined-out)
         (except-out (struct-out %type-expr))
         (except-out (struct-out %atomic-nd))
         (except-out (struct-out %atomic-d))
         (except-out (struct-out %one-of))
         (except-out (struct-out %compound))
         (except-out (struct-out %sref))
         (except-out (struct-out %ref))
         (except-out (struct-out %mref)))

;;
;; The underlying types have % names because we use syntax rather than the built-in constructors
;; to construct them. #:constructor-name doesn't solve the problem because struct binds the
;; structure metadata to the structure name.
;;
(struct %type-expr ()
  #:methods gen:custom-write [(define write-proc (lambda (t p m) (type-expr-print t p m)))])

(struct %atomic-nd %type-expr (name))
(struct %atomic-d  %type-expr (value))
(struct %one-of    %type-expr (subclasses))
(struct %compound  %type-expr (field-types constructor predicate selectors))
(struct %sref      %type-expr (type-name fn-for-t))
(struct  %ref      %type-expr (type-name fn-for-t))
(struct %mref      %type-expr (type-name fn-for-t))

;;
;; Now we essentially export what we want, define a few extras, and define constructor functions and
;; constructor macros that make calls to the constructors look better and also ensure we only produce
;; well-formed type exprs.
;;

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

;;
;; Constructors
;;

(define (atomic-nd name)  (%atomic-nd name))
(define (atomic-d  value) (%atomic-d  value))

(define (one-of . subclasses)
  (%one-of (parse-referred-to-types subclasses "one-of subclasses" #f #t #f)));!!! is last one really #f

(define-syntax (compound stx)
  (syntax-case stx ()
    [(_ (ft ...) constructor predicate (sel ...))
     #'(%compound (parse-referred-to-types (list ft ...) "compound field types" #f #f #t)
                  'constructor 'predicate
                  '(sel ...))]))

(define-syntax (sref stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%sref 'type-name 'fn-for-t)]))
(define-syntax (ref  stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%ref  'type-name 'fn-for-t)]))
(define-syntax (mref stx) (syntax-case stx () [(_ type-name fn-for-t) #'(%mref 'type-name 'fn-for-t)]))

;;
;; any kind of reference to a non-primitive named type has to be explicitly
;; inside of a ref/sref/mref
;;
(define (parse-referred-to-types lox who allow-one-of? allow-compound? allow-ref?)
  (map (lambda (x)
         (cond [(or (string? x) (memq x (list false empty true)) (and (number? x) (zero? x)))  (atomic-d x)]
               [(primitive-type? x)                 x]
               
               [(and allow-compound? (compound? x)) x]
               [(and allow-one-of?   (one-of?   x)) x]
               [(and allow-ref?      (or (sref? x) (ref? x) (mref? x))) x]
               
               [else (error* "~a cannot include ~s." who x)]))
       lox))


(define (make-listof-type lo-type-name fn-for-lot-name type [fn-for-t-name (void)])
  (%one-of (list (atomic-d empty)
                 (%compound (list (if (primitive-type? type) type (%ref type fn-for-t-name))
                                  (%sref lo-type-name fn-for-lot-name))
                           'cons 'cons?
                           '(first rest)))))

(define (type-expr-print type port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else display #;(lambda (p port) (print p port mode))])])
    (when mode (write-string "<Type " port))
    (recur (pretty-type-expr type) port)
    (when mode (write-string ">" port))))

(define (pretty-type-expr t)
  (cond [(atomic-nd? t) (atomic-nd-name t)]
        [(atomic-d?  t) (atomic-d-value t)]
        [(one-of?    t) `(one-of                    ,@(map pretty-type-expr (one-of-subclasses t)))]
        [(compound?  t) `(,(compound-constructor t) ,@(map pretty-type-expr (compound-field-types t)))]

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
