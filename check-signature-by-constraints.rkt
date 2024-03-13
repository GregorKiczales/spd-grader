#lang racket

(require "walker.rkt"
         "grader.rkt"
         "utils.rkt")

(module+ test
  (require rackunit))

(provide grade-signature-by-constraints
         
         check-signature-by-constraints
         make-constraints-from-signature

         ;; !!! does anyone outside here use this?
         ;; !!! maybe move grade-signature to this file, rename to signature
         constraint
         fn-sig
         select
         select*
         same?
         different?)


;; fns is (list (Sig -> X) (X -> Y) ... (W -> Boolean))
;; they are chained to test the constraint against a given signature
(struct constr (explanation fns))

;; provide syntactic sugar for writing (constraint ".." 'String)
(define (constraint explanation . fns)
  (constr explanation
          (map (lambda (f)
                 (cond [(symbol? f) (lambda (x) (eqv? x f))]
                       [else f]))
               fns)))


;; !!! rename this to grade-signature/tvars
(define-syntax (grade-signature-by-constraints stx)
  (syntax-case stx ()
    [(_ n sol)
     #'(recovery-point grade-signature-by-constraints
         (assert-context--@htdf)
	 (let* ([htdf (car (context))]
		[sigs (htdf-sigs htdf)]
                [sig  (and (pair? sigs)
                           (>= (length sigs) n)
                           (list-ref sigs (sub1 n)))])
           (grade-prerequisite 'signature (format "Cannot find the ~a signature." (number->ordinal n)) sig
             (check-signature-by-constraints sig
                                             (make-constraints-from-signature 'sol)))))]))

(define (check-signature-by-constraints sig constraints)
  (header "Abstract function signature:"
          (combine-scores
           (weights* 1 '(*)
             (for/list ([c constraints])
               (rubric-item 'signature (check-constraint (cdr sig) c) "~a" (constr-explanation c)))))))

(define (check-constraint sig c)
  (with-handlers ([void (lambda (e) #f)])
    (foldl (lambda (c rnr) (c rnr)) sig (constr-fns c))))





;; PRECONDITION: sig is well-formed
(define (make-constraints-from-signature sig)
  (cons (constraint (format "Signature has ~a argument types" (length (signature-args sig)))
                    (fn-sig (length (signature-args sig))))
        ;; to make the grader messages more structured, we process the locations in groups
        ;; rather than in the depth-first left-to-right order signature-locations produces
        (let ([locs (signature-locations sig)])
          (append (make-fn-type-constraints        sig (filter (compose function-type? loc-type) locs))
                  (make-list-type-constraints      sig (filter (compose list-type?     loc-type) locs))
                  (make-concrete-type-constraints  sig (filter (compose concrete-type? loc-type) locs))
                  (make-type-parameter-constraints sig (filter (compose tvar?          loc-type) locs))))))

;;
;; path is one of:
;;   '()             entire signature of main function
;;   (args   . <path>)
;;   (result . <path>)
;;   (fail?  . <path>)
;;   (Nat    . <path>)  (list-ref n) of result of <path>
;;
;;   Example paths in  ((X -> Y) (listof X) -> (listof Y))
;;   (0 0 args) first X
;;   (0 1 args) first 'listof
;;   (1 1 args) second X
;;

;; produce a list of every type or tvar location in sig
(define-struct loc (type path)) ;elt is type, path is index number(s)

(define (signature-locations sig)
  (define (walk t path)
    (cond [(list-type? t)
           (cons (make-loc t path)
                 (walk (cadr t) (cons 1 path)))]
          [(function-type? t)
           (append (walk-args (signature-args t) (cons 'args path))
                   (append (walk (signature-result t) (cons 'result path))
                           (if (signature-fail? t)
                               (list (make-loc #t (cons 'fail? path)))
                               '())))]
          [else
           (list (make-loc t path))]))

  (define (walk-args t path)
    (foldr append '()
           (for/list ([t2 t]
                      [i (in-naturals 0)])
             (walk t2 (cons i path)))))
  
  (walk sig '()))


(define (make-fn-type-constraints sig locs)
  (for/list ([loc locs])
    (let ([type (loc-type loc)]
          [path (loc-path loc)])
      (constraint (format "~a is a function-type with ~a arguments"
                          (desc-from-path sig path)
                          (length (signature-args type)))
                  (lambda (s2) (run-path path s2))
                  (fn-sig (length (signature-args type)))))))

(define (make-list-type-constraints sig locs)
  (for/list ([loc locs])
    (let ([type (loc-type loc)]
          [path (loc-path loc)])
      (constraint (format "~a is a (listof <type>) type" (desc-from-path sig path))
                  (lambda (s2) (run-path path s2))
                  list-type?))))                  

(define (make-concrete-type-constraints sig locs)
  (for/list ([loc locs])
    (let ([type (loc-type loc)]
          [path (loc-path loc)])
    (constraint (format "~a has type ~a" (desc-from-path sig path) type)
                (lambda (s2) (run-path path s2))
                (lambda (x) (eqv? x type))))))

(define (make-type-parameter-constraints sig locs)
  (let ([tvars (remove-duplicates (map loc-type locs))])
    (append (for/list ([tvar tvars]
                       [n (in-naturals 1)])
              (constraint (format "Has ~a distinct type parameter (~a in our solution)"
                                  (number->ordinal n)
                                  tvar)
                          (lambda (sig)
                            (>= (length (collect-unique-tvars sig)) n))))
            (for/list ([tvar tvars])
              (let* ([tvar-locs (filter (lambda (loc) (eqv? (loc-type loc) tvar)) locs)]
                     [tvar-paths (map loc-path tvar-locs)])
                (if (null? (cdr tvar-locs)) ;it's a type param that only appears once
                    (constraint (format "~a is a type parameter (~A in our solution)"
                                        (desc-from-path sig (car tvar-paths))
                                        tvar)
                                (lambda (s2)
                                  (run-path (car tvar-paths) s2))
                                tvar?)
                    (constraint (format "~a - all have same type (~A in our solution)"
                                        (list->text (map (lambda (p) (desc-from-path sig p)) tvar-paths))
                                        tvar)
                                (lambda (s2)
                                  (map (lambda (path) (run-path path s2)) tvar-paths))
                                same?)))))))



(define (collect-unique-tvars sig0)
  
  (define (walk sig)
    (cond [(empty? sig) '()]
          [(pair? sig) (append (walk (car sig)) (walk (cdr sig)))]
          [(tvar? sig) (list sig)]
          [else '()]))

  (remove-duplicates (walk sig0)))


(define (run-path path sig)
  (foldr (lambda (p rnr)
           (case p
             [(args)   (signature-args  rnr)]
             [(result) (signature-result rnr)]
             [(fail?)  (signature-fail?  rnr)]
             [else
              (list-ref rnr p)]))
         sig
         path))


(define (desc-from-path sig path0)

  (define (walk path)
    (cond [(empty? path) "overall signature"]
          [else
           (let ([p (car path)]
                 [r (cdr path)])
             (case p
               [(args)   (format "argument of ~a" (walk r))]
               [(result) (format "result of ~a"   (walk r))]
               [(fail?)  (format "or fail? of ~a" (walk r))]
               [else     (format "~a ~a" (number->ordinal (add1 p)) (walk r))]))]))

  (walk path0)
  #;
  (let* ([nfuns (length (filter pair? (signature-args sol)))]
         [fun-params (build-list nfuns
                                 (lambda (n)
                                   (string->symbol (format "c~a" (add1 n)))))])
    (define (convert-top p nargs)
      (case p
        [(args)   "args"]
        [(result) "result"]
        [(fail?)  "or fail?"]
        [else
         (if (< p nfuns)
             (list-ref fun-params p)
             (convert-sub p nargs))]))

    (define (convert-sub p nargs)
      (case p
        [(args)   "args"]
        [(result) "result"]
        [(fail?)  "or fail?"]
        [else
         (if (< p nargs)
             (format "~a argument" (number->ordinal (add1 p)))
             "result")]))

    (cond [(symbol? path)     (convert-top      path  (length (signature-args sol)))]
          [(null? (cdr path)) (convert-top (car path) (length (signature-args sol)))]
          [else
           (format "~a of ~a"
                   (convert-sub (car  path) (length (signature-args (list-ref sol (cadr path)))))
                   (convert-top (cadr path) (length (signature-args sol))))])))


(define (function-type? x)
  (and (list? x)
       (memq '-> x)))

(define (list-type? x)
  (and (list? x)
       (= (length x) 2)
       (eqv? (car x) 'listof)))

(define (tvar? x)
  (and (symbol? x)
       (let [(str (symbol->string x))]
         (and (= (string-length str) 1)
              (string=? str (string-upcase str))))))

(define (concrete-type? x)
  (and (not (function-type? x))
       (not (list-type? x))
       (not (tvar? x))))




;; constraint functions
(define (same? lox)
  (or (empty? lox)
      (andmap (lambda (x2) (eqv? x2 (car lox))) (cdr lox))))

(define (different? lox)
  (cond [(empty? lox) true]
        [else
         (and (not (member (car lox) (cdr lox)))
              (different? (cdr lox)))]))

;; Nat -> (sig -> Boolean)
(define (fn-sig n)
  (lambda (s) 
    (and (list? s)
         (>= (length s) (+ n 2)) 
         (eqv? (car (drop s n)) '->))))



;; Sig Exp -> (Sig -> (listof SExp))
;; produces function to find parts of exp that pattern and sel identify; fails softly
(define (select* sel pattern)
  (local [(define (traverse exp [pat pattern])            
            (cond [(empty? pat) '()]                            ;out of pattern means done
                  [(eqv? pat sel) (list exp)]                   ;pat=selector collect exp
                  [(and (pair? pat) (pair? exp))                ;recursion match -> recurse
                   (append (traverse (car exp) (car pat))
                           (traverse (cdr exp) (cdr pat)))]
                  [(pair? pat) '()]
                  [else '()]))]
    traverse))

;; just produces the first one
(define (select sel pattern)
  (lambda (exp)    
    (let ([lox ((select* sel pattern) exp)])
      (if (empty? lox)
          'FAIL
          (car lox)))))
       



(module+ test

  (define (check-constraints sig loc)
    (andmap (lambda (c) (check-constraint sig c)) loc))
  
  ;; contrary to the recipe, start with an all encompassing test
  (check-true
   (check-constraints  '((String Integer A -> B) (B A -> A) (listof A) Arbre -> B)
                       (make-constraints-from-signature '((String Integer X -> Y) (Y X -> X) (listof X) Arbre -> Y))))
  
  
  
  (check-true (check-constraints '(Natural String -> Image)
                                 (make-constraints-from-signature '(Natural String -> Image))))
  
  (check-true (check-constraints '(Natural Y -> Image)
                                 (make-constraints-from-signature '(Natural X -> Image))))
  
  (check-true (check-constraints '((Natural -> Natural) Y -> X)
                                 (make-constraints-from-signature '((Natural -> Natural) Y -> X))))
  
  
  (check-true (check-constraints '((Natural -> String) -> Boolean)
                                 (make-constraints-from-signature '((Natural -> String) -> Boolean))))
  
  (check-true (check-constraints '((Natural -> Natural) Y -> X)
                                 (make-constraints-from-signature '((Natural -> Natural) Y -> X))))
  
  (check-true (check-constraints '((Natural -> Natural) Y -> X)
                                 (make-constraints-from-signature '((Natural -> Natural) Y -> X))))
  
  
  (check-true (check-constraints '((Natural -> Natural) (listof X) -> (listof Y))
                                 (make-constraints-from-signature '((Natural -> Natural) (listof A) -> (listof B)))))
  
  
  
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "signature should have 4 args" 
                                 (fn-sig 4))))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "first arg is a function of 3 arguments"
                                 (select 'A '(A B C D -> R))
                                 (fn-sig 3))))
  
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "second arg is a function of 2 arguments"
                                 (select 'B '(A B C D -> R))
                                 (fn-sig 2))))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "fourth arg should be Arbre"
                                 (select 'D '(A B C D -> R))
                                 'Arbre)))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "first arg of c1 should be String"
                                 (select 'I '((I J K -> L) B C D -> R))
                                 'String)))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "second arg of c1 should be Integer"
                                 (select 'J '((I J K -> L) B C D -> R))
                                 'Integer)))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "result of c1, first arg to c2, and result of fold-arbre must be the same"
                                 (select* 'X '((I J K -> X) (X M -> N) C D -> X))
                                 same?)))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "third arg to c1, second arg to c2, result of c2, and b1 must be the same"
                                 (select* 'X '((String Integer X -> Y) (Y X -> X) X Arbre -> Y))
                                 same?)))
  
  (check-true
   (check-constraint '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)
                     (constraint "X and Y type parameters must be different"
                                 (lambda (x)
                                   (let [(Xs ((select* 'X '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)) x))
                                         (Ys ((select* 'Y '((String Integer X -> Y) (Y X -> X) X Arbre -> Y)) x))]
                                     (and (not (empty? Xs))
                                          (not (empty? Ys))
                                          (same? Xs)
                                          (same? Ys)
                                          (not (eqv? (car Xs) (car Ys))))))))))
