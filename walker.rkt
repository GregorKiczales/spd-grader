#lang racket

(require racket/list
         spd-grader/utils)

(module+ test
  (require rackunit))

(provide (all-defined-out))

(define TEMPLATE-VARIABLES '(.. ... .... ..... ......))


(define BIA-FNS '(foldr foldl map filter andmap ormap build-list))

(define BB-OPTIONS '(on-tick on-key on-key on-release on-mouse to-draw stop-when))

;; Kind is one of:
;;  - 'value
;;  - 'constant
;;  - 'null
;;  - 'bound
;;  - 'free
;;  - 'if
;;  - 'cond
;;  - 'and
;;  - 'or
;;  - 'define
;;  - 'local
;;  - 'local-define
;;  - 'local-body
;;  - 'lambda
;;  - 'call
(define-for-syntax KINDS
  '(value constant null bound free if cond and or define local local-define local-body lambda call))

;; Context (ctx) is one of:
;;  - 'tail         the expression is in tail position
;;  - 'question     the expression is in question position
;;  - 'other
;;  - (void)        when called with a definition

;; Env is (listof (cons/c symbol syntax))

(define-syntax (walker-case stx) ;used in grade-problem 
  (syntax-case stx ()
    [(_ expr [(k ...) result ...] ...)
     (let ([kinds-used (foldr append '() (map car (cddr (syntax->datum stx))))])
       (and (andmap (lambda (used) (member used KINDS)) kinds-used)
            (andmap (lambda (kind) (member kind kinds-used)) KINDS)))
     #'(case expr
         [(k ...) result ...]
         ...)]))

;; note that p cannot change during the walk
;; p is called with a fn that controls
;;    - whether to recurse
;;    - perhaps with parameterization
;;    - what value to produce
;;    - but not what to recurse on, for that, must call walk-stx
(define (walk-form stx0 env0 p [combine append])
  (define (walk-stx stx ctx env in-fn)
    (let ([d (syntax->datum stx)]
          [e (syntax-e stx)])
      (cond [(value? d)    (p 'value    stx d ctx env in-fn (lambda () d))] ;needs to be datum for lists
            [(constant? e) (p 'constant stx e ctx env in-fn (lambda () e))]
            [(null? e)     (p 'null     stx e ctx env in-fn (lambda () e))] ;!!! does this ever happen?
            [(symbol? e)
             (if (assq e env)
                 (p 'bound stx e ctx env in-fn (lambda () e))
                 (p 'free  stx e ctx env in-fn (lambda () e)))]
            [else
             ((walker-for e) stx e ctx env in-fn)])))
  
  (define (walker-for e)
    (case (syntax-e (car e))
      [(if)       walk-if]
      [(cond)     walk-cond]
      [(and or)   walk-and/or]
      [(define)   walk-define]
      [(local)    walk-local]
      [(lambda λ) walk-lambda]
      ;[(quote)  walk-quote]
      [else       walk-call]))
  
  (define (walk-if stx e ctx env in-fn)    
    (p 'if stx e ctx env in-fn
       (lambda () 
         (combine (walk-stx (cadr   e) 'question env in-fn)
                  (walk-stx (caddr  e) ctx       env in-fn)
                  (walk-stx (cadddr e) ctx       env in-fn)))))
  
  
  (define (walk-cond stx e ctx env in-fn) ; !!! not tested in check-expects
    (p 'cond stx e ctx env in-fn
       (lambda ()
         (for/fold ([result '()])
                   ([qa-stx (cdr e)])
           (let ([qa-e (syntax-e qa-stx)])
             (combine result
                      (walk-stx (car  qa-e) 'question env in-fn)
                      (walk-stx (cadr qa-e) ctx       env in-fn)))))))
  
  
  (define (walk-and/or stx e ctx env in-fn) ; !!! not tested in check-expects
    (p (syntax->datum (car e)) stx e ctx env in-fn
       (lambda ()
         (for/fold ([result '()])
                   ([rand-stx (cdr e)])
           (combine result
                    (walk-stx rand-stx 'other env in-fn))))))
  
  ;; this just builds up the env then calls walk-define-internal
  (define (walk-define stx e ctx env in-fn)
    (p 'define stx e ctx env in-fn
       (lambda ()
         (let ([header (syntax->datum (cadr e))])
           (if (pair? header)
               (walk-define-internal stx e 
                                     (cons (cons (car header) stx) 
                                           (append (for/list ([p (cdr (syntax-e (cadr e)))])
                                                     (cons p (cadr e)))
                                                   env))
                                     in-fn #f)
               (walk-define-internal stx e
                                     (cons (cons header stx) env)
                                     in-fn #f))))))
  
  ;; also just builds up the env then calls walk-define-internal
  (define (walk-local stx e ctx env in-fn)
    (p 'local stx e ctx env in-fn
       (lambda ()
         (let* ([defns (filter (lambda (x)
                                 (member (syntax-e (car (syntax-e x))) '(define define-struct)))
                               (syntax-e (cadr e)))]
                [body  (caddr e)]
                [ids (map (lambda (d)
                            (let* ([de (syntax-e d)]
                                   [dh (syntax-e (cadr de))])
                              (if (pair? dh)
                                  (syntax->datum (car dh))
                                  dh)))
                          defns)]
                [nenv (append (map cons ids defns) env)])
           (combine (for/fold ([result '()])
                              ([defn defns])
                      (combine result
                               (p 'local-define defn (syntax-e defn) (void) nenv in-fn
                                  (lambda ()
                                    (walk-define-internal defn (syntax-e defn) nenv in-fn #t)))))
                    (p 'local-body body (syntax-e body) ctx nenv in-fn
                       (lambda ()
                         (walk-stx body ctx nenv in-fn))))))))
  
  ;; env is already built up by walk-define/walk-local
  ;; dispatch to -fn-body or -stx and update in-fn
  (define (walk-define-internal stx e nenv in-fn in-local?)
    (let ([header (syntax->datum (cadr e))])
      (if (pair? header)
          (walk-fn-body (caddr e) nenv stx)
          (walk-stx (caddr e) (if in-local? 'other 'tail) nenv stx))))
  
  (define (walk-lambda stx e ctx env in-fn) ; !!! not tested in check-expects
    (p 'lambda stx e ctx env in-fn
       (lambda ()
         (walk-fn-body (caddr e)
                       (append (map cons (syntax->datum (cadr e)) (syntax-e (cadr e))) env)
                       stx))))
  
  (define (walk-fn-body stx env in-fn)
    (walk-stx stx 'tail env in-fn))
  
  (define (walk-call stx e ctx env in-fn)
    (p 'call stx e ctx env in-fn
       (lambda ()
         (combine (walk-stx (car e) 'other env in-fn)
                  (for/fold ([result '()])
                            ([rand (cdr e)])
                    (combine result
                             (walk-stx rand 'other env in-fn)))))))
  
  (walk-stx stx0 'tail env0 #f))



;; (listof syntax) -> (listof (cons/c symbol syntax))
(define (generate-env stxs)
  (define (generate-env/one stx env)
    (match (syntax->datum stx)
      [(list 'define-struct (? symbol? id) (? (λ (x) (and (pair? x) (list? x) (andmap symbol? x))) fields))
       (append (list* (string->symbol (string-append "make-" (symbol->string id))) ; constructor
                      (string->symbol (string-append (symbol->string id) "?")) ; type predicate
                      (map (compose string->symbol (curry string-append (symbol->string id) "-") symbol->string) fields)) ; selectors
               env)]
      [(list define (? symbol? id) value) (cons (cons id stx) env)]
      [(list define (cons (? symbol? id) args) body) (cons (cons id stx) env)]
      [else env]))
  
  (let loop ([env '()]
             [stxs stxs])
    (cond [(null? stxs) env]
          [else (loop (generate-env/one (car stxs) env) (cdr stxs))])))

(define (file->syntaxes filename)
  (cdr (syntax-e (cadddr (syntax-e (parameterize ([read-accept-reader #t])
                                     (with-input-from-file filename
                                       read-syntax
                                       #:mode 'text)))))))

;; path (syntax stx-e Kind (listof (or/c Context)) (listof (cons/c symbol syntax)) (or/c #f syntax) -> any) -> void
(define (walk-file filename p)
  (let* ([file-stxs (file->syntaxes filename)]
         [env (generate-env file-stxs)])
    (displayln env)
    (for/fold ([result '()])
              [(stx file-stxs)]
      (append result
              (walk-form stx env p)))))



;; sexp? -> (listof/c symbol?)
(define (free  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn-defn recur)
               (walker-case kind
                 [(free) (list e)]
                 [(value constant null bound #;free) '()]
                 [(if cond and or define local local-define local-body lambda call) (recur)]))))

(define (values f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn-defn recur)
               (walker-case kind
                 [(value) (list e)]
                 [(#;value constant null bound free) '()]
                 [(if cond and or define local local-define local-body lambda call) (recur)]))))



;; !!! replace with new version
(define (calls  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn-defn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(call) 
                  (cons (list in-fn-defn (syntax->datum (car e)) ctx stx)
                        (recur))]
                 [(if cond and or define local local-define local-body lambda) (recur)]))))


(define (called-fn-names f0) (map call-called-fn-name (calls2 f0)))


(struct call (stx called-fn-name ctx called-fn-defn in-fn-defn))
(struct call-graph (fn-defns call-stxs call-ctxs))

;; -> (listof call)
(define (calls2  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn-defn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(call) 
                  (cons (call stx 
                              (syntax->datum (car e))
                              ctx
                              (and (assq (syntax->datum (car e)) env)
                                   (cdr (assq (syntax->datum (car e)) env)))
                              in-fn-defn)
                        (recur))]
                 [(if cond and or define local local-define local-body lambda #;call) (recur)]))))

(define (calls?  f0 name)
  (calls-all? f0 (list name)))

(define (calls-all?  f0 names)
  (let/cc return 
    (walk-form (datum->syntax #f f0)
               '()
               (lambda (kind stx e ctx env in-fn-defn recur)
                 (walker-case kind
                   [(value constant null bound free) '()]
                   [(call)
                    (let ([name (syntax->datum (car e))])
                      (if (memq name names)
                          (begin (set! names (remove name names))
                                 (if (null? names)
                                     (return #t)
                                     (recur)))
                          (recur)))]
                   [(if cond and or define local local-define local-body lambda #;call) (recur)])))
    #f))

(define (calls-any?  f0 names)
  (let/cc return 
    (walk-form (datum->syntax #f f0)
               '()
               (lambda (kind stx e ctx env in-fn-defn recur)
                 (walker-case kind
                   [(value constant null bound free) '()]
                   [(call)
                    (let ([name (syntax->datum (car e))])
                      (if (memq name names)
                          (return #t)
                          (recur)))]
                   [(if cond and or define local local-define local-body lambda #;call) (recur)])))
    #f))

(define (calls-none?  f0 names)
  (let/cc return 
    (walk-form (datum->syntax #f f0)
               '()
               (lambda (kind stx e ctx env in-fn-defn recur)
                 (walker-case kind
                   [(value constant null bound free) '()]
                   [(call)
                    (let ([name (syntax->datum (car e))])
                      (if (memq name names)
                          (return #f)
                          (recur)))]
                   [(if cond and or define local local-define local-body lambda #;call) (recur)])))
    #t))

(define (ncalls  f0 name)
  (let ([n 0])
    (walk-form (datum->syntax #f f0)
               '()
               (lambda (kind stx e ctx env in-fn-defn recur)
                 (walker-case kind
                   [(value constant null bound free) '()]
                   [(call)
                    (when (eq? (syntax->datum (car e)) name)
                      (set! n (add1 n)))
                    (recur)]
                   [(if cond and or define local local-define local-body lambda #;call) (recur)])))
    n))

;; 
;; should be called on top-level define
;; doesn't get cycles involving first-class functions
;;
(define (recursive? defn)
  ;; env            is (listof (cons symbol stx))
  ;; calls          is (listof (list caller called))
  ;; callers        is (listof stx)
  ;; callers->calls is (listof (cons stx stx))
  (let ([calls 
         (walk-form (datum->syntax #f defn)
                    '()
                    (lambda (kind stx e ctx env in-fn-defn recur)
                      (walker-case kind
                        [(value constant null bound free) '()]
                        [(call) 
                         (cons (cons in-fn-defn
                                     (let ([pair (assoc (syntax->datum (car e)) env)])
                                       (if pair
                                           (cdr pair)
                                           (syntax->datum (car e)))))
                               (recur))]
                        [(if cond and or define local local-define local-body lambda #;call) (recur)])))])
    
    (let* ([callers (remove-duplicates (map car calls))]
           [callers->calls (map (lambda (caller)
                                  (cons caller
                                        (map cdr
                                             (filter (lambda (call)
                                                       (eqv? (car call) caller))
                                                     calls))))
                                callers)])
      
      (define (calls-from defn)
        (cond [(assoc defn callers->calls) => cdr]
              [else '()]))
      
      (define (recursive? caller callee)
        
        (define (find/defn? d path)
          (cond [(eqv? d caller) #t]
                [(member d path) #f]
                [else
                 (find/lodfn? (calls-from d) (cons d path))]))
        
        (define (find/lodfn? lod path)
          (ormap (curryr find/defn? path) lod))
        
        (find/defn? callee '()))
      
      (ormap (lambda (call) (recursive? (car call) (cdr call))) calls))))


(define (free-biafs f0 [bia-fns BIA-FNS])
  (filter (lambda (id) (member id bia-fns)) (free f0)))

(define (constants  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (walker-case kind
                 [(value constant null) (list e)]
                 [(bound free) '()]
                 [(if cond and or define local local-define local-body lambda call) (recur)]))))


(define (conditional-expressions f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(define local local-define local-body lambda call) (recur)]
                 [(if cond and or) (cons (list kind stx) (recur))]))))
  

;; !!! all these should produce syntax
(define (...s  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (if (memq e TEMPLATE-VARIABLES)
                   (list e)
                   (walker-case kind
                     [(value constant null bound free) '()]
                     [(if cond and or define local local-define local-body lambda call) (recur)])))))

(define (filled? defn)
  (null? (...s defn)))



;; sexp? -> (listof/c symbol?)
(define (defines  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(define local-define) (cons (syntax->datum stx) (recur))] ;!!! all these functions should produce syntax
                 [(if cond and or #;define local #;local-define local-body lambda call) (recur)]))))

(define (internal-fns  f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (walker-case kind
                 [(value constant null bound free) '()]
                 [(local-define lambda) (cons (syntax->datum stx) (recur))] ;!!! all these functions should produce syntax
                 [(if cond and or define local #;local-define local-body #;lambda call) (recur)]))))


(define (cond/empty?/zero? defn params-to-check)
  (let ([fn-name (car  (cadr defn))])
    
    (define (cond-question exp)
      (and (cdr exp)
           (list? (cadr exp))
           (caadr exp)))
    
    (define (if-question exp)
      (and (cdr exp)
           (cadr exp)))
    
    (define (bad-question? exp)
      (and (list? exp)
           (member (car exp) '(empty? zero?))
           (or (equal? params-to-check '(*))
               (member (cadr exp) params-to-check))))
    
    (let/cc return
      (walk-form (datum->syntax #f defn)
                 '()
                 (lambda (kind stx e ctx env in-fn recur)
                   (walker-case kind
                     [(value constant null bound free) '()]
                     [(if)   (if (bad-question? (if-question   (syntax->datum stx))) (return #t) (recur))]
                     [(cond) (if (bad-question? (cond-question (syntax->datum stx))) (return #t) (recur))]
                     [(#;if #;cond and or define local local-define local-body lambda call) (recur)])))
      #f)))

(define (big-bang-options f0)
  (walk-form (datum->syntax #f f0)
             '()
             (lambda (kind stx e ctx env in-fn recur)
               (if (member e BB-OPTIONS)
                   (list e)
                   (walker-case kind
                     [(value constant null bound free) '()]
                     [(if cond and or define local local-define local-body lambda call) (recur)])))))

(define (contains-exp? exp f0)
  (let/cc return
    (walk-form (datum->syntax #f f0)
               '()
               (lambda (kind stx e ctx env in-fn recur)
                 (if (equal? (syntax->datum stx) exp)
                     (return #t)
                     (walker-case kind
                       [(value constant null bound free) '()]
                       [(if cond and or define local local-define local-body lambda call) (recur)]))))
    #f))

(define (has-try-catch? f0)
  (with-handlers ([void (lambda () #f)])
    (let/cc return
      (walk-form (datum->syntax #f f0)
                 '()
                 (lambda (kind stx e ctx env in-fn recur)
                   (if (try-catch? e)
                       (return #t)
                       (walker-case kind
                         [(value constant null bound free) '()]
                         [(if cond and or define local local-define local-body lambda call) (recur)]))))
      #f)))

(define (try-catch? e)
  (and (list? e)
       (= (length e) 4)
       (eqv? (syntax-e (car e)) 'if)
       (let [(q (syntax->datum (cadr e)))
             (a (syntax->datum (caddr e)))]
         (and (list? q)                   ;(not (false? x))
              (= (length q) 2)
              (eqv? (car q) 'not)
              (list? (cadr q))            ;(false? x)
              (= (length (cadr q)) 2)
              (eqv? (caadr q) 'false?)
              (contains-exp? (cadadr q) a)))))
  

#;#;
;; sexp? -> boolean?
(define (value? f)
  (or (number? f)
      (string? f)
      ;(image? f)    ;we don't allow images in code anymore
      (boolean? f)
      (char? f)))

;; sexp? -> boolean?
(define (constant? f)
  (member f '(true false empty empty-image "")))


(define (->boolean x)
  (if x #t #f))

(module+ test
  
  (check-equal? (free '(+ 1 APPLES)) '(+ APPLES))
  
  (check-equal? (free '(local [(define APPLES 2)] (+ 1 APPLES))) '(+))
  
  (check-equal? (free '(if (foo x) (bar y) (baz z)))
                '(foo x bar y baz z))

  
  
  (check-equal? (values '(if (foo 1) (bar 2) (baz "foo" #t true #f false (cons 1 (cons 2 empty)))))
                '(1 2 "foo" #t true #f false (cons 1 (cons 2 empty))))
  
  
  (check-equal? (defines '(define x 1)) '((define x 1)))
  
  (check-equal? (defines '(define (foo x) 1)) '((define (foo x) 1)))
  
  (check-equal? (defines '(define (foo x)
                            (local [(define bar 1)
                                    (define (baz x) x)]
                              (baz bar))))
                '((define (foo x)
                    (local [(define bar 1)
                            (define (baz x) x)]
                      (baz bar)))
                  (define bar 1)
                  (define (baz x) x)))
  
  
  (check-true (not (recursive? '(define (foo x) 1))))
  
  (check-true      (recursive? '(define (foo x) (foo x))))
  
  (check-true      (recursive? '(define (foo x)
                                  (local [(define bar 1)
                                          (define (baz x) (foo x))]
                                    (baz bar)))))
  
  
  
  (check-equal? (map (curryr take 3) (calls '(foo (bar 1 empty #f))))
                '((#f foo tail) (#f bar other)))
  
  (check-true (let* ([foo-defn  #`(define (foo x) (foo (bar x)))]
                     [foo-call (caddr (syntax-e foo-defn))]
                     [bar-call (cadr (syntax-e foo-call))])
                
                (equal? (calls foo-defn)
                        `((,foo-defn foo tail ,foo-call)
                          (,foo-defn bar other ,bar-call)))))
  
  
  (check-true (let* ([top-defn #'(define (top a b c)
                                   (local [(define x (foo (bar a)))]
                                     (do (dont x))))]
                     [local-expr (caddr (syntax-e top-defn))]
                     [foo-defn (car (syntax-e (cadr (syntax-e local-expr))))]
                     [do-call   (caddr (syntax-e local-expr))]
                     [dont-call (cadr  (syntax-e do-call))]
                     [foo-call  (caddr (syntax-e foo-defn))]
                     [bar-call  (cadr  (syntax-e foo-call))]
                     
                     [corr
                      `((,foo-defn foo  other ,foo-call)
                        (,foo-defn bar  other ,bar-call)
                        (,top-defn do   tail  ,do-call)
                        (,top-defn dont other ,dont-call))])
                
                (equal? (calls top-defn) corr)))
  
  
  
  (check-true (let* ([top-defn #'(define (top a b c) 
                                   (local [(define (foo x)
                                             (list (top x x x)
                                                   (bar x)))
                                           (define (bar y)
                                             (list (top y y y)
                                                   (foo y)))]
                                     (foo a)))]
                     [local-expr (caddr (syntax-e top-defn))]
                     [foo-defn (car (syntax-e (cadr (syntax-e local-expr))))]
                     [bar-defn (cadr (syntax-e (cadr (syntax-e local-expr))))]
                     [called (calls top-defn)])
                
                (and (equal? (map car    called) (list foo-defn foo-defn foo-defn bar-defn bar-defn bar-defn top-defn))
                     (equal? (map cadr   called) '(    list     top      bar      list     top      foo      foo))
                     (equal? (map caddr  called) '(    tail     other    other    tail     other    other    tail))
                     (equal? (map syntax->datum
                                  (map cadddr called))
                             '((list (top x x x) (bar x))
                               (top x x x)
                               (bar x)                             
                               (list (top y y y) (foo y))
                               (top y y y)
                               (foo y)
                               (foo a))))))
  
  #|
  !!! make calls produce structs, with the definition being called
  !!! use that to build a graph
  !!! then can do use-abstract-fn
  
  |#
  
  (check-equal? (recursive? (syntax (define (foo los)
                                      (foo los))))
                #t)
  
  
  (check-equal? (recursive? (syntax (define (foo los)
                                      (cons [(empty? los) empty]
                                            [else (cons (car los)
                                                        (foo (cdr los)))]))))
                #t)

  (check-equal? (recursive? #'(define (rectangles n w h c)
                                (local [(define dw (- w (/ (- h w) (sub1 n))))
                                        (define dh (+ h (/ (- w h) (sub1 n))))]
                                  (foldr overlay empty-image
                                         (build-list n (lambda (x)
                                                         (rectangles x dw dh c)))))))
                #t)

  (check-equal? (recursive? #'(define (rectangles n w h c)
                                (local [(define dw (- w (/ (- h w) (sub1 n))))
                                        (define dh (+ h (/ (- w h) (sub1 n))))
                                        (define (rect x) (rectangles x dw dh c))]
                                  (foldr overlay empty-image
                                         (build-list n rect)))))
                #t)

  (check-equal? (recursive? #'(define (rectangles n w h c)
                                (local [(define dw (- w (/ (- h w) (sub1 n))))
                                        (define dh (+ h (/ (- w h) (sub1 n))))
                                        (define (rect x) (rectangles x dw dh c))]
                                  (rect 1))))
                #t)

  
  
  (check-equal? (recursive? (syntax (define (foo los)
                                      (local [(define (bar x)
                                                (foo x))]
                                        (bar los)))))
                #t)
  
  
  (check-equal? (recursive? #'(define (pyramid n i)
                                (local [(define (turn-image n)
                                          (if (zero? n)
                                              empty-image
                                              i))]
                                  (foldr beside empty-image (map turn-image (build-list n add1))))))
                #f)
  
  
  (check-equal? (recursive? (syntax (define (top los)
                                      (local [(define (foo los)
                                                (cons [(empty? los) empty]
                                                      [else (cons (bar (first los))
                                                                  (mumble (rest los)))]))
                                              (define (bar s)
                                                (foo (car los)))]
                                        (foo los)))))
                #t)
  
  (check-equal? (recursive? (syntax (define (append-all-odd-lengths los)
                                      (local [(define (append-all-odd-lengths los)
                                                (foldr string-append "" (filter all-odd? los)))
                                              (define (all-odd? s)
                                                (odd? (string-length s)))]
                                        
                                        (append-all-odd-lengths los)))))
                #f))
