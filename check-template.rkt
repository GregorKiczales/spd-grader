#lang racket

(require (for-syntax syntax/parse))

(require (only-in 2htdp/image image?)
         "walker.rkt"
         "grader.rkt"
         "utils.rkt")

(module+ test
  (require rackunit))

(provide grade-dd-rules-and-template
         grade-dd-rules
         grade-dd-template
         grade-template
         grade-template-intact
         grade-not-recursive

         grade-exact-template
         
         check-dd-rules
         check-dd-template
         check-template-intact

         make-listof-type)

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
;; Together these mean that while the type type is recursive, no actual data of the type can really be
;; arbitrary sized, or for that matter more than 3 deep (one-of -> compound -> atomic*,ref,self-ref).
;;

;; Types

(define (one-of?   x) (and (list? x) (eqv? (car x) 'one-of)))
(define (compound? x) (and (list? x) (eqv? (car x) 'compound) (= (length x) 5)))
(define (self-ref? x) (and (list? x) (eqv? (car x) 'self-ref) (= (length x) 2)))
(define (ref?      x) (and (list? x) (eqv? (car x) 'ref)      (= (length x) 2)))

(define (make-listof-type fn-for-lox x)
  `(one-of empty
           (compound (,x (self-ref ,fn-for-lox)) cons cons? (first rest))))


(define one-of-subclasses cdr)

(define (compound-fts       c) (list-ref c 1))
(define (compound-maker     c) (list-ref c 2))
(define (compound-predicate c) (list-ref c 3))
(define (compound-selectors c) (list-ref c 4))

(define self-ref-fn cadr)
(define ref-fn      cadr)

(define (distinct? t) (eqv? (rule-kind t) 'atomic-distinct))
(define (atomic?   t) (eqv? (rule-kind t) 'atomic-non-distinct))

(define (rule-kind t)
  (cond [(or (string? t) (member t '(true false empty)))           'atomic-distinct]
        [(member t '(Number Integer Natural String Image Boolean)) 'atomic-non-distinct]
        [(one-of? t)                                               'one-of]
        [(compound? t)                                             'compound]
        [(self-ref? t)                                             'self-ref]
        [(and (pair? t) (eqv? (car t) 'ref))                       'ref]
        [else
         (error "unrecognized type ~a" t)]))


;; -> raise internal error if type is malformed
(define (ensure-type-is-well-formed type)
  
  (define (check ty in-one-of? in-compound?)
    (cond [(atomic?   ty) #t]
          [(distinct? ty) #t]
	  [(one-of?   ty) (check-one-of   ty in-one-of? in-compound?)]
	  [(compound? ty) (check-compound ty in-one-of? in-compound?)]
          [(self-ref? ty) (and in-one-of? in-compound?)]
          [(ref?      ty) in-compound?]
	  [else
	   (error "unrecognized type ~a" ty)]))
  
  (define (check-one-of ty in-one-of? in-compound?)
    (and (not in-one-of?)
         (not in-compound?)
         (andmap (lambda (t)
                   (check t #t #f))
                 (one-of-subclasses ty))))
  
  (define (check-compound ty in-one-of? in-compound?)
    (and (not in-compound?)
         (andmap (lambda (t)
                   (check t in-one-of? #t))
                 (compound-fts ty))))
  
  (unless (check type #f #f)
    (error "Type is not well formed: ~a" type)))


;;
;; 
;;
(define-syntax (grade-dd-rules-and-template stx)
  (syntax-case stx ()
    [(_ type)   #'(grade-dd-rules-and-template 1 type)]
    [(_ n type)
     #'(recovery-point grade-dd-rules-and-template
         (assert-context--@htdd)
         (let* ([htdd      (car (context))]
                [names     (htdd-names htdd)]
                [rules     (htdd-rules htdd)]
                [templates (htdd-templates htdd)])
           (weights (.3 *)

             (if (<= (length rules) (sub1 n))
                 (score-it 'dd-rules
                           1 0
                           #f "dd template rules: could not find ~a @dd-template-rules tag." (number->ordinal* n))
                 (check-dd-rules    'type (cdr (list-ref rules (sub1 n)))))

             (if (<= (length templates) (sub1 n))
                 (score-it 'dd-template
                           1 0
                           #f "dd template: could not find ~a dd template function definition." (number->ordinal* n))
                 (check-dd-template 'type (list-ref templates (sub1 n)))))))]))

(define-syntax (grade-dd-rules stx)
  (syntax-case stx ()
    [(_ type)   #'(grade-dd-rules 1 type)]
    [(_ n type)
     #'(recovery-point grade-dd-rules
         (assert-context--@htdd)
         (let* ([htdd      (car (context))]
                [names     (htdd-names htdd)]
                [rules     (htdd-rules htdd)])
           (cond [(<= (length rules) (sub1 n))
                  (score-it 'dd-rules
                            1 0
                            #f "dd template rules: could not find ~a dd template rules tag." (number->ordinal* n))]
                 [else
                  (check-dd-rules 'type (cdr (list-ref rules (sub1 n))))])))]))

(define-syntax (grade-dd-template stx)
  (syntax-case stx ()
    [(_ type)   #'(grade-dd-template 1 type)]
    [(_ n type)
     #'(recovery-point grade-dd-template
         (assert-context--@htdd)
         (let* ([htdd      (car (context))]
                [templates (htdd-templates htdd)])
           (cond [(<= (length templates) (sub1 n))
                  (rubric-item 'dd-template #f
                               "dd template: could not find ~a dd template function definition." (number->ordinal* n))]
                 [else
                  (let ([defn (list-ref templates (sub1 n))])
                    (check-dd-template 'type defn))])))]))
#;#;#;
(define-syntax (grade-template stx)
  (syntax-case stx (define)
    [(_ . x ) #'(its-all-good-template)]))

(define-syntax (grade-exact-template stx)
  (syntax-case stx (define)
    [(_ . x) #'(its-all-good-template)]))

(define (its-all-good-template)
  (let* ([htdf      (car (context))]
         [templates (htdf-templates htdf)]
         [template  (and (pair? templates) (car templates))])
         
    (rubric-item 'template
                 (and template (pair? (cadr template)) (eqv? (caadr template) 'define))
                 "Template appears inside @template")))

(define-syntax (grade-template stx)
  (syntax-case stx (define)
    [(_   (define (fn-for . params) body)) #'(grade-template-from-define 1 'fn-for 'params 'body)]
    [(_ n (define (fn-for . params) body)) #'(grade-template-from-define n 'fn-for 'params 'body)]
    [(_   params type)                     #'(grade-template-from-type   1 'params 'type)]
    [(_ n params type)                     #'(grade-template-from-type   n 'params 'type)]))

(define-syntax (grade-exact-template stx)
  (syntax-case stx (define)
    [(_   (define (fn-for . params) body)) #'(grade-exact-template-helper 1 'fn-for 'params 'body)]
    [(_ n (define (fn-for . params) body)) #'(grade-exact-template-helper n 'fn-for 'params 'body)]))

(define (grade-template-from-type n params type)
  (recovery-point grade-template
    (let* ([htdf      (car (context))]
           [templates (htdf-templates htdf)])
      (cond [(<= (length templates) (sub1 n))
             (rubric-item 'template #f
                          "template: could not find ~a template function definition wrapped in @template." (number->ordinal* n))]
            [else
             (let* ([template  (list-ref templates (sub1 n))]
                    [defn (template-defn template)])
               (cond [(> (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too many parameters" (number->ordinal* n))]
                     [(< (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too few parameters" (number->ordinal* n))]
                     [else
                      (check-template type defn)]))]))))

(define (grade-template-from-define n fn-for params body)
  (recovery-point grade-template
    (assert-context--@htdf)
    (let* ([htdf      (car (context))]
           [templates (htdf-templates htdf)])
      (cond [(<= (length templates) (sub1 n))
             (rubric-item 'template #f
                          "template: could not find ~a template function definition wrapped in @template." (number->ordinal* n))]
            [else
             (let* ([template  (list-ref templates (sub1 n))]
                    [defn (template-defn template)])
               (cond [(> (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too many parameters" (number->ordinal* n))]
                     [(< (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too few parameters" (number->ordinal* n))]
                     [else
                      (check-template-intact defn `(define (,fn-for ,@params) ,body) "" "")]))]))))

(define (grade-exact-template-helper n fn-for params body)
  (recovery-point grade-template
    (assert-context--@htdf)
    (let* ([htdf      (car (context))]
           [templates (htdf-templates htdf)])
      (cond [(<= (length templates) (sub1 n))
             (rubric-item 'template #f
                          "template: could not find ~a template function definition wrapped in @template." (number->ordinal* n))]
            [else
             (let* ([template  (list-ref templates (sub1 n))]
                    [defn (template-defn template)])
               (cond [(> (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too many parameters" (number->ordinal* n))]
                     [(< (length (cdadr defn)) (length params))
                      (rubric-item 'template #f
                                   "template: ~a template has too few parameters" (number->ordinal* n))]
                     [else
                      (rubric-item 'template
                                   (equal? defn `(define (,fn-for ,@params) ,body))
                                   "template")]))]))))



(define-syntax (grade-template-intact stx)
  (syntax-case stx (define)
    [(_   (define (fn-for . params) body)) #'(grade-template-intact-from-define     1 `(define (fn-for . params) body))]
    [(_ n (define (fn-for . params) body)) #'(grade-template-intact-from-define     n `(define (fn-for . params) body))]
     
    [(_   param body)                      #'(grade-template-intact-from-param-body 1 `param `body)]
    [(_ n param body)                      #'(grade-template-intact-from-param-body n `param `body)]
    
    [(_ dd-name)                           #'(grade-template-intact-from-type-name  1 `dd-name)]))

(define (grade-template-intact-from-type-name n dd-name)
  (recovery-point grade-template-intact
    (if (not (pair? (htdd-templates `(@htdd ,dd-name))))
        (score-it 'template-intact 1 0 #f "Template intact: incorrect - could not find template in (@htdd ~a)." dd-name)
        (let* ([template (car (htdd-templates `(@htdd ,dd-name)))] ;(define (fn-for... a) (cond ..))
              ;[htdf (car (context))]
              ;[defn (car (htdf-defns htdf))]
               )
          (grade-template-intact-from-define n template)))))
                                             

(define (grade-template-intact-from-param-body n param body)
  (recovery-point grade-template-intact
    (assert-context--@htdf)
    (grade-template-intact-from-define n `(define (fn-for-x ,param) ,body))
    #;
    (let* ([htdf   (car (context))]
           [defns  (htdf-defns htdf)])
      (cond [(<= (length defns) (sub1 n))
             (score-it 'template-intact
                       1 0
                       #f "Template intact: incorrect - could not find ~a function definition in ~a." (number->ordinal* n) htdf)]
            [else
             (check-template-intact (list-ref defns (sub1 n)) `(define (fn-for-x ,param) ,body))]))))

(define (grade-template-intact-from-define n template)
  (recovery-point grade-template-intact
    (assert-context--@htdf)
    (let* ([htdf   (car (context))]
           [defns  (htdf-defns htdf)])
      (cond [(<= (length defns) (sub1 n))
             (score-it 'template-intact
                       1 0
                       #f "Template intact: incorrect - could not find ~a function definition in ~a." (number->ordinal* n) htdf)]
            [else
             (check-template-intact (list-ref defns (sub1 n)) template)]))))


(define (grade-not-recursive [n 1])
  (let* ([defn    (list-ref (htdf-defns (car (context))) (sub1 n))]
         [fn-name (cdadr defn)]
         [not-rec? (not (recursive? defn))])
    (rubric-item 'template not-rec? "Must not use any part of a recursive template")))






(define (check-dd-rules type rules)
  (ensure-type-is-well-formed type)
  (header "DD template rules:"
          (combine-scores (weights* 1.0 '(*) (check-dd-rules-internal type rules)))))

(define (check-dd-template type defn)
  (ensure-type-is-well-formed type)
  (header "DD template:"
          (combine-scores (weights* 1.0 '(*) (check-dd-template-internal type defn)))))

(define (check-template type defn)
  (ensure-type-is-well-formed type)
  (header "Template:"
          (combine-scores (weights* 1.0 '(*) (check-dd-template-internal type defn)))))


(define (check-dd-rules-internal type rules)
  
  (define (type->rules ty in-one-of? in-compound?)
    (cond [(atomic?   ty) (if in-compound? '() '(atomic-non-distinct))]
          [(distinct? ty) (if in-compound? '() '(atomic-distinct))]
	  [(one-of?   ty) (cons 'one-of
                                (foldr append '()
                                       (map (lambda (t2)
                                              (type->rules t2 #t #f))
                                            (one-of-subclasses ty))))]
	  [(compound? ty) (cons 'compound
                                (foldr append '()
                                       (map (lambda (t2)
                                              (type->rules t2 #f #t))
                                            (compound-fts ty))))]
          [(self-ref? ty) '(self-ref)]
          [(ref?      ty) '(ref)]
	  [else
	   (error "unrecognized type ~a" ty)]))

  (define (check sol sub)
    (cond [(and (empty? sol) (empty? sub)) '()]
          [(empty? sol)
           (cons (tally #f "~a is extra rule." (car sub))
                 (check sol (cdr sub)))]
          [(empty? sub)
           (cons (tally #f "~a rule is missing." (car sol))
                 (check (cdr sol) sub))]
          [else
           (cond [(eqv? (car sol) (car sub))
                  (cons (tally #t "~a is correct." (car sol))
                        (check (cdr sol) (cdr sub)))]
                 ;; next three are special cases of incorrect, specifically
                 ;;  - next two sub are correct but swapped
                 ;;  - this sub is incorrect, but this sub is next sol (drop one)
                 ;;  - this sub is incorrect, but next sub is this sol (insert one)
                 [(and (>= (length sol) 2)
                       (>= (length sub) 2)
                       (eqv? (car sol) (cadr sub))
                       (eqv? (cadr sol) (car sub)))
                  (list* (tally #f "~a and ~a are swapped in order, 1 marked wrong." (car sol) (cadr sol))
                         (tally #f "~a and ~a are swapped in order, 1 marked right." (car sol) (cadr sol))
                         (check (cddr sol) (cddr sub)))]
                 [(and (>= (length sol) 2)
                       (eqv? (cadr sol) (car sub)))
                  (cons (tally #f "~a rule is missing." (car sol))
                        (check (cdr sol) sub))]
                 [(and (>= (length sub) 2)
                       (eqv? (car sol) (cadr sub)))
                  (cons (tally #f "~a is extra rule." (car sub))
                        (check sol (cdr sub)))]
                 ;; after those special cases it's just incorrect
                 [else
                  (cons (tally #f "~a is incorrect." (car sub))
                        (check (cdr sol) (cdr sub)))])]))

  (define (tally corr? fmt . fmt-args)
    (score-it 'dd-rules 1 (if corr? 1 0) #f (apply format fmt fmt-args)))
  
  (check (type->rules type #f #f) rules))

#;
(define (check-dd-rules-internal type rules [allow-one-missing? #f])
  
  (define (peek) (and (pair? rules) (car rules)))
  (define (pop)  (if (pair? rules)
                     (begin0 (car rules) (set! rules (cdr rules)))
                     "missing rule"))
  
  (define scores '())
  
  (define (tally sol)
    (let ([sub (peek)])
      (cond [(eqv? sub sol)
             (pop)
             (set! scores (cons (rubric-item 'dd-rules #t "~a" sol) scores))]
            [allow-one-missing?
             (set! allow-one-missing? false)
             (set! scores (cons (rubric-item 'dd-rules #f "~a" sol) scores))]
            [else
             (pop)
             (set! scores (cons (rubric-item 'dd-rules #f "~a" sol) scores))])))
          
  
  (define (check ty in-one-of? in-compound?)
    (cond [(atomic?   ty) (check-atomic   ty in-one-of? in-compound?)]
          [(distinct? ty) (check-distinct ty in-one-of? in-compound?)]
	  [(one-of?   ty) (check-one-of   ty in-one-of? in-compound?)]
	  [(compound? ty) (check-compound ty in-one-of? in-compound?)]
          [(self-ref? ty) (tally 'self-ref)]
          [(ref?      ty) (tally 'ref)]
	  [else
	   (error "unrecognized type ~a" ty)]))
  
  (define (check-atomic ty in-one-of? in-compound?)
    (cond [in-compound? (void)]          
          [else         (tally 'atomic-non-distinct)]))
  
  (define (check-distinct ty in-one-of? in-compound?)
    (cond [in-compound? (void)]
          [else         (tally 'atomic-distinct)]))  ;let this happen at top level
  
  (define (check-one-of ty in-one-of? in-compound?)
    (cond [in-one-of?   (error "one-of inside one-of??")]   ;!!!can no longer happen
          [in-compound? (error "one-of inside compound??")] ;!!!can no longer happen
          [else
           (tally 'one-of)
           (for ([ty1 (one-of-subclasses ty)])
             (check ty1 #t #f))]))
  
  (define (check-compound ty in-one-of? in-compound?)
    (cond [in-compound? (error "compound inside compound??")] ;!!!can no longer happen
          [else
           (tally 'compound)
           (for ([ty1 (compound-fts ty)])
             (check ty1 #f #t))]))
  
  (check type #f #f)
  (for ([sub rules])
    (unless allow-one-missing?
      (set! scores
            (cons (rubric-item 'dd-rules #f "extra rule: ~a" sub)
                  scores)))
    (set! allow-one-missing? #f))
  
  (reverse scores))


(define fn-name    (make-parameter #f));name of the function
(define params     (make-parameter #f));parameters

;; precondition: type is well formed
(define (check-dd-template-internal type defn)

  (fn-name (caadr defn))
  (params  (cdadr defn))
  
  (define scores '())
  
  (define (tally corr? fmt-ctl . fmt-args)
    (set! scores
          (cons (if corr?
                    (score-it 'dd-template 1 1 #f (string-append (apply format fmt-ctl fmt-args) ": correct."))
                    (score-it 'dd-template 1 0 #f (string-append (apply format fmt-ctl fmt-args) ": incorrect.")))
                scores)))
  
  (define (check ty expr [prefix ""])
    (cond [(atomic?   ty) (tally (equal? expr `(... ,@(params))) "~aatomic non-distinct ~a" prefix expr)]
          [(distinct? ty) (tally (equal? expr '(...))            "~aatomic distinct ~a"     prefix expr)]
	  [(one-of?   ty) (check-one-of   ty expr prefix)]
	  [(compound? ty) (check-compound ty expr prefix)]
          ;; these can only be inside a compound
          ;; [(self-ref? ty) ...]
          ;; [(ref?      ty) ...]
	  [else
	   (error "unrecognized type ~a" ty)]))
  
  (define (check-one-of ty subx prefix)
    (cond [(not (and (pair? subx) (eqv? (car subx) 'cond) (not (null? (cdr subx)))))
           (tally #f "missing cond expression")
           ;; if no cond just match rules
           (for [(ty1 (one-of-subclasses ty))]
             (tally #f "missing question for subclass ~a" (rule-kind ty1))
             (tally #f "missing answer for subclass ~a" (rule-kind ty1)))]
          [else
           (tally #t "cond expression")
           (let loop [(ts (one-of-subclasses ty))
                      (qas (cdr subx))]
             (cond [(and (empty? ts) (empty? qas)) (void)]
                   [(empty? ts)
                    (tally #f "extra cond question answer pair")
                    (loop '() (cdr qas))]
                   [(empty? qas)
                    (tally #f "missing cond question answer pair")
                    (loop (cdr ts) '())]
                   [else
                    (let ([t   (car ts)]
                          [rst (cdr ts)]
                          [qa  (car qas)])
                      (cond [(not (and (pair? qa) (= (length qa) 2)))
                             (tally #f "no question ~a" qa)
                             (tally #f "no answer ~a" qa)]
                            [else
			     (check-question? t (car qa) rst)
			     (check t (cadr qa) "cond answer ")])
		      (loop (cdr ts) (cdr qas)))]))]))
  
  ;; precondition (eqv? (subr-pop) 'compound)
  (define (check-compound t subx prefix)
    (let ([wf? (and (list? subx) (eqv? (car subx) '...))])
      (tally wf? "(...   ) ~a" subx)
      (let loop [(fts   (compound-fts t))
                 (sels  (compound-selectors t))
                 (subxs (if wf?
                            (remove (params) (cdr subx)) ;drop possible atomics out
                            '()))]
        (cond [(and (empty? fts) (empty? subxs)) (void)]
              [(empty? fts)
               (unless (member (car subxs) (params)) ;means the add-param has to be second
                 (tally #f "extraneous expression inside ... for compound ~a" (car subxs)))
               (loop '() '() (cdr subxs))]
              [(empty? subxs)
               (tally #f "missing selector inside ... for compound")
               (loop (cdr fts) (cdr sels) '())]
              
              [else
               (check-field (car fts) (car sels) (car subxs))
               (loop (cdr fts)
                     (cdr sels)
                     (cdr subxs))]))))
  
  
  (define (check-field ft sel subx)
    (cond [(atomic? ft) ;!!! make this allow any order
           ;; !!! this could do an ormap over (params) 
           (tally (equal? subx `(,sel ,(car (params)))) "selector ~a" subx)]
          [(distinct? ft)
           (tally (or (equal? subx `(,sel ,(car (params))))
                      (equal? subx ft)
                      (equal? subx '(...)))
                  "selector ~a" subx)]
          [(self-ref? ft)
           (tally (contains-exp? `(,sel ,(car (params))) subx) "selector ~a" `(,sel ,(car (params))))
           (tally (equal? subx `(,(fn-name) (,sel ,(car (params))))) "natural recursion on result of selector ~a" subx)]
          [(ref? ft)
           (tally (contains-exp? `(,sel ,(car (params))) subx) "selector ~a" `(,sel ,(car (params))))
           (tally (equal? subx `(,(cadr ft) (,sel ,(car (params)))))
                  "natural helper/mutual-recursion on result of selector ~a"
                  subx)]))
  
  
  ;; can't have bare self-ref or ref as a field type because we don't know how to form the predicate
  (define (check-question? t1 subx rst [enum? #f] [allow-unsimplified? #t])
    (cond [(and (null? rst) (not enum?) (eqv? subx 'else))
           (tally #t "cond question ~s" subx)]
          [(eqv? (rule-kind t1) 'atomic-distinct)
           (let* ([must-guard? (not (andmap (lambda (t2) (same-type? t1 t2)) rst))]
                  [must-test?  (ormap (lambda (t2) (same-type? t1 t2)) rst)]
                  [canon (canonicalize-question subx)])
             (tally (cond [(and must-guard? must-test?)
                           (equal? canon `(and ,(guard t1) ,(test t1)))]
                          [must-guard?
                           (or (and allow-unsimplified?
                                    (equal? canon `(and ,(guard t1) ,(test t1))))
                               (equal? canon (guard t1)))]
                          [must-test?
                           (or (and allow-unsimplified?
                                    (equal? canon `(and ,(guard t1) ,(test t1))))
                               (equal? canon (test t1)))])
                    "cond question ~s" subx))]
          [(eqv? (rule-kind t1) 'atomic-non-distinct)
           (let* ([canon (canonicalize-question subx)])
             (tally (equal? canon (test t1)) "cond question ~s" subx))]
          [(compound? t1)
           (if (and (ormap distinct-value? (compound-fts t1))
                    (ormap (lambda (t2) (and (compound? t2)
                                             (eqv? (compound-predicate t1) (compound-predicate t2))))
                           rst))
               (let* [(idx  (index-where (compound-fts t1) distinct-value?))
                      (dft  (list-ref (compound-fts t1) idx))
                      (dsel (list-ref (compound-selectors t1) idx))]
                 (tally (equal?  `(,(equality-fn dft) (,dsel ,(car (params))) ,dft)
                                 (canonicalize-question subx))
                        "cond question ~s"
                        subx))
               (tally (equal? `(,(compound-predicate t1) ,(car (params)))
                              (canonicalize-question subx))
                      "cond question ~s"
                      subx))]
          [else
           (error "can't check question for type ~a" t1)]))
  
  (check type (caddr defn))
  
  (reverse scores))

(define (check-template-intact sub0 sol0 [prefix ""] [suffix "intact (cond questions not altered)"])
  ;; walk for equality, except after ... and in cond answers (deal w/ SR later)

  (define sub-params (cdadr sub0))
  (define sol-params (cdadr sol0))

  #;
  (define (param-map sub)
    (let loop ([su sub-params]
               [so sol-params])
      (cond [(empty? so) #f]
            [(empty? su) #f] ;this can't happen on code that checks syntax?
            [(eqv? (car su) sub) (car so)]
            [else
             (loop (cdr su) (cdr so))])))
  
  (define (walk sub sol)
    (cond [(member sol sol-params) (member sub sub-params)]
	  [(not (pair? sol))       (equal? sub sol)] ;1 2 3 "foo" #t...
          [(not (pair? sub)) #f]
         ;[(eqv? (car sol) '...) #t]  ;shouldn't happen
          [(eqv? (car sol) 'cond)
           (and (pair? sub)
                (eqv? (car sub) 'cond)
                (= (length sub) (length sol))
                (andmap walk (map car (cdr sub)) (map car (cdr sol))))] ;compare questions
          [else
           (and (= (length sub) (length sol))  ;compare expressions
                (andmap walk sub sol))]))


  (rubric-item 'template-intact
               (with-handlers ([void (lambda (e) #f)])
                 (walk (caddr sub0) (caddr sol0)))
               "~a~a~a"
               (if (string=? prefix "") "Template" (string-append prefix " template"))
               (if (string=? suffix "") "" " ")
               suffix))


(define (distinct-value? x)
  (or (number? x) (string? x) (member x '(true false empty))))

(define (boolean-value? x) (member x '(true false)))

(define (guard t)
  (cond [(string? t)     `(string? ,(car (params)))]
        [(eqv? t 'false) `(false? ,(car (params)))]
        [(eqv? t 'empty) `(empty? ,(car (params)))]        
        [else (error "Don't know how to guard ~a" t)]))

(define (test t)
  (cond [(string? t)      `(string=? ,(car (params)) ,t)]
        [(eqv? t 'false)  `(false? ,(car (params)))]        
        [(eqv? t 'empty)  `(empty? ,(car (params)))]
        [(compound? t)    `(,(compound-predicate t) ,(car (params)))]
        [else
         (case t
           [(Number Integer Natural) `(number?  ,(car (params)))]
           [(String)                 `(string?  ,(car (params)))]
           [(Image)                  `(image?   ,(car (params)))]
           [(Boolean)                `(boolean? ,(car (params)))])]))


(define (same-type? t1 t2)
  (cond [(not (distinct-value? t1)) (error "first arg to same-type? must be distinct value ~a" t1)]
        [(number?  t1)       (or (number? t2) (member t2 '(Number Integer Natural)))]
        [(string?  t1)       (or (string? t2) (eqv? t2 'String))]
        [(image?   t1)       (error "bad type, distinct value should not be an image " t1)]
        [(boolean-value? t1) (or (boolean-value? t2) (eqv? t2 'Boolean))]
        [(eqv? t1 'empty)    (eqv? t2 'empty)] ;what about list types?
        [(compound? t1)      (error "bad type, distinct values must be atomic " t1)]
        [else false]))


(define (equality-fn dv)
  (cond [(string? dv) 'string=?]
        [(number? dv) '=]
        [(boolean-value? dv) 'eqv?]
        [else
         (error "Bad type given to check-type, distinct field of compound is not string, number or boolean.")]))

(define (canonicalize-question subx)
  (cond [(pair? subx)
         (if (and (= (length subx) 3)
                  (member (car subx) '(string=? = eqv?))
                  (eqv? (caddr subx) (car (params))))
             `(,(car subx) ,(caddr subx) ,(cadr subx))
             (map canonicalize-question subx))]
        [else subx]))


(define (make-fn-for-name t)
  (string->symbol
   (string-append "fn-for"
                  (let loop ([chars (string->list (symbol->string t))]
                             [result '()])
                    (if (empty? chars)
                        (list->string (reverse result))
                        (loop (cdr chars)
                              (cond [(char-upper-case? (car chars))
                                     (list* (char-downcase (car chars)) #\- result)]
                                    [else
                                     (cons (car chars) result)])))))))                              

(define (2list? x) (and (pair? x) (= (length x) 2)))

(define (number->ordinal* n)
  (if (= n 1)
      ""
      (format "~a " (number->ordinal n))))


(module+ test
  
  ;; top-level distinct (shouldn't actually happen, but forms a base case for testing)
  (check-equal? (map score-m (check-dd-rules-internal "green"
                                                      '(atomic-distinct)))
                '(1))
  
  ;; simple atomic-non-distinct
  (check-equal? (map score-m (check-dd-rules-internal 'Number
                                                      '(atomic-non-distinct)))
                '(1))
  
  
  ;; String instead of Number
  (check-equal? (map score-m (check-dd-rules-internal 'String
                                                      '(atomic-non-distinct)))
                '(1))
  
  
  ;; 2 field compound of atomic non-distinct
  (check-equal? (map score-m (check-dd-rules-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                      '(compound)))
                '(1))
  
  
  ;; self-ref (note this type isn't well-formed SR since no base case, but it does
  ;; follow the less stringent formedness rules this checker needs
  (check-equal? (map score-m (check-dd-rules-internal '(compound (Integer (self-ref fn-for-cat))
                                                                 make-cat cat?
                                                                 (cat-x cat-foo))
                                                      '(compound self-ref)))
                '(1 1))
  
  
  ;; self-ref rule missing
  (check-equal? (map score-m (check-dd-rules-internal '(compound (Integer (self-ref fn-for-cat))
                                                                 make-cat cat?
                                                                 (cat-x cat-foo))
                                                      '(compound)))
                '(1 0))
  
  
  ;; ref
  (check-equal? (map score-m (check-dd-rules-internal '(compound (Integer (ref fn-for-foo))
                                                                 make-cat cat?
                                                                 (cat-x cat-foo))
                                                      '(compound ref)))
                '(1 1))
  
  (check-equal? (map score-m (check-dd-rules-internal '(one-of String Number)
                                                      '(one-of atomic-non-distinct atomic-non-distinct)))
                '(1 1 1))
  
  
  (check-equal? (map score-m (check-dd-rules-internal '(one-of "pre-launch" Number "post-flight")
                                                      '(one-of atomic-distinct
                                                               atomic-non-distinct
                                                               atomic-distinct)))
                '(1 1 1 1))
  
  (check-equal? (map score-m (check-dd-rules-internal '(one-of String Number)
                                                      '(one-of atomic-non-distinct atomic-non-distinct)))
                '(1 1 1))
  
  ;; LON
  (check-equal? (map score-m (check-dd-rules-internal '(one-of empty
                                                               (compound (Number (self-ref fn-for-lon))
                                                                         cons
                                                                         cons?
                                                                         (first rest)))
                                                      '(one-of atomic-distinct compound self-ref)))
                '(1 1 1 1))
  
  
  (check-equal? (map score-m (check-dd-rules-internal '(one-of empty
                                                               (compound ("L" (self-ref fn-for-path)) cons cons? (first rest))
                                                               (compound ("R" (self-ref fn-for-path)) cons cons? (first rest)))
                                                      '(one-of atomic-distinct compound self-ref compound self-ref)))
                '(1 1 1 1 1 1))
  
  (check-equal? (map score-m (check-dd-rules-internal '(one-of Number "x" "y" "z" Boolean)
                                                      '(one-of atomic-non-distinct
                                                               atomic-distinct
                                                               atomic-distinct
                                                               atomic-distinct
                                                               atomic-non-distinct)))
                '(1 1 1 1 1 1))
  
  
  
  
  ;;; ****************
  ;; top-level distinct (shouldn't actually happen, but forms a base case for testing)
  (check-equal? (map score-m (check-dd-template-internal "green"
                                                         '(define (fn-for-foo x)
                                                            (...))))
                '(1))
  
  
  ;; simple atomic-non-distinct
  (check-equal? (map score-m (check-dd-template-internal 'Number
                                                         '(define (fn-for-foo n)
                                                            (... n))))
                '(1))
  
  
  ;; String instead of Number
  (check-equal? (map score-m (check-dd-template-internal 'String
                                                         '(define (fn-for-foo x)
                                                            (... x))))
                '(1))
  
  
  
  ;; wrong parameter
  (check-equal? (map score-m (check-dd-template-internal 'String
                                                         '(define (fn-for-foo x)
                                                            (... y))))
                '(0))
  
  
  
  
  ;; 2 field compound of atomic non-distinct
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-foo c)
                                                            (... (cat-x c) (cat-y c)))))
                '(1 1 1))
  
  ;; first selector is missing, messes up remaining match.
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-y c)))))
                '(1 0 0))
  
  ;; second selector is missing
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c)))))
                '(1 1 0))
  
  ;; second selector is garbage
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c) (foo 1)))))
                '(1 1 0))
  
  
  
  
  ;; junk at end
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer Integer) make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c) (cat-y c) (foo 1)))))
                '(1 1 1 0))
  
  
  ;; 1 field is distinct
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer "hello") make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c) (cat-y c)))))
                '(1 1 1))
  
  ;; 1 field distinct, val in template
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer "hello") make-cat cat? (cat-x cat-y))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c) "hello"))))
                '(1 1 1))
  
  
  
  
  ;; self-ref (note this type isn't well-formed SR since no base case, but it does
  ;; follow the less stringent formedness rules this checker needs
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer (self-ref fn-for-cat))
                                                                    make-cat cat?
                                                                    (cat-x cat-foo))
                                                         '(define (fn-for-cat c)
                                                            (... (cat-x c) (fn-for-cat (cat-foo c))))))
                '(1 1 1 1))
  
  
  ;; NR missing
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer (self-ref fn-for-cat))
                                                                    make-cat cat?
                                                                    (cat-x cat-foo))
                                                         '(define (fn-for-foo c)
                                                            (... (cat-x c) (cat-foo c)))))
                '(1 1 1 0))
  
  
  ;; ref
  (check-equal? (map score-m (check-dd-template-internal '(compound (Integer (ref fn-for-foo))
                                                                    make-cat cat?
                                                                    (cat-x cat-foo))
                                                         '(define (fn-for-foo c)
                                                            (... (cat-x c) (fn-for-foo (cat-foo c))))))
                '(1 1 1 1))
  
  
  (check-equal? (map score-m (check-dd-template-internal '(one-of String Number)
                                                         '(define (fn-for-foo l)
                                                            (cond [(string? l) (... l)]
                                                                  [else (... l)]))))
                '(1 1 1 1 1))
  
  
  ;; number? instead of string? in question
  (check-equal? (map score-m (check-dd-template-internal '(one-of String Number)
                                                         '(define (fn-for-foo l)
                                                            (cond [(number? l) (... l)]
                                                                  [else (... l)]))))
                '(1 0 1 1 1))
  
  (check-equal? (map score-m (check-dd-template-internal '(one-of "pre-launch" Number "post-flight")
                                                         '(define (fn-for-foo a)
                                                            (cond [(and (string? a) (string=? a "pre-launch")) (...)]
                                                                  [(number? a) (... a)]
                                                                  [else (...)]))))
                '(1 1 1 1 1 1 1))
  
  
  ;; number? instead of else
  (check-equal? (map score-m (check-dd-template-internal '(one-of String Number)
                                                         '(define (fn-for-foo l)
                                                            (cond [(string? l) (... l)]
                                                                  [(number? l) (... l)]))))
                '(1 1 1 1 1))
  
  ;; LON
  (check-equal? (map score-m (check-dd-template-internal '(one-of empty
                                                                  (compound (Number (self-ref fn-for-lon))
                                                                            cons
                                                                            cons?
                                                                            (first rest)))
                                                         '(define (fn-for-lon lon)
                                                            (cond [(empty? lon) (...)]
                                                                  [else
                                                                   (... (first lon)
                                                                        (fn-for-lon (rest lon)))]))))
                '(1 1 1 1 1 1 1 1))
  
  
  
  
  
  
  ;; LON no NR
  (check-equal? (map score-m (check-dd-template-internal '(one-of empty
                                                                  (compound (Number (self-ref fn-for-lon))
                                                                            cons
                                                                            cons?
                                                                            (first rest)))
                                                         '(define (fn-for-foo lon)
                                                            (cond [(empty? lon) (...)]
                                                                  [else
                                                                   (... (first lon)
                                                                        (rest lon))]))))
                '(1 1 1 1 1 1 1 0))
  
  
  (check-equal? (map score-m (check-dd-template-internal '(one-of empty
                                                                  (compound ("L" (self-ref fn-for-path)) cons cons? (first rest))
                                                                  (compound ("R" (self-ref fn-for-path)) cons cons? (first rest)))
                                                         '(define (fn-for-path p)
                                                            (cond [(empty? p) (...)]
                                                                  [(string=? (first p) "L") (... "L" (fn-for-path (rest p)))]
                                                                  [else                     (... "R" (fn-for-path (rest p)))]))))
                '(1 1 1 1 1 1 1 1 1 1 1 1 1))
  
  
  
  
  
  
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (... f)]
                                                                  [(and (string? f) (string=? f "x")) (...)]
                                                                  [(and (string? f) (string=? f "y")) (...)]
                                                                  [     (string? f)                   (...)]
                                                                  [else                               (... f)]))))
                '(1 1 1 1 1 1 1 1 1 1 1))
  
  
  ;; goofy order of args to string=? 
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (... f)]
                                                                  [(and (string? f) (string=? "x" f)) (...)]
                                                                  [(and (string? f) (string=? "y" f)) (...)]
                                                                  [     (string? f)                   (...)]
                                                                  [else                               (... f)]))))
                '(1 1 1 1 1 1 1 1 1 1 1))
  
  
  
  ;; bad last answer
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (... f)]
                                                                  [(and (string? f) (string=? f "x")) (...)]
                                                                  [(and (string? f) (string=? f "y")) (...)]
                                                                  [     (string? f)                   (...)]
                                                                  [else                               false]))))
                '(1 1 1 1 1 1 1 1 1 1 0))
  
  
  
  
  ;; bad first and last answers
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (...)]
                                                                  [(and (string? f) (string=? f "x")) (...)]
                                                                  [(and (string? f) (string=? f "y")) (...)]
                                                                  [     (string? f)                   (...)]
                                                                  [else                               false]))))
                '(1 1 0 1 1 1 1 1 1 1 0))
  
  
  
  
  ;; missing guard on third question
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (... f)]
                                                                  [(and (string? f) (string=? f "x")) (...)]
                                                                  [                 (string=? f "y")  (...)]
                                                                  [     (string? f)                   (...)]
                                                                  [else                               (... f)]))))
                '(1 1 1 1 1 0 1 1 1 1 1))
  
  
  
  
  
  ;; missing guard on third question, missing guard and needless test on fourth question
  (check-equal? (map score-m (check-dd-template-internal '(one-of Number "x" "y" "z" Boolean)
                                                         '(define (fn-for-foo f)
                                                            (cond [(number? f)                        (... f)]
                                                                  [(and (string? f) (string=? f "x")) (...)]
                                                                  [                 (string=? f "y")  (...)]
                                                                  [                 (string=? f "x")  (...)]
                                                                  [else                               (... f)]))))
                '(1 1 1 1 1 0 1 0 1 1 1)))
