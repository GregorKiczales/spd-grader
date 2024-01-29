#lang racket

(require (for-syntax syntax/parse))

(require (only-in 2htdp/image image?)
         "walker.rkt"
         "grader.rkt"
         "utils.rkt"
         "type.rkt"
         spd/constants)

(module+ test
  (require rackunit))

(provide (all-from-out "type.rkt")
         grade-dd-rules-and-template
         grade-dd-rules
         grade-dd-template
         grade-template
         grade-template-intact
         grade-not-recursive
         
         check-dd-rules
         check-dd-template

         make-listof-type)






;; PLAN
;;
;;  grading unfilled template is in this file renamed to template, uses types, type names, and param and body
;;  grading intactness is in other file renamed to template-intact, uses types, type names, and param and body
;;
;; make check-template smarter, more flexible on things like order, maybe under flag control, ugh
;;  add mechanism to allow more params and different param orders to checking by type first
;;  then explore whether checking-by-type can be used for intactness in some cases
;;


(define-syntax (grade-dd-rules-and-template stx)
  (syntax-case stx ()
    [(_   type) #'(grade-dd-rules-and-template* 1 `type)]
    [(_ n type) #'(grade-dd-rules-and-template* n `type)]))

(define-syntax (grade-dd-rules stx)
  (syntax-case stx ()
    [(_   type) #'(grade-dd-rules* 1 `type)]
    [(_ n type) #'(grade-dd-rules* n `type)]))

(define-syntax (grade-dd-template stx)             ;used in m06 after
  (syntax-case stx ()                              ;@dd-template-rules
    [(_   type) #'(grade-dd-template* 1 `type)]    ;is not required
    [(_ n type) #'(grade-dd-template* n `type)]))

(define-syntax (grade-template stx)
  (syntax-case stx (define)
    [(_   (param ...) type) #'(grade-template* 1 `(param ...) `type)]
    [(_ n (param ...) type) #'(grade-template* n `(param ...) `type)]))



(define (grade-dd-rules-and-template* n type)
  (recovery-point grade-dd-rules-and-template
    (assert-context--@htdd)
    (weights (.3 *)
      (grade-dd-rules*    n type)
      (grade-dd-template* n type))))

(define (grade-dd-rules* n type)
  (recovery-point grade-dd-rules
    (assert-context--@htdd)
    (let* ([htdd    (car (context))]
           [dd-name (cadr htdd)]
           [rules   (find-@dd-template-rules-used-in-dd n)])
      (if (not rules)
          (rubric-item 'dd-template-rules #f
                       "DD template rules: could not find ~a @dd-template-rules tag in (@htdd ~a)" (number->ordinal* n) dd-name)
          (check-dd-rules type rules)))))

(define (grade-dd-template* n type)
  (recovery-point grade-dd-template
    (assert-context--@htdd)
    (let* ([htdd    (car (context))]
           [dd-name (cadr htdd)]
           [rules   (find-@dd-template-rules-used-in-dd n)]
           [defn    (find-template-in-dd n)])
      (if (not defn)
          (rubric-item 'dd-template #f
                       "DD template: could not find template function definition in (@htdd ~a)" dd-name)
          (check-dd-template type defn)))))

(define (grade-template* n params type-or-body)
  (recovery-point grade-template
    (assert-context--@htdf)
    (let* ([htdf (car (context))]
           [fn-name (cadr htdf)]
           [defn (find-template-in-@template n)])
      (cond [(not defn) 
             (rubric-item 'template #f
                          "Template: could not find ~a @template tag in (@htdf ~a)" (number->ordinal* n) fn-name)]
            ;; !!! this should probably go into check-template
            [(< (length (cdadr defn)) (length params))
             (rubric-item 'template #f
                          "Template: ~a template has too few parameters" (number->ordinal* n))]
            [(type? type-or-body) (check-template/type type-or-body defn)]
            [else                 (check-template/body defn `(define (fn-for ,@params) ,type-or-body))]))))



(define (check-dd-rules type rules)
  (ensure-type-is-well-formed type)
  (header "DD template rules:"
          (combine-scores (weights* 1.0 '(*) (check-dd-rules-internal type rules)))))

(define (check-dd-template type defn)
  (ensure-type-is-well-formed type)
  (header "DD template:"
          (combine-scores (weights* 1.0 '(*) (check-dd-template-internal type defn)))))

(define (check-template/type type defn)
  (ensure-type-is-well-formed type)
  (header "Template:"
          (combine-scores (weights* 1.0 '(*) (check-dd-template-internal type defn)))))

(define (check-template/body sub-defn sol-defn)
  (rubric-item 'template (check-template-bodies sub-defn sol-defn #t) "Template"))

;; !!! goes to other file?
(define (check-template-intact/body sub-defn sol-defn)
  (rubric-item 'template (check-template-bodies sub-defn sol-defn #f) "Template intact"))


(define (check-dd-rules-internal type rules)

  (define (type->rules ty in-one-of? in-compound?)
    (cond [(atomic-d?  ty) (if in-compound? '() '(atomic-distinct))]
          [(atomic-nd? ty) (if in-compound? '() '(atomic-non-distinct))]
          [(one-of?    ty) (cons 'one-of
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
           (error* "unrecognized type ~a" ty)]))
  
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



(define fn-name    (make-parameter #f))
(define params     (make-parameter #f))

;; precondition: type is well formed
;; !!! add args for questions? order? nr? r? mr?
(define (check-dd-template-internal type defn) ;!!! swap args to match sub sol order, and all throughout too

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
    (cond [(atomic-d? ty)  (tally (equal? expr '(...))            "~aatomic distinct ~a"     prefix expr)]
          [(atomic-nd? ty) (tally (equal? expr `(... ,@(params))) "~aatomic non-distinct ~a" prefix expr)]
	  [(one-of?   ty)  (check-one-of   ty expr prefix)]
	  [(compound? ty)  (check-compound ty expr prefix)]
          ;; Can only be inside compound
          ;; [(self-ref?  ty) ...]
          ;; [(ref?       ty) ...]
          ;; [(mref?      ty) ...]
	  [else
	   (error* "unrecognized type ~a" ty)]))
  
  (define (check-one-of ty subx prefix)
    (cond [(not (cond-expr? subx))
           (tally #f "missing cond expression")
           ;; if no cond all rules are incorrect
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
    (cond [(atomic-d? ft)
           (tally (or (equal? subx `(,sel ,(car (params))))
                      (equal? subx ft)
                      (equal? subx '(...)))
                  "selector ~a" subx)]
          [(atomic-nd? ft) ;!!! make this allow any order
           ;; !!! this could do an ormap over (params) 
           (tally (equal? subx `(,sel ,(car (params)))) "selector ~a" subx)]
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
          [(atomic-d? t1)
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
          [(atomic-nd? t1)
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
           (error* "can't check question for type ~a" t1)]))
  
  (check type (caddr defn))
  
  (reverse scores))

;; !!! goes to other file
(define (check-template-bodies sub0 sol0 answers?)
  ;; walk for equality, except after ... and in cond answers (deal w/ SR later)

  (define sub-params (cdadr sub0))
  (define sol-params (cdadr sol0))
  
  (define (walk sub sol)
    (cond [(member sol sol-params) (member sub sub-params)]
	  [(not (pair? sol))       (equal? sub sol)] ;1 2 3 "foo" #t...
          [(not (pair? sub)) #f]
         ;[(eqv? (car sol) '...) #t]  ;would have been a red error !!! going to have to look at this now that we are used for intactness
          [(eqv? (car sol) 'cond)
           (and (pair? sub)
                (eqv? (car sub) 'cond)
                (= (length sub) (length sol))
                (if answers?
                    (andmap walk          (cdr sub)           (cdr sol))
                    (andmap walk (map car (cdr sub)) (map car (cdr sol)))))]
          [else
           (and (= (length sub) (length sol))  ;compare expressions
                (andmap walk sub sol))]))
  (with-handlers ([void (lambda (e) #f)])    
    (walk (caddr sub0) (caddr sol0))))


;;
;; Helpers
;; 


(define (distinct-value? x)
  (or (number? x) (string? x) (member x '(true false empty))))

(define (boolean-value? x) (member x '(true false)))

(define (guard t)
  (cond [(string? t)     `(string? ,(car (params)))]
        [(eqv? t 'false) `(false? ,(car (params)))]
        [(eqv? t 'empty) `(empty? ,(car (params)))]        
        [else (error* "Don't know how to guard ~a" t)]))

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
  (cond [(not (distinct-value? t1)) (error* "first arg to same-type? must be distinct value ~a" t1)]
        [(number?  t1)       (or (number? t2) (member t2 '(Number Integer Natural)))]
        [(string?  t1)       (or (string? t2) (eqv? t2 'String))]
        [(image?   t1)       (error* "bad type, distinct value should not be an image ~a" t1)]
        [(boolean-value? t1) (or (boolean-value? t2) (eqv? t2 'Boolean))]
        [(eqv? t1 'empty)    (eqv? t2 'empty)] ;what about list types?
        [(compound? t1)      (error* "bad type, distinct values must be atomic " t1)]
        [else false]))


(define (equality-fn dv)
  (cond [(string? dv) 'string=?]
        [(number? dv) '=]
        [(boolean-value? dv) 'eqv?]
        [else
         (error* "Bad type given to check-type, distinct field of compound is not string, number or boolean.")]))

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






(define (find-@dd-template-rules-used-in-dd n)
  (let* ([htdd      (car (context))]
         [rules     (htdd-rules htdd)])
    (and (pair? rules)
         (>= (length rules) n)
         (cdr (list-ref rules (sub1 n))))))

(define (find-template-in-dd n)
  (let* ([htdd      (car (context))]
         [templates (htdd-templates htdd)])
    (and (pair? templates)
         (>= (length templates) n)
         (list-ref templates (sub1 n)))))

(define (find-template-in-@template n)
  (let* ([htdf  (car (context))]
         [ttags (htdf-templates htdf)])
    (and (pair? ttags)
         (>= (length ttags) n)
         (template-defn (list-ref ttags (sub1 n))))))



;; !!!!!!!!!!!!! this goes to other file

;; this is all just check-template-intact, so it's equal?, except cond anwsers are ignored


(define-syntax (grade-template-intact stx)
  (syntax-case stx (define)
    ;; !!! convert those possible
    [(_   (param ...) body)   #'(grade-template-intact-from-param-body 1 `(param ...) `body)]
    [(_ n (param ...) body)   #'(grade-template-intact-from-param-body n `(param ...) `body)]
    
    [(_   dd-name)            #'(grade-template-intact-from-type-name  1 `dd-name)]
    [(_ n dd-name)            #'(grade-template-intact-from-type-name  n `dd-name)]))                                             

(define (grade-template-intact-from-param-body n params body)
  (recovery-point grade-template-intact
    (assert-context--@htdf)
    (grade-template-intact-from-define n `(define (fn-for-x ,@params) ,body))))

(define (grade-template-intact-from-type-name n dd-name)
  (recovery-point grade-template-intact
    (if (not (pair? (htdd-templates `(@htdd ,dd-name))))
        (rubric-item 'template-intact #f "Template intact: incorrect - could not find template in (@htdd ~a)" dd-name)
        (let* ([template (car (htdd-templates `(@htdd ,dd-name)))] ;(define (fn-for... a) (cond ..))
              ;[htdf (car (context))]
              ;[defn (car (htdf-defns htdf))]
               )
          (grade-template-intact-from-define n template)))))

(define (grade-template-intact-from-define n template)
  (recovery-point grade-template-intact
    (assert-context--@htdf)
    (let* ([htdf   (car (context))]
           [defns  (htdf-defns htdf)])
      (cond [(<= (length defns) (sub1 n))
             (rubric-item 'template-intact #f
                          "Template intact: incorrect - could not find ~a function definition in ~a" (number->ordinal* n) htdf)]
            [else
             (check-template-intact/body (list-ref defns (sub1 n)) template)])))) ;!!! <<< GOES BACK TO CHECK-TEMPLATE-INTACT


(module+ test

  (define LON (make-listof-type 'Number 'fn-for-lon))

  (define Cat '(compound (Integer Integer) make-cat cat? (cat-x cat-y)))
  
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
  (check-equal? (map score-m (check-dd-rules-internal Cat '(compound)))
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
  (check-equal? (map score-m (check-dd-rules-internal LON
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
  
  (check-equal? (map score-m (check-dd-template-internal '(one-of "a" "b" "c")
                                                         '(define (fn-for-foo f)
                                                            (cond [(string=? f "a") (...)]
                                                                  [(string=? f "b") (...)]
                                                                  [(string=? f "c") (...)]))))
                '(1 1 1 1 1 1 1))

  
  
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
  (check-equal? (map score-m (check-dd-template-internal LON
                                                         '(define (fn-for-lon lon)
                                                            (cond [(empty? lon) (...)]
                                                                  [else
                                                                   (... (first lon)
                                                                        (fn-for-lon (rest lon)))]))))
                '(1 1 1 1 1 1 1 1))
  
  
  
  
  
  
  ;; LON no NR
  (check-equal? (map score-m (check-dd-template-internal LON
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
