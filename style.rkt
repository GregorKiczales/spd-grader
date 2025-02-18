#lang racket

(require racket/set
         spd-grader/defs
         spd-grader/utils
         spd-grader/walker
         spd-grader/score
         spd-grader/file-structure)

(provide grade-style
         grade-style*
         check-style
         check-style/file
         include-correct-htdf-style-scores?
         
         check-style/htdf)


;;
;; @htdf
;; all design elements present and in proper order
;;  one-line purpose
;;  at least two tests
;;  stub has single ; and space
;; function and parameter names in lower-caravan-case
;;

(define-syntax (grade-style stx)
  (syntax-case stx ()
    [(_ option ...) #'(grade-style* 'option ...)]))

(define (grade-style* . options)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (weights (*)
                       (rubric-item 'style #f "System error checking style")))])
    (check-style options)))

(define checking-struct?              (make-parameter #f))
(define checking-stub?                (make-parameter #f))
(define checking-@template?           (make-parameter #f))
(define checking-local?               (make-parameter #f))

(define include-correct-htdf-style-scores? (make-parameter #f))

(define (check-style options)
  (for ([option options])
    (unless (member option '(struct stub @template local))
      (error 'check-style "~a is not a legal option for grade-style or check-style." option)))
  
  (parameterize ([checking-struct?    (memq 'struct options)]
                 [checking-stub?      (memq 'stub options)]
                 [checking-@template? (memq '@template options)]          
                 [checking-local?     (memq 'local options)])

      (combine-scores
       (weights* 1.0 '(*)
         (let* ([htdf-tags (filter (lambda (stx) (let ([e (syntax-e stx)]) (and (pair? e) (eq? (syntax-e (car e)) '@htdf)))) (stxs))]
                [scores    (map check-style/htdf htdf-tags)]
                [scores-to-report
                 (filter (lambda (s)
                           (or (include-correct-htdf-style-scores?)
                               (not (= (score-m s) 1))))
                         scores)])
           (cond [(null? scores)           (list (rubric-item 'style #t "Style checking correct by default since no @htdf tags in file"))]
                 [(null? scores-to-report) (list (rubric-item 'style #t "Style checking"))]
                 [else scores-to-report]))))))


(define (check-style/file filename options [include-ones? #f]) ;only for handin/scripts/console.rkt
  (parameterize ([stxs  #f]
                 [lines #f]
                 [elts  #f]
                 [include-correct-htdf-style-scores? include-ones?])
    (let ([bytes (call-with-input-file* filename port->bytes)])
      (stxs  (call-with-input-bytes bytes (lambda (in) (read-syntaxes filename in))))
      (lines (call-with-input-bytes bytes port->lines))
      (elts  (parse-elts (stxs) (lines)))
      (let ([s (header (format "Style rules for ~a:" filename) (check-style options))])
        (when (or include-ones?
                  (not (= (score-m s) 1)))
          (display-score s (current-output-port) #t))))))
               

(define (check-style/htdf tag-stx)
  (let ([design (parse-htdf  tag-stx)])
    (header (format "Style rules for ~a:" (syntax->datum tag-stx))
      (combine-scores
       (weights* 1.0 '(*)
         (remove* '(#f)
                  (list (check-2-semi-comments (htdf-design-lines design))
                        (check-purpose design)
                        (and (checking-stub?) (check-stub design))
                        (check-names   (htdf-design-stxs design)))))))))

;;
;; @htdd
;; all @tags present and in proper order
;; type comment starts w/ TypeName is
;; .interp is present
;; type names in UpperCamelCase
;;
;;
;; General
;; all ;; followed by one space
;;  - nothing moves when pressing CMD-I
;;  - proper [( brackets in cond and local
;;  - no space after (
;;  - no space before )
;;  - no line breaks between (  or between )
;;  - space between id and (
;;



;;
;; lines w/ ;; must
;;   - not have ;;;
;;   - have the 2 semi preceded only by whitespace
;;
(define (check-2-semi-comments [lines (lines)])
  (let ([bad-lines
         (let loop ([lines lines]
                    [n 0])
           (if (null? lines)
               '()
               (cons-if (and (regexp-match? #rx";;" (car lines))
                             (or (and (not (regexp-match? #rx";;$" (car lines)))
                                      (not (regexp-match? #rx";; " (car lines))))
                                 (regexp-match? #rx";;;" (car lines))
                                 (regexp-match? #rx"; ;;" (car lines))))
                        (cons n (car lines))
                        (loop (cdr lines)
                              (add1 n)))))])
    (score #f 'style 1
           (if (null? bad-lines) 1 0)
           '()
           (cons (message #f
                          "All two semi-colon comments are properly formatted:~a"
                          (cond [(null? bad-lines) " correct."]
                                [(null? (cdr bad-lines)) " incorrect, 1 is not."]
                                [else
                                 (format " incorrect, ~a ~a not." (length bad-lines) (is/are bad-lines))]))
                 (map (lambda (bl) 
                        (message #f "  ~a" (cdr bl)))
                      bad-lines)))))


;;
;; Expects design elements to be in order (in place). Doesn't fail if things
;; are out of place, but once a stx element is out of place everything from
;; it to the end will be #f
;;
;; (@htdf foo)
;; (@signature ..)
;; <purpose line(s)>
;;
;; <checks>
;;
;; ;(define  ;stub
;;
;; <allows origins, templates, and definitions to be grouped or ungrouped>

(struct htdf-design (tag sigs purpose checks stub origins templates fn-defns stxs lines) #:transparent)
;;
;; All fields are (listof stx) except
;;  tag  is stx
;;  purpose and lines are (listof string)
;;  stub is sexp
;; Any field other than tag can be #f
;;

(define (parse-htdf tag-stx)  
  (let* ([tag-sexp   (syntax->datum tag-stx)]
         [in-list    (member tag-stx (stxs))]
         [sigs       (and (pair? in-list)
                          (takef (cdr in-list)
                                 (compose @signature? syntax->datum)))]
         [after-sigs (and (pair? sigs)
                          (next-stx (last sigs)))]

         [purpose    (cond [after-sigs   (lines-between (last sigs) after-sigs)]
                           [(pair? sigs) (lines-after (last sigs))]
                           [else '()])]

         [checks     (and after-sigs
                          (takef (member after-sigs in-list)
                                 (compose check? syntax->datum)))]

         [after-checks (and (pair? checks)
                            (next-stx (last checks)))]

         [stub         (cond [after-checks   (parse-stub (lines-between (last checks) after-checks))]
                             [(pair? checks) (parse-stub (lines-after (last checks)))]
                             [else #f])]

         [next-section-tag (findf (lambda (stx)
                                    (let ([sexp (syntax->datum stx)])
                                      (or (@htdf? sexp)
                                          (@htdd? sexp)    ;these really shouldn't
                                          (@htdw? sexp)))) ;happen but allow
                                  (cdr in-list))]

         [to-end           (and (pair? checks)
                                (if next-section-tag
                                    (takef (cdr (member (last checks) (stxs)))
                                           (lambda (stx) (not (eqv? stx next-section-tag))))
                                    (cdr (member (last checks) (stxs)))))]
         
         [origins          (and to-end (filter (compose @template-origin? syntax->datum) to-end))]         
         [templates        (and to-end (filter (compose @template?        syntax->datum) to-end))]         
         [fn-defns         (and to-end (filter (compose fn-defn?          syntax->datum) to-end))]

         [stxs             (if next-section-tag
                               (takef in-list (lambda (stx) (not (eqv? stx next-section-tag))))
                               in-list)]

         [lines            (cond [(pair? fn-defns) (lines-including tag-stx (last fn-defns))]
                                 [(pair? to-end)   (lines-including tag-stx (last to-end))]
                                 [(pair? in-list)  (lines-including tag-stx (last in-list))])])

    (htdf-design tag-stx sigs purpose checks stub origins templates fn-defns stxs lines)))


#|

#;
(define (foo x y)
  MTS)

;(define (foo x y) MTS)

|#
(define (parse-stub lines)
  (cond [(null? lines) #f]
        [else
         (if (or (regexp-match? #rx"^;\\(define" (car lines))
                 (regexp-match? #rx"^#;" (car lines)))
             (with-handlers ([exn:fail? (lambda (e) #f)])
               (call-with-input-string (trim-leading-comments-and-combine lines)
                                       read))
             (parse-stub (cdr lines)))]))


;; check a specific @htdf in the current file
(define (check-purpose design)
  (let* ([sigs   (htdf-design-sigs design)]
         [lines  (htdf-design-purpose design)]

         [2-semi-lines
          (takef (dropf lines (lambda (l) (regexp-match? #rx"^( *)$" l))) ;drop leading blanks
                 (lambda (l) (regexp-match? #rx"^;;" l)))]                ;take continguous ;; lines

         [correct?
          (and (pair? 2-semi-lines) 
               (regexp-match? #rx"^;; " (car 2-semi-lines))
               (or (null? (cdr 2-semi-lines))
                   (regexp-match? #rx"^;; CONSTRAINT:" (cadr 2-semi-lines))
                   (regexp-match? #rx"^;; !!!" (cadr 2-semi-lines))
                   (regexp-match? #rx"^;; (N|n)o tests for" (cadr 2-semi-lines))))])
 
    (score #f 'style 1
           (if correct? 1 0)
           '()
           (cons (message #f "Purpose properly formatted:~a" (if correct? " correct." " incorrect."))
                 (cond [(not (pair? sigs))   (list (message #f "No signature found, so could not find purpose."))]
                       [(null? 2-semi-lines) (list (message #f "No two semi-colon-space comment lines found after signature."))]
                       [(not correct?)       (map (lambda (l) (message #f l)) lines)]
                       [else '()])))))

(define (check-stub design)  
  (let* ([tag-stx  (htdf-design-tag design)]
         [htdf-sexp (syntax->datum tag-stx)]
         [sigs      (htdf-design-sigs design)]
         [sig-sexp  (and (pair? sigs) (syntax->datum (last sigs)))]         
         [stub-sexp (htdf-design-stub design)]

         [correct?
          (and (@signature? sig-sexp)
               (fn-defn?    stub-sexp)
               
               (eqv? (fn-defn-name stub-sexp) (cadr htdf-sexp))
               (= (length (fn-defn-parameters stub-sexp)) (length (signature-args sig-sexp)))
               (pair? (cddr stub-sexp)))])

    (score #f 'style 1
           (if correct? 1 0)
           '()
           (cons (message #f "Commented out stub:~a" (if correct? " correct." " incorrect."))
                 (cond [(not (@signature? sig-sexp))              (list (message #f "No signature found, so could not find stub."))]
                       [(not (pair? (htdf-design-checks design))) (list (message #f "No tests found, so could not find stub."))]
                       [else '()])))))

(define (trim-leading-comments-and-combine lines)
  (cond [(null? lines) ""]
        [else
         (string-append (cond [(regexp-match? #rx"^#;" (car lines)) (substring (car lines) 2)]
                              [(regexp-match? #rx"^;"  (car lines)) (substring (car lines) 1)]
                              [else (car lines)])
                        (trim-leading-comments-and-combine (cdr lines)))]))
               


(define (check-names stxs)
  (let ([bad-fn-names        (mutable-set)]
        [bad-param-names     (mutable-set)]
        [bad-constant-names  (mutable-set)]
        [bad-local-var-names (mutable-set)])

    (define (check-fn-name!        id) (unless (fn-name-ok?       id) (set-add! bad-fn-names id)))
    (define (check-param-name!     id) (unless (param-name-ok?    id) (set-add! bad-param-names id)))
    (define (check-constant-name!  id) (unless (constant-name-ok? id) (set-add! bad-constant-names id)))
    (define (check-local-var-name! id) (unless (param-name-ok?    id) (set-add! bad-local-var-names id)))

    (for ([stx stxs])
      (unless (let ([e (syntax-e stx)])
                (and (pair? e)
                     (eq? (syntax-e (car e)) '@template)
                     (not (checking-@template?))))
        (walk-form stx
                   '()
                   (lambda (kind stx e ctx env in-fn-defn recur)
                     ;;
                     ;; Normally we know that code that gets walked is syntactically correct,
                     ;; because it has been check-syntaxed. But here we are going to end up
                     ;; walking inside of @templates, which are unchecked.
                     ;;
                     (with-handlers ([exn:fail? (lambda (e) (void))])
                       
                       (walker-case kind
                                    [(value constant null bound free) '()]
                                    [(if cond and or #;define local #;local-define local-body lambda call) (recur)]
                                    [(define local-define)                               
                                     (let ([cadr-defn (syntax->datum (cadr e))])
                                       (cond [(pair? cadr-defn)
                                              (check-fn-name! (car cadr-defn))
                                              (map check-param-name! (cdr cadr-defn))
                                              (recur)]
                                             [(eqv? kind 'define)
                                              (check-constant-name! cadr-defn)
                                              (recur)]
                                             [(eqv? kind 'local-define)
                                              (check-local-var-name! cadr-defn)
                                              (recur)]))]))))))

    (let ([what (format "~a names"
                        (oxford-comma (reverse (cons-if (checking-local?) "local variable"
                                                        (cons-if (checking-struct?) "structure"
                                                                 (reverse '("Function" "parameter" "constant")))))))])
      

      (if (and (set-empty? bad-fn-names) (set-empty? bad-param-names) (set-empty? bad-constant-names) (set-empty? bad-local-var-names))
          (rubric-item 'style 1 what)
          (score #f 'style 1 0
                 '()
                 (remove* '(#t) 
                          (list (message #f (format "~a: incorrect." what))
                                (or (set-empty? bad-fn-names)        (name-is/are-bad-style "Function"  bad-fn-names))
                                (or (set-empty? bad-param-names)     (name-is/are-bad-style "Parameter" bad-param-names))
                                (or (set-empty? bad-constant-names)  (name-is/are-bad-style "Constant"  bad-constant-names))
                                (or (set-empty? bad-local-var-names) (name-is/are-bad-style "Constant"  bad-local-var-names)))))))))

(define (name-is/are-bad-style what which)
  (let ([lst (set->list which)])
    (message #f "~a name~a ~a bad style: ~a." what (plural lst) (is/are lst) (oxford-comma lst))))


(define (oxford-comma lo-string/symbol)
  (let ([lo-string (map (lambda (x) (if (string? x) x (symbol->string x))) lo-string/symbol)])
    (cond [(null? lo-string) ""]
          [(null? (cdr lo-string)) (format "~a" (car lo-string))]
          [(null? (cddr lo-string)) (format "~a and ~a" (car lo-string) (cadr lo-string))]
          [else
           (string-join lo-string ", " #:before-last ", and ")])))
    





(define SPD-WORDS 
  (set "fn" "lox" "loi" "lon" "los" "lot" "bst"
       "visited" "reachable" "encode" "butlast" "butfirst" "criteria" "int"
       "predicted" "email" "term" "lookup" "branches" "easiest" 
       "sublist"
       "escher"
       "foo" "bar" "baz" "bee" "cruft" "bizz" "buzz" "farfle"))

(define (fn-name-ok? id)
  (let ([str (symbol->string id)])
    (and (string-all-downcase? str)
         (not (string-has-underscore? str))
         (not (string-matches-is*? str)))))

(define (param-name-ok? id)
  (let ([str (symbol->string id)])
    (and (string-all-downcase? str)
         (not (string-has-underscore? str))
         (not (string-matches-is*? str)))))

(define (constant-name-ok? id)
  (let ([str (symbol->string id)])
    (and (string-all-upcase? str)
        (not (string-has-underscore? str))
        (not (string-matches-is*? str)))))

(define (type-name-ok? id)
  (let ([str (symbol->string id)])
    (and (string-mixed-case? str)
         (not (string-has-dash? str))
         (not (string-has-underscore? str))
         (not (string-matches-is*? str)))))


(define (string-all-downcase?  str) (string=? str (string-downcase str)))
(define (string-all-upcase?    str) (string=? str (string-upcase   str)))

(define (string-mixed-case? str)
  (let loop ([chars (string->list str)]
             [seen-lower? #f]
             [seen-upper? #f])
    (cond [(and seen-lower? seen-upper?) #t]
          [(null? chars) #f]
          [else
           (loop (cdr chars)
                 (or seen-lower? (char-lower-case? (car chars)))
                 (or seen-upper? (char-upper-case? (car chars))))])))

(define (string-has-dash?       str) (regexp-match? ".*-.*" str))
(define (string-has-underscore? str) (regexp-match? ".*_.*" str))
(define (string-matches-is*?    str) (and (regexp-match? #rx"^is.*?$" str)
                                          (not (regexp-match? #rx"^is-" str))))


#|
(define (in-teaching-lang? x) #f)

(define (lower-caravan-case? str)
  (and (string=? str (string-downcase str))
       (let ([splits
              (map (lambda (s) (string-trim s #rx"[-,\\?,\\=,\\>,\\<]" #:repeat? #t))
                   (remove ""
                           (string-split str #rx"-|//")))])
         (or (null? (cdr splits))
             (andmap (lambda (s)
                       (or (word? s)
                           (and (ends-in-s? s)
                                (word? (drop-last s)))))
                     splits)))))

(define (ends-in-s? s) (string=? (substring s (- (string-length s) 1)) "s"))
(define (drop-last  s)           (substring s 0 (- (string-length s) 1)))

(define (upper-camel-case? str)
  (and (string=? str (string-upcase str))
       #t)) ;!!!

(define (word? str)
  (or ;(<= (string-length str) 3)
     ;(set-member? SPD-WORDS str)
   (set-member? WORDS str)
))
|#

#|

There are a couple of funny things that can happen in terms of finding text after and
between two top-level syntax:

One is end-of-line comments. So for example, in

(@signature ...) ;useful comment
;; this is the purpose

we need to start looking for the purpose at the second line, not at ;useful comment

Another is dangling top-level syntax elements. So for example:

(check-expect (...) (...)) e
(check-expect (...) (...))

This second is a design rule error, so we don't worry about it confusing us.

The basic strategy is that

lines-from
  will generate a synthetic line in the odd case where it is called
  with with a top-level syntax element that doesn't begin at the
  beginning of a line

lines-after
  produces a result that always starts on the complete line after the
  last line of the top level syntax element


|#



(define (lines-between a-stx b-stx)
  (let* ([a-line-num (syntax-line a-stx)]
         [b-line-num (syntax-line b-stx)]

         [lines-from-a  (lines-from a-stx)]
         [lines-after-a (lines-after a-stx)])

    (if (= a-line-num b-line-num)
        '()    
        (take lines-after-a
              (- b-line-num a-line-num (- (length lines-from-a) (length lines-after-a)))))))

(define (lines-including a-stx b-stx)
  (let* ([a-line-num (syntax-line a-stx)]
         [b-line-num (syntax-line b-stx)]

         [lines-from-a  (lines-from a-stx)]
         [lines-after-b (lines-after b-stx)])

    (take lines-from-a
          (- (length lines-from-a) (length lines-after-b)))))
  

(define (lines-from stx)
  (let ([lines (drop (lines) (sub1 (syntax-line stx)))]
        [col   (syntax-column stx)])
    (if (zero? col)
        lines
        (cons (substring (car lines) col) (cdr lines)))))

(define (lines-after stx)
  (let loop ([lines (lines-from stx)]
             [to-skip (syntax-span stx)])
    (if (null? lines)
        '()
        (let ([line (car lines)])
          (cond [(< to-skip 0) (error 'lines-after "to-skip is less than zero")]
                [(= to-skip 0) lines]
                [(<= to-skip (string-length line)) (cdr lines)]  ;see note above, we do not make
                ;;                                               ;synthetic (substring line to-skip)
                [else
                 (loop (cdr lines)
                       (- to-skip (string-length line) 1))])))))

(define (next-stx stx)
  (let ([mem (member stx (stxs))])
    (and (pair? (cdr mem)) (cadr mem))))



(define (cons-if q x lst)
  (if q (cons x lst) lst))
