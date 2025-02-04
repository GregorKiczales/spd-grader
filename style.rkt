#lang racket

(require racket/set
         spd-grader/defs
         spd-grader/utils
         spd-grader/walker
         spd-grader/score
         spd-grader/file-structure
         spd-grader/harness
        ;spd-grader/wordlist
         )

(provide grade-style
         check-style
         check-htdf-style)


(struct htdf-design (tag sigs purpose checks stub origins templates fn-defns lines) #:transparent)
;;
;; All fields are (listof stx) except
;;  tag  is stx
;;  purpose, stub and lines are (listof string)
;;




;;
;; @htdf
;; all design elements present and in proper order
;;  one-line purpose
;;  at least two tests
;;  stub has single ; and space
;; function and parameter names in lower-caravan-case
;;

(define (grade-style)
  (check-style))

(define (check-style)
  (header "Style rules:"
    (combine-scores
     (weights* 1.0 '(*)
       (map check-htdf-style
            (filter (lambda (stx) (@htdf? (syntax->datum stx)))
                    (stxs)))))))

(define (check-htdf-style tag-stx)
  (let ([design (parse-htdf  tag-stx)])
    (header (format "Style rules for ~a:" (syntax->datum tag-stx))
      (weights (*)
        (check-2-semi-comments (htdf-design-lines design))
        (check-purpose tag-stx)
        (check-stub    tag-stx)
        (check-names   tag-stx)))))

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
                                 (format " incorrect, ~a is not." (length bad-lines))]))
                 (map (lambda (bl) 
                        (message #f "  ~a" (cdr bl)))
                      bad-lines)))))



;; Looks for design elements to be in order (in place). Will deliberately
;; miss elements that are out of place.
;;
;; (@htdf foo)
;; (@signature
;; <purpose line(s)>
;;
;; <checks>
;;
;; <allows origins, templates, and definitions to be grouped or ungrouped>
(define (parse-htdf tag-stx)  
  (let* ([tag-sexp   (syntax->datum tag-stx)]
         [in-list    (member tag-stx (stxs))]
         [sigs       (and (pair? in-list)
                          (takef (cdr in-list)
                                 (compose @signature? syntax->datum)))]
         [after-sigs (and (pair? sigs)
                          (next-stx (last sigs)))]

         [purpose    (cond [(and (pair? sigs) after-sigs) (lines-between (last sigs) after-sigs)]
                           [     (pair? sigs)             (lines-after (last sigs))]
                           [else '()])]

         [checks     (and after-sigs
                          (takef (member after-sigs in-list)
                                 (compose check? syntax->datum)))]

         [after-checks (and (pair? checks)
                            (next-stx (last checks)))]

         [stub         (cond [(and (pair? checks) after-checks) (lines-between (last checks) after-checks)]
                             [     (pair? checks)               (lines-after (last checks))]
                             [else '()])]

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
         
         [origins          (and to-end
                                (filter (compose @template? syntax->datum) to-end))]
         
         [templates        (and to-end
                                (filter (compose @template? syntax->datum) to-end))]
         
         [fn-defns         (and to-end
                                (filter (compose fn-defn?   syntax->datum) to-end))]

         [lines            (and (pair? fn-defns)
                                (lines-including tag-stx (last fn-defns)))])

    (htdf-design tag-stx sigs purpose checks stub origins templates fn-defns lines)))




;; check a specific @htdf in the current file
(define (check-purpose tag-stx)
  (let* ([tag-sexp (syntax->datum tag-stx)]
         [design (parse-htdf tag-stx)]
         [sigs   (htdf-design-sigs design)]
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
                       [(not (null? (cdr 2-semi-lines))) (list (message #f "Possible multi-line purpose found."))]
                       [else '()])))))

(define (check-stub tag-stx)  
  (let* ([htdf-sexp  (syntax->datum tag-stx)]
         [design     (parse-htdf tag-stx)]
         [sig-sexp   (syntax->datum (last (htdf-design-sigs design)))]
         [stub-lines (htdf-design-stub design)]
         #|

         #;
         (define (foo x y)
            MTS)

         ;(define (foo x y) MTS)

         |#
         [stub-sexp (find-stub stub-lines)]

         [correct?
          (and (fn-defn? stub-sexp)
               (eqv? (fn-defn-name stub-sexp) (cadr htdf-sexp))
               (= (length (fn-defn-parameters stub-sexp)) (length (signature-args sig-sexp)))
               (pair? (cddr stub-sexp)))])

    (score #f 'style 1
           (if correct? 1 0)
           '()
           (list (message #f "Commented out stub:~a"
                          (if correct? " correct." " incorrect."))))))

(define (find-stub lines)
  (cond [(null? lines) #f]
        [else
         (if (or (regexp-match? #rx"^;\\(define" (car lines))
                 (regexp-match? #rx"^#;" (car lines)))
             (with-handlers ([exn:fail? (lambda (e) #f)])
               (call-with-input-string (trim-leading-comments-and-combine lines)
                                       read))
             (find-stub (cdr lines)))]))

(define (trim-leading-comments-and-combine lines)
  (cond [(null? lines) ""]
        [else
         (string-append (cond [(regexp-match? #rx"^#;" (car lines)) (substring (car lines) 2)]
                              [(regexp-match? #rx"^;"  (car lines)) (substring (car lines) 1)]
                              [else (car lines)])
                        (trim-leading-comments-and-combine (cdr lines)))]))
               


(define (check-names stx)
  (let ([bad-fn-names        (mutable-set)]
        [bad-param-names     (mutable-set)]
        [bad-constant-names  (mutable-set)]
        [bad-local-var-names (mutable-set)])

    (define (check-fn-name!        id) (unless (fn-name-ok?       id) (set-add! bad-fn-names id)))
    (define (check-param-name!     id) (unless (param-name-ok?    id) (set-add! bad-param-names id)))
    (define (check-constant-name!  id) (unless (constant-name-ok? id) (set-add! bad-constant-names id)))
    (define (check-local-var-name! id) (unless (param-name-ok?    id) (set-add! bad-local-var-names id)))

    (walk-form stx
               '()
               (lambda (kind stx e ctx env in-fn-defn recur)
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
                                        (recur)]))])))

    (header "Naming:"
      (score #f 'style 1
             (if (and (set-empty? bad-fn-names) (set-empty? bad-param-names) (set-empty? bad-constant-names) (set-empty? bad-local-var-names)) 1 0)
             '()
             (remove* '(#t) 
                      (list (or (set-empty? bad-fn-names)        (name-is/are-bad-style "Function"  bad-fn-names))
                            (or (set-empty? bad-param-names)     (name-is/are-bad-style "Parameter" bad-param-names))
                            (or (set-empty? bad-constant-names)  (name-is/are-bad-style "Constant"  bad-constant-names))
                            (or (set-empty? bad-local-var-names) (name-is/are-bad-style "Constant"  bad-local-var-names))))))))

(define (name-is/are-bad-style what which)
  (let ([lst (set->list which)])
    (message #f "~a name~a ~a bad style: ~a." what (plural lst) (is/are lst) (oxford-comma lst))))


(define (oxford-comma lo-symbol)
  (let ([lo-string (map symbol->string lo-symbol)])
    (cond [(null? (cdr lo-string)) (format "~a" (car lo-string))]
          [(null? (cddr lo-string)) (format "~a and ~a" (car lo-string) (cadr lo-string))]
          [else
           (string-join lo-string ", " #:before-last " and ")])))
    





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
          [(empty? chars) #f]
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



(define (lines-between a-stx b-stx)
  (let* ([a-line-num (syntax-line a-stx)]
         [b-line-num (syntax-line b-stx)]

         [lines-from-a  (lines-from a-stx)]
         [lines-after-a (lines-after a-stx)])

    (take lines-after-a
          (- b-line-num a-line-num (- (length lines-from-a) (length lines-after-a))))))

(define (lines-including a-stx b-stx)
  (let* ([a-line-num (syntax-line a-stx)]
         [b-line-num (syntax-line b-stx)]

         [lines-from-a  (lines-from a-stx)]
         [lines-after-b (lines-after b-stx)])

    (take lines-from-a
          (- (length lines-from-a) (length lines-after-b)))))
  

(define (lines-from stx)
  (drop (lines) (sub1 (syntax-line stx))))

(define (lines-after stx)
  (let loop ([lines (lines-from stx)]
             [to-skip (syntax-span stx)])
    (cond [(null? lines) '()]
          [(<= to-skip 0) lines]
          [else
           (loop (cdr lines)
                 (- to-skip (string-length (car lines)) 1))])));1 extra for newline

(define (next-stx stx)
  (let ([mem (member stx (stxs))])
    (and (> (length mem) 1) (cadr mem))))



;; !!! for development


(define (in-110-materials fn) (check-in fn 110-MATERIALS #rx".*-(starter|solution).rkt"))
(define (in-110-starters  fn) (check-in fn 110-MATERIALS #rx".*-starter.rkt"))
(define (in-110-solutions fn) (check-in fn 110-MATERIALS #rx".*-solution.rkt"))

(define (in-spd-materials fn) (check-in fn SPD-MATERIALS #rx".*-(starter|solution).rkt"))
(define (in-spd-starters  fn) (check-in fn SPD-MATERIALS #rx".*-starter.rkt"))
(define (in-spd-solutions fn) (check-in fn SPD-MATERIALS #rx".*-solution.rkt"))

(define (check-in fn root regexp) (map fn (filter not-wxme-file? (find root regexp))))

(define (not-wxme-file? p)
  (call-with-input-file* p
    (lambda (in)
      (not (regexp-match? #rx"wxme" (read-line in))))))



(define (check-style-in-file p)
  (parameterize ([stxs  #f]
                 [lines #f]
                 [elts  #f])

    (stxs  (read-syntaxes p))
    (lines (file->lines p))
    (elts  (parse-elts (stxs) (lines)))
    (displayln "--------")
    (display-score (header (format "Style rules for ~a:" p) (check-style)) (current-output-port) #t)))




(define (find path regexp)
  (cond [(member (file-or-directory-type path) '(file)) (if (regexp-match? regexp path) (list path) '())]
        [(member (file-or-directory-type path) '(directory directory-link link))
         (foldr append '() (map (lambda (p) (find p regexp)) (directory-list path #:build? #true)))]
        [else '()]))


(define (cons-if q x lst)
  (if q (cons x lst) lst))




