#lang racket

(require (for-syntax racket/list)
         "utils.rkt"
         "defs.rkt")

(provide score
         score-m        ;only score selector exported
         message
         score-it
         rubric-item
        ;rubric
         
         reduce-it
         
         header
         combine-scores
         weights
         weights+
         weights*
        ;total-marks
         score-max
         score-min
         score-*
         display-overall-grade
         display-score)

;; Scores
;;
;
;;
;; (score <header-score?>
;;        (one-of TOPICS)
;;        <weight>           ;percentage of enclosing score this score contributes  
;;        <marks>            ;percentage of its weight this score has
;;        (list-of Score)
;;        (list-of Message))

(struct score (header? topic w m subs msgs) #:transparent #:name %score #:constructor-name %score)

(struct reduction (topic w correct? msg) #:transparent)

;; !!! temporary backward compatibility for old arguments
(define (score . args)
  (cond [(= (length args) 5)
         (apply %score (car args) 'other (cdr args))]
        [(void? (cadr args))
         (raise-argument-error 'topic "not (void)" (cadr args))]
        [else
         (apply %score args)]))

(struct msg (v? str) #:transparent)
;; Message is (message Boolean String)
;;    v? is boolean, #f -> only include in concise
;;                   #t -> include in concise and verbose

(define (message verb? form . vs)
  (msg verb? (apply format form vs)))

(define (score-it topic w m v? fmt-ctl . fmt-args)
  (score #f topic w m '() (list (msg v? (apply format fmt-ctl fmt-args)))))

(define (rubric-item topic correct? fmt-ctl . fmt-args)
  (if correct?
      (score #f topic 1 1 '() (list (message #f "~a: correct."   (apply format fmt-ctl fmt-args))))
      (score #f topic 1 0 '() (list (message #f "~a: incorrect." (apply format fmt-ctl fmt-args))))))

(define (reduce-it topic w correct? fmt-ctl . fmt-args)  
  (if correct?
      (reduction topic w correct? (message #f "Reduction not applied: ~a." (apply format fmt-ctl fmt-args)))    ;display-score hides these
      (reduction topic w correct? (message #f "Reduction: ~a."             (apply format fmt-ctl fmt-args)))))

#;
(define-syntax (rubric stx)
  (syntax-case stx ()
    [(_ TOPIC PREFIX [Q V T] ...)
     #'(rubric TOPIC PREFIX [Q V T] ... [else "correct"])]
    
    [(_ TOPIC PREFIX [Q V T] ... [else Te])
     #'(let* ([max-score (+ V ...)]
              [items (list (list V (lambda () Q) T) ...)]
              [applied (filter (lambda (i) (not ((cadr i)))) items)])
         (if (null? applied)
             (score-it 'TOPIC 1 1 #f "~a: ~a." PREFIX Te)
             (score #f
                    'TOPIC
                    1
                    (/ (- max-score (foldl + 0 (map car applied))) max-score)
                    '()
                    (cons (message #f "~a: incorrect." PREFIX); #f Te)
                          (map (lambda (item) (message #t (format "~a: ~a." PREFIX (caddr item))))
                               applied)))))]))


(define (header text s)
  (score #t 'other (score-w s) (score-m s) (score-subs s) (list (message #f text))))


(define-syntax (weights stx)
  (syntax-case stx (*)
    [(_ (w ... *) s ...) #`(combine-scores (weights* 1.00 '#,(parse-weights stx #'(w ... *)) (list s ...)))]
    [(_ (w ...  ) s ...) #`(combine-scores (weights* 1.00 '#,(parse-weights stx #'(w ... ))  (list s ...)))]))

(define-syntax (weights+ stx)
  (syntax-case stx ()
    [(_ . exprs)
     (let* ([stxs (syntax-e #'exprs)]
            [es (map syntax-e stxs)])
       (check-well-formed-weights+ stx stxs)
       #`(weights #,(map car es)
           #,@(map cadr es)))]))

(define (weights* left low los)
  (let loop ([left left] [low low] [los (filter (lambda (x) (or (score? x) (reduction? x))) los)])
    (cond [(and (not (null? los))
                (reduction? (car los)))
           (cons (car los)
                 (loop left low (cdr los)))]
          [(and (null? low) (null? los)) '()]
          [(null? low) (error 'weights/weights* "More scores than weights and weights does not end with *.")]
          [(null? los) (error 'weights/weights* "More weights than scores.")]
          [(eqv? (car low) '*) (map (curry weight (/ left (length los))) los)]
          [else
           (cons (weight (car low) (car los))
                 (loop (- left (car low))
                       (cdr low)
                       (cdr los)))])))

(define (weight n item)
  (struct-copy %score item [w n]))


(define (combine-scores all)
  (let* ([scores                 (filter score? all)]
         [reductions             (filter reduction? all)]
         [applied-reductions     (filter (lambda (r) (not (reduction-correct? r))) reductions)]
         
         [score-total-weight     (foldl + 0 (map score-w     scores))]
         [reduction-total-weight (foldl + 0 (map reduction-w reductions))])
    
    (unless (= (round* score-total-weight 2) 1)
      (error* "Scores do not total to a weight of 1. (Totals to ~a.)" score-total-weight))
    (unless (<= (round* reduction-total-weight 2) 1)
      (error* "Reductions total to more than 1. (Total to ~a.)" reduction-total-weight))

    (score #f 1
           (round* (max 0
                        (- (foldl + 0 (map * (map score-w scores) (map score-m scores)))
                           (foldl + 0 (map reduction-w applied-reductions))))
                   2)
           all
           '())))



(define-for-syntax (parse-weights stx los)
  ;; los is #'(list Num | * | (@ Natural Num))
  (let loop ([los (syntax->list los)])
    (if (null? los)
        '()
        (let* ([s (car los)]
               [e (syntax-e s)])
          (cond [(number? e) (cons e (loop (cdr los)))]
                [(eqv? e '*)
                 (if (null? (cdr los))
                     '(*)
                     (raise-syntax-error #f "Can only use * in the last position in a list of scores." stx s))]
                [(and (pair? e)
                      (= (length e) 3)
                      (eqv? (syntax-e (car e)) '@)
                      (number? (syntax-e (cadr e))))
                 (let [(n (syntax-e (cadr e)))
                       [w (caddr e)]]
                   (append (apply append (make-list n (parse-weights stx #`(#,w))))
                           (loop (cdr los))))] 
                [else
                 (raise-syntax-error #f "Expecting a percentage weight, (@ Natural <weight>), or *." stx s)])))))

(define-for-syntax (check-well-formed-weights+ stx stxs)
  (let loop ([stxs stxs])
    (or (null? stxs)
        (let ([last? (null? (cdr stxs))]
              [e (syntax-e (car stxs))])
          (cond [(not (pair? e))
                 (if last?
                     (raise-syntax-error #f "Expected [<number-or-*> <expression>]" stx (car stxs))
                     (raise-syntax-error #f "Expected [<number> <expression>]" stx (car stxs)))]
                [(number? (syntax-e (car e)))             (loop (cdr stxs))]
                [last?
                 (or (eqv? (syntax-e (car e)) '*)
                     (raise-syntax-error #f "Expected number or *." stx (car e)))]
                [else
                 (raise-syntax-error #f "Expected number." stx (car e))])))))


(define (score-max . scores) (score-max/min 'score-max >= scores))
(define (score-min . scores) (score-max/min 'score-min <= scores))

(define (score-max/min who comp scores)
  (unless (pair? scores)
    (error who "called with no scores"))
  
  (let loop ([scores (filter score? scores)]
             [rsf (first scores)])
    (cond [(null? scores) rsf]
          [(not (= (score-w rsf) (score-w (car scores))))
           (error who "not all scores have same weight")]
          [else
           (loop (cdr scores)
                 (if (comp (score-m rsf) (score-m (car scores)))
                     rsf
                     (car scores)))])))

(define (score-* percent s)
  (score (score-header? s)
         (score-topic s)
         (score-w s)
	 (* percent (score-m s))
         (score-subs s)
	 (score-msgs s)))


(define (display-overall-grade n msg rpt)
  (displayln/f (format "~%~%~%AUTOGRADING GRADE:   ~a    (out of 100)~%~%~a" n msg) rpt))


(define (display-score s out internal?)

  (define (score-report-early? s)
    (or (score-header? s)
        (member (score-topic s) EARLY-REPORT-TOPICS)))
  ;;
  ;; walk displays the pretty score as it goes; also produces a list for pretty-printing
  ;;
  ;; %total (% of total) is Number
  ;; percent of total assignment the current score contributes
  ;; when arriving at walk/s, %total is the weight of the enclosing s
  ;;
  (define (walk/s s %total indent)
    (cond [(score? s)
           (let [(new-%total (* (score-w s) %total))
                 (new-indent (string-append indent " "))]
             (cond [(and (score? s) (or (not (early?)) (score-report-early? s)))
                    (walk/lomsg (score-msgs s) (score-header? s) (score-m s) new-%total indent new-indent #t)
                    (list new-%total (score-m s) (walk/los (score-subs s) new-%total new-indent))]
                   [else
                    (walk/los (score-subs s) new-%total new-indent)]))]
          [(reduction? s)
           (let [(new-%total (* (reduction-w s) %total))
                 (new-indent (string-append indent " "))]
             (cond [(not (reduction-correct? s))
                    (walk/msg (reduction-msg s) #f 0 (reduction-w s) indent new-indent #t #f)
                    (list new-%total 0 '())]
                   [else
                    (list new-%total new-%total '())]))]))

  (define (walk/los los %total indent)
    (cond [(null? los) '()]
          [else
           (cons (walk/s (car los) %total indent)
                 (walk/los (cdr los) %total indent))]))

  (define (walk/lomsg lomsg header? %mark %total old-indent new-indent first?)
    (cond [(null? lomsg) '()]
          [else
           (walk/msg (first lomsg) header? %mark %total old-indent new-indent first? #t)
           (walk/lomsg (cdr lomsg) header? %mark %total old-indent new-indent #f)]))

  (define (walk/msg msg header? %mark %total old-indent new-indent first? include-%of?)
    (when (if (msg-v? msg) (verbose?) #t)
      (displayln (if (and first? (not (early?)) (not (zero? %total)))
                     ;; first message in a score gets old-indent and then display the score information
;                     (format "~a  of ~a ~a~a~a ~a"
;                             (format-percent (* %mark %total))
                     ;                             (format-percent %total)
                     (format "~a ~a~a~a ~a"
                             (if include-%of?
                                 (format "~a  of ~a"
                                         (format-percent (* %mark %total))
                                         (format-percent %total))
                                 (format "           ~a"
                                         (format-percent %total)))
                             (if (= %mark 1) "   " (if header? " - " " x "))
                             old-indent
                             (msg-str msg)
                             (if (and header? (= %mark 1)) "correct." ""))
                     ;; all other messages are new-indent, no score
                     (format "                    ~a~a"
                             new-indent
                             (msg-str msg)))
                 out)))

  (define (format-percent p)
    (format " ~a%" (~r (* 100 p) #:precision 1 #:notation 'positional #:min-width 4)))
  
  (let [(internal (walk/s s 1 ""))] ;by side-effect has displayed report
    (when internal?
      (displayln (format "~%~%~a" INTERNAL-DATA-LINE) out) 
      (pretty-print internal out))))
