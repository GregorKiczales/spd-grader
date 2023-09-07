#lang racket

(require (for-syntax racket/list)
         "utils.rkt"
         "defs.rkt")

(provide score
         score-m        ;only score selector exported
         message
         score-it
         rubric-item 
         header
         combine-scores
         weights
         weights+
         weights*
         total-marks
         score-max
         score-min
         score-*
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



(define (header text s)
  (score #t 'other (score-w s) (score-m s) (score-subs s) (list (message #f text))))


(define (combine-scores scores)
  (let ([scores (filter score? scores)]);remove null-scores ;!!! not used?
    (score #f 1 (total-marks scores) scores '())))


(define-syntax (weights stx)
  (syntax-case stx (*)
    [(_ (w ... *) s ...) #`(combine-scores (weights* 1.00 '#,(parse-weights stx #'(w ... *)) (list s ...)))]
    [(_ (w ...  ) s ...) #`(combine-scores (weights* 1.00 '#,(parse-weights stx #'(w ... ))  (list s ...)))]))

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

  

(define-syntax (weights+ stx)
  (syntax-case stx ()
    [(_ . exprs)
     (let* ([stxs (syntax-e #'exprs)]
            [es (map syntax-e stxs)])
       (check-well-formed-weights+ stx stxs)
       #`(weights #,(map car es)
                  #,@(map cadr es)))]))

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



(define (weights* left low los)
  (let loop ([left left] [low low] [los (filter score? los)])
    (cond [(and (empty? low) (empty? los)) '()]
          [(empty? low) (error 'weights/weights* "More scores than weights and weights does not end with *.")]
          [(empty? los) (error 'weights/weights* "More weights than scores.")]
          [(eqv? (car low) '*) (map (curry weight (/ left (length los))) los)]
          [else
           (cons (weight (car low) (car los))
                 (loop (- left (car low))
                       (rest low)
                       (rest los)))])))
         

(define (total-marks scores)
  (let* ([scores (filter score? scores)]
         [total-weight (foldl + 0 (map score-w scores))])
    (unless (= 1 (round* total-weight 2))
      (error (format "Scores do not total to a weight of 1. (Totals to ~a.)" total-weight)))
    (round* (foldl + 0 (map * (map score-w scores) (map score-m scores)))
            2)))

(define (weight n item)
  (struct-copy %score item [w n]))


(define (score-max . scores) (apply score-max/min >= scores))
(define (score-min . scores) (apply score-max/min <= scores))

(define (score-max/min comp . scores)
  (let ([scores (filter score? scores)])
    (foldl (lambda (s1 s2)
             (cond [(not (= (score-w s1) (score-w s2)))
                    (error (format "~a (weighted ~a) and ~a (weighted ~a) should have same weight."
                                   (score-m s1) (score-w s1)
                                   (score-m s2) (score-w s2)))]
                   [(comp (score-m s1) (score-m s2)) s1]
                   [else s2]))
           (car scores)
           (cdr scores))))

(define (score-* percent s)
  (score (score-header? s)
         (score-topic s)
         (score-w s)
	 (* percent (score-m s))
         (score-subs s)
	 (score-msgs s)))



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
    (let [(new-%total (* (score-w s) %total))
          (new-indent (string-append indent " "))]
      (cond [(or (not (early?)) (score-report-early? s))
             (walk/lomsg (score-msgs s) (score-header? s) (score-m s) new-%total indent new-indent #t)
             (list new-%total (score-m s) (walk/los (score-subs s) new-%total new-indent))]
            [else
             (walk/los (score-subs s) new-%total new-indent)])))

  (define (walk/los los %total indent)
    (cond [(empty? los) '()]
          [else
           (cons (walk/s (car los) %total indent)
                 (walk/los (cdr los) %total indent))]))

  (define (walk/lomsg lomsg header? %mark %total old-indent new-indent first?)
    (cond [(empty? lomsg) '()]
          [else
           (walk/msg (first lomsg) header? %mark %total old-indent new-indent first?)
           (walk/lomsg (cdr lomsg) header? %mark %total old-indent new-indent #f)]))

  (define (walk/msg msg header? %mark %total old-indent new-indent first?)
    (when (if (msg-v? msg) (verbose?) #t)
      (displayln (if (and first? (not (early?)) (not (zero? %total)))
                     ;; first message in a score gets old-indent and then display the score information
                     (format "~a  of ~a ~a~a~a ~a"
                             (format-percent (* %mark %total))
                             (format-percent %total)
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
