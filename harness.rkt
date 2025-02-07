#lang racket

(require spd-grader/grader
         spd-grader/style)

(provide grade-some-starters grade-some-solutions
         grade-exam-starters grade-exam-solutions
         grade-spd-starters  grade-spd-solutions
         grade-110-starters  grade-110-solutions   ;!!! factor this 110 stuff out of spd
         grade-one
         grade-tree SPD-MATERIALS 110-MATERIALS) ;!!! during development of style.rkt

(define SPD-MATERIALS (build-path (find-system-path 'home-dir) "spd-materials"))
(define 110-MATERIALS (build-path (find-system-path 'home-dir) "110-materials"))
(define EXAMS         (build-path 110-MATERIALS "exams/"))


(define (grade-some-starters  [verbose? #f]) (grade-some #rx".*-starter\\.rkt$"  verbose? grade-not-0?))
(define (grade-some-solutions [verbose? #f]) (grade-some #rx".*-solution\\.rkt$" verbose? grade-not-1?))

(define (grade-exam-starters  [verbose? #f]) (grade-tree EXAMS #rx".*-starter\\.rkt$"   verbose? grade-not-0?))
(define (grade-exam-solutions [verbose? #f]) (grade-tree EXAMS #rx".*-solution\\.rkt$"  verbose? grade-not-1?))

(define (grade-spd-starters   [verbose? #f]) (grade-tree SPD-MATERIALS #rx".*-starter\\.rkt$"  verbose? grade-not-0?))
(define (grade-spd-solutions  [verbose? #f]) (grade-tree SPD-MATERIALS #rx".*-solution\\.rkt$" verbose? grade-not-1?))
(define (grade-110-starters   [verbose? #f]) (grade-tree 110-MATERIALS #rx".*-starter\\.rkt$"  verbose? grade-not-0?))
(define (grade-110-solutions  [verbose? #f]) (grade-tree 110-MATERIALS #rx".*-solution\\.rkt$" verbose? grade-not-1?))

(define (grade-not-1? g) (or (not (number? g)) (not (= g 1.0))))
(define (grade-not-0? g) (or (not (number? g)) (not (= g 0.0))))

(define (grade-one path [out (current-output-port)])  
  (autograde-file path #f #f out displayln))

(define (grade-some regexp [verbose? #f] [warn? grade-not-1?])
  (grade-listof-path (directory-list "." #:build? #t) regexp verbose? warn?))


(define (grade-tree dir regexp [verbose? #f] [warn? grade-not-1?])
  
  (define (find path)
    (cond [(member (file-or-directory-type path) '(file)) (list path)]
          [(member (file-or-directory-type path) '(directory directory-link link))
           (foldr append '() (map (lambda (p) (find p)) (directory-list path #:build? #true)))]
          [else '()]))
    
  (grade-listof-path (find dir) regexp verbose? warn?))

(define (grade-listof-path paths regexp [verbose? #f] [warn? grade-not-1?])
  (let ([paths-with-grader (filter (lambda (p) (and (regexp-match? regexp p) (has-grader? p))) paths)])
    (displayln (format "Grading ~a files"
                       (length paths-with-grader)))
    (flush-output)
    (parameterize ([verbose-error-logging? #t])
      (for [(path paths-with-grader)]
        (cond [verbose?
               (display   (format " ~a: " path)) (flush-output)
               (displayln (format "~a" (grade-one path (open-output-nowhere "grade-some")))) (flush-output)]
              [else
               (let ([grade (grade-one path (open-output-nowhere "grade-some"))])
                 (when (warn? grade)
                   (displayln (format " ~a: ~a" path grade))))])))))

(define (in-110-materials fn) (check-in fn 110-MATERIALS #rx".*-(starter|solution).rkt"))
(define (in-110-starters  fn) (check-in fn 110-MATERIALS #rx".*-starter.rkt"))
(define (in-110-solutions fn) (check-in fn 110-MATERIALS #rx".*-solution.rkt"))

(define (in-spd-materials fn) (check-in fn SPD-MATERIALS #rx".*-(starter|solution).rkt"))
(define (in-spd-starters  fn) (check-in fn SPD-MATERIALS #rx".*-starter.rkt"))
(define (in-spd-solutions fn) (check-in fn SPD-MATERIALS #rx".*-solution.rkt"))

(define (check-in fn root regexp) (for ([p (find root regexp)])
                                    (when (not-wxme-file? p)
                                      (fn p))))

(define (not-wxme-file? p)
  (call-with-input-file* p
    (lambda (in)
      (not (regexp-match? #rx"wxme" (read-line in))))))



(define (check-style/file p options [only-display-zeroes? #f])
  (parameterize ([stxs  #f]
                 [lines #f]
                 [elts  #f])
    (stxs  (read-syntaxes p))
    (lines (file->lines p))
    (elts  (parse-elts (stxs) (lines)))

    (let ([s (header (format "Style rules for ~a:" p) (check-style options))])
      (when (or (not only-display-zeroes?)
                (not (= (score-m s) 1)))
        (display-score s (current-output-port) #t)))))




(define (find path regexp)
  (cond [(member (file-or-directory-type path) '(file)) (if (regexp-match? regexp path) (list path) '())]
        [(member (file-or-directory-type path) '(directory directory-link link))
         (foldr append '() (map (lambda (p) (find p regexp)) (directory-list path #:build? #true)))]
        [else '()]))




