#lang racket

(require "grader.rkt")

(provide grade-all-starters  grade-all-solutions
         grade-some-starters grade-some-solutions
         grade-one)

(define MATERIALS (build-path (find-system-path 'home-dir) "spd-materials/"))
(define EXAMS     (build-path MATERIALS "exams/"))


(define (grade-some-starters  [verbose? #f]) (grade-some #rx".*-starter\\.rkt$"  verbose?))
(define (grade-some-solutions [verbose? #f]) (grade-some #rx".*-solution\\.rkt$" verbose?))
(define (grade-many-starters  [verbose? #f]) (grade-many #rx".*-starter\\.rkt$"  verbose?))
(define (grade-many-solutions [verbose? #f]) (grade-many #rx".*-solution\\.rkt$" verbose?))
(define (grade-exam-starters  [verbose? #f]) (grade-tree EXAMS #rx".*-starter\\.rkt$"   verbose?))
(define (grade-exam-solutions [verbose? #f]) (grade-tree EXAMS #rx".*-solution\\.rkt$"  verbose?))
(define (grade-all-starters   [verbose? #f]) (grade-tree MATERIALS #rx".*-starter\\.rkt$"  verbose?))
(define (grade-all-solutions  [verbose? #f]) (grade-tree MATERIALS #rx".*-solution\\.rkt$" verbose?))


(define (grade-one path [out (current-output-port)])  
  (autograde-file path #f #f out displayln))

(define (grade-some regexp [verbose? #f])
  (grade-listof-path (directory-list "." #:build? #t) regexp verbose?))

(define (grade-many regexp [verbose? #f])
  (grade-listof-path (foldr append
                            '()
                            (map (lambda (dir)
                                   (directory-list (build-path MATERIALS dir) #:build? #t))
                                 '("psets" "lectures" "labs" "bank")))
                     regexp
                     verbose?))


(define (grade-tree dir regexp [verbose? #f])
  
  (define (find path)
    (cond [(member (file-or-directory-type path) '(file link)) (list path)]
          [(member (file-or-directory-type path) '(directory directory-link))
           (foldr append '() (map (lambda (p) (find p)) (directory-list path #:build? #true)))]
          [else '()]))
    
  (grade-listof-path (find dir) regexp verbose?))

(define (grade-listof-path paths regexp [verbose? #f])
  (let ([paths-with-grader (filter (lambda (p) (and (regexp-match? regexp p) (has-grader? p))) paths)])
    (displayln (format "Grading ~a files~a"
                       (length paths-with-grader)
                       (if verbose? "..." ", showing scores less than 1.0...")))
    (flush-output)
    (parameterize ([verbose-error-logging? #t])
      (for [(path paths-with-grader)]
        (cond [verbose?
               (display   (format " ~a: " path)) (flush-output)
               (displayln (format "~a" (grade-one path (open-output-nowhere "grade-some")))) (flush-output)]
              [else
               (let ([grade (grade-one path (open-output-nowhere "grade-some"))])
                 (unless (and (number? grade) (= grade 1.0))
                   (displayln (format " ~a: ~a" path grade))))])))))

#|
(define (run-tests test-dir report-dir)
  (call-with-output-file (build-path report-dir (string-append "log." (number->string (current-seconds))))
    (lambda (log-out)
      (parameterize [(current-output-port log-out)
                     (current-error-port log-out)]
        (let ([all-tests   (filter (curry regexp-match? #rx".*\\.rkt$")             (directory-list test-dir))]     ;note #:build? arg defaults to #f
              [all-reports (filter (curry regexp-match? #rx".*\\.report\\.[0-9]*$") (directory-list report-dir))])
          (for ([test all-tests])
            (let* ([reports (reports-for test all-reports)]
                   [last    (last-report test reports)]
                   [next    (next-report test reports)])
              (display (format "~%~a to ...report.~a    " test (path-version next)) log-out)
              (flush-output log-out)
              (call-with-output-file (build-path report-dir next)
                (lambda (report-out)                  
                  (autograde-file (build-path test-dir test) #t #f report-out (lambda (x) (display x report-out)))))
              (when (file-exists? (build-path report-dir next))
                (diff-reports (build-path report-dir last)
                              (build-path report-dir next)))))
          
          #t)))))

(define (diff-reports last next)
  (unless (equal? (path->string last) (path->string next))
    (display (format "diff ~a vs ~a   " (path-version last) (path-version next)))
    (flush-output)
    (cond [(zero? (system/exit-code (format "diff ~a ~a" last next)))
           (display "no differences between reports.")]
          [(equal? (read-internal-grading-data last)
                   (read-internal-grading-data next))
           (displayln "... but same internal grading data.")]
          [else
           (display "... and different internal grading data.~%")])
    (flush-output)))
                

(define (read-internal-grading-data file)
  (call-with-input-file* file
    (lambda (p)
      (let loop ([line (read-line p)])
        (cond [(eof-object? line) (error (format "Could not find internal grading data in ~a." file))]
              [(string=? line INTERNAL-DATA-LINE) (read p)]
              [else
               (loop (read-line p))])))))
                 


(define (path-fn1       p) (car   (string-split (path->string (file-name-from-path p)) ".")))
(define (path-extension p) (cadr  (string-split (path->string (file-name-from-path p)) ".")))
(define (path-version   p) (caddr (string-split (path->string (file-name-from-path p)) ".")))

(define (report-name-base tst-path)
  (string-append (path-fn1 tst-path) ".report"))


(define (reports-for tst-path all-reports)
  (let ([rgx1 (regexp (report-name-base tst-path))]
        [rgx2 (regexp (string-append (report-name-base tst-path) "*~"))])
    (filter (lambda (f)
              (and (regexp-match rgx1 f)
                   (not (regexp-match rgx2 f))))
            all-reports)))

(define (last-report tst-path reports)
  (if (null? reports)
      (build-path (string-append (report-name-base tst-path) ".1"))
      (let* ([nums (map (lambda (fn) (string->number (path-version fn)))
                        reports)]
             [lst (foldr max 0 nums)])
        (path-replace-extension (car reports)
                                (string-append "."
                                               (number->string lst))))))

(define (next-report tst-path reports)
  (if (null? reports)
      (build-path (string-append (report-name-base tst-path) ".1"))
      (let* ([nums (map (lambda (fn) (string->number (path-version fn)))
                        reports)]
             [nxt (add1 (foldr max 0 nums))])
        (path-replace-extension (car reports)
                                (string-append "."
                                               (number->string nxt))))))

|#
        
    
                                      
      
