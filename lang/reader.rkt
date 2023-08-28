#lang racket

(require syntax/readerr
         syntax/strip-context)
 
(provide (rename-out [grader-read read]
                     [grader-read-syntax read-syntax]))

(define grader-readtable (make-readtable (current-readtable)
                                         #\{
                                         'terminating-macro
                                         (lambda args (apply read-curly args))))


(define (grader-read in)
  (syntax->datum (grader-read-syntax #f in)))
 
(define (grader-read-syntax src in)
  (parameterize ([current-readtable grader-readtable])
    (strip-context
     #`(module ignored racket
         #,@(read-syntaxes src in)))))

(define (read-syntaxes src in)
  (if (eof-object? (peek-char in))
      '()
      (let ([stx (read-syntax src in)])
        (if (eof-object? stx)
            '()
            (cons stx
                  (read-syntaxes src in))))))
  
(define read-curly
  (case-lambda
    [(ch in)
    `(WEIGHT ,(read in)
             ,(begin (eat-curly-after in (object-name in))
                     (read in)))]
    [(ch in src line col pos)
     #`(WEIGHT #,(read-syntax src in)
               #,(begin (eat-curly-after in src)
                        (read-syntax src in)))]))

(define (eat-curly-after in src)
  (regexp-match #px"^\\s*" in) ; skip whitespace
  (let ([ch (peek-char in)])
    (unless (equal? ch #\}) (bad-ending ch src in))
    (read-char in)))
 
(define (bad-ending ch src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)
         raise-read-error
         raise-read-eof-error)
     "expected a closing `}'"
     src line col pos
     (if (eof-object? ch) 0 1))))
