#lang racket/base

(provide big-bang
         key=?
         mouse=?
         key-event?
         mouse-event?)

(define-syntax-rule (big-bang e ...)
  (void))

(define key=? equal?)
(define mouse=? equal?)

;; TODO: Could cause bugs, but is annoying to write out in full
(define (key-event? s)
  (string? s))

(define (mouse-event? s)
  (case s
    [("button-down" "button-up" "drag" "move" "enter" "leave")
     #t]
    [else #f]))
