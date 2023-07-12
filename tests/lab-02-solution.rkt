;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-02)

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)
(@htdf square?) 

(@signature Image -> Boolean)
;; determine if the image is square
(check-expect (square? (circle 80 "solid" "blue")) true)
(check-expect (square? (rectangle 70 50 "solid" "blue")) false)

;(define (square? i) false)

(@template Image)
(define (square? i)
  (= (image-width i) (image-height i)))



;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
;; 
;; 
;; For example, 
;; 
;;   (underline (circle 20 "solid" "green"))
;; 
;; should produce
;;
;;   (above (circle 20 "solid" "green")
;;          (rectangle 40 2 "solid" "black"))
(@problem 2)
(@htdf underline)

(@signature Image -> Image)
;; add an underline to the given image
(check-expect (underline (circle 20 "solid" "green"))
              (above (circle 20 "solid" "green")
                     (rectangle 40 2 "solid" "black")))
(check-expect (underline (rectangle 70 45 "outline" "blue"))
              (above (rectangle 70 45 "outline" "blue")
                     (rectangle 70 2 "solid" "black")))
                       
;(define (underline i) i)

(@template Image)
(define (underline i)
  (above i
         (rectangle (image-width i) 2 "solid" "black")))


;; HtDF Lab, Problem 3

;; PROBLEM:
;; 
;; A (much too) simple scheme for pluralizing words in English is to add an 
;; s at the end unless the word already ends in s.
;; 
;; Design a function that consumes a string, and adds s to the end unless 
;; the string already ends in s.
(@problem 3)
(@htdf pluralize) 
(@signature String -> String)
;; pluralize the given word
(check-expect (pluralize "cat") "cats")
(check-expect (pluralize "apple") "apples")
(check-expect (pluralize "dogs") "dogs")
(check-expect (pluralize "oranges") "oranges")

;(define (pluralize s) s)

(@template String)
(define (pluralize s)
  (cond [(string=? s "") ""]
        [(string=? (substring s (sub1 (string-length s))) "s") s]
        [else
         (string-append s "s")]))



;; HtDF Lab, Problem 4

;; PROBLEM:
;; 
;; Design a function called nth-char-equal? that consumes two strings 
;; and a natural and produces true if the strings both have length greater 
;; than n and have the same character at position n.
;; 
;; 
;; Note, the signature for such a function is:
;; 
;; (@signature String String Natural -> Boolean)
;; 
;; 
;; The tag and template for such a function are:
;; 
;; (@template String)
;; 
;; (define (nth-char-equal? s1 s2 n)
;;   (... s1 s2 n))
(@problem 4)
(@htdf nth-char-equal?) 
(@signature String String Natural -> Boolean)
;; determine if s1 and s2 are longer than n and have the same nth character
(check-expect (nth-char-equal? "cat" "cats" 3) false)
(check-expect (nth-char-equal? "me" "met" 3) false)
(check-expect (nth-char-equal? "mat" "met" 1) false)
(check-expect (nth-char-equal? "banana" "canada" 2) true)

;(define (nth-char-equal? s1 s2 n) false)

(@template String)
(define (nth-char-equal? s1 s2 n)
  (and (> (string-length s1) n)
       (> (string-length s2) n)
       (string=? (string-ith s1 n) (string-ith s2 n))))      





