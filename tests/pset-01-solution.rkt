;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset-01-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER
;;
(require 2htdp/image)
(require spd/tags)

(@assignment psets/pset-01);Do not edit or remove this tag


(@problem 1)
;;
;; Write the BSL expression that most clearly represents the following
;; arithmetic expression:
;;
;;     3^2 + 6 * (6 - 10) / 3
;;
;; which equals 1
;;
;; There is a single BSL expression that both produces the correct result, 
;; and most closely matches the structure of the equation given above.
;; Although 1 is an expression that evaluates to 1, that is not the
;; correct answer, because it doesn't show the structure of the expression
;; above.
;;

(+ (sqr 3) (/ (* 6 (- 6 10)) 3))




(@problem 2)
;;
;; Given the definitions below (LOWER, UPPER and foo),
;; write the step-by-step evaluation of the following expression:
;;
;;     (foo (* 3 4) (+ 0 2))
;;
;; Be sure to show every intermediate evaluation step, including the original
;; expression and the final result.  The steps should NOT be in comments.
;;
;; Stepping questions like this one have only one correct answer.  The goal
;; of this problem is to assess whether you have learned the exact BSL step
;; by step evaluation rules; not whether you can figure out the final
;; result of an expression.
;;

(define LOWER 10)
(define UPPER 20)

(define (foo x y)
  (if (and (> x LOWER) (< x UPPER))
      (- x (* y 2))
      (+ x (* y 3))))

(foo (* 3 4) (+ 0 2))

(foo 12 (+ 0 2))

(foo 12 2)

(if (and (> 12 LOWER) (< 12 UPPER))
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(if (and (> 12 10) (< 12 UPPER))
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(if (and true (< 12 UPPER))
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(if (and true (< 12 20))
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(if (and true true)
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(if true
    (- 12 (* 2 2))
    (+ 12 (* 2 3)))

(- 12 (* 2 2))

(- 12 4)

8



(@problem 3)
;;
;; Skye has written the following expression, and expects it to produce
;; "hahaha". Instead, there is an error.  Please correct the expression below
;; so that it produces "hahaha".
;;
;; First uncomment the expression, then fix it.
;;
;; When you are done, do NOT add the semi-colon back to the beginning
;; of the line, UNLESS your expression still produces an error. 
;;
;; Remember the Help Desk can be used to access documentation on
;; BSL primitive functions.
;;

(replicate 3 "ha")



(@problem 4)
;;
;; Skye has now written the following expression, and expects it to produce
;; "so". Instead, "o" is produced. Please uncomment and then correct the
;; expression below so that it produces "so".
;;

(substring "Wow so funny" 4 6)



(@problem 5)
;;
;; Skye has now written a function that is supposed to produce the given image
;; with a caption underneath it. Please correct the expression below so that
;; it correctly produces a blue circle with the text "Blue Circle" under it.
;;
;; First uncomment the expression, then fix it.
;;
;; Do NOT change the function definition, just fix the expression that is
;; commented.  When you are done, do NOT add the semi-colon back to the
;; beginning of the line, UNLESS your expression still produces an error.
;;


(define (give-caption img cap)
  (above img
         (text cap 15 "dark gray")))

(give-caption (circle 40 "solid" "blue") "Blue Circle")




(@problem 6)
;;
;; NOTE: Do not attempt to solve this problem until you have covered
;; the How to Design Functions (HtDF) design recipe in lecture.
;;
;; Design a function called taller-image that consumes two images and produces
;; the image that has the greater height.
;;
;; Note that the template tag for a function that consumes two images is:
;;
;; (@template Image)
;;
;; and the template itself is:
;;
;; (define (taller-image i1 i2)
;;   (... i1 i2))
;;

(@htdf taller-image)
(@signature Image Image -> Image)
;; Produce image with greater height; or first image if same height
(check-expect (taller-image (rectangle 10 50 "solid" "blue")
			    (rectangle 30 40 "solid" "red"))
	      (rectangle 10 50 "solid" "blue"))
(check-expect (taller-image (rectangle 10 50 "solid" "blue")
			    (rectangle 30 50 "solid" "red"))
	      (rectangle 10 50 "solid" "blue"))
(check-expect (taller-image (rectangle 10 50 "solid" "blue")
			    (rectangle 30 60 "solid" "red"))
	      (rectangle 30 60 "solid" "red"))
(check-expect (taller-image (rectangle 10 50 "solid" "blue")
			    (rectangle 10 80 "solid" "red"))
	      (rectangle 10 80 "solid" "red"))
(check-expect (taller-image (square 50 "solid" "yellow")
			    (circle 90 "outline" "red"))
	      (circle 90 "outline" "red"))

;(define (taller-image i1 i2) empty-image)  ;stub

(@template Image)

(define (taller-image i1 i2)
  (if (>= (image-height i1) (image-height i2))
      i1
      i2))

;; Alternative Solution:

;(@htdf taller-image)
;(@signature Image Image -> Image)
;;; Produce image with greater height; or second image if same height
;(check-expect (taller-image (rectangle 10 50 "solid" "blue")
;			    (rectangle 30 40 "solid" "red"))
;	      (rectangle 10 50 "solid" "blue"))
;(check-expect (taller-image (rectangle 10 50 "solid" "blue")
;			    (rectangle 30 50 "solid" "red"))
;	      (rectangle 30 50 "solid" "red"))
;(check-expect (taller-image (rectangle 10 50 "solid" "blue")
;			    (rectangle 30 60 "solid" "red"))
;	      (rectangle 30 60 "solid" "red"))
;(check-expect (taller-image (rectangle 10 80 "solid" "blue")
;			    (rectangle 10 50 "solid" "red"))
;	      (rectangle 10 80 "solid" "blue"))
;(check-expect (taller-image (square 50 "solid" "yellow")
;			    (circle 90 "outline" "red"))
;	      (circle 90 "outline" "red"))
;
;;(define (taller-image i1 i2) empty-image)  ;stub
;
;(@template Image)
;
;(define (taller-image i1 i2)
;  (if (> (image-height i1) (image-height i2))
;      i1
;      i2))



