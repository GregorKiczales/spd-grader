;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-01-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Intro Lab
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-01)


(@problem 1)
;; Complete Problem 1 below using the following constants

(define PREFIX "hello")
(define SUFFIX "world")

(string-append PREFIX "_" SUFFIX)



(@problem 2)
;; Complete Problem 2 below using the following constants


(define STR "helloworld")
(define i 5)


(string-append (substring STR 0 i) "_" (substring STR i))




(@problem 3)
;; Complete Problem 3 below using the following constant

(define CAT
  (bitmap/url
   "https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/cat.png"))

(* (image-width CAT) (image-height CAT))







(@problem 4)
;; Complete Problem 4 below using CAT as defined above

;; Note: make sure it checks for all three branches
(if (= (image-width CAT) (image-height CAT))
    "square"
    (if (> (image-width CAT) (image-height CAT))
        "wide"
        "tall"))


(@problem 5)
;; Complete Problem 5 below using STR as defined above


(string=? "h" (substring STR 0 1))

