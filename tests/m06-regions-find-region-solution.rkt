;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname m06-regions-find-region-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)
(@assignment lectures/m06-regions-find-region)
;;
;; Region and ListOfRegion data definitions provided.  
;;
(@problem 1)
(@htdd Region ListOfRegion)
(define-struct single (label weight color))
(define-struct group (color subs))
;; Region is one of:
;;  - (make-single String Natural Color)
;;  - (make-group Color ListOfRegion)
;; interp.
;;  an arbitrary-arity tree of regions
;;  single regions have label, weight and color
;;  groups have a color and a list of sub-regions
;;
;;  weight is a unitless number indicating how much weight
;;  the given single region contributes to whole tree

;; ListOfRegion is one of:
;;  - empty
;;  - (cons Region ListOfRegion)
;; interp. a list of regions

;; All the Ss and Gs are Regions
(define S1 (make-single "one" 20 "red"))
(define S2 (make-single "two" 40 "blue"))
(define S3 (make-single "three" 60 "orange"))
(define S4 (make-single "four" 30 "black"))
(define S5 (make-single "five" 50 "purple"))
(define S6 (make-single "six" 80 "yellow"))

(define G1 (make-group "red"  (list S1 S2 S3)))
(define G2 (make-group "blue" (list G1 S4)))
(define G3 (make-group "orange" (list S5 S6)))
(define G4 (make-group "black" (list G2 G3)))

(define LORE empty)
(define LOR123 (list S1 S2 S3))

(define (fn-for-region r)
  (cond [(single? r)
         (... (single-label r)
              (single-weight r)
              (single-color r))]
        [(group? r)
         (... (group-color r)
              (fn-for-lor (group-subs r)))]))

(define (fn-for-lor lor)
  (cond [(empty? lor) (...)]
        [else
         (... (fn-for-region (first lor))
              (fn-for-lor (rest lor)))]))



(@htdd ListOfString)


;; Design a function that consumes a string and a region
;; and looks for a region with the given label.  If there is one
;; the function should produce the first one it finds.  If there is
;; not one it should signal failure by producing false.
;; The signature for the function is given below.   Be sure to
;; follow the structure above, where the @htdf tag names both
;; functions, the signatures are grouped, single purpose, tests
;; are grouped, and each function has its own template tag.

(@htdf find-region--region find-region--lor)
(@signature String Region -> Region or false)
(@signature String ListOfRegion  -> Region or false)
;; find region w/ given label
(check-expect (find-region--lor "one" empty) false)
(check-expect (find-region--region "one" S1) S1)
(check-expect (find-region--region "one" S2) false)
(check-expect (find-region--lor "two" LOR123) S2)
(check-expect (find-region--region "three" G4) S3)

(@template Region)

(define (find-region--region l r)
  (cond [(single? r) (if (string=? (single-label r) l) r false)]
        [(group?  r) (find-region--lor l (group-subs r))]))

(@template ListOfRegion try-catch)

(define (find-region--lor l lor)
  (cond [(empty? lor) false]
        [else
         (if (not (false? (find-region--region l (first lor))))
             (find-region--region l (first lor))
             (find-region--lor l (rest lor)))]))
