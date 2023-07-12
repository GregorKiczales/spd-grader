;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-08-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; CPSC 110 - Abstraction Lab

(require spd/tags)
(require racket/file)

(@assignment labs/lab-08)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl ??? ???)

(@problem 1)

;; The first four problems use the following data definition,
;; which represents a path through a binary search tree.

(@htdd Path)
;; Path is one of:
;; - empty
;; - (cons "L" Path)
;; - (cons "R" Path)
;; interp. 
;;  A sequence of left and right 'turns' down through a binary tree
;;  (list "L" "R" "R") means take the left child of the root, then
;;  the right child of that node, and the right child again.
;;  empty means you have arrived at the destination.
(define P1 empty)
(define P2 (list "L" "R"))
(define P3 empty)
(define P4 (cons "L" (cons "R" empty)))
(define P5 (cons "L" (cons "R" (cons "R" empty))))

(@dd-template-rules one-of atomic-distinct self-ref self-ref)
(define (fn-for-path p)
  (cond [(empty? p) (...)]
        [(string=? (first p) "L") (... (fn-for-path (rest p)))]
        [(string=? (first p) "R") (... (fn-for-path (rest p)))]))



;; Design an abstract function (including signature, purpose, and tests)
;; called num-lr to simplify the lefts-minus-rights and rights-minus-lefts
;; functions defined below.
;;
;; Then re-define the original lefts-minus-rights and rights-minus-lefts
;; functions to use your abstract function. Remember, the signature and tests
;; should not change from the original functions. For simplicity, assume 
;; that all numbers throughout this problem have type Integer.


(@htdf lefts-minus-rights)
(@signature Path -> Integer)
;; produce the difference between left turns and right turns

(check-expect (lefts-minus-rights empty) 0)
(check-expect (lefts-minus-rights (list "R" "L" "R")) -1)
(check-expect (lefts-minus-rights (list "L" "R" "L")) 1)

(@template use-abstract-fn)
(define (lefts-minus-rights p) (num-lr add1 sub1 p))


(@htdf rights-minus-lefts)
(@signature Path -> Integer)
;; produce the difference between right turns and left turns

(check-expect (rights-minus-lefts empty) 0)
(check-expect (rights-minus-lefts (list "R" "L" "R")) 1)
(check-expect (rights-minus-lefts (list "L" "R" "L")) -1)

(@template use-abstract-fn)
(define (rights-minus-lefts p) (num-lr sub1 add1 p))




(@htdf num-lr)
;; (num-lr l r (list "L" "R" ... "R")) = (l (r ... (r 0)))
(@signature (Integer -> Integer) (Integer -> Integer) Path -> Integer)

(check-expect (num-lr add1 sub1 empty) 0)
(check-expect (num-lr add1 sub1 (list "R" "L" "R")) -1)
(check-expect (num-lr sub1 add1 (list "R" "L" "R")) 1)
(check-expect (num-lr sub1 add1 (list "L" "R" "L")) -1)


(@template Path)
(define (num-lr lc rc p)
  (cond [(empty? p) 0]
        [(string=? (first p) "L") (lc (num-lr lc rc (rest p)))]
        [(string=? (first p) "R") (rc (num-lr lc rc (rest p)))]))


(@problem 2)
;;
;; Use your abstract function from the previous problem to design a function
;; called path-length that determines the length of a given path.

(@htdf path-length)

(@signature Path -> Natural)
;; produce length of path (number of "L" plus "R")
(@signature Path -> Natural)

(check-expect (path-length empty) 0)
(check-expect (path-length (list "L" "L" "R" "L" "R")) 5)

(@template use-abstract-fn)

(define (path-length p) (num-lr add1 add1 p))


(@problem 3)
;;
;; Design an abstract fold function for Path called fold-path

(@htdf fold-path)

;; fold function for path
(@signature (X -> X) (X -> X) X -> X)

(check-expect (fold-path add1 sub1 0 empty) 0)
(check-expect (fold-path add1 sub1 0 (list "R" "L" "R")) -1)
(check-expect (local [(define (lc x) (string-append "L" x))
                      (define (rc x) (string-append "R" x))]
                (fold-path lc rc "" (list "L" "R" "R" "L")))
              "LRRL")


(@template Path)

(define (fold-path lc rc x p)
  (cond [(empty? p) x]
        [(string=? (first p) "L") (lc (fold-path lc rc x (rest p)))]
        [(string=? (first p) "R") (rc (fold-path lc rc x (rest p)))]))


(@problem 4)
;;
;; Use your fold-path function called path-string to design a function called
;; path-string that produces a single string that concatenates all of the turns
;; in a path.

(@htdf path-string)
(@signature Path -> String)
;; for (list "L" "R" "R" "L") produce "LRRL"
(check-expect (path-string (list "L" "R" "R" "L")) "LRRL")
(check-expect (path-string (list "R" "R" "L")) "RRL")

(@template use-abstract-fn)

(define (path-string p)
  (local [(define (lc x) (string-append "L" x))
          (define (rc x) (string-append "R" x))]
    (fold-path lc rc "" p)))


(@problem 5)
;;
;; Design a function called popular-spring-class-count that takes a list of
;; class data and produces the number of classes from Term 2 where enrollment
;; exceeded 70% capacity (that is, enrollment / capacity > 0.7).
;;
;; The function that you design must make at least one call to 
;; built-in abstract functions.

(@htdd Class)
(define-struct class (id sec term credits enrolled capacity title))
;; Class is (make-class String String Natural
;;             Natural Natural Natural String)
;; interp. (make-class id sec term credits enrolled capacity title) is
;; data about a UBC CS class where:
;; - id is the class identifier
;; - sec is the class section
;; - term is the term during which the class is held, restricted to [1,2]
;; - credits is the number of credits the course is worth
;; - enrolled is the number of students enrolled
;; - capacity is the number of students that could be enrolled
;; - title is an abbreviated title for the course
;; CONSTRAINT: a class's capacity is always >= 1

(define C0 (make-class "CPSC229" "202" 2 4 40 84 "CMPTNL BSKT WVNG"))
(define C1 (make-class "CPSC259" "201" 2 4 190 188 "DTA&ALG ELEC ENG"))
(define C2 (make-class "CPSC400" "123" 1 0 190 188 "SNRITIS FOR MJRS"))

(@dd-template-rules compound)
(define (fn-for-class c)
  (... (class-id c)
       (class-sec c)
       (class-term c)
       (class-credits c)
       (class-enrolled c)
       (class-capacity c)
       (class-title c)))


;; Use the space below to design the function for Problem 5:

(@htdf popular-spring-class-count)
(@signature (listof Class) -> Natural)

(check-expect (popular-spring-class-count empty) 0)
(check-expect (popular-spring-class-count
               (list (make-class "CPSC121" "202" 2 4 149 170 "MODEL COMPTU")
                     (make-class "CPSC320" "102" 2 3 34 49 "INTR ALG DSGN&AN")
                     (make-class "CPSC406" "201" 2 3 70 100 "ALGORITHMS OPTZN")
                     (make-class "CPSC317" "101" 1 3 77 80 "INTERNET COMP")
                     (make-class "CPSC317" "201" 2 3 102 125 "INTERNET COMP")
                     (make-class "CPSC403" "101" 1 4 3 30 "COOL CPSC CLASS")
                     (make-class "CPSC404" "102" 1 3 70 100 "COOL CLASS 2"))) 
              2)

(@template fn-composition use-abstract-fn)

(define (popular-spring-class-count loc)
  (foldr (lambda (c y) (add1 y))
         0
         (filter (lambda (c)
                   (= (class-term c) 2))
                 (filter (lambda (c)
                           (> (/ (class-enrolled c) (class-capacity c)) 0.7))
                         loc))))
















