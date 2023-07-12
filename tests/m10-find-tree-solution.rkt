;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname m10-find-tree-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(@assignment lectures/m10-find-tree)
(@problem 1)

(@htdd Tree) 

(define-struct node (name subs))
;; Tree is (make-node String (listof Tree))
;; interp. a bare bones arbitrary arity tree, each node has a name and subs
#;
(define (fn-for-tree t)  
  (local [(define (fn-for-t t)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              
              (... name (fn-for-lot subs))))
          
          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-t (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-t t)))

(define L1 (make-node "L1" empty))
(define L2 (make-node "L2" empty))
(define L3 (make-node "L3" empty))

(define M1 (make-node "M1" (list L1)))
(define M2 (make-node "M2" (list L2 L3)))

(define TOP (make-node "TOP" (list M1 M2)))


;;
;; PROBLEM
;;
;; Design a function that given a tree and a name tries to find a tree
;; with that name.
;;


(@htdf find-tree)

(@signature Tree String -> Tree or false)
;; produce path to node w/ given name (or fail)
(check-expect (find-tree L1 "L1") L1)
(check-expect (find-tree L1 "L2") false)
(check-expect (find-tree L2 "L2") L2)
(check-expect (find-tree M1 "L1") L1)
(check-expect (find-tree TOP "L3") L3)

#;#;
(@template encapsulated try-catch Tree (listof Tree))

;; Structural recursion
(define (find-tree t to)
  (local [(define (fn-for-t t)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              (if (string=? name to)
                  t
                  (fn-for-lot subs))))
          
          (define (fn-for-lot lot)
            (cond [(empty? lot) false]
                  [else
                   (local [(define try (fn-for-t (first lot)))]
                     (if (not (false? try))
                         try
                         (fn-for-lot (rest lot))))]))]
    
    (fn-for-t t)))

;;
;; PROBLEM
;;
;; Define a new version of the function which is tail recursive.  You only
;; need a new function definition. Proceed by
;;   - copying your solution to the problem above
;;   - then convert the code to be tail recursive
;;


(@template Tree (listof Tree) accumulator)

;; Tail recursion
(define (find-tree t to)
  ;; t-wl is (listof Tree); worklist of pending trees to visit
  (local [(define (fn-for-t t t-wl)
            (local [(define name (node-name t))  ;unpack the fields
                    (define subs (node-subs t))] ;for convenience
              (if (string=? name to)
                  t
                  (fn-for-lot (append subs t-wl)))))
          
          (define (fn-for-lot t-wl)
            (cond [(empty? t-wl) false]
                  [else
                   (fn-for-t (first t-wl)
                             (rest t-wl))]))]
    
    (fn-for-t t empty)))
