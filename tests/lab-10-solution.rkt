;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-10-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Accumulators Lab

(@assignment labs/lab-10)


(@problem 1)

;; On-ground students: Do the following as your pre-lab
;; Online students: Do the following as Task 1

;; Using the data definitions below, design a function called can-encode? that
;; takes a letter (NOT an entire word) and a Huffman code tree and produces true 
;; if that letter is reachable in the given tree, false otherwise. You should
;; not use any accumulators. As a reminder, we have asked you to blend the
;; backtracking into your design using try-catch.

;; We have provided the signature, purpose, stub, and some tests.

(@htdd HTree)
(define-struct htree (zero one))
;; HTree is one of
;; - String
;; - (make-htree HTree HTree)
;; interp. A Huffman code tree where
;; a string s is an encoded value
;; (make-htree z o) is a tree with zero-branch z and one-branch o
(define HT0 "A")

(define HTMAGIC
  (make-htree "A"
              (make-htree 
               (make-htree "B"
                           (make-htree "C" "D"))
               "R")))

#;
(define (fn-for-htree ht)
  (cond [(string? ht) (... ht)]
        [else
         (... (fn-for-htree (htree-zero ht))
              (fn-for-htree (htree-one ht)))]))



(@htdf can-encode?)
(@signature HTree String -> Boolean)
;; produce if the given string is reachable in the given tree, false otherwise
(check-expect (can-encode? "A" "A") true)
(check-expect (can-encode? "A" "B") false) 
(check-expect (can-encode? HTMAGIC "D") true)
(check-expect (can-encode? HTMAGIC "E") false)

;(define (can-encode? ht s) false)


(@template HTree try-catch)
(define (can-encode? ht s)
  (cond [(string? ht) (string=? ht s)]
        [else
         (local [(define try (can-encode? (htree-zero ht) s))]
           (if (not (false? try))
               try
               (can-encode? (htree-one ht) s)))]))

;; Problem 2:

;; When spelling correction first appeared in typewriters, it was very important
;; to store the information about legal words as compactly as possible. One
;; technique for doing so was to use prefix trees, in which all words that start
;; with the same prefix share the same data representation for that common
;; prefix. They only have different data for their different endings. For
;; example, part of the prefix tree starting at t would look like this: 

#;
(bitmap/url
 "https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/lab-10-prefix-tree.png")


;; Of course, the complete tree starting at t is much bigger.

;; Each node in the prefix tree has its letter and a flag indicating whether the
;; path from the root of the tree to that node is a legal node. In the picture,
;; the * on some nodes indicate that the tree up to that point is a legal word.
;; For example, the sequence of letters t-o-o is a legal word because when we
;; start from t, then to o, then to the second o, we are a node with *. But the
;; sequence t-o-u is not legal because going from t, then o, then u lands at a
;; node without *. Every full path is a legal word because it would waste space
;; to store bad words that you don't need.


;; Design a function called count-odds that takes a prefix spelling tree
;; and determines how many odd-length words are contained in it. 

;; We have provided the data definition for prefix trees below. 

(@htdd PrefixTree)
(define-struct pt (l f? subs))
;; PrefixTree is (make-pt String Boolean (listof PrefixTree))
;; interp. a prefix spelling tree.  The sequence of letters up to and including
;;         a given node are a legal word if f? is true, not legal otherwise.

(define PT0 (make-pt "K" true empty))
(define PTT 
  (make-pt "t" false
           (list 
            (make-pt "o" true
                     (list 
                      (make-pt "o" true (list (make-pt "k" true empty)))
                      (make-pt "t" true empty)
                      (make-pt "l" false (list (make-pt "l" true empty)))
                      (make-pt "u" false
                               (list 
                                (make-pt "c" false
                                         (list (make-pt "h" true empty)))))))
            (make-pt "i" false
                     (list (make-pt "n" true empty))))))


(define LOPT0 (list (make-pt "o" true empty)
                    (make-pt "t" true empty)))


#;
(define (fn-for-pt pt)
  (local [(define (fn-for-pt pt)
            (... (pt-l pt)
                 (pt-f? pt)
                 (fn-for-lopt (pt-subs pt))))
          (define (fn-for-lopt lopt)
            (cond [(empty? lopt) (...)]
                  [else
                   (... (fn-for-pt (first lopt))
                        (fn-for-lopt (rest lopt)))]))]
    (fn-for-pt pt)))

(@problem 2)
(@htdf count-odds)
(@signature PrefixTree -> Natural)
;; Produce the number of odd lengthed words in tree
(check-expect (count-odds PT0) 1)                    ;1 letter long legal word
(check-expect (count-odds (make-pt "l" false empty)) ;no legal words
              0) 
(check-expect (count-odds PTT) 4)                    ;larger tree

;(define (count-odds pt) 0)

(@template PrefixTree (listof PrefixTree) accumulator)

;; Non-tail recursive w/o compound worklist
(define (count-odds pt)
  ;; rsf is Natural; the result so far
  ;; cl is the current length
  (local [(define (fn-for-pt pt cl)
            (if (and (pt-f? pt) (odd? cl))
                (add1 (fn-for-lopt (pt-subs pt) (add1 cl)))
                (fn-for-lopt (pt-subs pt)(add1 cl))))
          (define (fn-for-lopt lopt cl)
            (cond [(empty? lopt) 0]
                  [else
                   (+ (fn-for-pt (first lopt) cl)
                      (fn-for-lopt (rest lopt) cl))]))]
    (fn-for-pt pt 1)))

#;
;; Tail-recursive solution with compound worklist
(define (count-odds pt)
  ;; rsf is Natural; the result so far
  ;; wle is (make-wle Natural PrefixTree); a worklist entry
  ;;        where cl is current length and pt is the tree
  (local [(define-struct wle (cl pt))
          (define (fn-for-pt pt cl rsf todo)
            (if (and (pt-f? pt) (odd? cl))
                (fn-for-lopt (add1 rsf)
                             (append (map (lambda (x) (make-wle (add1 cl) x))
                                          (pt-subs pt))
                                     todo))
                (fn-for-lopt rsf
                             (append (map (lambda (x) (make-wle (add1 cl) x))
                                          (pt-subs pt))
                                     todo))))
          (define (fn-for-lopt rsf todo)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-pt (wle-pt (first todo))
                              (wle-cl (first todo))
                              rsf
                              (rest todo))]))]
    (fn-for-pt pt 1 0 empty)))



;; Problem 3:

;; Copy your function design from Task 1 and change it to produce a list of 
;; the binary digits on the path to a given string in the tree, or false if  
;; that string cannot be reached in the given tree. Use a result-so-far
;; accumulator. Call this function encode.
(@problem 3)
(@htdf encode)
(@signature HTree String -> (listof Natural) or false)
;; produce the encoding of the given string using the htree, or false if can't.
(check-expect (encode "A" "A") (list)) ; need no info according to this tree
(check-expect (encode "A" "B") false)  ; can't do it
(check-expect (encode HTMAGIC "A") (list 0))
(check-expect (encode HTMAGIC "D") (list 1 0 1 1))
(check-expect (encode HTMAGIC "Z") false)

;(define (encode ht s) false)

(@template HTree accumulator
	   try-catch) ;twice

(define (encode ht0 s)
  ;; rsf is (listof Natural); the result so far
  (local [(define (encode ht rsf)
            (cond [(string? ht) (if (string=? ht s)
                                    (reverse rsf)
                                    false)]
                  [else
                   (local [(define try (encode (htree-zero ht) rsf))]
                     (if (not (false? try))
                         (cons 0 try)
                         (local [(define try2 (encode (htree-one ht) rsf))]
                           (if (not (false? try2))
                               (cons 1 try2)
                               false))))]))]
    (encode ht0 empty)))



;; EXTRA PRACTICE:

;; Refactor your design of encode from Problem 3 so it is also tail recursive.
;; Note that you will want to keep a copy of your solution from Problem 3 to
;; hand in.
(@htdf encode-tail)
(@signature HTree String -> (listof Natural) or false)
;; produce the encoding of the given string using the htree, or false if can't.
(check-expect (encode-tail "A" "A") (list))
(check-expect (encode-tail "A" "B") false)
(check-expect (encode-tail HTMAGIC "A") (list 0))
(check-expect (encode-tail HTMAGIC "D") (list 1 0 1 1))


(@template HTree accumulator)
(define (encode-tail ht0 s0)
  ;; rsf : (listof Natural) ; bit path from ht0 to ht
  ;; todo : (listof WLE) ; Work left to do
  ;; WLE (worklist entry) is (make-wle HTree (listof Natural[0,1]))
  ;; interp. a worklist entry with current node and encoding up to that point
  (local [(define-struct wle (ht rsf))
          (define (encode-acc ht rsf todo)
            (cond [(string? ht)
                   (if (string=? ht s0)
                       rsf
                       (encode-todo todo))]
                  [else
                   (encode-todo (cons (make-wle (htree-zero ht)
                                                (append rsf (list 0)))
                                      (cons (make-wle (htree-one ht)
                                                      (append rsf (list 1)))
                                            todo)))]))

          (define (encode-todo todo)
            (cond [(empty? todo) false]
                  [else
                   (encode-acc (wle-ht (first todo))
                               (wle-rsf (first todo))
                               (rest todo))]))]
    (encode-acc ht0 (list) (list))))
