;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-06-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-06)

(@problem 1)
;; DATA DEFINITIONS ===============
;; Data definition for sentence tree

(@htdd SentenceTree ListOfSentenceTree)
(define-struct stree (str subs))
;; SentenceTree is (make-stree String ListOfSentenceTree)
;; interp: a sentence tree where
;;          str  is the prefix for all sentences in the subtrees
;;          subs is the subtrees

;; ListOfSentenceTree is one of:
;;  - empty
;;  - (cons SentenceTree ListOfSentenceTree)
;; interp. a list of sentence trees

(define ST8 (make-stree "MY FAVOURITE SONG ON REPEAT" empty))
(define ST7 (make-stree "FREEZE TIME" empty))
(define ST6
  (make-stree "WE ARE"
              (list (make-stree "IN A BACK TO SCHOOL SPECIAL ABOUT MONO"
                                empty)
                    (make-stree "PERCHED ON THE TIP OF A SINKING SHIP"
                                empty))))
(define ST4 (make-stree "TO"
                        (list ST7 ST8)))
(define ST3 (make-stree "LIKE"
                        (list (make-stree "YOU REALLY MEAN IT" empty)
                              ST6)))
(define ST1 (make-stree "KISS ME"
                        (list (make-stree "JOKING ABOUT JEALOUSY" empty)
                              ST3
                              ST4))) 

(define LOST1 (list ST8 ST7 ST6))

                   
(define (fn-for-stree sn)
  (... (stree-str sn)                  
       (fn-for-losn (stree-subs sn))))

(define (fn-for-losn losn)
  (cond [(empty? losn) (...)]
        [else
         (... (fn-for-stree (first losn))
              (fn-for-losn (rest losn)))]))


;; FUNCTIONS ======================

(@problem 2)
(@htdf num-sentences--sn num-sentences--losn)
(@signature SentenceTree -> Natural)
(@signature ListOfSentenceTree -> Natural)
;; Produce the number of sentences in the sentence tree
(check-expect (num-sentences--losn empty) 0)
(check-expect (num-sentences--sn ST8) 1)
(check-expect (num-sentences--sn ST6) 2)
(check-expect (num-sentences--sn
               (make-stree "a"
                           (list
                            (make-stree "b"
                                        (list (make-stree "c" empty))))))
              1)
(check-expect (num-sentences--sn ST1) 6)
(check-expect (num-sentences--losn LOST1) 4)

(@template SentenceTree)
(define (num-sentences--sn sn)
  (if (empty? (stree-subs sn))
      1
      (num-sentences--losn (stree-subs sn)))) 

(@template ListOfSentenceTree)
(define (num-sentences--losn losn)
  (cond [(empty? losn) 0]
        [else
         (+ (num-sentences--sn (first losn))  
            (num-sentences--losn (rest losn)))])) 



(@problem 3)
;; Constants:
(define TEXT-SIZE 20)
(define TEXT-COLOUR "black")

(@htdf render--sn render--losn)
(@signature SentenceTree -> Image)
(@signature ListOfSentenceTree -> Image)
;; render an image of the sentence tree
(check-expect (render--losn empty) empty-image)
(check-expect (render--sn ST8)
              (beside (text (stree-str ST8) TEXT-SIZE TEXT-COLOUR)
                      (rectangle 10 1 "solid" "white")))
(check-expect
 (render--sn ST6)
 (beside
  (text "we are" TEXT-SIZE TEXT-COLOUR)
  (rectangle 10 1 "solid" "white")
  (above/align
   "left"
   (beside (text "in a back to school special about mono" TEXT-SIZE TEXT-COLOUR)
           (rectangle 10 1 "solid" "white"))
   (rectangle 1 10 "solid" "white")
   (beside (text "perched on the tip of a sinking ship" TEXT-SIZE TEXT-COLOUR)
           (rectangle 10 1 "solid" "white"))
   (rectangle 1 10 "solid" "white"))))


;(define (render--sn sn) empty-image)
;(define (render--losn losn) empty-image)

(@template SentenceTree)
(define (render--sn sn)
  (beside (text (stree-str sn) TEXT-SIZE TEXT-COLOUR)
          (rectangle 10 1 "solid" "white")
          (render--losn (stree-subs sn))))

(@template ListOfSentenceTree)
(define (render--losn losn)
  (cond [(empty? losn) empty-image]
        [else
         (above/align "left"
                      (render--sn (first losn))
                      (rectangle 1 10 "solid" "white")
                      (render--losn (rest losn)))]))
