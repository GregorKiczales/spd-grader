;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-balloon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-03)

; Balloon popping

(@htdw Balloon)
;; CONSTANTS ==========================

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define BALLOON-COLOR "red")
(define POP-IMAGE
  (overlay (text "POP!" 80 "black")
           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2) "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 2)

(define MAX-SIZE (/ WIDTH 2))





;; DATA DEFINITIONS ============================
 
(@problem 1)

(@htdd Balloon)
;; Balloon is one of:
;; - Natural
;; - false
;; interp. Balloon is either a radius from 0 to MAX-SIZE
;;        or it is popped (false)

(define B1 0)
(define B2 23)
(define B3 MAX-SIZE)
(define B4 false)

(@dd-template-rules one-of
                    atomic-non-distinct
                    atomic-distinct)

(define (fn-for-balloon b)
  (cond [(number? b) (... b)]
        [else
         (...)]))


;; FUNCTIONS ====================================
(@problem 2)
(@htdf main)
(@signature Balloon -> Balloon)  
;; starts the world program with 0
; no examples for main function

(@template htdw-main)
(define (main b)
  (big-bang b               ; Balloon
    (on-tick tick)   ; Balloon -> Balloon
    (to-draw render) ; Balloon -> Image
    ;           (stop-when ...)  ; Balloon -> Boolean
    ;           (on-mouse ...)   ; Balloon Integer Integer MouseEvent -> Balloon
    ;           (on-key ...)     ; Balloon KeyEvent -> Balloon
    ))

(@problem 3)
(@htdf tick)
(@signature Balloon -> Balloon) 
;; produce the next balloon
(check-expect (tick 0) (+ 0 SPEED))
(check-expect (tick 4) (+ 4 SPEED))
(check-expect (tick MAX-SIZE) false)
(check-expect (tick (- MAX-SIZE SPEED)) MAX-SIZE)
(check-expect (tick (add1 (- MAX-SIZE SPEED))) false)
(check-expect (tick false) false)

;(define (tick b) b)
(@template Balloon)
(define (tick b)
  (cond [(number? b) (if (< MAX-SIZE (+ b SPEED))
                         false
                         (+ b SPEED))]
        [else
         false]))


(@problem 4)
(@htdf render)
(@signature Balloon -> Image) 
;; render the ballon
(check-expect (render 0) MTS)
(check-expect (render 4) (place-image (circle 4 "solid" BALLOON-COLOR)
                                      CTR-X
                                      CTR-Y
                                      MTS))
(check-expect (render false) (place-image POP-IMAGE
                                          CTR-X
                                          CTR-Y
                                          MTS))

;(define (render b) MTS)
(@template Balloon)
(define (render b)
  (cond [(number? b) (place-image (circle b "solid" BALLOON-COLOR)
                                  CTR-X
                                  CTR-Y
                                  MTS)]
        [else
         (place-image POP-IMAGE
                      CTR-X
                      CTR-Y
                      MTS)]))
