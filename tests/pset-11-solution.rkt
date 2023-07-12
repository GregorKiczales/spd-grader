;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-11-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-11); Do not edit or remove this tag

(@problem 1)

;;
;; Please read through the MODIFIED data definition introduced in Problem Set 6
;; for Treasure that can be found in a Scavenger Hunt. It has been modified
;; to add route durations to travel between treasure boxes.
;;

(@htdd Status)
;; Status is one of:
;; - "buried"
;; - "sunken"
;; - "locked"
;; interp. the status of an unopened treasure box
;;<examples are redundant for enumeration>

(@htdd Treasure)
(define-struct treasure (label amount difficulty status routes))
;; Treasure is (make-treasure String Natural Natural Status (listof Route))
;; interp. a treasure box with a label name,
;;         the number of gold coins contained in the treasure box,
;;         a rating of difficulty to find and open the treasure box between 1
;;         and 5, where 1 is very easy to find and open and 5 is very difficult,
;;         the status of the treasure box before it was opened,
;;         and a list of routes leading from this treasure box
;;         to other treasure boxes

(@htdd Route)
(define-struct route (duration destination))
;; Route is (make-route Natural String)
;; interp. a route leading from one treasure box to another       
;;         duration is the time in hours it will take to travel to it and
;;         destination is the name of the treasure box the route leads to

(define TREASURE-MAP
  (list (make-treasure "E" 32 3 "buried"  (list (make-route 3 "A")))
        (make-treasure "F" 10 2 "locked"  (list (make-route 7 "C")))
        (make-treasure "B" 6 5 "locked"   (list (make-route 9 "E")
                                                (make-route 15 "F")))
        (make-treasure "J" 1 1 "sunken"   (list (make-route 6 "I")))
        (make-treasure "H" 17 2 "sunken"  (list (make-route 15 "J")
                                                (make-route 4 "I")))
        (make-treasure "G" 52 3 "buried"  (list (make-route 2 "D")))
        (make-treasure "I" 100 5 "locked" empty)
        (make-treasure "D" 21 1 "sunken"  (list (make-route 8 "G")
                                                (make-route 13 "H")
                                                (make-route 9 "I")
                                                (make-route 11 "A")))
        (make-treasure "C" 41 4 "buried"  (list (make-route 6 "G")))
        (make-treasure "A" 7 1 "locked"   (list (make-route 12 "B")
                                                (make-route 7 "C")
                                                (make-route 27 "D")))))

;; Consider this to be a primitive function that comes with the data definitions
;; and that given a treasure name it produces the corresponding treasure.
;; Because this consumes a string and generates a treasure calling it will
;; amount to a generative step in a recursion through a graph of treasures and
;; routes. You must not edit this function, but you can experiment with it to
;; see how it works.

;;(@htdf lookup-treasure)
;;(@signature String -> Treasure)
(define (lookup-treasure name)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No treasure named " name)]
                  [else
                   (if (string=? (treasure-label (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan TREASURE-MAP)))

(define TE (lookup-treasure "E"))
(define TF (lookup-treasure "F"))
(define TB (lookup-treasure "B"))
(define TJ (lookup-treasure "J"))
(define TH (lookup-treasure "H"))
(define TG (lookup-treasure "G"))
(define TI (lookup-treasure "I"))
(define TD (lookup-treasure "D"))
(define TC (lookup-treasure "C"))
(define TA (lookup-treasure "A"))




;;
;; These templates traverse a grap with no cycle detection mechanism, so as
;; they appear here they WILL NOT TERMINATE.  Any function that uses them
;; must add some appropriate termination guarantee.
;;
(define (fn-for-treasure t)
  (local [(define (fn-for-status s)
            (cond [(string=? s "buried") (...)]
                  [(string=? s "sunken") (...)]
                  [(string=? s "locked") (...)]))

          (define (fn-for-treasure t)
            (... (treasure-label t)
                 (treasure-amount t)
                 (treasure-difficulty t)
                 (fn-for-status (treasure-status t))
                 (fn-for-lor (treasure-routes t))))

          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else
                   (... (fn-for-route (first lor))
                        (fn-for-lor (rest lor)))]))

          (define (fn-for-route r)
            (... (route-duration r)
                 ;; lookup-treasure is the generative step that makes the whole 
                 ;; MR generative
                 (fn-for-treasure (lookup-treasure (route-destination r)))))]
    
    (fn-for-treasure t)))



;;
;; Design a function that consumes a treasure and produces the total amount
;; of gold that can be obtained by opening that treasure, and all treasures
;; reachable from that treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf reachable-gold)
(@signature Treasure -> Natural)
;; produce the total amount of reachable gold
(check-expect (reachable-gold TI) 100)
(check-expect (reachable-gold TH) (+ 17 1 100))
(check-expect (reachable-gold TA) (+ 7 6 32 10 41 52 21 17 1 100))

(@template genrec Treasure (listof Route) Route accumulator)

(define (reachable-gold t)
  ;; TERMINATION: never operates on the same treasure twice (visited)
  ;; r-wl is (listof Route); worklist accumulator of routes to visit
  ;; visited is (listof String); labels of treasures already visited
  ;; rsf is Natural; total gold found so far
  (local [(define (fn-for-treasure t r-wl visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor r-wl visited rsf)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) r-wl)
                               (cons (treasure-label t) visited)
                               (+ (treasure-amount t) rsf))]))

          (define (fn-for-lor r-wl visited rsf)
            (cond [(empty? r-wl) rsf]
                  [else
                   (fn-for-route (first r-wl) (rest r-wl) visited rsf)]))

          (define (fn-for-route r r-wl visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             r-wl
                             visited
                             rsf))]
    
    (fn-for-treasure t empty empty 0)))





(@problem 2)
;;
;; Complete the function that lists the label names of all reachable treasure.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf all-labels)
(@signature Treasure -> (listof String))
;; produce the label names of all reachable treasures
(check-expect (all-labels TI) (list "I"))
(check-expect (all-labels TH) (list "H" "J" "I"))
(check-expect (all-labels TA) (list "A" "B" "E" "F" "C"
                                     "G" "D" "H" "J" "I"))

(@template genrec Treasure (listof Route) Route accumulator)

(define (all-labels t)
  ;; TERMINATION: never operates on the same treasure twice (visited)
  ;; r-wl is (listof Route); worklist accumulator of routes to visit
  ;; visited is (listof String); labels of treasures already visited
  (local [(define (fn-for-treasure t r-wl visited)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor r-wl visited)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) r-wl)
                               (cons (treasure-label t) visited))]))

          (define (fn-for-lor r-wl visited)
            (cond [(empty? r-wl) (reverse visited)]
                  [else
                   (fn-for-route (first r-wl) (rest r-wl) visited)]))

          (define (fn-for-route r r-wl visited)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             r-wl
                             visited))]
    
    (fn-for-treasure t empty empty)))




(@problem 3)
;;
;; Complete the function that lists the labels names of all reachable treasures
;; when following only routes with a duration less than n hours long.
;;
;; Your solution MUST be tail recursive.
;;

(@htdf short-dur-reachable)
(@signature Treasure Number -> (listof String))
;; produce labels of all treasures reachable by following routes < n hr long
(check-expect (short-dur-reachable TI 12) (list "I"))
(check-expect (short-dur-reachable TH 4)  (list "H"))
(check-expect (short-dur-reachable TH 5)  (list "H" "I"))
(check-expect (short-dur-reachable TH 16) (list "H" "J" "I"))
(check-expect (short-dur-reachable TA 9) (list "A" "C" "G" "D"))

(@template genrec Treasure (listof Route) Route accumulator)

(define (short-dur-reachable t n)
  ;; TERMINATION: never operates on the same treasure twice (visited)
  ;; r-wl is (listof Route); worklist accumulator of routes to visit
  ;; visited is (listof String); labels of treasures already visited
  (local [(define (fn-for-treasure t r-wl visited)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor r-wl visited)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) r-wl)
                               (cons (treasure-label t) visited))]))

          (define (fn-for-lor r-wl visited)
            (cond [(empty? r-wl) (reverse visited)]
                  [else
                   (fn-for-route (first r-wl) (rest r-wl) visited)]))

          (define (fn-for-route r r-wl visited)
            (if (< (route-duration r) n)
                (fn-for-treasure (lookup-treasure (route-destination r))
                                 r-wl
                                 visited)
                (fn-for-lor r-wl visited)))]
    
    (fn-for-treasure t empty empty)))





(@problem 4)
;;
;; Complete the design of a function that consumes two treasures and counts
;; the number of routes reachable from te that lead to tf.
;;
;; Note: This is counting the number of routes found in treasure boxes where tf
;; is the destination, NOT the total number of paths between the two treasure
;; boxes. It is asking how many routes have tf as their destination (how many
;; arrows are pointing to tf).
;;
;; Examples:
;;
;; (num-lead-to TA TI) produces 3. This is because there are three routes that
;; are reachable from TA that lead to TI. These routes are the route leading
;; from TH to TI, the route leading from TJ to TI, and the route leading from
;; TD to TI.
;;
;; (num-lead-to TI TA) produces 0. Even though two routes lead to TA (the
;; route from TD to TA and the route from TE to TA), neither route can be
;; reached from TI so the function produces 0.
;;
;; Note that you can use the built-in function equal? to compare if two
;; treasures are equal. For example:
;;   - (equal? TE TF) produces false
;;   - (equal? TB TB) produces true
;;
;; Your solution MUST be tail recursive.
;;

(@htdf num-lead-to)
(@signature Treasure Treasure -> Natural)
;; count the number of reachable routes that lead to tf
(check-expect (num-lead-to TI TA) 0)
(check-expect (num-lead-to TJ TI) 1)
(check-expect (num-lead-to TH TI) 2)
(check-expect (num-lead-to TA TI) 3)
(check-expect (num-lead-to TF TA) 2)
(check-expect (num-lead-to TD TC) 2)

(@template genrec Treasure (listof Route) Route accumulator)

(define (num-lead-to te tf)
  ;; TERMINATION: never operates on the same treasure twice (visited)
  ;; r-wl is (listof Route); worklist accumulator of routes to visit
  ;; visited is (listof String); label names of treasures already visited
  ;; rsf is Natural; number of routes leading to tf found so far
  (local [(define (fn-for-treasure t r-wl visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor r-wl visited rsf)]
                  [else 
                   (fn-for-lor (append (treasure-routes t) r-wl)
                               (cons (treasure-label t) visited)
                               rsf)]))

          (define (fn-for-lor r-wl visited rsf)
            (cond [(empty? r-wl) rsf]
                  [else
                   (fn-for-route (first r-wl) (rest r-wl) visited rsf)]))

          (define (fn-for-route p r-wl visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination p))
                             r-wl
                             visited
                             (if (string=? (route-destination p)
                                           (treasure-label tf))
                                 (add1 rsf)
                                 rsf)))]
    
    (fn-for-treasure te empty empty 0)))





(@problem 5)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the time in hours of route durations
;; it would take to travel from t to the treasure labeled s. This function
;; produces the total duration of the routes followed as soon as it finds a
;; treasure labeled s. The function produces false if there is no way of
;; reaching a treasure with the given label.
;;

(@htdf route-to)
(@signature Treasure String -> Natural or false)
;; produce the total duration traveled on route to s from t, false if not found
(check-expect (route-to TE "X") false)
(check-expect (route-to TE "E") 0)
(check-expect (route-to TE "A") 3)
(check-expect (route-to TH "I") 21)
(check-expect (route-to TA "G") 40)
(check-expect (route-to TA "J") 70)

(@template genrec Treasure (listof Route) Route accumulator try-catch) 
#;
(define (route-to t s)
  ;; TERMINATION: never operates on the same treasure twice (path)
  ;; path is (listof String); labels of treasures visited so far
  ;; rsf is Natural; duration of travel on route so far
  (local [(define (fn-for-treasure t path rsf)
            (cond [(member? (treasure-label t) path) false]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (treasure-routes t)
                                    (cons (treasure-label t) path)
                                    rsf)]))

          (define (fn-for-lor lor path rsf)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (fn-for-route (first lor) path rsf))]
                     (if (not (false? try))
                         try
                         (fn-for-lor (rest lor) path rsf)))]))

          (define (fn-for-route r path rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             path
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty 0)))

(define (route-to t s)
  ;; TERMINATION: never operates on the same treasure twice (visited)
  ;; t-wl is (listof Route); worklist accumulator
  ;; d-wl is (listof Natural); tandem worklist accumulator for duration along
  ;;                           path, in correspondance with t-wl
  ;; visited is (listof String); labels of treasures visited so far
  ;; rsf is Natural; duration of travel on route so far
  (local [(define (fn-for-treasure t t-wl d-wl visited rsf)
            (cond [(member? (treasure-label t) visited)
                   (fn-for-lor t-wl d-wl visited)]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (append (treasure-routes t) t-wl)
                                    (append (map (λ (r) rsf)
                                                 (treasure-routes t))
                                            d-wl)
                                    (cons (treasure-label t) visited))]))

          (define (fn-for-lor t-wl d-wl visited)
            (cond [(empty? t-wl) false]
                  [else
                   (fn-for-route (first t-wl)
                                 (rest t-wl)
                                 (rest d-wl)
                                 visited
                                 (first d-wl))]))

          (define (fn-for-route r t-wl d-wl visited rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             t-wl
                             d-wl
                             visited
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty empty empty 0)))






(@problem 6)
;;
;; Complete the design of a function that consumes a treasure and the label
;; of another treasure, and produces the MINIMUM time in hours of all of the
;; possible routes that could be taken to travel from t to the treasure
;; labeled s. The function produces false if there is no way of reaching the
;; a treasure with the given label.
;;

(@htdf min-route-to)
(@signature Treasure String -> Natural or false)
;; produce the min duration traveled on route to s from t, false if not found
(check-expect (min-route-to TE "X") false)
(check-expect (min-route-to TE "E") 0)
(check-expect (min-route-to TE "A") 3)
(check-expect (min-route-to TH "I") 4)
(check-expect (min-route-to TA "G") 13)
(check-expect (min-route-to TA "J") 43)

(@template genrec Treasure (listof Route) Route accumulator)
#;
(define (min-route-to t s)
  ;; TERMINATION: never operates on the same treasure twice (path)
  ;; path is (listof String); labels of treasures visited so far
  ;; rsf is Natural; duration of travel on route so far
  (local [(define (fn-for-treasure t path rsf)
            (cond [(member? (treasure-label t) path) false]
                  [(string=? (treasure-label t) s) rsf]
                  [else (fn-for-lor (treasure-routes t)
                                    (cons (treasure-label t) path)
                                    rsf)]))

          (define (fn-for-lor lor path rsf)
            (cond [(empty? lor) false]
                  [else
                   (local [(define try (fn-for-route (first lor) path rsf))
                           (define try2 (fn-for-lor (rest lor) path rsf))]
                     (if (not (false? try))
                         (if (not (false? try2))
                             (min try try2)
                             try)
                         try2))]))

          (define (fn-for-route r path rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             path
                             (+ (route-duration r) rsf)))]
    
    (fn-for-treasure t empty 0)))

(define (min-route-to t s)
  ;; TERMINATION: never operates on the same treasure twice (path)
  ;; visited is (listof String); labels of treasures visited so far
  ;; rsf is Natural or false; duration of travel on route or false if no path
  ;;                          found so far 
  (local [(define (fn-for-treasure t path dur r-wl p-wl d-wl rsf)
            (cond [(member? (treasure-label t) path)
                   (fn-for-lor r-wl p-wl d-wl rsf)]
                  [(string=? (treasure-label t) s)
                   (if (false? rsf)
                       (fn-for-lor r-wl p-wl d-wl dur)
                       (fn-for-lor r-wl p-wl d-wl (min rsf dur)))]
                  [else (fn-for-lor (append (treasure-routes t) r-wl)
                                    (append (map (λ (r) (cons (treasure-label t)
                                                              path))
                                                 (treasure-routes t))
                                            p-wl)
                                    (append (map (λ (r) dur)
                                                 (treasure-routes t))
                                            d-wl)
                                    rsf)]))

          (define (fn-for-lor r-wl p-wl d-wl rsf)
            (cond [(empty? r-wl) rsf]
                  [else
                   (fn-for-route (first r-wl)
                                 (first p-wl)
                                 (first d-wl)
                                 (rest r-wl)
                                 (rest p-wl)
                                 (rest d-wl)
                                 rsf)]))

          (define (fn-for-route r path dur r-wl p-wl d-wl rsf)
            (fn-for-treasure (lookup-treasure (route-destination r))
                             path
                             (+ (route-duration r) dur)
                             r-wl
                             p-wl
                             d-wl
                             rsf))]
    
    (fn-for-treasure t empty 0 empty empty empty false)))
