;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab-10-accumulators) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
;; CPSC 110 - Graphs Lab

(@assignment labs/lab-11)


(@problem 1)

(@htdd Venue)
(define-struct vnu (nm los))
;; Venue is (make-vnu String (listof Streetway))
;; interp. a venue name and streetways leading from that venue

(@htdd Streetway)
(define-struct sw (nm dst))
;; Streetway is (make-sw String String)
;; interp. (make-sw s v) is a streetway s that leads to the destination venue v.

(define VENUES
  (list (make-vnu "West Coast Express"
                  (list (make-sw "Cordova Eastbound"
                                 "Shine Night Club")))
        (make-vnu "Shine Night Club"
                  (list (make-sw "Cordova Westbound"
                                 "West Coast Express")
                        (make-sw "Richards Southbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Cordova Eastbound"
                                 "Rocket Reprographics")))
        (make-vnu "Rocket Reprographics"
                  (list (make-sw "Cordova Eastbound"
                                 "Anti-Hero 13 Boutique")))
        (make-vnu "Anti-Hero 13 Boutique" (list))
        (make-vnu "Vancouver Lookout"
                  (list (make-sw "Seymour Northbound"
                                 "West Coast Express")
                        (make-sw "Hastings Eastbound"
                                 "WOW Tasty Food Delivery")))
        (make-vnu "WOW Tasty Food Delivery"
                  (list (make-sw "Hastings Westbound"
                                 "Vancouver Lookout")
                        (make-sw "Richards Southbound"
                                 "MacLeod's Books")
                        (make-sw "Hastings Eastbound"
                                 "Vancouver Film School")))
        (make-vnu "Vancouver Film School"
                  (list (make-sw "Hastings Westbound"
                                 "WOW Tasty Food Delivery")
                        (make-sw "Homer Northbound"
                                 "Rocket Reprographics")
                        (make-sw "Hastings Eastbound"
                                 "BC Marijuana Party")))
        (make-vnu "BC Marijuana Party"
                  (list (make-sw "Hastings Westbound"
                                 "Vancouver Film School")
                        (make-sw "Hamilton Southbound"
                                 (string-append "London School of Hairdressing "
                                                "and Aesthetics"))))
        (make-vnu "F.M. Classic Pizza"
                  (list (make-sw "Seymour Northbound"
                                 "Vancouver Lookout")
                        (make-sw "Pender Eastbound"
                                 "MacLeod's Books")))
        (make-vnu "MacLeod's Books"
                  (list (make-sw "Pender Westbound"
                                 "F.M. Classic Pizza")
                        (make-sw "Richards Southbound"
                                 "D&S Bubble Tea")
                        (make-sw "Pender Eastbound"
                                 "Capital Tax and Accounting")))
        (make-vnu "Capital Tax and Accounting"
                  (list (make-sw "Pender Westbound"
                                 "MacLeod's Books")
                        (make-sw "Homer Northbound"
                                 "Vancouver Film School")
                        (make-sw "Pender Eastbound"
                                 (string-append "London School of Hairdressing "
                                                "and Aesthetics"))))
        (make-vnu "London School of Hairdressing and Aesthetics"
                  (list (make-sw "Pender Westbound"
                                 "Capital Tax and Accounting")
                        (make-sw "Hamilton Southbound"
                                 "Candy Meister")))
        (make-vnu "7-11"
                  (list (make-sw "Seymour Northbound"
                                 "F.M. Classic Pizza")))
        (make-vnu "D&S Bubble Tea"
                  (list (make-sw "Dunsmuir Westbound"
                                 "7-11")))
        (make-vnu "BC Hydro"
                  (list (make-sw "Homer Northbound"
                                 "Capital Tax and Accounting")
                        (make-sw "Dunsmuir Westbound"
                                 "D&S Bubble Tea")))
        (make-vnu "Candy Meister"
                  (list (make-sw "Hamilton Northbound"
                                 "London School of Hairdressing and Aesthetics")
                        (make-sw "Dunsmuir Westbound"
                                 "BC Hydro")))))



;; Consider this to be a primitive function that comes with the data definitions
;; and that given a venue name it produces the corresponding venue.  Because
;; this consumes a string and generates a venue calling it will amount to a
;; generative step in a recursion through a graph of venues and streetways.
;; You must not edit this function, but you can experiment with it to see how
;; it works.

;;(@htdf lookup-venue)
;;(@signature String -> Venue)
(define (lookup-venue name)
  (local [(define (scan lst)
            (cond [(empty? lst) (error "No venue named " name)]
                  [else
                   (if (string=? (vnu-nm (first lst)) name)
                       (first lst)
                       (scan (rest lst)))]))]
    (scan VENUES)))



;; Complete the data definition by finishing the template for fn-for-venue
;; and fn-for-los in the encapsulated fn-for-map. DO NOT ADD ACCUMULATORS,
;; simply produce the template that corresponds to the type comments.

(@template encapsulated genrec Venue (listof Streetway) Streetway)
(define (fn-for-map v)
  (local [(define (fn-for-venue v)
            (... (vnu-nm v)
                 (fn-for-los (vnu-los v))))
          
          (define (fn-for-los los)
            (cond [(empty? los) (...)]
                  [else
                   (... (fn-for-sw (first los))
                        (fn-for-los (rest los)))]))
          
          (define (fn-for-sw sw)
            (... (sw-nm sw)
                 ;; lookup-venue is the generative step that makes the whole MR
                 ;; generative
                 (fn-for-venue (lookup-venue (sw-dst sw)))))]
    (fn-for-venue v)))



;; Problem 1:

;; Design a function that, given a venue and the name of a venue IN THAT ORDER,
;; produces true if the named venue can be reached from the given venue.
;; Call it can-get-to?.

(@htdf can-get-to?)
(@signature Venue String -> Boolean)
;; produce true if venue with name n is reachable from v
(check-expect (can-get-to? (lookup-venue "West Coast Express")
                           "West Coast Express")
              true)
(check-expect (can-get-to? (lookup-venue "Anti-Hero 13 Boutique")
                           "West Coast Express")
              false)
(check-expect (can-get-to? (lookup-venue "West Coast Express")
                           "Does Not Exist")
              false)
(check-expect (can-get-to? (lookup-venue "West Coast Express")
                           "Candy Meister")
              true)

(@template encapsulated genrec Venue (listof Streetway) Streetway accumulator)
(define (can-get-to? v0 n)
  ;; visited is (listof String); list of names of visited venues
  (local [(define (fn-for-venue v visited)
            (cond [(string=? (vnu-nm v) n) true]
                  [(member? (vnu-nm v) visited) false]
                  [else
                   (fn-for-los (vnu-los v) (cons (vnu-nm v) visited))]))

          (define (fn-for-los los visited)
            (cond [(empty? los) false]
                  [else
                   (or (fn-for-sw (first los) visited)
                       (fn-for-los (rest los) visited))]))

          (define (fn-for-sw sw visited)
            (fn-for-venue (lookup-venue (sw-dst sw)) visited))]
    (fn-for-venue v0 empty)))



;; Problem 2:

;; Design a function, called find-route, that given a venue and the name of
;; some other venue IN THAT ORDER, produces a list of names of streetways that
;; can get you to the named venue (i.e a list of driving directions), or false
;; if the named venue cannot be reached from the given venue.

;; Your find-route design does not need to be tail-recursive. But if you finish
;; early, try refactoring your solution so it is! You may want to keep a copy
;; of your non-tail-recursive solution to hand in.

(@problem 2)

(@htdf find-route)
(@signature Venue String -> (listof String) or false)
;; produce list of streetnames on path if venue with name n is reachable from v
(check-expect (find-route (lookup-venue "West Coast Express")
                          "West Coast Express")
              empty)
(check-expect (find-route (lookup-venue "Anti-Hero 13 Boutique")
                          "West Coast Express")
              false)
(check-expect (find-route (lookup-venue "West Coast Express")
                          "Does Not Exist")
              false)
(check-expect (find-route (lookup-venue "West Coast Express")
                          "Candy Meister")
              (list "Cordova Eastbound" "Richards Southbound"
                    "Richards Southbound" "Pender Eastbound"
                    "Homer Northbound" "Hastings Eastbound"
                    "Hamilton Southbound" "Hamilton Southbound"))

(@template genrec encapsulated Venue (listof Streetway) Streetway try-catch accumulator)

(define (find-route v0 n)
  ;; visited is (listof String); list of names of visited venues
  ;; path is (listof String); list of names of streets along path so far
  (local [(define (fn-for-venue v visited path)
            (cond [(string=? (vnu-nm v) n) path]
                  [(member? (vnu-nm v) visited) false]
                  [else
                   (fn-for-los (vnu-los v) (cons (vnu-nm v) visited) path)]))

          (define (fn-for-los los visited path)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (fn-for-sw (first los) visited path))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) visited path)))]))

          (define (fn-for-sw sw visited path)
            (fn-for-venue (lookup-venue (sw-dst sw))
                          visited
                          (append path (list (sw-nm sw)))))]
    (fn-for-venue v0 empty empty)))

;; ALTERNATE SOLUTION WITHOUT PATH ACCUMULATOR
#;
(define (find-route v0 n)
  ;; visited is (listof String); list of names of visited venues
  ;; path is (listof String); list of names of streets along path so far
  (local [(define (fn-for-venue v visited)
            (cond [(string=? (vnu-nm v) n) empty]
                  [(member? (vnu-nm v) visited) false]
                  [else
                   (fn-for-los (vnu-los v) (cons (vnu-nm v) visited))]))

          (define (fn-for-los los visited)
            (cond [(empty? los) false] 
                  [else
                   (local [(define try (fn-for-sw (first los) visited))]
                     (if (not (false? try))
                         try
                         (fn-for-los (rest los) visited)))]))

          (define (fn-for-sw sw visited)
            (local [(define try (fn-for-venue (lookup-venue (sw-dst sw))
                                              visited))]
              (if (not (false? try))
                  (cons (sw-nm sw) try)
                  false)))]
    (fn-for-venue v0 empty)))
