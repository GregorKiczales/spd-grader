;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-07-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; CPSC 110 - 2 One Of + Local Lab

(require spd/tags)

(@assignment labs/lab-07)


(@problem 1)

;; Problem 1
;;
;; In the game 20 questions, there are two players.  The first player
;; (the "answerer") thinks of an object.  Then the second player
;; (the "questioner") asks a series of yes-no questions, and the answerer
;; answers each of them.  After the questioner thinks that they have
;; asked enough questions, they guess the identity of the object
;; that the answerer had in mind.
;;
;; We can model this scenario using a binary tree.
;; Here is such a tree from a second-year computer science class

;(bitmap/url
; "https://edx-course-spdx-kiczales.s3.amazonaws.com/HTC/lab/lab-07-yes-no-tree.png")

; http://www.cs.duke.edu/courses/cps100/fall12/assignments/20q/

;; The nodes at the bottom of the tree (the "leaves") represent guesses
;; to the identity of the object.  The other nodes correspond to 
;; questions.  The left child of a question node describes what the
;; questioner will do if the answerer responds "yes" to the question:  
;; the right child describes the response to a "no" answer.  In general,
;; a tree of questions-and-answers is called a decision tree.
;;
;; Design a function that takes a decision tree and a natural number
;; and determines whether there is a path in the tree that arrives
;; at an answer after exactly n questions. Please design this as a
;; 2-one-of problem.
;;
;; NOTE: your function must take two arguments: a YesNoTree and a Natural, IN
;; THAT ORDER.

(@htdd YesNoTree)
(define-struct yntree (s y n))
;; YesNoTree is one of:
;; - String
;; - (make-yntree String YesNoTree YesNoTree)
;; interp. a yes-no question tree where:
;; + a string s is an answer
;; + (make-yntree s y n) represents a question s, with yes-decision y
;;    and no decision n
(define YNT0 "Raven")
(define YNT1 (make-yntree "Does it gobble?"
                          "Turkey"
                          (make-yntree "Does it say 'Nevermore'?"
                                       "Raven"
                                       "Eagle")))
(define YNT2 (make-yntree "Does it hop?"
                          "Kangaroo"
                          "Elephant"))
(define YNT3 (make-yntree "Is it wise?"
                          "Owl"
                          YNT1))

(@dd-template-rules one-of atomic-non-distinct compound self-ref self-ref)
(define (fn-for-ynt ynt)
  (cond
    [(string? ynt) (... ynt)]
    [else 
     (... (yntree-s ynt)
          (fn-for-ynt (yntree-y ynt))
          (fn-for-ynt (yntree-n ynt)))]))


;; Complete Problem 1 below:
(@htdf contains-n-path?) 
(@signature YesNoTree Natural -> Boolean)
;; Produce true if there is an answer after n questions in tree
(check-expect (contains-n-path? "Bat" 0) true)
(check-expect (contains-n-path? "Bat" 1) false)
(check-expect (contains-n-path? YNT2 0) false)
(check-expect (contains-n-path? YNT3 3) true)
(check-expect (contains-n-path? (make-yntree "Check both sides?"
                                             (make-yntree "maybe"
                                                          "only"
                                                          "if")
                                             "this works") 2) true)
(check-expect (contains-n-path? YNT3 4) false)

;(define (contains-n-path? ynt n) false)


;                            Natural
;YNT                    0             add1 Natural
;---------------------------------------------------
;String                 (zero? n)     (zero? n)
;                       true [1]      false [1]
;---------------------------------------------------
;(make-yntree String    false [2]     (or (rec-call (left side) (sub1 n))
;             YesNoTree                   (rec-call (right side) (sub1 n)))
;             YesNoTree)                       [3]

(@template 2-one-of)

(define (contains-n-path? ynt n)
  (cond [(string? ynt) (zero? n)]    ;[1]
        [(zero? n) false]            ;[2]
        [else                        ;[3]
         (or (contains-n-path? (yntree-y ynt) (sub1 n))
             (contains-n-path? (yntree-n ynt) (sub1 n)))]))



;; Problem 2
;;
;; Suppose you have rosters for players on two opposing tennis teams, and each
;; roster is ordered by team rank, with the best player listed first. When both
;; teams play, the best players of each team play one another,
;; and the second-best players play one another, and so on down the line. When
;; one team has more players than the other, the lowest ranking players on
;; the larger team do not play.
;;
;; Design a function that takes two lists of players and produces a list of
;; matches, according to the approach described above. 

(@htdd Player)
;; Player is String
;; interp.  the name of a tennis player.
(define PL0 "Maria")
(define PL2 "Serena")

(@dd-template-rules atomic-non-distinct)
(define (fn-for-player pl)
  (... pl))


(@htdd ListOfPlayer)
;; ListOfPlayer is one of:
;; - empty
;; - (cons Player ListOfPlayer)
;; interp.  a team roster, ordered from best player to worst.
(define LOPL0 empty)
(define LOPL1 (list "Eugenie" "Gabriela" "Sharon" "Aleksandra"))
(define LOPL2 (list "Maria" "Nadia" "Elena" "Anastasia" "Svetlana"))

(@dd-template-rules one-of atomic-distinct compound ref self-ref)
(define (fn-for-lopl lopl)
  (cond [(empty? lopl) (...)]
        [else 
         (... (fn-for-player (first lopl))
              (fn-for-lopl (rest lopl)))]))


(@htdd Match)
(define-struct match (p1 p2))
;; Match is (make-match Player Player)
;; interp.  a match between player p1 and player p2, with same team rank
(define M0 (make-match "Eugenie" "Maria"))
(define M1 (make-match "Gabriela" "Nadia"))

(@dd-template-rules compound)
(define (fn-for-match m)
  (... (match-p1 m) (match-p2 m)))


(@htdd ListOfMatch)
;; ListOfMatch is one of:
;; - empty
;; - (cons Match ListOfMatch)
;; interp. a list of matches between one team and another.
(define LOM0 empty)
(define LOM1 (list (make-match "Eugenie" "Maria")
                   (make-match "Gabriela" "Nadia")))

(@dd-template-rules one-of atomic-distinct compound ref self-ref)
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-match (first lom))
              (fn-for-lom (rest lom)))]))



;; Complete Problem 2 below:
(@problem 2)
(@htdf set-matches)
(@signature ListOfPlayer ListOfPlayer -> ListOfMatch)
;; create a list of matches where each player plays the corresponding player
(check-expect (set-matches empty empty) empty)
(check-expect (set-matches LOPL1 empty) empty)
(check-expect (set-matches empty LOPL2) empty)
(check-expect (set-matches LOPL1 LOPL2)
              (list (make-match "Eugenie" "Maria")
                    (make-match "Gabriela" "Nadia")
                    (make-match "Sharon" "Elena")
                    (make-match "Aleksandra" "Anastasia")))

;(define (set-matches lop1 lop2) empty)

;                           Roster 1
;Roster 2               empty       (cons Player Roster)
;------------------------------------------------------------------
;empty                  empty [1]    empty [1]
;------------------------------------------------------------------
;(cons Player Roster)   empty [1]   (cons (make-match (first lop1) (first lop2))
;                                         (set-matches (rest lop1) (rest lop2)))
;                                         [2]

(@template 2-one-of)

(define (set-matches lop1 lop2)
  (cond [(or (empty? lop1) (empty? lop2)) empty]
        [else
         (cons (make-match (first lop1) (first lop2))
               (set-matches (rest lop1) (rest lop2)))]))



;; Problem 3
;;
;; The starter contains the data definitions that represent a
;; family tree, as well as a completed design of a function
;; that tries to find the age of a given name in the family
;; tree. Use local expressions to improve the function. Be
;; prepared to tell the TA which of three uses of local to
;; improve a function you are using.

(@htdd Person)
(define-struct person (name age kids))
;; Person is (make-person String Natural ListOfPerson)
;; interp. A person, with first name, age and their children
(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P3 P2)))

(@dd-template-rules compound ref)
(define (fn-for-person p)
  (... (person-name p)			;String
       (person-age p)			;Natural  
       (fn-for-lop (person-kids p))))   

(@htdd ListOfPerson)
;; ListOfPerson is one of:
;;  - empty
;;  - (cons Person ListOfPerson)
;; interp. a list of persons

(@dd-template-rules one-of atomic-distinct compound ref self-ref)
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-person (first lop))   
              (fn-for-lop (rest lop)))]))


(@problem 3)
(@htdf find)
(@signature String Person -> Natural or false) 
;; search p for a person with the name n0, produce age if found; false otherwise
(check-expect (find "N2" P1) false)
(check-expect (find "N1" P1) 5)
(check-expect (find "N1" P2) 5)
(check-expect (find "N3" P2) false)
(check-expect (find "N2" P4) 25)
(check-expect (find "N1" P4) 5)
(check-expect (find "N5" P4) false) 

(@template Person ListOfPerson encapsulated try-catch)
(define (find n0 p0)
  (local [(define (find--person n p)
            (if (string=? (person-name p) n)
                (person-age p) 
                (find--lop n (person-kids p))))

          (define (find--lop n lop)
            (cond [(empty? lop) false]
                  [else
                   (if (not (false? (find--person n (first lop)))) 
                       (find--person n (first lop))
                       (find--lop n (rest lop)))]))]
    (find--person n0 p0)))
          
