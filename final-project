; PROBLEM 1:
; 
; Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
; whether or not they are a verified user, and follows some number of people. 
; 
; Design a data definition for Chirper, including a template that is tail recursive and avoids 
; cycles. 
; 
; Then design a function called most-followers which determines which user in a Chirper Network is 
; followed by the most people.


(define-struct user (name verified? users))
;; User is (make-user String Boolean (listof User))
;; interp. a user of social network Chirper with name, field verified?
;;         and list of users whom the user follows 

(define U1 (make-user "Jessica" false (list (make-user "Tim" true (list)))))
(define U2
  (shared ((-A- (make-user "Jessica" false (list -B-)))
           (-B- (make-user "Tim" true (list -A-))))
    -A-))
(define U3
  (shared ((-A- (make-user "Marge" false (list -B- -C-)))
           (-B- (make-user "Tim" true (list)))
           (-C- (make-user "Jessica" false (list -B-))))
    -A-))
(define U4
  (shared ((-A- (make-user "Steve" true (list -B- -C-)))
           (-B- (make-user "Julia" false (list -A- -C- -D-)))
           (-C- (make-user "Grace" true (list -D-)))
           (-D- (make-user "Bruce" true (list -C-))))
    -A-))

;; template: structural recursion, encapsulate w/local,
;;           added todo (worklist acc for tail recursion)
;;           added visited (context-preserving acc, users visited)

#;
(define (fn-for-user u0)
  ;; todo is (listof User); worklist accumulator for tail-recursive
  ;; visited is (listof String); context-preserving acc, rooms visited so far
  (local [(define (fn-for-user u todo visited)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited)
                (fn-for-lou (append (user-users u) todo)
                            (cons (user-name u) visited)))) ;(user-verified? u)
                 
          (define (fn-for-lou todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited)]))]
    (fn-for-user u0 empty empty)))


;; User -> User
;; given a user, produce a user that is most followed (if equal amount of , produce the first); ???
(check-expect (most-followers U1) (make-user "Tim" true (list)))
(check-expect (most-followers U2)
              (shared ((-A- (make-user "Jessica" false (list -B-)))
                       (-B- (make-user "Tim" true (list -A-))))
                -B-))
(check-expect (most-followers U3)
              (shared ((-A- (make-user "Marge" false (list -B- -C-)))
                       (-B- (make-user "Tim" true (list)))
                       (-C- (make-user "Jessica" true (list -B-))))
                -B-))
(check-expect (most-followers U4)
              (shared ((-A- (make-user "Steve" true (list -B- -C-)))
                       (-B- (make-user "Julia" false (list -A- -C- -D-)))
                       (-C- (make-user "Grace" true (list -D-)))
                       (-D- (make-user "Bruce" true (list -C-))))
                -C-))
              
;(define (most-followers u) (make-user "" false (list))) ;stub

;; template from User (graph)

(define (most-followers u0)
  ;; todo is (listof User); worklist accumulator for tail-recursive
  ;; visited is (listof String); context-preserving acc, rooms visited so far
  ;; louc is (listof UC); result-so far acc, stores info about how many times user is followed 
  (local [(define-struct uc (user count))
          ;; UC (UserCount) is (make-uc User Natural)
          ;; interp. (listof UC) entry with user number (times user met in user-users)
          (define (fn-for-user u todo visited louc)
            (if (member (user-name u) visited)
                (fn-for-lou todo visited louc)
                (fn-for-lou (append (user-users u) todo)
                            (cons (user-name u) visited)
                            (update louc (user-users u))))) ; -> (listof UC)
                 
          (define (fn-for-lou todo visited louc)
            (cond [(empty? todo) (get-most-followed louc)]
                  [else
                   (fn-for-user (first todo)
                                (rest todo)
                                visited
                                louc)]))

          (define (update louc lou)
            (foldr update-one louc lou))

          (define (update-one u louc)
            (cond [(empty? louc) (list (make-uc u 1))]
                  [else
                   (if (string=? (user-name (uc-user (first louc))) (user-name u))
                       (cons (make-uc u (add1 (uc-count (first louc))))
                             (rest louc))
                       (cons (first louc)
                             (update-one u (rest louc))))]))

          (define (get-most-followed louc) 
            (uc-user (foldr (lambda (uc2 uc1) (if (> (uc-count uc2) (uc-count uc1)) uc2 uc1))
                            (first louc)
                            (rest louc))))]
    (fn-for-user u0 empty empty empty)))



; PROBLEM 2:
; 
; In UBC's version of How to Code, there are often more than 800 students taking 
; the course in any given semester, meaning there are often over 40 Teaching Assistants. 
; 
; Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
; a program to do it for us! 
; 
; Below are some data definitions for a simplified version of a TA schedule. There are some 
; number of slots that must be filled, each represented by a natural number. Each TA is 
; available for some of these slots, and has a maximum number of shifts they can work. 
; 
; Design a search program that consumes a list of TAs and a list of Slots, and produces one
; valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
; maximum shifts. If no such schedules exist, produce false. 
;
; You should supplement the given check-expects and remember to follow the recipe!

;; ============================= DATA DEFINITIONS

;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define TA-F (make-ta "" 0 (list))) ; dummy TA

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))


(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)

(define-struct acc (slots schedule tas tasconst))
;; Accumulator is (make-acc ((listof Natural) Schedule (listof TA) (listof TA))
;; interp. A context-preserving acc with
;;         - slots - yet not assigned slots for each acc
;;         - schedule - (listof Assignment) current list of account, rsf
;;         - tas - changing (list of TA), updated for each new accumulator
;;         - tasconst - initial (list of TA), used to match tas to slots


;; ============================= FUNCTIONS

;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible
;; NOTE: schedule is valid when all the slots are assigned to TAs
;;       and no TA is working more than their maximum shifts

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)
;(define (schedule-tas tas slots) empty) ;stub

;; template as Arb-Arity Tree (Mutual Recursion), wrapped w/ local,
;;          added generative recursion, backtracking search,
;;          function composition at trampoline,
;;          acc (compound data, accumulator for each variant of schedule)

;; Cross-product of 2 one-of types
;;    next               empty                    (cons (first next) (rest next))
;; todo                | 
;;             empty   | false                   | (fill-one (first loa) 
;; ____________________|_________________________|           (append (rest loa)
;; (cons (first todo)  | (fill-one (first todo)  |                   (todo)))
;;       (rest todo))  |           (rest todo))  |

(define (schedule-tas tas0 slots0)
  ;; todo is (listof Accumulator), worklist accumulator for tail recursion
  ;; acc is Accumulator, Compound Data accumulator 
  (local [;; Return filled Schedule or pass new (listof Accumulator) to fill--list
          (define (fill--one acc todo)
            (if (empty? (acc-slots acc)) 
                (acc-schedule acc)
                (fill--list (next-loa acc) todo)))
 
          ;; Traverse loa, passing each acc to fill--one; produce valid filled Schedule or false
          (define (fill--list next todo)
            (cond [(and (empty? next) (empty? todo)) false]
                  [(empty? next) (fill--one (first todo) (rest todo))]
                  [else
                   (fill--one (first next) (append (rest next) todo))]))]
                         
    (fill--one (initialize-acc tas0 (reverse slots0)) empty)))


;; (listof TA) (listof Slot) -> Accumulator
;; produce initial Accumulator 
(check-expect (initialize-acc (list) (list)) (make-acc (list) (list) (list) (list)))
(check-expect (initialize-acc (list SOBA RAMEN) (list 1 3))
              (make-acc (list 1 3) (list) (list SOBA RAMEN) (list SOBA RAMEN)))
;(define (initialize-acc tas slots) (make-acc (list) (list))) ;stub

(define (initialize-acc tas slots)
  (make-acc slots empty tas tas))

;; Accumulator -> (listof Accumulator)
;; Produce next (listof Accumulator) to pass from fill-one to fill--list
(check-expect (next-loa (make-acc empty empty empty empty)) empty) 
; reached the end of list acc-slots
(check-expect (next-loa (make-acc empty empty (list SOBA) (list SOBA))) empty)
; no TA is available 
(check-expect (next-loa (make-acc (list 1) empty empty (list SOBA))) empty)
; TA is not available for the slot
(check-expect (next-loa (make-acc (list 1)
                                  (list)
                                  (list (make-ta "Ramen" 1 (list 2)))
                                  (list RAMEN)))
              empty)
; TA is available, ta-max = 1 -> rmv slot, rmv ta from acc-tas
(check-expect (next-loa (make-acc (list 2)
                                  (list)
                                  (list (make-ta "Ramen" 1 (list 2)))
                                  (list RAMEN)))
              (list (make-acc (list)
                              (list (make-assignment RAMEN 2))
                              (list)  
                              (list RAMEN))))
; TA is available, ta-max >=2, 2+ ta-avail -> rmv slot, sub1 ta-max
(check-expect (next-loa (make-acc (list 1)
                                  (list)       
                                  (list (make-ta "Soba" 2 (list 1 3)))
                                  (list SOBA)))
              (list (make-acc (list)
                              (list (make-assignment SOBA 1))
                              (list (make-ta "Soba" 1 (list 1 3)))
                              (list SOBA))))
; 2 TAs are available -> create new acc for each TA
(check-expect (next-loa (make-acc (list 3)
                                  (list)    
                                  (list (make-ta "Udon" 1 (list 3 4))
                                        (make-ta "Soba" 2 (list 1 3)))
                                  (list UDON SOBA)))
              (list (make-acc (list)
                              (list (make-assignment SOBA 3))
                              (list (make-ta "Udon" 1 (list 3 4))
                                    (make-ta "Soba" 1 (list 1 3)))
                              (list UDON SOBA))
                    (make-acc (list)
                              (list (make-assignment UDON 3))
                              (list (make-ta "Soba" 2 (list 1 3)))
                              (list UDON SOBA))))
;(define (next-loa acc) empty) ;stub

;; template as reference fn as trampoline,
;;          helper functions wrapped w/ local (for closures that use fileds of acc0)

(define (next-loa acc0)
  (local [;; Slots -> (listof Accumulator)
          ;; 1) get names of TA's available for the given slot, 2) produce new acc for each name
          (define (build-lo-acc slots)
            (if (not (empty? slots))
                (map produce-acc
                     (get-ta-names (first slots) (acc-tas acc0) empty))
                empty))
         
          ;; Slot (listof TA) (listof TA) -> (listof String)
          ;; produce all the names of TAs available for the given slot
          ;; ASSUME: all tas have ta-max > 1 and updated ta-avail
          ;; rsf is (listof TA); result-so-far acc for tail recursion
          (define (get-ta-names slot tas rsf)
            (cond [(empty? tas) rsf]
                  [else
                   (if (member slot (ta-avail (first tas)))
                       (get-ta-names slot (rest tas) (cons (ta-name (first tas)) rsf))
                       (get-ta-names slot (rest tas) rsf))]))

          ;; String -> Accumulator
          ;; Produce 1 Accumulator; update acc-slots, acc-schedule, acc-tas
          (define (produce-acc name)
            (make-acc (remove (first (acc-slots acc0)) (acc-slots acc0))
                      (new-schedule name)
                      (new-tas name (acc-tas acc0) empty)
                      (acc-tasconst acc0)))
          
          ;; String -> Schedule
          ;; Match TA to current slot, add new assignment to the end of existing acc-schedule
          (define (new-schedule name)
            (append (acc-schedule acc0)
                    (list (make-assignment (get-ta name (acc-tasconst acc0)) 
                                           (first (acc-slots acc0))))))

          ;; String (listof TA) -> TA
          ;; search for TA in acc-tasconst via name
          (define (get-ta name tasconst)
            (foldr (lambda (ta b) (if (string=? name (ta-name ta)) ta b))
                   TA-F
                   tasconst))
         
          ;; String (listof TA) (listof TA) -> (listof TA)
          ;; Produce updated tas for acc
          ;; rsf is (listof TA); result-so-far accumulator for tail recursion
          (define (new-tas name tas rsf)
            (cond [(empty? tas) rsf]
                  [else
                   (cond [(and (= (ta-max (first tas)) 1)
                               (string=? name (ta-name (first tas))))
                          (append rsf (rest tas))]                           ;remove ta
                         [(string=? name (ta-name (first tas)))  
                          (append rsf
                                  (list (make-ta (ta-name (first tas))       ;update ta
                                                 (sub1 (ta-max (first tas)))    
                                                 (ta-avail (first tas))))
                                  (rest tas))]
                         [else
                          (new-tas name                                      ;traverse rest
                                   (rest tas)
                                   (append rsf (list (first tas))))])]))]
    
    (build-lo-acc (acc-slots acc0))))
