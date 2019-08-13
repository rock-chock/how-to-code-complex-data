
;; graphs-v4.rkt

; 
; PROBLEM: 
; 
; Imagine you are suddenly transported into a mysterious house, in which all
; you can see is the name of the room you are in, and any doors that lead OUT
; of the room.  One of the things that makes the house so mysterious is that
; the doors only go in one direction. You can't see the doors that lead into
; the room.
;
; 
; In computer science, we refer to such an information structure as a directed
; graph. Like trees, in directed graphs the arrows have direction. But in a
; graph it is  possible to go in circles, as in the second example above. It
; is also possible for two arrows to lead into a single node, as in the fourth
; example.
;
;
; Design a data definition to represent such houses. Also provide example data
; for the four houses above.
; 


(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to
 
(define H1 (make-room "A" (list (make-room "B" empty))))
 
(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template:
;;  tail recursive encapsulate w/ local, worklist, context-preserving
;;  accumulator with what rooms have we already visited in complete
;;  tail recursive traversal so far

#;
(define (fn-for-house r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited))))  ; (room-name r)
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty))) 


;; Room String -> Boolean
;; produce true if starting at r0 it is possible to reach a room named rn
(check-expect (reachable? H1 "A") true)
(check-expect (reachable? H1 "B") true)
(check-expect (reachable? H1 "C") false)
(check-expect (reachable? (first (room-exits H1)) "A") false)
(check-expect (reachable? H4 "F") true)

;(define (reachable? r0 rn) false) ;stub

;; template: Room (graph)

(define (reachable? r0 rn)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); names of rooms already visited
  (local [(define (fn-for-room r todo visited)
            (cond [(string=? (room-name r) rn) true]
                  [else
                   (if (member (room-name r) visited)
                       (fn-for-lor todo visited)
                       (fn-for-lor (append (room-exits r) todo)
                                   (cons (room-name r) visited)))]))
          
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty))) 




;; Room -> Natural
;; produce the total number of rooms reachable from a given room, including the room itself
(check-expect (num-rooms H1) 2)
(check-expect (num-rooms (first (room-exits H1))) 1)
(check-expect (num-rooms H2) 2)
(check-expect (num-rooms H3) 3)
(check-expect (num-rooms (first (room-exits H3))) 3)
(check-expect (num-rooms H4) 6)
(check-expect (num-rooms (first (room-exits H4))) 6)

(define (num-rooms r0)
  ;; todo is (listof Room); worklist acc
  ;; visited is (listof String); context preserving acc, names of rooms visited
  (local [(define (fn-for-room r todo visited)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo) 
                            (cons (room-name r) visited))))
           (define (fn-for-lor todo visited)
             (cond [(empty? todo) (length visited)] 
                   [else 
                    (fn-for-room (first todo)
                                 (rest todo)
                                 visited)]))]
    (fn-for-room r0 empty empty)))



;; Room -> (listof String)
;; Given a room, produces a list of the names of all the reachable  rooms
(check-expect (reachable-rooms H1) (list "A" "B"))
(check-expect (reachable-rooms (first (room-exits H1))) (list "B"))
(check-expect (reachable-rooms H2) (list "A" "B"))
(check-expect (reachable-rooms H3) (list "A" "B" "C"))
(check-expect (reachable-rooms (first (room-exits H3))) (list "B" "C" "A"))

(check-expect (reachable-rooms H4) (list "A" "B" "C" "E" "F" "D")) ;; ???


(define (reachable-rooms r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited)
                (fn-for-lor (append (room-exits r) todo)
                            (append visited (cons (room-name r) empty))))) ; (... (room-name r))
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) visited]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    (fn-for-room r0 empty empty)))



;; Room -> (listof Room)
;; Produces a list of all the reachable rooms
(check-expect (reachable-lor H1) (shared ((-A- (make-room "A" (list -B-)))
                                          (-B- (make-room "B" (list))))
                                   (list -A- -B-)))
(check-expect (reachable-lor (first (room-exits H1))) (list (make-room "B" empty)))
(check-expect (reachable-lor H2)
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -A-))))
                (list -A- -B-)))
(check-expect (reachable-lor H4)
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                (list -A- -B- -C- -E- -F- -D-)))

(define (reachable-lor r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf is (listof Room); a result-so-far acc, rooms that are already visited
  (local [(define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (append visited (cons (room-name r) empty))  ; (... (room-name r))
                            (append rsf (cons r empty)))))

          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf)]))]
    
    (fn-for-room r0 empty empty empty)))


; 
; PROBLEM:
; 
; Using the following data definition, design a function that consumes a room and produces 
; the total number of rooms reachable from the given room. Include the starting room itself. 
; Your function should be tail recursive, but you should not use the primitive length function.
; 



;; Room -> Natural
;; Produce number of reachable rooms 
(check-expect (count-rooms H1) 2)
(check-expect (count-rooms (first (room-exits H1))) 1)
(check-expect (count-rooms H2) 2)
(check-expect (count-rooms H3) 3)
(check-expect (count-rooms (first (room-exits H3))) 3)
(check-expect (count-rooms H4) 6)
(check-expect (count-rooms (first (room-exits H4))) 6)

;(define (count-rooms r0) 0) ;stub


(define (count-rooms r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf is Natural; result-so-far acc, amount of rooms seen so far 
  (local [(define (fn-for-room r todo visited rsf) 
            (if (member (room-name r) visited)
                (fn-for-lor todo visited rsf)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (add1 rsf)))) ; (... (room-name r))
          (define (fn-for-lor todo visited rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf)]))]
    (fn-for-room r0 empty empty 0)))


; 
; PROBLEM:
; 
; Using the following data definition, design a function that consumes a room and a room 
; name and tries to find a room with the given name starting at the given room.
; 


;; Room String -> Room || False
;; produce a room with a given name
(check-expect (lookup-room H1 "Z") false) ; didn't find room with name "Z" 
(check-expect (lookup-room H1 "A")
              (shared ((-A- (make-room "A" (list (make-room "B" (list))))))
                -A-))
(check-expect (lookup-room H4 "B")
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -B-))

;(define (lookup-room r0 "") (make-room "A" (list))) ;stub

(define (lookup-room r0 s)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  (local [(define (fn-for-room r todo visited) 
            (cond [(member (room-name r) visited) (fn-for-lor todo visited)]
                  [(string=? (room-name r) s) r]
                  [else
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited))]))
          
          (define (fn-for-lor todo visited)
            (cond [(empty? todo) false]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited)]))]
    
    (fn-for-room r0 empty empty)))
    

; 
; PROBLEM:
; 
; Using the following data definition, design a function that produces the room with the most exits 
; (in the case of a tie you can produce any of the rooms in the tie).
; 


;; Room -> Room
;; produce a room with the most exits
;; (if there are 2 or more rooms with the same amount of exits, produce visited earlier) ???
(check-expect (max-exits-from H1)
              (shared ((-A- (make-room "A" (list (make-room "B" (list))))))
                -A-))
(check-expect (max-exits-from H2)
              (shared ((-A- (make-room "A" (list (make-room "B" (list -A-))))))
                -A-))
(check-expect (max-exits-from (first (room-exits H3)))
              (shared ((-B- (make-room "B" (list -C-)))
                       (-C- (make-room "C" (list -A-)))
                       (-A- (make-room "A" (list -B-))))
                -B-))
(check-expect (max-exits-from (first (room-exits H4)))
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -B-))
(define H5 (shared ((-A- (make-room "A" (list -B-)))
                    (-B- (make-room "B" (list -C- -E-)))
                    (-C- (make-room "C" (list -B-)))
                    (-E- (make-room "E" (list -F- -A- -D-)))
                    (-D- (make-room "D" (list -E-)))
                    (-F- (make-room "F" (list))))
             -A-))
(check-expect (max-exits-from H5)
              (shared ((-A- (make-room "A" (list -B-)))
                    (-B- (make-room "B" (list -C- -E-)))
                    (-C- (make-room "C" (list -B-)))
                    (-E- (make-room "E" (list -F- -A- -D-)))
                    (-D- (make-room "D" (list -E-)))
                    (-F- (make-room "F" (list))))
             -E-))

;(define (max-exits-from r0) (make-room "A" (list))) ;stub

(define (max-exits-from r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; rsf-room is Room; a result-so-far acc, room with the most exits
  (local [(define (fn-for-room r todo visited rsf-room) 
            (cond [(member (room-name r) visited)
                   (fn-for-lor todo visited rsf-room)]
                  [(< (length (room-exits rsf-room)) (length (room-exits r)))
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited)
                               r)]
                  [else
                   (fn-for-lor (append (room-exits r) todo)
                               (cons (room-name r) visited)
                               rsf-room)]))                
          
          (define (fn-for-lor todo visited rsf-room)
            (cond [(empty? todo) rsf-room]
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                rsf-room)]))]
    
    (fn-for-room r0 empty empty r0)))
 


; 
; PROBLEM:
; 
; Using the following data definition, design a function that produces the room to which the greatest 
; number of other rooms have exits (in the case of a tie you can produce any of the rooms in the tie).
; 


;; Room -> Room
;; Produce the room which has more arrows (the most frequently met in room-exits) from other rooms
;; If 2 rooms have equal amount of arrows, produce any
;; ASSUME: graph has at least 2 rooms
(check-expect (max-exits-to H1)
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list))))
    -B-))
(check-expect (max-exits-to H2)
              (shared ((-A- (make-room "A" (list -B-)))
                       (-B- (make-room "B" (list -A-))))
    -B-))
(check-expect (max-exits-to H4)
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -B-))

(define H6 (shared ((-A- (make-room "A" (list -B- -C-)))
                    (-B- (make-room "B" (list -C-)))
                    (-C- (make-room "C" (list -A-))))
             -A-))

(check-expect (max-exits-to H6)
              (shared ((-A- (make-room "A" (list -B- -C-)))
                       (-B- (make-room "B" (list -C-)))
                       (-C- (make-room "C" (list -A-))))
                -C-))

(define (max-exits-to r0)
  (local [(define-struct rc (room count))
          ;; RC (RoomCount) is (make-rc Room Natural)
          ;; interp. a (list of RSF) entry with Room and counter (number of times room is in exits)

          
          ;; todo is (listof Room); a worklist accumulator
          ;; visited is (listof String); context preserving accumulator, names of rooms visited
          ;; lorc is (list of RC); a result-so-far accumulator
          (define (fn-for-room r todo visited lorc)
            (if (member (room-name r) visited)
                (fn-for-lor todo
                            visited
                            (update lorc (room-exits r)))   ; -> (list of RC)
                (fn-for-lor (append (room-exits r) todo)
                            (cons (room-name r) visited)
                            (update lorc (room-exits r))))) ; -> (list of RC)
          
          (define (fn-for-lor todo visited lorc)
            (cond [(empty? todo)
                   (most-visited lorc (first lorc))] ; -> Room
                  [else
                   (fn-for-room (first todo) 
                                (rest todo)
                                visited
                                lorc)]))
          
          
          ;; (listof RC) (listof Room) -> (list of RC)
          ;; update lorc with lor
          (define (update lorc lor)
            (cond [(empty? lor) lorc] 
                  [else
                   (update (update-one empty lorc (first lor) lorc) ; -> (list of RC)
                           (rest lor))]))
          
          ;; (listof RC) (listof RC) Room (listof RC) -> (list of RC)
          ;; update lorc with one r
          (define (update-one visited worklist r lorc)
            ;; visited is (list of RC); context preserving acc
            ;; worklist is (list of RC); worklist acc
            (cond [(empty? worklist)
                   (append lorc                   
                           (list (make-rc r 1)) )]
                  [else 
                   (local [(define this (first worklist))]      ; RC 
                     (if (string=? (room-name (rc-room this))   
                                   (room-name r))
                         (append visited                       
                                 (list (make-rc (rc-room this)            
                                                (add1 (rc-count this)))) 
                                 (rest worklist))
                         (update-one (append visited (list this))
                                     (rest worklist)
                                     r
                                     lorc)))]))
 
          
          ;; (list of RC) -> Room
          ;; produce the room with the greatest counter.
          (define (most-visited lorc rsf)
            ;; rsf is RC; a result-so-far acc for storing rc with the largest counter.
            (cond [(empty? lorc) (rc-room rsf)]
                  [else
                   (local [(define this (first lorc))]
                     (if (> (rc-count this)
                            (rc-count rsf))
                         (most-visited (rest lorc) this)
                         (most-visited (rest lorc) rsf)))]))]
    
    (fn-for-room r0 empty empty empty)))
