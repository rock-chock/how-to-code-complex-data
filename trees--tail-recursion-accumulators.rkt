;; Accumulators P11

; Consider the following house diagram:
; 
;  _____________________
; |Porch         Living |
; |_________|    Room   |           
; |Dining   |           |
; |Room     _   ________|
; |         |   | Study |
; |         | H         |
; |___    __| a |_______|
; |Kitchen  | l | Bed-  |
; |         | l | room  |
; |         |    _______|
; |         |   | Bath- |
; |               room  |
; |_________|___|_______|     
; 
; 
; Starting from the porch, there are many paths through the house that you can
; follow without retracing your steps.  If we represent these paths as lists:
; (list 
;  (list "Porch")
;  (list "Porch" "Living Room")
;  (list "Porch" "Living Room" "Hall")
;  ...)
; 
; you can see that a lot of these paths start with the same sequence of rooms.
; We can represent these paths, and capture their shared initial parts, by using
; a tree.
; 
; The following data definition does exactly this.


(define-struct path (room nexts))
;; Path is (make-path String (listof Path))
;; interp. An arbitrary-arity tree of paths.
;;  - (make-path room nexts) represents all the paths downward from room
(define P0 (make-path "A" empty)) ; a room from which there are no paths

(define PH 
  (make-path "Porch"
   (list 
    (make-path "Living Room"
      (list (make-path "Dining Room"
              (list (make-path "Kitchen"
                      (list (make-path "Hall"
                              (list (make-path "Study" (list))
                                    (make-path "Bedroom" (list))
                                    (make-path "Bathroom" (list))))))))
            (make-path "Hall"
              (list (make-path "Kitchen"
                      (list (make-path "Dining Room" (list))))
                    (make-path "Study" (list))
                    (make-path "Bedroom" (list))
                    (make-path "Bathroom" (list)))))))))
   
#;
(define (fn-for-path p)
  (local [(define (fn-for-path p)
            (... (path-room p)
                 (fn-for-lop (path-nexts p))))
          (define (fn-for-lop lop)
            (cond [(empty? lop) (...)]
                  [else
                   (... (fn-for-path (first lop))
                        (fn-for-lop (rest lop)))]))]
    (fn-for-path p)))



; The problems below also make use of the following data definition and function:


;; Result is one of:
;; - Boolean
;; - "never"
;; interp. three possible answers to a question
(define R0 true)
(define R1 false)
(define R2 "never")

#;
(define (fn-for-result r)
  (cond 
    [(boolean? r) (... r)]
    [else (...)]))

;; Result Result -> Result
;; produce the logical combination of two results
; Cross Product of Types Table:
; 
;  ╔════════════════╦═══════════════╦══════════════╗
;  ║                ║               ║              ║
;  ║            r0  ║   Boolean     ║   "never"    ║
;  ║                ║               ║              ║
;  ║    r1          ║               ║              ║
;  ╠════════════════╬═══════════════╬══════════════╣
;  ║                ║               ║              ║
;  ║   Boolean      ║ (and r0 r1)   ║              ║
;  ║                ║               ║              ║
;  ╠════════════════╬═══════════════╣  r1          ║
;  ║                ║               ║              ║
;  ║   "never"      ║  r0           ║              ║
;  ║                ║               ║              ║
;  ╚════════════════╩═══════════════╩══════════════╝


(check-expect (and-result false false) false)
(check-expect (and-result false true) false)
(check-expect (and-result false "never") false)
(check-expect (and-result true false) false)
(check-expect (and-result true true) true)
(check-expect (and-result true "never") true)
(check-expect (and-result "never" true) true)
(check-expect (and-result "never" false) false)
(check-expect (and-result "never" "never") "never")

(define (and-result r0 r1)
  (cond [(and (boolean? r0) (boolean? r1)) (and r0 r1)]
        [(string? r0) r1]
        [else r0]))


;; Result Result -> Result
;; produce the logical combination of two results
; Cross Product of Types Table:
; 
;  ╔════════════════╦═══════════════╦══════════════╗
;  ║                ║               ║              ║
;  ║            r0  ║   Boolean     ║   "never"    ║
;  ║                ║               ║              ║
;  ║    r1          ║               ║              ║
;  ╠════════════════╬═══════════════╬══════════════╣
;  ║                ║               ║              ║
;  ║   Boolean      ║ (or r0 r1)    ║              ║
;  ║                ║               ║              ║
;  ╠════════════════╬═══════════════╣  r1          ║
;  ║                ║               ║              ║
;  ║   "never"      ║  r0           ║              ║
;  ║                ║               ║              ║
;  ╚════════════════╩═══════════════╩══════════════╝


(check-expect (or-result false false) false)
(check-expect (or-result false true) true)
(check-expect (or-result false "never") false)
(check-expect (or-result true false) true)
(check-expect (or-result true true) true)
(check-expect (or-result true "never") true)
(check-expect (or-result "never" true) true)
(check-expect (or-result "never" false) false)
(check-expect (or-result "never" "never") "never")

(define (or-result r0 r1)
  (cond [(and (boolean? r0) (boolean? r1)) (or r0 r1)]
        [(string? r0) r1]
        [else r0]))

; PROBLEM 1:   
; 
; Design a function called always-before that takes a path tree p and two room
; names b and c, and determines whether starting from p:
; 1) you must pass through room b to get to room c (produce true),
; 2) you can get to room c without passing through room b (produce false), or
; 3) you just can't get to room c (produce "never").
; 
; Note that if b and c are the same room, you should produce false since you don't
; need to pass through the room to get there.
; 

 
;; Path String String -> Result
;; Consume a path tree p and two rooms (b and c), produce:
;; - true if you must pass through room b to get to room c
;; - false if you can get to room c without passing through room b (and if both rooms are the same)
;; - "never" if you just can't get to room c
(check-expect (always-before (make-path "Study" (list)) "Study" "Bedroom") "never")
(check-expect (always-before (make-path "Study" (list)) "Kitchen" "Dining Room") "never")
(check-expect (always-before (make-path "Bedroom" (list)) "Study" "Study") "never")
(check-expect (always-before (make-path "Study" (list)) "Study" "Study") false)
(check-expect (always-before (make-path "Hall"
                                        (list (make-path "Study" (list))
                                              (make-path "Bedroom" (list))
                                              (make-path "Bathroom" (list))))
                             "Study"
                             "Hall")
              false)
(check-expect (always-before (make-path "Hall"
                                        (list (make-path "Study" (list))
                                              (make-path "Bedroom" (list))
                                              (make-path "Bathroom" (list))))
                             "Hall"
                             "Study")
              true)
(check-expect (always-before PH "Kitchen" "Dining Room") true)
(check-expect (always-before PH "Dining Room" "Kitchen") true)
(check-expect (always-before PH "Living Room" "Kitchen") true)
(check-expect (always-before PH "Kitchen" "Study") true)
(check-expect (always-before PH "Hall" "Dining Room") true)  
(check-expect (always-before PH "Dining Room" "Bathroom") true)

;(define (always-before p b c) "never") ;stub

;; template as: Path (arb-arity tree, wrapped in local)
;;              added passed-b? (result-so-far accumulator)


(define (always-before p b c)
  ;; passed-b? is Result; a result-so-far accumulator
  (local [(define (fn-for-path p passed-b?)
            (cond [(and passed-b? (string=? (path-room p) c)) 
                   true]
                  [(string=? (path-room p) c) false]
                  [(string=? (path-room p) b)
                    (fn-for-lop (path-nexts p) true)]    ; update passed-b?
                  [else
                   (fn-for-lop (path-nexts p) passed-b?)]))

          
          (define (fn-for-lop lop passed-b?)
            (cond [(empty? lop) "never"]
                  [else
                   (or-result (fn-for-path (first lop) passed-b?)
                              (fn-for-lop (rest lop) passed-b?))]))]
    (fn-for-path p false)))



; OPTIONAL EXTRA PRACTICE PROBLEM:
; 
; Once you have always-before working, make a copy of it, rename the copy to
; always-before-tr, and then modify the function to be tail recursive.
; 

;; tail-recursive version

(check-expect (always-before-tr (make-path "Study" (list)) "Study" "Bedroom") "never")
(check-expect (always-before-tr (make-path "Study" (list)) "Kitchen" "Dining Room") "never")
(check-expect (always-before-tr (make-path "Bedroom" (list)) "Study" "Study") "never")
(check-expect (always-before-tr (make-path "Study" (list)) "Study" "Study") false)
(check-expect (always-before-tr (make-path "Hall"
                                           (list (make-path "Study" (list))
                                                 (make-path "Bedroom" (list))
                                                 (make-path "Bathroom" (list))))
                                "Study"
                                "Hall")
              false)
(check-expect (always-before-tr (make-path "Hall"
                                           (list (make-path "Study" (list))
                                                 (make-path "Bedroom" (list))
                                                 (make-path "Bathroom" (list))))
                                "Hall"
                                "Study")
              true)
(check-expect (always-before-tr PH "Kitchen" "Dining Room") true)
(check-expect (always-before-tr PH "Dining Room" "Kitchen") true)
(check-expect (always-before-tr PH "Living Room" "Kitchen") true)
(check-expect (always-before-tr PH "Kitchen" "Study") true)
(check-expect (always-before-tr PH "Hall" "Dining Room") true)       
(check-expect (always-before-tr PH "Dining Room" "Bathroom") true)
(check-expect (always-before-tr PH "Dining Room" "Kitchen") true)

;; template as: Path (arbitrary-arity tree, wrapped in local)
;;              added passed-b? (context-preserving accumulator, part of WLE)
;;              added worklist (worklist accumulator for tail recursion; depth-first traversal)
;;              added WLE (compound data definition for worklist entries - to store context-preserving accumulator)
;;              added result (result-so-far accumulator)
(define (always-before-tr p b c)
  ;; passed-b? is Result; a context-preserving accumulator;  
  ;; worklist  is (listof WLE); a worklist accumulator for tail recursion; paths not yet visited
  ;; result    is Result; a result-so-far accumulator
  (local [(define-struct wle (p passed-b?))
          ;; WLE (worklist entry) is (make-wle Path Result)
          ;; interp. a worklist entry with 1)the path to pass to a fn-for-lop and 2)passed-b? for that path
          (define (fn-for-path p passed-b? worklist result)
            (cond [(and passed-b? (string=? (path-room p) c))  ;If already passed b and now found c
                   true]                                       ;   (worklist: no worklist) (result: immediately return true)
                  [(string=? (path-room p) c)                  ;If found c, but didn't pass b
                   (fn-for-lop worklist false)]                ;   (worklist: old worklist) (result: false)
                  [(string=? (path-room p) b)                  ;If found b
                   (fn-for-lop                                 ;   (worklist: set passed-b? of subs to true) (result: old result)
                    (append (map (lambda (sub-p)                       
                                   (make-wle sub-p true))      ;pack worklist
                                 (path-nexts p))
                            worklist)
                    result)]                 
                  [else                                        ;Didn't find b nor c
                   (fn-for-lop                                 ;   (worklist: set passed-b? of subs to current passed-b?) (result: old result)
                    (append (map (lambda (sub-p)                       
                                   (make-wle sub-p passed-b?)) ;pack worklist
                                 (path-nexts p))
                            worklist)
                    result)]))
          
          (define (fn-for-lop worklist result)
            (cond [(empty? worklist)  result]                   ;return "never" or "false"                   
                  [else
                   (fn-for-path (wle-p (first worklist))        ;unpack first of worklist
                                (wle-passed-b? (first worklist)) 
                                (rest worklist)
                                result)]))]
    (fn-for-path p false empty "never")))



;;==============================================================
;; Accumulators P12

; Problem:
; 
; Starting with the following data definition for a binary tree (not a binary search tree) 
; design a tail-recursive function called contains? that consumes a key and a binary tree 
; and produces true if the tree contains the key.
; 


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))
#;
(define (fn-for-bt bt)
  (cond [(false? bt) (...)]
        [else
         (... (node-k bt)
              (node-v bt)
              (fn-for-bt (node-l bt))
              (fn-for-bt (node-r bt)))]))


;; Integer BT -> Boolean
;; produce true if a given BT contains a given tree
(check-expect (contains? 1 false) false)
(check-expect (contains? 3 (make-node 7 "g" false false)) false)
(check-expect (contains? 7 (make-node 7 "g" false false)) true)
(check-expect (contains? 3 BT2) false)
(check-expect (contains? 7 BT2) true)
(check-expect (contains? 1 BT2) true)

;(define (contains? k bt) false) ;stub

;; template as: BT (binary tree)
;;              added worklist (worklist accumulator for tail recursion)
;;              added fn-for-worklist (for traversing a worklist)
(define (contains? k bt0)
  ;; worklist is (listof BT); a worklist accumulator for tail recursion; trees yet not visited
  (local [(define (fn-for-bt bt worklist)
            (cond [(false? bt) (fn-for-worklist worklist)] ;if reached the end of bt, continue to search k in worklist
                  [else
                   (if (equal? (node-k bt) k)
                       true                                ;return true if found k in bt0
                       (fn-for-bt (node-l bt)              ;call this fn-for-bt to reach the end of current tree
                                  (cons (node-r bt) worklist)))]))

          (define (fn-for-worklist worklist)
            (cond [(empty? worklist) false]                ;base case: reached the end of worklist
                  [else
                    (fn-for-bt (first worklist) (rest worklist))]))]
    
    (fn-for-bt bt0 empty)))




