#lang racket/base
(require lang/posn)
(require test-engine/racket-tests)
(require racket/local)
(require 2htdp/image)
(require 2htdp/universe) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Types

; A S_state is a struture:
;    (s_state (pos dir sp col))
; where:
;    pos: List<Posn>
;    dir: String
;    sp: Number
;    col: Color
; Interpretation: a snake state contains a list of positions of all its parts(pos), a string states that the current direction of snake(dir),
;   a number set the size fo each part of snake(sp) and teh color of the snake(col).

(struct s_state (pos dir sp col) #:transparent)

; A F_state is a struture:
;    (f_state (pos col ty))
; where:
;    pos: Posn
;    col: Color
;    ty: Number
; Interpretation: a food state contains the position of the food, color of the food and a number stand for the type of food.(0 stand for normal food
;    will increase the length of snake by 1, -1 stand for poison food will decrease the length of snake by 1, 1 stand for double food will increase
;    the length of snake by 2)
(struct f_state (pos col ty) #:transparent)

; A W_state is a struture
;    (w_state snake food)
; where:
;    snake: S_state
;    food: List<F_state>
; Interpretation: a world state contains a S-state and a list of F_state.

(struct w_state (snake food) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

; The width of each part of snake on canvas
(define gap 20)

;; Some constants for testing
(define pos_list (list (make-posn 100 100) (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100)))
(define pos_list1 (list (make-posn 720 100) (make-posn 740 100) (make-posn 760 100) (make-posn 780 100) (make-posn 800 100)))
(define sk_1 (s_state pos_list "right" gap (make-color 0 0 0)))
(define sk_2 (s_state pos_list1 "right" gap (make-color 0 0 0)))
(define fd_1 (f_state (make-posn 200 200) (make-color 0 0 0) 0))
(define fd_2 (f_state (make-posn 300 300) (make-color 255 0 0) 1))
(define i (w_state sk_1 (list fd_1)))
(define i1 (w_state sk_2 (list fd_1 fd_2)))

; Frame refresh rates
(define speed 0.2)

; Game canvas
(define canvas_w 1620)
(define canvas_h 820)
(define BACKGROUND (empty-scene canvas_w canvas_h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper function

; get_index: List<Any> Any -> Number
; return the index of input element in a non-empty list
; header: (define (get_index '() 0))

(define (get_index los a)
  (cond
    [(equal? a (car los)) 0]
    [else (+ 1 (get_index (cdr los) a))]));

(check-expect (get_index (list (make-posn 1 1) (make-posn 2 1) (make-posn 3 1)) (make-posn 3 1)) 2)


; delete _by_value: List<F_state> Posn -> List<F_state>
; return a new f_state list that delete the element in given f_state list by the given position
; header: (define (delete_by_value '() 0))

(define (delete_by_value los val)
  (cond
    [(null? los) los]
    [(equal? val (f_state-pos (car los))) (cdr los)]
    [else (cons (car los) (delete_by_value (cdr los) val))]))

(check-expect (delete_by_value (list fd_1 fd_2) (make-posn 120 100)) (list fd_1 fd_2))
(check-expect (delete_by_value (list fd_1 fd_2) (make-posn 200 200)) (list fd_2))
(check-expect (delete_by_value (list fd_1 fd_2) (make-posn 300 300)) (list fd_1))

; get_head: List<S_state> -> S_state
; return the position of last element in s_state list which is defined as the position pf head of the snake.
; header: (define (get_head sk_1))

(define (get_head s)
  (list-ref (s_state-pos s) (- (length (s_state-pos s)) 1)))

(check-expect (get_head sk_1) (make-posn 180 100))

; get_random_food: -> F_state
; return a random f_state
; header: (define (get_random_food))

(define (get_random_food)
  (f_state (make-posn (* 20 (random -20 20)) (* 20 (random -20 20))) (make-color (random 0 255) (random 0 255) (random 0 255)) (random -1 1)))

; get_by_value: List<F_state> Posn -> F_state
; return a f_state based on the input position
; header: (define (get_by_value fd_1))

(define (get_by_value los val)
  (cond
    [(null? los) los]
    [(equal? val (f_state-pos (car los))) (car los)]
    [else (get_by_value (cdr los) val)]))

(check-expect (get_by_value (list fd_1 fd_2) (make-posn 200 200)) fd_1)
(check-expect (get_by_value (list fd_1 fd_2) (make-posn 300 300)) fd_2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move

; move_right: S_state -> S_state
; return a new s_state which stand that the snake move 1 step to right


(define (move_right s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (+ (s_state-sp s) (posn-x x)) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p))
          (define (cr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (- (- (/ canvas_w 2) (/ gap 2))) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
    (cond
     [(equal? (- (/ canvas_w 2) (/ gap 2)) (posn-x (get_head s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
     [else (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))])))

(check-expect (s_state-pos (move_right sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 200 100)))


; move_left: S_state -> S_state
; return a new s_state which stand that the snake move 1 step to left

(define (move_left s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (+ (- (s_state-sp s)) (posn-x x)) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p))
  (define (cr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (- (/ canvas_w 2) (/ gap 2)) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
    (cond
     [(equal? (- (- (/ canvas_w 2) (/ gap 2))) (posn-x (get_head s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
     [else (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))])))

(check-expect (s_state-pos (move_left sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 160 100)))

; move_up: S_state -> S_state
; return a new s_state which stand that the snake move 1 step up

(define (move_up s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (+ (- (s_state-sp s)) (posn-y x)))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p))
  (define (cr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (- (/ canvas_h 2) (/ gap 2)))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
    (cond
     [(equal? (- (- (/ canvas_h 2) (/ gap 2))) (posn-y (get_head s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
     [else (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))])))

(check-expect (s_state-pos (move_up sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 180 80)))

; move_down: S_state -> S_state
; return a new s_state which stand that the snake move 1 step down

(define (move_down s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (+ (s_state-sp s) (posn-y x)))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p))
  (define (cr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (- (- (/ canvas_h 2) (/ gap 2))))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
    (cond
     [(equal? (+ (- (/ canvas_h 2) (/ gap 2))) (posn-y (get_head s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
     [else (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))])))

(check-expect (s_state-pos (move_down sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 180 120)))

;; Render

(define (draw_snake los col)
  (local ((define s_head (rectangle 20 20 "solid" "red"))
          (define s_part (rectangle 20 20 "solid" col)))
    (cond
     [(null? (cdr los)) (underlay/offset BACKGROUND (posn-x (car los)) (posn-y (car los))  s_head)]
     [else (underlay/offset (draw_snake (cdr los) col) (posn-x (car los)) (posn-y (car los))  s_part)])))

(define (draw_food cav los)
  (local ((define f_n (rectangle 20 20 "solid" (f_state-col (car los))))
          (define f_p (circle 10 "solid" (f_state-col (car los))))
          (define f_d (triangle 20 "solid" (f_state-col (car los)))))
          
  (cond
    [(null? los) cav]
    [(null? (cdr los)) (cond
                         [(equal? 0 (f_state-ty (car los))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_n)]
                         [(equal? -1 (f_state-ty (car los))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_p)]
                         [(equal? 1 (f_state-ty (car los))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_d)])]
    [else (cond
            [(equal? 0 (f_state-ty (car los))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_n)]
            [(equal? -1 (f_state-ty (car los))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_p)]
            [(equal? 1 (f_state-ty (car los))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_d)])]))) 


(define (draw_score cav score)
  (underlay/offset cav -20 -450 (text (string-append "Score:  " (number->string (- score 5))) 40 "indigo")))

(define (render w)
  (draw_score (draw_food (draw_snake (s_state-pos (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)) (length (s_state-pos (w_state-snake w)))))
 
;; Key

(define (handle-key w key-event)
  (cond
    [(string=? "w" key-event) (if (equal? "down" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "up" (s_state-sp (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)))]
    [(string=? "a" key-event) (if (equal? "right" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "left" (s_state-sp (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)))]
    [(string=? "s" key-event) (if (equal? "up" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "down" (s_state-sp (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)))]
    [(string=? "d" key-event) (if (equal? "left" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "right" (s_state-sp (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)))]
    [else w]))

;; Collision

(define (detect head los)
  (map (lambda (x) (if (equal? head (f_state-pos x)) #t x)) los))

(check-expect (detect (make-posn 180 100) (list fd_1 fd_2)) (list fd_1 fd_2))
(check-expect (car (detect (make-posn 200 200) (list fd_1 fd_2))) #t)
(check-expect (car (cdr (detect (make-posn 300 300) (list fd_1 fd_2)))) #t)

(define (det? los)
  (cond
    [(null? los) #f]
    [(equal? #t (car los)) #t]
    [else (det? (cdr los))]))

(check-expect (det? (detect (make-posn 180 100) (list fd_1 fd_2))) #f)
(check-expect (det? (detect (make-posn 200 200) (list fd_1 fd_2))) #t)
(check-expect (det? (detect (make-posn 300 300) (list fd_1 fd_2))) #t)


(define (eat? w)
  (det? (detect (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)) (w_state-food w))))

(check-expect (eat? i) #f)

(define (eat-normal w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))
          (define right_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ gap (posn-x last_element)) (posn-y last_element)))))
          (define left_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ (- gap) (posn-x last_element)) (posn-y last_element)))))
          (define up_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ (- gap)(posn-y last_element))))))
          (define down_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ gap (posn-y last_element))))))
          (define (update_food f_list) (append (list (get_random_food)) (delete_by_value (w_state-food w) last_element)))
          )
  (cond
    [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (s_state right_to_head "right" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "left" (s_state-dir (w_state-snake w))) (w_state (s_state left_to_head "left" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "up" (s_state-dir (w_state-snake w))) (w_state (s_state up_to_head "up" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "down" (s_state-dir (w_state-snake w))) (w_state (s_state down_to_head "down" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))])))


(define (eat-poison w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))
          (define pop_head (remove last_element (s_state-pos (w_state-snake w))))
          (define (update_food f_list) (append (list (get_random_food) (get_random_food)) (delete_by_value (w_state-food w) last_element)))
          )
  (w_state (s_state pop_head (s_state-dir (w_state-snake w)) gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))))


(define (eat-double w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))
          (define right_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ gap (posn-x last_element)) (posn-y last_element)) (make-posn (+ (* 2 gap) (posn-x last_element)) (posn-y last_element)))))
          (define left_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ (- gap) (posn-x last_element)) (posn-y last_element)) (make-posn (+ (- (* 2 gap)) (posn-x last_element)) (posn-y last_element)))))
          (define up_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ (- gap)(posn-y last_element))) (make-posn (posn-x last_element) (+ (- (* 2 gap)) (posn-y last_element))))))
          (define down_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ gap (posn-y last_element))) (make-posn (posn-x last_element) (+ (* 2 gap) (posn-y last_element))))))
          (define (update_food f_list) (append (list (get_random_food)) (delete_by_value (w_state-food w) last_element)))
          )
  (cond
    [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (s_state right_to_head "right" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "left" (s_state-dir (w_state-snake w))) (w_state (s_state left_to_head "left" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "up" (s_state-dir (w_state-snake w))) (w_state (s_state up_to_head "up" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))]
    [(equal? "down" (s_state-dir (w_state-snake w))) (w_state (s_state down_to_head "down" gap (f_state-col (get_by_value (w_state-food w) last_element))) (update_food (w_state-food w)))])))
(test)

;; Tick

(define (tick w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1))))
          (cond
            [(eat? w) (cond
                        [(equal? -1 (f_state-ty (get_by_value (w_state-food w) last_element))) (eat-poison w)]
                        [(equal? 0 (f_state-ty (get_by_value (w_state-food w) last_element))) (eat-normal w)]
                        [(equal? 1 (f_state-ty (get_by_value (w_state-food w) last_element))) (eat-double w)])]
            [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (move_right (w_state-snake w)) (w_state-food w))]
            [(equal?  "left" (s_state-dir (w_state-snake w))) (w_state (move_left (w_state-snake w)) (w_state-food w))]
            [(equal?  "up" (s_state-dir (w_state-snake w))) (w_state (move_up (w_state-snake w)) (w_state-food w))]
            [(equal?  "down" (s_state-dir (w_state-snake w))) (w_state (move_down (w_state-snake w)) (w_state-food w))])))

(define (exist? los h)
  (cond
    [(null? (cdr los)) #f]
    [(equal? h (car los)) #t]
    [else (exist? (cdr los) h)]))
  

  
(define (quit? w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))
          (define (colli? s) (exist? (s_state-pos s) last_element))
          )
    (cond
      [(colli? (w_state-snake w)) #t]
      [else #f])))


(define (play initial-state)
(big-bang initial-state
[on-key handle-key]
[on-tick tick speed]
[to-draw render]
[stop-when quit?]))

(play i)

