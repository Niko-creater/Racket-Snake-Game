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
; Interpretation: a food state contains the position of the food, color of the food and a number stand for the type of food.(1 2 stand for normal food
;    will increase the length of snake by 1, 3 stand for poison food will decrease the length of snake by 1, 4 stand for double food will increase
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
(define fd_1 (f_state (make-posn 200 200) (make-color 0 0 0) 4))
(define fd_2 (f_state (make-posn 300 300) (make-color 255 0 0) 1))
(define i (w_state sk_1 (list fd_1)))
(define i1 (w_state sk_2 (list fd_1 fd_2)))

; Frame refresh rates
(define speed 0.2)

; Game canvas
(define canvas_w 1020)
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

; get_head_s: List<S_state> -> S_state
; return the position of last element in s_state list which is defined as the position pf head of the snake.
; header: (define (get_head_s sk_1))

(define (get_head_s s)
  (list-ref (s_state-pos s) (- (length (s_state-pos s)) 1)))

(check-expect (get_head_s sk_1) (make-posn 180 100))

; get_random_food: -> F_state
; return a random f_state
; header: (define (get_random_food))

(define (get_random_food)
  (f_state (make-posn (* 20 (random -20 20)) (* 20 (random -20 20))) (make-color (random 0 255) (random 0 255) (random 0 255)) (random 1 5)))

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
     [(equal? (- (/ canvas_w 2) (/ gap 2)) (posn-x (get_head_s s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
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
     [(equal? (- (- (/ canvas_w 2) (/ gap 2))) (posn-x (get_head_s s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
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
     [(equal? (- (- (/ canvas_h 2) (/ gap 2))) (posn-y (get_head_s s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
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
     [(equal? (+ (- (/ canvas_h 2) (/ gap 2))) (posn-y (get_head_s s))) (s_state (cr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))]
     [else (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s) (s_state-col s))])))

(check-expect (s_state-pos (move_down sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 180 120)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render

; draw_snake: List<Posn> Color -> Image
; return a image that paint the snake with color on the game canvas
; header: (define (draw_snake '() (make-color 0 0 0)))

(define (draw_snake los col)
  (local ((define s_head (rectangle 20 20 "solid" "red"))
          (define s_part (rectangle 20 20 "solid" col)))
    (cond
     [(null? (cdr los)) (underlay/offset BACKGROUND (posn-x (car los)) (posn-y (car los))  s_head)]
     [else (underlay/offset (draw_snake (cdr los) col) (posn-x (car los)) (posn-y (car los))  s_part)])))

; draw_food: Image List<F_state> -> Image
; return the image according to the list of food and snake painted canvas
; header: (define (draw_food BACKGROUND '()))

(define (draw_food cav los)
  (local ((define f_n (rectangle 20 20 "solid" (f_state-col (car los))))
          (define f_p (circle 10 "solid" (f_state-col (car los))))
          (define f_d (triangle 20 "solid" (f_state-col (car los)))))
          
  (cond
    [(null? los) cav]
    [(null? (cdr los)) (cond
                         [(or (equal? 1 (f_state-ty (car los)))
                              (equal? 2 (f_state-ty (car los)))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_n)]
                         [(equal? 3 (f_state-ty (car los))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_p)]
                         [(equal? 4 (f_state-ty (car los))) (underlay/offset cav (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_d)])]
    [else (cond
            [(or (equal? 1 (f_state-ty (car los)))
                              (equal? 2 (f_state-ty (car los)))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_n)]
            [(equal? 3 (f_state-ty (car los))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_p)]
            [(equal? 4 (f_state-ty (car los))) (underlay/offset (draw_food cav (cdr los)) (posn-x (f_state-pos (car los))) (posn-y (f_state-pos (car los))) f_d)])]))) 

; draw_score: Image Number -> Image
; paint the score of the game on the canvas
; header; (define (draw_score BACKGROUND 0))

(define (draw_score cav score)
  (underlay/offset cav -430 -375 (text (string-append "Score:  " (number->string (- score 5))) 25 "indigo")))

; render: W_state -> Image
; return the image based the current w_state
; header: (define (render i))

(define (render w)
  (draw_score (draw_food (draw_snake (s_state-pos (w_state-snake w)) (s_state-col (w_state-snake w))) (w_state-food w)) (length (s_state-pos (w_state-snake w)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key

; handle-key: W_state String -> W_state
; Interpretation: handles mouse events; that
;   is, it updates the state according to the input’s mouse events.
;                  w: move up
;                  a: move left
;                  s: move down
;                  d: move right

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

; detect: Posn List<F_state> -> List<F_state>
; return a new f_state list which convert a f_state that have same position with input head to a boolean value.
; header: (define (detect (make-posn 0 0) fd_1))
(define (detect head los)
  (map (lambda (x) (if (equal? head (f_state-pos x)) #t x)) los))

(check-expect (detect (make-posn 180 100) (list fd_1 fd_2)) (list fd_1 fd_2))
(check-expect (car (detect (make-posn 200 200) (list fd_1 fd_2))) #t)
(check-expect (car (cdr (detect (make-posn 300 300) (list fd_1 fd_2)))) #t)

; det?: List<F_state> -> Boolean
; return ture if list has a true value.
; header: (define (det? ()'))

(define (det? los)
  (cond
    [(null? los) #f]
    [(equal? #t (car los)) #t]
    [else (det? (cdr los))]))

(check-expect (det? (detect (make-posn 180 100) (list fd_1 fd_2))) #f)
(check-expect (det? (detect (make-posn 200 200) (list fd_1 fd_2))) #t)
(check-expect (det? (detect (make-posn 300 300) (list fd_1 fd_2))) #t)

; eat?: W_state -> Boolean
; return ture if snake eat a food
; header: (define (eat? i))

(define (eat? w)
  (det? (detect (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)) (w_state-food w))))

(check-expect (eat? i) #f)

;; Three eat function
(define (get_head_w w) (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))

(define (right_to_head w n)
  (cond
    [(= 1 n) (append (s_state-pos (w_state-snake w)) (list (make-posn (+ gap (posn-x (get_head_w w))) (posn-y (get_head_w w)))))]
    [else (append (right_to_head w (- n 1)) (list (make-posn (+ (* n gap) (posn-x (get_head_w w))) (posn-y (get_head_w w)))))]))

(define (left_to_head w n)
  (cond
    [(= 1 n) (append (s_state-pos (w_state-snake w)) (list (make-posn (+ (- gap) (posn-x (get_head_w w))) (posn-y (get_head_w w)))))]
    [else (append (left_to_head w (- n 1)) (list (make-posn (+ (- (* n gap)) (posn-x (get_head_w w))) (posn-y (get_head_w w)))))]))

(define (up_to_head w n)
  (cond
    [(= 1 n) (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x (get_head_w w)) (+ (- gap)(posn-y (get_head_w w))))))]
    [else (append (up_to_head w (- n 1)) (list (make-posn (posn-x (get_head_w w)) (+ (- (* n gap))(posn-y (get_head_w w))))))])) 

(define (down_to_head w n)
  (cond
    [(= 1 n) (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x (get_head_w w)) (+ gap (posn-y (get_head_w w))))))]
    [else (append (down_to_head w (- n 1)) (list (make-posn (posn-x (get_head_w w)) (+ (* n gap) (posn-y (get_head_w w))))))])) 

(define (update_food w n)
  (cond
   [(= 1 n) (append (list (get_random_food)) (delete_by_value (w_state-food w) (get_head_w w)))]
   [else (append (list (get_random_food)) (update_food w (- n 1)))]))

(define (pop_head w) (remove (get_head_w w) (s_state-pos (w_state-snake w))))

; eat-normal: W_state -> W_state
; append a new part on the snake body
; header: (define (eat-normal i))
(define (eat-normal w)
  (cond
    [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (s_state (right_to_head w 1) "right" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "left" (s_state-dir (w_state-snake w))) (w_state (s_state (left_to_head w 1) "left" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "up" (s_state-dir (w_state-snake w))) (w_state (s_state (up_to_head w 1) "up" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "down" (s_state-dir (w_state-snake w))) (w_state (s_state (down_to_head w 1) "down" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]))

; eat-poison: W_state -> W_state
; pop the head part of the snake body
; header: (define (eat-poison i))
(define (eat-poison w)
  (w_state (s_state (pop_head w) (s_state-dir (w_state-snake w)) gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 2)))

; eat-double: W_state -> W_state
; append two new parts on the snake body
; header: (define (eat-double i))
(define (eat-double w)
  (cond
    [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (s_state (right_to_head w 2) "right" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "left" (s_state-dir (w_state-snake w))) (w_state (s_state (left_to_head w 2) "left" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "up" (s_state-dir (w_state-snake w))) (w_state (s_state (up_to_head w 2) "up" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]
    [(equal? "down" (s_state-dir (w_state-snake w))) (w_state (s_state (down_to_head w 2) "down" gap (f_state-col (get_by_value (w_state-food w) (get_head_w w)))) (update_food w 1))]))
(test)

;; Tick

; tick: W_state -> W_state
; tick he world state
; header: (define (tick w))

(define (tick w)
          (cond
            [(eat? w) (cond
                        [(equal? 3 (f_state-ty (get_by_value (w_state-food w) (get_head_w w)))) (eat-poison w)]
                        [(or (equal? 1 (f_state-ty (get_by_value (w_state-food w) (get_head_w w))))
                             (equal? 2 (f_state-ty (get_by_value (w_state-food w) (get_head_w w))))) (eat-normal w)]
                        [(equal? 4 (f_state-ty (get_by_value (w_state-food w) (get_head_w w)))) (eat-double w)])]
            [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (move_right (w_state-snake w)) (w_state-food w))]
            [(equal?  "left" (s_state-dir (w_state-snake w))) (w_state (move_left (w_state-snake w)) (w_state-food w))]
            [(equal?  "up" (s_state-dir (w_state-snake w))) (w_state (move_up (w_state-snake w)) (w_state-food w))]
            [(equal?  "down" (s_state-dir (w_state-snake w))) (w_state (move_down (w_state-snake w)) (w_state-food w))]))

(define (exist? los h)
  (cond
    [(null? (cdr los)) #f]
    [(equal? h (car los)) #t]
    [else (exist? (cdr los) h)]))
  
; quit?: W_state -> Boolean
; return false when the snake have collision with its body
; header: (define (quit? w))
  
(define (quit? w)
  (local (
          (define (colli? s) (exist? (s_state-pos s) (get_head_w w)))
          )
    (cond
      [(colli? (w_state-snake w)) #t]
      [else #f])))

(define STARTPAGE (empty-scene 500 1000 "green"))


(define (gameover w)
  (local ((define LOSEPAGE (underlay/offset BACKGROUND 0 0
                    (overlay/offset (text/font "GAME OVER" 40 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                                    0 50 (text/font
                                          (string-append "Your Score is:  " (number->string (- (length (s_state-pos (w_state-snake w))) 5))) 30 "Green" "Gill Sans" 'swiss 'normal 'bold #f))))
          (define WINPAGE (underlay/offset BACKGROUND 0 0
                    (overlay/offset (text/font "You WIN!!!!!" 40 "black" "Gill Sans" 'swiss 'normal 'bold #f)
                                    0 50 (text/font
                                          (string-append "Your Score is:  " (number->string (- (length (s_state-pos (w_state-snake w))) 5))) 30 "Green" "Gill Sans" 'swiss 'normal 'bold #f)))))
  (if (>= (length (s_state-pos (w_state-snake w))) 105) WINPAGE LOSEPAGE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Difficulty levels
(define (play-easy initial-state)
(big-bang initial-state
[on-key handle-key]
[on-tick tick 0.25]
[to-draw render]
[stop-when quit? gameover]))

(define (play-medium initial-state)
(big-bang initial-state
[on-key handle-key]
[on-tick tick 0.1]
[to-draw render]
[stop-when quit? gameover]))

(define (play-hard initial-state)
(big-bang initial-state
[on-key handle-key]
[on-tick tick 0.05]
[to-draw render]
[stop-when quit? gameover]))


;Start screen
(define (beginning w)
  (underlay/offset (underlay/offset(underlay/offset (underlay/offset (underlay/offset (underlay/offset STARTPAGE
                   0 -200 (rectangle 200 100 "solid" "black")) 0 -200 (text/font "EASY" 36 "Green" "Gill Sans" 'swiss 'normal 'bold #f))
                   0 0 (underlay/offset (rectangle 200 100 "solid" "black") 0 0 (text/font "MEDIUM" 36 "Yellow" "Gill Sans" 'swiss 'normal 'bold #f)))
                   0 200 (underlay/offset (rectangle 200 100 "solid" "black") 0 0 (text/font "HARD" 36 "Red" "Gill Sans" 'swiss 'normal 'bold #f)))
                   0 -450 (text/font "SNAKE GAME" 36 "Dark Slate Gray" "Gill Sans" 'swiss 'normal 'bold #f))
                   0 -350 (text/font "SELECT A DIFFICULTY" 36 "Dark Slate Gray" "Gill Sans" 'swiss 'normal 'bold #f)))

;Mouse handler
(define (handle-mouse ap x-mouse y-mouse mouse-event)
  (cond 
   [(and (and (> x-mouse 150) (< x-mouse 350)) (and (> y-mouse 250) (< y-mouse 350)) (string=? "button-down" mouse-event)) (play-easy i)]
   [(and (and (> x-mouse 150) (< x-mouse 350)) (and (> y-mouse 450) (< y-mouse 550)) (string=? "button-down" mouse-event)) (play-medium i)]
   [(and (and (> x-mouse 150) (< x-mouse 350)) (and (> y-mouse 650) (< y-mouse 750)) (string=? "button-down" mouse-event)) (play-hard i)]))
   

(define (start-game initial-state)
(big-bang initial-state
[to-draw beginning]
[on-mouse handle-mouse]))
;[stop-when quit?]

(start-game i)














