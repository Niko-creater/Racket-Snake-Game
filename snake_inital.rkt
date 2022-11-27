#lang racket/base
(require lang/posn)
(require test-engine/racket-tests)
(require racket/local)
(require 2htdp/image)
(require 2htdp/universe) 


(struct s_state (pos dir sp))
(struct f_state (pos))
(struct w_state (snake food))

; Const
(define gap 20)

(define pos_list (list (make-posn 100 100) (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100)))
(define fod_list (list (make-posn 200 200) (make-posn 300 300)))
(define sk_1 (s_state pos_list "right" gap))
(define fd_1 (f_state fod_list))
(define i (w_state sk_1 fd_1))
(define speed 0.2)

; Helper function
(define (get_index los a)
  (cond
    [(equal? a (car los)) 0]
    [else (+ 1 (get_index (cdr los) a))]));

(check-expect (get_index (list (make-posn 1 1) (make-posn 2 1) (make-posn 3 1)) (make-posn 3 1)) 2)

(define (delete_by_value los val)
  (cond
    [(null? los) los]
    [(equal? val (car los)) (cdr los)]
    [else (cons (car los) (delete_by_value (cdr los) val))]))

(check-expect (delete_by_value pos_list (make-posn 120 100)) (list (make-posn 100 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100)))
(check-expect (delete_by_value pos_list (make-posn 180 100)) (list (make-posn 100 100) (make-posn 120 100) (make-posn 140 100) (make-posn 160 100)))
(check-expect (delete_by_value pos_list (make-posn 100 100)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100)))




;; Move

;; right

(define (move_right s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (+ (s_state-sp s) (posn-x x)) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
  (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s))))

(check-expect (s_state-pos (move_right sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 200 100)))

;; left

(define (move_left s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (+ (- (s_state-sp s)) (posn-x x)) (posn-y x))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
  (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s))))

(check-expect (s_state-pos (move_left sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 160 100)))

;; up

(define (move_up s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (+ (- (s_state-sp s)) (posn-y x)))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
  (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s))))

(check-expect (s_state-pos (move_up sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 180 80)))

;; down

(define (move_down s)
  (local ((define (mr p) (map (lambda (x) (cond
                                  [(= (get_index p x) (- (length p) 1)) (make-posn (posn-x x) (+ (s_state-sp s) (posn-y x)))]
                                  [else (list-ref p (+ 1 (get_index p x)))])) p)))
  (s_state (mr (s_state-pos s)) (s_state-dir s) (s_state-sp s))))

(check-expect (s_state-pos (move_down sk_1)) (list (make-posn 120 100) (make-posn 140 100) (make-posn 160 100) (make-posn 180 100) (make-posn 180 120)))

;; Render

(define s_head (rectangle 20 20 "solid" "blue"))
(define s_part (rectangle 20 20 "solid" "black"))
(define food_element (rectangle 20 20 "solid" "red"))
(define BACKGROUND (empty-scene 1500 1000))

(define (draw_list los)
  (cond
    [(null? (cdr los)) (underlay/offset BACKGROUND (posn-x (car los)) (posn-y (car los))  s_head)]
    [else (underlay/offset (draw_list (cdr los)) (posn-x (car los)) (posn-y (car los))  s_part)]))

(define (draw_food cav los)
  (cond
    [(null? los) cav]
    [(null? (cdr los)) (underlay/offset cav (posn-x (car los)) (posn-y (car los))  food_element)]
    [else (underlay/offset (draw_food cav (cdr los)) (posn-x (car los)) (posn-y (car los))  food_element)]))

(define (render w)
  (draw_food (draw_list (s_state-pos (w_state-snake w))) (f_state-pos (w_state-food w))))
 
;; Key

(define (handle-key w key-event)
  (cond
    [(string=? "w" key-event) (if (equal? "down" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "up" (s_state-sp (w_state-snake w))) (w_state-food w)))]
    [(string=? "a" key-event) (if (equal? "right" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "left" (s_state-sp (w_state-snake w))) (w_state-food w)))]
    [(string=? "s" key-event) (if (equal? "up" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "down" (s_state-sp (w_state-snake w))) (w_state-food w)))]
    [(string=? "d" key-event) (if (equal? "left" (s_state-dir (w_state-snake w)))
                                  w
                                  (w_state (s_state (s_state-pos (w_state-snake w)) "right" (s_state-sp (w_state-snake w))) (w_state-food w)))]
    [else w]))

;; Collision

(define (detect head los)
  (map (lambda (x) (if (equal? head x) #t x)) los))

(check-expect (detect (make-posn 180 100) fod_list) (list (make-posn 200 200) (make-posn 300 300)))
(check-expect (detect (make-posn 200 200) fod_list) (list #t (make-posn 300 300)))
(check-expect (detect (make-posn 300 300) fod_list) (list (make-posn 200 200) #t))

(define (det? los)
  (cond
    [(null? los) #f]
    [(equal? #t (car los)) #t]
    [else (det? (cdr los))]))

(check-expect (det? (detect (make-posn 180 100) fod_list)) #f)
(check-expect (det? (detect (make-posn 200 200) fod_list)) #t)
(check-expect (det? (detect (make-posn 300 300) fod_list)) #t)


(define (eat? w)
  (det? (detect (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)) (f_state-pos (w_state-food w)))))

(check-expect (eat? i) #f)

(define (eat w)
  (local ((define last_element (list-ref (s_state-pos (w_state-snake w)) (- (length (s_state-pos (w_state-snake w))) 1)))
          (define right_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ gap (posn-x last_element)) (posn-y last_element)))))
          (define left_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (+ (- gap) (posn-x last_element)) (posn-y last_element)))))
          (define up_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ (- gap)(posn-y last_element))))))
          (define down_to_head (append (s_state-pos (w_state-snake w)) (list (make-posn (posn-x last_element) (+ gap (posn-y last_element))))))
          )
  (cond
    [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (s_state right_to_head "right" gap) (f_state (delete_by_value (f_state-pos (w_state-food w)) last_element)))]
    [(equal? "left" (s_state-dir (w_state-snake w))) (w_state (s_state left_to_head "left" gap) (f_state (delete_by_value (f_state-pos (w_state-food w)) last_element)))]
    [(equal? "up" (s_state-dir (w_state-snake w))) (w_state (s_state up_to_head "up" gap) (f_state (delete_by_value (f_state-pos (w_state-food w)) last_element)))]
    [(equal? "down" (s_state-dir (w_state-snake w))) (w_state (s_state down_to_head "down" gap) (f_state (delete_by_value (f_state-pos (w_state-food w)) last_element)))])))
(test)

;; Tick

(define (tick w)
 (cond
   [(eat? w) (eat w)]
   [(equal? "right" (s_state-dir (w_state-snake w))) (w_state (move_right (w_state-snake w)) (w_state-food w))]
   [(equal?  "left" (s_state-dir (w_state-snake w))) (w_state (move_left (w_state-snake w)) (w_state-food w))]
   [(equal?  "up" (s_state-dir (w_state-snake w))) (w_state (move_up (w_state-snake w)) (w_state-food w))]
   [(equal?  "down" (s_state-dir (w_state-snake w))) (w_state (move_down (w_state-snake w)) (w_state-food w))]))

(define b (list-ref (s_state-pos (w_state-snake i)) (- (length (s_state-pos (w_state-snake i))) 1)))
(define a (append (s_state-pos (w_state-snake i)) (list (make-posn (+ gap (posn-x b)) (posn-y b))))) 

(define (play initial-state)
(big-bang initial-state
[on-key handle-key]
[on-tick tick 1]
[to-draw render]
))

