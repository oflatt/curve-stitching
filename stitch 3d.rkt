#lang racket
(require 2htdp/image 2htdp/universe)


(define WIDTH 500)
(define HEIGHT 500)
(struct cloth (time surface))

(define (get-slope t f)
  (define oldpos (f (- t 0.01)))
  (define newpos (f (+ t 0.01)))
  (/ (- (second newpos) (second oldpos))
     (- (first newpos) (first oldpos))))

;; takes time, a surface (2htdp image), and a function that takes in time and returns a pair '(x y)
(define (draw-line t s f)
  (define pos (f t))
  (define slope (get-slope t f))
  (define x (first pos))
  (define y (second pos))
    ;;checks if it intersects the top or bottom of the screen
  (define left-hits-top-or-bottom? (or (< (- y (* x slope)) (- (/ HEIGHT 2))) (> (- y (* x slope)) (/ HEIGHT 2))))
  (define right-hits-top-or-bottom? (or (> (+ y (* x slope)) (/ HEIGHT 2)) (< (+ y (* x slope)) (/ HEIGHT 2))))
  (define x1
    (if
     left-hits-top-or-bottom?
     (- x (* (/ 1 slope) y))
     0))
  (define y1
    (if
     left-hits-top-or-bottom?
     0
     (- y (* slope x))))
  (define x2
    (if
     left-hits-top-or-bottom?
     (+ x (* (/ 1 slope) (- HEIGHT y)))
     WIDTH))
  (define y2
    (if
     left-hits-top-or-bottom?
     HEIGHT
     (+ y (* slope (- WIDTH x)))))
  (overlay/xy
   (circle 2 "outline" "red")
   (- (+ x (/ WIDTH 2)))
   (- (+ y (/ HEIGHT 2)))
   (add-line
    s
    (+ x1 (/ WIDTH 2))
    (+ y1 (/ WIDTH 2))
    (+ x2 (/ WIDTH 2))
    (+ y2 ( / WIDTH 2))
    (pen "black" 1 "solid" "butt" "round"))))

(define (circle-formula t)
  (list
   (* (cos (/ t (* 10 pi))) (/ WIDTH 3))
   (* (sin (/ t (* 10 pi)))(/ WIDTH 3))))

(define (tick c)
  (cloth
   (+ (cloth-time c) 1)
   (draw-line (+ 1 (cloth-time c)) (cloth-surface c) circle-formula)))

(define (draw c)
  (cloth-surface c))

(big-bang (cloth 20 (rectangle WIDTH HEIGHT "outline" "black"))
          (on-tick tick 0.1)
          (to-draw draw))