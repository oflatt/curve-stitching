#lang racket
(require 2htdp/image 2htdp/universe)

(define WIDTH 500)
(define HEIGHT 500)
(struct cloth (time surface))

;;returns the angle from the time and the function
(define (get-polar t f)
  (define oldpos (f (- t 0.1)))
  (define newpos (f (+ t 0.1)))
  (define slope (/ (- (second newpos) (second oldpos))
                   (- (first newpos) (first oldpos))))
  (atan slope))
      
;; takes time, a surface (2htdp image), and a function that takes in time and returns a pair '(x y)
(define (draw-line t surface f drawcircle?)
  (define pos (f t))
  (define angle (get-polar t f))
  (define x (first pos))
  (define y (second pos))
  (define x1 (+ x (* (* WIDTH 2) (cos angle))))
  (define y1 (+ y (* (* WIDTH 2) (sin angle))))
  (define x2 (- x (* (* WIDTH 2) (cos angle))))
  (define y2 (- y (* (* WIDTH 2) (sin angle))))
  (define cropx
    (cond
      [(< x1 0)
       (- x1)]
      [(< x2 0)
       (- x2)]
      [else
       0]))
  (define cropy
    (cond
      [(< y1 0)
       (- y1)]
      [(< y2 0)
       (- y2)]
      [else
       0]))
  (define line-added
    (add-line surface x1 y1 x2 y2
              (pen (color (+ 150 (modulo (+ 100 (inexact->exact (floor t))) 106)) 100 100) 1 "solid" "butt" "round")))
  (crop
   cropx
   cropy
   WIDTH
   HEIGHT
   (if drawcircle?
       (overlay/xy
        (circle 2 "outline" "red")
        (- (- x) cropx)
        (- (- y) cropy)
        line-added)
       line-added)))

(define (circle-formula t)
  (list
   (+ (/ WIDTH 2) (* (cos (* (/ t 50) pi)) (/ WIDTH 3)))
   (+ (/ WIDTH 2) (* (sin (* (/ t 50) pi)) (/ WIDTH 3)))))

(define (draw-entire-helper counter t s f)
  (if (>= counter 100)
      s
      (draw-entire-helper (+ counter 2)
                          t
                          (draw-line (+ counter t) s f #f)
                          f)))

(define (draw-entire t f)
  (draw-entire-helper 0 t (rectangle WIDTH HEIGHT "outline" "black") f))

(define (tick c)
  (if (> 100 (cloth-time c))
      (cloth
       (+ (cloth-time c) 2)
       (draw-line (+ 2 (cloth-time c)) (cloth-surface c) circle-formula #f))
      (cloth
       (+ (cloth-time c) 0.1)
       (draw-entire (+ 0.1 (cloth-time c)) circle-formula))))

(define (draw c)
  (cloth-surface c))

(big-bang (cloth 0 (rectangle WIDTH HEIGHT "outline" "black"))
          (on-tick tick (/ 1 60))
          (to-draw draw))