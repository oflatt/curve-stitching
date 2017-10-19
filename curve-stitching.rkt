#lang racket
(require 2htdp/image 2htdp/universe)

(define WIDTH 500)
(define HEIGHT 500)
(define NUM-OF-LINES 130)
(define DOTSPEED 100)
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
              (pen (color (+ 150 (modulo (inexact->exact (floor t)) 106)) (+ 50 (modulo (inexact->exact (floor (/ t 3))) 106)) 50) 1 "solid" "butt" "round")))
  (crop
   cropx
   cropy
   WIDTH
   HEIGHT
   (if drawcircle?
       (overlay/xy
        (circle 2 "solid" "red")
        (- (- (+ x (* (* (/ WIDTH 3) (sin (/ t DOTSPEED))) (cos angle)))) cropx)
        (- (- (+ y (* (* (sin (/ t DOTSPEED)) (/ WIDTH 3)) (sin angle)))) cropy)
        line-added)
       line-added)))

(define (circle-formula t)
  (list
   (+ (/ WIDTH 2) (* (cos (* (/ t 50) pi)) (/ WIDTH 3)))
   (+ (/ WIDTH 2) (* (sin (* (/ t 50) pi)) (/ WIDTH 3)))))

(define (heart-formula time)
  (define t (/ time 20))
  (list
   (+ (/ WIDTH 2) (* (* 16 (expt (sin t) 3)) (/ WIDTH 50)))
   (+ (/ WIDTH 2) (* (- (- (- (* 13 (cos t)) (* 5 (cos (* 2 t)))) (* 2 (cos (* 3 t))))) (/ WIDTH 50)))))

(define (rose-formula time)
  (define petal-factor 7)
  (define t (* (/ time 50) pi))
  (list
   (+ (/ WIDTH 2) (* (* (cos (* petal-factor t)) (cos t)) (/ WIDTH 3)))
   (+ (/ WIDTH 2) (* (* (cos (* petal-factor t)) (sin t)) (/ WIDTH 3)))))

(define (draw-entire-helper counter t s f)
  (if (>= counter NUM-OF-LINES)
      s
      (draw-entire-helper (+ counter 1)
                          t
                          (if (= (modulo counter 5) 0)
                              (draw-line (+ counter t) s f #t)
                              (draw-line (+ counter t) s f #f))
                          f)))

(define (draw-entire t f)
  (draw-entire-helper 0 t (rectangle WIDTH HEIGHT "outline" "black") f))

(define speed-up 1)

(define (tick c)
  (define new-time (+ (cloth-time c) {* 0.01 speed-up}))
  (if (> NUM-OF-LINES (cloth-time c))
      (cond
       [(> (floor new-time) (floor (cloth-time c)))
        (set! speed-up (+ speed-up 1))
        (cloth
         new-time
         (draw-line new-time (cloth-surface c) heart-formula #f))]
       [else
        (cloth new-time (cloth-surface c))])
      (cloth
       (+ (cloth-time c) 0.01)
       (draw-entire (+ 0.01 (- (cloth-time c) NUM-OF-LINES)) heart-formula))))

(define (draw c)
  (cloth-surface c))

(big-bang (cloth 0 (rectangle WIDTH HEIGHT "outline" "black"))
          (on-tick tick (/ 1 60))
          (to-draw draw))