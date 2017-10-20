#lang racket
(require 2htdp/image 2htdp/universe
         (only-in file/gif
                  gif-start
                  gif-end
                  gif-add-image
                  gif-add-loop-control
                  gif-add-control
                  quantize)
         (only-in racket/gui/base put-file))

(define WIDTH 480)
(define HEIGHT 480)

(define filepath (put-file))

(define (color->argblist c)
  (list (color-alpha c) (color-red c) (color-green c) (color-blue c)))

(define (image->quantizedbytes i)
  (define colorlist (image->color-list i))
  (define rgblist (apply append (map color->argblist colorlist)))
  (quantize (list->bytes rgblist)))

(define port (open-output-file filepath))

(define gif (gif-start port WIDTH HEIGHT 0 #f))
(gif-add-loop-control gif 0)


(define (save-frame surface)
  (define image (freeze surface))
  (define-values (bytes colormap transparent) (image->quantizedbytes image))
  (gif-add-control gif 'any #f 10 #f)
  (gif-add-image gif 0 0 WIDTH HEIGHT #f colormap bytes))

(define NUM-OF-LINES 130)
(define DOTSPEED 2)
(struct cloth (time surface))

;;returns the angle from the time and the function
(define (get-polar t f)
  (define oldpos (f (- t 0.1)))
  (define newpos (f (+ t 0.1)))
  (define slope (/ (- (second newpos) (second oldpos))
                   (- (first newpos) (first oldpos))))
  (atan slope))

(define (add-growing-line s x1 y1 x2 y2 pen timefactorunchanged)
  (define timefactor (/ timefactorunchanged 3))
  (define px2
    (+ x1 (* timefactor (- x2 x1))))
  (define py2
    (+ y1 (* timefactor (- y2 y1))))
  (define growx2
    (if
     (< (abs (- x2 x1)) (abs (- px2 x1)))
     x2
     px2))
  (define growy2
    (if
     (< (abs (- y2 y1)) (abs (- py2 y1)))
     y2
     py2))
  (add-line s x1 y1 growx2 growy2 pen))

(define (draw-blue-line s t pos1 pos2 actualt)
  (if
   (> (+ t (* 10 (- actualt 3))) (+ 1 NUM-OF-LINES))
   (overlay/xy
    (circle 2 "solid" "red")
    (- (- (first pos2) 1)) (- (- (second pos2) 1))
    (add-growing-line s (first pos2) (second pos2) (first pos1) (second pos1)
              (pen (color 50 (+ 50 (modulo (inexact->exact (floor (/ t 2))) 106)) (+ 150 (modulo (inexact->exact (floor t)) 106))) 2 "solid" "butt" "round") (- (+ t (* 10 (- actualt 3))) (+ 1 NUM-OF-LINES))))
   (if
    (> (+ t (* 25 (- actualt 1))) {+ 1 NUM-OF-LINES})
    (overlay/xy
     (circle 2 "solid" "red")
     (- (- (first pos1) 1)) (- (- (second pos1) 1))
     s)
    s)))

(define (get-point-pos pos angle t cropx cropy)
  (define x (first pos))
  (define y (second pos))
  (list
   (+ (+ x (* (* (/ WIDTH 3) (sin (/ t DOTSPEED))) (cos angle))) cropx)
   (+ cropy (+ y (* (* (sin (/ t DOTSPEED)) (/ WIDTH 3)) (sin angle))))))

;; takes time, a surface (2htdp image), and a function that takes in time and returns a pair '(x y)
(define (draw-line t surface f drawcircle? actualt)
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
   (+ 1 WIDTH)
   (+ 1 HEIGHT)
   (if drawcircle?
       (draw-blue-line line-added t
                       (get-point-pos pos angle t cropx cropy)
                       (get-point-pos (f (+ t 5)) (get-polar (+ 5 t) f) (+ 5 t) cropx cropy)
                       actualt)
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

(define (draw-entire-helper counter t s f actualt)
  (if (>= counter (+ NUM-OF-LINES 1))
      s
      (draw-entire-helper (+ counter 1)
                          t
                          (if (= (modulo counter 5) 0)
                              (draw-line (+ counter t) s f #t actualt)
                              (draw-line (+ counter t) s f #f actualt))
                          f
                          actualt)))

(define (draw-entire t f)
  (draw-entire-helper 0 t (rectangle WIDTH HEIGHT "solid" "black") f t))

(define speed-up 1)

(define current-equation heart-formula)

(define (tick c)
  (define new-time (+ (cloth-time c) {* 0.06 speed-up}))
  (begin (save-frame (cloth-surface c))
         (if (> NUM-OF-LINES (cloth-time c))
             (cond
               [(> (floor new-time) (floor (cloth-time c)))
                (set! speed-up (+ speed-up 1))
                (cloth
                 new-time
                 (draw-line new-time (cloth-surface c) current-equation #f 0))]
               [else
                (cloth new-time (cloth-surface c))])
             (cloth
              (+ (cloth-time c) 0.06)
              (draw-entire (+ 0.06 (- (cloth-time c) NUM-OF-LINES)) current-equation)))))

(define (draw c)
  (cloth-surface c))

(big-bang (cloth 0 (rectangle WIDTH HEIGHT "solid" "black"))
          (on-tick tick (/ 1 10))
          (to-draw draw))


(gif-end gif)
(close-output-port port)