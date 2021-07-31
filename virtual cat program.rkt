(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

(define cat1 . )

(define cat2 .)

(define WIDTH  300)
(define HEIGHT 100)
(define RESTART-POINT (+ WIDTH (image-width cat1)))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define cat1-CENTER-TO-TOP
  (- HEIGHT (/ (image-height cat1) 2)))

(define Y-cat1 cat1-CENTER-TO-TOP)

(define cat1-CENTER-TO-WIDTH
  (/ (image-width cat1) 2))

(define cat1-R-S
  (- (image-width cat1) cat1-CENTER-TO-WIDTH))

(define (main x0)
  (big-bang x0
    [on-tick tock]
    [to-draw render]))

(define (tock x)
  (+ x 3))

(define (render x)
  (cond
    [ (<  x  RESTART-POINT)
    (cond
      [(odd?  x)  (place-image cat2 (tock x) Y-cat1 BACKGROUND)]
      [else (place-image cat1 (tock x) Y-cat1 BACKGROUND)])]
    [(>=  x RESTART-POINT)
     (cond
       [(odd? x) (place-image cat2 (modulo x RESTART-POINT) Y-cat1 BACKGROUND)]
       [else (place-image cat1 (modulo x RESTART-POINT) Y-cat1 BACKGROUND)])]))


(define (add-sub-bar x key)
  ; x number
  ; key whatever (ignore)
  (cond
    [(key=? key "up")
     (+ x (* x (/ 1 3)))]
    [(key=? key "down")
     (- x (* x (/ 1 5)))]))

(define (gauge-decrease x)
  (- x 0.1))

; gauge-prog: Number -> Image
; returns a image based on number
; gauge-prog-template: Number -> Image
(define (progress-bar progress-bar-percentage)
  ; bar-width number
  ; bar-height number
  ; progress-bar-percentage number
  ; bar-color string
  (overlay/align "left" "middle"
                 (rectangle  progress-bar-percentage 20 "solid" "blue")
                 (rectangle 100 20 "outline" "blue")))

(define (happy-main x)
  (big-bang x
    (on-draw  progress-bar)
    (on-tick gauge-decrease)
    (on-key add-sub-bar)))

;(check-expect (gauge-prog 5) (place-image 
