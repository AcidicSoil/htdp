(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)
(define WHEEL-RADIUS 5)


(define D (* WHEEL-RADIUS 2))
(define ca (* (sqr WHEEL-RADIUS) pi))
;WHEEL DEFINITIONS
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define CAR-LENGTH (* D 5))
(define wsv1 (/ CAR-LENGTH 3))
(define SPACE
  (rectangle wsv1 0 "solid" "white"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

;CAR DEFINITIONS
(define CAR-HEIGHT (* .6(* 2.25 D)))
(define CAR-TOP-H (/ CAR-HEIGHT 3))
(define CAR-TOP-L (* CAR-LENGTH .7))
(define CAR-BODY
  (above/align "middle"
               (rectangle CAR-TOP-L CAR-TOP-H "solid" "blue")
               (rectangle CAR-LENGTH CAR-HEIGHT "solid" "blue")))

(define CAR (underlay/align/offset
             "middle" "bottom"
             CAR-BODY
             0 5
             BOTH-WHEELS))

(define WIDTH-OF-WORLD  (*(image-width CAR)3))
(define HEIGHT-OF-WORLD (*(image-height CAR)4))
(define AREA-OF-WORLD (* WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define CAR-CENTER-TO-TOP
  (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))

(define Y-CAR CAR-CENTER-TO-TOP)

(define CAR-CENTER-TO-WIDTH
  (/ (image-width CAR) 2))

(define CAR-R-S
  (- (image-width CAR) CAR-CENTER-TO-WIDTH))

(define tree-top (* (image-height CAR) 1.5))
(define trunk-h (* .9 tree-top))
(define trunk-l (* .3 trunk-h))
(define tree
  (above/align "middle"
               (circle tree-top "solid" "green")
               (rectangle trunk-l trunk-h "solid" "brown")))
(define TREE-CENTER-TO-TOP
  (- HEIGHT-OF-WORLD (/ (image-height tree) 2)))

(define BACKGROUND (place-image tree
                                100 TREE-CENTER-TO-TOP
                                (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))
(check-expect (render 50)
              (place-image CAR 50 Y-CAR BACKGROUND))
(check-expect (render 200)
              (place-image CAR 200 Y-CAR BACKGROUND))

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car
; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image 
;(define (render x)
;  BACKGROUND) 
; WorldState -> WorldState
; adds 3 to x to move the car right 
;(define (tock x)
;  x)
;examples for a rendering function
;cw -> its image
;.
;.
;.
;.
;cw -> an expression
;50 (place-image CAR 50 Y-CAR BACKGROUND)
;100 (place-image CAR 100 Y-CAR BACKGROUND)
;150 (place-image CAR 150 Y-CAR BACKGROUND)
;200 (place-image CAR 200 Y-CAR BACKGROUND)

; WorldState -> WorldState
; launches the program from some initial state
;(define cw  (place-image CAR CAR-R-S Y-CAR BACKGROUND))
; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
;   given: 20, expect 23
;   given: 78, expect 81
(define (tock cw)
  (+ cw 3))
; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state

(define cw  (place-image CAR CAR-R-S Y-CAR BACKGROUND))
(define (render cw)
  (place-image CAR (tock cw) Y-CAR BACKGROUND))
;animates images like render
(define (cls cw)
  (cond
    [ (<=  (tock (image-width CAR)) (+ (image-width BACKGROUND)CAR-CENTER-TO-WIDTH))
      (place-image CAR (tock cw) Y-CAR BACKGROUND)]
    [(>  (tock cw)  cw)
     (place-image CAR (+ (tock cw) CAR-R-S) Y-CAR BACKGROUND)]))



;(define (hyper x-position-of-car x-mouse y-mouse me)
;  x-position-of-car)

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 

; given: 21 10 20 "enter"
; wanted: 21
; given: 42 10 20 "button-down"
; wanted: 10
; given: 42 10 20 "move"
; wanted: 42
;(check-expect (hyper 21 10 20 "enter")
;       (place-image CAR 21 Y-CAR BACKGROUND))

;(check-expect (hyper 42 10 20 "button-down")
;       (place-image CAR 10 Y-CAR BACKGROUND))

;(check-expect (hyper 42 10 20 "move")
;       (place-image CAR 42 Y-CAR BACKGROUND))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) x-mouse]
    [else x-position-of-car]))


(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-mouse hyper]
    [to-draw cls]
    [stop-when stop]))

(define (stop cls)
  (cond
    [(<= (tock cls) (+ (image-width cw) CAR-CENTER-TO-WIDTH)) #false]
    [(>= (tock cls) (+ (image-width cw) CAR-CENTER-TO-WIDTH)) #true]))
