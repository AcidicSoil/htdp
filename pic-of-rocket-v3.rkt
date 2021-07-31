 (require 2htdp/image)
 (require 2htdp/universe)


(image-width . )

(image-height . )


(place-image (circle 5 "solid" "green")
             10 10
             (empty-scene 100 60))



; properties of the "world" and the descending rocket
(define WIDTH  200)
(define HEIGHT  400)
(define V 3)
(define X 50)
(define (distance t)
  (* V t))
; graphical constants 
(define MTSCN (place-image (rectangle 200 10 "solid" "orange")
              100 395
  (empty-scene WIDTH HEIGHT "blue")))
(define ROCKET . )
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

; functions
(define (picture-of-rocket t)
  (cond
 [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X (distance t) MTSCN)]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET X ROCKET-CENTER-TO-TOP MTSCN)]))
 

(define UFO
  (overlay (circle 10 "solid" "green")
         (rectangle 40 4 "solid" "green")))


(animate picture-of-rocket) 

(define UFO-CENTER-TO-TOP
  (- HEIGHT (/ (image-height UFO) 2)))

(define (picture-of-ufo t)
  (cond
 [(<= (distance t) UFO-CENTER-TO-TOP)
     (place-image UFO X (distance t) MTSCN)]
    [(> (distance t) UFO-CENTER-TO-TOP)
     (place-image UFO X UFO-CENTER-TO-TOP MTSCN)]))
 

(animate picture-of-ufo)
