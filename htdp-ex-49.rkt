#lang racket
(require 2htdp/image)
(require 2htdp/universe)


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



(define (rocket-helper t)
  (cond
    [(<= (distance t)  ROCKET-CENTER-TO-TOP) (distance t)]
    [else ROCKET-CENTER-TO-TOP]))

#|(define (rocket-posn? t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP) true]
    [else false]))|#

(define (rocket-posn? t)
  (or (<= (distance t) ROCKET-CENTER-TO-TOP)
      (> (distance t))))

; functions
(define (picture-of-rocket t)
  (place-image ROCKET X  
               (cond
                 [(<= (distance t) ROCKET-CENTER-TO-TOP) (rocket-helper t)]
                 [else ROCKET-CENTER-TO-TOP]) MTSCN))
        

     
;(place-image ROCKET X (distance t) MTSCN)]
(animate picture-of-rocket ) 
