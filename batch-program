;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname batchprogram) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)
;(write-file 'stdout "212\n")
;(read-file "sample.dat")
(define (C f)
  (* 5/9 (- f 32)))

(define (convert in out)
  (write-file out
              (string-append
               (number->string
                (C
                 (string->number
                  (read-file in))))
               "\n")))

(define (letter fst lst signature-name)
  (string-append
   (opening fst)
   "\n\n"
   (body fst lst)
   "\n\n"
   (closing signature-name)))
 
(define (opening fst)
  (string-append "Dear " fst ","))
 
(define (body fst lst)
  (string-append
   "We have discovered that all people with the" "\n"
   "last name " lst " have won our lottery. So, " "\n"
   fst ", " "hurry and pick up your prize."))
 
(define (closing signature-name)
  (string-append
   "Sincerely,"
   "\n\n"
   signature-name
   "\n"))

;(define (main in-fst in-lst in-signature out.dat)
 ; (write-file out.dat
  ;            (letter (read-file in-fst)
   ;                   (read-file in-lst)
    ;                  (read-file in-signature))))

;(write-file "sample.dat" "212")
;(read-file "sample.dat")
;(write-file 'stdout "212\n")

(define (number->square s)
  (square s "solid" "red"))

;(big-bang 100 [to-draw number->square])

;(big-bang 100
;    [to-draw number->square]
;    [on-tick sub1]
;    [stop-when zero?])

(define (reset s ke)
  100)

(define BACKGROUND (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))
 
(define (main y)
  (big-bang y
    [on-tick sub1]
    [stop-when zero?]
    [to-draw place-dot-at]
    [on-key stop]))
 
(define (place-dot-at y)
  (place-image DOT 50 y BACKGROUND))
 
(define (stop y ke)
  0)

;Exercise 34 Design the function string-first
; We use letters to represent strings
; String -> String
; Takes a string as str and outputs the first char in str
; given: "pen", expect: "p"
; given: "cat", expect: "c"
(define (string-first str) (substring str 0 1))

;Exercise 35 Design the function string-last
; String -> String
; Takes a string as str and outputs the last char in str
; given: "pen", expect: "n"
; given: "cat", expect: "t"
(define (string-last str) (substring str (- (string-length str) 1)(string-length str)))

;Exercise 36 Design the function image-area
; Image -> Number
; Takes an image as input and outputs the area of image
; given: (square 10 "solid" "red"), expect: 100
; given: (rectangle 5 5 "solid" "blue"), expect: 25
(define (image-area img) (* (image-height img) (image-width img)))

;Exercise 37 Design the function string-rest
; String -> String
; Takes a string and removes the first char and returns string
; given: "pen", expect: "en"
; given: "cat", expect: "at"
(define (string-rest str) (substring str (/ (string-length str) (string-length str))))

;Exercise 38 Design the function string-remove-last
; String -> String
; Takes a string and removes the last char and prints value
; given: "pen", expect: "pe"
(define (string-remove-last str) (substring str 0 (- (string-length str) 1)))

; Number -> Number 
; converts Fahrenheit temperatures to Celsius
; given 32, expect 0
; given 212, expect 100
; given -40, expect -40
(define (f2c f)
  (* 5/9 (- f 32)))

(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)
