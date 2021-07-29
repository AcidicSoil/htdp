(require picturing-programs)
(require 2htdp/image)
(require 2htdp/batch-io)
(provide distance-to-origin)

;(define x 2)
;(define y 5)

;(define dto (sqrt(+ (sqr x)(sqr y))))

#|(define prefix "hello")
(define suffix "world")

(define hw (string-append prefix "_" suffix))


;(define str "helloworld")
(define ind "0123456789")
;(define i 5)

;(define ns (string-append (substring str 0 i) "_" (substring str i 10)))

;trying to create a formula for series and sequences below
;(define rs (+ (expt i 2) 1))
;(define d (/ i (expt (string-length str) i)))


;exercise 5 create a tree image
(define tree (above (circle 50 "solid" "green")
                    (rectangle 25 100 "solid" "brown")))
                                          

;used for cat line 21-31 checks to see if cat is tall
(define cat .)

(define cw (image-width cat))
(define ch (image-height cat))
  
(define wide #false)
(define tall #true)

(define wide-cat (if (< ch cw) "wide" (>= ch ch)))
(define tall-cat (if (>= ch cw) "tall" (< ch cw)))

;used for line 33-45 checks to see if pic is square
(define pic (rectangle 20 10 "solid" "red"))

(define pw (image-width pic))
(define ph (image-height pic))

(define wide-picture #false)
(define tall-picture #false)
(define square-picture #true)

(define tall-pic (if (< ph pw) "wide" (>= ph pw)))
(define wide-pic (if (>= ph pw) "tall" (< ph pw)))
(define square-pic (if (= ph pw) "square" (and (not wide-picture) tall-picture)))

(define sunny #true)
(define friday #false)

(define mall-day (or (not (not friday)) (not sunny)))

;(+ (image-width (circle 10 "solid" "red"))
   (image-height (rectangle 10 20 "solid" "blue")))

(define x 2)
(define inverse-of-x (/ 1 x))
;(if (= x 0) 0 (/ 1 x))

;(overlay/align "right" "bottom"
               (circle 10 "solid" "red")
               (rectangle 10 20 "solid" "blue"))

;(define (ff x)(* 2 x))

;(+ (ff 2)(+ (ff 2) 3))
;(ff (ff 2))

;(define dto (sqrt(+ (sqr x)(sqr y))))

;(define (dto x y)(sqrt(+ (sqr x)(sqr y))))


(define (cvolume l) (expt l 3))

(define (csurface a) (* (expt a 2) 6))

;(define (string-first s) (substring s 0 1))

(define (string-last s) (substring s (- (string-length s) 1)(string-length s)))

;(define (==> sunny friday) (or (not (not sunny)) (not friday)))

(define (string-join s s2) (string-append s (string-append "_" s2)))

;(define (string-insert str i) (string-append (substring str 0 i) "_" (substring str i (string-length str))))
;(define (string-insert-v1 str i) (if (<= (string-length str) 0)  "enter string" (string-insert str i)))

;(define (string-insert s i)(string-append (substring s 0 i)"_"(substring s i)))
;(string-insert "helloworld" 6)
(define (string-delete str i) (string-append (substring str 0 i) "" (substring str i (- (string-length str) 1))))

;(define wide-pic (if (>= ph pw) "tall" (< ph pw)))
(define (string-delete-v1 str i) (if (<= (string-length str) 0)  "enter string" (string-delete str i)))

(define (ff a) (* 10 a))
;(ff (* 1 1))
;(ff (ff 1))
;(+ (ff 1) (ff 1))

;(define (opening first-name last-name)
;  (string-append "Dear " first-name ","))|#

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))
;(distance-to-origin 3 4)

#|(define (string-first s)
  (substring s 0 1))
;(string-first "hello world")

(define (==> x y)
  (or (not x) y))
;(==> #true #false)

(define (image-classify img)
  (cond
    [(>= (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img)) "square"]
    [(<= (image-height img) (image-width img)) "wide"]))
;(image-classify cat)

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
   "\n"))|#


;(define (attendees ticket-price)
;  (- 120 (* (- ticket-price 5.0) (/ 15 0.1))))
;(define (revenue ticket-price)
;  (* ticket-price (attendees ticket-price)))
;(define (cost ticket-price)
;  (+ 180 (* 0.04 (attendees ticket-price))))
;(define (profit ticket-price)
;  (- (revenue ticket-price)
;     (cost ticket-price)))

#|(define AUDIENCE 120)
(define AVG-ATTEND 15)
(define COST-PER-ATTENDEE-PER-SHOW 0.04)
(define TICKET_PRICE-CHANGE 0.1)
(define BASE-TICKET 5.0)
(define PRICE-SENZS-TO-ATTENDANCE (/ AVG-ATTEND TICKET_PRICE-CHANGE))

(define (attendees ticket-price)
  (- AUDIENCE (* (- ticket-price BASE-TICKET) PRICE-SENS-TO-ATTENDANCE)))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (* COST-PER-ATTENDEE-PER-SHOW (attendees ticket-price)))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
(define (profit1 price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (* 0.04
        (+ 120
           (* (/ 15 0.1)
              (- 5.0 price))))))|#

;imperative slogan: For every constant mentioned in a problem statement, introduce one constant definition

