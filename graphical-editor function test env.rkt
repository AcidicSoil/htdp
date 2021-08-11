(require 2htdp/image)
(require 2htdp/universe)
(require picturing-programs)



(define (string-join s s2) (string-append s (string-append "_" s2)))

(define (string-remove-last str) (substring str 0 (- (string-length str) 1)))

; string-last : string -> String
; string-last takes in a string and outputs the string with the last string removed
(define (string-last s)
  ; string-last takes in a string and outputs the string with the last string removed
  (cond
    [(> (string-length s) 0) (substring s (- (string-length  s) 1)(string-length s)) ]
    [else s]))

;(define (string-insert str i) (string-append (substring str 0 i) "_" (substring str i (string-length str))))
(define (string-insert-v1 str i) (if (<= (string-length str) 0)  "enter string" (string-insert str i)))
(define (string-insert s i)(string-append (substring s 0 i)"_"(substring s i)))

(define-struct editor [pre post])

#|(check-expect (delete-last (make-editor "hello" 5)) "hell")
(check-expect (delete-last (make-editor "dog" 3)) "do")
(check-expect (delete-last (make-editor "" 0)) "")
(check-expect (delete-last (make-editor "dog " 4)) "dog")
(check-expect (delete-last (make-editor "hello" 3)) "helo")
(define (delete-last ed)
  (cond
    [(string=? (editor-pre ed) "") (make-editor "" 0)]   ;"hello" -> "helo"      2 -> "he"                  "hello"          3     "lo"                      5    "he" + "lo" -> "helo"
    [else (string-append (substring  (editor-pre ed) 0 (- (editor-post ed) 1)) (substring (editor-pre ed) (editor-post ed) (string-length  (editor-pre ed)))) ]))|#

; valid-key? : key-event -> boolean
; valid-key? takes in a key-event and
; determines if the key a string-length of 1 if so returns true
; if the ke is "\b" (back-space), "left",
; "right" or any other 1string. If the ke is equal to
; "\r" (return) or "\t" (tab) then it returns false.
(check-expect (valid-key? "\b") true)
(check-expect (valid-key? "b") true)
(check-expect (valid-key? "\t") false)
(check-expect (valid-key? "t") true)
(check-expect (valid-key? "\r") false)
(check-expect (valid-key? "r") true)
(check-expect (valid-key? "0") true)
(check-expect (valid-key? "left") true)
(check-expect (valid-key? "right") true)
(define (valid-key? ke)
  (cond
    [(and(= (string-length ke) 1)
         (key=? ke "\b")) true]
    [(and(> (string-length ke) 1)
         (key=? ke "left" )) true]
    [(and(> (string-length ke) 1)
         (key=? ke "right" )) true]
    [(and(= (string-length ke) 1)
         (key=? ke "\t")) false]
    [(and(= (string-length ke) 1)
         (key=? ke "\r")) false]
    [else true]))

; edit : editor key-event -> Editor
; edit takes in an editor structure (make-editor pre post)
; (editor-pre String) and (editor-post Number)
; The second argument to edit should be a key-event
; Produces a new editor structure appending valid key-events to the end of editor-pre 
; If ke equals "left" or "right" it should move the cursor to that direction if
; editor-post is > 0. If < 0 then the cursor should do nothing.
; If the "\b" (back-space) ke is triggered then it should delete the string to
; at the i'th place indicated by the editor-post
; if editor-post is 0 then it will do nothing.
(check-expect (edit (make-editor "h" 1) "\b") (make-editor "" 0))
(check-expect (edit (make-editor "" 0) "h") (make-editor "h" 1))
(check-expect (edit (make-editor "he" 2) "\b") (make-editor "h" 1))
(check-expect (edit (make-editor "hel" 3) "\b") (make-editor "he" 2))
(check-expect (edit (make-editor "" 0) "\b") (make-editor "" 0))
(check-expect (edit (make-editor "hello " 6) "\b") (make-editor "hello" 5))
(check-expect (edit (make-editor " hello " 7) "\b") (make-editor " hello" 6))
(check-expect (edit (make-editor "" 0) "left") (make-editor "" 0))
(check-expect (edit (make-editor "h" 1) "left") (make-editor "h" 0))
(check-expect (edit (make-editor "h" 1) "right") (make-editor "h" 1))
(check-expect (edit (make-editor "he" 2) "left") (make-editor "he" 1))
(check-expect (edit (make-editor "he" 2) "right") (make-editor "he" 2))
(check-expect (edit (make-editor "hel" 3) "left") (make-editor "hel" 2))
(check-expect (edit (make-editor "hel" 3) "right") (make-editor "hel" 3))
(check-expect (edit (make-editor "hel" 2) "right") (make-editor "hel" 3))
(check-expect (edit (make-editor "hel" 1) "right") (make-editor "hel" 2))
(check-expect (edit (make-editor "hello" 5) "\t") (make-editor "hello" 5))
(check-expect (edit (make-editor "hello" 5) "\r") (make-editor "hello" 5)) 
(define (edit ed ke)
  (cond
    [(key=? ke "\b" ) (delete-last ed)]
    [(valid-key? "left") (left-cursor ed)]
    [(valid-key? "right") (right-cursor ed)]
    [else (right-cursor (make-editor (string-append (editor-pre ed) ke)
                       (cond
                         [(key=? ke "right") (editor-post (right-cursor ed))]
                         [(key=? ke "left") (editor-post (left-cursor ed))]
                         [(key=? ke "\b") (editor-post (delete-last ed))]
                         [else (editor-post ed)])))])) 

; right-cursor : editor -> Editor
; This should take in an editor structure and add 1 to the
; editor-post if it is < (string-length (editor-pre ed)) 
; add 1 for every ke that equals "right". If the editor-post value
; is equal to the (string-length (editor-pre ed)) then it will do nothing. 
(check-expect (right-cursor (make-editor "hello" 5)) (make-editor "hello" 5))
(check-expect (right-cursor (make-editor "hello" 0)) (make-editor "hello" 1))
(check-expect (right-cursor (make-editor "" 0)) (make-editor "" 0))
(check-expect (right-cursor (make-editor "hello" 1)) (make-editor "hello" 2))
(check-expect (right-cursor (make-editor "hello" 2)) (make-editor "hello" 3))
(check-expect (right-cursor (make-editor "hell o" 4)) (make-editor "hell o" 5))
(check-expect (right-cursor (make-editor "hello " 5)) (make-editor "hello " 6))
(define (right-cursor ed)
  (cond
    [(= (editor-post ed) (string-length (editor-pre ed)))
     (make-editor (editor-pre ed) (editor-post ed))]
    [(< (editor-post ed) (string-length (editor-pre ed)))
     (make-editor (editor-pre ed) (+ (editor-post ed) 1))]
    [else (make-editor (editor-pre ed) (editor-post ed))]))

; left-cursor : editor -> Editor
; This should take in an editor structure and subtract 1 from the
; editor-post if it is > 0 
; subtract 1 for every ke that equals "left". If the editor-post value
; is 0 then it will do nothing.
(check-expect (left-cursor (make-editor "hello" 5)) (make-editor "hello" 4))
(check-expect (left-cursor (make-editor "hello" 0)) (make-editor "hello" 0))
(check-expect (left-cursor (make-editor "" 0)) (make-editor "" 0))
(define (left-cursor ed)
  (cond
    [(= (editor-post ed) 0)
     (make-editor (editor-pre ed) 0)]
    [(> (editor-post ed) 0)
     (make-editor (editor-pre ed) (- (editor-post ed) 1))])) 

; delete-last : editor -> Editor
; This should take in an editor structure
; and remove a char from that string in the editor-pre at the
; (index minus 1) editor-post minus 1.
(check-expect (delete-last (make-editor "hello" 5)) (make-editor "hell" 4))
(check-expect (delete-last (make-editor "dog" 3)) (make-editor "do" 2))
(check-expect (delete-last (make-editor "" 0)) (make-editor "" 0))
(check-expect (delete-last (make-editor "dog " 4)) (make-editor "dog" 3))
(check-expect (delete-last (make-editor "hello" 3)) (make-editor "helo" 2))
(define (delete-last ed)  
  (cond
    [(string=? (editor-pre ed) "") (make-editor "" 0)] 
    [else (left-cursor (make-editor (string-append (substring  (editor-pre ed) 0 (- (editor-post ed) 1)) (substring (editor-pre ed) (editor-post ed) (string-length  (editor-pre ed)))) (editor-post ed)))]))
