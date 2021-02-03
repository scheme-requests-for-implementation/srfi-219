(import (except (scheme base) define) (scheme write) (srfi 219))

(define (disp . xs) (for-each display xs) (newline))

(define ((greet/prefix prefix) name) (disp prefix name))
(define greet (greet/prefix "Hello "))
(greet "there!")

(define ((append-to . b) . a) (apply append (append b a)))
(disp ((append-to '(1 2) '(3 4)) '(5 6) '(7 8)))
