(import (except (rnrs) define) (srfi 64) (srfi nnn))

(test-begin "srfi-NNN")

(begin (define ((greet/prefix prefix) suffix)
         (string-append prefix " " suffix))
       (let ((greet (greet/prefix "Hello")))
         (test-equal "Hello there!" (greet "there!"))))

(begin (define ((append-to . b) . a)
         (apply append (append b a)))
       (test-equal '()
         ((append-to '()) '()))
       (test-equal '(1 2 3 4 5 6 7 8)
         ((append-to '(1 2) '(3 4)) '(5 6) '(7 8))))

(test-end)
