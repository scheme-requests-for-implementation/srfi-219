(library (srfi 219)
  (export define)
  (import (rename (rnrs base (6)) (define define/native)))

  (define-syntax pile-em-up
    (syntax-rules ()
      ((pile-em-up ((head . outer-args) . args) lam)
       (pile-em-up (head . outer-args) (lambda args lam)))
      ((pile-em-up (head . args) lam)
       (define/native head (lambda args lam)))))

  (define-syntax define
    (syntax-rules ()
      ((define (head . args) . body)
       (pile-em-up head (lambda args . body)))
      ((define other-things ...)
       (define/native other-things ...)))))
