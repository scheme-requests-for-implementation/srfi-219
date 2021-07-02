(library (srfi :219)
  (export define)
  (import (rename (rnrs base (6)) (define define/native)))
  (define-syntax define
    (syntax-rules ()
      ((define ((head . outer-args) . args) . body)
       (define (head . outer-args) (lambda args . body)))
      ((define (head . args) . body)
       (define head (lambda args . body)))
      ((define head . body)
       (define/native head . body)))))
