(define-library (srfi 219)
  (export define)
  (import (rename (scheme base) (define define/standard)))
  (begin

    (define-syntax define
      (syntax-rules ()

        ((_ ((name args ... . tail) xargs ... . xtail) xbody ...)
         (define/standard (name args ... . tail)
           (lambda (xargs ... . xtail) xbody ...)))

        ((_ ((name args ... . tail) xargs ...) xbody ...)
         (define/standard (name args ... . tail)
           (lambda (xargs ...) xbody ...)))

        ((_ ((name args ...) xargs ... . xtail) xbody ...)
         (define/standard (name args ...)
           (lambda (xargs ... . xtail) xbody ...)))

        ((_ ((name args ...) xargs ...) xbody ...)
         (define/standard (name args ...)
           (lambda (xargs ...) xbody ...)))

        ((_ things ...)
         (define/standard things ...))))))
