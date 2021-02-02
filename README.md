# SRFI nnn: Define higher-order lambda

by Firstname Lastname, Another Person, Third Person

# Status

Early Draft

# Abstract

This SRFI codifies the shorthand syntax (define ((_name_ _outer-args_
...) _inner-args_ ...) _body_ ...) which some Scheme implementations
have had for a long time.

# Rationale

Procedures that return procedures are commonly useful in Scheme.

## Survey of prior art

Scheme implementations that have it:

* Chicken
* Gauche
* Larceny
* MIT Scheme
* Racket
* Sagittarius
* Scheme 9 from Empty Space

Scheme implementations that don't have it: Bigloo, BiwaScheme, Chez
Scheme, Chibi-Scheme, Cyclone, Gambit, Gerbil, Guile, Ikarus,
IronScheme, Kawa, Loko, Mosh, s7, Scheme 48, SigScheme, STklos,
TinyScheme, Vicare, Ypsilon

# Specification

The library `(srfi NNN)` exports a version of `define` that acts
according to the following `syntax-rules` macro:

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
         (define/standard things ...))))

where `define/standard` is the standard version of `define`.

# Examples

Simplest example:

```
(define ((greet-with-prefix prefix) suffix)
  (string-append prefix " " suffix))

(define greet (greet-with-prefix "Hello"))

(greet "there!") => "Hello there!"
```

With a dotted list to take a variable number of arguments:

```
(define ((append-to . b) . a)
  (apply append (append b a)))

((append-to '(1 2) '(3 4)) '(5 6) '(7 8)) => (1 2 3 4 5 6 7 8)
```

# Implementation

Attached.

# Acknowledgements

Thanks to Arthur Gleckler for explaining the trick, and to Göran
Weinholt for collaborating on Docker containers that made it easy to
do the survey.

# References

# Copyright

Copyright (C) Firstname Lastname (20XY).

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
