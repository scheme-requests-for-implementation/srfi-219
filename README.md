# SRFI nnn: Define higher-order lambda

by Lassi Kortela

## Status

Draft

## Abstract

This SRFI codifies the following shorthand syntax, which some Scheme
implementations have had for a long time.

    (define ((outer-name outer-args ...) inner-args ...)
      inner-body ...)

## Rationale

Procedures that make other procedures are commonly used in Scheme. A
shorthand syntax makes them easier to define, obviating the need to
write a lambda inside a lambda. The following code:

    (define (foo a b c)
      (lambda (d e)
        body ...))

can be shortened to:

    (define ((foo a b c) d e)
      body ...)

Apart from helping define higher-order functions, the shorthand syntax
also partially alleviates the lack of syntactic sugar for making
partially applied functions in Scheme.

### Survey of prior art

The following Scheme implementations have the shorthand syntax built
in:

* Chicken
* Gauche
* Larceny
* MIT Scheme
* Racket
* Sagittarius
* Scheme 9 from Empty Space

The following implementations don't have it: Bigloo, BiwaScheme, Chez
Scheme, Chibi-Scheme, Cyclone, Gambit, Gerbil, Guile, Ikarus,
IronScheme, Kawa, Loko, Mosh, s7, Scheme 48, SigScheme, STklos,
TinyScheme, Vicare, Ypsilon.

### Exporting the shorthand from a library

Since the shorthand is non-standard (i.e. not defined in _RnRS_), it
can be controversial or confusing to the uninitiated programmer.

This SRFI handles the conflict by storing the shorthand version of
`define` in a library that does not have to be imported by default.
Then programmers can choose whether to import it or not. In portable
code the `import` serves to document the dependency on this SRFI.

## Specification

The shorthand version of `define` behaves according to the following
`syntax-rules` macro:

    (define-syntax define
      (syntax-rules ()

        ((define ((name args ... . tail) xargs ... . xtail)
           xbody ...)
         (define/native (name args ... . tail)
           (lambda (xargs ... . xtail)
             xbody ...)))

        ((define ((name args ... . tail) xargs ...)
           xbody ...)
         (define/native (name args ... . tail)
           (lambda (xargs ...)
             xbody ...)))

        ((define ((name args ...) xargs ... . xtail)
           xbody ...)
         (define/native (name args ...)
           (lambda (xargs ... . xtail)
             xbody ...)))

        ((define ((name args ...) xargs ...)
           xbody ...)
         (define/native (name args ...)
           (lambda (xargs ...)
             xbody ...)))

        ((define things ...)
         (define/native things ...))))

where `define/native` is the implementation's native version of
`define`. The native `define` can be equivalent to the `define` from a
_RnRS_ report, or may have extensions to _RnRS_ `define`.

### Importing in R6RS and R7RS

In R6RS Scheme implementations, the shorthand version of `define` is
exported from the library `(srfi :NNN)`. In R7RS Scheme
implementations, it is exported from the library `(srfi NNN)`.

The shorthand version of `define` is exported under the name `define`,
which means that it shadows _RnRS_ `define`. To avoid the name clash,
a program using this SRFI should import the _RnRS_ base library as:

* R6RS: `(import (except (rnrs) define))`
* R7RS: `(import (except (scheme base) define))`

Alternatively, Scheme's import renaming can be used to import the
shorthand `define` under a different name, in which case the same
program can alternate between using the shorthand `define` and _RnRS_
`define`. For example:

* R6RS: `(import (rename (srfi :NNN) (define define/higher)))`
* R7RS: `(import (rename (srfi NNN)  (define define/higher)))`

### Importing from other libraries

The shorthand `define` may also be imported from libraries, possibly
under other names.

## Importing by default

This SRFI does not say whether or not the shorthand `define` is
imported into the default interaction environment.

## Examples

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

((append-to)) => ()
((append-to '(1 2) '(3 4)) '(5 6) '(7 8)) => (1 2 3 4 5 6 7 8)
```

## Implementation

R6RS and R7RS libraries using `syntax-rules` are attached.

## Acknowledgements

Thanks to Arthur Gleckler for explaining the trick, and to Göran
Weinholt for collaborating on Docker containers that made it easy to
do the survey.

## Copyright

Copyright (C) Lassi Kortela (2021).

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
