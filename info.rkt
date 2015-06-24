#lang info

(define collection "ppict")

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "slideshow-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "pict-doc"
                     "slideshow-doc"))

(define pkg-desc "progressive picts")
(define pkg-authors '(ryanc))

(define scribblings
  '(("ppict.scrbl" ())))
