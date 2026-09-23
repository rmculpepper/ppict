;; Copyright 2011-2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang info

(define version "1.3")

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
  '(("ppict.scrbl" () ("Slideshow Libraries"))))

(define license
  '(Apache-2.0 OR MIT))
