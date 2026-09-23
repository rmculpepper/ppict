;; Copyright 2011-2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0 OR MIT

#lang info

;; pkg info

(define version "1.3")
(define collection "ppict")
(define deps
  '("base"
    "ppict-lib"
    "slideshow-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "pict-doc"
    "slideshow-doc"))
(define pkg-authors '(ryanc))
(define pkg-desc "progressive picts")
(define license '(Apache-2.0 OR MIT))

;; collection info

(define name "ppict")
(define scribblings
  '(("ppict.scrbl" () ("Slideshow Libraries"))))

(define compile-omit-paths '("examples"))
(define test-omit-paths '("examples"))
