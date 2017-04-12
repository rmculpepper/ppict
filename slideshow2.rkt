#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template
                     "private/ppict-syntax.rkt")
         racket/contract/base
         slideshow/base
         pict
         "private/ppict.rkt")
(provide pslide)

(define-syntax (pslide stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:title title))
             (~optional (~seq #:name name))
             (~optional (~seq #:layout layout))
             (~optional (~seq #:gap-size gap-size))
             (~optional (~seq #:inset inset))
             (~optional (~seq #:timeout timeout))
             (~optional (~seq #:condense? condense?)))
        ... . fs)
     #:declare title (expr/c #'maybe-string/c)
     #:declare name (expr/c #'maybe-string/c)
     #:declare layout (expr/c #'layout/c)
     #:declare gap-size (expr/c 'real?)
     #:declare inset (expr/c #'slide-inset?)
     #:declare timeout (expr/c #'maybe-real/c)
     #:declare condense? expr
     #:declare fs (fragment-sequence 'pslide #'xp #'rpss)
     (template
      (pslide* 'pslide
               (?? (?@ #:title title.c))
               (?? (?@ #:name name.c))
               (?? (?@ #:layout layout.c))
               (?? (?@ #:gap-size gap-size.c))
               (?? (?@ #:inset inset.c))
               (?? (?@ #:timeout timeout.c))
               (?? (?@ #:condense? condense?))
               (lambda (xp)
                 (let ([rpss null])
                   fs.code))))]))

(define maybe-string/c (or/c string? #f))
(define layout/c (or/c 'auto 'center 'full-center 'top 'tall))
(define maybe-real/c (or/c real? #f))

;; ============================================================

;; pslide* : symbol (pict -> (values pict (listof pict)) -> void
(define (pslide* who proc
                 #:title [title #f]
                 #:name [name title]
                 #:layout [layout 'auto]
                 #:gap-size [gap-size (current-gap-size)]
                 #:inset [inset no-inset]
                 #:timeout [timeout #f]
                 #:condense? [condense? (and timeout #t)])
  (define (do-slide p)
    (slide #:title title
           #:layout (convert-layout layout)
           #:gap-size gap-size
           #:inset inset
           #:timeout timeout
           #:condense? condense?
           p))
  (define-values (final-pict picts)
    (proc (get-base-ppict title layout gap-size)))
  (for-each do-slide picts)
  (do-slide final-pict)
  (void))

;; get-base-ppict : String/#f Layout Real -> PPict
(define (get-base-ppict title layout gap)
  (case layout
    [(auto)
     (get-base-ppict title (if title 'center 'full-center) gap)]
    [(center)
     (ppict-go (blank client-w (- client-h title-h gap gap))
               (coord 1/2 1/2 'cc #:sep gap))]
    [(full-center)
     (ppict-go full-page
               (coord 1/2 1/2 'cc #:sep gap))]
    [(top)
     (ppict-go (blank client-w (- client-h title-h gap gap))
               (coord 1/2 0 'ct #:sep gap))]
    [(tall)
     (ppict-go (blank client-w (- client-h title-h gap))
               (coord 1/2 0 'ct #:sep gap))]))

(define no-inset (make-slide-inset 0 0 0 0))

;; convert pslide layout symbol to slide layout symbol
(define (convert-layout layout)
  (case layout
    [(auto center full-center) 'center]
    [(top) 'top]
    [(tall) 'tall]))
