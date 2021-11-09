#lang racket/base
(require pict
         racket/contract/base)
(provide
 (contract-out
  [halign->vcompose (-> halign/c procedure?)]
  [valign->hcompose (-> valign/c procedure?)]
  [pin-over/align (-> pict? real? real? halign/c valign/c pict? pict?)]
  [pin-over/align2 (-> pict? align/c real? real? pict? align/c pict?)]
  [inset-to/align (-> pict? (or/c real? #f) (or/c real? #f) align/c pict?)]  
  [align->frac (-> (or/c halign/c valign/c) real?)]
  [align->x (-> (or/c halign/c align/c) pict? real?)]
  [align->y (-> (or/c valign/c align/c) pict? real?)]
  [align/c contract?]
  [halign/c contract?]
  [align->h (-> align/c halign/c)]
  [valign/c contract?]
  [align->v (-> align/c valign/c)]
  [make-align (-> halign/c valign/c align/c)]))

(define (pin-over/align scene x y halign valign pict)
  (let ([localrefx (align->x halign pict)]
        [localrefy (align->y valign pict)])
    (pin-over scene (- x localrefx) (- y localrefy) pict)))

(define pin-over/align2
  (case-lambda
    [(scene s-align x y pict p-align)
     (pin-over scene
               (+ x (align->x s-align scene) (- (align->x p-align pict)))
               (+ y (align->y s-align scene) (- (align->y p-align pict)))
               pict)]
    [(scene sh-align sv-align x y pict ph-align pv-align)
     (pin-over scene
               (+ x (align->x sh-align scene) (- (align->x ph-align pict)))
               (+ y (align->y sv-align scene) (- (align->y pv-align pict)))
               pict)]))

(define inset-to/align
  (case-lambda
    [(p w h align)
     (inset-to/align p w h (align->h align) (align->v align))]
    [(p w h halign valign)
     (define dw (if w (- w (pict-width p)) 0))
     (define dh (if h (- h (pict-height p)) 0))
     (inset p
            (* dw (case halign [(l) 0] [(c) 1/2] [(r) 1]))
            (* dh (case valign [(t) 0] [(c) 1/2] [(b bl tl) 1]))
            (* dw (case halign [(l) 1] [(c) 1/2] [(r) 0]))
            (* dh (case valign [(t) 1] [(c) 1/2] [(b bl tl) 0])))]))

(define (align->x align pict)
  (case align
    [(l) 0]
    [(c) (* 1/2 (pict-width pict))]
    [(r) (pict-width pict)]
    [else (align->x (align->h align) pict)]))
(define (align->y align pict)
  (case align
    [(t) 0]
    [(c) (* 1/2 (pict-height pict))]
    [(b) (pict-height pict)]
    [(bl) (- (pict-height pict) (pict-descent pict))]
    [(tl) (pict-ascent pict)]
    [else (align->y (align->v align) pict)]))

(define (align->frac align)
  (case align
    ((t l)   0)
    ((c)   1/2)
    ((b r bl tl)   1)))

(define (align->h align)
  (case align
    ((lt lc lb lbl ltl) 'l)
    ((ct cc cb cbl ctl) 'c)
    ((rt rc rb rbl rtl) 'r)
    (else (error 'align->h "bad alignment: ~e" align))))

(define (align->v align)
  (case align
    ((lt ct rt) 't)
    ((lc cc rc) 'c)
    ((lb cb rb) 'b)
    ((lbl cbl rbl) 'bl)
    ((ltl ctl rtl) 'tl)
    (else (error 'align->v "bad alignment: ~e" align))))

(define (halign->vcompose halign)
  (case halign
    ((l) vl-append)
    ((c) vc-append)
    ((r) vr-append)))

(define (valign->hcompose align)
  (case align
    ((t) ht-append)
    ((c) hc-append)
    ((b) hb-append)
    ((bl) hbl-append)
    ((tl) htl-append)))

(define align/c
  (or/c 'lt 'ct 'rt
        'lc 'cc 'rc
        'lb 'cb 'rb
        'lbl 'cbl 'rbl
        'ltl 'ctl 'rtl))
(define halign/c
  (or/c 'l 'c 'r))
(define valign/c
  (or/c 't 'c 'b 'bl 'tl))

(define (make-align halign valign)
  (case valign
    [(t) (case halign [(l) 'lt] [(c) 'ct] [(r) 'rt])]
    [(c) (case halign [(l) 'lc] [(c) 'cc] [(r) 'rc])]
    [(b) (case halign [(l) 'lb] [(c) 'cb] [(r) 'rb])]
    [(bl) (case halign [(l) 'lbl] [(c) 'cbl] [(r) 'rbl])]
    [(tl) (case halign [(l) 'ltl] [(c) 'ctl] [(r) 'rtl])]))
