#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/class
         racket/match
         racket/stxparam
         racket/contract/base
         pict
         "../align.rkt"
         "tag-pict.rkt")
(provide (all-defined-out))

#|
TODO
- document composer contract
- generalize ppict-add to ppict-add* (put 'next support there)
- find a way to support slide animation
|#

;; ============================================================
;; Progressive Picts

;; A ppict is a pict extended with a placer.
(struct ppict pict (placer))

(define (mk-ppict p placer)
  (ppict (pict-draw p)
         (pict-width p)
         (pict-height p)
         (pict-ascent p)
         (pict-descent p)
         (list (make-child p 0 0 1 1 0 0))
         #f
         (pict-last p)
         placer))

(define (ppict-pict dp)
  (child-pict (car (pict-children dp))))

;; ----------------------------------------

(define-syntax-parameter ppict-do-state
  (lambda (stx)
    (raise-syntax-error #f "used out of context" stx)))

;; ppict-go : Pict Placer -> PPict
(define (ppict-go dp pl)
  (cond [(ppict? dp)
         (mk-ppict (ppict-pict dp) pl)]
        [(pict? dp)
         (mk-ppict dp pl)]))

;; An Element is one of
;; - CoreElement
;; - #f     -- ignored
;; - 'next  -- emit intermediate pict

;; A CoreElement is one of
;; - Pict   -- place this pict
;; - Real   -- adjust spacing (if supported)

;; ppict-add : PPict Element ... -> PPict
(define (ppict-add dp . parts)
  (let-values ([(final intermediates)
                (ppict-add/internal 'ppict-add dp parts)])
    final))

;; ppict-add* : PPict Element ... -> (values Pict (Listof Pict))
(define (ppict-add* dp . parts)
  (ppict-add/internal 'ppict-add* dp parts))

;; ppict-add/internal : Symbol Pict (Listof Element)
;;                   -> (values Pict (Listof Pict))
;; In second return value, one pict per 'next occurrence.
;; FIXME: avoid applying ghost to previously ghosted pict?
(define (ppict-add/internal who base parts)
  (cond [(for/and ([part (in-list parts)])
           (or (eq? part #f) (eq? part 'next)))
         ;; Special case; don't need a ppict
         (values base (make-list (length parts) base))]
        [else
         (unless (ppict? base) (error who "missing placer"))
         (send (ppict-placer base) place-all (ppict-pict base) parts)]))

;; ----------------------------------------

(define (placer? x) (is-a? x placer<%>))
(define (refpoint-placer? x) (is-a? x refpoint%))

(define (merge-refpoints x y)
  (send x take-y-from y))

(define placer<%>
  (interface ()
    place-all   ;; Pict (Listof Element) -> (Listof Pict)
    place       ;; Pict (Listof CoreElement) -> Pict
    place*      ;; Pict Real Real Real Real (Listof CoreElement) -> Pict
    ))

(define placer/within-zone<%>
  (interface (placer<%>)
    within-zone ;; Zone -> Placer
    ))

(define placer-base%
  (class* object% (placer<%>)
    (super-new)

    (define/public (place-all scene parts)
      (define (ghost* v) (if (pict? v) (ghost v) v))
      (define elem-chunks  ;; (Listof (Listof Pict))
        ;; length is N+1, where N is number of 'next parts
        (let loop ([parts parts] [ghosts #f] [rprefix null] [acc null])
          (match parts
            [(cons 'next parts)
             (let ([ghosts (or ghosts (map ghost* parts))])
               (define chunk (append (reverse rprefix) ghosts))
               (loop parts ghosts rprefix (cons chunk acc)))]
            [(cons #f parts)
             (loop parts (and ghosts (cdr ghosts)) rprefix acc)]
            [(cons part parts)
             (loop parts (and ghosts (cdr ghosts)) (cons part rprefix) acc)]
            ['() (reverse (cons (reverse rprefix) acc))])))
      (let out-loop ([chunks elem-chunks] [rpicts null])
        (match chunks
          [(cons chunk chunks)
           (out-loop chunks (cons (place scene chunk) rpicts))]
          ['() (values (car rpicts) (reverse (cdr rpicts)))])))

    (define/public (place scene elems)
      (place* scene (pict-width scene) (pict-height scene) 0 0 elems))

    (abstract place*)
    ))

;; ----------------------------------------

(define associative-placer<%>
  (interface (placer<%>)
    get-sep                     ;; -> Real
    compose-elements            ;; (Listof Element) -> (values Pict Real)
    check-associative-vcompose  ;; -> (U VAlign #f)

    ;; A Placer has an associative vertical composer if
    ;; (compose-elements (list e1 ... e2 ...)
    ;; = (let*-values ([(p1 sep1) (compose-elements (list e1 ...))]
    ;;                 [(p2 sep2) (compose-elements (list sep1 e2 ...))])
    ;;     (values (v??-append sep1 p1 p2) sep2))
    ))

(define refpoint%
  (class* placer-base% (placer/within-zone<%> associative-placer<%>)
    (init-field xa ya depxy halign valign compose
                [sep 0]
                [cont? #f])
    (super-new)

    (define/public (get-sep) sep)

    (define/public (get-xy p iw ih ix iy)
      (define-values (depx depy)
        (if depxy (depxy p iw ih ix iy) (values ix iy)))
      (values (+ depx xa) (+ depy ya)))

    (define/override (place* scene iw ih ix iy elems)
      (define-values (newpict newsep)
        (compose-elements (if cont? (cons (blank 0) elems) elems)))
      (define-values (dx dy) (get-xy scene iw ih ix iy))
      (define newscene
        (pin-over/align scene dx dy halign valign newpict))
      (cond [(eq? (check-associative-vcompose) 't)
             ;; ie, going top-down: align is '<H>t and compose is v<H>-append
             (define newplacer (move 0 (pict-height newpict) #:sep newsep #:cont? #t))
             (mk-ppict newscene newplacer)]
            [(eq? (check-associative-hcompose) 'l)
             ;; ie, going left to right: align is 'l<V> and compose is h<V>-append
             (define newplacer (move (pict-width newpict) 0 #:sep newsep #:cont? #t))
             (mk-ppict newscene newplacer)]
            [else newscene]))

    ;; check-associative-hcompose : -> (U HAlign #f)
    ;; check-associative-vcompose : -> (U VAlign #f)
    (define/public (check-associative-hcompose)
      (if (equal? compose (valign->hcompose valign)) halign #f))
    (define/public (check-associative-vcompose)
      (if (equal? compose (halign->vcompose halign)) valign #f))

    (define/public (move dx dy #:sep [sep sep] #:cont? [cont? cont?])
      (new refpoint% (xa (+ xa dx)) (ya (+ ya dy)) (depxy depxy)
           (halign halign) (valign valign) (compose compose) (sep sep) (cont? cont?)))

    (define/public (compose-elements elems)
      (apply-compose compose sep elems))

    (define/public (take-y-from other)
      (new refpoint%
           (xa xa) (ya (get-field ya other))
           (depxy (lambda (p iw ih ix iy)
                    (define-values (myx myy) (get-xy p iw ih ix iy))
                    (define-values (ox oy) (send other get-xy p iw ih ix iy))
                    (values myx oy)))
           (halign halign) (valign valign) (compose compose) (sep sep) (cont? cont?)))

    (define/public (within-zone z)
      (new refpoint%
           (xa xa) (ya ya)
           (depxy (and depxy
                       (let ([depxy depxy])
                         (lambda (p iw ih ix iy)
                           (define-values (zw zh zx zy) (send z get-zone* p iw ih ix iy))
                           (depxy p zw zh zx zy)))))
           (halign halign) (valign valign) (compose compose) (sep sep) (cont? cont?)))
    ))

(define (grid cols rows col row [align 'cc]
              #:abs-x [abs-x 0]
              #:abs-y [abs-y 0]
              #:sep [sep 0]
              #:compose [compose (halign->vcompose (align->h align))])
  ;; row, column indexes are 1-based
  (define halign (align->h align))
  (define valign (align->v align))
  (define xfrac (/ (+ (sub1 col) (align->frac halign)) cols))
  (define yfrac (/ (+ (sub1 row) (align->frac valign)) rows))
  (new refpoint%
       (xa abs-x) (ya abs-y) (sep sep)
       (depxy (lambda (p iw ih ix iy)
                (values (+ ix (* xfrac iw))
                        (+ iy (* yfrac ih)))))
       (halign halign) (valign valign) (compose compose)))

(define (coord xfrac yfrac [align 'cc]
               #:abs-x [abs-x 0]
               #:abs-y [abs-y 0]
               #:sep [sep 0]
               #:compose [compose (halign->vcompose (align->h align))])
  (define (convert u abs-u)
    (match u
      [(? real? u) (values u abs-u)]
      [(list (? real? ur) (? real? ua)) (values u (+ abs-u ua))]))
  (define-values (xr xa) (convert xfrac abs-x))
  (define-values (yr ya) (convert yfrac abs-y))
  (define halign (align->h align))
  (define valign (align->v align))
  (new refpoint%
       (xa xa) (ya ya) (sep sep)
       (depxy (lambda (p iw ih ix iy)
                (values (+ ix (* xr iw))
                        (+ iy (* yr ih)))))
       (halign halign) (valign valign) (compose compose)))

;; at-find-pict : ... -> placer
(define (at-find-pict path
                      [find cc-find]
                      [align 'cc]
                      #:abs-x [abs-x 0]
                      #:abs-y [abs-y 0]
                      #:sep [sep 0]
                      #:compose [compose (halign->vcompose (align->h align))])
  (define halign (align->h align))
  (define valign (align->v align))
  (new refpoint%
       (xa abs-x) (ya abs-y) (sep sep)
       (depxy (lambda (p iw ih ix iy)
                ;; Note: ignores zone context (i[whxy]) completely
                (let ([pict-path (if (tag-path? path) (find-tag p path) path)])
                  (unless pict-path
                    (error 'at-find-path "failed finding ~e" path))
                  (find p pict-path))))
       (halign halign) (valign valign) (compose compose)))

;; ----------------------------------------

(define cascade%
  (class placer-base%
    (init-field step-x0 step-y0)
    (super-new)

    (define/override (place* scene iw ih ix iy elems)
      (for ([e (in-list elems)])
        (when (real? e) (error 'cascade "spacing changes not allowed: ~e" e)))
      (let* ([picts (filter pict? elems)]
             [max-w (apply max 1 (map pict-width picts))]  ;; avoid 0
             [max-h (apply max 1 (map pict-height picts))] ;; avoid 0
             [auto-step-x (/ (- iw max-w) (+ 1 (length picts)))]
             [auto-step-y (/ (- ih max-h) (+ 1 (length picts)))]
             [step-x (if (eq? step-x0 'auto) auto-step-x step-x0)]
             [step-y (if (eq? step-y0 'auto) auto-step-y step-y0)]
             [bbox (blank max-w max-h)]
             [newscene
              (for/fold ([scene scene])
                  ([pict (in-list picts)]
                   [i (in-naturals 1)])
                (pin-over scene
                          (+ ix (* i step-x)) (+ iy (* i step-y))
                          (cc-superimpose bbox pict)))])
        ;; Can't continue a cascade, since depends on number of picts.
        ;; FIXME: If step is given rather than computed, then we can.
        newscene))))

;; cascade : ... -> placer
(define (cascade [step-x0 'auto] [step-y0 'auto])
  ;; Auto cascade by largest bounding box.
  ;; FIXME: add align arg, determines position of each pict w/in bbox
  (new cascade% (step-x0 step-x0) (step-y0 step-y0)))

(define tile%
  (class placer-base%
    (init-field cols rows
                [start-at 0])
    (super-new)

    (define/override (place* scene scene-w scene-h ix iy elems)
      (for ([e (in-list elems)])
        (when (real? e) (error 'tile "spacing changes not allowed: ~e" e)))
      (let* ([picts (filter pict? elems)]
             [dx (/ scene-w cols)]
             [dy (/ scene-h rows)]
             [newscene
              (for/fold ([scene scene])
                  ([pict (in-list picts)]
                   [i (in-naturals start-at)])
                (let ([r (quotient i cols)]
                      [c (remainder i cols)])
                  (pin-over/align scene
                                  (+ ix (/ dx 2) (* c dx))
                                  (+ ix (/ dy 2) (* r dy))
                                  'c 'c pict)))])
        (mk-ppict newscene
                  (new tile%
                       (cols cols)
                       (rows rows)
                       (start-at (+ start-at (length picts)))))))))

(define (tile cols rows)
  (new tile% (cols cols) (rows rows)))

;; ------------------------------------------------------------

(define overflow-placer%
  (class* placer-base% (associative-placer<%>)
    (init-field align overflow-align sep)
    (super-new)

    (define/public (get-sep) sep)

    (define/public (check-associative-vcompose)
      (let ([halign (align->h align)])
        (and (eq? halign (align->h overflow-align)) (align->v align))))

    (define/public (compose-elements elems [align align])
      (define compose (halign->vcompose (align->h align)))
      (apply-compose compose sep elems))

    (define/override (place* scene iw ih ix iy elems)
      (define (go newpict align)
        (let ([halign (align->h align)] [valign (align->v align)])
          (define x (+ ix (* iw (align->frac halign))))
          (define y (+ iy (* ih (align->frac valign))))
          (pin-over/align scene x y (align->h align) (align->v align) newpict)))
      (define-values (newpict newsep) (compose-elements elems align))
      (cond [(<= (pict-height newpict) ih)
             (go newpict align)]
            [else
             (define-values (newpict newsep) (compose-elements elems overflow-align))
             (go newpict overflow-align)]))
    ))

(define (overflow-placer [align 'cc] [overflow-align 'ct] #:sep [sep 0])
  (new overflow-placer% (align align) (overflow-align overflow-align) (sep sep)))

;; ------------------------------------------------------------

;; apply-compose : Compose Real (Listof CoreElement) -> (values Pict Real)
;; Returns composed pict and last given separator num in elems (or init-sep, if none)
(define (apply-compose compose init-sep elems)
  (define (start-loop sep elems)
    (match elems
      [(cons (? real? new-sep) elems)
       (start-loop new-sep elems)]
      [(cons (? pict? p) elems)
       (join-loop p sep elems)]
      ['() (values (blank 0) sep)]))
  (define (join-loop base sep elems)
    (match elems
      [(cons (? real? new-sep) elems)
       (join-loop base new-sep elems)]
      [(cons (? pict? p) elems)
       (join-loop (compose sep base p) sep elems)]
      ['() (values base sep)]))
  (start-loop init-sep (filter values elems)))

(define (rel/abs? v)
  (match v
    [(? real?) #t]
    [(list (? real?) (? real?)) #t]
    [_ #f]))

(define (convert-rel+abs u relto)
  (match u
    [(? real?) (* u relto)]
    [(list (? real? ur) (? real? ua)) (+ ua (* ur relto))]))

;; ------------------------------------------------------------

(define zone<%>
  (interface ()
    get-zone    ;; Pict -> (values Real Real Real Real)
    get-zone*   ;; Pict Real Real Real Real -> (values Real Real Real Real)
    within-zone ;; Zone -> Zone
    ))

(define zone%
  (class object%
    (init-field fs)  ;; (Listof ZoneFunction)
    (super-new)

    ;; A ZoneFunction is (Pict Real Real Real Real -> (values Real Real Real Real))

    (define/public (get-zone p)
      (get-zone* p (pict-width p) (pict-height p) 0 0))

    (define/public (get-zone* p w h x y)
      (for/fold ([w w] [h h] [x x] [y y]) ([f (in-list fs)])
        (f p w h x y)))

    ;; within-zone : ZoneFunction -> Zone
    (define/public (within-zone z)
      ;; Interpret this with respect to an outer zone => put the outer
      ;; zone's functions at the beginning of the list.
      (new zone% (fs (append (get-field fs z) fs))))
    ))

(define (zone? v) (is-a? v zone%))

(define (make-zone f) (new zone% (fs (list f))))

(define (subzone inner outer)
  (send inner within-zone outer))

(define placer-in-zone%
  (class* placer-base% (placer/within-zone<%>)
    (init-field placer zone)
    (super-new)

    (define/override (place* scene iw ih ix iy elems)
      (define-values (zw zh zx zy) (send zone get-zone* scene iw ih ix iy))
      (send placer place* scene zw zh zx zy elems))

    (define/public (within-zone outer)
      (new this% (placer placer) (zone (send zone within-zone outer))))
    ))

(define (subplacer placer zone)
  (cond [(is-a? placer placer/within-zone<%>)
         (send placer within-zone zone)]
        [else (new placer-in-zone% (placer placer) (zone zone))]))

(define (coord-zone x1 y1 x2 y2)
  (define (coord-zone-fun p iw ih ix iy)
    (define xx1 (convert-rel+abs x1 iw))
    (define yy1 (convert-rel+abs y1 ih))
    (define xx2 (convert-rel+abs x2 iw))
    (define yy2 (convert-rel+abs y2 ih))
    (values (max 0 (- xx2 xx1)) (max 0 (- yy2 yy1)) (+ ix xx1) (+ iy yy1)))
  (make-zone coord-zone-fun))

(define (grid-zone cols rows col row)
  (define (grid-zone-fun p iw ih ix iy)
    (define zw (/ iw cols))
    (define zh (/ iw rows))
    (values zw zh (+ ix (* zw (sub1 col))) (+ iy (* zh (sub1 row)))))
  (make-zone grid-zone-fun))

(define (placer-zone refpoint w h)
  (define (placer-zone-fun p iw ih ix iy)
    (define-values (x y) (send refpoint get-xy p iw ih ix iy))
    (define ww (convert-rel+abs w (pict-width p)))
    (define hh (convert-rel+abs h (pict-height p)))
    (values ww hh
            (- x (* ww (align->frac (get-field halign refpoint))))
            (- y (* hh (align->frac (get-field valign refpoint))))))
  (make-zone placer-zone-fun))
