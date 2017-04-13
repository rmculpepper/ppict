#lang racket/base
(require racket/match
         racket/class
         racket/gui
         pict
         "tag-pict.rkt")
(provide tag-pict-regions-gui)

;; This module private and unstable; it may change or disappear in a
;; future version of this library.

;; GUI for helping craft calls to tag-pict-regions
;; - Left-click on the pict to start drawing a rectangular region.
;; - Left-click again to complete the region and print out the
;;   bounding-box coordinates (and a _ placeholder for the tag).
;; - Right-clicking or leaving the canvas cancels the current box.

(define XMARGIN 20)
(define YMARGIN 20)
(define MAGNIFY 2)

(define (tag-pict-regions-gui p)
  (define f (new frame% (label "Tag Regions") (min-width 400) (min-height 300)))
  (define pane (new vertical-pane% (parent f) (alignment '(center center))))
  (define c (new tag-pict-canvas% (parent pane) (pict0 p)))
  (send f show #t))

(define tag-pict-canvas%
  (class canvas%
    (inherit get-dc
             refresh)

    (init-field pict0)
    (define pict (scale pict0 MAGNIFY))

    (super-new (min-width (+ XMARGIN XMARGIN (exact-ceiling (pict-width pict))))
               (min-height (+ YMARGIN YMARGIN (exact-ceiling (pict-height pict))))
               (stretchable-width #f)
               (stretchable-height #f))

    (define pict-drawer (make-pict-drawer pict))

    ;; State is one of
    ;; - #f
    ;; - (list dcx dcy) -- first corner of bbox or last position of mouse

    (define state #f)
    (define last-mouse #f)

    (define/override (on-paint)
      (super on-paint)
      (pict-drawer (get-dc) XMARGIN YMARGIN)

      (call/dc (get-dc)
        (lambda (dc)
          (define w (pict-width pict))
          (define h (pict-height pict))
          (send* dc
            [set-pen "gray" 1 'dot]
            [draw-line 0 YMARGIN (+ XMARGIN XMARGIN w) YMARGIN]
            [draw-line 0 (+ YMARGIN h) (+ XMARGIN XMARGIN w) (+ YMARGIN h)]
            [draw-line XMARGIN 0 XMARGIN (+ YMARGIN YMARGIN h)]
            [draw-line (+ XMARGIN w) 0 (+ XMARGIN w) (+ YMARGIN YMARGIN h)])))

      (match* [state last-mouse]
        [[(list x1 y1) (list xm ym)]
         (let-values ([(x1 y1 x2 y2) (get-rectangle x1 y1 xm ym)])
           (call/dc (get-dc)
             (lambda (dc)
               (send dc set-pen (get-highlight-background-color) 1 'xor-dot-dash)
               (send dc set-brush "white" 'transparent)
               (send dc draw-rectangle x1 y1 (- x2 x1) (- y2 y1)))))]
        [[_ _] (void)]))

    (define/override (on-char ke)
      (super on-char ke)
      (void))

    (define/override (on-event me)
      (super on-event me)
      (define xm (send me get-x))
      (define ym (send me get-y))
      (set! last-mouse (list xm ym))
      (case (send me get-event-type)
        [(left-down)
         (match state
           [(list x1 y1)
            (finish-bbox x1 y1 xm ym)
            (set! state #f)]
           [#f
            (set! state (list xm ym))])]
        [(leave right-down)
         (set! state #f)])
      (refresh))

    (define (finish-bbox x1 y1 x2 y2)
      (let-values ([(x1 y1 x2 y2) (get-rectangle x1 y1 x2 y2)])
        (printf "bbox: ~s\n"
                (list (dcx->pictx x1)
                      (dcy->picty y1)
                      (dcx->pictx x2)
                      (dcy->picty y2)
                      '_))))

    (define/private (get-rectangle x1 y1 x2 y2)
      (values (min x1 x2) (min y1 y2)
              (max x1 x2) (max y1 y2)))
    (define/private (dcx->pictx x)
      (exact-ceiling (/ (- x XMARGIN) MAGNIFY)))
    (define/private (dcy->picty y)
      (exact-ceiling (/ (- y YMARGIN) MAGNIFY)))
    ))

(define (call/dc dc proc)
  (define old-pen (send dc get-pen))
  (define old-brush (send dc get-brush))
  (begin0 (proc dc)
    (send dc set-pen old-pen)
    (send dc set-brush old-brush)))

(define (exact-ceiling x) (inexact->exact (ceiling x)))
