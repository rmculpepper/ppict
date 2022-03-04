#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/examples
          (for-label racket/base
                     slideshow
                     ppict/pict
                     ppict/tag
                     ppict/align
                     ppict/slideshow))

@title[#:tag "ppict"]{Progressive Picts and Slides}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define the-eval (make-base-eval))
@(the-eval '(require pict ppict/pict ppict/tag ppict/align))

@defmodule[ppict/2]

The @racketmodname[ppict/2] module re-exports the contents of
@racketmodname[ppict/pict], @racketmodname[ppict/tag],
@racketmodname[ppict/align], and @racketmodname[ppict/slideshow2].

@history[#:added "1.1"]

@defmodule[ppict #:no-declare]

Deprecated. Like @racketmodname[ppict/2] but re-exports the deprecated
module @racketmodname[ppict/slideshow] instead of
@racketmodname[ppict/slideshow2].

@history[#:changed "1.1" @elem{Deprecated @racketmodname[ppict].}]

@section[#:tag "ppicts"]{Progressive Picts}

@defmodule[ppict/pict]

A @deftech{progressive pict} or ``ppict'' is a kind of @racket[pict]
that has an associated ``pict placer,'' which generally represents a
position and alignment. New picts can be placed on the progressive
pict by calling @racket[ppict-add], and the placer can be updated by
calling @racket[ppict-go]. The @racket[ppict-do] form provides a
compact notation for sequences of those two operations.

@deftogether[[
@defform[(ppict-do base-expr ppict-do-fragment ...)]
@defform/subs[(ppict-do* base-expr ppict-do-fragment ...)
              ([ppict-do-fragment (code:line #:go placer-expr)
                                  (code:line #:set pict-expr)
                                  (code:line #:next)
                                  (code:line #:alt (ppict-do-fragment ...))
                                  (code:line #:do [def-or-expr ...])
                                  (code:line elem-expr)])
              #:contracts ([base-expr pict?]
                           [placer-expr placer?]
                           [pict-expr pict?]
                           [elem-expr (or/c pict? real? #f)])]]]{

Builds a pict (and optionally a list of intermediate picts)
progressively. The @racket[ppict-do] form returns only the final pict;
any uses of @racket[#:next] are ignored. The @racket[ppict-do*] form
returns two values: the final pict and a list of all partial picts
emitted due to @racket[#:next] (the final pict is not included).

A @racket[#:go] fragment changes the current placer. A @racket[#:set]
fragment replaces the current pict state altogether with a new
computed pict. A @racket[#:next] fragment saves a pict including only
the contents emitted so far (but whose alignment takes into account
picts yet to come). A @racket[#:alt] fragment saves the current pict
state, executes the sub-sequence that follows, saves the result (as if
the sub-sequence ended with @racket[#:next]), then restores the saved
pict state before continuing.

A @racket[#:do] fragment embeds definitions and expressions which are
run when the pict state is computed. The definitions are bound in the
rest of the fragments in the pict.

The @racket[elem-expr]s are interpreted by the current placer. A
numeric @racket[elem-expr] usually represents a spacing change, but
some placers do not support them. A spacing change only affects added
picts up until the next placer is installed; when a new placer is
installed, the spacing is reset, usually to @racket[0].

The @racket[ppict-do-state] form tracks the current state of the
pict. It is updated before a @racket[#:go] or @racket[#:set] fragment
or before a sequence of @racket[elem-expr]s. It is not updated in the
middle of a chain of @racket[elem-expr]s, however.

@examples[#:eval the-eval
(define base
  (ppict-do (colorize (rectangle 200 200) "gray")
            #:go (coord 1/2 1/2 'cc)
            (colorize (hline 200 1) "gray")
            #:go (coord 1/2 1/2 'cc)
            (colorize (vline 1 200) "gray")))
base
]
The use of @racket[ppict-do] in the definition of @racket[base] above
is equivalent to
@racketblock[
(let* ([pp (colorize (rectangle 200 200) "gray")]
       [pp (ppict-go pp (coord 1/2 1/2 'cc))]
       [pp (ppict-add pp (colorize (hline 200 1) "gray"))]
       [pp (ppict-go pp (coord 1/2 1/2 'cc))]
       [pp (ppict-add pp (colorize (vline 1 200) "gray"))])
  pp)
]

@examples[#:eval the-eval
(define circles-down-1
  (ppict-do base
            #:go (grid 2 2 2 1 'ct)
            10
            (circle 20)
            (circle 20)
            30
            (circle 20)))
circles-down-1
(define circles-down-2
  (ppict-do circles-down-1
            (colorize (circle 20) "red")
            40
            (colorize (circle 20) "red")))
(code:line (inset circles-down-2 20) (code:comment "draws outside its bounding box"))
(inset (clip circles-down-2) 20)
(ppict-do base
          #:go (coord 0 0 'lt)
          (tag-pict (circle 20) 'circA)
          #:go (coord 1 1 'rb)
          (tag-pict (circle 20) 'circB)
          #:set (let ([p ppict-do-state])
                  (pin-arrow-line 10 p
                                  (find-tag p 'circA) rb-find
                                  (find-tag p 'circB) lt-find)))
(let-values ([(final intermediates)
              (ppict-do* base
                         #:go (coord 1/4 1/2 'cb)
                         (text "shapes:")
                         #:go (coord 1/2 1/2 'lb)
                         #:alt [(circle 20)]
                         #:alt [(rectangle 20 20)]
                         (text "and more!"))])
  (append intermediates (list final)))
]

The following demonstrates the use of the @racket[#:do] fragment:

@examples[#:eval the-eval
(ppict-do base
          #:do [(define c (circle 20))
                (define r (rectangle 20 20))
                (set! c (circle 25))]
          #:go (coord 0.5 0.5)
          (hc-append c r))
]

More examples of @racket[ppict-do] are scattered throughout this
section.
}

@defidform[ppict-do-state]{

Tracks the current state of a @racket[ppict-do] or @racket[ppict-do*]
form.
}

@defproc[(ppict? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a @tech{progressive pict},
@racket[#f] otherwise.
}

@defproc[(ppict-go [p pict?] [pl placer?]) ppict?]{

Creates a @tech{progressive pict} with the given base pict @racket[p]
and the placer @racket[pl].
}

@deftogether[[
@defproc[(ppict-add [pp ppict?]
                    [elem (or/c pict? real? #f 'next)] ...)
         pict?]
@defproc[(ppict-add* [pp ppict?]
                     [elem (or/c pict? real? #f 'next)] ...)
         (values pict? (listof pict?))]]]{

Creates a new pict by adding each @racket[elem] pict on top of
@racket[pp] according to @racket[pp]'s placer. The result pict may or
may not be a @tech{progressive pict}, depending on the placer
used. The @racket[ppict-add] function only the final pict; any
occurrences of @racket['next] are ignored. The @racket[ppict-add*]
function returns two values: the final pict and a list of all partial
picts emitted due to @racket['next] (the final pict is not included).

An @racket[elem] that is a real number changes the spacing for
subsequent additions. A @racket[elem] that is @racket[#f] is
discarded; it is permitted as a convenience for conditionally
including sub-picts. Note that @racket[#f] is not equivalent to
@racket[(blank 0)], since the latter will cause spacing to be added
around it.
}

@defproc[(placer? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a placer, @racket[#f] otherwise.
}

@defproc[(refpoint-placer? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a placer based on a reference
point, @racket[#f] otherwise.
}

@defproc[(coord [rel-x real?] 
                [rel-y real?]
                [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
                [#:abs-x abs-x real? 0]
                [#:abs-y abs-y real? 0]
                [#:sep sep real? 0]
                [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to @racket[rel-x] and
@racket[rel-y], which are interpeted as fractions of the width and
height of the base @tech{progressive pict}. That is, @racket[0],
@racket[0] is the top left corner of the base's bounding box, and
@racket[1], @racket[1] is the bottom right. Then @racket[abs-x] and
@racket[abs-y] offsets are added to get the final reference point.

Additions are aligned according to @racket[align], a symbol whose name
consists of a horizontal alignment character followed by a vertical
alignment character. For example, if @racket[align] is @racket['lt],
the pict is placed so that its left-top corner is at the reference
point; if @racket[align] is @racket['rc], the pict is placed so that
the center of its bounding box's right edge coincides with the
reference point.

By default, if there are multiple picts to be placed, they are
vertically appended, aligned according to the horizontal component of
@racket[align]. For example, if @racket[align] is @racket['cc], the
default @racket[composer] is @racket[vc-append]; for @racket['lt], the
default @racket[composer] is @racket[vl-append]. The spacing is
initially @racket[sep].

@examples[#:eval the-eval
(ppict-do base 
          #:go (coord 1/2 1/2 'rb)
          (colorize (circle 20) "red")
          #:go (coord 1/2 1/2 'lt)
          (colorize (circle 20) "darkgreen"))
(ppict-do base
          #:go (coord 1 0 'rt #:abs-x -5 #:abs-y 10)
          50 (code:comment "change spacing")
          (text "abc")
          (text "12345")
          0  (code:comment "and again")
          (text "ok done"))
(ppict-do base
          #:go (coord 0 0 'lt #:compose ht-append)
          (circle 10)
          (circle 20)
          (circle 30))
]

@history[#:changed "1.1" @elem{Added @racket[#:sep] argument.}]
}

@defproc[(grid [cols exact-positive-integer?]
               [rows exact-positive-integer?]
               [col exact-integer?]
               [row exact-integer?]
               [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
               [#:abs-x abs-x real? 0]
               [#:abs-y abs-y real? 0]
               [#:sep sep real? 0]
               [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to a position in a
virtual grid. The @racket[row] and @racket[col] indexes are numbered
starting at @racket[1].

Uses of @racket[grid] can be translated into uses of @racket[coord],
but the translation depends on the alignment. For example,
@racket[(grid 2 2 1 1 'lt)] is equivalent to @racket[(coord 0 0 'lt)],
but @racket[(grid 2 2 1 1 'rt)] is equivalent to @racket[(coord 1/2 0 'rt)].

@examples[#:eval the-eval
(define none-for-me-thanks
  (ppict-do base
            #:go (grid 2 2 1 1 'lt)
            (text "You do not like")
            (colorize (text "green eggs and ham?") "darkgreen")))
none-for-me-thanks
(ppict-do none-for-me-thanks
          #:go (grid 2 2 2 1 'rb)
          (colorize (text "I do not like them,") "red")
          (text "Sam-I-am."))
]

@history[#:changed "1.1" @elem{Added @racket[#:sep] argument.}]
}

@defproc[(cascade [step-x (or/c real? 'auto) 'auto]
                  [step-y (or/c real? 'auto) 'auto])
         placer?]{

Returns a placer that places picts by evenly spreading them diagonally
across the base pict in ``cascade'' style. This placer does not
support changing the spacing by including a real number within the
pict sequence.

When a list picts is to be placed, their bounding boxes are normalized
to the maximum width and height of all picts in the list; each pict is
centered in its new bounding box. The picts are then cascaded so there
is @racket[step-x] space between each of the picts' left edges; there
is also @racket[step-x] space between the base pict's left edge and
the first pict's left edge. Similarly for @racket[step-y] and the
vertical spacing.

If @racket[step-x] or @racket[step-y] is @racket['auto], the spacing
between the centers of the picts to be placed is determined
automatically so that the inter-pict spacing is the same as the
spacing between the last pict and the base.

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (colorize (filled-rectangle 100 100) "red")
          (colorize (filled-rectangle 100 100) "blue"))
(ppict-do base
          #:go (cascade 40 20)
          (colorize (filled-rectangle 100 100) "red")
          (colorize (filled-rectangle 100 100) "blue"))
]
}

@defproc[(tile [cols exact-positive-integer?]
               [rows exact-positive-integer?])
         placer?]{

Returns a placer that places picts by tiling them in a grid
@racket[cols] columns wide and @racket[rows] rows high.

@examples[#:eval the-eval
(ppict-do base
          #:go (tile 2 2)
          (circle 50)
          (rectangle 50 50)
          (jack-o-lantern 50)
          (standard-fish 50 30 #:color "red"))
]
}

@defproc[(at-find-pict [find-path (or/c tag-path? pict-path?)]
                       [finder procedure? cc-find]
                       [align (or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb) 'cc]
                       [#:abs-x abs-x real? 0]
                       [#:abs-y abs-y real? 0]
                       [#:sep sep real? 0]
                       [#:compose composer procedure? #, @elem{computed from @racket[align]}])
         refpoint-placer?]{

Returns a placer that places picts according to a reference point
based on an existing pict within the base.

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (tag-pict (standard-fish 40 20 #:direction 'right #:color "red") 'red-fish)
          (tag-pict (standard-fish 50 30 #:direction 'left #:color "blue") 'blue-fish)
          #:go (at-find-pict 'red-fish rc-find 'lc #:abs-x 10)
          (text "red fish"))
]

@history[#:changed "1.1" @elem{Added @racket[#:sep] argument.}]
}

@defproc[(merge-refpoints [x-placer refpoint-placer?] 
                          [y-placer refpoint-placer?])
         refpoint-placer?]{

Returns a placer like @racket[x-placer] except that the y-coordinate of its
reference point is computed by @racket[y-placer].

@examples[#:eval the-eval
(ppict-do base
          #:go (cascade)
          (tag-pict (standard-fish 40 20 #:direction 'right #:color "red") 'red-fish)
          (tag-pict (standard-fish 50 30 #:direction 'left #:color "blue") 'blue-fish)
          #:go (merge-refpoints (coord 1 0 'rc)
                                (at-find-pict 'red-fish))
          (text "red fish"))
]
}


@; ============================================================
@section[#:tag "pslide"]{Progressive Slides}

@defmodule[ppict/slideshow2]

@history[#:added "1.1"]

@defform[(pslide slide-option ... ppict-do-fragment ...)
         #:grammar
         ([slide-option (code:line #:title title-expr)
                        (code:line #:name name-expr)
                        (code:line #:aspect aspect-expr)
                        (code:line #:layout layout-expr)
                        (code:line #:gap-size gap-expr)
                        (code:line #:inset inset-expr)
                        (code:line #:timeout timeout-expr)
                        (code:line #:condense? condense?-expr)])
         #:contracts
         ([title-expr (or/c string? #f)]
          [name-expr (or/c string? #f)]
          [aspect-expr aspect?]
          [layout-expr (or/c 'auto 'center 'full-center 'top 'tall)]
          [gap-expr real?]
          [inset-expr slide-inset?]
          [timeout-expr (or/c real? #f)]
          [condense?-expr any/c])]{

Produce slide(s) using @tech{progressive picts}.  The slide body is
constructed from the @racket[ppict-do-fragment]s using
@racket[ppict-do] with an initial ppict that depends on
@racket[_aspect] and @racket[_layout] (and potentially @racket[_title] and
@racket[_gap-size] as well).

The @racket[slide-option]s are interpreted the same as for the
@racket[slide] procedure with the exception of @racket[#:layout]. The
result of @racket[layout-expr] is interpreted as follows:

@itemlist[

@item{@racket['auto]: same as @racket['center] if the slide has a
title, otherwise same as @racket['full-center]}

@item{@racket['center]: the initial ppict is sized like
@racket[titleless-page]}

@item{@racket['full-center]: the initial ppict is sized like
@racket[full-page]}

@item{@racket['top], @racket['tall]: interpreted similarly to
@racket[slide]'s treatment}
]

@history[#:changed "1.2" @elem{Added the @racket[#:aspect] optional argument.}]
}

@; ----------------------------------------
@subsection[#:tag "pslide1"]{Progressive Slides Legacy Library}

@defmodule[ppict/slideshow]

@history[#:changed "1.1" @elem{Deprecated @racketmodname[ppict/slideshow].}]

@deftogether[[
@defform[(pslide ppict-do-fragment ...)]
@defparam[pslide-base-pict make-base-pict (-> pict)]
@defparam[pslide-default-placer placer placer?]
]]{

Deprecated; use @racketmodname[ppict/slideshow2] instead.
}

@; ============================================================
@section[#:tag "tag-pict"]{Tagged Picts}

@defmodule[ppict/tag]

@defproc[(tag-pict [p pict?] [tag symbol?]) pict?]{

Returns a pict like @racket[p] that carries a symbolic tag. The tag
can be used with @racket[find-tag] to locate the pict.
}

@defproc[(pict-tag [p pict?]) (or/c symbol? #f)]{
Return the symbolic tag carried by @racket[p].

@examples[#:eval the-eval
  (pict-tag (blank))
  (pict-tag (tag-pict (blank) 'a-blank))
]
}

@defproc[(find-tag [p pict?] [find tag-path?])
         (or/c pict-path? #f)]{

Locates a sub-pict of @racket[p]. Returns a pict-path that can be used
with functions like @racket[lt-find], etc.

@examples[#:eval the-eval
(let* ([a (tag-pict (colorize (disk 20) "red") 'a)]
       [b (tag-pict (colorize (filled-rectangle 20 20) "blue") 'b)]
       [p (vl-append a (hb-append (blank 100) b))])
  (pin-arrow-line 10 p
                  (find-tag p 'a) rb-find
                  (find-tag p 'b) lt-find))
]
}

@defproc[(find-tag* [p pict?] [find tag-path?])
         (listof pict-path?)]{

Like @racket[find-tag], but returns all pict-paths corresponding to
the given tag-path.

@examples[#:eval the-eval
(let* ([a (lambda () (tag-pict (colorize (disk 20) "red") 'a))]
       [b (lambda () (tag-pict (colorize (filled-rectangle 20 20) "blue") 'b))]
       [as (vc-append 10 (a) (a) (a))]
       [bs (vc-append 10 (b) (b) (b))]
       [p (hc-append as (blank 60 0) bs)])
  (for*/fold ([p p])
      ([apath (in-list (find-tag* p 'a))]
       [bpath (in-list (find-tag* p 'b))])
    (pin-arrow-line 4 p
                    apath rc-find
                    bpath lc-find)))
]
}

@defproc[(tag-path? [x any/c]) boolean?]{

Returns @racket[#t] if @racket[x] is a symbol or a non-empty list of
symbols, @racket[#f] otherwise.
}

@defproc[(tag-pict-regions [p pict?]
                           [regions (listof (list/c real? real? real? real? symbol?))])
         pict?]{

For each entry @racket[(list _x1 _y1 _x2 _y2 _tag)] in
@racket[regions], places a blank pict at the region (@racket[_x1],
@racket[_y1])-(@racket[_x2], @racket[_y2]) tagged with @racket[_tag].

For example, use @racket[tag-pict-regions] with pixel-based regions to
identify features within a bitmap-based pict so that they can be the
targets of arrows, anchors for balloons, etc.

@history[#:added "1.1"]
}

@; ============================================================
@section[#:tag "align"]{Alignment}

@defmodule[ppict/align]

@defthing[align/c contract?]{

Equivalent to @racket[(or/c 'lt 'ct 'rt 'lc 'cc 'rc 'lb 'cb 'rb
'lbl 'cbl 'rbl 'ltl 'ctl 'rtl)].

@history[#:changed "1.3" @elem{Added the @litchar{bl} and @litchar{tl}
variants.}]}

@defthing[halign/c contract?]{

Equivalent to @racket[(or/c 'l 'c 'r)].
}

@defthing[valign/c contract?]{

Equivalent to @racket[(or/c 't 'c 'b 'bl 'tl)].

@history[#:changed "1.3" @elem{Added @racket['bl] and @racket['tl].}]}

@deftogether[[
@defproc[(align->h [a align/c]) halign/c]
@defproc[(align->v [a align/c]) valign/c]
]]{

Extracts the @racket[halign/c] or @racket[valign/c] part from
@racket[a], respectively.

@examples[#:eval the-eval
(align->h 'rt)
(align->v 'rt)
]}

@defproc[(make-align [halign halign/c] [valign valign/c]) align/c]{

Returns the alignment consisting of @racket[halign] and
@racket[valign] components.

@examples[#:eval the-eval
(make-align 'r 't)
]

@history[#:added "1.3"]}

@defproc[(align->frac [a (or/c halign/c valign/c)]) real?]{

@bold{Deprecated: } Use @racket[align->x] and @racket[align->y] instead.

Computes the fraction corresponding to an alignment where the top-left
is @racket[0]. If @racket[a] is @racket['bl] or @racket['tl], an
exception is raised.
}

@deftogether[[
@defproc[(align->x [a (or/c halign/c align/c)] [p pict?]) real?]
@defproc[(align->y [a (or/c valign/c align/c)] [p pict?]) real?]
]]{

Returns the horizontal distance from the left edge of @racket[p] or
the vertical distance from the top of @racket[p], respectively, to the
point in the pict specified by @racket[a].

@examples[#:eval the-eval
(align->x 'rt (blank 200 100))
(align->y 'lbl (text "hello"))
]

@history[#:added "1.3"]}

@deftogether[[
@defproc[(halign->vcompose [ha halign/c]) procedure?]
@defproc[(valign->hcompose [va valign/c]) procedure?]
]]{

Returns the @racket[h*-append] or @racket[v*-append] function for the
given horizontal or vertical alignment, respectively.
}

@defproc[(pin-over/align [scene pict?]
                         [x real?] [y real?]
                         [halign halign/c] [valign valign/c]
                         [pict pict?])
         pict?]{

Pins @racket[pict] over @racket[scene] so that the point on
@racket[pict] specified by @racket[halign] and @racket[valign] is
placed at (@racket[x], @racket[y]) relative to the top-left corner of
@racket[scene].
}

@defproc[(pin-over/align2 [scene pict?]
                          [scene-align align/c]
                          [x real?] [y real?]
                          [pict pict?]
                          [pict-align align/c])
         pict?]{

Pins @racket[pict] over @racket[scene] so that the point on
@racket[pict] identified by @racket[pict-align] is placed (@racket[x],
@racket[y]) units right and down from the point on @racket[scene]
identified by @racket[scene-align].

@examples[#:eval the-eval
(pin-over/align2 (rectangle 100 100) 'rc 0 0 (disk 20 #:color "red") 'rb)
]

@history[#:added "1.3"]}

@defproc[(inset-to/align [pict pict?]
                         [width (or/c real? #f)]
                         [height (or/c real? #f)]
                         [align align/c])
         pict?]{

Returns a pict with dimensions (@racket[width], @racket[height])
created by calling @racket[inset] on @racket[pict] such that
@racket[pict] occupies the space specified by @racket[align] in the
result. If either @racket[width] or @racket[height] are @racket[#f],
the @racket[pict]'s corresponding dimension is preserved.

@examples[#:eval the-eval
(frame (inset-to/align (disk 20 #:color "red") 50 30 'lb))
(frame (clip (inset-to/align (disk 40 #:color "blue") 50 30 'rc)))
(frame (inset-to/align (disk 30 #:color "green") 50 #f 'cc))
]

@history[#:added "1.3"]}

@(close-eval the-eval)
