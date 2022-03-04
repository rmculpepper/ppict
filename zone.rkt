#lang racket/base
(require racket/contract/base
         pict
         "private/ppict.rkt")
(provide zone?
         (contract-out
          [subplacer
           (-> placer? #:in zone? placer?)]
          [subzone
           (-> zone? #:in zone? zone?)]
          [make-zone
           (-> (-> pict? real? real? real? real? (values real? real? real? real?)) zone?)]
          [coord-zone
           (-> rel/abs-length? rel/abs-length? rel/abs-length? rel/abs-length? zone?)]
          [grid-zone
           (->* [exact-positive-integer? exact-positive-integer? exact-integer? exact-integer?]
                [(>=/c 0) (>=/c 0)]
                zone?)]
          [placer-zone
           (-> refpoint-placer? rel/abs-length? rel/abs-length? zone?)]))
