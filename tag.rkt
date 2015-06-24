#lang racket/base
(require racket/contract/base
         pict
         "private/tag-pict.rkt")

(provide/contract
 [tag-path?
  (-> any/c boolean?)]
 [tag-pict
  (-> pict? symbol? pict?)]
 [pict-tag
  (-> pict? (or/c symbol? #f))]
 [find-tag
  (-> pict? tag-path? (or/c pict-path? #f))]
 [find-tag*
  (-> pict? tag-path?
      (listof pict-path?))])
