#lang racket

(provide pict->argb-vectors
         argb-vectors->pict)

(require pict)
(require "chunk.rkt")

;; (: pict->argb-vectors (-> pict (Listof (Vector Integer Integer Integer Integer))))
(define (pict->argb-vectors pict)
  (map list->vector (chunk (bytes->list (pict->argb-pixels pict)) 4)))

;; (: argb-vectors->pict (-> (Listof (Vector Integer Integer Integer Integer)) Integer pict))
(define (argb-vectors->pict vs width)
  (argb-pixels->pict (list->bytes (flatten (map vector->list vs))) width))
