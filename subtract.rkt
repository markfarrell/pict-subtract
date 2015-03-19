#lang racket

(provide subtract)

(require pict)
(require "conversions.rkt")
(require "chunk.rkt")
(require "zip.rkt")

;; (: subtract-rgb 
;;    (-> (Vector Integer Integer Integer Integer)
;;        (Vector Integer Integer Integer Integer)
;;        (Vector Integer Integer Integer Integer)))
(define (subtract-rgb v2 v1)
  (define (clamp-byte x)
    (max (min x 255) 0))
  (match* (v2 v1)
    [[(vector a2 r2 g2 b2) (vector a1 r1 g1 b1)]
     (vector a2
             (clamp-byte (- r2 r1))
             (clamp-byte (- g2 g1))
             (clamp-byte (- b2 b1)))]))

;; (: subtract (-> pict pict pict))
(define (subtract pict-2 pict-1)
  (argb-vectors->pict
    (map (λ (ps)
            (match ps
              [(list p2 p1)
               (subtract-rgb p2 p1)]))
         (zip (pict->argb-vectors pict-2)
              (pict->argb-vectors
                (scale-to-fit pict-1 pict-2))))
         (pict-width pict-2)))
