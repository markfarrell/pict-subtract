#lang typed/racket

(provide chunk)

(: chunk (All (a) (-> (Listof a) Integer (Listof (Listof a)))))
(define (chunk xs n)
  (cond [(empty? xs) empty]
        [(< (length xs) n)
         (cons xs empty)]
        [else (cons (take xs n)
                    (chunk (drop xs n) n))]))
