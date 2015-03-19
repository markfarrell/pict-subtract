#lang typed/racket

(provide zip)

(: zip (All (a) (-> (Listof a) * (Listof (Listof a)))))
;; Example:
;; > (zip '(A B C) '(D E F) '(G H I))
;; '((G D A) (B E H) (I F C))
(define (zip . xss)
  (zip-lists xss))

(: zip-lists (All (a) (-> (Listof (Listof a)) (Listof (Listof a)))))
(define (zip-lists xss)
  (cond [(empty? (remove-empty xss)) empty]
        [else (cons (foldl (λ ([xs : (Listof a)] [ys : (Listof a)])
                              (cons (first xs) ys))
                           empty
                           xss)
                    (zip-lists (remove-empty
                                 (foldl (λ ([xs : (Listof a)] [ys : (Listof (Listof a))])
                                           (cons (rest xs) ys))
                                        empty
                                        xss))))]))

(: remove-empty (All (a) (-> (Listof (Listof a)) (Listof (Listof a)))))
(define (remove-empty xss)
  (filter (compose not empty?) xss))
