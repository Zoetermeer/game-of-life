#lang racket/base
(require "game-common.rkt"
         racket/future)
(provide (all-from-out "game-common.rkt")
         step)

;; live-neighbor-count : universe uint uint -> uint
(define (live-neighbor-count univ r c)
  (define MAX-IND (- (universe-size univ) 1))
  (define prev-row (if (zero? r) MAX-IND (- r 1)))
  (define next-row (if (< r MAX-IND) (+ r 1) 0))
  (define prev-col (if (zero? c) MAX-IND (- c 1)))
  (define next-col (if (< c MAX-IND) (+ c 1) 0))
  (define (one-if-alive? r c)
    (if (alive? univ r c) 1 0))
  (+ (one-if-alive? prev-row prev-col)
     (one-if-alive? prev-row c)
     (one-if-alive? prev-row next-col)
     (one-if-alive? r next-col)
     (one-if-alive? next-row next-col)
     (one-if-alive? next-row c)
     (one-if-alive? next-row prev-col)
     (one-if-alive? r prev-col)))

(define P (processor-count))

;; step : universe -> void
(define (step univ)
  (define sz (universe-size univ)) 
  (define chunk-sz (/ sz P))
  (define fs
    (for/list ([p (in-range P)])
      (define start (* p chunk-sz))
      (future 
       (λ ()
         (for* ([r (in-range start (+ start chunk-sz))]
                [c (in-range sz)])
           (define nbs (live-neighbor-count univ r c))
           (update-cell! univ r c 
                         (cond 
                           [(alive? univ r c) 
                            (case nbs
                              [(2 3) 'alive]
                              [else 'dead])]
                           [else 
                            (if (= 3 nbs)
                                'alive
                                'dead)])))))))
  (for-each touch fs)
  (write-universe-bitmap! univ)
  (swap-buffers! univ))

   
          