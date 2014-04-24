#lang racket/gui
(require "step-core.rkt"
         "game-common.rkt"
         racket/cmdline)

(define usize (make-parameter 512))
(define uname (make-parameter 'random))
(define strategy (make-parameter 'demo))
(define num-threads (make-parameter (processor-count)))

(command-line 
 #:program "game-of-life"
 #:once-each 
 [("-n" "--size") sz 
                  "Size of the universe (length of one square side)"
                  (usize (string->number sz))]
 [("-u" "--universe") univ-name
                      "Type of seed universe to use"
                      (uname (string->symbol univ-name))]
 [("-j" "--num-threads") num-threads
                         "Number of parallel threads to use"
                         (string->number num-threads)])

(define (make-universe-creator fun)
  (λ (sz) (fun sz (cons (/ sz 2) (/ sz 2)))))
                               
(define universe-creator
  (case (uname)
    [(acorn) (λ (sz) (acorn-universe sz (cons (/ sz 2) (/ sz 2))))]
    [(beehive) (λ (sz) (beehive-universe sz (cons (/ sz 2) (/ sz 2))))]
    [(toad) (λ (sz) (toad-universe sz (cons (/ sz 2) (/ sz 2))))]
    [(glider-gun) (make-universe-creator glider-gun-universe)]
    [else random-universe]))


;Input validation
;Number of threads must be a factor of universe size (for simplicity)
(when (and (not (case (strategy) [(seq1 seq2) #f] [else #t])) 
           (not (zero? (remainder (usize) (num-threads)))))
  (error 'game-of-life 
         "Universe size (~a) is not evenly divisible by number of threads (~a)."
         (usize)
         (num-threads)))

(printf "Game of life: ~a threads with ~ax~a ~a universe\n"
        (num-threads)
        (usize)
        (usize)
        (uname))
(flush-output)

(define u (universe-creator (usize)))
(define fps 0.0)
(define FPS-DISPLAY-COLOR "yellow")
(define recent-Δs (make-vector 5 0))
(define Δ-ptr 0)

(define post-callback-sema (make-semaphore 1))

(define (move-Δ-ptr!)
  (if (= Δ-ptr (- (vector-length recent-Δs) 1))
      (set! Δ-ptr 0)
      (set! Δ-ptr (+ Δ-ptr 1))))

(define (save-Δ! Δ)
  (vector-set! recent-Δs Δ-ptr Δ)
  (move-Δ-ptr!))

(define (get-fps)
  (define avg-Δ
    (/ (for/fold ([sum 0]) ([Δ (in-vector recent-Δs)])
         (+ sum Δ))
       (vector-length recent-Δs)))
  (exact->inexact (/ 1000.0 avg-Δ)))

(define (format-fps str) (format "~a FPS" str))
(define bdc (make-object bitmap-dc% (make-bitmap 1 1)))
(define fps-bitmap 
  (let-values ([(w h _1 _2) (send bdc get-text-extent (format-fps "100.000"))])
    (make-bitmap (inexact->exact (ceiling w)) (inexact->exact (ceiling h)))))

(send bdc set-text-foreground FPS-DISPLAY-COLOR)
  
;; draw-it : dc% universe -> void
(define (draw-it dc univ)
  (semaphore-wait (universe-mut univ))
  (send bdc set-bitmap fps-bitmap)
  (send bdc erase)
  (send bdc draw-text (format-fps (real->decimal-string fps)) 0 0)
  (send dc draw-bitmap
        (universe-bmp univ)
        0 0)
  (semaphore-post (universe-mut univ))
  (send dc draw-bitmap fps-bitmap 20 20))

(define f (new frame%
               [label (format "Game of Life ~a x ~a (~a)"
                              (usize) (usize)
                              (case (strategy)
                                [(seq) "Sequential"]
                                [(par) (format "Parallel with ~a threads" (num-threads))]))]
               [width (usize)]
               [height (usize)]))
(define c (new canvas%
               [parent f]
               [min-width (usize)]
               [min-height (usize)]
               [stretchable-width #t]
               [stretchable-height #t]
               [paint-callback 
                (λ (cvs dc) 
                  (draw-it dc u))]))
                            
(send f show #t)
(send f center 'vertical)
  
(define t (current-milliseconds))
(void 
 (thread 
  (λ () 
    (let loop () 
      (step u)
      
      (when (semaphore-try-wait? post-callback-sema)
        (queue-callback 
         (λ () 
           (draw-it (send c get-dc) u)
           (define t′ (current-milliseconds))
           (save-Δ! (- t′ t))
           (set! fps (get-fps))
           (set! t t′)
           (semaphore-post post-callback-sema))))
      
      (loop)))))



