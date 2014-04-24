#lang racket/base
(require racket/draw
         racket/match
         (only-in racket/class send))
(provide (struct-out universe)
         write-universe-bitmap!
         swap-buffers!
         index-of
         alive?
         die!
         spawn!
         update-cell!
         all-dead-except?
         toroidal+
         toroidal-
         ALIVE
         DEAD
         DEFAULT-ALPHA
         empty-universe
         random-universe
         block-universe
         beehive-universe
         toad-universe
         acorn-universe
         glider-gun-universe)

(struct universe (size [write-buf #:mutable] 
                       [read-buf #:mutable] 
                       bmp
                       mut
                       [win-origin-r #:mutable]
                       [win-origin-c #:mutable]
                       [win-width #:mutable]
                       [win-height #:mutable]))

(define ALIVE 255)
(define DEAD 0)
(define DEFAULT-ALPHA 255)

(define ALIVE-COLOR-VALUES `(,ALIVE 0 0 255))
(define DEAD-COLOR-VALUES `(,DEAD 0 0 255))

(define (write-universe-bitmap! univ)
  (define sz (universe-size univ))
  (send (universe-bmp univ)
        set-argb-pixels
        0 0 sz sz
        (universe-write-buf univ)))

(define (swap-buffers! univ)
  (semaphore-wait (universe-mut univ))
  (define prev-write (universe-write-buf univ))
  (set-universe-write-buf! univ (universe-read-buf univ))
  (set-universe-read-buf! univ prev-write)
  (semaphore-post (universe-mut univ)))

;; the index is the cell's 'r' value in the byte array
;; (so alive is red, dead is black)
(define (index-of univ r c)
  (+ (* 4 (+ (* r (universe-size univ)) c)) 1))

(define (index->row+col univ ind)
  (values 
   (quotient ind (universe-size univ))
   (/ (- (remainder ind (universe-size univ)) 1) 4)))

(define-syntax-rule (alive? univ r c)
  (= ALIVE (bytes-ref (universe-read-buf univ) (index-of univ r c))))

(define (die! univ r c)
  (bytes-set! (universe-write-buf univ) (index-of univ r c) DEAD))

(define (spawn! univ r c)
  (bytes-set! (universe-write-buf univ) (index-of univ r c) ALIVE))

;; update-cell! : universe uint uint symbol -> bool
(define (update-cell! univ r c state)
  (case state
    [(alive) (spawn! univ r c) #t]
    [(dead) (die! univ r c) #f]))

(define (all-dead-except? univ . alives)
  (let loop ([alives alives])
    (cond 
      [(null? alives) #t]
      [else 
       (define a (car alives))
       (if (not (alive? univ (car a) (cdr a)))
           #f
           (loop (cdr alives)))])))

(define (toroidal+ univ a b)
  (define d (+ a b))
  (define sz (universe-size univ))
  (if (d . >= . sz)
      (- d sz)
      d))   

(define (toroidal- univ a b)
  (define d (- a b))
  (define sz (universe-size univ))
  (if (d . < . 0)
      (+ sz d)
      d))

(define (empty-universe width)
  (define buf (make-bytes (* width width 4) DEAD))
  (for ([i (in-range 0 (* (* width width) 4) 4)])
    (bytes-set! buf i DEFAULT-ALPHA))
  (universe width 
            buf
            (bytes-copy buf) 
            (make-bitmap width width) 
            (make-semaphore 1)
            0 0
            width width))

(define (random-universe width)
  (define u (empty-universe width))
  (for* ([r (in-range 0 width)]
         [c (in-range 0 width)])
    (when (zero? (random 2))
      (spawn! u r c)))
  (swap-buffers! u)
  u)

(define (draw-pattern-at u start . points)
  (match-define `(,start-row . ,start-col) start)
  (for ([p (in-list points)])
    (spawn! u (+ start-row (car p)) (+ start-col (cdr p))))
  (swap-buffers! u)
  u)

(define (block-universe size [origin (cons 0 0)])
  (draw-pattern-at 
   (empty-universe size)
   origin
   (cons 1 1)
   (cons 1 2)
   (cons 2 1)
   (cons 2 2)))

(define (beehive-universe size [origin (cons 0 0)])
  (draw-pattern-at 
   (empty-universe size)
   origin
   (cons 1 2)
   (cons 1 3)
   (cons 2 1)
   (cons 2 4)
   (cons 3 2)
   (cons 3 3)))

(define (toad-universe size [origin (cons 0 0)])
  (draw-pattern-at
   (empty-universe size)
   origin
   (cons 2 2)
   (cons 2 3)
   (cons 2 4)
   (cons 3 1)
   (cons 3 2)
   (cons 3 3)))

(define (acorn-universe size [origin (cons 0 0)])
  (draw-pattern-at 
   (empty-universe size)
   origin
   (cons 1 2)
   (cons 2 4)
   (cons 3 1)
   (cons 3 2)
   (cons 3 5)
   (cons 3 6)
   (cons 3 7)))

(define (glider-gun-universe size [origin (cons 0 0)])
  (draw-pattern-at
   (empty-universe size)
   origin
   (cons 2 25)
   (cons 3 23)
   (cons 3 25)
   (cons 4 13)
   (cons 4 14)
   (cons 4 21)
   (cons 4 22)
   (cons 4 35)
   (cons 4 36)
   (cons 5 12)
   (cons 5 16)
   (cons 5 21)
   (cons 5 22)
   (cons 5 35)
   (cons 5 36)
   (cons 6 1)
   (cons 6 2)
   (cons 6 11)
   (cons 6 17)
   (cons 6 21)
   (cons 6 22)
   (cons 7 1)
   (cons 7 2)
   (cons 7 11)
   (cons 7 15)
   (cons 7 17)
   (cons 7 18)
   (cons 7 23)
   (cons 7 25)
   (cons 8 11)
   (cons 8 17)
   (cons 8 25)
   (cons 9 12)
   (cons 9 16)
   (cons 10 13)
   (cons 10 14)))



;Tests
(module+ test
  (require rackunit)
  (define tu (empty-universe 100))
  (check-equal? (index-of tu 0 0) 1)
  (check-equal? (index-of tu 1 0) 401)
  (check-equal? (index-of tu 1 1) 405)
  (check-equal? (index-of tu 1 2) 409)
  
  
  
  (define u (empty-universe 20))
  (for* ([r (in-range 20)]
         [c (in-range 20)])
    (check-true (not (alive? u r c))))
  
  (define au (acorn-universe 24))
  (define (acorn-check u)
    (check-true (alive? au 1 2))
    (check-true (alive? au 2 4))
    (check-true (alive? au 3 1))
    (check-true (alive? au 3 2))
    (check-true (alive? au 3 5))
    (check-true (alive? au 3 6))
    (check-true (alive? au 3 7)))
  
  (acorn-check au)
  (swap-buffers! au)
  (define added 
    (for/list ([i (in-range (universe-size au))])
      (define cell (cons (random (universe-size au)) (random (universe-size au))))
      (spawn! au (car cell) (cdr cell))
      cell))
  (swap-buffers! au)
  (acorn-check au)
  (for ([cell (in-list added)])
    (check-true (alive? au (car cell) (cdr cell))))
  
  (check-equal? (toroidal+ au 10 5) 15)
  (check-equal? (toroidal+ au 23 1) 0)
  (check-equal? (toroidal+ au 23 4) 3)
  
  (check-equal? (toroidal- au 0 3) 21)
  (check-equal? (toroidal- au 20 20) 0)
  (check-equal? (toroidal- au 4 8) 20))







    