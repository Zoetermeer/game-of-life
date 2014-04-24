#lang racket/base
(require future-visualizer
         "step-core.rkt")

(define u (glider-gun-universe 64))
(visualize-futures
 (step u))