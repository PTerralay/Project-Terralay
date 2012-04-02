#lang racket

(require "Map.rkt")

(provide Agent%)

(define Agent%
  (class object%
    (super-new)
    (init-field xpos
                ypos)
    (field (triggerlist '()))
    (define/public (get-x)
      xpos)
    (define/public (get-y)
      ypos)
    (define/public (render)
      "not implemented yet")
    ))