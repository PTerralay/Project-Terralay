#lang racket

(provide Agent%)

(define Agent%
  (class object%
    (super-new)
    (init-field xpos
                ypos
                gridx
                gridy
                triggerlist
                world)
    (define/public (get-x)
      gridx)
    (define/public (get-y)
      gridy)
    (define/public (render)
      "not implemented yet")
    ))