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
                world
                agent-ID)
    (define/public (getx)
      gridx)
    (define/public (gety)
      gridy)
    (define/public (getname)
      agent-ID)
    (define/public (render)
      "not implemented yet")
    ))