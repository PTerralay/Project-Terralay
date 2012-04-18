#lang racket

(provide Agent%)

(define Agent%
  (class object%
    (super-new)
    (init-field gridx
                gridy
                triggerlist
                world
                agent-ID)
    (field (xpos (* gridx 32))
           (ypos (* gridy 32)))
    (define/public (getx)
      gridx)
    (define/public (gety)
      gridy)
    (define/public (getname)
      agent-ID)
    (define/public (render)
      "not implemented yet")
    ))