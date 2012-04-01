(define Thing%
  (class Agent%
    (super-new)
    (inherit-field xpos ypos triggerlist)
    (define/public (use) "not implemented yet")
    (define/public (setx newx)
      (set! xpos newx))
    (define/public (getx)
      xpos)
    (define/public (render)
      "not implemented yet")))