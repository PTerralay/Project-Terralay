(define Thing%
  (class Agent%
    (super-new)
    (inherit-field xpos 
                   ypos 
                   gridx
                   gridy
                   triggerlist)
    (define/public (use) "not implemented yet")
    
    (define/public (getx)
      gridx)
    (define/public (gety)
      gridy)
    
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (get-triggers)
      triggerlist)
    
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))
    (define/public (render)
      "not implemented yet"
      )))