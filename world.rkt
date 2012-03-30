(define World%
  (class object%
    (super-new)
    (init-field maplist
                current-map
                state)
    (field (tilegraphics '()))
    (define/public (render)
      "Not implemented yet")))

(define Tile%
  (class object%
    (super-new)
    (init-field triggerlist
                tilebackground
                passable)
    (define/public (render)
      "not implemented yet")
    ))

(define Map%
  (class object%
    (super-new)
    (init-field sizex
                sizey)
    (field (agentlist (mcons '() '()))
           (tiles (make-vector sizex (make-vector sizey (new Tile% (triggerlist '()) (tilebackground #f) (passable #f))))))
    
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tiles gridx) gridy))
    (define/public (render) 
      "not implemented yet!")))

(define Agent%
  (class object%
    (super-new)
    (init-field xpos
                ypos
                triggerlist)))

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

(define Character%
  (class Agent%
    (super-new)
    (define/public (talk-to) "not implemented yet")
    (define/private (move!) "not implemented yet" )
    (define/public (render)
      "not implemented yet")))

(define Player%
  (class object%
    (init-field xpos
                ypos)
    (define/public (move direction) "not implementet yet")
    (define/public (render) "not implemented yet")))