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
