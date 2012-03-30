(define World%
  (class object%
    (super-new)
    (field (maplist '()))
    (define/public (pubichair) 2)
    (define/private (privatechair) 3)))

(define Tile%
  (class object%
    (super-new)
    ))

(define Map%
  (class object%
    (super-new)
    (init-field sizex)
    (init-field sizey)
    (field (agentlist (mcons '() '()))
           (tiles (make-vector sizex (make-vector sizey (new Tile%)))))
    (define/public (render)"not implemented yet!")
    (define/public (gettile gridx gridy) 
      (vector-ref (vector-ref tiles gridx) gridy))
    ))
    
(define Agent%
  (class object%
    (super-new)
    (field (name "horsie"))
    (define/public (getname) name)))

(define Thing%
  (class Agent%
    (super-new)
    (define/override (getname) (display name) (display (super getname)))))