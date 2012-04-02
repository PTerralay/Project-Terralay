#lang racket/gui
(provide Player%)

(define Player%
  (class object%
    (init-field xpos
                ypos
                gridx
                gridy)

    (super-new)
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    (define/public (move! direction) 
      (case direction
        ((up) (when (send (send (send Trollworld get-current-map) gettile gridx (- gridy 1)) passable?) 
                (set! gridy (- gridy 1)))) 
        ((down) (when (send (send (send Trollworld get-current-map) gettile gridx (+ gridy 1)) passable?) 
                  (set! gridy (+ gridy 1))))
        ((left) (when (send (send (send Trollworld get-current-map) gettile (- gridx 1) gridy) passable?) 
                  (set! gridx (- gridx 1))))
        ((right) (when (send (send (send Trollworld get-current-map) gettile (+ gridx 1) gridy) passable?) 
                   (set! gridx (+ gridx 1))))))
    (define/public (render) "not implemented yet")
    (define/public (gety) gridy)
    (define/public (getx) gridx)))

    
    
