#lang racket/gui

(provide Player%)

(define Player%
  (class object%
    (init-field xpos
                ypos
                gridx
                gridy
                dir
                world
                glcanvas)
    (super-new)
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (get-dir)
      dir)
    (define/public (move! direction) 
      (case direction
        ((up) (when (send (send (send world get-current-map) gettile gridx (- gridy 1)) passable?) 
                (set! gridy (- gridy 1))
                (set! ypos (- ypos 32)))) 
        ((down) (when (send (send (send world get-current-map) gettile gridx (+ gridy 1)) passable?) 
                  (set! gridy (+ gridy 1))
                  (set! ypos (+ ypos 32))))
        ((left) (when (send (send (send world get-current-map) gettile (- gridx 1) gridy) passable?) 
                  (set! gridx (- gridx 1))
                  (set! xpos (- xpos 32))))
        ((right) (when (send (send (send world get-current-map) gettile (+ gridx 1) gridy) passable?) 
                   (set! gridx (+ gridx 1))
                   (set! xpos (+ xpos 32))))))
    (define/public (render) "not implemented yet")
    
    (define/public (gety)
      gridy)
    (define/public (getx)
      gridx)
    
    (define/public (update! ticks last-moved)
      ;------- Movement ---------
      (let ((keys (send glcanvas get-keys))
            (last-key (send glcanvas get-last-key))
            (left 0)
            (right 1)
            (up 2)
            (down 3)
            (facing dir))
        
        (cond ((and (vector-ref keys left) 
                    (not (vector-ref keys right))
                    (not (vector-ref keys up))
                    (not (vector-ref keys down)))
               (set! dir 'left))
              
              ((and (not (vector-ref keys left))
                    (vector-ref keys right)
                    (not (vector-ref keys up))
                    (not (vector-ref keys down)))
               (set! dir 'right))
              
              ((and (not (vector-ref keys left))
                    (not (vector-ref keys right))
                    (vector-ref keys up)
                    (not (vector-ref keys down)))
               (set! dir 'up))
              
              ((and (not (vector-ref keys left))
                    (not (vector-ref keys right))
                    (not (vector-ref keys up))
                    (vector-ref keys down))
               (set! dir 'down)))          
        
        (case last-key
          ((left) (when (vector-ref keys left)
                    (set! dir 'left)))
          ((right) (when (vector-ref keys right)
                     (set! dir 'right)))
          ((up) (when (vector-ref keys up)
                  (set! dir 'up)))
          ((down) (when (vector-ref keys down)
                    (set! dir 'down))))
        
        
        (when (and (or (vector-ref keys left) 
                       (vector-ref keys right) 
                       (vector-ref keys up) 
                       (vector-ref keys down))
                   (eq? dir facing)
                   (> ticks (+ (unbox last-moved) 20)))
          (move! dir)
          (set-box! last-moved ticks)))
      ;-------- Check the tile triggers ------
      (let ((tile (send (send world get-current-map) gettile gridx gridy)))
        
        (for-each (lambda (trigger)
                    (send trigger poll&act tile world))
                  (send tile get-triggers)))
      )
    ))
