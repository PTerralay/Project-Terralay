#lang racket
(require sgl/gl sgl/gl-vectors "Map.rkt" "Player.rkt")
(provide World%)

(define World%
  (class object%
    (super-new)
    
    
    
    (init-field maplist
                current-map
                state
                canvas)
    
    (define player (instantiate Player% (32 32 1 1 'up this canvas)))
    
    (field (tilegraphics '()))
    
    (define/public (get-player)
      player)
    (define/public (get-current-map)
      current-map)))










