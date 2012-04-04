#lang racket
(require sgl/gl sgl/gl-vectors  "Player.rkt")
(provide World%)

(define World%
  (class object%
    (super-new)
    
    (init-field maplist
                current-map
                state
                canvas)
    
    (define player (instantiate Player% (32 32 1 1 'up this canvas 8)))
    
    (field (tilegraphics '()))
    (define/public (get-current-state)
      state)
    (define/public (set-state! new-state)
      (set! state new-state))
    (define/public (get-player)
      player)
    
    (define/public (set-current-map! arg)
      (if (eq? arg 'first)
          (set! current-map (car maplist))
          (set! current-map arg)))
    
    (define/public (add-map! new-map)
      (set! maplist (cons new-map maplist)))
    (define/public (get-current-map)
      current-map)))










