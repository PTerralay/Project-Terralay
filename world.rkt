#lang racket
(require sgl/gl sgl/gl-vectors racket/mpair "Map.rkt" "Player.rkt")
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
          (begin (display (findf (lambda (map)
                                   (eqv? (send map get-name) arg))
                                 maplist))
                 (when (not (eq? #f (findf (lambda (map)
                                             (eqv? (send map get-name) arg))
                                           maplist)))
                   (set! current-map (findf (lambda (map)
                                              (eqv? (send map get-name) arg))
                                            maplist)))
                 (display current-map)))
      (add-neighbours! current-map))
    
    (define/public (add-map! new-map)
      (set! maplist (cons new-map maplist)))
    
    (define/public (get-current-map)
      current-map)
    
    (define/public (add-neighbours! map)
      (for-each (lambda (element)
                  (display element)
                  (load&create-map (car element) (cadr element) this))
                (send map get-neighbours)))))