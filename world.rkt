#lang racket
(require sgl/gl sgl/gl-vectors "Map.rkt")
(provide World%)

(define World%
  (class object%
    (super-new)
    (init-field maplist
                current-map
                state)
    (field (tilegraphics '()))
    
    
    (define/public (get-current-map)
      current-map)))










