#lang racket

(provide World%)

(define World%
  (class object%
    (super-new)
    (init-field maplist
                current-map
                state)
    (field (tilegraphics '()))
    (define/public (render)
      "Not implemented yet")))










