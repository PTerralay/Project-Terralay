#lang racket/gui
(define Player%
  (class object%
    (init-field xpos
                ypos)
    (define/public (move direction) "not implementet yet")
    (define/public (render) "not implemented yet")))