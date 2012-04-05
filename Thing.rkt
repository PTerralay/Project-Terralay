#lang racket

(require "Agent.rkt")
(provide Thing%)

(define Thing%
  (class Agent%
    (super-new)
    (inherit-field xpos 
                   ypos 
                   gridx
                   gridy
                   triggerlist)
    
    (init-field name)
    (define/public (use) "not implemented yet")
    
    (define/public (getx)
      gridx)
    (define/public (gety)
      gridy)
    
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (get-triggers)
      triggerlist)
    
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))
    (define/override (render)
      "not implemented yet"
      )))