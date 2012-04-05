#lang racket

(require "Agent.rkt")
(provide Thing% Load-things)

(define Thing%
  (class Agent%
    (super-new)
    (inherit-field xpos
                   ypos 
                   gridx
                   gridy
                   triggerlist
                   world)
    (init-field interaction)
    
    (init-field name)
    
    (define/public (getx)
      gridx)
    (define/public (gety)
      gridy)
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (interact)
      (interaction))
    
    (define/public (get-triggers)
      triggerlist)
    
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))))

(define (Load-things thing-list the-world)
  (define (createloop thing-name)
    (let* ((data (dynamic-require "Gamedata/thingdata.rkt" thing-name)))
      (new Thing%
           (xpos (cdr (assq 'X data)))
           (ypos (cdr (assq 'Y data)))
           (gridx (cdr (assq 'GX data)))
           (gridy (cdr (assq 'GY data)))
           (triggerlist (cdr (assq 'triggers data)))
           (interaction (cdr (assq 'interaction-code data)))
           (world the-world)
           (name thing-name))))
  
  (if (null? thing-list)
      '()
      (mcons (createloop (car thing-list)) (Load-things (cdr thing-list) the-world))))