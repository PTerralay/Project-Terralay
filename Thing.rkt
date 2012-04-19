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
                   agent-ID
                   place
                   world)
    (init-field interaction)
    
    (define/public (get-xpos)
      xpos)
    (define/public (get-ypos)
      ypos)
    
    (define/public (interact)
      (interaction world this))
    
    (define/public (get-place)
      place)
    
    (define/public (get-triggers)
      triggerlist)
    
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))))

(define (Load-things thing-list the-world)
  (define (createloop thing-name)
    (let* ((data (dynamic-require "Gamedata/Agentdata.rkt" thing-name)))
      (new Thing%
           (gridx (cdr (assq 'GX data)))
           (gridy (cdr (assq 'GY data)))
           (triggerlist (cdr (assq 'triggers data)))
           (interaction (cdr (assq 'interaction-code data)))
           (world the-world)
           (agent-ID thing-name)
           (place (cdr (assq 'placement data))))))
  
  (if (null? thing-list)
      '()
      (mcons (createloop (car thing-list)) (Load-things (cdr thing-list) the-world))))