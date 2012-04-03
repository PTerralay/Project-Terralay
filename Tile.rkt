#lang racket

(provide Tile%)


(define Tile%
  (class object%
    (super-new)
    (init-field type
                x
                y)
    
    (define triggerlist '())
    
    (define/public (get-triggers)
      triggerlist)
    
    (define/public (add-trigger! trigger)
      (set! triggerlist (cons trigger triggerlist)))
    
    (define tilebackground "fucked if i know")
    (define passable #t)
    (define/public (render)
      "not implemented yet")
    
    (define/public (get-x)
      x)
    (define/public (get-y)
      y)
    (define/public (passable?)
      passable)
    (define/public (get-type)
      type)
    (set! passable
          (case type
            ((#\r #\l #\t #\b #\h) #f)
            (else #t)))))