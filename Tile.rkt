#lang racket

(provide Tile%)


(define Tile%
  (class object%
    (super-new)
    (init-field type)
    (define triggerlist '())
    (define tilebackground "fucked if i know")
    (define passable #t)
    (define/public (render)
      "not implemented yet")
    (define/public (get-type)
      type)
    (set! passable
          (case type
            ((#\r #\l #\t #\b #\h) #t)
            (else #f)))))