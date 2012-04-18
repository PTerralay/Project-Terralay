#lang racket

(provide Testthing Statebutton)

(define Testthing
  (list
   (cons 'X 672)
   (cons 'Y 96)
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world thing) 
                            (display "Don't touch me!")))))

(define Statebutton
  (list
   (cons 'GX 11)
   (cons 'GY 9)
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world thing)
                             (when (eq? (send world get-state) 0)
                               (send world set-state! 1)
                               (display "Button pressed, a door opens"))))))