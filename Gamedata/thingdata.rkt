#lang racket

(provide Testthing)

(define Testthing
  (list
   (cons 'X 672)
   (cons 'Y 96)
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda () 
                            (display "Don't touch me!")))))