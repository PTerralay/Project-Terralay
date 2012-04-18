#lang racket

(provide Testthing Character-list)

(define Testthing
  (list
   (cons 'X 672)
   (cons 'Y 96)
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'triggers '())
   (cons 'interaction-code (lambda () 
                             (display "Don't touch me!")))
   (cons 'placement 'Awesomeroom)))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")))