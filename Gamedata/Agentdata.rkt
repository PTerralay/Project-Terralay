#lang racket
(require "../Thing.rkt")
(provide Testthing
         Character-list
         Statebutton
         Thing-list
         Generator
         Screwdriver)

(define Screwdriver
  (list
   (cons 'GX 3)
   (cons 'GY 4)
   (cons 'tex-ID 2)
   (cons 'inv-name "Screwdriver")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (when (null? use-with)
                               (send (get-field inventory (get-field player world)) add-thing! self world))))
   (cons 'placement 'outside_workroom)
   (cons 'state 0)
   (cons 'type 'thing)))

(define Generator
  (list
   (cons 'GX 4)
   (cons 'GY 12)
   (cons 'tex-ID 0)
   (cons 'inv-name "Generator")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (if (null? use-with)
                                 (display "the cable from the generator is broken, seems rodents are to blame...\n")
                                 (when (eqv? (get-field inv-name use-with) "Screwdriver")
                                   (begin (send (get-field inventory (get-field player world)) delete-thing! use-with world)
                                          (set-field! state world 1)
                                          (display "there, the cable is fixed, for now...\n"))))))
   (cons 'placement 'Engineroom1)
   (cons 'state 0)
   (cons 'type 'thing)))

(define Testthing
  (list
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'tex-ID 1)
   (cons 'inv-name "Trol")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (display "Don't touch me!")))
   (cons 'placement 'Anotherawesomeroom)
   (cons 'state 0)
   (cons 'type 'thing)))

(define Statebutton
  (list
   (cons 'GX 11)
   (cons 'GY 9)
   (cons 'tex-ID 1)
   (cons 'inv-name "Button")
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code (lambda (world thing use-with)
                             (if (eq? (get-field place thing) 'Inventory)
                                 (void)
                                 (begin (send world set-state! 1)
                                        (send (get-field inventory (get-field player world)) add-thing! thing)
                                        (send thing set-place! 'Inventory)))))
   (cons 'state 0)
   (cons 'type 'thing)))

(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")
   (cons 'Eiresmile "Gamedata/Eiresmile.rkt")))

(define Thing-list
  (list 'Testthing
        'Statebutton
        'Generator
        'Screwdriver))