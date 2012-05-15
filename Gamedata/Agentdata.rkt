#lang racket
(require "../Thing.rkt")
(provide (all-defined-out))

(define Screwdriver
  (list
   (cons 'GX 3)
   (cons 'GY 4)
   (cons 'tex-ID 2)
   (cons 'inv-name "Screwdriver")
   (cons 'triggers '())
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'interaction-code (lambda (world self use-with)
                             (when (null? use-with)
                               (send (get-field inventory (get-field player world)) add-thing! self world)
                               (send world draw-text-ingame 'outside_workroom -3 3 0.6 "A screwdriver, maybe I can use this for something" 200))))
   (cons 'placement 'outside_workroom)
   (cons 'state 0)
   (cons 'passable? #t)))

(define Generator1
  (list
   (cons 'GX 4)
   (cons 'GY 11)
   (cons 'tex-ID 0)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Generator1")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (if (null? use-with)
                                 (send world draw-text-ingame 'Engineroom1 -2 11 0.6 "the cable from the generator is broken, seems rodents are to blame...\n" 200)
                                 (when (eqv? (get-field agent-ID use-with) 'Screwdriver)
                                   (begin (send (get-field inventory (get-field player world)) delete-thing! use-with world)
                                          (set-field! state world 1)
                                          (send world draw-text-ingame 'Engineroom1 -2 11 0.6 "there, the cable is fixed, for now...\n" 200))))))
   (cons 'placement 'Engineroom1)
   (cons 'state 0)
   (cons 'passable? #f)))

(define WRDesk
  (list
   (cons 'GX 2)
   (cons 'GY 3)
   (cons 'tex-ID 4)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -45)
   (cons 'inv-name "Desk")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (send world draw-text-ingame 'Workroom -2 -2 0.6 "A mess of jumbled calculations and theorems,\n unintelligble for anyone but you" 300)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define WRChair
  (list
   (cons 'GX 3)
   (cons 'GY 2)
   (cons 'tex-ID 5)
   (cons 'tex-width 32)
   (cons 'tex-height 50)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Chair")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (void)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))

(define Generator2
  (list
   (cons 'GX 3)
   (cons 'GY 11)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -20)
   (cons 'inv-name "Generator2")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (if (null? use-with)
                                 (send world draw-text-ingame 'Engineroom1 -2 11 0.6 "the cable from the generator is broken, seems rodents are to blame...\n" 200)
                                 (when (eqv? (get-field agent-ID use-with) 'Screwdriver)
                                   (begin (send (get-field inventory (get-field player world)) delete-thing! use-with world)
                                          (set-field! state world 1)
                                          (send world draw-text-ingame 'Engineroom1 -2 11 0.6 "there, the cable is fixed, for now...\n" 200))))))
   (cons 'placement 'Engineroom1)
   (cons 'state 0)
   (cons 'passable? #f)))

(define SlidedoorLclosed
  (list
   (cons 'GX 24)
   (cons 'GY 4)
   (cons 'tex-ID 3)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Slidedoor")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                                 (send world draw-text-ingame 'outside_workroom 20 3 0.6 "The elevator door is closed, I need to get the power running.\n I can hear some strange noises behind the door though..." 200)))
   (cons 'placement 'outside_workroom)
   (cons 'state 0)
   (cons 'passable? #f)))

(define Testthing
  (list
   (cons 'GX 21)
   (cons 'GY 3)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Trol")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (display "Don't touch me!")))
   (cons 'placement 'Anotherawesomeroom)
   (cons 'state 0)
   (cons 'passable? #t)))

(define Statebutton
  (list
   (cons 'GX 4)
   (cons 'GY 7)
   (cons 'tex-ID 1)
   (cons 'tex-width 32)
   (cons 'tex-height 32)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y 0)
   (cons 'inv-name "Button")
   (cons 'triggers '())
   (cons 'placement 'Awesomeroom)
   (cons 'interaction-code (lambda (world thing use-with)
                             (if (eq? (get-field masked world) #t)
                                 (set-field! masked world #f)
                                 (set-field! masked world #t))))
   (cons 'state 0)
   (cons 'passable? #f)))

(define WRServer1
  (list
   (cons 'GX 6)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (send world draw-text-ingame 'Workroom 5 -2 0.6 "It's a hitech server, normally running complex calculations" 300)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))
(define WRServer2
  (list
   (cons 'GX 7)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (send world draw-text-ingame 'Workroom 5 -2 0.6 "It's a hitech server, normally running complex calculations" 300)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))
(define WRServer3
  (list
   (cons 'GX 8)
   (cons 'GY 2)
   (cons 'tex-ID 6)
   (cons 'tex-width 32)
   (cons 'tex-height 64)
   (cons 'tex-rel-x 0)
   (cons 'tex-rel-y -32)
   (cons 'inv-name "Server")
   (cons 'triggers '())
   (cons 'interaction-code (lambda (world self use-with)
                             (send world draw-text-ingame 'Workroom 5 -2 0.6 "It's a hitech server, normally running complex calculations" 300)))
   (cons 'placement 'Workroom)
   (cons 'state 0)
   (cons 'passable? #f)
   (cons 'type 'thing)))


(define Character-list
  (list 
   (cons 'Tetsy "Gamedata/testmonster.rkt")
   (cons 'Tetsytoo "Gamedata/testmonster2.rkt")
   (cons 'Eiresmile "Gamedata/Eiresmile.rkt")
   (cons 'Curious "Gamedata/Curious.rkt")))

(define Thing-list
  (list 'Testthing
        'Statebutton
        'Generator1
        'Generator2
        'Screwdriver
        'SlidedoorLclosed
        'WRDesk
        'WRChair
        'WRServer1
        'WRServer2
        'WRServer3))
