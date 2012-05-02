#lang racket
(require sgl/gl sgl/gl-vectors racket/mpair racket/gui "Map.rkt" "Player.rkt" "Menu.rkt" "Character.rkt" "Inventory.rkt")

(provide World%)

(define World%
  (class object%
    (super-new)
    
    (init-field maplist
                current-map
                state
                canvas)
    
    (define player (instantiate Player% (32 32 1 1 'right this canvas 8 (new Inventory% (things '()) (width 5)  (height 3)))))
    
    (field (tilegraphics '())
           (chars '())
           (Things '()))
    
    (define/public (get-chars . arg)
      (cond 
        ((null? arg) (let ((result '()))
                       (mfor-each (λ (pair)
                                    (set! result (mcons (mcdr pair) result)))
                                  chars)
                       result))
        ((eq? arg 'with-place) chars)))
    
    (define/public (get-state)
      state)
    
    (define/public (get-agents . arg)
      (cond 
        ((null? arg) (mappend (get-things) (get-chars)))
        ((eq? arg 'with-place) 
         (mappend (get-things 'with-place) (get-chars 'with-place)))))
    
    (define/public (set-state! new-state)
      (set! state new-state))
    
    (define/public (get-player)
      player)
    
    (define/public (get-maps)
      maplist)
    
    (define/public (set-current-map! arg)
      (if (eq? arg 'first)
          (set! current-map (car maplist))
          (begin (display (findf (lambda (map)
                                   (display "world.rkt set-current-map!")
                                   (display map)
                                   (display (get-field mapID map))
                                   (display "#\n")
                                   (eqv? (get-field mapID map) arg))
                                 maplist))
                 (when (findf (lambda (map)
                                (eqv? (get-field mapID map)  arg))
                              maplist)
                   (set! current-map (findf (lambda (map)
                                              (display "set current-map to")
                                              (display map)
                                              (display "#\n")
                                              (eqv? (get-field mapID map) arg))
                                            maplist)))))
      (add-neighbours! current-map))
    
    (define/public (add-map! new-map)
      (set! maplist (cons new-map maplist)))
    
    (define/public (add-things! thing-list)
      (mfor-each (lambda (thing)
                   (set! Things (mcons thing Things)))
                 thing-list))
    
    (define/public (get-things . arg)
      (cond 
        ((null? arg) Things)
        ((eq? arg 'with-place)
         (let ((result '()))
           (mfor-each (λ (thing)
                        (set! result (mcons (send thing getplace) thing)))
                      Things)
           result))))
    
    (define/public (get-current-map)
      current-map)
    
    (define/public (add-neighbours! map)
      (display "added neighbours;")
      (for-each (lambda (element)
                  (display (car element))
                  (add-map! (load&create-map (car element) (cadr element) this)))
                (get-field neighbours map)))
    
    (define/public (map-change! mapname door-exit-x door-exit-y exit-dir)
      (send player set-pos! door-exit-x door-exit-y)
      (send player set-dir! exit-dir)
      (let ((charlist (let ((result '()))
                        (mfor-each 
                         (lambda (pair)
                           (set! result (mcons (mcdr pair) result)))
                         chars)
                        result)))
        (mfor-each 
         (lambda (char)
           (when (send char chasing?)
             (new timer% 
                  [notify-callback 
                   (lambda ()
                     (when (eqv? (get-field mapID current-map) mapname)
                       (send char set-pos door-exit-x door-exit-y)
                       (send char setplace! mapname)))]
                  [interval (+ (sqr (- (send player getx) (send char getx)))
                               (sqr (- (send player gety) (send char gety)))
                               1000)]
                  [just-once? #t])))
         charlist))
      (set-current-map! mapname))
    
    
    (define/public (character-load datalist)
      
      (define (load-loop charlist)
        (if (null? charlist)
            '()
            (let ((new-char (new Character%
                                 (gridx (dynamic-require (cdar charlist) 'GX))
                                 (gridy (dynamic-require (cdar charlist) 'GY))
                                 (triggerlist (dynamic-require (cdar charlist) 'triggers))
                                 (AI-update (dynamic-require (cdar charlist) 'AI))
                                 (interaction (dynamic-require (cdar charlist) 'interact-code))
                                 (agent-ID (dynamic-require (cdar charlist) 'ID))
                                 (world this)
                                 (place (dynamic-require (cdar charlist) 'placement)))))
              
              (mcons (mcons (send new-char getplace) new-char)
                     (load-loop (cdr charlist))))))
      (set! chars (load-loop datalist)))))
