#lang racket
(require sgl/gl sgl/gl-vectors racket/mpair racket/gui "Map.rkt" "Player.rkt" "Character.rkt")
(provide World%)

(define World%
  (class object%
    (super-new)
    
    (init-field maplist
                current-map
                state
                canvas)
    
    (define player (instantiate Player% (32 32 1 1 'up this canvas 8)))
    
    (field (tilegraphics '())
           (chars '()))
    
    (define/public (get-chars)
      chars)
    
    (define/public (get-state)
      state)
    
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
                                   (display (send map get-name))
                                   (display "#\n")
                                   (eqv? (send map get-name) arg))
                                 maplist))
                 (when (findf (lambda (map)
                                (eqv? (send map get-name) arg))
                              maplist)
                   (set! current-map (findf (lambda (map)
                                              (display "set current-map to")
                                              (display map)
                                              (display "#\n")
                                              (eqv? (send map get-name) arg))
                                            maplist)))))
      (add-neighbours! current-map))
    
    (define/public (add-map! new-map)
      (set! maplist (cons new-map maplist)))
    
    (define/public (get-current-map)
      current-map)
    
    (define/public (add-neighbours! map)
      (display "added neighbours;")
      (for-each (lambda (element)
                  (display (car element))
                  (add-map! (load&create-map (car element) (cadr element) this)))
                (send map get-neighbours)))
    
    (define/public (map-change! mapname door-exit-x door-exit-y exit-dir)
      (send player set-pos! door-exit-x door-exit-y)
      (send player set-dir! exit-dir)
      (unless (null? (send current-map get-agents))
        (let ((charlist (send (get-current-map) get-characters)))
          (mfor-each 
           (lambda (char)
             (if (send char chasing?)
                 (new timer% 
                      [notify-callback 
                       (lambda ()
                         (send char set-pos door-exit-x door-exit-y)
                         (send char setplace! mapname)
                         (send (findf (lambda (map)
                                        (eqv? (send map get-name) mapname)) 
                                      maplist) add-char! char))]
                      [interval 800]
                      [just-once? #t])
                 (send current-map delete-character! (send char getname) (send current-map get-characters))))
           charlist)))
      (set-current-map! mapname))
    
    
    (define/public (character-load)
      (let ((datalist (dynamic-require "Gamedata/Agentdata.rkt" 'Character-list)))
        
        (define (load-loop datafile charlist)
          (let ((new-char (new Character%
                               (gridx (dynamic-require datafile 'GX))
                               (gridy (dynamic-require datafile 'GY))
                               (triggerlist (dynamic-require datafile 'triggers))
                               (AI-update (dynamic-require datafile 'AI))
                               (interaction (dynamic-require datafile 'interact-code))
                               (agent-ID (dynamic-require datafile 'ID))
                               (world this)
                               (place (dynamic-require datafile 'placement)))))
            (if (null? charlist)
                '()
                (mcons (mcons (send new-char getplace) new-char)
                       (load-loop (cdar charlist) (cdr charlist))))))
        (set! chars (load-loop (cdar datalist) (cdr datalist)))))))
