#lang racket

(provide AI)

(define (AI monster ticks last-moved world)
  (when (> ticks (+ (unbox last-moved) 20))
    
    (define (get-passable-neighbours x-pos y-pos steps)
      (let* ((left-tile (send (send world get-current-map) gettile (- x-pos 1) y-pos))
             (right-tile (send (send world get-current-map) gettile (+ x-pos 1) y-pos))
             (up-tile (send (send world get-current-map) gettile x-pos (- y-pos 1)))
             (down-tile (send (send world get-current-map) gettile x-pos (+ y-pos 1))))
        (list
         (when (send left-tile passable?)
           (cons steps (cons (cons (- x-pos 1) y-pos) 'left)))
         
         (when (send right-tile passable?)
           (cons steps (cons (cons (+ x-pos 1) y-pos) 'right)))
         
         (when (send up-tile passable?)
           (cons steps (cons (cons x-pos (- y-pos 1)) 'up)))
         
         (when (send down-tile passable?)
           (cons steps (cons (cons x-pos (+ y-pos 1)) 'down))))))
    
    
    ; är vi på samma plats som player?
    
    (define (character-found? xpos ypos)
      (= 0 (+ (- (send (send world get-player) getx) xpos)
              (- (send (send world get-player) gety) ypos))))
    
    ;checks if the tile we're currently on has been stepped on in this path.
    (define (stepped-on? structure path)
      (not (or (for-each (lambda (comparison)
                           (equal? structure comparison)) path))))
    
    
    ; getting to know our surroundings
    
    (define (get-position-of-tile cons-struktur)
      (cons (caadr cons-struktur)
            (cdadr cons-struktur)))
    
    ;;the real deal
    (define (find-path)
      (let ((path-list '()))
        (define (finderloop structure path current-steps)
          (cond
            ((null? structure) #f)
            
            ((stepped-on? structure path) #f)
            
            ((character-found?
              (car (get-position-of-tile structure)) 
              (cdr (get-position-of-tile structure)))
             (set! path (cons #t path))
             (set! path-list (cons path path-list)))
            
            (else
             (for-each (lambda (element)
                         (finderloop element (cons element path) (+ current-steps 1))) 
                       (get-passable-neighbours
                        (car (get-position-of-tile structure)) 
                        (cdr (get-position-of-tile structure))
                        current-steps)))))
        path-list
        
        (let ((check '()))
          (for-each (lambda (steps)
                      (set! check (cons steps check)))
                    (finderloop (cons 0 (cons (cons (send monster getx)
                                                    (send monster gety))
                                              'start-tile)) '() 0))
          (assq (min check) path-list))))
    
    (send monster move! (cddr (reverse (find-path))))
    (set-box! last-moved ticks)))