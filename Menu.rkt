#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors
         "drawtext.rkt")

(provide Menu% main-menu-functions get-active-menu Inventory%)

(define Menu%
  (class object%
    (super-new)
    (init-field parent button-functions children)
    
    (define/public (get-parent)
      parent)
    (define/public (get-children)
      children)
    (define/public (set-children! adoptees)
      (set! children adoptees))
    
    (define state -1)
    
    (define/public (set-state! new-state)
      (set! state new-state))
    
    (define/public (get-state)
      state)
    
    (define/public (get-buttons)
      button-functions)
    
    (define/public (menu-action action)
      (case action
        ((up) (if (eq? state 0)
                  (set! state (- (length button-functions) 1))
                  (set! state (- state 1))))
        ((down) (if (eq? state (- (length button-functions) 1))
                    (set! state 0)
                    (set! state (+ state 1))))
        ((enter)  ((cdr (assq 'fn (list-ref button-functions state))) this))
        ((back) (if (is-a? parent Menu%)
                    (begin 
                      (set! state -1)
                      (send parent set-state! 0))
                    (send parent leave-menu!)))))
    
    (define/public (render main-menu texture-list)
      (if (> state -1)
          (let ((render-state 0))
            (for-each (λ (button)
                        (if (eq? render-state state)
                            (glColor4f 0 0.4 0 1)
                            (glColor4f 0.4 0.4 0.4 1))
                        (glBegin GL_TRIANGLE_STRIP)
                        (glVertex2f 0 0)
                        (glVertex2f 250 0)
                        (glVertex2f 0 50)
                        (glVertex2f 250 50)
                        (glEnd)
                        (glColor4f 0.7 0.7 0.7 1)
                        (draw-text 20 10 (cdr (assq 'text button)) texture-list)
                        (glTranslatef 0 60 0)
                        
                        (set! render-state (+ render-state 1)))
                      button-functions))
          
          (send (get-active-menu main-menu) render main-menu texture-list)))))

(define (get-active-menu ancestor)
  (if (> (send ancestor get-state) -1)
      ancestor
      (get-active-menu (findf (λ (menu)
                                (> (send menu get-state) -1))
                              (send ancestor get-children)))))


(define main-menu-functions
  (list (list 
         (cons 'text "Back")
         (cons 'fn (lambda (menu)
                     (send (send menu get-parent) leave-menu!))))
        (list
         (cons 'text "New Game")
         (cons 'fn (lambda (menu)
                     (display "wehoe, new game is running! ... kinda...\n"))))
        (list
         (cons 'text "Load Game")
         (cons 'fn (lambda (menu)
                     (send menu set-state! -1)
                     (send (list-ref (send menu get-children) 0) set-state! 0))))
        (list
         (cons 'text "Save Game")
         (cons 'fn (lambda (menu)
                     (send menu set-state! -1)
                     (send (list-ref (send menu get-children) 1) set-state! 0))))
        (list
         (cons 'text "Help and Options")
         (cons 'fn (lambda (menu)
                     (send menu set-state! -1)
                     (send (list-ref (send menu get-children) 2) set-state! 0))))
        (list
         (cons 'text "Exit")
         (cons 'fn (lambda (menu)
                     (exit))))))

(define Inventory%
  (class object%
    (super-new)
    (init-field width height things)
    (field (grid (make-vector height)))
    
    ; We must use this approach since initializing grid to (make-vector height (make-vector width #f)) creates 
    ; a grid with the _SAME_ object in all the rows, and mutating one will mutate all the others.
    (define/private (gridloop rownum)
      (when (< rownum height)
        (vector-set! grid rownum (make-vector width #f))
        (gridloop (+ rownum 1))))
    (gridloop 0)
     
    (define/public (add-thing! thing)
      (set! things (mcons thing things))
      (update-inventory))
    
    (define/private (update-inventory)
      (let ((number-of-things (mlength things))
            (thingcounter 0))
        (define (yloop rownum)
          (when (< rownum height)
            (define row (vector-ref grid rownum))
            (define (xloop colnum)
              (when (< colnum width)
                (if (< thingcounter number-of-things) 
                    (let ((thing (mlist-ref things thingcounter)))
                      (display thing)
                      (vector-set! row colnum thing)
                      (set! thingcounter (+ thingcounter 1))
                      (display row)
                      (newline))
                    (begin 
                      (vector-set! row colnum #f)))
                (xloop (+ colnum 1))))
            (xloop 0)
            (newline)
            (display row)
            (newline)
            (vector-set! grid rownum row)
            (display grid)
            (yloop (+ rownum 1))))
        (yloop 0)))
    
    
    (define/public (delete-thing! thing)
      (define (delete-iter list)
        (cond ((null? list) (error "Inventory is empty"))
              ((eq? (mcar list) thing)
               (set! list (mcdr list))
               list)
              ((null? (mcdr list)) (error "Thing not found"))
              ((eq? (mcar (mcdr list)) thing)
               (set-mcdr! list (mcdr (mcdr list)))
               list)
              (else (mcons (mcar list) (delete-iter (mcdr list))))))
      (set! things (delete-iter things))
      (update-inventory))
    
    (define/public (get-grid)
      grid)
    (define/public (draw texture-list)
      (glDisable GL_TEXTURE_2D)
      (define (yloop row)
        (define (xloop col)
          (when (< col width)
            (let ((thing (vector-ref (vector-ref grid row) col)))
              
              (glPushMatrix)
              (glTranslatef (* col 40) (* row 40) 0)
              (glColor3f 1 1 0)
              (glBegin GL_TRIANGLE_STRIP)
              (glVertex2i 0 0)
              (glVertex2i 39 0)
              (glVertex2i 0 39)
              (glVertex2i 39 39)
              (glEnd)
              (if (not (eq? thing #f))
                  (begin
                    (display "blubb")
                    (glColor3f 1 0 0)
                    (glBegin GL_TRIANGLE_STRIP)
                    (glVertex2i 0 0)
                    (glVertex2i 39 0)
                    (glVertex2i 0 39)
                    (glVertex2i 39 39)
                    (glEnd)
                    (draw-text 20 20 (symbol->string (get-field agent-ID thing)) texture-list))
                  (void))
              (glPopMatrix))
            (xloop (+ col 1))))
        (when (< row height)
          (xloop 0)
          (yloop (+ row 1))))
      (yloop 0)
      (glEnable GL_TEXTURE_2D))))




(define (show-inventory player)
  (display "Inventory:\n")
  (if (null? (send player get-inventory))
      (display "nothing\n")
      (begin (display (send (mcar (send player get-inventory)) name))
             (mfor-each (lambda (thing)
                          (display ", ")
                          (display (send thing name))
                          (newline))
                        (mcdr (send player get-inventory)))
             (newline))))