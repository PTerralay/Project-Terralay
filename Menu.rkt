#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors
         "drawtext.rkt")

(provide Menu% get-active-menu)

;------------------------------------------------------------------------------
;Class: Menu%
;Description: A Menu% object is a menu with buttons that the player can operate with the arrows.
; It can have any number of Menu% objects as children.
;------------------------------------------------------------------------------
(define Menu%
  (class object%
    (super-new)
    (init-field parent title button-functions children world)
    
    (field (state -1))
    
    ;------------------------------------------------------------------------------
    ;menu-action: Reacts to user input and changes the states of the buttons accordingly
    ;params:
    ; action - the action to be taken
    ;------------------------------------------------------------------------------
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
    
    ;------------------------------------------------------------------------------
    ;render: draws this menu on the screen if it's the active one, otherwise it will
    ; try to find another menu that is active and draw that one instead.
    ;params: 
    ; main-menu - the main menu, top level in the menu hiearchy
    ; texture-list - the global texture list passed on by the caller
    ;------------------------------------------------------------------------------
    (define/public (render main-menu text-texture-list)
      (if (> state -1)
          (let ((render-state 0))
            (glColor4f 1 1 1 1)
            (draw-text 0 0 3 title text-texture-list)
            (glTranslatef 0 100 0)
            (for-each (λ (button)
                        (if (eq? render-state state)
                            (glColor4f 0.3 0.4 0.3 1)
                            (glColor4f 0.5 0.5 0.45 1))
                        (glBegin GL_TRIANGLE_STRIP)
                        (glVertex2f 0 0)
                        (glVertex2f 250 0)
                        (glVertex2f 0 50)
                        (glVertex2f 250 50)
                        (glEnd)
                        (glColor4f 0.8 0.8 0.8 1)
                        (draw-text 20 10 1 (cdr (assq 'text button)) text-texture-list)
                        (glTranslatef 0 60 0)
                        
                        (set! render-state (+ render-state 1)))
                      button-functions))
          
          (let ((active-menu (get-active-menu main-menu)))
            (when active-menu
              (send active-menu render main-menu text-texture-list)))))))

;------------------------------------------------------------------------------
;get-active-menu: Will loop through all the menus to find the one with a state > -1.
;params: 
; ancestor - the parent from which the current search is based.
;------------------------------------------------------------------------------
(define (get-active-menu ancestor)
  (define active-menu #f)
  (define (active-loop menu)
    (for-each (λ (child)
                (if (> (get-field state child) -1)
                    (begin
                      
                      (set! active-menu child)
                      (printf "Found an active! ~a\n" active-menu))
                    (begin
                      (printf "~a is not active...\n" child)
                      (unless (null? (get-field children child))
                        (active-loop child)))))
              (get-field children menu)))
  
  (when ancestor
    (if (> (get-field state ancestor) -1)
        ancestor
        (begin
          (active-loop ancestor)
          active-menu))))

