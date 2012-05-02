#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors
         "drawtext.rkt")

(provide Menu% main-menu-functions get-active-menu)

(define main-menu-functions
  (list (list 
         (cons 'text "Back")
         (cons 'fn (lambda (menu)
                     (send (get-field parent menu) leave-menu!))))
        (list
         (cons 'text "New Game")
         (cons 'fn (lambda (menu)
                     (display "wehoe, new game is running! ... kinda...\n"))))
        (list
         (cons 'text "Load Game")
         (cons 'fn (lambda (menu)
                     (set-field! state menu -1)
                     (set-field! state (list-ref (get-field children menu) 0) 0))))
        (list
         (cons 'text "Save Game")
         (cons 'fn (lambda (menu)
                     (set-field! state menu -1)
                     (set-field! state (list-ref (get-field children menu) 1) 0))))
        (list
         (cons 'text "Help and Options")
         (cons 'fn (lambda (menu)
                     (set-field! state menu -1)
                     (set-field! state (list-ref (get-field children menu) 2) 0))))
        (list
         (cons 'text "Exit")
         (cons 'fn (lambda (menu)
                     (exit))))))


;------------------------------------------------------------------------------
;Class: Menu%
;Description: A Menu% object is a menu with buttons that the player can operate with the arrows.
; It can have any number of Menu% objects as children.
;------------------------------------------------------------------------------
(define Menu%
  (class object%
    (super-new)
    (init-field parent title button-functions children)
    
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
    (define/public (render main-menu texture-list)
      (if (> state -1)
          (let ((render-state 0))
            (glColor4f 1 1 1 1)
            (draw-text 0 0 3 title texture-list)
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
                        (draw-text 20 10 1 (cdr (assq 'text button)) texture-list)
                        (glTranslatef 0 60 0)
                        
                        (set! render-state (+ render-state 1)))
                      button-functions))
          
          (let ((active-menu (get-active-menu main-menu)))
            (when active-menu
              (send active-menu render main-menu texture-list)))))))

;------------------------------------------------------------------------------
;get-active-menu: Will loop through all the menus to find the one with a state > -1. 
; still not working properly...
;params: 
; ancestor - the parent from which the current search is based.
;------------------------------------------------------------------------------
(define (get-active-menu ancestor)
  (define (active-loop menu)
    (let ((active-menu #f))
      (for-each (λ (child)
                  (if (> (get-field state child) -1)
                      (begin
                        (set! active-menu child)
                        (display "Found an active: ") (display (get-field title child)) (newline))
                      (begin
                        (printf "~a is not active\n" (get-field title child))
                        (unless (null? (get-field children child))
                          (active-loop child)))))
                (get-field children menu))
      active-menu))
  
  (when ancestor
    (if (> (get-field state ancestor) -1)
        ancestor
        (active-loop ancestor))))

