#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors
         "drawtext.rkt")

(provide Menu% main-menu-functions get-active-menu)

(define Menu%
  (class object%
    (super-new)
    (init-field parent title button-functions children)
    
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

(define (get-active-menu ancestor)
;  (define (active-loop lst)
;    (if null?
;  (cond ((eq? ancestor #f) #f)
;        ((> (send ancestor get-state) -1)
;         ancestor)
;        (else (findf (λ (menu)
;                       
;  
;  
;  
;  
  
  (cond ((eq? ancestor #f) #f)
        ((> (send ancestor get-state) -1)
         ancestor)
        (else 
         (get-active-menu (findf (λ (menu)
                                   (> (send menu get-state) -1))
                                 (send ancestor get-children))))))


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

