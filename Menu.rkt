#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors)

(provide show-inventory Menu% main-menu-functions get-active-menu)

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
        ((enter)  ((cdr (assq 'fn (list-ref button-functions state))) this) 
                  (set! state -1))
        ((back) (if (is-a? parent Menu%)
                    (begin 
                      (set! state -1)
                      (send parent set-state! 0))
                    (send parent leave-menu!)))))
    
    (define/public (render)
      (if (> state -1)
          (let ((render-state 0))
            (for-each (λ (button)
                        (if (eq? render-state state)
                            (glColor4f 0 1 0 1)
                            (glColor4f 1 1 1 1))
                        (glBegin GL_TRIANGLE_STRIP)
                        (glVertex2f 0 0)
                        (glVertex2f 200 0)
                        (glVertex2f 0 100)
                        (glVertex2f 200 100)
                        (glEnd)
                        (glTranslatef 0 120 0)
                        
                        (set! render-state (+ render-state 1)))
                      button-functions))
          
          (send (get-active-menu this) render)))))

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
         (cons 'text "Exit")
         (cons 'fn (lambda (menu)
                     (exit))))))



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