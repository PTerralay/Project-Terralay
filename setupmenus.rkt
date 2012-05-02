#lang racket
(require "Menu.rkt")
(provide setup-main-menu) 

(define (setup-main-menu)
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
                       
                       (display (get-field children menu))
                       (set-field! state (list-ref (get-field children menu) 2) 0))))
          (list
           (cons 'text "Exit")
           (cons 'fn (lambda (menu)
                       (exit))))))
  
  (define main-menu
    (new Menu% 
         (parent #f)
         (title "Main Menu")
         (button-functions main-menu-functions)
         (children '())))
  
  
  (define loadmenu (new Menu% 
                        (parent main-menu)
                        (title "Load game")
                        (button-functions (list
                                           (list
                                            (cons 'text "Back")
                                            (cons 'fn (λ (menu)
                                                        (set-field! state menu -1)
                                                        (set-field! state (get-field parent menu) 0))))
                                           (list
                                            (cons 'text "Load")
                                            (cons 'fn (λ (menu)
                                                        (display "Loadieloadieloadie eheheh"))))))
                        (children'())))
  
  (define savemenu (new Menu%
                        (parent main-menu)
                        (title "Save game")
                        (button-functions (list
                                           (list
                                            (cons 'text "Back")
                                            (cons 'fn (λ (menu)
                                                        (set-field! state menu -1)
                                                        (set-field! state (get-field parent menu) 0))))
                                           (list
                                            (cons 'text "Save")
                                            (cons 'fn (λ (menu)
                                                        (display "SHAVE THE CHEERLEADER"))))
                                           (list
                                            (cons 'text "Delete save")
                                            (cons 'fn (λ (menu)
                                                        (void))))))
                        (children'())))
  
  
  (define homenu (new Menu% 
                      (parent main-menu)
                      (title "Help and Options")
                      (button-functions (list
                                         (list
                                          (cons 'text "Back")
                                          (cons 'fn (λ (menu)
                                                      (set-field! state menu -1)
                                                      (set-field! state (get-field parent menu) 0))))
                                         (list
                                          (cons 'text "How to play")
                                          (cons 'fn (λ (menu)
                                                      (display "This is how you play... derp"))))
                                         (list
                                          (cons 'text "Options")
                                          (cons 'fn (λ (menu)
                                                      (set-field! state menu -1)
                                                      (display (get-field children menu))
                                                      (set-field! state (list-ref (get-field children menu) 0) 0))))))
                      
                      (children '())))
  
  (define ho-optionsmenu (new Menu%
                              (parent homenu)            
                              (title "Options")
                              (button-functions (list
                                                 (list
                                                  (cons 'text "Back")
                                                  (cons 'fn (λ (menu)
                                                              (set-field! state menu -1)
                                                              (set-field! state (get-field parent menu) 0))))
                                                 (list
                                                  (cons 'text "Eat horse poop")
                                                  (cons 'fn (λ (menu)
                                                              (display "Nothing interesting going on..."))))))
                              (children '())))
  
  (set-field! children homenu (list ho-optionsmenu))
  (set-field! children main-menu (list loadmenu savemenu homenu))
  
  main-menu)

