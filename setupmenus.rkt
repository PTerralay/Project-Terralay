
#lang racket
(require racket/gui "Menu.rkt")
(provide setup-main-menu) 

(define (setup-main-menu world)
  (define main-menu-functions
    (list (list 
           (cons 'text "Back")
           (cons 'fn (lambda (menu)
                       (send (get-field parent menu) leave-menu!))))
          (list
           (cons 'text "New Game")
           (cons 'fn (lambda (menu)
                       (send world loadgame "Gamedata/NewGame.rkt")
                       (send (get-field parent menu) leave-menu!)
                       (send world draw-text-ingame 'Workroom 1 4 1.5 "Project Terralay" 300))))
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
         (world world)
         (button-functions main-menu-functions)
         (children '())))
  
  
  (define loadmenu (new Menu% 
                        (parent main-menu)
                        (title "Load game")
                        (world world)
                        (button-functions (list
                                           (list
                                            (cons 'text "Back")
                                            (cons 'fn (λ (menu)
                                                        (set-field! state menu -1)
                                                        (set-field! state (get-field parent menu) 0))))
                                           (list
                                            (cons 'text "Load")
                                            (cons 'fn (λ (menu)
                                                        (send world loadgame (get-file))
                                                        (send (get-field parent (get-field parent menu)) leave-menu!))))))
                        (children'())))
  
  (define savemenu (new Menu%
                        (parent main-menu)
                        (title "Save game")
                        (world world)
                        (button-functions (list
                                           (list
                                            (cons 'text "Back")
                                            (cons 'fn (λ (menu)
                                                        (set-field! state menu -1)
                                                        (set-field! state (get-field parent menu) 0))))
                                           
                                           (list (cons 'text "New save")
                                                 (cons 'fn (λ (menu)
                                                             (define (saveloop n)
                                                               (let ((filecandidate
                                                                      (string-append "Saves/"
                                                                                     (symbol->string (get-field mapID (get-field current-map world)))
                                                                                     (number->string n)
                                                                                     ".rkt")))
                                                                 (if (file-exists? filecandidate)
                                                                     (saveloop (+ n 1))
                                                                     filecandidate)))
                                                             (send world savegame (saveloop 1))
                                                             (set-field! state menu -1)
                                                             (set-field! state (get-field parent menu) 0))))
                                           
                                           (list (cons 'text "Overwrite save")
                                                 (cons 'fn (λ (menu)
                                                             (send world savegame (get-file)))))
                                           

                                           (list
                                            (cons 'text "Delete save")
                                            (cons 'fn (λ (menu)
                                                        (delete-file (get-file)))))))
                        (children '())))
  
  
  (define homenu (new Menu% 
                      (parent main-menu)
                      (title "Help and Options")
                      (world world)
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
                              (world world)
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