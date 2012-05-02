(list
 (new Menu% 
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
      (children '()))
 
 (new Menu%
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
                                      (void))))
                         ))
      (children '()))
 
 (letrec ((thechild (new Menu% ;recursive let to enable the children of this child to refer to their parent
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
                                                         (set-field! state (list-ref (get-field children menu) 0) 0))))))
                         
                         (children (list
                                    (new Menu%
                                         (parent thechild)              
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
                                         (children '())))))))
   thechild))