(list
 (new Menu% 
           (parent-box (box main-menu))
           (button-functions (list
                              (list
                               (cons 'text "Back")
                               (cons 'fn (λ (menu)
                                           (send menu set-state! -1)
                                           (send (send menu get-parent) set-state! 0))))
                              (list
                               (cons 'text "Load")
                               (cons 'fn (λ (menu)
                                           (display "Loadieloadieloadie eheheh"))))))
           (children-boxes '()))
 
 (new Menu%
           (parent-box (box main-menu))
           (button-functions (list
                              (list
                               (cons 'text "Back")
                               (cons 'fn (λ (menu)
                                           (send menu set-state! -1)
                                           (send (send menu get-parent) set-state! 0))))
                              (list
                               (cons 'text "Save")
                               (cons 'fn (λ (menu)
                                           (display "SHAVE THE CHEERLEADER"))))
                              (list
                               (cons 'text "Delete save")
                               (cons 'fn (λ (menu)
                                           (void))))
                              ))
           (children-boxes '()))
 
 (new Menu% 
           (parent-box (box main-menu))
           (button-functions (list
                              (list
                               (cons 'text "Back")
                               (cons 'fn (λ (menu)
                                           (send menu set-state! -1)
                                           (send (send menu get-parent) set-state! 0))))
                              (list
                               (cons 'text "How to play")
                               (cons 'fn (λ (menu)
                                           (display "This is how you play... derp"))))
                              (list
                               (cons 'text "Options")
                               (cons 'fn (λ (menu)
                                           (send menu set-state! -1)
                                           (send (list-ref (send menu get-children) 0) set-state! 0))))))
           
           (children-boxes (list
                      (box (new Menu%
                           (parent-box (list-ref (send main-menu get-children-boxes) 2))
                           (button-functions (list
                                              (list
                                               (cons 'text "Back")
                                               (cons 'fn (λ (menu)
                                                           (send menu set-state! -1)
                                                           (send (send menu get-parent) set-state! 0))))
                                              (list
                                               (cons 'text "Eat horse poop")
                                               (cons 'fn (λ (menu)
                                                           (display "Nothing interesting going on..."))))))
                           (children-boxes '())))))))