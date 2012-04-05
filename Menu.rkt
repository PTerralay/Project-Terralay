#lang racket
(require racket/mpair
         sgl/gl
         sgl/gl-vectors)

(provide show-inventory); Menu%)
;
;(define Menu%
;  (class object%
;    (super-new)
;    (init-field parent button-functions children)
;    
;    (define state -1)
;    
;    (define/public (set-state! new-state)
;      (set! state new-state))
;    
;    (define/public (menu-action action)
;      (case action
;        ((up) (if (eq? state 0)
;                  (set! state (- (length button-functions) 1))
;                  (set! state (- state 1))))
;        ((down) (if (eq? state (- (length button-functions) 1))
;                    (set! state 0)
;                    (set! state (+ state 1))))
;        ((enter) (set! state -1)
;                 ((list-ref button-functions state)))
;        ((back) (if (is-a? parent Menu%)
;                    (send parent set-state! 0)
;                    (send parent leave-menu!)))))))
;
;(define main-menu-functions
;  (list
;   (lambda ()
;     (send parent leave-menu!))
;   (lambda ()
;     (set! state -1)
;     (send (list-ref children 0) set-state! 0))))



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