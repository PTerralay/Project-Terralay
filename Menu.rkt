#lang racket
(require racket/mpair)

(provide show-inventory)

(define (show-inventory player)
  (display "Inventory:\n")
  (display (send (mcar (send player get-things)) name))
  (mfor-each (lambda (thing)
               (display ", ")
               (display (send thing name))
               (newline))
             (mcdr (send player get-things))))