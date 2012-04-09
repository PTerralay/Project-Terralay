#lang racket

(provide Trigger%)

(define Trigger%
  (class object%
    (super-new)
    (init-field trigger-assoc)
    (define poll-fn (cdr (assq 'poll trigger-assoc)))
    (define act-fn (cdr (assq 'act trigger-assoc)))
    
    (define/public (poll&act obj world)
        (when (poll-fn obj  world)
          (act-fn obj world)))))
