#lang racket

(define-syntax-rule (mylet ([id expr] ...) body ...)
  ((λ (id ...) body ...) expr ...))

(define-syntax mylet*
  (syntax-rules ()
    [(mylet* ()                         body ...)   ((λ() body ...))]
    [(mylet* ([id expr])                body ...)   (mylet ([id expr]) body ...)]
    [(mylet* ([id0 expr0] [id expr]...) body ...)   (mylet ([id0 expr0])
                                                           (mylet* ([id expr] ...) body ...))]))
(mylet  ([x 1] [y 2])        (+ x y))    
(mylet* ([x 1] [y (+ x 10)]) (+ x y))

(syntax->datum (expand #'(mylet  ([x 1] [y 2]) (+ x y))))
(syntax->datum (expand #'(mylet* ([x 1] [y (+ x 10)]) (+ x y))))