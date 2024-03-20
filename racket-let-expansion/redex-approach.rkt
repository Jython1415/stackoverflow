#lang racket/base

(require redex/reduction-semantics)

(define-language Lambda-Let
  (e ::=
     natural                    ; natural numbers
     x                          ; variables
     (lambda (x ...) e)         ; abstractions
     (let ([x e] ...) e)        ; `let` forms
     (let* ([x e] ...) e)       ; `let*` forms
     (e e ...))                 ; applications
  (x ::= variable-not-otherwise-mentioned))

(define-metafunction Lambda-Let
  let->lambda : e -> e
  [(let->lambda (lambda (x ...) e_body))
   (lambda (x ...) (let->lambda e_body))]
  [(let->lambda (let ([x e] ...) e_body))
   (let->lambda ((lambda (x ...) e_body) e ...))]
  [(let->lambda (let* ([x e]) e_body))
   (let->lambda (let ([x e]) e_body))]
  [(let->lambda (let* ((x1 e1) (x2 e2) ...) e_body))
   (let->lambda (let* ((x1 e1)) (let* ((x2 e2) ...) e_body)))]
  [(let->lambda (e ...))
   ((let->lambda e) ...)]
  [(let->lambda e)
   e])

(term (let->lambda (let ([x 1]
                         [y 2])
                     (+ x y))))

(term (let->lambda (let* ([x 1]
                          [y (+ x 1)])
                     (* x y))))

;(for*/list ([a (cdr (x ...))] [b (cdr (e ...))]) (list a b))

;(cdr (x ...))

;(cdr (e ...))