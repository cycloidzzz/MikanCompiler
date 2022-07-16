#lang racket

(require "utilities.rkt")

;;; Stage 1 : R1 interpreter
(define (interpreter-R1 env)
  (lambda (expr)
    (define recursive (interpreter-R1 env))
    (match expr
      [(? symbol?) (lookup expr env)]
      [`(let ([,var ,(app recursive value)]) ,body)
        (define new-env (cons (cons var value) env))
        ((interpreter-R1 new-env) body)]
      [(? fixnum?) expr]
      [`(read)
        (define read-value (read))
        (cond [(fixnum? read-value) read-value]
              [else
               (error `interpreter-R1 "expect and integer" read-value)])]
      [`(- ,(app recursive value))
        (- 0 value)]
      [`(+ ,(app recursive value1) ,(app recursive value2))
        (+ value1 value2)]
      [`(program ,e) ((interpreter-R1 '()) e)]
      [else
       (error `interpreter-R1 "expect valid R1-expression, not ~a" expr)])))


(displayln (+ 1 2))
((interpreter-R1 '()) `(program (let ([x (+ 12 20)]) (+ x x))))