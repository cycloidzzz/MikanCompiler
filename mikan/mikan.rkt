#lang racket

(require "utilities.rkt")


(define (interpreter-R1 env)
  (λ (expr)
    (define recursive (interpreter-R1 env))
    (match expr
      [(? symbol?) (lookup expr env)]
      [`(let ([,vars ,(app recursive values)]...) ,body)
        (define new-env (append (map cons vars values) env))
        ((interpreter-R1 new-env) body)]
      [(? fixnum?) expr]
      [`(read)
        (define read-value (read))
        (cond [(fixnum? read-value) read-value]
              [else
               (error `interpreter-R1 "expect and integer" read-value)])]
      [`(- ,(app recursive values) ...)
        (apply - values)]
      [`(+ ,(app recursive values) ...)
        (apply + values)]
      [`(program ,e) ((interpreter-R1 '()) e)]
      [else
       (error `interpreter-R1 "expect valid R1-expression, not ~a" expr)])))

(define (uniquify alist)
  (λ (expr)
    (match expr
      [(? number?) expr]
      [(? symbol?) (lookup expr alist)]
      [`(let ([,vars ,exprs]...) ,body)
        (define uvars (map gensym vars))
        (define new-alist (append (map cons vars uvars) alist))
        `(let ,(map list
               uvars
               (map (uniquify alist) exprs))
              ,((uniquify new-alist) body))]
      [`(program ,expr)
        `(program ,((uniquify alist) expr))]
      [`(,op ,exprs ...)
        `(,op ,@(map (uniquify alist) exprs))])))


(define text-test1
  `(program
    (let [(x 1)
          (y 1)]
      (let [(x 2)
            (y 2)]
         (let [(y 1919000)
               (x 810)
               (z 1145140000000)]
             (+ x y z))))))

(displayln ((interpreter-R1 '()) text-test1))
(define uniquify-test1 ((uniquify '()) text-test1))
(displayln uniquify-test1)
(displayln ((interpreter-R1 '()) uniquify-test1))