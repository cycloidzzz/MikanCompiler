#lang racket

(require rackunit rackunit/text-ui)
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
      [`(- ,(app recursive value))
        (- values)]
      [`(+ ,(app recursive value1) ,(app recursive value2))
        (+ value1 value2)]
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

;;; Flatten pass
;;; (return-value variables assignments)
;;; cases for let-bindings:
;;; `[,var ,(? fixnum?)]
;;; `[,var ,(? symbol?)]
;;; `[,var ,value]
;;; (define-values (rval def-vars assigns) (recursive value))
(define flatten
  (λ (expr)
    (match expr
      [(? fixnum?) (values expr '() '())]
      [(? symbol?) (values expr '() '())]
      [`(let ([,var ,value]) ,body)
        (define-values (rval rdef rassigns) (flatten value))
        (define-values (rb-val rb-def rb-assigns) (flatten body))
        (values rb-val
                (append rdef `(,var) rb-def)
                (append rassigns `((assign ,var ,rval)) rb-assigns))]
      [`(+ ,value1 ,value2)
        (define-values (rval1 rdef1 rassigns1) (flatten value1))
        (define-values (rval2 rdef2 rassigns2) (flatten value2))
        (define rval (gensym 'tmp))
        (values rval
                (append rdef1 `(,rval) rdef2)
                (append rassigns1
                        rassigns2
                        `((assign ,rval (+ ,rval1 ,rval2)))))]
      [`(- ,value1)
        (define-values (rval1 rdef1 rassigns1) (flatten value1))
        (define rval (gensym 'tmp))
          (values rval
                  (append rdef1 `(,rval))
                  (append rassigns1 `((assign ,rval (- ,rval1)))))]
      [`(program ,expr)
        (define-values (rval rdef rassigns) (flatten expr))
        `(program ,rdef
                  ,@(append rassigns
                          (list `(return ,rval))))])))

;;; TODO (cycloidz): require some naive peephole optimization
;;; to eliminate the redundant variable assignment.

;;; TODO (cycloidz): refactoring unittest with rackunit.
(define flatten-test1 `(program
                          (+ 52 (- 10))))
(define flatten-test1-result (flatten flatten-test1))
(displayln flatten-test1-result)

(define flatten-test2 `(program
                        (let ([x (+ (- 10) 11)])
                          (+ x 41))))
(define flatten-test2-result (flatten flatten-test2))
(displayln flatten-test2-result)


(define text-test1
  `(program
    (let [(x 1)
          (y 1)]
      (let [(x 2)
            (y 2)]
         (let [(y 1919000)
               (x 810)
               (z 1145140000000)]
           (+ z
              (+ x y)))))))

(displayln ((interpreter-R1 '()) text-test1))
(define uniquify-test1 ((uniquify '()) text-test1))
(displayln uniquify-test1)
(displayln ((interpreter-R1 '()) uniquify-test1))