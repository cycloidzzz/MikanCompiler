#lang racket

(require rackunit rackunit/text-ui)
(require "utilities.rkt")


(define (interpreter-R1 env)
  (λ (expr)
    (define recursive (interpreter-R1 env))
    (match expr
      [(? symbol?) (lookup expr env)]
      [`(let ([,vars ,(app recursive e)]...) ,body)
       (define new-env (append (map cons vars e) env))
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
(define (flatten need-atomic)
  (λ (e)
    (match e
      [(? fixnum?) (values e '() '())]
      [(? symbol?) (values e '() '())]
      [`(let ([,vars ,(app (flatten #f) new-vals ss-vals xs-vals)]...) ,body)
       (define-values (new-body ss-body xs-body) ((flatten #t) body))
       (define ss-bindings (append* ss-vals))
       (define xs-bindings (append* xs-vals))
       (values new-body
               (append vars ss-bindings ss-body)
               (append xs-bindings
                       (map
                        (λ (var val)
                          `(assign ,var ,val))
                        vars new-vals)
                       xs-body))]
      [`(program ,e)
       (define-values (new-e ss xs) ((flatten #t) e))
       `(program ,ss
                 ,@(append xs (list `(return ,new-e))))]
      [`(,op ,(app (flatten #t) new-es ss xs) ...)
       (define apply-op `(,op ,@new-es))
       (define op-ss (append* ss))
       (define op-xs (append* xs))
       (cond [need-atomic
              (define tmp (gensym 'tmp))
              (values tmp
                      (cons tmp op-ss)
                      (append op-xs
                              (list `(assign ,tmp ,apply-op))))]
             [else
              (values apply-op op-ss op-xs)])]
      [else
       (error `flatten "expected valid R1-expressionm, not ~a" e)])))

(define (binary-op->inst op)
  (match op
    ['+ 'addq]
    [else
     (error `binaryop->inst "invalid binary operation ~a" op)]))

(define (unary-op->inst op)
  (match op
    ['- 'negq]
    [else
     (error `unary-op->inst "invalid unary operation ~a" op)]))


;;; Select instructions pass
(define (select-instructions)
  (λ (e)
    (match e
      [(? fixnum?) `(int ,e)]
      [(? symbol?) `(var ,e)]
      [`(return ,var)
       `((movq (var ,var) (reg rax)))]
      [`(assign ,var ,x) #:when (symbol? x)
                         (define new-var ((select-instructions) var))
                         (cond [(eq? var x) '()]
                               [else
                                `((movq (var ,x) ,new-var))])]
      [`(assign ,var ,n) #:when (fixnum? n)
                         (define new-var ((select-instructions) var))
                         `((movq (int ,n) ,new-var))]
      [`(assign ,var (read))
       (define new-var ((select-instructions) var))
       `((callq read_int)
         (movq (reg rax) ,new-var))]
      [`(assign ,var (,op ,var1 ,var2))
       (let ([new-var1 ((select-instructions) var1)]
             [new-var2 ((select-instructions) var2)]
             [new-var ((select-instructions) var)])
         (cond [(eq? var var1) `((addq ,new-var2 new-var))]
               [(eq? var var2) `((addq ,new-var1 new-var))]
               [else
                `((movq ,new-var2 ,new-var)
                  (addq ,new-var1 ,new-var))]))]
      [`(assign ,var (,op ,var1))
       (let ([new-var ((select-instructions) var)]
             [new-var1 ((select-instructions) var1)])
         (cond [(eq? var var1)
                `((negq ,new-var))]
               [else
                `((movq ,new-var1 ,new-var)
                  (negq ,new-var))]))]
      [`(program ,ss ,xs ...)
       (let ([new-xs (append* (map (select-instructions) xs))])
         `(program ,ss ,@new-xs))]
      [else
       (error `select-instructions "expect valid pseudo-x86 instruction, not ~a" e)])))

;;; Assign homes pass
(define (assign-homes homes)
  (define first-offset 8)
  (define variable-size 8)
  (define (make-stack-offset homes x)
    (- (+ first-offset
          (* variable-size
             (hash-ref homes x)))))
  (λ (e)
    (match e
      [`(int ,n) e]
      [`(reg ,r) e]
      [`(var ,x)
       `(deref rbp ,(make-stack-offset homes x))]
      [`(callq read_int) e]
      [`(program ,es ,xs ...)
       (define es-loc (stream->list (in-range 0 (length es))))
       (let ([new-homes
              (make-hash (map cons es es-loc))])
         (define new-xs (map (assign-homes new-homes) xs))
         `(program ,(length es) ,@new-xs))]
      [`(,op ,(app (assign-homes homes) es) ...)
       `(,op ,@es)]
      [else
       (error `assign-homes "invalid C0 instruction ~a" e)])))

(define (memory-operand? e)
  (match e
    [`(deref ,reg ,loc) #t]
    [else #f]))

(define (patch-instructions)
  (λ (e)
    (match e
      [`(movq (int ,n) ,dst) #:when (memory-operand? dst)
                             `((movq (int ,n) (reg rax))
                               (movq (reg rax) ,dst))]
      [`(movq ,src ,dst)
       (cond [(and (memory-operand? src) (memory-operand? dst))
              `((movq ,src (reg rax))
                (movq (reg rax) ,dst))]
             [else `((movq ,src ,dst))])]
      [`(callq ,f) `((call ,f))]
      [`(,inst ,src ,dst)
       (cond [(and (memory-operand? src) (memory-operand? dst))
              `((movq ,src (reg rax))
                (,inst (reg rax) ,dst))]
             [else `((,inst ,src ,dst))])]
      [`(,inst ,src) `((,inst ,src))]
      [`(program ,stack-frame-size ,xs ...)
       (let ([new-xs (append* (map (patch-instructions) xs))])
         `(program ,stack-frame-size ,@new-xs))])))

;; print x86-pass
(define (emit-x86)
  (λ (e)
    (match e
      [`(deref ,reg ,offset)
       (format "~a(%~a)" offset reg)]
      [`(int ,n)
       (format "$~a" n)]
      [`(reg ,r)
       (format "%~a" r)]
      [`(callq ,f)
       (format "\tcallq\t~a" f)]
      [`(program ,stack-frame-size ,xs ...)
       (let ([new-xs (map (emit-x86) xs)])
         (string-append
          (format "\t.global ~a\n" "_main")
          (format "\t_main:\n")
          (format "\tpushq\t%rbp\n")
          (format "\tmovq\t%rsp, %rbp\n")
          (format "\tsubq\t$~a, %rsp\n" (* 8 stack-frame-size))
          (string-append* (map (emit-x86) xs))
          (format "\tmovq\t%rax, %rdi\n")
          (format "\tcallq\tprint_int\n")
          (format "\taddq\t$~a, %rsp\n" (* 8 stack-frame-size))
          (format "\tpopq\t%rbp\n")
          (format "\tretq\n")))]
      [`(,inst ,(app (emit-x86) src) ,(app (emit-x86) dst))
       (format "\t~a\t~a, ~a\n" inst src dst)]
      [`(,inst ,(app (emit-x86) src))
       (format "\t~a\t~a\n" inst src)])))

(define (compiler path)
  (λ (e)
    (letrec([unique-e ((uniquify '()) e)]
            [flatten-e ((flatten #t) unique-e)]
            [select-inst-e ((select-instructions) flatten-e)]
            [assign-homes-e ((assign-homes '()) select-inst-e)]
            [patch-inst-e ((patch-instructions) assign-homes-e)]
            [emit-x86-e ((emit-x86) patch-inst-e)])
      (with-output-to-file path
        (λ ()
          (display emit-x86-e))
        #:mode 'binary
        #:exists 'truncate))))


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

;;; TODO (cycloidz): refactoring unittest with rackunit.
(displayln "Start of Test 1 : ****************************************")
(define test1 `(program
                (+ 52 (- 10))))
(define test1-uniquify-result ((uniquify '()) test1))
(define test1-flatten-result ((flatten #t) test1-uniquify-result))
(define test1-select-inst-result ((select-instructions) test1-flatten-result))
(displayln test1-flatten-result)
(define test1-assign-homes-result ((assign-homes '()) test1-select-inst-result))
(define test1-patch-inst-result ((patch-instructions) test1-assign-homes-result))
(displayln test1-patch-inst-result)
(define test1-x86-result ((emit-x86) test1-patch-inst-result))
(displayln test1-x86-result)
(displayln "End of Test 1 : *******************************************")

(displayln "Start of Test 2 : *****************************************")
(define flatten-test2 `(program
                        (let ([a 42])
                          (let ([b a])
                            b))))
(define test2-flatten-result ((flatten #t) flatten-test2))
(define test2-select-inst-result ((select-instructions) test2-flatten-result))
(define test2-assign-homes-result ((assign-homes '()) test2-select-inst-result))
(define test2-patch-inst-result ((patch-instructions) test2-assign-homes-result))
(displayln test2-patch-inst-result)
(define test2-compiler (compiler "./output/test2.S"))
(test2-compiler flatten-test2)
(displayln "End of Test 2 : *******************************************")

(displayln "Start of Test 3 : ****************************************")
(define flatten-test3 `(program
                        (let ([x 1919000]
                              [y 810]
                              [z 1145140000000])
                          (+ z (+ x y)))))
(define flatten-test3-result ((flatten #t) flatten-test3))
(displayln flatten-test3-result)
(displayln "End of Test 3 : *******************************************")

(define select-instructions-test1
  `(program (x y z)
            (assign x (read))
            (assign y (read))
            (assign z (+ x y))))
(define select-inst-result1 ((select-instructions) select-instructions-test1))
(displayln select-inst-result1)

(define assign-homes-result1 ((assign-homes '()) select-inst-result1))
(displayln assign-homes-result1)
