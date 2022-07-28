#lang racket

(require racket/set)
(require rackunit)
(require graph)

(define (uncover-liveness)
  (define (calculate-variables xs)
    (map (λ (e)
           (match e
             [`(var ,x) x]))
         (filter (λ (e)
                   (match e
                     [`(var ,x) #t]
                     [else #f]))
                 xs)))
  (define (calculate-read-variables inst)
    (match inst
      [`(callq ,f) '()]
      [`(addq ,xs ...) (calculate-variables xs)]
      [`(,inst-name (var ,src) ,dst) `(,src)]
      [`(,inst-name (var ,src)) `(,src)]
      [else '()]))
  (define (calculate-write-variables inst)
    (match inst
      [`(callq ,f) '()]
      [`(,inst-name ,src (var ,dst)) `(,dst)]
      [`(,inst-name (var ,dst)) `(,dst)]
      [else '()]))
  (define (collect-liveness live-after ss)
    (cond [(null? ss) (values `(,live-after) ss)]
          [else
           (define-values (live-afters rest-ss) (collect-liveness live-after (cdr ss)))
           (letrec([curs (car ss)]
                   [cur-live-after (car live-afters)]
                   [cur-rx (calculate-read-variables curs)]
                   [cur-wx (calculate-write-variables curs)]
                   [live-before (set-union (set-subtract cur-live-after
                                                         (list->set cur-wx))
                                           (list->set cur-rx))])
             (values (cons live-before
                           live-afters)
                     (cons curs rest-ss)))]))
  (λ (e)
    (match e
      [`(program ,xs ,ss ...)
       (define-values (live-afters new-ss) (collect-liveness (set) ss))
       `(program (,xs ,(cdr live-afters)) ,@new-ss)])))

(define (build-interference)
  (define (add-interference g inst live-after)
    (displayln (format "inst: ~a, live-after: ~a" inst live-after))
    (match inst
      [`(movq (var ,src) (var ,dst))
       (for ([x (filter (λ (e)
                          (not (or (eq? e src) (eq? e dst))))
                        (set->list live-after))])
         (add-edge! g dst x)) g]
      [`(movq ,src (var ,dst))
       (for ([x (filter (λ (e)
                          (not (eq? e dst)))
                        (set->list live-after))])
         (add-edge! g dst x))]
      [`(addq ,src (var ,dst))
       (for ([x (filter (λ (e)
                          (not (eq? e dst)))
                        (set->list live-after))])
         (add-edge! g dst x))]
      [`(call ,f) `()]
      [else `()]))
  (define (add-interference-helper g ss live-afters)
    (cond [(null? live-afters)
           (if (null? ss)
               '()
               (error `add-interference-helper
                      "live-afters ~a and ss ~a doesn't match." live-afters ss))]
          [else
           (add-interference g (car ss) (car live-afters))
           (add-interference-helper g (cdr ss) (cdr live-afters))]))
  (λ (e)
    (match e
      [`(program (,xs ,live-afters) ,ss ...)
       (let ([g (unweighted-graph/undirected '())])
         (add-interference-helper g ss live-afters)
         `(program (,xs ,g) ,@ss))])))

(test-case
 "A simple test case"
 (define uncover-testcase1
   `(program (v w x y z t.2 t.1)
             (movq (int 1) (var v))
             (movq (int 46) (var w))
             (movq (var v) (var x))
             (addq (int 7) (var x))
             (movq (var x) (var y))
             (addq (int 4) (var y))
             (movq (var x) (var z))
             (addq (var w) (var z))
             (movq (var y) (var t.1))
             (negq (var t.1))
             (movq (var z) (var t.2))
             (addq (var t.1) (var t.2))
             (movq (var t.2) (reg rax))))
 (define uncover-testcase1-res ((uncover-liveness) uncover-testcase1))
 (define build-graph-res ((build-interference) uncover-testcase1-res))
 (displayln build-graph-res)
 (match build-graph-res
   [`(program (,xs ,g) ,ss ...)
    (check-true (has-edge? g 'w 'v))
    (check-true (has-edge? g 'w 'x))
    (check-true (has-edge? g 'w 'y))
    (check-true (has-edge? g 'w 'z))
    (check-true (has-edge? g 'x 'y))
    (check-true (has-edge? g 'y 'z))
    (check-true (has-edge? g 'z 't.1))
    (check-true (has-edge? g 't.1 't.2))]))

(test-case
 "Liveness analysis with function calls."
 (define uncover-testcase2
   `(program (x y t.2 t.1)
             (callq read_int)
             (movq (reg rax) (var x))
             (callq read_int)
             (movq (reg rax) (var y))
             (movq (var x) (var t.1))
             (addq (var y) (var t.1))
             (movq (var t.1) (var t.2))
             (addq (int 42) (var t.2))
             (movq (var t.2) (reg rax))))
 (define uncover-testcase2-res ((uncover-liveness) uncover-testcase2))
 (displayln uncover-testcase2-res))