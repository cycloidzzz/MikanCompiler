#lang racket

(require racket/set)
(require rackunit)

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
 (displayln uncover-testcase1-res))