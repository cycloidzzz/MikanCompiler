#lang racket

(require racket/pretty racket/match)
(require (for-syntax racket))
(require rackunit rackunit/text-ui)

(provide lookup)

(define (lookup x ls)
  (let recur ([xs ls])
    (cond
      [(null? xs)
       (error 'lookup "didn't find ~a in ~a" x (car ls))]
      [(pair? xs)
       (define curr (car xs))
       (cond
         [(and (pair? curr) (eq? (car curr) x)) (cdr curr)]
         [(not (pair? curr))
          (error 'lookup "expected pair in alist, not ~a" curr)]
         [else (recur (cdr xs))])]
      [else (error 'lookup "expected an alist, not ~a" ls)])))