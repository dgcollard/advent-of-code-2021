#lang racket

(require rackunit)
(require "common.rkt")

(define test-input '(16 1 2 0 4 2 7 1 2 14))

(define (parse-input-line input-line)
  (map string->number (string-split input-line ",")))

(define input (first (file-input parse-input-line "day-07-input.txt")))

; Part 1

(define (cost crabs target cost-fn)
  (define (crab-cost crab)
    (cost-fn crab target))
  (apply + (map crab-cost crabs)))

(define (linear a b)
  (abs (- a b)))

(define (min-cost crabs cost-fn)
  (define min-crab (apply min crabs))
  (define max-crab (apply max crabs))
  (define-values (best-cost best-target)
    (for/fold ([best-cost +inf.0] [best-target #f])
              ([target (in-inclusive-range min-crab max-crab)])
      (if (< (cost crabs target cost-fn) best-cost)
          (values (cost crabs target cost-fn) target)
          (values best-cost best-target))))
  best-cost)

(check-equal? (min-cost test-input linear) 37)
(min-cost input linear)

; Part 2

(define (sum a b)
  (define n (linear a b))
  (/ (* n (+ n 1)) 2))

(check-equal? (min-cost test-input sum) 168)
(min-cost input sum)
