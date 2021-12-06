#lang racket

(require rackunit)
(require "common.rkt")

(define test-input '((0 0 1 0 0)
                     (1 1 1 1 0)
                     (1 0 1 1 0)
                     (1 0 1 1 1)
                     (1 0 1 0 1)
                     (0 1 1 1 1)
                     (0 0 1 1 1)
                     (1 1 1 0 0)
                     (1 0 0 0 0)
                     (1 1 0 0 1)
                     (0 0 0 1 0)
                     (0 1 0 1 0)))

(define (parse-input-line line)
  (map (compose string->number string) (string->list line)))

(check-equal? (parse-input-line "00100") '(0 0 1 0 0))

(define input (file-input parse-input-line "day-03-input.txt"))

; Part 1

(define (bit-counts nums)
  (foldl
   (lambda (acc num) (map + acc num))
   (fill-list (length (first nums)) 0)
   nums))

(define (bin-to-dec bits)
  (apply + (for/list ([x (reverse bits)]
                      [i (in-naturals)])
             (* x (expt 2 i)))))

(define (gamma nums)
  (map
   (lambda (bit-count)
     (if (>= bit-count (/ (length nums) 2)) 1 0))
   (bit-counts nums)))

(define (epsilon nums)
  (map
   (lambda (bit-count)
     (if (<= bit-count (/ (length nums) 2)) 1 0))
   (bit-counts nums)))

(define (power-consumption nums)
  (* (bin-to-dec (gamma nums)) (bin-to-dec (epsilon nums))))

(check-equal? (gamma test-input) '(1 0 1 1 0))
(check-equal? (epsilon test-input) '(0 1 0 0 1))
(check-equal? (power-consumption test-input) 198)

(power-consumption input)

; Part 2

(define (calculate-rating nums fn)
  (define (recur nums i)
    (define count-ones
      (apply + (map (lambda (n)
                      (list-ref n i))
                    nums)))
    (define most-common-bit
      (if (fn count-ones (/ (length nums) 2)) 1 0))
    (define filter-nums
      (filter (lambda (n)
                (eq? (list-ref n i) most-common-bit))
              nums))
    (if (equal? (length filter-nums) 1)
        (car filter-nums)
        (recur filter-nums (add1 i))))
  (recur nums 0))

(define (oxygen-rating nums)
  (calculate-rating nums >=))

(check-equal? (oxygen-rating test-input) '(1 0 1 1 1))

(define (co2-scrubber-rating nums)
  (calculate-rating nums <))

(check-equal? (co2-scrubber-rating test-input) '(0 1 0 1 0))

(define (life-support-rating nums)
  (* (bin-to-dec (oxygen-rating nums)) (bin-to-dec (co2-scrubber-rating nums))))

(check-equal? (life-support-rating test-input) 230)
(life-support-rating input)
