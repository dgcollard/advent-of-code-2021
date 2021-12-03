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

(define (fill-list n k)
  (build-list n (lambda (i) k)))

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
  (bin-to-dec (map
   (lambda (bit-count)
     (if (> bit-count (/ (length nums) 2)) 1 0))
   (bit-counts nums))))

(define (epsilon nums)
  (- (expt 2 (length (first nums))) (gamma nums) 1))

(define (power-consumption nums)
  (* (gamma nums) (epsilon nums)))

(check-equal? (gamma test-input) 22)
(check-equal? (epsilon test-input) 9)
(check-equal? (power-consumption test-input) 198);

(power-consumption input)

; Part 2

