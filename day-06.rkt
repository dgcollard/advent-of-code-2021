#lang racket

(require rackunit)
(require "common.rkt")

(define test-input '(3 4 3 1 2))

(define (parse-input-line input-line)
  (map string->number (string-split input-line ",")))

(define input (first (file-input parse-input-line "day-06-input.txt")))

; Part 1

(define (next-lanternfish lf)
  (if (> lf 0) (list (sub1 lf))
      '(6 8)))

(define (next-lanternfishes lfs)
  (flatmap next-lanternfish lfs))

(define (next-lanternfishes-gens lfs gens)
  (if (equal? gens 0)
      lfs
      (next-lanternfishes-gens (next-lanternfishes lfs)
                               (sub1 gens))))

(check-equal? (sort (next-lanternfishes-gens test-input 0) <) '(1 2 3 3 4))
(check-equal? (sort (next-lanternfishes-gens test-input 1) <) '(0 1 2 2 3))
(check-equal? (sort (next-lanternfishes-gens test-input 2) <) '(0 1 1 2 6 8))

(check-equal? (length (next-lanternfishes-gens test-input 18)) 26)
(check-equal? (length (next-lanternfishes-gens test-input 80)) 5934)
(length (next-lanternfishes-gens input 80))

; Part 2
; Part 1 takes way too much memory so i am doing the operations over the count of lanternfish of each age,
; rather than an individual cell per lanternfish

; count lanternfish by current age
(define (group-lanternfishes lfs)
  (for/list [(i (in-inclusive-range 0 8))]
    (count (curry equal? i) lfs)))

; total lanternfishes in the next generation 1=>0 2=>1 3=>4 etc
; except! 0=>8 and (0 or 7)=>6
(define (next-group-lanternfishes glfs)
  (match glfs [(list 0s 1s 2s 3s 4s 5s 6s 7s 8s)
               (list 1s 2s 3s 4s 5s 6s (+ 7s 0s) 8s 0s)]))

(define (next-group-lanternfishes-gens glfs gens)
  (if (equal? gens 0)
      glfs
      (next-group-lanternfishes-gens (next-group-lanternfishes glfs)
                                     (sub1 gens))))

(define (how-many-lanternfishes lfs gens)
  (apply + (next-group-lanternfishes-gens (group-lanternfishes lfs) gens)))

(check-equal? (how-many-lanternfishes test-input 80) 5934)
(check-equal? (how-many-lanternfishes test-input 256) 26984457539)
(how-many-lanternfishes input 256)
