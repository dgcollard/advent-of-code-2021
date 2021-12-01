#lang racket

(require rackunit)
(require "common.rkt")

(define test-input '(199 200 208 210 200 207 240 269 260 263))

(define input (file-input string->number "day-01-input.txt"))

; Part 1

(define (count-increases xs)
  (define pairs
    (map list (drop xs 1) (drop-right xs 1)))
  (count (curry apply >) pairs))

(check-equal? (count-increases test-input) 7)
(count-increases input)

; Part 2

(define (count-3-window-increases xs)
  (count-increases
    (map + (drop xs 2 ) (drop (drop-right xs 1) 1) (drop-right xs 2))))

(check-equal? (count-3-window-increases test-input) 5)
(count-3-window-increases input)