#lang racket

(require rackunit)
(require "common.rkt")

(define test-input '(199 200 208 210 200 207 240 269 260 263))

(define input (file-input string->number "day-01-input.txt"))

; Part 1

(define (count-increases lst)
  (define lst-paired
    (map list (reverse (cdr (reverse lst)))
              (cdr lst)))
  (count (curry apply <) lst-paired))

(check-equal? (count-increases test-input) 7)
(count-increases input)

; Part 2

(define (count-3-window-increases lst)
  (count-increases
   (map + (reverse (cdr (cdr (reverse lst))))
          (cdr (reverse (cdr (reverse lst))))
          (cdr (cdr lst)))))

(check-equal? (count-3-window-increases test-input) 5)
(count-3-window-increases input)