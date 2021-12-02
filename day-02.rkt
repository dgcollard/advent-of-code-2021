#lang racket

(require rackunit)
(require "common.rkt")

(define test-input (list (cons 'forward 5)
                         (cons 'down 5)
                         (cons 'forward 8)
                         (cons 'up 3)
                         (cons 'down 8)
                         (cons 'forward 2)))

(define (parse-input-line input-line)
  (define parts (string-split input-line " "))
  (cons (string->symbol (first parts)) (string->number (second parts))))

(check-equal? (parse-input-line "forward 5") (cons 'forward 5))
(check-equal? (parse-input-line "down 9") (cons 'down 9))
(check-equal? (parse-input-line "up 5") (cons 'up 5))

(define input (file-input parse-input-line "day-02-input.txt"))

; Part 1

(define (sub-pos mvs)
  (foldl
   (lambda (mv pos)
     (let ([dir (car mv)]
           [d (cdr mv)]
           [x (car pos)]
           [y (cdr pos)])
       (match dir
         ['forward (cons (+ x d) y)]
         ['up      (cons x       (- y d))]
         ['down    (cons x       (+ y d))])))
   (cons 0 0)
   mvs))

(define (multiply-sub-pos mvs)
  (define pos (sub-pos mvs))
  (* (car pos) (cdr pos)))

(check-equal? (multiply-sub-pos test-input) 150)
(multiply-sub-pos input)

; Part 2

(define (sub-pos-tilt mvs)
  (define pos
    (foldl
     (lambda (mv pos)
       (let ([dir (car mv)]
             [d (cdr mv)]
             [x (first pos)]
             [y (second pos)]
             [aim (third pos)])
         (match dir
           ['forward (list (+ x d) (+ y (* d aim)) aim)]
           ['up      (list x y (- aim d))]
           ['down    (list x y (+ aim d))])))
     '(0 0 0)
     mvs))
  (take pos 2))

(define (multiply-sub-pos-tilt mvs)
  (apply * (sub-pos-tilt mvs)))

(check-equal? (multiply-sub-pos-tilt test-input) 900)
(multiply-sub-pos-tilt input)
