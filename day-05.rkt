#lang racket

(require rackunit)
(require "common.rkt")

(struct line-segment (x1 y1 x2 y2) #:prefab)

(define (parse-line-segment input-line)
  (match (regexp-match #rx"^([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)$" input-line)
    [(list _ x1 y1 x2 y2)
     (apply line-segment (map string->number (list x1 y1 x2 y2)))]))

(check-equal? (parse-line-segment "0,9 -> 5,9") (line-segment 0 9 5 9))

(define test-input (file-input parse-line-segment "day-05-test-input.txt"))
(define input (file-input parse-line-segment "day-05-input.txt"))

; Part 1

; find the size of a grid that covers all line segments
(define (grid-size lss)
  (define size
    (foldl (lambda (ls acc)
             (cons 
              (max (car acc) (line-segment-x1 ls) (line-segment-x2 ls))
              (max (cdr acc) (line-segment-y1 ls) (line-segment-y2 ls))))
           (cons 0 0)
           lss))
  (cons (add1 (car size)) (add1 (cdr size))))

; create a grid of zeros
(define (grid xy)
  (fill-list (car xy) (fill-list (cdr xy) 0)))

; list all points covered by a line segment
(define (line-segment-points ls diag?)
  (let ([x1 (line-segment-x1 ls)]
        [y1 (line-segment-y1 ls)]
        [x2 (line-segment-x2 ls)]
        [y2 (line-segment-y2 ls)])
    (cond [(equal? x1 x2) (for/list [(y (in-inclusive-range y1 y2 (if (< y1 y2) 1 -1)))]
                            (cons x1 y))]
          [(equal? y1 y2) (for/list [(x (in-inclusive-range x1 x2 (if (< x1 x2) 1 -1)))]
                            (cons x y1))]
          [else (if diag? (for/list [(x (in-inclusive-range x1 x2 (if (< x1 x2) 1 -1)))
                                     (y (in-inclusive-range y1 y2 (if (< y1 y2) 1 -1)))]
                            (cons x y))
                    '())])))
                    

(check-equal? (line-segment-points (line-segment 0 9 5 9) #f)
              (list (cons 0 9)
                    (cons 1 9)
                    (cons 2 9)
                    (cons 3 9)
                    (cons 4 9)
                    (cons 5 9)))
(check-equal? (line-segment-points (line-segment 9 0 9 5) #f)
              (list (cons 9 0)
                    (cons 9 1)
                    (cons 9 2)
                    (cons 9 3)
                    (cons 9 4)
                    (cons 9 5)))

; increment the grid number in all positions covered by a horizontal or vertical line segment
; this is very inefficient and does many, many, many list traversals

(define (grid-inc-line-segments lss diag?)
  (foldl
   (lambda (ls grid)
     (define ps (line-segment-points ls diag?))
     (foldl
      (lambda (p g)
        (define x (car p))
        (define y (cdr p))
        (list-set g x
                  (list-set (list-ref g x) y
                            (add1 (list-ref (list-ref g x) y)))))
      grid
      ps))
   (grid (grid-size lss))
   lss))
   

(check-equal? (grid-size test-input) (cons 10 10))

(define (displaygrid grid)
  (for [(x (apply map list grid))]
    (displayln
     (string-join
      (map (lambda (y)
             (if (equal? y 0) "." (number->string y)))
           x)
      ""))))

(define (grid-score grid)
  (count (lambda (c) (>= c 2)) (flatten grid)))

(check-equal? (grid-score (grid-inc-line-segments test-input #f)) 5)
(grid-score (grid-inc-line-segments input #f))

; Part 2

(check-equal? (grid-score (grid-inc-line-segments test-input #t)) 12)
(grid-score (grid-inc-line-segments input #t))