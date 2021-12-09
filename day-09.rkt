#lang racket

(require rackunit)
(require "common.rkt")

(define test-input 
  '((2 1 9 9 9 4 3 2 1 0)
    (3 9 8 7 8 9 4 9 2 1)
    (9 8 5 6 7 8 9 8 9 2)
    (8 7 6 7 8 9 6 7 8 9)
    (9 8 9 9 9 6 5 6 7 8)))

(define (parse-line line)
  (map (compose string->number string) (string->list line)))

(define input (file-input parse-line "day-09-input.txt"))

; Part 1

(define (cell grid x y)
  (if (or (< x 0)
          (> x (sub1 (length (first grid))))
          (< y 0)
          (> y (sub1 (length grid))))
      +inf.0 
      (list-ref (list-ref grid y) x)))

(define (low-point? grid x y)
  (let ([c (cell grid x y)])
    (and (> (cell grid x (sub1 y)) c)
         (> (cell grid x (add1 y)) c)
         (> (cell grid (sub1 x) y) c)
         (> (cell grid (add1 x) y) c))))

(define (low-points grid)
  (filter number?
          (flatten
           (for/list ([i (in-range (length grid))])
             (for/list ([j (in-range (length (first grid)))])
               (if (low-point? grid j i) (cell grid j i) #f))))))

(define (risk-level grid)
  (let [(grid-low-points (low-points grid))]
    (+ (apply + grid-low-points) (length grid-low-points))))

(check-equal? (risk-level test-input) 15)
(risk-level input)

; Part 2

(define (basin grid xy)
  (define (adj p)
    (filter (lambda (q) (< (cell grid (car q) (cdr q)) 9))
            (append
             (if (> (car p) 0)                     (list (cons (sub1 (car p)) (cdr p))) '())
             (if (< (car p) (length (first grid))) (list (cons (add1 (car p)) (cdr p))) '())
             (if (> (cdr p) 0)                     (list (cons (car p) (sub1 (cdr p)))) '())
             (if (< (cdr p) (length grid))         (list (cons (car p) (add1 (cdr p)))) '()))))

  (define (expand basin)
    (remove-duplicates (append basin (flatmap adj basin))))

  ; expand the basin until it don't expand no more
  (define (recur basin)
    (let [(expanded-basin (expand basin))]
      (if (equal? basin expanded-basin)
          expanded-basin
          (recur expanded-basin))))

  (recur (list xy)))

(define (low-xys grid)
  (filter pair? (apply append
                       (for/list ([i (in-range (length grid))])
                         (for/list ([j (in-range (length (first grid)))])
                           (if (low-point? grid j i) (cons j i) #f))))))

(define (score grid)
  (apply * (take
            (sort (map (compose length (curry basin grid))
                       (low-xys grid)) >)
            3)))

(check-equal? (score test-input) 1134)
(score input)