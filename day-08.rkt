#lang racket

(require rackunit)
(require "common.rkt")

(struct patterns (in out) #:prefab)

(define (parse-line line)
  (define (parse-line-part line-part)
    (map string->list (string-split line-part " ")))
  (match (string-split line "|")
    [(list before after) (patterns (parse-line-part before)
                                   (parse-line-part after))]))

;  (define (parse-part p)
;    (map string->list (string-split p " ")))
;  (map (lambda (p)
;         (match p [(list in out) (patterns (map parse-part in)
;                                           (map parse-part out))]))
;       (string-split l "|")))

(define test-inputs
  (file-input parse-line "day-08-test-input.txt"))

(define inputs
  (file-input parse-line "day-08-input.txt"))

; Part 1

(define (out-1478 inputs)
  (apply + (map (lambda (input)
                  (count (lambda (out)
                           (member (length out) '(2 3 4 7)))
                         (patterns-out input)))
                inputs)))

(check-equal? (out-1478 test-inputs) 26)
(out-1478 inputs)

; Part 2

(define (segments ins)
  (define (length-eq? ln lst)
    (eq? ln (length lst)))

  (define input-1
    (findf (curry length-eq? 2) ins))
  (define input-4
    (findf (curry length-eq? 4) ins))
  (define input-7
    (findf (curry length-eq? 3) ins))
  (define input-8
    (findf (curry length-eq? 7) ins))

  (define seg-a (remove* input-1 input-7))
  (define seg-bd (remove* input-1 input-4))
  (define seg-eg (remove* (append seg-a input-4) input-8))

  (define inputs-length-5
    (filter (curry length-eq? 5) ins))
  
  ; 2 is the only input of length 5 that has 3 remaining with e and g removed
  (define input-2
    (findf (lambda (in)
             (equal? 3 (length (remove* seg-eg in))))
           inputs-length-5))

  (define seg-f (remove* input-2 input-1))
  (define seg-c (remove* seg-f input-1))

  ; 3 is the only input of length 5 that has c and f
  (define input-3
    (findf (lambda (in)
             (and (member (car seg-c) in) (member (car seg-f) in)))
           inputs-length-5))

  ; 5 is the only input of length 5 that does not have c
  (define input-5
    (findf (lambda (in)
             (not (member (car seg-c) in)))
           inputs-length-5))

  (define input-6 (remove* seg-c input-8))
  (define seg-e (remove* input-5 input-6))
  (define seg-g (remove* seg-e seg-eg))
  (define seg-b (remove* input-3 input-5))
  (define seg-d (remove* seg-b seg-bd))

  (define input-0 (remove* seg-d input-8))
  (define input-9 (remove* seg-e input-8))

  (list
   (cons (sort input-0 char<?) 0)
   (cons (sort input-1 char<?) 1)
   (cons (sort input-2 char<?) 2)
   (cons (sort input-3 char<?) 3)
   (cons (sort input-4 char<?) 4)
   (cons (sort input-5 char<?) 5)
   (cons (sort input-6 char<?) 6)
   (cons (sort input-7 char<?) 7)
   (cons (sort input-8 char<?) 8)
   (cons (sort input-9 char<?) 9)))

(define (solve input)
  (define solved-segments (segments (patterns-in input)))
  (define d
    (map (lambda (out)
           (cdr (assoc (sort out char<?) solved-segments)))
         (patterns-out input)))
  (+ (* 1000 (first d)) (* 100 (second d)) (* 10 (third d)) (fourth d)))

(define (solve-all inputs)
  (apply + (map solve inputs)))

(check-equal? (solve-all test-inputs) 61229)
(solve-all inputs)
