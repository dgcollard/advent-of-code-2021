#lang racket

(provide file-input)
(provide chunk)

(define (file-input p file)
  (map p (file->lines file)))

(define (chunk lst n)
  (if (empty? lst) '()
      (cons (take lst n)
            (chunk (drop lst n) n))))