#lang racket

(provide file-input)
(provide chunk)
(provide fill-list)

(define (file-input p file)
  (map p (file->lines file)))

(define (chunk lst n)
  (if (empty? lst) '()
      (cons (take lst n)
            (chunk (drop lst n) n))))

(define (fill-list n k)
  (build-list n (lambda (i) k)))