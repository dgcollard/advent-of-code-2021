#lang racket

(provide file-input)

(define (file-input p file)
  (map p (file->lines file)))
