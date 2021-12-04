#lang racket

(require rackunit)
(require "common.rkt")

; parse the input string into a list of moves and a list of 2d boards, returning both
(define (parse-input input)
  (define moves (map string->number (string-split (first input) ",")))
  (define (parse-board-line board-line)
    (map string->number (string-split board-line)))
  (define boards
    (map (curryr drop 1)
         (chunk (map parse-board-line
                     (drop input 1))
                6)))
  (values moves boards))
  
(define-values (test-input-moves test-input-boards)
  (parse-input (file->lines "day-04-test-input.txt")))
(define-values (input-moves input-boards)
  (parse-input (file->lines "day-04-input.txt")))

; Part 1

; find each winning line in the board
(define (board-lines board)
  (append board (apply map list board)))

; find the sequence of moves for which a line wins
(define (win-move moves line)
  (take moves (apply max (map (lambda (line-n) (add1 (index-of moves line-n))) line))))

; find the shortest sequence of moves for which a line in the board wins
(define (board-win-move moves board)
  (define winning-moves
    (map (curry win-move moves) (board-lines board)))
  (first (sort winning-moves (lambda (m1 m2) (< (length m1) (length m2))))))

; find the board with the shortest win
(define (board-win moves boards)
  (define winning-board
    (first (sort boards (lambda (b1 b2) (< (length (board-win-move moves b1))
                                           (length (board-win-move moves b2)))))))
  (define winning-move
    (board-win-move moves winning-board))
  (values winning-board winning-move))

; total the score of the board - sum of the board minus the sum of move numbers on it
(define (board-score board move)
  (define board-left
    (filter (lambda (n) (not (member n move))) (flatten board)))
  (* (apply + board-left) (last move)))

(define-values (test-winning-board test-winning-move)
  (board-win test-input-moves test-input-boards))
(check-equal? (board-score test-winning-board test-winning-move) 4512)

(define-values (winning-board winning-move)
  (board-win input-moves input-boards))
(board-score winning-board winning-move)

; Part 2

; find the board with the longest win
(define (board-loss moves boards)
  (define losing-board
    (first (sort boards (lambda (b1 b2) (> (length (board-win-move moves b1))
                                           (length (board-win-move moves b2)))))))
  (define losing-move
    (board-win-move moves losing-board))
  (values losing-board losing-move))

(define-values (test-losing-board test-losing-move)
  (board-loss test-input-moves test-input-boards))
(check-equal? (board-score test-losing-board test-losing-move) 1924)

(define-values (losing-board losing-move)
  (board-loss input-moves input-boards))
(board-score losing-board losing-move)