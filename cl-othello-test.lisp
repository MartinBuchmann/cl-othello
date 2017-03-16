(in-package #:cl-othello)

(setq *print-failures* t)

(define-test test-h8->88
  (assert-equal 11 (h8->88 "a1"))
  (assert-equal 88 (h8->88 "h8")))

(define-test test-88->h8
  (assert-equal 'A1 (88->h8 11))
  (assert-equal 'H8 (88->h8 88)))

(define-test test-gültig-p
    (assert-true (gültig-p 11))
  (assert-false (gültig-p 9))
  (assert-false (gültig-p 89)))

(define-test test-spielfeld
  (assert-equal 100 (length (neues-spielfeld))))

(defvar test-spiel (neues-spielfeld) "Ein Othello-Spielfeld für Tests.")

(define-test test-spielstand
  (assert-equal 0 (spielstand schwarz test-spiel)))

(defvar test-spiel2 '#(3 3 3 3 3 3 3 3 3 3 3 0 0 0 0 0 0 0 0 3 3 0 0 0 0 0 0 0 0 3 3 0 0 0 0 0 0 0 0
  3 3 0 0 0 2 2 0 0 0 3 3 0 1 1 1 2 0 0 0 3 3 0 0 0 0 0 0 0 0 3 3 0 0 0 0 0 0 0
                       0 3 3 0 0 0 0 0 0 0 0 3 3 3 3 3 3 3 3 3 3 3))

(define-test test-erlaubte-züge
  (assert-equal '(51 62 63 64) (erlaubte-züge weiß test-spiel2))
  (assert-equal '(34 35 36 56) (erlaubte-züge schwarz test-spiel2)))

(run-tests)
