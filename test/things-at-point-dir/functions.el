;;; functions.el --- Test fixtures for thing-at-point -*- lexical-binding: t; -*-

;; First defun: standalone, simple.
(defun fixture-alpha ()
  "First fixture function."
  (message "alpha"))

;; Second defun: immediately after first.
(defun fixture-beta (arg)
  "Second fixture function."
  (list arg (1+ arg)))

;; Third defun: contains nested sexps.
(defun fixture-gamma (x y)
  "Third fixture function with nesting."
  (let ((sum (+ x y))
        (product (* x y)))
    (cons sum product)))

;; A top-level variable for symbol/sexp tests.
(defvar fixture-delta 42
  "A variable for extraction tests.")

;; A top-level list for list thing tests.
(defcustom fixture-epsilon '(one two three four five)
  "A list value for extraction tests."
  :type '(repeat symbol))

(defvar some-other-var 2
  "Another variable for extraction tests.")

;;; functions.el ends here
