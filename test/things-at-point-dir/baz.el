;;; baz.el --- test -*- lexical-binding: t; -*-

(defun bar (arg)
  "Documentation for bar."
  arg)

;; Comments between defuns.
(defun foo ()
  "Documentation for this function."
  (bar (let ((fuzz (buzz)))       ;<id:1234567890>
         (baz fuzz))))

(defun quux (a b c)
  "A third function for multi-defun extraction."
  (+ a b c))

(defvar baz-var '(alpha beta gamma)
  "A variable after the functions.")

;;; baz.el ends here
