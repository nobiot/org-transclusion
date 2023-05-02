(defun bar (arg)
  "Documentation for bar"
  arg)


;; Comments
(defun foo ()
  "Documentation for this function."
  (bar (let ((fuzz (buzz))) ;<id:1234567890>
            (baz fuzz)))
