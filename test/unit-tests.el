;;; unit-tests.el --- Description -*- lexical-binding: t; -*-
;; org-transclusion unit tests
;; To run the tests you can use: M-x ert RET t RET
;; You can read more about ert here: https://www.gnu.org/software/emacs/manual/html_node/ert/index.html
(require 'org-transclusion)

(ert-deftest org-transclusion-org-file-p-test ()
  "Tests org-transclusion-org-file-p against string inputs."
  (should (equal (org-transclusion-org-file-p "test.org.gpg") t))
  (should (equal (org-transclusion-org-file-p "test.org") t))
  (should (equal (org-transclusion-org-file-p "test.gpg") nil))
  (should (equal (org-transclusion-org-file-p "no-extention") nil))
  (should (equal (org-transclusion-org-file-p ".org") nil))
  (should (equal (org-transclusion-org-file-p ".gpg") nil))
  (should (equal (org-transclusion-org-file-p ".") nil))
  (should (equal (org-transclusion-org-file-p "") nil)))

;;; unit-tests.el ends here
