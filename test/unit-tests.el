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


(ert-deftest org-transclusion-test-noweb-chunk ()
  "Test `:noweb-chunk' property."
  (let* ((filename-test "test-noweb-chunk.org")
         (filename-expected "test-noweb-chunk-detached.org")
         (buf-test (progn (find-file filename-test)
                          (org-transclusion-add-all) ;transclude explicitly
                          (get-buffer filename-test)))
         (buf-expected (progn (find-file filename-expected)
                              (get-buffer filename-expected))))
    (should (equal (compare-buffer-substrings
                    buf-test nil nil
                    buf-expected nil nil)
                   0))
    ;; clean up
    (kill-buffer buf-test)
    (kill-buffer buf-expected)
    (kill-buffer "test-noweb-chunk-1.nw")
    (kill-buffer "test-noweb-chunk-2.nw")
    (kill-buffer "test-noweb-chunk-3.nw")
    (kill-buffer "test-noweb-chunk-4.nw")
    ))


(defun org-transclusion-detach-all ()
  "Detach all transclusions.
Used to created the `..-detached.org' files for unit testing."
  (interactive)
  (save-restriction
    (let ((current-marker (move-marker (make-marker) (point)))
          match list)
      (widen)
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'org-transclusion-id))
        (goto-char (prop-match-beginning match))
        (org-transclusion-detach))
      (goto-char current-marker)
      (move-marker current-marker nil) ; point nowhere for GC
      list)))


;;; unit-tests.el ends here
