;;; org-transclusion-font-lock.el --- font-lock for Org-transclusion -*- lexical-binding: t; -*-

;;; Commentary:
;;  This file is part of Org-transclusion
;;  URL: https://github.com/nobiot/org-transclusion

;;; Code:

(require 'org)
(add-hook 'org-font-lock-set-keywords-hook #'org-transclusion-font-lock-set)

(defface org-transclusion-keyword
  '((((class color) (min-colors 88) (background light))
     :foreground "#0030b4")
    (((class color) (min-colors 88) (background dark))
     :foreground "#34cfff")
    (t
     :foreground "darkgray"))
  "Face for #+transclude keyword."
  :group 'org-transclusion)

(defun org-transclusion-font-lock-set ()
  "Add font-lock function to Org's hook.
The hook is `org-font-lock-set-keywords-hook'."
  (add-to-list 'org-font-lock-extra-keywords
               '(org-transclusion-fontify-meta-lines-and-blocks) 'append))

(defun org-transclusion-fontify-meta-lines-and-blocks (limit)
  "Override Org's font-lock for #+transclude keyword.
This function does the following:

1. Apply face `org-transclusion-keyword' to #+keyword
2. Re-applies Org's font-lock for links to the transclusion link
3. Apply Org's face `org-meta-line' to transclusion properties

Argument LIMIT is to limit scope of `re-search-forward'; it's the
same with `org-fontify-meta-lines-and-blocks'."
  (let ((case-fold-search t)
        (regexp "\\(^[ 	]*#\\+TRANSCLUDE:\\)\\(.*]]\\)?\\(.*$\\)")
        (beg)(end)(keyword-end)(prop-beg)(prop-end))
    (when (re-search-forward regexp limit t)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq keyword-end (match-end 1))
      (setq prop-beg (match-beginning 3))
      (setq prop-end (match-end 3))
      (remove-text-properties beg end
                              '(font-lock-fontified t face org-meta-line))
      (add-text-properties beg keyword-end
                           '(font-lock-fontified t
                                                 face org-transclusion-keyword))
      (add-text-properties prop-beg prop-end
                           '(font-lock-fontified t
                                                 face org-meta-line))
      (save-excursion
        (goto-char beg)
        (org-activate-links end)))))

(provide 'org-transclusion-font-lock)

;;; org-transclusion-font-lock.el ends here
