(defface org-transclusion-keyword
  '((((class color) (min-colors 88) (background light))
     :foreground "#0030b4")
    (((class color) (min-colors 88) (background dark))
     :foreground "#34cfff")
    (t
     :foreground "darkgray"))
  "Face for transclude keyword."
  :group 'org-transclusion)

(defun org-transclusion-font-lock-set ()
  "."
  (add-to-list 'org-font-lock-extra-keywords
	       '(org-transclusion-fontify-meta-lines-and-blocks) 'append))

(defun org-transclusion-fontify-meta-lines-and-blocks (limit)
  (let ((case-fold-search t)
	(regexp "\\(^[ \t]*#\\+TRANSCLUDE:\\)\\(.*$\\)")
	(beg)(end)(keyword-end))
    (when (re-search-forward regexp limit t)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq keyword-end (match-end 1))
      (remove-text-properties beg end
			      '(font-lock-fontified t face org-meta-line))
      (add-text-properties beg keyword-end
			   '(font-lock-fontified t
			     face org-transclusion-keyword))
      (save-excursion
	(goto-char beg)
	(org-activate-links end)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-transclusion-font-lock-set)
