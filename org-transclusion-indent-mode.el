;;; org-transclusion-indent-mode.el --- support org-indent-mode -*- lexical-binding: t; -*-

;;; Commentary:
;;  This file is part of Org-transclusion

;;; Code:

(require 'org-indent)
(declare-function org-transclusion-within-transclusion-p
                  'org-transclusion)


(defun org-translusion-indent-add-properties (beg end)
  "BEG END."
  (advice-add #'org-indent-set-line-properties
	      :override
	      #'org-transclusion-indent-set-line-properties-ad)
  
  (org-indent-add-properties beg end)
  
  (advice-remove #'org-indent-set-line-properties
		 #'org-transclusion-indent-set-line-properties-ad))

(defun org-transclusion-indent-set-line-properties-ad (level indentation &optional heading)
  "Set prefix properties on current line an move to next one.

LEVEL is the current level of heading.  INDENTATION is the
expected indentation when wrapping line.

When optional argument HEADING is non-nil, assume line is at
a heading.  Moreover, if it is `inlinetask', the first star will
have `org-warning' face."

  (let* ((line (aref (pcase heading
		       (`nil org-indent--text-line-prefixes)
		       (`inlinetask org-indent--inlinetask-line-prefixes)
		       (_ org-indent--heading-line-prefixes))
		     level))
	 (wrap
	  (org-add-props
	      (concat line
		      (if heading (concat (make-string level ?*) " ")
			(make-string indentation ?\s)))
	      nil 'face 'org-indent)))

    ;; Org-transclusion's addition begin
    (when (org-transclusion-within-transclusion-p)
      (setq line
	    (concat line
		    (propertize
		     "x"
		     'display
		     '(left-fringe org-transclusion-fringe-bitmap
				   org-transclusion-fringe))))
      (setq wrap
	    (concat line
		    (propertize
		     "x"
		     'display
		     '(left-fringe org-transclusion-fringe-bitmap
				   org-transclusion-fringe)))))
    ;; Org-transclusion's addition end

    ;; Add properties down to the next line to indent empty lines.
    (add-text-properties (line-beginning-position) (line-beginning-position 2)
			 `(line-prefix ,line wrap-prefix ,wrap)))
  (forward-line))

(provide 'org-transclusion-indent-mode)

;;; org-transclusion-indent-mode.el ends here
