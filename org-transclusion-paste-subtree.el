(defun org-transclusion-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.

The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree.

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done.

When REMOVE is non-nil, remove the subtree from the clipboard."
  (interactive "P")
  (setq tree (or tree (and kill-ring (current-kill 0))))
  (unless (org-kill-is-subtree-p tree)
    (user-error
     (substitute-command-keys
      "The kill is not a (set of) tree(s).  Use `\\[yank]' to yank anyway")))
  (org-with-limited-levels
   (let* ((visp (not (org-invisible-p)))
	  (txt tree)
	  (old-level (if (string-match org-outline-regexp-bol txt)
			 (- (match-end 0) (match-beginning 0) 1)
		       -1))
	  (force-level
	   (cond
	    (level (prefix-numeric-value level))
	    ;; When point is after the stars in an otherwise empty
	    ;; headline, use the number of stars as the forced level.
	    ((and (org-match-line "^\\*+[ \t]*$")
		  (not (eq ?* (char-after))))
	     (org-outline-level))
	    ((looking-at-p org-outline-regexp-bol) (org-outline-level))))
	  (previous-level
	   (save-excursion
	     (org-previous-visible-heading 1)
	     (if (org-at-heading-p) (org-outline-level) 1)))
	  (next-level
	   (save-excursion
	     (if (org-at-heading-p) (org-outline-level)
	       (org-next-visible-heading 1)
	       (if (org-at-heading-p) (org-outline-level) 1))))
	  (new-level (or force-level (max previous-level next-level)))
	  (shift (if (or (= old-level -1)
			 (= new-level -1)
			 (= old-level new-level))
		     0
		   (- new-level old-level)))
	  (delta (if (> shift 0) -1 1))
	  (func (if (> shift 0) #'org-demote #'org-promote))
	  (org-odd-levels-only nil)
	  beg end newend)
     ;; Remove the forced level indicator.
     (when (and force-level (not level))
       (delete-region (line-beginning-position) (point)))
     ;; Paste before the next visible heading or at end of buffer,
     ;; unless point is at the beginning of a headline.
     (unless (and (bolp) (org-at-heading-p))
;;       (org-next-visible-heading 1) ;; nobiot removed
       (unless (bolp) (insert "\n")))
     (setq beg (point))
     (when (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
     (insert-before-markers txt)
     (unless (string-suffix-p "\n" txt) (insert "\n"))
     (setq newend (point))
     (org-reinstall-markers-in-region beg)
     (setq end (point))
     (goto-char beg)
     (skip-chars-forward " \t\n\r")
     (setq beg (point))
     (when (and (org-invisible-p) visp)
       (save-excursion (outline-show-heading)))
     ;; Shift if necessary.
     (unless (= shift 0)
       (save-restriction
	 (narrow-to-region beg end)
	 (while (not (= shift 0))
	   (org-map-region func (point-min) (point-max))
	   (setq shift (+ delta shift)))
	 (goto-char (point-min))
	 (setq newend (point-max))))
     (when (or for-yank (called-interactively-p 'interactive))
       (message "Clipboard pasted as level %d subtree" new-level))
     (when (and (not for-yank) ; in this case, org-yank will decide about folding
		kill-ring
		(equal org-subtree-clip (current-kill 0))
		org-subtree-clip-folded)
       ;; The tree was folded before it was killed/copied
       (org-flag-subtree t))
     (when for-yank (goto-char newend))
     (when remove (pop kill-ring)))))
