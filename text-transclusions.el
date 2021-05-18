(defvar text-transclusion--maintaining nil)

(defun text-transclusion--maintain (ol1 after beg end &optional _len)
  "Propagate the changes made under the overlay OL1 to the other clones.
This is used on the `modification-hooks' property of text clones."
  (when (and after ;(not undo-in-progress) ;; < nobit removed undo-in-progress
             (not text-transclusion--maintaining)
             (overlay-start ol1))
    (let ((margin (if (overlay-get ol1 'text-clone-spreadp) 1 0)))
      (setq beg (max beg (+ (overlay-start ol1) margin)))
      (setq end (min end (- (overlay-end ol1) margin)))
      (when (<= beg end)
        (save-excursion
          (when (overlay-get ol1 'text-clone-syntax)
            ;; Check content of the clone's text.
            (let ((cbeg (+ (overlay-start ol1) margin))
                  (cend (- (overlay-end ol1) margin)))
              (goto-char cbeg)
              (save-match-data
                (if (not (re-search-forward
                          (overlay-get ol1 'text-clone-syntax) cend t))
                    ;; Mark the overlay for deletion.
                    (setq end cbeg)
                  (when (< (match-end 0) cend)
                    ;; Shrink the clone at its end.
                    (setq end (min end (match-end 0)))
                    (move-overlay ol1 (overlay-start ol1)
                                  (+ (match-end 0) margin)))
                  (when (> (match-beginning 0) cbeg)
                    ;; Shrink the clone at its beginning.
                    (setq beg (max (match-beginning 0) beg))
                    (move-overlay ol1 (- (match-beginning 0) margin)
                                  (overlay-end ol1)))))))
          ;; Now go ahead and update the clones.
          (let ((head (- beg (overlay-start ol1)))
                (tail (- (overlay-end ol1) end))
                (str (buffer-substring beg end))
                (nothing-left t)
                (text-transclusion--maintaining t))
            (dolist (ol2 (overlay-get ol1 'text-clones))
              (with-current-buffer (overlay-buffer ol2) ;;< Tobias
                (save-restriction
                  (widen)
                  ;(outline-show-all)
                  (let ((oe (overlay-end ol2)))
                    (unless (or (eq ol1 ol2) (null oe))
                      (setq nothing-left nil)
                      (let ((mod-beg (+ (overlay-start ol2) head)))
                        ;;(overlay-put ol2 'modification-hooks nil)
                        (goto-char (- (overlay-end ol2) tail))
                        (unless (> mod-beg (point))
                          (save-excursion (insert str))
                          (delete-region mod-beg (point)))
                        ;;(overlay-put ol2 'modification-hooks '(text-clone--maintain))
                        ))))))
            (if nothing-left (delete-overlay ol1))))))))

(defun org-transclusion--text-clone--maintain (ol1 after beg end &optional _len)
  "Propagate the change made under the overlay OL1 to the other paired clone.
This is used on the `modification-hooks' property of text clones.
AFTER, BEG, and END are the fixed args for `modification-hooks'
and friends in an overlay."
  (when (and after ;(not undo-in-progress) ;; < nobit removed undo-in-progress
             (not org-transclusion--text-clone-maintaining)
             (overlay-start ol1))
    (let ((margin (if (overlay-get ol1 'text-clone-spreadp) 1 0)))
      (setq beg (max beg (+ (overlay-start ol1) margin)))
      (setq end (min end (- (overlay-end ol1) margin)))
      (when (<= beg end)
        (save-excursion
          ;; Remove text-clone-syntax case; we don't use it.
          ;; Now go ahead and update the clones.
          (let ((head (- beg (overlay-start ol1)))
                (tail (- (overlay-end ol1) end))
                (str (buffer-substring-no-properties beg end)) ;changed to no-properties
                (org-transclusion--text-clone-maintaining t))
            (dolist (ol2 (overlay-get ol1 'text-clones))
              (with-current-buffer (overlay-buffer ol2) ;;< Tobias
                (save-restriction
                  (widen)
                                        ;(outline-show-all)
                  (let ((oe (overlay-end ol2)))
                    (unless (or (eq ol1 ol2) (null oe))
                      (setq nothing-left nil)
                      (let ((mod-beg (+ (overlay-start ol2) head)))
                        ;;(overlay-put ol2 'modification-hooks nil)
                        (goto-char (- (overlay-end ol2) tail))
                        (if (not (> mod-beg (point)))
                            (progn
                              (save-excursion (insert str))
                              (delete-region mod-beg (point)))
                          (user-error "No live-sync.  The source and transclusion are not identical"))
                        ;;(overlay-put ol2 'modification-hooks '(text-clone--maintain))
                        ))))))))))))

(defun text-transclusion--maintain-create (start end &optional spreadp syntax)
  "Create a text clone of START...END at point.
Text clones are chunks of text that are automatically kept identical:
changes done to one of the clones will be immediately propagated to the other.

  The buffer's content at point is assumed to be already identical to
  the one between START and END.
  If SYNTAX is provided it's a regexp that describes the possible text of
  the clones; the clone will be shrunk or killed if necessary to ensure that
  its text matches the regexp.
  If SPREADP is non-nil it indicates that text inserted before/after the
  clone should be incorporated in the clone."
  ;; To deal with SPREADP we can either use an overlay with `nil t' along
  ;; with insert-(behind|in-front-of)-hooks or use a slightly larger overlay
  ;; (with a one-char margin at each end) with `t nil'.
  ;; We opted for a larger overlay because it behaves better in the case
  ;; where the clone is reduced to the empty string (we want the overlay to
  ;; stay when the clone's content is the empty string and we want to use
  ;; `evaporate' to make sure those overlays get deleted when needed).
  ;;
  (let* ((clone-buf (or (and (markerp start) (marker-buffer start))
                        (current-buffer)))
         (pt-end (+ (point) (- end start)))
         (start-margin (if (or (not spreadp) (bobp) (with-current-buffer clone-buf (<= start (point-min))))
                           0 1))
         (end-margin (if (or (not spreadp)
                             (>= pt-end (point-max))
                             (with-current-buffer clone-buf (>= start (point-max))))
                         0 1))
         ;; FIXME: Reuse overlays at point to extend dups!
         (ol1 (make-overlay (- start start-margin) (+ end end-margin) clone-buf t)) ;;< Tobias

         (ol2 (make-overlay (- (point) start-margin) (+ pt-end end-margin) nil t))
         (dups (list ol1 ol2)))
    (overlay-put ol1 'modification-hooks '(text-transclusion--maintain)) ;;< nobiot
    (when spreadp (overlay-put ol1 'text-clone-spreadp t))
    (when syntax (overlay-put ol1 'text-clone-syntax syntax))
    ;;(overlay-put ol1 'face 'underline)
    (overlay-put ol1 'evaporate t)
    (overlay-put ol1 'face 'org-transclusion-source-block) ;; < nobiot
    (overlay-put ol1 'text-clones dups)
    ;;
    (overlay-put ol2 'modification-hooks '(text-transclusion--maintain)) ;;< Tobias
    (when spreadp (overlay-put ol2 'text-clone-spreadp t))
    (when syntax (overlay-put ol2 'text-clone-syntax syntax))
    ;;(overlay-put ol2 'face 'underline)
    (overlay-put ol2 'evaporate t)
    (overlay-put ol2 'face 'org-transclusion-block) ;; < nobiot
    (overlay-put ol2 'text-clones dups)
    dups)) ;; < nobiot return dups
