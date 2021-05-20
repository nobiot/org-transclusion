(defvar text-clone-live-sync-in-progress nil)

(defun text-clone-live-sync (ol1 after beg end &optional _len)
  "Propagate the change made under the overlay OL1 to the other paired clone.
This is used on the `modification-hooks' property of text clones.
AFTER, BEG, and END are the fixed args for `modification-hooks'
and friends in an overlay.

It's a simplified versino of the orignal
`text-clone--maintain'. We do not work with SPREADP or
SYNTAX (both defined in `text-clone-create').

Overlay is also assumed to be always SPREADP but insteaf we opt
for nil t -- tighter overlay size. And have post-command-hook to
deal with the case when either of the overlay is deleted.

This function also works during undo in progress; that is, when
`undo-in-progress' is non-nil."
  (when (and after
             (not text-clone-live-sync-in-progress)
             (overlay-start ol1)
             (<= beg end))
    (save-excursion
      ;; Now go ahead and update the clones.
      (let ((head (- beg (overlay-start ol1)))
            (tail (- (overlay-end ol1) end))
            (str (buffer-substring-no-properties beg end)) ;changed to no-properties
            (text-clone-live-sync-in-progress t))
        (dolist (ol2 (overlay-get ol1 'text-clones))
          (with-current-buffer (overlay-buffer ol2)
            (save-restriction
              (widen)
              (let ((oe (overlay-end ol2)))
                (unless (or (eq ol1 ol2) (null oe))
                  (let ((mod-beg (+ (overlay-start ol2) head)))
                    (goto-char (- (overlay-end ol2) tail))
                    (if (not (> mod-beg (point)))
                        (progn
                          (save-excursion (insert str))
                          (delete-region mod-beg (point)))
                      (user-error "No live-sync done. The text strings in the overlays are not identical"))))))))))))

(defun text-clone-create-overlays (src-beg src-end src-buf)
      (let* ((src-ov (with-current-buffer src-buf
                       ;; front-advanced should be nil
                       (org-transclusion-make-overlay src-beg src-end)))
             (tc-ov (org-transclusion-make-overlay (point) (+ (point) src-end)))
             (dups (list src-ov tc-ov)))
        (put 'text-clone-overlay 'type "source")
        (put 'text-clone-overlay 'evaporate t)
        ;; Source Overlay
        (overlay-put src-ov 'category 'text-clone-overlay)
        ;;(overlay-put src-ov 'evaporate t)
        (overlay-put src-ov 'text-clones dups)
        (overlay-put src-ov 'modification-hooks
                     '(text-clone-live-sync))
        (overlay-put src-ov 'insert-in-front-hooks
                     '(text-clone-live-sync))
        (overlay-put src-ov 'insert-behind-hooks
                     '(text-clone-live-sync))
        (overlay-put src-ov 'face 'org-transclusion-source-edit)
        (overlay-put src-ov 'priority -50)
        ;; Transclusion Overlay
        (overlay-put tc-ov 'modification-hooks
                     '(text-clone-live-sync))
        (overlay-put tc-ov 'insert-in-front-hooks
                     '(text-clone-live-sync))
        (overlay-put tc-ov 'insert-behind-hooks
                     '(text-clone-live-sync))
        (overlay-put tc-ov 'evaporate t)
        (overlay-put tc-ov 'tc-paired-src-edit-ov src-ov)
        (overlay-put tc-ov 'tc-type "src-edit-ov")
        (overlay-put tc-ov 'face 'org-transclusion-edit)
        (overlay-put tc-ov 'text-clones dups)))

(defun test-text-clone ()
  (interactive)
  (let ((buf (find-file-noselect "./test.txt"))
        (src-beg) (src-end) (content))
    (with-current-buffer buf
      (setq src-beg (point-min))
      (setq src-end (point-max))
      (setq content (buffer-string)))
    (save-excursion (insert content))
    (text-clone-create-overlays src-beg src-end buf)))
