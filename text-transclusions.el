(defvar text-clone-modify-overlays-functions nil
  "Argument passed is a list of clone overlays.")

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
                    (goto-char (- oe tail))
                    (if (not (> mod-beg (point)))
                        (progn
                          (save-excursion (insert str))
                          (delete-region mod-beg (point)))
                      (user-error "No live-sync done. The text strings in the overlays are not identical"))))))))))))

(defun text-clone-make-overlay (beg end &optional buf)
  "Wrapper for make-ovelay.
BEG and END can be point or marker.  Optionally BUF can be passed.
FRONT-ADVANCE is nil, and REAR-ADVANCE is t."
  (make-overlay beg end buf nil t))

(defun text-clone-create-overlays (&rest overlays)
  "

OOV :: Overlay for Original (original overlay)
COVS :: Overlays for Clone (clone overlay)

The distinction can be arbitary but can differentiate the
original overlay from the clones passed to this function. For
instance, if you would like to put a different faces for them to
visually differentiate them."
  (if (or (not overlays)
          (> 2 (length overlays)))
      (user-error "Nothing done. You need to pass 2 or more overlays")
    (dolist (ov overlays)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'text-clones overlays)
      (overlay-put ov 'modification-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'insert-in-front-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'insert-behind-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'priority -50))
    (run-hook-with-args 'text-clone-modify-overlays-functions overlays)))

(add-hook 'text-clone-modify-overlays-functions 'my/add-overlays)

(defun my/add-overlays (ovs)
  (let ((overlays ovs))
    (overlay-put (pop overlays) 'face 'org-transclusion-source-edit)
    (dolist (overlay overlays)
      (overlay-put overlay 'face 'org-transclusion-edit))))

;; (overlay-put tc-ov 'tc-type "src-edit-ov")
;; (overlay-put tc-ov 'tc-paired-src-edit-ov src-ov)

(defun test-text-clone ()
  (interactive)
  (let ((src-buf (find-file-noselect "./test.txt"))
        (cbuf2 (find-file-noselect "./test-clone-2.txt"))
        src-beg src-end src-content src-ov clone-ov
        cov2)
    (with-current-buffer src-buf
      ;; front-advanced should be nil
      (setq src-beg (point-min))
      (setq src-end (point-max))
      (setq src-content (buffer-string))
      (setq src-ov (text-clone-make-overlay src-beg src-end)))
    (with-current-buffer cbuf2
      (insert src-content)
      (setq cov2 (text-clone-make-overlay (point-min) (+ (point-min) src-end))))
    (setq clone-ov (text-clone-make-overlay (point) (+ (point) src-end)))
    (save-excursion (insert src-content))
    (text-clone-create-overlays src-ov clone-ov cov2)))
