(defvar text-clone-modify-overlays-functions nil
  "Argument passed is a list of clone overlays.")

(defvar text-clone-overlays nil)

(defvar text-clone-live-sync-in-progress nil)

(defun text-clone-make-overlay (beg end &optional buf)
  "Wrapper for make-ovelay.
BEG and END can be point or marker.  Optionally BUF can be passed.
FRONT-ADVANCE is nil, and REAR-ADVANCE is t."
  (make-overlay beg end buf nil t))

(defun text-clone-set-overlays (&rest overlays)
  "Add text-clone properties to OVERLAYS.
This function directly modifies the OVERLAYS passed and returns
modified OVERLAYS as a list in the same order as passed to it.
No copy is produced.

This function also refreshes `text-clone-overlays' as a
side-effect to keep track of the current text-clone overlays,
which is primarily used for clean up when you exit live-sync.

This function does not explicitly differentiate overlays for the
orginal text region and its clones.  Where such distinction is
important, use the sequence of OVERLAYS list; for example, the
first element of the list can be the overlay for the original and
rest, clones.  You can use the abnormal hook functions
`text-clone-modify-overlays-functions' to further modify the
overlays.  The hook passes overlays after this function has added
the properties.  For instance, you can put different faces for
them to visually differentiate them."
  (if (or (not overlays)
          (> 2 (length overlays)))
      (user-error "Nothing done. You need to pass 2 or more overlays")
    (setq text-clone-overlays nil)
    (dolist (ov overlays)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'text-clones overlays)
      (overlay-put ov 'modification-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'insert-in-front-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'insert-behind-hooks
                   '(text-clone-live-sync))
      (overlay-put ov 'priority -50)
      ;; Add a local post-command-hook for each overlay buffer
      (with-current-buffer (overlay-buffer ov)
        (add-hook 'post-command-hook #'text-clone-post-command-h nil t)))
    (run-hook-with-args 'text-clone-modify-overlays-functions overlays)
    (setq text-clone-overlays overlays)
    overlays))

(defun text-clone-post-command-h ()
  "Delete all the text-clone overlays when any one is non-existent."
  (when-let ((ovs text-clone-overlays))
    (let ((deleted nil))
      (dolist (ov ovs)
        (unless (or deleted
                    (overlay-buffer ov))
          (setq deleted t)))
      (when deleted
        (text-clone-delete-overlays)))))

(defun text-clone-delete-overlays ()
  "Delete all live-sync overlays.
As side-effects, this function also does the following to clean
up text-clone:

- Remove the local post-command-hook
  `text-clone-post-command-h' for text-clone each overlay

- Reset tracking of text-clone overlays by setting
  `text-clone-overlays' to nil"
  (when text-clone-overlays
    (dolist (ov text-clone-overlays)
      ;; Clean up the local post-command-hook
      (when (overlay-buffer ov)
        (with-current-buffer (overlay-buffer ov)
          (remove-hook 'post-command-hook
                       #'text-clone-post-command-h t)))
      (delete-overlay ov))
    (setq text-clone-overlays nil)))

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
                      (user-error "No live-sync done.  \
The text strings in the overlays are not identical"))))))))))))
