;;; org-transclusion.el --- transclude text contents of linked target -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (org "9.3")

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library is an attempt to enable transclusion with Org Mode.

;; It is still VERY experimental. As it modifies your files (notes),
;; use it with care. The author and contributors cannot be held
;; responsible for loss of important work.

;;; Code:
(require 'org)
(require 'org-element)
(require 'org-id)

;;-----------------------------------------------------------------------------
;; Variables
;; Most of these should be defcustom
(defvar org-transclusion-link "ortc")
(defvar org-transclusion-activate-persistent-message t)

;;-----------------------------------------------------------------------------
;; Custom link parameter
;; :follow fn should be a one that can do-list for functions, where
;; each function support different type of links: e.g. file, ID, etc.
(org-link-set-parameters
 org-transclusion-link
 :follow #'org-transclusion-call-tranclusions-functions
 :help-echo "Cannot edit transclusion link unless you remove the whole transclusion")

;;-----------------------------------------------------------------------------
;; Functions
;; - Deal with transclusion links
;; - Retrive text contents of the link target buffer / element
;; - Check if the transclusion link has bee aleady been activated

(defun org-transclusion-call-tranclusions-functions (path &rest _)
  "Call functions to insclude source text for PATH in current buffer.
It is meant to be used as a :follow function in the custom Org Mode link type.
You should be able to (dolist func-list) to allow for cusutom functions.
At the moment, I have only one function, even without a list of functions for
the prototyping purpose."
  
  (org-transclusion--add-at-point path))

(defun org-transclusion--get-link-location ()
  "Get the start and end of the link being worked on at point.
If the current point is a translusion link, return BEGIN and END
   plist: '(begin: BEGIN  end: END)
of the link.  If not link, return nil."

  (let ((location '())
        (context (org-element-context)))
    (when-let ((link (plist-get context 'link)))
      (setq location (plist-put location ':begin (plist-get link ':begin)))
      (setq location (plist-put location ':end (plist-get link ':end)))
      location)))

(defun org-transclusion--split-path-and-id (path)
  "Chech if PATH is the form `id:uuid`.  If so, return the ID.
If not return nil."
  (when (string-prefix-p "id:" path)
    (string-match "\\(id:\\)\\([[:alnum:]|-]*\\)" path)
    (match-string 2 path)))

(defun org-transclusion--get-buf-and-pos-of-source (path)
  "Return buffer and marker (beginning of headline) for PATH.
Assume path is either a valid link to a file, or an reachable ID in Org Mode.
Return plist in the form of '(:buf BUF :marker MARKER).
When ID is nil, the link is for the whole beuffer and :marker is nil."

  (if-let ((id (org-transclusion--split-path-and-id path))
           (marker (org-id-find id 'marker))
           (buf (marker-buffer marker)))
      (list :buf buf :marker marker)

    (list :buf (find-file-noselect path) :marker nil)))

(defun org-transclusion--yank-source-to-target (buf marker)
  "Retrieve and yank at point the text content specified by BUF and MARKER.
Assume when MARKER is non-nil, it always points to the beginning of a headline."
  
  ;; Assume the buffer is Org file -- org-narrow-to-subtree used to narrow
  ;; FIXME?
  ;; This means, at the moment, the error when you cannot narrow is not properly
  ;; treated (no text returned).
  
  (let ((marker marker)
        (targetbuf (current-buffer)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (when marker
         (goto-char marker)
         (org-narrow-to-subtree))
       (let ((tempbuf (current-buffer)))
         (set-buffer targetbuf)
         (insert-buffer-substring-as-yank tempbuf))))))

(defun org-transclusion--create-at-point (path raw-link buf marker)
  "Create transclusion for PATH, storing RAW-LINK, BUF and MARKER in overlay.
Assume the RAW-LINK is a valid tranclusion link."
  (when-let ((link-loc (org-transclusion--get-link-location))
             (link-beg (plist-get link-loc ':begin))
             (link-end (plist-get link-loc ':end)))
    (goto-char link-beg)
    (delete-region link-beg link-end)
    ;; FIXME You need to check if the link is at the bottom of buffer
    ;; If it is, then yank won't work.
    (let* ((beg (point))
           (end nil)
           (ov nil))
      ;;(forward-line)
      (org-transclusion--yank-source-to-target buf marker)
      ;; FIXME The following is not necessary if there is no source buf.
      (setq end (point))
      (setq ov (make-overlay beg end nil t nil))
      ;; It is important to flag FRONT-ADVANCE t when making an overlay.  It
      ;; ensures that the location of overlay is not shifted for the remove
      ;; function.when a new line is added back to allow space for the original
      ;; tranclusion link for the remove function.
      (overlay-put ov 'face 'secondary-selection)
      (overlay-put ov 'path path)
      (overlay-put ov 'tc-src-buf buf)
      (overlay-put ov 'tc-src-marker marker)
      (overlay-put ov 'tc-raw-link raw-link)
      (overlay-put ov 'priority -50)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'help-echo (concat "transclusion for: " raw-link)))))

(defun org-transclusion--transclusion-link-p ()
  "Check if the link at point is a tranclusion link."
  (when-let ((link (plist-get (org-element-context) 'link)))
    (when-let ((type (plist-get link ':type)))
      (string= type org-transclusion-link))))

(defun org-transclusion--add-at-point (path)
  "Add atranclusion for a PATH.

It is meant to be called from an Org custom link with using :follow property.
Assume the link is already checked to be for tranclusion."
  
  (cond ((cdr (get-char-property-and-overlay (point) 'tc-src-buf))
         ;; The link is within a transclusion overlay.
         ;; Do nothing to avoid recurrsive transclusion.
         nil)
        (t
         (when-let ((link (plist-get (org-element-context) 'link)))
           (let* ((path path)
                  (raw-link (progn
                              (let ((beg (plist-get link ':begin))
                                    (end (plist-get link ':end)))
                                (buffer-substring-no-properties beg end))))
                  (buf_marker (org-transclusion--get-buf-and-pos-of-source path))
                  (buf (plist-get buf_marker ':buf))
                  (marker (plist-get buf_marker ':marker)))
             (save-excursion
               (org-transclusion--create-at-point path raw-link buf marker)))))))

(defun org-transclusion-update-src-at-point (pos &optional savebuf)
  "Update the transclusion source buffer with the tranclusion at POS.
It can be used interactively.

TODO allow C-u to manually do savebuf

When SAVEBUF is non-nil, save the source buffer to file.  When SAVEBUF is nil,
only update buffer without saving it to the file."
  
  (interactive "d")
  ;; Check POS has an tranclusion overlay
  ;; If not, get out the function with a message telling the user.
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-src-buf))))
      (let ((edited-content (buffer-substring (overlay-start ov) (overlay-end ov)))
            (src_buf (overlay-get ov 'tc-src-buf))
            (src_marker (overlay-get ov 'tc-src-marker)))
        (with-current-buffer src_buf
          ;;(undo-boundary)
          (org-with-wide-buffer
           (cond (src_marker
                  (goto-char src_marker)
                  (org-narrow-to-subtree))
                 (t (goto-char (point-min))))
           (delete-region (point-min) (point-max))
           (let ((expecting-bol (bolp)))
             ;; This insersion of "\n" comes from org-edit-src-exit org-edit-src-save.
             (insert edited-content)
             (when (and expecting-bol (not (bolp))) (insert "\n")))
           (when savebuf
             (unless make-backup-files (setq-local make-backup-files t))
             (save-buffer)))))
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-remove-at-point (pos &optional detach)
  "Remove transclusion and the copied text around POS.
When DETACH is non-nil, remove the tranclusion overlay only, keeping the copied
text."
  
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-src-buf))))
      (save-excursion
        (save-restriction
          ;; Bring back the transclusion link.
          ;; Show all the folded parts is needed
          ;; as the transcluded heading might have folded
          ;; texts outside the tranclusion overlay
          (widen)
          (outline-show-all)
          (let* ((beg (overlay-start ov))
                 (raw-link (overlay-get ov 'tc-raw-link))
                 (new-beg (progn
                            (goto-char beg)
                            (newline)
                            (forward-line -1)
                            (insert raw-link)
                            (forward-line)
                            (point)))
                 (new-end (overlay-end ov)))
            (delete-overlay ov)
            ;; When remove fn, delete the copied texts
            (unless detach
              (delete-region new-beg new-end)))))
    ;; The message below is common for remove and detach
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-detach-at-point (pos)
  "Detach the transclusion at POS, removing the overlay only.
It needs remove the link type as well, otherwise, when the tranclusion
is active, it will automatically bring the transclusion back."
  (interactive "d")
  (org-transclusion-remove-at-point pos t)
  ;; this OR is necessary to check if the function is called at
  ;; the beginning of the overlay.
  (let* ((link (or (org-element-link-parser)
                   (progn (org-next-link t)
                          (org-element-link-parser))))
         (end (org-element-property :end link))
         (type (concat org-transclusion-link ":")))
    (search-forward type end t 1)
    (delete-char (- 0 (length type)))))

;;-----------------------------------------------------------------------------
;; Function
;; - For the whole buffer to iterate on all the links, etc.
;; - Some can be used interactively

(defun org-transclusion--process-all-in-buffer-before-save ()
  "Update and remove all translusions in the current buffer `before-save-hook'."
  (org-transclusion-update-all-src-in-buffer) ; no saving, just insert the new content
  (org-transclusion-remove-all-in-buffer)) ; clean up current buffer before writing to file)

(defun org-transclusion--process-all-in-buffer-after-save ()
  "Add tranclusions back into current buffer, and save source buffers.
Meant obe for `after-save-hook'.
It adds all the transcluded copies back into the current buffer.
And then saves all the transclusion source buffers."
  (org-transclusion-add-all-in-buffer) ; put all tranclusions back in
  (org-transclusion-update-all-src-in-buffer t)) ; save to file
  
(defun org-transclusion-update-all-src-in-buffer (&optional savebuf)
  "Update all transclusion sources from the current buffer.
When SAVEBUF is non-nil, call `org-transclusion-update-src-at-point' with non-nil SAVEBUF.
This saves the updated buffer to file."

  (interactive)
  (save-restriction
    (widen)
    (outline-show-all)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (org-transclusion-update-src-at-point (overlay-start ov) savebuf))))

(defun org-transclusion-add-all-in-buffer ()
  "Add all the transclusions in the current buffer.
As this should be used only when the buffer is current, no argment passed.

As transclusing adds text after the link, the loop needs to process from top to
one by one.  The transcluded text may contrain transclusion link.  To avoid
infinite,check is done within each add function."
  
  (interactive)
  ;; Prevent background hook (e.g. save hooks) from updating the transclusion
  ;; target buffer.
  (when (eq (current-buffer)(window-buffer (selected-window)))
    (save-excursion
      (save-restriction
        (widen)
        (outline-show-all)
        (goto-char (point-min))
        ;; eq t is needed for this while loop as if not link, fn returns a message string.
        (while (eq t (org-next-link))
          ;; check if the link at point is tranclusion link
          (when (org-transclusion--transclusion-link-p)
            (let* ((link (org-element-link-parser))
                   (path (org-element-property :path link)))
              (org-transclusion-call-tranclusions-functions path))))))))

(defun org-transclusion-remove-all-in-buffer (&optional buf)
  "Remove all the translusion overlay and copied text in current buffer.
Caller can pass BUF to specify which BUF needs to remove transclusions.
This feature is meant for `org-transclusion--toggle-transclusion-when-out-of-focus'."
  
  (interactive)
  (when buf (set-buffer buf))
  (save-excursion
    (save-restriction
      (widen)
      (outline-show-all)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let ((pos (overlay-start ov)))
          (org-transclusion-remove-at-point pos))))))

;;-----------------------------------------------------------------------------
;; Functions
;; - Activate / deactivate
;; - Toggle translusions when in and out of transclusion buffer

(defun org-transclusion-activate ()
  "Activate automatic transclusions in the local buffer.
This should be a buffer-local minior mode.  Not done yet."
  (interactive)
  (if (memq 'org-transclusion--toggle-transclusion-when-out-of-focus
            window-selection-change-functions)
      (run-with-idle-timer 0 nil 'message
                           "Nothing done. Transclusion is aleady active.")
    (setq-local window-selection-change-functions
                (push 'org-transclusion--toggle-transclusion-when-out-of-focus
                      window-selection-change-functions))
    (add-hook 'before-save-hook #'org-transclusion--process-all-in-buffer-before-save nil t)
    (add-hook 'after-save-hook #'org-transclusion--process-all-in-buffer-after-save nil t)
    (when org-transclusion-activate-persistent-message
      (setq header-line-format
	    "Transclusion active in this buffer"))))

(defun org-transclusion-deactivate ()
  "Deactivate automatic transclusions in the local buffer."
  ;; Consider keeping the tc copies as read-only to be able to read
  ;; or for export mode
  (interactive)
  (if (memq 'org-transclusion--toggle-transclusion-when-out-of-focus
            window-selection-change-functions)
      (progn
        (org-transclusion-remove-all-in-buffer)
        (setq-local window-selection-change-functions
                    (remove 'org-transclusion--toggle-transclusion-when-out-of-focus
                            window-selection-change-functions))
        (remove-hook 'before-save-hook #'org-transclusion--process-all-in-buffer-before-save t)
        (remove-hook 'after-save-hook #'org-transclusion--process-all-in-buffer-after-save t)
        (when org-transclusion-activate-persistent-message
          (setq header-line-format nil)))
    (run-with-idle-timer 0 nil 'message
                         "Nothing done. Transclusion is not active.")))
  
(defun org-transclusion--toggle-transclusion-when-out-of-focus (win)
  "Detect focus state of window WIN, and toggle tranclusion on and off.
The toggling is done via adding and removing all from the appropirate buffer,
depending on whether the focus is coming in or out of the tranclusion buffer."
  (let ((buf (window-buffer win)))
    (cond ((minibufferp (current-buffer))
           (message "going into minibuffer"))
          ((eq buf (current-buffer))
           (message "coming into %s" win)
           (org-transclusion-add-all-in-buffer))
          (t
           (message "going from %s into %s" buf (current-buffer))
           (with-current-buffer buf
             (org-transclusion-update-all-src-in-buffer)) ;; update from copy to source
           (org-transclusion-remove-all-in-buffer buf))))) ;; clean up copy

(provide 'org-transclusion)
;;; org-transclusion.el ends here
