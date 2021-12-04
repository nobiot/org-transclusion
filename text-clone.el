;;; text-clone.el --- clone and live-sync text -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Noboru Ota <me@nobiot.com>
;; Created: 22 May 2021
;; Last modified: 4 December 2021

;; Keywords: text-clone, transclusion, org-transclusion

;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;  This file is part of Org-transclusion
;;  URL: https://github.com/nobiot/org-transclusion

;;;; Credits

;; It is an extention of text-clone functions written as part of GNU Emacs in
;; subr.el.  The first adaption to extend text-clone functions to work across
;; buffers was published in StackExchange by the user named Tobias in March
;; 2020. It can be found at https://emacs.stackexchange.com/questions/56201/
;; is-there-an-emacs-package-which-can-mirror-a-region/56202#56202

;; Noboru Ota has made further adaptions for version 0.0.1 in order for it
;; to work with the Org-transclusion package.

;;; Code:

;;;; Variables

(defvar text-clone-overlays nil
  "Global variable to keep track of all the text-clone.
overlays.  Used primarily by `text-clone-delete-overlays'.")

(defvar text-clone-live-sync-in-progress nil
  "Global varible used by `text-clone-live-sync' function.")

;;;; Functions

(defun text-clone-make-overlay (beg end &optional buf)
  "Wrapper for `make-ovelay' to standardize the parameters passed to it.
BEG and END can be point or marker.  Optionally BUF can be
passed.  FRONT-ADVANCE is nil, and REAR-ADVANCE is t."
  (make-overlay beg end buf nil t))

(defun text-clone-set-overlays (&rest overlays)
  "Add text-clone properties to OVERLAYS.
This function directly modifies the OVERLAYS passed and returns
modified OVERLAYS as a list in the same order as passed to it.
No copy is produced.

This function also refreshes `text-clone-overlays' as a
side-effect to keep track of the current text-clone overlays,
which is primarily used to clean up text-clone overlays with
`text-clone-delete-overlays'.

This function does not explicitly differentiate overlays for the
orginal text region and its clones.  Where such distinction is
important, use the sequence of OVERLAYS list; for example, the
first element of the list can be the overlay for the original and
rest, clones.

As this function returns modified overlays, a calling function
can further modify them.  For instance, you can put different
faces to visually differentiate them."
  (if (or (not overlays)
          (> 2 (length overlays)))
      (user-error "Nothing done.  You need to pass 2 or more overlays")
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
  "Remove all live-sync overlays.
Return a list of the buffer, beginning and ending points of the
deleted overlays.  Each element of the list is in this structure:

    (buf (beg . end))

This function checks `text-clone-overlays' for overlays being
tracked.  Return nil if there is no overlay in it.

As side-effects, this function also does the following to clean
up text-clone:

- Remove the local `post-command-hook'
  `text-clone-post-command-h' for text-clone each overlay

- Reset tracking of text-clone overlays by setting
  `text-clone-overlays' to nil"
  (when text-clone-overlays
    (let ((ovs text-clone-overlays)
          deleted-overlays)
      (dolist (ov ovs)
        ;; Clean up the local post-command-hook
        (let ((element (list (overlay-buffer ov) (cons (overlay-start ov) (overlay-end ov)))))
          (push element deleted-overlays)
          (when (overlay-buffer ov)
            (with-current-buffer (overlay-buffer ov)
              (remove-hook 'post-command-hook
                           #'text-clone-post-command-h t)

          (delete-overlay ov)))))
      (setq text-clone-overlays nil)
      ;; As push is used to construct the list, the sequence needs to be reversed
      (nreverse deleted-overlays))))

(defun text-clone-live-sync (ol1 after beg end &optional _len)
  "Propagate the change made under the overlay OL1 to the other paired clone.
This is used on the `modification-hooks' property of text clones.
AFTER, BEG, and END are the fixed args for `modification-hooks'
and friends in an overlay.

It's a simplified version of the orignal `text-clone--maintain'.
This function does not use SPREADP or SYNTAX (both defined in
`text-clone-create').

Overlay is also assumed to be always SPREADP but insteaf we opt
for (nil t) -- refer to `text-clone-make-overlay'.  This enables
tighter overlay size and has `post-command-hook' to deal with the
case when one of the overlays is deleted (refer to
`text-clone-post-command-h').

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

(provide 'text-clone)
;;; text-clone.el ends here
