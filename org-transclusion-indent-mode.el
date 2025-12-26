;;; org-transclusion-indent-mode.el --- support org-indent-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024  Free Software Foundation, Inc.

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
;; Created: 22 August 2021
;; Last modified: 21 January 2024

;;; Commentary:
;;  This file is part of Org-transclusion
;;  URL: https://github.com/nobiot/org-transclusion
;;
;;  This extension ensures org-indent-mode properties are correctly
;;  applied to transcluded content and refreshed after transclusion
;;  removal. It also preserves fringe indicators in both source and
;;  destination buffers when org-indent-mode regenerates line-prefix
;;  properties.
;;
;;  The timing mechanism for synchronizing with org-indent's asynchronous
;;  initialization is copied from org-modern-indent.

;;; Code:

(require 'org-indent)

;;;; Variables

(defvar-local org-transclusion-indent--timer nil
  "Timer for debounced fringe re-application.")

(defvar-local org-transclusion-indent--last-change-tick nil
  "Buffer modification tick at last fringe application.")

(defvar-local org-transclusion-indent--has-overlays nil
  "Non-nil if buffer has ever had source overlays.
Used to prevent premature mode deactivation during buffer refresh.")

(defvar-local org-transclusion-indent--init nil
  "Initialization state for waiting on org-indent.
Either nil, t (initialized), or (TIMER ATTEMPT-COUNT).")

;;;; Forward Declarations

;; Silence byte-compiler warnings for functions defined in org-transclusion.el
(declare-function org-transclusion--prefix-has-fringe-p "org-transclusion" (prefix))
(declare-function org-transclusion-add-fringes "org-transclusion" (buffer beg end face))
(declare-function org-transclusion-remove-fringes "org-transclusion" (buffer beg end))


;; Variable defined by define-minor-mode later in this file
(defvar org-transclusion-indent-mode)

(defun org-transclusion-indent--find-source-overlays ()
  "Return list of all transclusion source overlays in current buffer."
  (seq-filter
   (lambda (ov) (overlay-get ov 'org-transclusion-by))
   (overlays-in (point-min) (point-max))))

(defun org-transclusion-indent--reapply-all-fringes ()
  "Re-apply fringe indicators to all transcluded regions in buffer.
This function is called after any change that might have removed
`line-prefix' or `wrap-prefix' properties.

In graphical mode, optimizes by checking only the first line of each
overlay region, since org-indent regenerates entire subtrees at once.

In terminal mode, always re-applies fringes to all lines, since
org-indent may regenerate individual lines during typing."
  (when (buffer-live-p (current-buffer))
    (let ((current-tick (buffer-modified-tick))
          (overlays (org-transclusion-indent--find-source-overlays)))
      ;; Track if we have overlays
      (when overlays
        (setq org-transclusion-indent--has-overlays t))

      ;; Only re-apply if buffer actually changed since last application
      (unless (eq current-tick org-transclusion-indent--last-change-tick)
        (setq org-transclusion-indent--last-change-tick current-tick)
        (dolist (ov overlays)
          (let ((ov-beg (overlay-start ov))
                (ov-end (overlay-end ov)))
            (when (and ov-beg ov-end)
              (if (display-graphic-p)
                  ;; Graphical mode: optimize by checking only first line
                  (save-excursion
                    (goto-char ov-beg)
                    (let* ((line-beg (line-beginning-position))
                           (line-prefix (get-text-property line-beg 'line-prefix)))
                      (when (and line-prefix
                                 (not (org-transclusion--prefix-has-fringe-p line-prefix)))
                        (org-transclusion-add-fringes
                         (current-buffer) ov-beg ov-end
                         'org-transclusion-source-fringe))))
                ;; Terminal mode: always re-apply to all lines
                (org-transclusion-add-fringes
                 (current-buffer) ov-beg ov-end
                 'org-transclusion-source-fringe)))))))))

(defun org-transclusion-indent--schedule-reapply ()
  "Schedule fringe re-application after a short delay.
This debounces rapid changes to avoid excessive processing.

In graphical mode, uses a shorter delay (0.2s) since bitmap rendering
is fast and flicker-free.  In terminal mode, uses a longer delay (0.7s)
to reduce visible flicker of ASCII fringe indicators during rapid typing."
  (when org-transclusion-indent--timer
    (cancel-timer org-transclusion-indent--timer))
  (setq org-transclusion-indent--timer
        (run-with-idle-timer
         (if (display-graphic-p) 0.2 0.7)  ; Shorter delay for graphical, longer for terminal
         nil
         (lambda (buf)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (org-transclusion-indent--reapply-all-fringes))))
         (current-buffer))))

(defun org-transclusion-indent--after-change (_beg _end _len)
  "Schedule fringe re-application after buffer change.
Added to `after-change-functions' in source buffers."
  (org-transclusion-indent--schedule-reapply))

(defun org-transclusion-indent--check-and-disable ()
  "Disable mode if no source overlays remain in buffer.
Only disables if overlays have been checked and confirmed absent,
not during temporary states like buffer refresh."
  (when (and org-transclusion-indent--has-overlays
             (not (org-transclusion-indent--find-source-overlays)))
    ;; Wait a bit to ensure this isn't just a temporary state
    (run-with-idle-timer
     0.2 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (unless (org-transclusion-indent--find-source-overlays)
             (org-transclusion-indent-mode -1)))))
     (current-buffer))))

(defun org-transclusion-indent--wait-and-init (buf)
  "Wait for org-indent to finish initializing BUF, then apply fringes.
Copied from org-modern-indent's timing mechanism."
  (if (or (not (bound-and-true-p org-indent-agentized-buffers))
          (memq buf org-indent-agentized-buffers))
      ;; org-indent is ready
      (org-transclusion-indent--init buf)
    ;; Still waiting
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if org-transclusion-indent--init
            (let ((cnt (cl-incf (cadr org-transclusion-indent--init))))
              (if (> cnt 5)
                  (progn
                    (message "org-transclusion-indent-mode: Gave up waiting for %s to initialize" buf)
                    (setq org-transclusion-indent--init t))
                (timer-activate
                 (timer-set-time (car org-transclusion-indent--init)
                                 (time-add (current-time) 0.2)))))
          (setq org-transclusion-indent--init
                (list (run-at-time 0.1 nil #'org-transclusion-indent--wait-and-init buf)
                      1)))))))

(defun org-transclusion-indent--init (buf)
  "Initialize indent mode in BUF after org-indent completes.
To be added to `org-indent-post-buffer-init-functions'."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq org-transclusion-indent--init t)
      (org-transclusion-indent--reapply-all-fringes))))

(defun org-transclusion-indent--auto-enable-maybe ()
  "Auto-enable indent mode if source overlays are detected.
Added to `post-command-hook' in `org-mode' buffers with `org-indent-mode'."
  (when (and (not org-transclusion-indent-mode)
             (org-transclusion-indent--find-source-overlays))
    (org-transclusion-indent-mode +1)))

;;;; Destination Buffer Support

(defun org-transclusion-indent--add-properties-and-fringes (beg __end)
  "Ensure org-indent properties and fringe indicators in transcluded region.
BEG and END are approximate bounds; we find actual bounds from text properties.

When org-indent-mode is active, `org-indent-add-properties' overwrites
the uniform `line-prefix' and `wrap-prefix' properties set by the main
package, removing fringe indicators. This function re-applies fringes
by appending them to org-indent's indentation prefixes."
  (when org-indent-mode
    ;; Find actual transclusion bounds using text properties
    ;; The transclusion that was just added should be at or near BEG
    (save-excursion
      (goto-char beg)
      ;; Search forward for org-transclusion-type property
      (when-let* ((match (text-property-search-forward 'org-transclusion-type))
                  (actual-beg (prop-match-beginning match))
                  (actual-end (prop-match-end match)))
        ;; Apply org-indent properties and fringes to actual bounds
        (org-indent-add-properties actual-beg actual-end)
        (org-transclusion-add-fringes
         (current-buffer) actual-beg actual-end 'org-transclusion-fringe)))))

(defun org-transclusion-indent--refresh-source-region (src-buf src-beg src-end)
  "Refresh org-indent properties in source region after transclusion removal.
SRC-BUF is the source buffer, SRC-BEG and SRC-END are the region bounds.

For `org-mode' buffers with `org-indent-mode', refreshes indentation properties.
For non-org buffers, removes fringe indicators that were added during
transclusion."
  (with-current-buffer src-buf
    (if (buffer-local-value 'org-indent-mode src-buf)
        ;; Org buffer with indent-mode: refresh properties
        (progn
          (org-indent-add-properties src-beg src-end)
          (when (and (boundp 'org-transclusion-indent-mode)
                     org-transclusion-indent-mode)
            (org-transclusion-indent--check-and-disable)))
      ;; Non-org buffer or org buffer without indent-mode: just remove fringes
      (org-transclusion-remove-fringes src-buf src-beg src-end))))

;;;; Minor Mode Definition

;;;###autoload
(define-minor-mode org-transclusion-indent-mode
  "Minor mode for org-indent-mode support in org-transclusion.

This mode serves two purposes:

1. In destination buffers: ensures org-indent properties are applied
   and fringe indicators are preserved when org-indent overwrites them.

2. In source buffers: preserves fringe indicators when org-indent-mode
   regenerates `line-prefix' properties.

The mode auto-activates in source buffers when transclusion source
overlays are detected, and auto-deactivates when all transclusions
are removed."
  :init-value nil
  :lighter " OT-Indent"
  :group 'org-transclusion
  (if org-transclusion-indent-mode
      (progn
        ;; Install hooks for source buffer fringe preservation
        (add-hook 'after-change-functions
                  #'org-transclusion-indent--after-change nil t)

        ;; Register with org-indent or wait for it
        (cond
         ;; Already initialized before, just toggle
         ((or (called-interactively-p 'any) org-transclusion-indent--init)
          (org-transclusion-indent--init (current-buffer)))
         ;; Register with buffer init hook if available
         ((boundp 'org-indent-post-buffer-init-functions)
          (add-hook 'org-indent-post-buffer-init-functions
                    #'org-transclusion-indent--init nil t))
         ;; Fallback: wait for org-indent
         (t (org-transclusion-indent--wait-and-init (current-buffer)))))

    ;; Cleanup
    (remove-hook 'after-change-functions
                 #'org-transclusion-indent--after-change t)
    (when (boundp 'org-indent-post-buffer-init-functions)
      (remove-hook 'org-indent-post-buffer-init-functions
                   #'org-transclusion-indent--init t))
    (when org-transclusion-indent--timer
      (cancel-timer org-transclusion-indent--timer)
      (setq org-transclusion-indent--timer nil))
    (when (and (listp org-transclusion-indent--init)
               (timerp (car org-transclusion-indent--init)))
      (cancel-timer (car org-transclusion-indent--init)))
    (setq org-transclusion-indent--has-overlays nil
          org-transclusion-indent--init nil)))

;;;###autoload
(defun org-transclusion-indent-mode-setup ()
  "Set up auto-activation of `indent mode' in `org-mode' buffers.
Adds `post-command-hook' to detect when source overlays appear."
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p org-indent-mode))
    (add-hook 'post-command-hook
              #'org-transclusion-indent--auto-enable-maybe nil t)))

;; Auto-setup in org-mode buffers - add late to hook like org-modern-indent
(add-hook 'org-mode-hook #'org-transclusion-indent-mode-setup 90)

;;;; Hook Registration

(add-hook 'org-transclusion-after-add-functions
          #'org-transclusion-indent--add-properties-and-fringes)
(add-hook 'org-transclusion-after-remove-functions
          #'org-transclusion-indent--refresh-source-region)

(provide 'org-transclusion-indent-mode)

;;; org-transclusion-indent-mode.el ends here
