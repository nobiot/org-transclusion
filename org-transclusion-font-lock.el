;;; org-transclusion-font-lock.el --- font-lock for Org-transclusion -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Free Software Foundation, Inc.

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
;; Last modified: 03 January 2026

;;; Commentary:
;;  This file is part of Org-transclusion
;;  URL: https://github.com/nobiot/org-transclusion
;;
;;  By default, this library does not need to be explicitly configured as it
;;  will be automatically activated via `org-transclusion-extensions' (part of
;;  `org-transclusion') (`org-transclusion-font-lock' is set to be active by
;;  default).
;;
;;  Alternatively, you can set it up without loading the whole
;;  `org-transclusion' library.
;;
;;  (use-package org-transclusion-font-lock
;;   :after org
;;   :config (org-transclusion-font-lock-mode +1))

;;; Code:

(require 'org)

;;;###autoload
(define-minor-mode org-transclusion-font-lock-mode ()
  :lighter nil
  :global t
  :group 'org-transclusion
  (if org-transclusion-font-lock-mode
      (add-hook 'org-mode-hook #'org-transclusion-font-lock-set)
    (remove-hook 'org-mode-hook #'org-transclusion-font-lock-set)))

(defface org-transclusion-keyword
  '((((class color) (min-colors 88) (background light))
     :inherit font-lock-keyword-face)
    (((class color) (min-colors 88) (background dark))
     :inherit font-lock-keyword-face)
    (t :inherit font-lock-keyword-face))
  "Face for #+transclude keyword."
  :group 'org-transclusion)

(defun org-transclusion-font-lock-set ()
  "Add font-lock function to Org's hook.
The hook is `org-font-lock-set-keywords-hook'."
  (font-lock-add-keywords nil '(org-transclusion-fontify-meta-lines-and-blocks)))

(defun org-transclusion-fontify-meta-lines-and-blocks (limit)
  "Override Org's font-lock for #+transclude keyword.
This function does the following:

1. Apply face `org-transclusion-keyword' to #+keyword
2. Re-applies Org's font-lock for links to the transclusion link
3. Apply Org's face `org-meta-line' to transclusion properties

Argument LIMIT is to limit scope of `re-search-forward'; it's the
same with `org-fontify-meta-lines-and-blocks'."
  (let ((case-fold-search t)
        (regexp "\\(^[  ]*#\\+TRANSCLUDE:\\)\\(.*]]\\)?\\(.*$\\)")
        (beg)(end)(keyword-end)(prop-beg)(prop-end))
    (when (re-search-forward regexp limit t)
      (setq beg (match-beginning 0))
      (setq end (match-end 0))
      (setq keyword-end (match-end 1))
      (setq prop-beg (match-beginning 3))
      (setq prop-end (match-end 3))
      (remove-text-properties beg end
                              '(font-lock-fontified t face org-meta-line))
      (add-text-properties beg keyword-end
                           '(font-lock-fontified t
                                                 face org-transclusion-keyword))
      (add-text-properties prop-beg prop-end
                           '(font-lock-fontified t
                                                 face org-meta-line))
      (save-excursion
        (goto-char beg)
        (org-activate-links end)))))

(provide 'org-transclusion-font-lock)

;;; org-transclusion-font-lock.el ends here
