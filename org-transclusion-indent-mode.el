;;; org-transclusion-indent-mode.el --- support org-indent-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc.

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
;; Last modified: 02 January 2025

;;; Commentary:
;;  This file is part of Org-transclusion
;;  URL: https://github.com/nobiot/org-transclusion

;;; Code:

(require 'org-transclusion)
(require 'org-indent)
(declare-function org-transclusion-within-transclusion-p
                  "org-transclusion")

;;;###autoload
(define-minor-mode org-transclusion-indent-mode ()
  :lighter nil
  :global t
  :group 'org-transclusion
  (if org-transclusion-indent-mode
      (org-transclusion-extension-functions-add-or-remove
       org-transclusion-indent-extension-functions)
    (org-transclusion-extension-functions-add-or-remove
     org-transclusion-indent-extension-functions :remove)))

(defvar org-transclusion-indent-extension-functions
  '((cons 'org-transclusion-after-add-functions
          #'org-translusion-indent-add-properties))
  "Alist of functions to activate `org-transclusion-indent-mode'.
CAR of each cons cell is a symbol name of an abnormal hook
\(*-functions\). CDR is either a symbol or list of symbols, which
are names of functions to be called in the corresponding abnormal
hook.")

(defun org-translusion-indent-add-properties (beg end)
  "BEG END."
  (when org-indent-mode
    (advice-add #'org-indent-set-line-properties
                :override
                #'org-transclusion-indent-set-line-properties-ad)
    (org-indent-add-properties beg end)
    (advice-remove #'org-indent-set-line-properties
                   #'org-transclusion-indent-set-line-properties-ad)))

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
