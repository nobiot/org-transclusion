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
;;  removal.

;;; Code:

(require 'org-indent)

(defun org-transclusion-indent--add-properties (beg end)
  "Ensure org-indent properties exist in transcluded region.
BEG and END are the transcluded region bounds.

The main package adds uniform fringe indicators to transcluded content
via text properties. This function ensures org-indent-mode's indentation
properties are applied if org-indent-mode is active, but does not modify
the fringe indicators."
  (when org-indent-mode
    ;; Ensure org-indent properties exist
    (org-indent-add-properties beg end)))

(defun org-transclusion-indent--refresh-source-region (src-buf src-beg src-end)
  "Refresh org-indent properties in source region after transclusion removal.
SRC-BUF is the source buffer, SRC-BEG and SRC-END are the region bounds.
This ensures visual indentation updates immediately when org-indent-mode
is active."
  (when (buffer-local-value 'org-indent-mode src-buf)
    (with-current-buffer src-buf
      (org-indent-add-properties src-beg src-end))))

;; Register hooks when extension loads
(add-hook 'org-transclusion-after-add-functions
          #'org-transclusion-indent--add-properties)
(add-hook 'org-transclusion-after-remove-functions
          #'org-transclusion-indent--refresh-source-region)

(provide 'org-transclusion-indent-mode)

;;; org-transclusion-indent-mode.el ends here
