;;; org-transclusion-menu.el --- Menu for Org-transclusion -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

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
;; Created: 23 April 2023
;; Last modified: 23 April 2023

;;; Commentary:

;; This library is part of Org-tralclusion.  It provides a menu for
;; Org-translcusion.

;;; Code:

;;;; Requirements

;;;; Customization

(defgroup org-transclusion nil
  "Insert text contents by way of link references."
  :group 'org
  :prefix "org-transclusion-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-transclusion")
  :package-version '("Org-transclusion" . "1.0.0"))

(defcustom org-transclusion-menu t
  "Provides menu for `org-transclusion' when non-nil.
Deactivate it when you do not wish to see the menu and
context-menu for `org-transcluson'."
  :type 'boolean)

;;;; Commands / interactive functions

;;   n/a org-transclusion-keyword-value-link
;;   x    org-transclusion-keyword-value-level
;;        org-transclusion-keyword-value-disable-auto
;;   x    org-transclusion-keyword-value-only-contents
;;        org-transclusion-keyword-value-exclude-elements
;;        org-transclusion-keyword-value-expand-links
;;   n/a  org-transclusion-keyword-current-indentation)
;;   TODO org-transclusion-src commands

(defun org-transclusion-menu-level ()
  "Update/add the \":level\" property to \"#+transclude\" at point.\
You will be promted to enter a number from 1 to 9.  Any other
values will not be accepted."
  (interactive)
  (org-transclusion-menu-keyword-update-at-point
   (lambda (plist)
     (let ((level (read-number
                   "Set org-transclusion content headline level \(1-9\): ")))
       (if (or (< level 0) (> level 9))
           (user-error (format "Level %d must be 1-9 \(inclusive\)" level))
         (plist-put plist :level level))))))

(defun org-transclusion-menu-only-contents ()
  (interactive)
  (org-transclusion-menu-keyword-update-at-point
   (lambda (plist)
     (plist-put plist :only-contents t))))

;;;; Functions

(defun org-transclusion-menu-keyword-update-at-point (function)
  "Update the keyword string of Org-transclusion at point.
This function is a utilitiy function to be used to create a new
interactive function to update a property and value in the
\"#+transclude\" keyword at point.  See examples in
`org-transclusion-menu-level' and
`org-transclusion-menu-only-contents' on how it can be used.

FUNCTION must take PLIST as a single argument, which is a return
value of function
`org-transclusion-keyword-string-to-plist'. FUNCTION is then
meant to modify it via `plist-put' to add a new prop-value pair
or modify an existing one,  and return the modified plist.

If FUNCTION is nil, this function does not modify PLIST and uses
the current PLIST as called by
`org-transclusion-keyword-string-to-plist'."
  (save-excursion
    (if (not (org-transclusion-transclusion-keyword-p))
        (user-error "The point is not on \"#+transclude\" line")
      (save-excursion
        (let* ((current-plist (org-transclusion-keyword-string-to-plist))
               (new-plist (if function (funcall function current-plist)
                            current-plist)))
          (org-transclusion-keyword-remove)
          (insert (org-transclusion-keyword-plist-to-string new-plist)))))))

(provide 'org-transclusion-menu)
;;; org-transclusion-menu.el ends here
