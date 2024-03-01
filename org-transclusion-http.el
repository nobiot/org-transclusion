;;; org-transclusion-http.el --- Transclude over HTTP -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functionality related to transcluding content over HTTP
;; using plz.el.  Features include:
;;
;; - Transclude plain text
;;   + Transclude only Org headings matching search options
;; - Transclude HTML converted to Org using Pandoc, a lá `org-web-tools'
;;   + Transclude only HTML headings matching link anchor
;; - TODO: Support :lines

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'org-transclusion)
(require 'org-transclusion-html)
(require 'cl-lib)
(require 'pcase)
(require 'plz)

;;;; Functions

(defun org-transclusion-http-add-file (link _plist)
  "Return callback function when HTTP transclusion is appropriate.
Otherwise, return nil.  Intended to be added to
`org-transclusion-add-functions', which see for descriptions of
arguments LINK and PLIST."
  (pcase (org-element-property :type link)
    ((or "http" "https")
     (message "Asynchronously transcluding over HTTP at point %d, line %d..."
              (point) (org-current-line))
     #'org-transclusion-http-add-callback)))

(add-hook 'org-transclusion-add-functions #'org-transclusion-http-add-file)

(defun org-transclusion-http-add-callback (link plist copy)
  "Load HTTP file at LINK and call
`org-transclusion-add-callback' with PAYLOAD, LINK, PLIST, COPY."
  (pcase-let* ((target-mkr (point-marker))
               (url (org-element-property :raw-link link))
               ((cl-struct url filename target) (url-generic-parse-url url))
               (tc-type))
    (plz 'get url
      :as 'buffer
      :then
      (lambda (_response-buffer)
        (when-let ((target-buf (marker-buffer target-mkr)))
          (cond
           ((org-transclusion-html--html-p (current-buffer))  ; HTML
            (let ((dom (libxml-parse-html-region)))
              (when (dom-by-id dom (format "\\`%s\\'" target))
                ;; Page contains id element matching link target.
                (erase-buffer)
                (dom-print (org-transclusion-html--target-content dom target)))
              (org-transclusion--insert-org-from-html-with-pandoc)
              ;; Use "org-http" `tc-type' since HTML is converted to Org mode.
              (setf tc-type "org-http")))
           ((org-transclusion-org-file-p filename) ; Org-mode
            ;; FIXME: filename may contain a query string, so it may not end
            ;; with "org" or "org.gpg".  For example,
            ;; https://example.com/foobar.org?query=answer has the filename
            ;; /foobar.org?query=answer and therefore doesn't match.
            (when target
              (org-mode)
              (let ((org-link-search-must-match-exact-headline t))
                (when (with-demoted-errors "org-transclusion-http error:\n%s\ntranscluding whole file..."
                        (org-link-search (format "#%s" target)))
                  (org-narrow-to-subtree))))
            (setf tc-type "org-http"))
           (t  ; All other file types
            (setf tc-type "others-http")))
          (let* ((payload-without-type
                  (org-transclusion-content-org-buffer-or-element nil plist))
                 (payload (append `(:tc-type ,tc-type) payload-without-type)))
            (with-current-buffer target-buf
              (org-with-wide-buffer
               (goto-char (marker-position target-mkr))
               (org-transclusion-add-callback payload link plist copy)))))))))

;;;; Footer

(provide 'org-transclusion-http)

;;; org-transclusion-http.el ends here
