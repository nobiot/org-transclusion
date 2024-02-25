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
           ((org-transclusion-http-html-p (current-buffer))  ; HTML
            (let ((dom (libxml-parse-html-region)))
              (when (dom-by-id dom (format "\\`%s\\'" target))
                ;; Page contains id element matching link target.
                (erase-buffer)
                (dom-print (org-transclusion-http--target-content dom target)))
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

(defun org-transclusion-http--target-content (dom target)
  "Return DOM element(s) that correspond to the TARGET.
Since anchors may refer to headings but not the text following
the heading, this function may not return the expected element."
  ;; HTML link fragments (targets) point to a specific point in a document,
  ;; not a range of text.  This function attempts to guess what range of
  ;; text a target refers to based on what HTML element is targeted.
  ;; See <https://github.com/alphapapa/org-web-tools/issues/72>.
  (let ((id-element (car (dom-by-id dom (format "\\`%s\\'" target)))))
    (pcase (car id-element)
      ((and (or 'h1 'h2 'h3 'h4 'h5 'h6)
            target-heading)
       ;; If the HTML element is a heading, include it and subsequent
       ;; sibling elements until next heading of same level or higher.
       (let* ((siblings (dom-children (dom-parent dom id-element)))
              (heading-position (cl-position id-element siblings))
              (next-heading-position
               (cl-position
                nil siblings
                :start (1+ heading-position)
                :test (lambda (_a b)
                        (and (not (stringp b))
                             (pcase (car b)
                               ((and (or 'h1 'h2 'h3 'h4 'h5 'h6)
                                     subsequent-heading)
                                (not (string>
                                      (symbol-name target-heading)
                                      (symbol-name subsequent-heading))))))))))
         (append '(div ())  ; Wrap in div so all elements are rendered
                 (cl-subseq siblings heading-position
                            (when next-heading-position
                              (1+ next-heading-position))))))
      ('dt
       ;; Include <dt> and subsequent <dd> element.
       ;; TODO: Consider using next-sibling combinator with
       ;; `esxml-query' once it's supported.
       (let* ((siblings (dom-children (dom-parent dom id-element)))
              (dt-position (cl-position id-element siblings))
              (subsequent-dd-position
               (cl-position
                nil siblings
                :start (1+ dt-position)
                :test (lambda (_a b) (and (not (stringp b))
                                     (eq 'dd (car b)))))))
         (append '(div ())  ; Wrap in div so all elements are rendered
                 (cl-subseq siblings dt-position
                            (when subsequent-dd-position
                              (1+ subsequent-dd-position))))))
      ('nil ; Invalid target: Return whole dom.
       dom)
      (_ ; Any other valid target: Return it.
       id-element))))

;;;;; Helpers

(defun org-transclusion-http-html-p (buffer)
  "Return non-nil if BUFFER is visiting an HTML file."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; Assume DOCTYPE is within the first 5 lines
      (search-forward "!DOCTYPE html" (pos-eol 5) t))))

;;;;;; Copied/Adapted from `org-web-tools'

(defun org-transclusion--insert-org-from-html-with-pandoc (&optional buffer)
  "Replace current HTML contents of BUFFER with Org with Pandoc.
When nil, BUFFER defaults to current buffer."
  ;; Based on `org-web-tools--html-to-org-with-pandoc'.
  (with-current-buffer (or buffer (current-buffer))
    (unless (zerop (call-process-region
                    (point-min) (point-max) "pandoc" t t nil
                    "--wrap=none" "-f" "html-raw_html-native_divs" "-t" "org"))
      ;; TODO: Add error output, see org-protocol-capture-html
      (error "Pandoc failed"))
    (org-mode)
    (org-transclusion--clean-pandoc-output)))

(defun org-transclusion--clean-pandoc-output ()
  "Remove unwanted things in current buffer of Pandoc output."
  (org-transclusion--remove-bad-characters)
  (org-transclusion--remove-html-blocks)
  (org-transclusion--remove-custom_id_properties))

(defun org-transclusion--remove-bad-characters ()
  "Remove unwanted characters from current buffer."
  (save-excursion
    (cl-loop for (re . replacement) in '(("" . ""))
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match replacement))))))

(defun org-transclusion--remove-html-blocks ()
  "Remove \"#+BEGIN_HTML...#+END_HTML\" blocks from current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (optional "\n")
                                  "#+BEGIN_HTML"
                                  (minimal-match (1+ anything))
                                  "#+END_HTML"
                                  (optional "\n"))
                              nil t)
      (replace-match ""))))

(defun org-transclusion--remove-custom_id_properties ()
  "Remove property drawers containing CUSTOM_ID properties.
This is a blunt instrument: any drawer containing the CUSTOM_ID
property is removed, regardless of other properties it may
contain.  This seems to be the best course of action in current
Pandoc output."
  (let ((regexp (org-re-property "CUSTOM_ID" nil nil)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (when (org-at-property-p)
          (org-back-to-heading)
          ;; As a minor optimization, we don't bound the search to the current
          ;; entry.  Unless the current property drawer is malformed, which
          ;; shouldn't happen in Pandoc output, it should work.
          (re-search-forward org-property-drawer-re)
          (setf (buffer-substring (match-beginning 0) (match-end 0)) ""))))))

;;;; Footer

(provide 'org-transclusion-http)

;;; org-transclusion-http.el ends here
