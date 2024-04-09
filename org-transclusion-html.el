;;; org-transclusion-html.el --- Converting HTML content to Org -*- lexical-binding: t; -*-

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

;; This is an extension to `org-transclusion'.  When active, it enables
;; transclusion of HTML files by converting HTML to Org with Pandoc.
;; When a link anchor is specified only the HTML headings matching are
;; transcluded.  Does not support live-sync.

;; Requires Pandoc to be installed and in the $PATH.  Conversion of
;; HTML to Org using Pandoc inspired by `org-web-tools'.


;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'pcase)
(require 'dom)

;;;; Hook into org-transclusion

(add-hook 'org-transclusion-add-functions #'org-transclusion-add-html-file)

;;;; Functions

;;;;; Add HTML file

(defun org-transclusion-add-html-file (link plist)
  "Return a list for HTML file LINK object and PLIST.
Return nil if not found."
  (and (string= "file" (org-element-property :type link))
       (or (string-suffix-p ".html" (org-element-property :path link))
           (with-current-buffer (find-file-noselect
                                 (org-element-property :path link) t)
             (org-transclusion-html--html-p (current-buffer))))
       (append '(:tc-type "html-org-file")
               (org-transclusion-html-org-file-content link plist))))

(defun org-transclusion-html-org-file-content (link _plist)
  "Return payload list without :tc-type.
:src-content value will be Org format converted from HTML at LINK."
  (let* ((path (org-element-property :path link))
         (html-buf (find-file-noselect path t))
         (org-buf
          (generate-new-buffer
           (format " *org-transclusion-html-org %s*" (expand-file-name path))))
         (src-content
          (with-current-buffer org-buf
            (insert-buffer-substring html-buf)
            ;; TODO: It's not currently possible to link an HTML
            ;; anchor inside of a 'file:' Org link, but if it ever
            ;; becomes possible, we can use this:

            ;; (let ((dom (with-current-buffer html-buf
            ;;              (libxml-parse-html-region))))
            ;;   (when (dom-by-id dom (format "\\`%s\\'" target))
            ;;     ;; Page contains id element matching link target.
            ;;     (erase-buffer)
            ;;     (dom-print (org-transclusion-html--target-content dom target))))
            (org-transclusion--insert-org-from-html-with-pandoc)
            (buffer-string))))
    (with-current-buffer html-buf
      (org-with-wide-buffer
       (list :src-buf (current-buffer)
             :src-beg (point-min)
             :src-end (point-max)
             :src-content src-content)))))

;;;;; Utilities

(defun org-transclusion-html--target-content (dom target)
  "Return DOM element(s) that correspond to the TARGET.
Since anchors may refer to headings but not the text following
the heading, this function may not return the expected element.

While is not possible to specify an HTML anchor in a file: Org
link, this function is useful in other libraries for transcluding
sections of HTML documents linked via http://, hyper://, etc.."
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

(defun org-transclusion-html--html-p (buffer)
  "Return non-nil if BUFFER is visiting an HTML file."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; Assume DOCTYPE is within the first 5 lines
      (search-forward "!DOCTYPE html" (pos-eol 5) t))))

;;;;; Copied/Adapted from `org-web-tools'

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
          (delete-region (match-beginning 0) (match-end 0)))))))

;;;; Footer

(provide 'org-transclusion-html)

;;; org-transclusion-html.el ends here
