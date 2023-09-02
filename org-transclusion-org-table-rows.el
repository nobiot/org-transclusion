;;; org-transclusion-org-table-rows.el --- Extension -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'org-element)

;;;; Setting up the extension

;; Add a new transclusion type
;;; Not applicable. Reuse existing Org add functions
;; Keyword values
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-org-table-rows)

;; plist back to string
(add-hook 'org-transclusion-keyword-plist-to-string-functions
          #'org-transclusion-keyword-plist-to-string-org-table-rows)

;; Transclusion content formating
;;; Not applicable. Org

;; Open source buffer
;;; Not applicable. Org

;; Live-sync
;;; Not applicable. Org

;; Org-content-filter
(add-hook 'org-transclusion-content-filter-org-functions
          #'org-transclusion-content-filter-org-table-rows-function)

(defun org-transclusion-keyword-value-org-table-rows (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional \"1-10\"."
  (when (string-match ":table-rows +\\(\"?[0-9]*-[0-9]*\"?\\)" string)
    (list :table-rows (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-plist-to-string-org-table-rows (plist)
  "Convert a keyword PLIST to a string.
This function is meant to be used as an extension for function
`org-transclusion-keyword-plist-to-string'.  Add it to the
abnormal hook
`org-transclusion-keyword-plist-to-string-functions'."
  (let ((table-rows (plist-get plist :table-rows)))
    (concat
     (when table-rows (format ":table-rows %s" table-rows)))))

(defun org-transclusion-content-filter-org-table-rows-function (obj plist)
  "TODO Document string.
Currently ALWAYS remove the 3rd row (hard coded)."
  (when-let ((table-rows (plist-get plist :table-rows)))
    (let ((counter 0))
      (org-element-map obj 'table
        (lambda (table)
          (org-element-map table 'table-row
            (lambda (row)
              ;; Currently always remove the third row (hard coded)
              ;; This should be changed
              (setq counter (1+ counter))
              (if (eq counter 3) (org-element-extract-element
                                  row))))
          table)))
    obj))

(provide 'org-transclusion-org-table-rows)
;;; org-transclusion-org-table-rows.el ends here.
