;;; org-transclusion-src-lines.el --- Extension -*- lexical-binding: t; -*-

;;; Commentary:
;; (add-to-list 'org-transclusion-extensions 'org-transclusion-plus)
;;
;;; Code:

(require 'org-element)
;; (declare-function text-clone-make-overlay 'text-clone)
;; (declare-function org-transclusion-live-sync-buffers-others-default
;;                   'org-transclusion)

;;;; Customization
(defcustom org-transclusion-plus-enable-cache t
  "Define whether to enable plus use cache file."
  :type 'boolean
  :group 'org-transclusion)

;;;; Setting up the extension

;; Add a new transclusion type
(add-hook 'org-transclusion-add-functions
          #'org-transclusion-add-plus)
;; Keyword values
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-plus)
(add-hook 'org-transclusion-keyword-plist-to-string-functions
          #'org-transclusion-keyword-plist-to-string-plus)

;; Transclusion content formating
;; Not needed. Default works for text files.


;;; Functions

(defun org-transclusion-add-plus (link plist)
  "Return a list for non-Org text and source file.
Determine add function based on LINK and PLIST.

Return nil if PLIST does not contain \":plus\" properties."
  (cond
   ((plist-get plist :plus)
    (append '(:tc-type "plus")
            (org-transclusion-plus-payload link plist)))
   )
  )

(defun org-transclusion-plus-payload (link plist)
  "Return a payload processed with default procedure use the file
that created by plus ."
  (let* ((work-file (org-transclusion-plus-content link plist))
         (new-link (concat "[[file:" work-file "]]"))
         (plist (plist-put plist :cache-file work-file))  ;; reserve for live edit
         (new-plist (copy-sequence plist))
         (new-plist (plist-put new-plist :link new-link))
         (new-link (org-transclusion-wrap-path-to-link new-link))
         (org-transclusion-add-functions
          (remove 'org-transclusion-add-plus org-transclusion-add-functions ))
         (payload (run-hook-with-args-until-success
                   'org-transclusion-add-functions new-link new-plist)))

    payload
    ))


(defun org-transclusion-plus-content (link plist)
  "Return a temp file processed with plus eval."
  (let* ((path (org-element-property :path link))
         (r-file (if (file-name-absolute-p path) path (expand-file-name path)))
         (work-file-name (file-name-nondirectory r-file))
         (plus (string-trim (plist-get plist :plus)))
         (cache-id (md5 (concat path plus)))
         (work-dir (concat (temporary-file-directory) cache-id "/"))
         (work-file (expand-file-name work-file-name work-dir))
         (src (cond
               ((string-match "\(.*\)" plus) plus)
               ((string-match "^[^\(\)]+$" plus) (org-babel-expand-noweb-references (org-babel-lob--src-info plus)))
               (t "()")
               ))
         )
    (unless (and org-transclusion-plus-enable-cache
                 (file-exists-p! work-file-name work-dir)
                 (time-less-p
                  (file-attribute-modification-time (file-attributes r-file))
                  (file-attribute-modification-time (file-attributes work-file)))
                 )
      (make-directory work-dir t)
      (copy-file r-file work-dir t)
      (let* ((buf (find-file-noselect work-file)))
             (when buf
               (with-current-buffer buf

                   (goto-char (point-min))
                   (eval (car (read-from-string (format "(progn %s)" src))))
                   (save-buffer)
                   ))
             )
      )

    work-file)
  )



(defun org-transclusion-keyword-value-plus (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are mandatory."
  (when (string-match ":plus +\\([^:]*\\)" string)
    (list :plus (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-plist-to-string-plus (plist)
  "Convert a keyword PLIST to a string.
This function is meant to be used as an extension for function
`org-transclusion-keyword-plist-to-string'.  Add it to the
abnormal hook
`org-transclusion-keyword-plist-to-string-functions'."
  (let ((string nil)
        (plus (plist-get plist :plus))
        )
    (concat string
     (when plus (format ":plus %s" plus))
     )))


(provide 'org-transclusion-plus)
;;; org-transclusion-plus.el ends here
