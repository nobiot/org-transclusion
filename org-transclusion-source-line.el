(push "src-lines" org-transclusion-add-at-point-functions)
(add-hook 'org-transclusion-get-keyword-values-hook
          #'org-transclusion-keyword-get-value-lines)


(defun org-transclusion--match-src-lines (_path plist)
  "Check if \"src-lines\" can be used for the PATH.
Returns non-nil if check is pass."
  (when (plist-get plist :lines) t))

(defun org-transclusion--add-src-lines (path plist)
  "Use PATH to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.
TODO need to handle when the file does not exist."
  (let ((buf (find-file-noselect path)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let ((content (buffer-string))
             (beg (point-min-marker))
             (end (point-max-marker)))
         (list :tc-content content
               :tc-beg-mkr beg
               :tc-end-mkr end))))))

(defun org-transclusion-keyword-get-value-lines (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'."
  (when (string-match ":lines +\\([0-9]*-[0-9]*\\)" string)
    (list :lines (org-strip-quotes (match-string 1 string)))))

;; TODO This needs to be customizable..
(defun org-transclusion-keyword-plist-to-string (plist)
  "Convert a keyword PLIST to a string."
  (let ((active-p (plist-get plist :active-p))
        (link (plist-get plist :link))
        (level (plist-get plist :level))
        (lines (plist-get plist :lines))) ;;<< need to add
    (concat "#+transclude: "
            (symbol-name active-p)
            " " link
            (when level (format " :level %d" level))
            (when lines (format " :lines %s" lines)) << Need to add
            "\n")))
