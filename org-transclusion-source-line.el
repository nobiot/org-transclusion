(push "src-lines" org-transclusion-add-at-point-functions)
(add-hook 'org-transclusion-get-keyword-values-hook
          #'org-transclusion-keyword-get-value-lines)
(add-hook 'org-transclusion-get-keyword-values-hook
          #'org-transclusion-keyword-get-value-src)

(defun org-transclusion--match-src-lines (_path plist)
  "Check if \"src-lines\" can be used for the PATH.
Returns non-nil if check is pass."
  (when (plist-get plist :lines) t))

(defun org-transclusion--add-src-lines (path plist)
  "Use PATH to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.
TODO need to handle when the file does not exist.  The logic to
pars n-m for :lines is taken from
`org-export--inclusion-absolute-lines' in ox.el."
  (let ((buf (find-file-noselect path))
        (src-lang (plist-get plist :src)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let* ((str (plist-get plist :lines))
              (lines (split-string str "-"))
              (lbeg (string-to-number (car lines)))
              (lend (string-to-number (cadr lines)))
              (beg (if (zerop lbeg) (point-min)
                     (goto-char (point-min))
                     (forward-line (1- lbeg))
                     (point)))
              (end (if (zerop lend) (point-max)
                     (goto-char beg)
                     (forward-line (1- lend))
                     (point)))
              (content))
         (if (not src-lang)
             (setq content (buffer-substring-no-properties beg end))
           (setq content
                 (concat
                  (format "#+begin_src %s\n" src-lang)
                  (buffer-substring-no-properties beg end)
                  "\n"
                  "#+end_src")))
         (list :tc-content content
               :tc-beg-mkr (set-marker (make-marker) beg)
               :tc-end-mkr (set-marker (make-marker) end)))))))

(defun org-transclusion-keyword-get-value-lines (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'."
  (when (string-match ":lines +\\([0-9]*-[0-9]*\\)" string)
    (list :lines (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-get-value-src (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'."
  (when (string-match ":src\\(?: +\\(.*\\)\\)?" string)
    (list :src (org-strip-quotes (match-string 1 string)))))

;; TODO This needs to be customizable..
(defun org-transclusion-keyword-plist-to-string (plist)
  "Convert a keyword PLIST to a string."
  (let ((active-p (plist-get plist :active-p))
        (link     (plist-get plist :link))
        (level    (plist-get plist :level))
        (lines    (plist-get plist :lines)) ;;<<
        (src      (plist-get plist :src))) ;;<< need to add
    (concat "#+transclude: "
            (symbol-name active-p)
            " " link
            (when level (format " :level %d" level))
            (when lines (format " :lines %s" lines)) ;;<< Need to add
            (when src (format " :src %s" src))
            "\n")))
