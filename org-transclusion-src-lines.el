;;;-*- lexical-binding: t; -*-

;;; Setting up the extension

;; Add a new transclusion type
(add-hook 'org-transclusion-add-functions
          #'org-transclusion-add-src-lines)
;; Keyword values
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-lines)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-src)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-rest)
(add-hook 'org-transclusion-keyword-plist-to-string-functions
          #'org-transclusion-keyword-plist-to-string-src-lines)
;; Transclusion content formating
(add-hook 'org-transclusion-content-format-functions
          #'org-transclusion-content-format-src-lines)
;; Open source buffer
(add-hook 'org-transclusion-open-source-marker-functions
          #'org-transclusion-open-source-marker-src-lines)
;; Live-sync
(add-hook 'org-transclusion-live-sync-buffers-functions
          #'org-transclusion-live-sync-buffers-src-lines)

;;; Functions

(defun org-transclusion-add-src-lines (link plist)
  "Check if \"src-lines\" can be used for the LINK.
Returns non-nil if check is pass."
  (cond
   ((plist-get plist :src)
    (append '(:tc-type "src")
            (org-transclusion-content-src-lines link plist)))
   ((plist-get plist :lines)
    (append '(:tc-type "lines")
            (org-transclusion-content-src-lines link plist)))))

(defun org-transclusion-content-src-lines (link plist)
  "Use PATH to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.
TODO need to handle when the file does not exist.  The logic to
pars n-m for :lines is taken from
`org-export--inclusion-absolute-lines' in ox.el with one
exception.  Instead of :lines 1-10 to exclude line 10, the logic
below has been adjusted to include line 10.  This should be more
intuitive when it comes to including lines of code.

One of the numbers can be omitted. When the first number is
omitted (e.g. -10), it means from the beginning of the file to
line 10. Likewise, when the second number is omitted (e.g. 10-),
it means from line 10 to the end of file.

In order to include a single line, have the the same number in
both places (e.g. 10-10, meaning line 10 only)."
  (let* ((path (org-element-property :path link))
         (search-option (org-element-property :search-option link))
         (buf (find-file-noselect path))
         (src-lines (plist-get plist :lines))
         (src-lang (plist-get plist :src))
         (rest (plist-get plist :rest)))
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (let* ((start-pos (or (when search-option
                                 (save-excursion
                                   (ignore-errors
                                     (org-link-search search-option)
                                     (line-beginning-position))))
                               (point-min)))
                (lines (when src-lines (split-string src-lines "-")))
                (lbeg (if lines (string-to-number (car lines))
                        0))
                (lend (if lines (string-to-number (cadr lines))
                        0))
                (beg (if (zerop lbeg) (point-min)
                       (goto-char start-pos)
                       (forward-line (1- lbeg))
                       (point)))
                (end (if (zerop lend) (point-max)
                       (goto-char start-pos)
                       (forward-line (1- lend))
                       (end-of-line);; include the line
                       ;; Ensure to include the \n into the end point
                       (1+ (point))))
                (content))
           (setq content
                 (concat
                  (when src-lang
                    (concat
                     (format "#+begin_src %s" src-lang)
                     (when rest (format " %s" rest))
                     "\n"))
                  (buffer-substring-no-properties beg end)
                  (when src-lang "#+end_src\n")))
           (list :src-content content
                 :src-buf (current-buffer)
                 :src-beg beg
                 :src-end end)))))))

(defun org-transclusion-keyword-value-lines (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional \"1-10\"."
  (when (string-match ":lines +\\(\"?[0-9]*-[0-9]*\"?\\)" string)
    (list :lines (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-value-src (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are optional :src \"python\"."
  (when (string-match ":src +\\(\"?\\w*\"?\\)" string)
    (list :src (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-value-rest (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are mandatory."
  (when (string-match ":rest +\"\\(.*\\)\"" string)
    (list :rest (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-plist-to-string-src-lines (plist)
  (let ((string)
        (lines (plist-get plist :lines))
        (src (plist-get plist :src))
        (rest (plist-get plist :rest)))
    (concat string
     (when lines (format ":lines %s" lines))
     (when src (format " :src %s" src))
     (when rest (format " :rest \"%s\"" rest)))))

(defun org-transclusion-src-lines-p (type)
  "Return non-nil when TYPE is \"src\" or \"lines\".
Return nil if neither."
  (or (string= type "src")
      (string= type "lines")))

(defun org-transclusion-open-source-marker-src-lines (type)
  "Return marker for `org-transclusion-open-source'."
  (when (org-transclusion-src-lines-p type)
    (get-text-property (point) 'tc-src-beg-mkr)))

(defun org-transclusion-content-format-src-lines (type content)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).
Currently it only re-aligns table with links in the content."
  (when (org-transclusion-src-lines-p type)
    (with-temp-buffer
      (insert content)
      ;; Return the temp-buffer's string
      (buffer-string))))

(defun org-transclusion-live-sync-buffers-src-lines (type)
  "Return cons cell of overlays for source and trasnclusion.
The cons cell to be returned is in this format:

    (src-ov . tc-ov)

This function uses TYPE to identify relevant files; it's meant
for non-Org text files including program source files."
  (when (org-transclusion-src-lines-p type)
    ;; Let's not allow live-sync when source is transcluded into a source block.
    (when (string= "src" type)
      (user-error "No live sync for src-code block"))
    (let* ((tc-pair (get-text-property (point) 'org-transclusion-pair))
           (src-ov (text-clone-make-overlay
                    (overlay-start tc-pair)
                    (overlay-end tc-pair)
                    (overlay-buffer tc-pair)))
           (beg (marker-position (get-text-property (point) 'org-transclusion-beg-mkr)))
           (end (marker-position (get-text-property (point) 'org-transclusion-end-mkr)))
           (tc-ov)
           (context (org-element-context))
           (elem-type (car context))
           (src-ov-len (- (overlay-end src-ov) (overlay-start src-ov))))
      (if (/= src-ov-len (- end beg))
          (user-error "Error.  Lengths of transclusion and source are not identical")
        (setq tc-ov (text-clone-make-overlay beg end))
        (cons src-ov tc-ov)))))

(provide 'org-transclusion-src-lines)
