;;;-*- lexical-binding: t; -*-
(push "src-lines" org-transclusion-add-at-point-functions)
(add-hook 'org-transclusion-get-keyword-values-functions
          #'org-transclusion-keyword-get-value-lines)
(add-hook 'org-transclusion-get-keyword-values-functions
          #'org-transclusion-keyword-get-value-src)
(add-hook 'org-transclusion-get-keyword-values-functions
          #'org-transclusion-keyword-get-value-src-options)
(add-hook 'org-transclusion-keyword-plist-to-string-functions
          #'org-transclusion-keyword-plist-to-string-src-lines)
(add-hook 'org-transclusion-content-insert-functions
          #'org-transclusion-content-format-src-lines)

(defun org-transclusion--match-src-lines (_link plist)
  "Check if \"src-lines\" can be used for the LINK.
Returns non-nil if check is pass."
  (when (or (plist-get plist :lines)
            (plist-get plist :src))
    t))

(defun org-transclusion--add-src-lines (link plist)
  "Use PATH to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.
TODO need to handle when the file does not exist.  The logic to
pars n-m for :lines is taken from
`org-export--inclusion-absolute-lines' in ox.el with one
exception.  Instead of :lines 1-10 to exclude line 10, the logic
below has been adjusted to include line 10.  This should be more
intuitive when it comes to including lines of code."
  (let* ((path (org-element-property :path link))
         (search-option (org-element-property :search-option link))
         (buf (find-file-noselect path))
         (src-lines (plist-get plist :lines))
         (src-lang (plist-get plist :src))
         (src-options (plist-get plist :src-options)))
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
                ;; Need markers here so that they can move
                ;; when #+begin/end_src added
                (beg-mkr (set-marker (make-marker) beg))
                (end-mkr (set-marker (make-marker) end))
                (content))
           (setq content
                 (concat
                  (when src-lang
                    (concat
                     (format "#+begin_src %s" src-lang)
                     (when src-options (format " %s" src-options))
                     "\n"))
                  (buffer-substring-no-properties beg end)
                  (when src-lang "#+end_src\n")))
           (list :tc-content content
                 :tc-beg-mkr beg-mkr
                 :tc-end-mkr end-mkr)))))))

(defun org-transclusion-keyword-get-value-lines (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are optional \"1-10\"."
  (when (string-match ":lines +\\(\"?[0-9]*-[0-9]*\"?\\)" string)
    (list :lines (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-get-value-src (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are optional :src \"python\"."
  (when (string-match ":src +\\(\"?\\w*\"?\\)" string)
    (list :src (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-get-value-src-options (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are mandatory."
  (when (string-match ":src-options +\"\\(.*\\)\"" string)
    (list :src-options (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-plist-to-string-src-lines (plist)
  (let ((string)
        (lines (plist-get plist :lines))
        (src (plist-get plist :src))
        (src-options (plist-get plist :src-options)))
    (concat string
     (when lines (format ":lines %s" lines))
     (when src (format " :src %s" src))
     (when src-options (format " :src-options \"%s\"" src-options)))))

(defun org-transclusion-open-source-src-lines (&optional arg)
  "Open the source buffer of transclusion at point.
When ARG is non-nil (e.g. \\[universal-argument]), the point will
remain in the source buffer for further editing."
  (interactive "P")
  (unless (overlay-buffer (get-text-property (point) 'tc-pair))
    (org-transclusion-refresh-at-point))
  (let* ((src-buf (overlay-buffer (get-text-property (point) 'tc-pair)))
         (src-beg-mkr (get-text-property (point) 'tc-src-beg-mkr)))
    (if (not src-buf)
        (user-error (format "No paired source buffer found here: at %d" (point)))
      (unwind-protect
          (progn
            (pop-to-buffer src-buf
                           '(display-buffer-reuse-window . '(inhibit-same-window)))
            (goto-char src-beg-mkr)
            (recenter-top-bottom))
        (unless arg (pop-to-buffer src-buf))))))

(defun org-transclusion-content-format-src-lines (content)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).
Currently it only re-aligns table with links in the content."
  (with-temp-buffer
    (insert content)
    (put-text-property (point-min) (point-max)
                       'tc-open-fn
                       'org-transclusion-open-source-src-lines)
    (put-text-property (point-min) (point-max)
                       'tc-live-sync-buffers
                       'org-transclusion-live-sync-buffers-get-src-lines)
    ;; Return the temp-buffer's string
    (buffer-string)))

(defun org-transclusion-live-sync-buffers-get-src-lines ()
  "Return cons cell of overlays for source and trasnclusion.
    (src-ov . tc-ov)
This function is for non-Org text files."
  ;; Get the transclusion source's overlay but do not directly use it; it is
  ;; needed after exiting live-sync, which deletes live-sync overlays.
 (when (string= "src-lines" (get-text-property (point) 'tc-type))
   (when electric-indent-mode
     (user-error "No live sync for src-code block when `electric-indent-mode' is on"))
   (let* ((tc-pair (get-text-property (point) 'tc-pair))
          (src-ov (text-clone-make-overlay
                   (overlay-start tc-pair)
                   (overlay-end tc-pair)
                   (overlay-buffer tc-pair)))
          (beg (marker-position (get-text-property (point) 'tc-beg-mkr)))
          (end (marker-position (get-text-property (point) 'tc-end-mkr)))
          (tc-ov)
          (context (org-element-context))
          (type (car context))
          (src-ov-len (- (overlay-end src-ov) (overlay-start src-ov))))
     ;; If the region is in src-block, get the content
     (when (string= type "src-block")
       (save-excursion
         (goto-char (org-element-property :begin context))
         (forward-line 1)
         (setq beg (line-beginning-position))
         (goto-char (- (org-element-property :end context)
                       (org-element-property :post-blank context)))
         (forward-char -1)
         (forward-line -1)
         (setq end (1+ (line-end-position)))))
     (if (/= src-ov-len (- end beg))
         (user-error "Error.  Lengths of transclusion and source are not identical")
       (setq tc-ov (text-clone-make-overlay beg end))
       (cons src-ov tc-ov)))))

(provide 'org-transclusion-src-lines)
