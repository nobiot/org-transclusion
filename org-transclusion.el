;;; org-transclusion.el --- transclude text contents of linked target -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; This library is an attempt to enable transclusion with Org Mode.
;; Transclusion is the ability to include content from one file into
;; another by reference.

;; It is still VERY experimental.  As it modifies your files (notes), use
;; it with care.  The author and contributors cannot be held responsible
;; for loss of important work.

;; Org-transclusion is a buffer-local minor mode.  It is suggested to set a
;; keybinding like this to make it easy to toggle it:
;;     (define-key global-map (kbd "<f12>") #'org-transclusion-mode)

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-element)
(require 'org-id)

;;;; Customization

(defgroup org-transclusion nil
  "Insert text contents by way of link references."
  :group 'org
  :prefix "org-translusion-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-transclusion"))

(defcustom org-transclusion-exclude-elements (list 'property-drawer)
  "Define the Org elements that are excluded from transcluded copies.
It is a list of elements to be filtered out.
Refer to variable `org-element-all-elements' for names of elements accepted."
  :type '(repeat symbol)
  :group 'org-transclusion)

(defcustom org-transclusion-include-first-section nil
  "Define whether or not transclusion for Org files includes \"first section\".
If t, the section before the first headline is
transcluded. Default is nil."
  :type 'boolean
  :group 'org-transclusion)

(defcustom org-transclusion-add-at-point-functions (list "others-default")
  "Define list of `link types' org-tranclusion supports.
In addtion to a element in the list, there must be two
corresponding functions with specific names

The functions must conform to take specific arguments, and to returnbvalues.

org-transclusion-match-<org-id>
org-transclusion-add-<org-id>

See the functions delivered within org-tranclusion for the API signatures."
  :type '(repeat string)
  :group 'org-transclusion)

;;;; Faces

(defface org-transclusion-source-block
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

(defface org-transclusion-block nil
  "Face for transcluded block's fringe."
  :group 'org-transclusion)

(defface org-transclusion-block-edit
  '((((class color) (min-colors 88) (background light))
     :background "#dde3f4" :extend t)
    (((class color) (min-colors 88) (background dark))
     :foreground "#bfc0c4" :background "#1e1e1e" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

;;;; Variables

(defvar-local org-transclusion-remember-point nil)

(defvar org-transclusion-link-open-hook
  '(org-transclusion-link-open-org-id
    org-transclusion-link-open-org-file-links
    org-transclusion-link-open-other-file-links))

(defvar org-transclusion-get-keyword-values-hook
  '(org-transclusion--get-keyword-active-p-value
    org-transclusion--get-keyword-path-value
    org-transclusion--get-keyword-level-value))

(defvar org-transclusion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'org-transclusion-edit-live-start-at-point)
    (define-key map (kbd "g") #'org-transclusion-refresh-at-poiont)
    (define-key map (kbd "d") #'org-transclusion-remove-at-point)
    (define-key map (kbd "TAB") #'org-cycle)
    map))

(define-fringe-bitmap 'org-transclusion-fringe
  [#b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000]
  nil nil '(center t))

;;;; Commands

(define-minor-mode org-transclusion-mode
  "Toggle Org-transclusion minor mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :init-value nil
  :lighter nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (cond
   (org-transclusion-mode
    (org-transclusion-activate))
   (t (org-transclusion-deactivate))))

(defun org-transclusion-activate ()
  "Activate automatic transclusions in the local buffer."
  (interactive)
  (add-hook 'before-save-hook #'org-transclusion-remove-all-in-buffer nil t)
  (add-hook 'after-save-hook #'org-transclusion-add-all-in-buffer nil t)
  (add-hook 'kill-buffer-hook #'org-transclusion-remove-all-in-buffer nil t))

(defun org-transclusion-deactivate ()
  "Deactivate automatic transclusions in the local buffer."
  (interactive)
  (org-transclusion-remove-all-in-buffer)
  (remove-hook 'before-save-hook #'org-transclusion-remove-all-in-buffer t)
  (remove-hook 'after-save-hook #'org-transclusion-add-all-in-buffer t))

(defun org-transclusion-add-at-point ()
  "Transclude keyword.
Pass Org mode's link object to `org-transclusion-link-open'.
This function assumes the point is at the beginning of a link."
  (interactive)
  (when-let* ((keyword-values (org-transclusion--get-keyword-values-at-point))
              (link (org-transclusion-wrap-path-to-link
                     (plist-get keyword-values :path)))
              (type (org-element-property :type link)))
    ;; The transclusion needs to be active, and the link type needs to be
    ;; either id or file
    (cond ((and (plist-get keyword-values :active-p)
                (or (string= "id" type)
                    (string= "file" type)))
           (let ((tc-params))
             (setq tc-params (run-hook-with-args-until-success
                              'org-transclusion-link-open-hook tc-params link))
             (if (not tc-params)
                 (progn (message "No transclusion added.") nil) ; return nil)
               (let* ((tc-type (plist-get tc-params :tc-type))
                      (tc-arg (plist-get tc-params :tc-arg))
                      (tc-fn (plist-get tc-params :tc-fn))
                      (tc-payload (funcall tc-fn tc-arg))
                      (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
                      (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
                      (tc-content (plist-get tc-payload :tc-content)))
                 (if (or (string= tc-content "")
                         (eq tc-content nil))
                     (progn (message "Nothing done. No content is found through the link.") nil)
                   (save-excursion
                     (org-transclusion-with-silent-modifications
                       ;; Remove keyword
                       ;;(org-transclusion-remove-keyword)
                       ;;(insert (org-transclusion-turn-off keyword-values))
                       (forward-line 1)
                       ;; Insert & overlay
                       (org-transclusion--insert-content
                        keyword-values tc-type tc-content
                        tc-beg-mkr tc-end-mkr)
                       (org-transclusion-mode 1)
                       t)))))))
          ;; For other cases. Do nothing
          (t (message "Nothing done. Transclusion inactive or link missing.") nil))))

(defun org-transclusion-add-all-in-buffer ()
  "Add all the transclusions in the current buffer.
As this function should be used only on the current buffer, no
argument is passed."
  (interactive)
  (org-with-point-at 1
    (let ((regexp "^[ \t]*#\\+TRANSCLUDE:"))
      (while (re-search-forward regexp nil t)
        ;; Don't transclude if in transclusion overlay to avoid infinite
        ;; recursion
        (unless (org-transclusion--within-transclusion-p)
          (org-transclusion-add-at-point)))))
  (when org-transclusion-remember-point
    (goto-char org-transclusion-remember-point)
    ;;(recenter)
    (setq org-transclusion-remember-point nil)))

(defun org-transclusion-remove-at-point ()
  "Remove transclusion and the copied text at point."
  (interactive)
  (if-let* ((beg (marker-position (get-char-property (point) 'tc-beg-mkr)))
            (end (marker-position (get-char-property (point) 'tc-end-mkr)))
            (keyword (org-transclusion-keyword-values-to-keyword
                      (get-char-property (point) 'tc-orig-keyword)))
            (tc-pair-ov (get-char-property (point) 'tc-pair)))
      (progn
        (org-transclusion--remove-source-buffer-edit-overlay beg end)
        (delete-overlay tc-pair-ov)
        (org-with-wide-buffer
         (org-transclusion-with-silent-modifications
           (delete-region beg end)))
        (forward-char -1) ;; back to the keyword line
        t)
    (message "Nothing done. No transclusion exists here.") nil))

(defun org-transclusion-remove-all-in-buffer ()
  "Remove all the translusion overlay and copied text in current buffer."
  (interactive)
  (setq org-transclusion-remember-point (point))
  (org-with-point-at 1
    (while (text-property-search-forward 'tc-id)
      (forward-char -1)
      (org-transclusion-with-silent-modifications
        (org-transclusion-remove-at-point)))))

(defun org-transclusion-refresh-at-poiont ()
  "Refresh the transclusion at point."
  (interactive)
  (when (org-transclusion--within-transclusion-p)
    (let ((pos (point)))
      (org-transclusion-remove-at-point)
      (org-transclusion-add-at-point)
      (goto-char pos))
    t))

(defun org-transclusion-edit-live-start-at-point ()
  "Put overlay for edit live.
Analogous to Occur Edit for Occur Mode.

It temporarily remove the filter so that all the elements get
transcluded. This is necessary to ensure that both the source and
transcluded elements have exactly the same content.

The issue of filter is present for example when a block quote
contains a keyword. Filtering out the keyword will misalign the
source block quote and the transcluded one.

While live sync is on, before- and after-save-hooks to remove/add
transclusions are also temporariliy disabled. This prevents
auto-save from getting in the way of live sync.

TODO State management of live sync together with more explicit
managemetn of before- and after-save-buffer-hooks.

TODO source buffer opens when it is not open. The line position
may or may not be useful. This needs to be thought through."
  (interactive)
  (if (not (org-transclusion--within-transclusion-p))
      (progn (message "This is not a translusion.") nil)
    ;; Temporarily deactivate filter
    (let ((org-transclusion-exclude-elements nil))
      (org-transclusion-refresh-at-poiont))
    (remove-hook 'before-save-hook #'org-transclusion-remove-all-in-buffer t)
    (remove-hook 'after-save-hook #'org-transclusion-add-all-in-buffer t)
    (let* ((tc-elem (org-transclusion-get-enclosing-element))
           (tc-beg (if (org-element-property :contents-begin tc-elem)
                       (org-element-property :contents-begin tc-elem)
                     (org-element-property :begin tc-elem)))
           (tc-end (if (org-element-property :contents-end tc-elem)
                       (org-element-property :contents-end tc-elem)
                     (org-element-property :end tc-elem)))
           (tc-ov (make-overlay tc-beg tc-end nil t t)) ;front-advance should be t
           (src-ov (org-transclusion--find-src-ov-for-edit tc-end))
           (dups (list src-ov tc-ov)))
      (display-buffer (overlay-buffer src-ov))
      ;; Source Overlay
      (overlay-put src-ov 'evaporate t)
      (overlay-put src-ov 'text-clones dups)
      (overlay-put src-ov 'modification-hooks
                   '(org-transclusion--text-clone--maintain))
      (overlay-put src-ov 'face 'org-transclusion-source-block)
      ;; Transclusion Overlay
      (overlay-put tc-ov 'modification-hooks
                   '(org-transclusion--text-clone--maintain))
      (overlay-put tc-ov 'evaporate t)
      (overlay-put tc-ov 'tc-paired-src-edit-ov src-ov)
      (overlay-put tc-ov 'tc-type "src-edit-ov")
      (overlay-put tc-ov 'face 'org-transclusion-block-edit)
      (overlay-put tc-ov 'text-clones dups)
      (overlay-put tc-ov 'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map (kbd "C-c C-c")
                                     #'org-transclusion-refresh-at-poiont)
                                   map))
      (with-silent-modifications
        (remove-text-properties tc-beg tc-end '(read-only)))
      t)))

(defun org-transclusion--find-src-ov-for-edit (limit)
  "."
  (let ((src-buf (overlay-buffer (get-text-property (point) 'tc-pair)))
        ;; I will keep src-buf to be gotten from tc-pair.
        ;; I might not keep org-transclusion-text-beg-mkr
        (src-search-beg
         (get-text-property (point) 'org-transclusion-text-beg-mkr)))
    (unless src-search-beg
      (save-excursion
        (goto-char (next-property-change (point) nil limit))
        (setq src-search-beg (get-text-property
                              (point) 'org-transclusion-text-beg-mkr))))

    (with-current-buffer src-buf
      (goto-char src-search-beg)
      (let* ((src-elem (org-transclusion-get-enclosing-element))
             (src-beg (if (org-element-property :contents-begin src-elem)
                          (org-element-property :contents-begin src-elem)
                        (org-element-property :begin src-elem)))
             (src-end (if (org-element-property :contents-end src-elem)
                          (org-element-property :contents-end src-elem)
                        (org-element-property :end src-elem))))
        (make-overlay src-beg src-end nil t t)))))

(defun org-transclusion-create-from-link ()
  "WIP."
  ;; check if at-point is a link file or id
  (interactive)
  (let* ((context
          (org-element-lineage
           (org-element-context)'(link) t))
         (type (org-element-property :type context)))
    (when (or (string= type "file")
              (string= type "id"))
      (let ((raw-link (org-element-property :raw-link context)))
        (org-forward-element)
        (insert (format "\n\n#+transclude: t \"%s\"\n\n" raw-link))
        (forward-char -3)
        (org-transclusion-add-at-point)))))


;;;;-----------------------------------------------------------------------------
;;;; Functions for Transclude Keyword
;;   #+transclude: t "~/path/to/file.org::1234"

(defun org-transclusion--get-keyword-values-at-point ()
  "Return the \"#+transcldue:\" keyword's values if any at point"
  (save-excursion
    (beginning-of-line)
    (let ((plist))
      (when (string= "TRANSCLUDE" (org-element-property :key (org-element-at-point)))
        ;; #+transclude: keyword exists.
        ;; Further checking the value
        (when-let ((value (org-element-property :value (org-element-at-point))))
          (dolist (fn org-transclusion-get-keyword-values-hook) plist
                  (setq plist (append plist (funcall fn value)))))
        plist))))

(defun org-transclusion--get-keyword-active-p-value (value)
  (when (string-match "^\\(t\\|nil\\).*$" value)
    (list :active-p (org-transclusion--not-nil (match-string 1 value)))))

(defun org-transclusion--get-keyword-path-value (value)
  (when (string-match "\\(\".+?\"\\)" value)
    (list :path (org-strip-quotes (match-string 0 value)))))

(defun org-transclusion--get-keyword-level-value (value)
  (when (string-match ":level *\\([1-9]\\)" value)
    (list :level (string-to-number (org-strip-quotes (match-string 1 value))))))

(defun org-transclusion-remove-keyword ()
  (let* ((elm (org-element-at-point))
         (beg (org-element-property :begin elm))
         (end (org-element-property :end elm))
         (post-blank (org-element-property :post-blank elm)))
    (delete-region beg (- end post-blank)) t))

(defun org-transclusion-turn-off (values)
  "Return the keyword string with the \":active-p\" prop removed."
  (let ((path (plist-get values :path))
        (level (plist-get values :level)))
    (concat "#+transclude: "
            " \"" path "\""
            (when level (format " :level %d" level))
            "\n")))

(defun org-transclusion-keyword-values-to-keyword (values)
  "."
  (let ((active-p (plist-get values :active-p))
        (path (plist-get values :path))
        (level (plist-get values :level)))
    (concat "#+transclude: "
            (symbol-name active-p)
            " \"" path "\""
            (when level (format " :level %d" level))
            "\n")))

;;;;-----------------------------------------------------------------------------
;;;; Functions for inserting content

(defun org-transclusion--insert-content (keyword-values type content src-beg-m src-end-m)
  "Add content and overlay."
  (let* ((tc-id (substring (org-id-uuid) 0 8))
         (sbuf (marker-buffer src-beg-m)) ;source buffer
         (beg (point)) ;; before the text is inserted
         (beg-mkr)
         (end) ;; at the end of text content after inserting it
         (end-mkr)
         (ov-src) ;; source-buffer
         (ov-tc) ;; transclusion-buiffer
         (tc-pair))
    (when (org-kill-is-subtree-p content)
      (let ((level (plist-get keyword-values :level)))
        ;; FIXME Ugly to adjust level...
        (when level (setq level (1- level)))
        (with-temp-buffer
          (org-transclusion-paste-subtree level content t t)
          (setq content (buffer-string)))))
    (insert (org-transclusion--format-content content))
    (setq beg-mkr (save-excursion (goto-char beg)
                                  (org-transclusion--make-marker (point))))
    (setq end (point))
    (setq end-mkr (org-transclusion--make-marker (point)))
    (setq ov-src (make-overlay src-beg-m src-end-m sbuf t nil))
    (setq tc-pair ov-src)
    (add-text-properties beg end
                         `(local-map ,org-transclusion-map
                                     read-only t
                                     front-sticky nil
                                     rear-nonsticky nil
                                     tc-id ,tc-id
                                     tc-type ,type
                                     tc-beg-mkr ,beg-mkr
                                     tc-end-mkr ,end-mkr
                                     tc-src-beg-mkr ,src-beg-m
                                     tc-pair ,tc-pair
                                     tc-orig-keyword ,keyword-values
                                     ;; TODO Fringe is not supported for terminal
                                     line-prefix ,(propertize
                                                   "x"
                                                   `display
                                                   `(left-fringe org-transclusion-fringe org-transclusion-block))
                                     wrap-prefix ,(propertize
                                                   "x"
                                                   `display
                                                   `(left-fringe org-transclusion-fringe org-transclusion-block))))
    ;; Put to the source overlay
    (overlay-put ov-src 'tc-by beg-mkr)
    (overlay-put ov-src 'evaporate t)
;;    (overlay-put ov-src 'face 'org-transclusion-source-block)
    (overlay-put ov-src 'tc-pair tc-pair)))

(defun org-transclusion--format-content (content)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).
Currently it only re-aligns table with links in the content."
  (with-temp-buffer
    (org-mode)
    (insert content)
    (let ((point (point-min)))
      (while point
        (goto-char (1+ point))
        (when (org-at-table-p)
          (org-table-align)
          (goto-char (org-table-end)))
        (setq point (search-forward "|" (point-max) t))))
    (buffer-string)))

(defun org-transclusion-link-open-org-id (tc-params link)
  "For Org-id.
Return nil if not found."
  (when (string= "id" (org-element-property :type link))
    ;; when type is id, the value of path is the id
    (let* ((id (org-element-property :path link))
           (mkr (ignore-errors (org-id-find id t))))
      (if mkr (progn
                (setq tc-params (list :tc-type "org-id"
                                      :tc-arg mkr
                                      :tc-fn #'org-transclusion--get-org-content-from-marker)))
        (message "No transclusion done for this ID. Ensure it works.")))))

(defun org-transclusion-link-open-org-file-links (tc-params link)
  ;; Other Org file links
  (when (org-transclusion--org-file-p (org-element-property :path link))
   (setq tc-params (list :tc-type "org-link"
                         :tc-arg link
                         :tc-fn #'org-transclusion--get-org-content-from-link))))

(defun org-transclusion-link-open-other-file-links (tc-params link)
     ;; For non-Org files
     ((setq tc-params (org-transclusion--get-custom-tc-params link))))

(defun org-transclusion--get-org-content-from-marker (marker)
  "Return tc-beg-mkr, tc-end-mkr, tc-content from MARKER.
This is meant for Org-ID."
  (if (and marker (marker-buffer marker)
           (buffer-live-p (marker-buffer marker)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           ;;(outline-show-all)
           (goto-char marker)
           (if (org-before-first-heading-p)
               (org-transclusion--get-org-buffer-or-element-at-point)
             (org-transclusion--get-org-buffer-or-element-at-point 'only-element)))))
    (message "Nothing done. Cannot find marker for the ID.")))

(defun org-transclusion--get-org-content-from-link (link &rest _arg)
  "Return tc-beg-mkr, tc-end-mkr, tc-content from LINK."
  (save-excursion
    ;; First visit the buffer and go to the relevant elelement if id or
    ;; search-option is present.
    (let* ((path (org-element-property :path link))
           (search-option (org-element-property :search-option link))
           (buf (find-file-noselect path)))
      (with-current-buffer buf
        (org-with-wide-buffer
         ;;(outline-show-all)
         (if search-option
             (progn
               (org-link-search search-option)
               (org-transclusion--get-org-buffer-or-element-at-point 'only-element))
           (org-transclusion--get-org-buffer-or-element-at-point)))))))

(defun org-transclusion--get-org-buffer-or-element-at-point (&optional only-element)
  "Return content for transclusion.
When ONLY-ELEMENT is t, only the element.  If nil, the whole buffer.
Assume you are at the beginning of the org element to transclude."
  (if-let* ((el (org-element-context))
            (type (org-element-type el)))
      (let ((parse-mode 'section) ;; default is 'section. For org-element--parse-elements
            (no-recursion '(headline section))
            (tc-content)(tc-beg-mkr)(tc-end-mkr))
        ;; For dedicated target, we want to get the parent paragraph,
        ;; rather than the target itself
        (when (and (string= "target" type)
                   (string= "paragraph" (org-element-type (org-element-property :parent el))))
          (setq el (org-element-property :parent el))
          ;; for dedicated darget = paragraph, parse-mode should be nil to
          ;; avoid getting the whole section
          (setq parse-mode nil))
        (let* ((tree (progn (if only-element
                                ;; Parse only the element in question (headline, table, paragraph, etc.)
                                (progn
                                  (setq parse-mode nil) ; needed for table, list, block-quote, etc.
                                  (push type no-recursion)
                                  (advice-add 'buffer-substring-no-properties :around #'org-transclusion-buffer-substring-advice)
                                  (org-element--parse-elements
                                   (org-element-property :begin el)
                                   (org-element-property :end el)
                                   nil nil 'object nil (list 'tc-paragraph nil)))
                              ;; If not only-element, then parse the entire buffer
                              (advice-add 'buffer-substring-no-properties :around #'org-transclusion-buffer-substring-advice)
                              (org-element-parse-buffer))))
               (obj (org-element-map
                        tree
                        org-element-all-elements
                      ;; Map all the elements (not objects).  But for the
                      ;; output (transcluded copy) do not do recursive for
                      ;; headline and section (as to avoid duplicate
                      ;; sections; headlines contain section) Want to remove
                      ;; the elements of the types included in the list from
                      ;; the AST.
                      #'org-transclusion--filter-buffer
                      nil nil no-recursion nil)))
          (advice-remove 'buffer-substring-no-properties #'org-transclusion-buffer-substring-advice)
          (setq tc-content (org-element-interpret-data obj))
          (setq tc-beg-mkr (progn (goto-char
                                   (if only-element (org-element-property :begin el)
                                     (point-min))) ;; for the entire buffer
                                   (point-marker)))
          (setq tc-end-mkr (progn (goto-char
                                   (if only-element (org-element-property :end el)
                                     (point-max))) ;; for the entire buffer
                                   (point-marker)))
          (list :tc-content tc-content
                :tc-beg-mkr tc-beg-mkr
                :tc-end-mkr tc-end-mkr)))
    (message "Nothing done. Content is empty.")))

(defun org-transclusion--filter-buffer (data)
  "Filter DATA before transcluding its content.
DATA is meant to be a parse tree for ‘org-element.el'.

This function is used within
`org-transclusion--get-org-buffer-or-element-at-point'.

Use `org-transclusion-exclude-elements' variable to specify which
elements to remove from the transcluded copy.

The \"first section\" (the part before the first headline) is by
default excluded -- this is the intended behavior.

Use `org-transclusion-include-first-section' customizing variable
to include the first section."
  (cond ((and (memq (org-element-type data) '(section))
              (not (eq 'tc-paragraph (org-element-type (org-element-property :parent data)))))
         ;; This condition is meant to filter out the first section; that is,
         ;; the part before the first headline.  The DATA should have the type
         ;; `org-data' by default, with one exception.  I put `tc-paragraph'
         ;; as the type when a paragraph is parased (via dedicated target).
         ;; In this case, the whole DATA should be returned.
         ;; Sections are included in the headlines Thies means that if there
         ;; is no headline, nothing gets transcluded.
         (if org-transclusion-include-first-section
             ;; Add filter to the first section as well
             (progn (org-element-map data org-transclusion-exclude-elements
                      (lambda (d) (org-element-extract-element d)))
                    data)
           nil))
        ;; Rest of the case.
        (t (org-element-map data org-transclusion-exclude-elements
             (lambda (d) (org-element-extract-element d) nil))
           data)))

;;;;-----------------------------------------------------------------------------
;;;; Functions to support non-Org-mode link types

(defun org-transclusion--get-custom-tc-params (link)
  "Return PARAMS with TC-FN if link type is supported for LINK object."
  (let ((types org-transclusion-add-at-point-functions)
        (params nil)
        (str nil))
    (setq str (org-element-property :path link))
    (while (and (not params)
                types)
      (let* ((type (pop types))
             (match-fn
              (progn (intern (concat "org-transclusion--match-" type))))
             (add-fn
              (progn (intern (concat "org-transclusion--add-" type)))))
        (when (and (functionp match-fn)
                   (funcall match-fn str)
                   (functionp add-fn))
          (setq params (list :tc-type type :tc-fn add-fn :tc-arg str)))))
    params))

(defun org-transclusion--match-others-default (_path)
  "Check if `others-default' can be used for the PATH.
Returns non-nil if check is pass."
  t)

(defun org-transclusion--add-others-default (path)
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

;;-----------------------------------------------------------------------------
;;; Utility Functions and Macros

(defmacro org-transclusion-with-silent-modifications (&rest body)
  "It's like `with-silent-modifications' but keeps the undo list."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (inhibit-read-only t)
            (inhibit-modification-hooks t))
       (unwind-protect
           (progn
             ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defun org-transclusion-wrap-path-to-link (path)
  "Take PATH string. Return Org link object."
  (with-temp-buffer
    (insert (concat "[[" path "]]"))
    (org-element-context)))

(defun org-transclusion--org-file-p (path)
  "Return non-nil if PATH is an Org file.
Checked with the extension `org'."
  (let ((ext (file-name-extension path)))
    (string= ext "org")))

(defun org-transclusion--not-nil (v)
  "Return t or nil.
It is like `org-not-nil', but when the V is non-nil or not
string \"nil\", return symbol t."
  (when (org-not-nil v) t))

(defun org-transclusion--within-transclusion-p ()
  "Return t if the current point is within a tranclusion overlay."
  (when (get-char-property (point) 'tc-id) t))

(defun org-transclusion--make-marker (point)
  "Return marker of the insertion-type t for POINT.
The insertion-type is important in order for the translusion beg
and end markers are correctly set. This fixes the problem of
transclude keyword not correctly removed when the keywords are
placed without a blank line."
  (let ((marker (set-marker (make-marker) point)))
    (set-marker-insertion-type marker t)
    marker))

;;-----------------------------------------------------------------------------
;;;; Functions for live-sync

(defun org-transclusion--remove-source-buffer-edit-overlay (beg end)
  "Remove the overlay in the source buffer being edited when applicable.
This function checks if such overlays exist - it should support
multiple edit overlays.  It is meant to be used in
`org-transclusion-remove-at-point'. The overlay needs to be
deleted before the transclusion itself is deleted; otherwise,
live edit will try to sync the deletion, and causes an error."
  (when-let ((src-edit-ovs (overlays-in beg end)))
    (dolist (ov src-edit-ovs)
      (when (string= "src-edit-ov" (overlay-get ov 'tc-type))
        (delete-overlay (overlay-get ov 'tc-paired-src-edit-ov))))))

(defun org-transclusion-get-enclosing-element ()
  "."
  (interactive)
  (let* ((context
          (org-element-lineage
           (org-element-context) '(center-block comment-block
           drawer dynamic-block example-block export-block
           fixed-width latex-environment plain-list
           property-drawer quote-block special-block src-block
           table verse-block keyword) t)))
    ;; For a paragraph
    (unless context
      (setq context
            (org-element-lineage
             (org-element-context) '(paragraph)
             t)))
    context))

(defun org-transclusion-buffer-substring-advice (orgfn start end)
  "Add id, copy the text-properties via `buffer-substring'"
  ;;(unless (get-text-property start 'org-transclusion-text-beg-mkr)
    (put-text-property start end
                       'org-transclusion-text-beg-mkr (org-transclusion--make-marker start))
    (put-text-property start end
                       'org-transclusion-text-end-mkr (org-transclusion--make-marker end))
    ;; (put-text-property start end
    ;;                      'org-transclusion-text-id (org-id-uuid)))
    (buffer-substring start end))

;;-----------------------------------------------------------------------------
;;;; Functions for meta-left/right
(defun org-transclusion-promote-subtree ()
  "Promote transcluded subtree."
  (interactive)
  (if (not (org-transclusion--within-transclusion-p))
      (message "Not in a transcluded headline.")
    (let ((inhibit-read-only t)
          (beg (get-text-property (point) 'tc-beg-mkr)))
      (when (org-at-heading-p)
        (org-promote-subtree)))))

(defun org-transclusion-promote-or-demote-subtree (&optional demote)
  "Promote transcluded subtree."
  (interactive "P")
  (if (not (org-transclusion--within-transclusion-p))
      (message "Not in a transcluded headline.")
    (let ((inhibit-read-only t)
          (beg (get-text-property (point) 'tc-beg-mkr)))
      (save-excursion
        (goto-char beg)
        (when (org-at-heading-p)
          (if demote (org-demote-subtree) (org-promote-subtree)))))))

;;-----------------------------------------------------------------------------
;;;; Definition of org-transclusion-paste-subtree


(defun org-transclusion-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.

----- Note for Org-transclusion -----
This is a Org-transclusion version of the Org Mode's standard function
`org-paste-subtree' (based on v9.4). Only one line has been commented out.
This is noted with a comment \";; nobiot removed\".
----------

The entire subtree is promoted or demoted in order to match a new headline
level.

If the cursor is at the beginning of a headline, the same level as
that headline is used to paste the tree.

If not, the new level is derived from the *visible* headings
before and after the insertion point, and taken to be the inferior headline
level of the two.  So if the previous visible heading is level 3 and the
next is level 4 (or vice versa), level 4 will be used for insertion.
This makes sure that the subtree remains an independent subtree and does
not swallow low level entries.

You can also force a different level, either by using a numeric prefix
argument, or by inserting the heading marker by hand.  For example, if the
cursor is after \"*****\", then the tree will be shifted to level 5.

If optional TREE is given, use this text instead of the kill ring.

When FOR-YANK is set, this is called by `org-yank'.  In this case, do not
move back over whitespace before inserting, and move point to the end of
the inserted text when done.

When REMOVE is non-nil, remove the subtree from the clipboard."
  (interactive "P")
  (setq tree (or tree (and kill-ring (current-kill 0))))
  (unless (org-kill-is-subtree-p tree)
    (user-error
     (substitute-command-keys
      "The kill is not a (set of) tree(s).  Use `\\[yank]' to yank anyway")))
  (org-with-limited-levels
   (let* ((visp (not (org-invisible-p)))
          (txt tree)
          (old-level (if (string-match org-outline-regexp-bol txt)
                         (- (match-end 0) (match-beginning 0) 1)
                       -1))
          (force-level
           (cond
            (level (prefix-numeric-value level))
            ;; When point is after the stars in an otherwise empty
            ;; headline, use the number of stars as the forced level.
            ((and (org-match-line "^\\*+[ \t]*$")
                  (not (eq ?* (char-after))))
             (org-outline-level))
            ((looking-at-p org-outline-regexp-bol) (org-outline-level))))
          (previous-level
           (save-excursion
             (org-previous-visible-heading 1)
             (if (org-at-heading-p) (org-outline-level) 1)))
          (next-level
           (save-excursion
             (if (org-at-heading-p) (org-outline-level)
               (org-next-visible-heading 1)
               (if (org-at-heading-p) (org-outline-level) 1))))
          (new-level (or force-level (max previous-level next-level)))
          (shift (if (or (= old-level -1)
                         (= new-level -1)
                         (= old-level new-level))
                     0
                   (- new-level old-level)))
          (delta (if (> shift 0) -1 1))
          (func (if (> shift 0) #'org-demote #'org-promote))
          (org-odd-levels-only nil)
          beg end newend)
     ;; Remove the forced level indicator.
     (when (and force-level (not level))
       (delete-region (line-beginning-position) (point)))
     ;; Paste before the next visible heading or at end of buffer,
     ;; unless point is at the beginning of a headline.
     (unless (and (bolp) (org-at-heading-p))
;;       (org-next-visible-heading 1) ;; nobiot removed
       (unless (bolp) (insert "\n")))
     (setq beg (point))
     (when (fboundp 'org-id-paste-tracker) (org-id-paste-tracker txt))
     (insert-before-markers txt)
     (unless (string-suffix-p "\n" txt) (insert "\n"))
     (setq newend (point))
     (org-reinstall-markers-in-region beg)
     (setq end (point))
     (goto-char beg)
     (skip-chars-forward " \t\n\r")
     (setq beg (point))
     (when (and (org-invisible-p) visp)
       (save-excursion (outline-show-heading)))
     ;; Shift if necessary.
     (unless (= shift 0)
       (save-restriction
         (narrow-to-region beg end)
         (while (not (= shift 0))
           (org-map-region func (point-min) (point-max))
           (setq shift (+ delta shift)))
         (goto-char (point-min))
         (setq newend (point-max))))
     (when (or for-yank (called-interactively-p 'interactive))
       (message "Clipboard pasted as level %d subtree" new-level))
     (when (and (not for-yank) ; in this case, org-yank will decide about folding
                kill-ring
                (equal org-subtree-clip (current-kill 0))
                org-subtree-clip-folded)
       ;; The tree was folded before it was killed/copied
       (org-flag-subtree t))
     (when for-yank (goto-char newend))
     (when remove (pop kill-ring)))))


;;-----------------------------------------------------------------------------
;; Text Clone
;; Based on StackExchange user Tobias' code; adapted by nobiot
;; https://emacs.stackexchange.com/questions/56201/is-there-an-emacs-package-which-can-mirror-a-region/56202#56202
;; Since I'm not using SPREADP argument (or margin), I can simplify
;; the code much more.

(defvar org-transclusion--text-clone-maintaining nil)

(defun org-transclusion--text-clone--maintain (ol1 after beg end &optional _len)
  "Propagate the changes made under the overlay OL1 to the other clones.
This is used on the `modification-hooks' property of text clones."
  (when (and after ;(not undo-in-progress) ;; < nobit removed undo-in-progress
             (not org-transclusion--text-clone-maintaining)
             (overlay-start ol1))
    (let ((margin (if (overlay-get ol1 'text-clone-spreadp) 1 0)))
      (setq beg (max beg (+ (overlay-start ol1) margin)))
      (setq end (min end (- (overlay-end ol1) margin)))
      (when (<= beg end)
        (save-excursion
          ;; Remove text-clone-syntax case; we don't use it.
          ;; Now go ahead and update the clones.
          (let ((head (- beg (overlay-start ol1)))
                (tail (- (overlay-end ol1) end))
                (str (buffer-substring-no-properties beg end)) ;changed to no-properties
                (nothing-left t)
                (org-transclusion--text-clone-maintaining t))
            (dolist (ol2 (overlay-get ol1 'text-clones))
              (with-current-buffer (overlay-buffer ol2) ;;< Tobias
                (save-restriction
                  (widen)
                  ;(outline-show-all)
                  (let ((oe (overlay-end ol2)))
                    (unless (or (eq ol1 ol2) (null oe))
                      (setq nothing-left nil)
                      (let ((mod-beg (+ (overlay-start ol2) head)))
                        ;;(overlay-put ol2 'modification-hooks nil)
                        (goto-char (- (overlay-end ol2) tail))
                        (unless (> mod-beg (point))
                          (save-excursion (insert str))
                          (delete-region mod-beg (point)))
                        ;;(overlay-put ol2 'modification-hooks '(text-clone--maintain))
                        ))))))
            (if nothing-left (delete-overlay ol1))))))))

(provide 'org-transclusion)
;;; org-transclusion.el ends here
