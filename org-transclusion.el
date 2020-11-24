;;; org-transclusion.el --- transclude text contents of linked target -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing

;; Version: 0.0.6
;; Package-Requires: ((emacs "27.1") (org "9.4"))

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
(require 'org)
(require 'org-element)
(require 'org-id)

;;-----------------------------------------------------------------------------
;; Variables
;;

(defvar-local org-transclusion-buffer-modified-p nil)
(defvar-local org-transclusion-original-position nil)
(defvar-local org-transclusion-edit-src-at-mkr nil)
(defvar org-transclusion-last-edit-src-buffer nil
  "Keep track of the cloned buffer for transclusion sources.
There should be only one edit source buffer at a time.  This is
so that you avoid opening too many clone buffers.  It is also
used to close the edit source buffer when minor mode is turned
off.

Note that the minor mode is buffer local, but this variable is
global.  This is deliberte design choice.  You may activate
Org-transclusion for multiple buffers at a time.  But editing
their sources should be focused, and thus one edit buffer can be
open at a time.

Killing a clone buffer is assumed to be safe in general, as its original
buffer is in sync and the content is reflected there.")

(defvar org-transclusion-debug nil
  "Disable the toggle of transclusions.
It is meant to enable edebugging.  Without this, switching to the source
code buffer for runtime debugger toggles off the transclusion, and thus
makes it impossible to debug at runtime.")

;;;; Customization variables
(defgroup org-transclusion nil
  "Insert text contents by way of link references."
  :group 'org
  :prefix "org-translusion-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-transclusion"))

(defcustom org-transclusion-activate-persistent-message t
  "Define whether or not a header line is added when transclusion is active."
  :type 'boolean
  :group 'org-transclusion)

(defcustom org-transclusion-auto-add-on-activation t
  "Define whether or not add all the transclusion contents on activation.
If true, add text contents for all the transclusion links where possible.
Default to true."
  :type 'boolean
  :group 'org-transclusion)

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

(defcustom org-transclusion-link "otc"
  "Define custom Org link type name used for transclusion links."
  :type 'string
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

;;-----------------------------------------------------------------------------
;; Faces
;; (WIP)

(defface org-transclusion-source-block
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#fff3da" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

(defface org-transclusion-block
  '((((class color) (min-colors 88) (background light))
     :background "#f3f3ff" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#f3f3ff" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

(defface org-transclusion-keyword
  '((t (:foreground "gray90" :extend t)))
    "Face for the :transclusion keyword."
    :group 'org-transclusion)

;;-----------------------------------------------------------------------------
;; Custom link parameter
;; :follow fn should be a one that can do-list for functions, where
;; each function support different type of links: e.g. file, ID, etc.
;; At the moment, it does not do anything special.

(org-link-set-parameters org-transclusion-link)

;;-----------------------------------------------------------------------------
;; Functions to override org-link-open
;; Transclude standard Org Mode file links
;; Add Support different non-Org link types

(defun org-transclusion-link-open-at-point (&optional arg)
  "Meant to be for hook `org-open-at-point-functions'.
Make it local hook and increase priority.
Only for type  is \"file\" or \"id\".
Assume it is called in the beginning of a link."
  (let* ((context (org-element-context))
         (type (org-element-property :type context)))
    (cond
     ((or (string= "id" type)
          (string= "file" type))
      (org-transclusion-link-open context arg)
      ;; return t to stop run-hook-with-args-until-success
      t)
     ;; For all the other cases, return nil so that standard org functions
     ;; (and other lower-prio functions in the hook can run.
     (t nil))))

(defun org-transclusion-link-open (link &optional arg)
  "Override Org Mode's default `org-link-open' (ORGFN) for LINK.
Meant to be used with add-advice/remove-advice in activate/deactivate.

If the link type is not supported by org-transclusion, or \\[universal-argument]
is used (ARG is non-nil), then use `org-link-open'."
  (let ((tc-params nil))
    (cond
     ((not (org-transclusion--ok-to-transclude)) (org-link-open link arg))

     (arg (org-link-open link arg))

     ((string= "id" (org-element-property :type link))
      ;; when type is id, the value of path is the id
      (let* ((id (org-element-property :path link))
             (tc-payload (org-transclusion--get-org-content-from-marker (org-id-find id t))))
        (setq tc-params (list :tc-type "org-id"
                              :tc-fn (lambda ()
                                       tc-payload)))))

     ((org-transclusion--org-file-p (org-element-property :path link))
      (let ((tc-payload (org-transclusion--get-org-content-from-link link arg)))
        (setq tc-params (list :tc-type "org-link"
                              :tc-fn (lambda ()
                                       tc-payload)))))

     ((setq tc-params (org-transclusion--get-custom-tc-params link)))

     ;; If arg is not added, do nothing.
     ;; This is used by transclude-all-in-buffer; you don't want to
     ;; navigate to these files.
     (t (message "No transclusion added.")))

    (when tc-params (org-transclusion--create-at-point tc-params))))

(defun org-transclusion-marker-open (marker)
  (if-let ((tc-payload (org-transclusion--get-org-content-from-marker marker)))
      (org-transclusion--create-at-point (list :tc-type "org-id"
                                               :tc-fn (lambda ()
                                                        tc-payload)))
    (message "No transclusion added.")))

(defun org-transclusion--get-org-content-from-marker (marker)
  "Return tc-beg-mkr, tc-end-mkr, tc-content from MARKER.
This is meant for Org-ID, and advice for `org-goto-marker-or-bmk'"
  (if (and marker (marker-buffer marker)
           (buffer-live-p (marker-buffer marker)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (outline-show-all)
           (goto-char marker)
           (org-transclusion--get-org-buffer-or-element-at-point t))))
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
         (outline-show-all)
         (if search-option
             (progn
               (org-link-search search-option)
               (org-transclusion--get-org-buffer-or-element-at-point t))
           (org-transclusion--get-org-buffer-or-element-at-point)))))))

(defun org-transclusion--get-org-buffer-or-element-at-point (&optional only-element)
  "Return content for transclusion.
When ONLY-ELEMENT is t, only the element.  If nil, the whole buffer.
Assume you are in the beginning of the org element to transclude."
  (let* ((el (org-element-context))
         (type (org-element-type el))
         (tc-content)(tc-beg-mkr)(tc-end-mkr))
    ;; For dedicated target, we want to get the parent paragraph,
    ;; rather than the target itself
    (when (and (string= "target" type)
               (string= "paragraph" (org-element-type (org-element-property :parent el))))
      (setq el (org-element-property :parent el)))
    (let* ((tree (progn (if only-element
                            ;; Parse only the element in question (headline, table, paragraph, etc.)
                            (org-element--parse-elements
                             (org-element-property :begin el)
                             (org-element-property :end el)
                             'section nil 'object nil (list 'tc-paragraph nil))
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
                  nil nil '(headline section) nil)))
      (setq tc-content (org-element-interpret-data obj))
      (setq tc-beg-mkr (progn (goto-char (point-min)) (point-marker)))
      (setq tc-end-mkr (progn (goto-char (point-max)) (point-marker)))
      (list :tc-content tc-content
            :tc-beg-mkr tc-beg-mkr
            :tc-end-mkr tc-end-mkr))))

(defun org-transclusion--filter-buffer (data)
  (cond ((and (memq (org-element-type data) '(section))
              (not (eq 'tc-paragraph (org-element-type (org-element-property :parent data)))))
         ;; Intended to remove the first section, taht is the part before the first headlne
         ;; the rest of the sections are included in the headlines
         ;; Thies means that if there is no headline, nothing gets transcluded.
         (if org-transclusion-include-first-section data nil))
        (t
         ;; Rest of the case.
         (org-element-map data org-transclusion-exclude-elements (lambda (d)
                                                    (org-element-extract-element d)
                                                    nil))
         data)))

;;-----------------------------------------------------------------------------
;; Functions to support non-Org-mode link types

(defun org-transclusion--get-custom-tc-params (link)
  "Return PARAMS with TC-FN if link type is supported for LINK object."
  (let ((types org-transclusion-add-at-point-functions)
        (params nil)
        (link-type (org-element-property :type link))
        (str nil))
    (if (string= link-type org-transclusion-link)
        (setq str (org-element-property :raw-link link))
      (setq str (org-element-property :path link)))

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
          (setq params (list :tc-type type :tc-fn (lambda () (funcall add-fn str)))))))
    params))

;;-----------------------------------------------------------------------------
;; Functions to support otc: link type
;; Currently, only a few Org links are supported, so they are redundant.
;; TODO to add more e.g. Markdown

(defun org-transclusion--match-others-default (path)
  "Check if `others-default' can be used for the PATH.
Returns non-nil if check is pass."
  (not (string-prefix-p (concat org-transclusion-link ":") path)))

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
;; Core Functions
;; - Core operations: create-, save-, remove-, detach-at-point
;; - edit-src-buffer-at-point
;; - Supporting functions for these core operations

(defun org-transclusion--create-at-point (tc-params)
  "Create transclusion by unpackng TC-PARAMS."
  ;; Remove #+transclude keyword
  ;; Assume in the beginning of a link
  (when (org-transclusion--ok-to-transclude)
    (let ((key-params (org-transclusion--ok-to-transclude)))
      (save-excursion
        (forward-line -1)
        (beginning-of-line)
        ;; Assume there is no space or line feed between the keyword and link in question
        (let* ((elm (org-element-at-point))
               (beg (org-element-property :begin elm))
               (end (org-element-property :end elm)))
          (delete-region beg end))
        ;; Remove the link
        (when-let ((link-loc (org-transclusion--get-link-location))
                   (link-beg (plist-get link-loc ':begin))
                   (link-end (plist-get link-loc ':end))
                   (raw-link (buffer-substring-no-properties link-beg link-end)))

          (delete-region link-beg link-end)
          ;; Delete a char after the link has been removed to remove the line
          ;; the link used to occupy. Without this, you end up moving one line
          ;; every time add operation is called.
          (unless (eobp) (delete-char 1))

          ;; Add content and overlay
          (let* ((tc-raw-link raw-link)
                 (tc-type (plist-get tc-params :tc-type))
                 (tc-fn (plist-get tc-params :tc-fn))
                 (tc-payload (funcall tc-fn))
                 (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
                 (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
                 (tc-content (plist-get tc-payload :tc-content))
                 (beg (point)) ;; at the beginning of the text content before inserting it
                 (beg-mkr (point-marker))) ;; for source overlay

            (if (and
                 org-transclusion-use-paste-subtree
                 (org-kill-is-subtree-p tc-content))
                ;; Deactivate org-adapt-indentation temporarlily.  This is
                ;; necessary; otherwise, the transclusion links included the
                ;; demoted subtree will have a space by adaptation. It
                ;; disables further adding of transclusion links.
                (let ((org-adapt-indentation nil))
                  (org-transclusion-paste-subtree 1 tc-content t t)) ;; one line removed from original
              (insert tc-content))

            (let* ((sbuf (marker-buffer tc-beg-mkr))
                   (end (point)) ;; at the end of text content after inserting it
                   (ov-src (make-overlay tc-beg-mkr tc-end-mkr sbuf t nil)) ;; source-buffer
                   (ov-tc (make-overlay beg end nil t nil)) ;; transclusion-buiffer
                   (tc-pair (list ov-src ov-tc)))
              ;; Put to transclusion overlay
              (overlay-put ov-tc 'tc-type tc-type)
              (overlay-put ov-tc 'tc-raw-link tc-raw-link)
              (overlay-put ov-tc 'tc-beg-mkr tc-beg-mkr)
              (overlay-put ov-tc 'tc-end-mkr tc-end-mkr)
              (overlay-put ov-tc 'priority -50)
              (overlay-put ov-tc 'evaporate t)
              (overlay-put ov-tc 'face 'org-transclusion-block)
              (overlay-put ov-tc 'tc-pair tc-pair)
              (overlay-put ov-tc 'tc-key-params key-params)
              (overlay-put ov-tc 'help-echo
                           (substitute-command-keys
                            (concat "Original link: " tc-raw-link ". Visit with `\\[org-transclusion-open-src-buffer-at-point]'.")))
              (add-text-properties (overlay-start ov-tc) (overlay-end ov-tc) '(read-only t))
              ;; Put to the source overlay
              (overlay-put ov-src 'tc-by beg-mkr)
              (overlay-put ov-src 'evaporate t)
              (overlay-put ov-src 'face 'org-transclusion-source-block)
              (overlay-put ov-src 'tc-pair tc-pair))))))))

(defun org-transclusion-remove-at-point (pos &optional detach)
  "Remove transclusion and the copied text around POS.
When DETACH is non-nil, remove the tranclusion overlay only, keeping the copied
text."
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (save-excursion
        (save-restriction
          ;; Bring back the transclusion link.
          ;; Show all the folded parts is needed
          ;; as the transcluded heading might have folded
          ;; texts outside the tranclusion overlay
          (widen)
          (outline-show-all)
          (let* ((inhibit-read-only t)
                 (beg (overlay-start ov))
                 (raw-link (overlay-get ov 'tc-raw-link))
                 (key-params (overlay-get ov 'tc-key-params))
                 (new-beg (progn
                            (goto-char beg)
                            (newline)
                            (forward-line -1)
                            (insert raw-link)
                            (forward-line)
                            (point)))
                 (new-end (overlay-end ov))
                 (tc-pair (overlay-get ov 'tc-pair)))
            ;;Remove overlays
            (dolist (ol tc-pair)
              (delete-overlay ol))
            ;; TODO
            ;; Also ensure to delete all the possible orphan overlays from the source
            ;; When remove fn, delete the copied texts
            (cond
             (detach
              (setq key-params "nil")
              (remove-text-properties new-beg new-end '(read-only)))
             (t ;; remove
              ;; TODO called-interactively-p is not correctly evaluated
              (let ((inhibit-read-only t))
                (setq key-params "t")
                ;; for when detatch-at-point is called interactively
                ;; We want to stop adding it back again.

                (when (called-interactively-p) (setq key-params "nil"))
                (delete-region new-beg new-end))))
            ;; Add back #+transclusion:
            (goto-char beg)
            (insert (concat "#+transclude: " key-params "\n")))))
    ;; The message below is common for remove and detach
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-detach-at-point (pos)
  "Detach the transclusion at POS, removing the overlay only.
It needs remove the link type as well, otherwise, when the tranclusion
is active, it will automatically bring the transclusion back."
  (interactive "d")
  (org-transclusion-remove-at-point pos t))

(defun org-transclusion-open-edit-src-buffer-at-point (pos)
  "Open a clone buffer of transclusions source at POS for editting."
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (let ((from-mkr (point-marker))
            (to-mkr (overlay-get ov 'tc-beg-mkr)))
        (with-current-buffer (marker-buffer to-mkr)
          (org-with-wide-buffer
           (outline-show-all)
           (setq org-transclusion-edit-src-at-mkr from-mkr)
           (goto-char to-mkr)
           (if (and (org-transclusion--buffer-org-file-p)
                   (org-up-heading-safe))
               (org-tree-to-indirect-buffer)
             (org-transclusion--src-indirect-buffer)))
          ;; Only one edit buffer globally at a time
          (when (buffer-live-p org-transclusion-last-edit-src-buffer)
            (kill-buffer org-transclusion-last-edit-src-buffer))
          (setq org-transclusion-last-edit-src-buffer org-last-indirect-buffer)
          (pop-to-buffer org-last-indirect-buffer)
          (rename-buffer (concat "*" (buffer-name) "*"))
          (org-transclusion-edit-src-mode)))
    ;; The message below is common for remove and detach
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-open-src-buffer-at-point (pos)
  "Open source buffer link for the transclusion at point."
  (interactive "d")
  (if-let ((link (car (get-char-property-and-overlay (point) 'tc-raw-link))))
      (org-link-open-from-string link '(t))
    ;; Not in the transclusion overlay
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-edit-src-commit ()
  "Save and kill the buffer.
Meant to be used in the -edit-src-mode."
  (interactive)
  (save-buffer)
  (let ((m org-transclusion-edit-src-at-mkr))
    (pop-to-buffer (marker-buffer m))
    (org-transclusion-remove-at-point m)
    (org-transclusion-add-all-in-buffer))
  (kill-buffer org-last-indirect-buffer))

;;-----------------------------------------------------------------------------
;; Utility functions used in the core functions above

(defun org-transclusion--not-nil (v)
  "Returns t or nil.
It is like `org-not-nil', but when the value is non-nil or
not "nil", return symbol t."
  (when (org-not-nil v) t))

(defun org-transclusion--ok-to-transclude ()
  "Return t if the transclusion link at point is OK to include."
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (let ((transclude-re "^[ \t]*#\\+transclude:"))
      (when (looking-at-p transclude-re)
        ;; #+transclude: keyword exists.
        ;; Further checking the value
        (when-let ((value (org-element-property :value (org-element-at-point))))
          (when (string-match "^\\(t\\|nil\\).*$" value)
            (org-transclusion--not-nil (match-string 1 value))))))))

(defun org-transclusion--keyword-p ()
  "Return t if keyword #+transclusion: is present.
It assumes that this function is called in the beginning of a link.

TODO Add check it is indeed called in the beginning of a link"
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (let ((transclude-re "^[ \t]*#\\+transclude:"))
      (when (looking-at-p transclude-re) t))))

(defun org-transclusion--buffer-org-file-p (&optional buf)
  "Check if BUF is visiting an org file.
When BUF is nil, use current buffer. This function works for
indirect buffers."

  (let ((cbuf (or buf (current-buffer))))
    (org-transclusion--org-file-p (buffer-file-name cbuf))))

(defun org-transclusion--org-file-p (path)
  "Return non-nil if PATH is an Org file.
Checked with the extension `org'."

  (let ((ext (file-name-extension path)))
    (string= ext "org")))

(defun org-transclusion--get-link-location ()
  "Get the start and end of the link being worked on at point.
If the current point is a translusion link, return BEGIN and END
   plist: '(begin: BEGIN  end: END)
of the link.  If not link, return nil."
  (let ((location '())
        (context (org-element-context)))
    (when-let ((link (plist-get context 'link)))
      (setq location (plist-put location ':begin (plist-get link ':begin)))
      (setq location (plist-put location ':end (plist-get link ':end)))
      location)))

(defun org-transclusion--is-within-transclusion ()
  "Return t if the current point is within a tranclusion overlay."
  (when (cdr (get-char-property-and-overlay (point) 'tc-type)) t))

(defun org-transclusion--src-indirect-buffer ()
  "Clones current buffer for editing transclusion source.
It is meant to be used within
`org-transclusion-open-edit-src-buffer-at-point'.
`org-narrow-to-subtree' does not work if the point/marker is
before the first headline.  This function covers this case."
  (when (buffer-live-p org-last-indirect-buffer)
    (kill-buffer org-last-indirect-buffer))
  (let ((ibuf (org-get-indirect-buffer)))
    (setq org-last-indirect-buffer ibuf)))

;;-----------------------------------------------------------------------------
;; Define minor modes
;;

(define-minor-mode org-transclusion-mode
  "Toggle Org-transclusion minor mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :init-value nil
  :lighter " [OT]"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c n e") #'org-transclusion-open-edit-src-buffer-at-point)
            (define-key map (kbd "C-c n o") #'org-transclusion-open-src-buffer-at-point)
            map)
  (cond
   (org-transclusion-mode
    (org-transclusion-activate))
   (t
    (org-transclusion-deactivate))))

(define-minor-mode org-transclusion-edit-src-mode
  "Toggle Org-transclusion edit source mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :init-value nil
  :lighter nil
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c")
              #'org-transclusion-edit-src-commit)
            map)
  (setq header-line-format
        (substitute-command-keys
         "Editing the source directly. When done, save and return with `\\[org-transclusion-edit-src-commit]'.")))

;;-----------------------------------------------------------------------------
;; Functions to work with all transclusions in the buffer.
;; These typically call their correspondong `at-point` function

(defun org-transclusion--process-all-in-buffer-before-save ()
  "Update and remove all translusions in the current buffer `before-save-hook'."
  (setq org-transclusion-original-position (point))
  (org-transclusion-remove-all-in-buffer)) ; clean up current buffer before writing to file)

(defun org-transclusion--process-all-in-buffer-after-save ()
  "Add tranclusions back into current buffer, and save source buffers.
Meant to be for `after-save-hook'.  It adds all the transcluded copies back
into the current buffer.  And then saves all the transclusion source
buffers."

  (org-transclusion-add-all-in-buffer) ; put all tranclusions back in
  (goto-char org-transclusion-original-position)
  (setq org-transclusion-original-position nil)
  (set-buffer-modified-p nil))

(defun org-transclusion-add-all-in-buffer ()
  "Add all the transclusions in the current buffer.  As this function
should be used only on the current buffer, no argument is passed.

Adding transclusion inserts text contents after the link.  As the while
loop processes links from the top of buffer to bottom one by one, this
function avoids infinite recursion of transclusions.

The following conditions are checked before calling a function to work on
each link:

- Check if the link is in the beginning of a line
- Check if the link at point is NOT within transclusion"

  (interactive)
  ;; Check the windows being worked on is in focus (selected)
  ;; This is to prevent background hook (e.g. save hooks) from updating
  ;; the transclusion buffer.
  (cond ((not org-transclusion-mode)
         (message "Org-transclusion mode is not active."))

        ((eq (current-buffer)(window-buffer (selected-window)))
         (setq org-transclusion-buffer-modified-p (buffer-modified-p))
         (save-excursion
           (save-restriction
             (widen)
             (outline-show-all)
             (goto-char (point-min))
             ;; For `org-next-link', eq t is needed for this while loop to check
             ;; no link.  This is because fn returns a message string when there
             ;; is no further link.
             (when (org-element-link-parser)  ;; when a link is in the begging of buffer
               (when (eq (line-beginning-position)(point))
                 (org-transclusion-link-open-at-point))) ;org-open-at-point
             (while (eq t (org-next-link))
               ;; Check if the link is in the beginning of a line
               ;; Check if the link immediately follows the keyword line #+transclude:
               ;; Check if the link at point is NOT within tranclusion
               (when (and (bolp)
                          (org-transclusion--ok-to-transclude)
                          (not (org-transclusion--is-within-transclusion)))
                 ;; org-link-open (used by org-open-at-point) advised when minor mode is on
                 (org-transclusion-link-open-at-point))))) ;org-open-at-point
         (set-buffer-modified-p org-transclusion-buffer-modified-p))))

(defun org-transclusion-remove-all-in-buffer (&optional buf)
  "Remove all the translusion overlay and copied text in current buffer.
Caller can pass BUF to specify which BUF needs to remove transclusions.
This feature is meant for `org-transclusion--toggle-transclusion-when-out-of-focus'."

  (interactive)
  (when buf (set-buffer buf))
  (setq org-transclusion-buffer-modified-p (buffer-modified-p))
  (save-excursion
    (save-restriction
      (widen)
      (outline-show-all)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let ((pos (overlay-start ov)))
          (org-transclusion-remove-at-point pos)))))
  (set-buffer-modified-p org-transclusion-buffer-modified-p))

;;-----------------------------------------------------------------------------
;; Functions
;; - Activate / deactivate
;; - Toggle translusions when in and out of transclusion buffer

(defun org-transclusion-activate ()
  "Activate automatic transclusions in the local buffer.
This should be a buffer-local minior mode.  Not done yet."
  (interactive)
  (if (memq 'org-transclusion--toggle-transclusion-when-out-of-focus
            window-selection-change-functions)
      (run-with-idle-timer 0 nil 'message
                           "Nothing done. Transclusion is aleady active.")
    (setq-local window-selection-change-functions
                (push 'org-transclusion--toggle-transclusion-when-out-of-focus
                      window-selection-change-functions))
    (add-hook 'before-save-hook #'org-transclusion--process-all-in-buffer-before-save nil t)
    (add-hook 'after-save-hook #'org-transclusion--process-all-in-buffer-after-save nil t)
    ;;WIP not included yet
    ;;(advice-add 'org-metaup :around #'org-transclusion-metaup-down)
    ;;(advice-add 'org-metadown :around #'org-transclusion-metaup-down)
    (advice-add 'org-metaleft :around #'org-transclusion-metaleft-right)
    (advice-add 'org-metaright :around #'org-transclusion-metaleft-right)
    (advice-add 'org-shiftmetaleft :around #'org-transclusion-metaleft-right)
    (advice-add 'org-shiftmetaright :around #'org-transclusion-metaleft-right)
    (when org-transclusion-activate-persistent-message
      (setq header-line-format
            (substitute-command-keys
             "Transclusion active in this buffer. `\\[org-transclusion-open-edit-src-buffer-at-point]' to edit the transclusion at point."))))
  (when org-transclusion-auto-add-on-activation
    (org-transclusion-add-all-in-buffer)))

(defun org-transclusion-deactivate ()
  "Deactivate automatic transclusions in the local buffer."
  ;; Consider keeping the tc copies as read-only to be able to read
  ;; or for export mode
  (interactive)
  (if (memq 'org-transclusion--toggle-transclusion-when-out-of-focus
            window-selection-change-functions)
      (progn
        (org-transclusion-remove-all-in-buffer)
        (setq-local window-selection-change-functions
                    (remove 'org-transclusion--toggle-transclusion-when-out-of-focus
                            window-selection-change-functions))
        (remove-hook 'before-save-hook #'org-transclusion--process-all-in-buffer-before-save t)
        (remove-hook 'after-save-hook #'org-transclusion--process-all-in-buffer-after-save t)
        ;;WIP not included yet
        ;;(advice-remove 'org-metaup #'org-transclusion-metaup-down)
        ;;(advice-remove 'org-metadown #'org-transclusion-metaup-down))))
        (advice-remove 'org-metaleft #'org-transclusion-metaleft-right)
        (advice-remove 'org-metaright #'org-transclusion-metaleft-right)
        (advice-remove 'org-shiftmetaleft #'org-transclusion-metaleft-right)
        (advice-remove 'org-shiftmetaright #'org-transclusion-metaleft-right)
        (when (buffer-live-p org-transclusion-last-edit-src-buffer)
            (kill-buffer org-transclusion-last-edit-src-buffer))
        (when org-transclusion-activate-persistent-message
          (setq header-line-format nil)))
    (run-with-idle-timer 0 nil 'message
                         "Nothing done. Transclusion is not active.")))

(defun org-transclusion--toggle-transclusion-when-out-of-focus (win)
  "Detect focus state of window WIN, and toggle tranclusion on and off.
The toggling is done via adding and removing all from the appropirate buffer,
depending on whether the focus is coming in or out of the tranclusion buffer."
  (unless org-transclusion-debug
    (let ((buf (window-buffer win)))
      (cond ((minibufferp (current-buffer))
             (message "going into minibuffer") nil) ;; do nothing
            ((string-match-p "*.*" (buffer-name (current-buffer)))
             (message "going into buffer with *<buffer-name>*") nil)
            ((eq buf (current-buffer))
             (message "coming into %s" win)
             (org-transclusion-add-all-in-buffer)) ;; add all back in
            (t
             (message "going from %s into %s" buf (current-buffer))
             (org-transclusion-remove-all-in-buffer buf)))))) ;; remove all

;;-----------------------------------------------------------------------------
;; Metaup/down; metaleft/right metashiftleft/right
(defun org-transclusion-metaup-down (oldfn &optional arg)
  "TODO. WIP. "
  ;;; This implementation does not do what I want.
  ;;; When headline moves, the overlay is deleted.
  ;;; Changing the evaporate property to nil does not work either.
  ;;; Need to add stars to the keyword for all the overlays (not a bit issue.)
  "Temporarily remove text-only attributes to allow for metaup/down to move headlines around."
  (interactive)

  (if-let (ov (cdr (get-char-property-and-overlay (point) 'tc-type)))
      ;; Only if you are in the transclusion overlay
      (progn
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((inhibit-read-only t))
            (add-text-properties (overlay-start ov) (overlay-end ov) '(read-only nil))))
        ;; Call the normal metaup/down
        (funcall oldfn arg)
        ;; After calll metaup/down
        (dolist (ov (overlays-in (point-min) (point-max)))
          (add-text-properties (overlay-start ov) (overlay-end ov) '(read-only t))))
    ;; If not in the transclusion overlay, do as normal.
    (funcall oldfn arg)))

(defun org-transclusion-metaleft-right (oldfn &optional arg)
  "Metashift/right, rather than metaleft/right.
This is because we want to treat the whole subtree as one unit."
  (interactive)
    (if-let (ov (cdr (get-char-property-and-overlay (point) 'tc-type)))
      ;; Only if you are in the transclusion overlay
      (progn
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((inhibit-read-only t))
            (add-text-properties (overlay-start ov) (overlay-end ov) '(read-only nil))))
        ;; Call the normal metaup/down
        (funcall oldfn)
        ;; After calll metaup/down
        (dolist (ov (overlays-in (point-min) (point-max)))
          (add-text-properties (overlay-start ov) (overlay-end ov) '(read-only t))))
    ;; If not in the transclusion overlay, do as normal.
    (funcall oldfn)))

;;-----------------------------------------------------------------------------
;; Definition of org-transclusion-paste-subtree
;;

(defvar org-transclusion-use-paste-subtree t)

(defun org-transclusion-paste-subtree (&optional level tree for-yank remove)
  "Paste the clipboard as a subtree, with modification of headline level.

----- Note for Org-transclusion -----
This is a Org-transclusion version of the Org Mode's standard function
`org-paste-subtree' (based on v9.4). Only one line has been commented out.
This is noted with a comment \";; nobiot removed\".

Use of this function in Org-transclusion is still experimental.
Use variable `org-transclusion-use-paste-subtree' to control its usage.
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

(provide 'org-transclusion)
;;; org-transclusion.el ends here
