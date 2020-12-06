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
global.  This is a deliberte design choice.  You may activate
Org-transclusion for multiple buffers at a time.  But editing
their sources should be a focused task, and thus one edit buffer
can be open at a time.

Killing a clone buffer is assumed to be safe in general, as its original
buffer is in sync and the content is reflected there.")

(defvar org-transclusion-debug nil
  "Disable the toggle of transclusions.
It is meant to enable edebugging.  Without this, switching to the source
code buffer for runtime debugger toggles off the transclusion, and thus
makes it impossible to debug at runtime.")

(defvar org-transclusion-use-paste-subtree t)

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
;;

(defface org-transclusion-source-block
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

(defface org-transclusion-block
  '((((class color) (min-colors 88) (background light))
     :background "#efefef" :extend t)
    (((class color) (min-colors 88) (background dark))
     :foreground "#bfc0c" :background "#1e1e1e" :extend t))
  "Face for transcluded block."
  :group 'org-transclusion)

;; WIP. Not working
(defface org-transclusion-keyword
  '((t (:foreground "gray90" :extend t)))
    "Face for the :transclusion keyword."
    :group 'org-transclusion)

;;-----------------------------------------------------------------------------
;; Functions to override org-link-open
;; Transclude standard Org Mode file links
;; Add Support different non-Org link types

(defun org-transclusion-link-open-at-point (&optional arg)
  "Pass Org mode's link object to `org-transclusion-link-open'.
This function assumes the point is at the beginning of a link.
\\[universal-argument] (ARG) can be passed to force it to open the link
with `org-link-open'."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-property :type context)))
    (cond
     ((and org-transclusion-mode
           (or (string= "id" type)
               (string= "file" type)))
      (org-transclusion-link-open context))
     ;; For other cases. Do nothing
     (t (message "Nothing done. Not at a link, or link not supported.")))))

(defun org-transclusion-link-open (link &optional arg)
  "Check the LINK can be transcluded, and open it to transclude its content.
If the LINK type is not supported by org-transclusion, or \\[universal-argument]
is used (ARG is non-nil), then use the standard `org-link-open'."
  (let ((tc-params nil))
    (cond
     ;; Check the link is meant for translusion
     ((not (org-transclusion--ok-to-transclude)) (org-link-open link arg))
     ;; Check if ARG is passed
     (arg (org-link-open link arg))
     ;; For Org-ID
     ((string= "id" (org-element-property :type link))
      ;; when type is id, the value of path is the id
      (let* ((id (org-element-property :path link))
             (mkr (ignore-errors (org-id-find id t))))
        (if mkr (progn
                  (setq tc-params (list :tc-type "org-id"
                                        :tc-arg mkr
                                        :tc-fn #'org-transclusion--get-org-content-from-marker)))
          (message "No transclusion done for this ID. Ensure it works."))))
     ;; Other Org file links
     ((org-transclusion--org-file-p (org-element-property :path link))
      (setq tc-params (list :tc-type "org-link"
                            :tc-arg link
                            :tc-fn #'org-transclusion--get-org-content-from-link)))
     ;; For non-Org files
     ((setq tc-params (org-transclusion--get-custom-tc-params link)))
     ;;All the other cases
     (t (message "No transclusion added.")))
    ;; Do transclusion when tc-params are populated
    (when tc-params (org-transclusion--create-at-point tc-params))))

;; Not used but might be useful...
;; (defun org-transclusion-marker-open (marker)
;;   (if-let ((tc-payload (org-transclusion--get-org-content-from-marker marker)))
;;       (org-transclusion--create-at-point (list :tc-type "org-id"
;;                                                :tc-fn (lambda ()
;;                                                         tc-payload)))
;;     (message "No transclusion added.")))

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
         ;;(outline-show-all)
         (if search-option
             (progn
               (org-link-search search-option)
               (org-transclusion--get-org-buffer-or-element-at-point t))
           (org-transclusion--get-org-buffer-or-element-at-point)))))))

(defun org-transclusion--get-org-buffer-or-element-at-point (&optional only-element)
  "Return content for transclusion.
When ONLY-ELEMENT is t, only the element.  If nil, the whole buffer.
Assume you are at the beginning of the org element to transclude."
  (if-let* ((el (org-element-context))
            (type (org-element-type el)))
      (let ((parse-mode 'section) ;; default is 'section. For org-element--parse-elements
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
                                (org-element--parse-elements
                                 (org-element-property :begin el)
                                 (org-element-property :end el)
                                 parse-mode nil 'object nil (list 'tc-paragraph nil))
                              ;; If not only-element, then parse the entire buffer
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
          (setq tc-beg-mkr (progn (goto-char (org-element-property :begin el)) (point-marker)))
          (setq tc-end-mkr (progn (goto-char (org-element-property :end el)) (point-marker)))
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

;;-----------------------------------------------------------------------------
;; Functions to support non-Org-mode link types

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
;; Core Functions
;; - Core operations: create-, save-, remove-, detach-at-point
;; - edit-src-buffer-at-point
;; - Supporting functions for these core operations

(defun org-transclusion--create-at-point (tc-params)
  "Create transclusion by unpacking TC-PARAMS.
TC-PARAM is a plist of the following properties:

tc-type :: a string to indicate what link type the content came
from: e.g. \"org-id\"

tc-arg :: argment used by the tc-fn

tc-fn :: function's symbol used.

A \"tc-payload\" is then obtained by (funcall tc-fn tc-arg).
tc-pay-load is anotehr plist consisting of the following properties:

tc-beg-mkr :: a marker pointing to the beginning of the content in the source
buffer

tc-end-mkr :: a marker pointing to the end of the content in the source
buffer

tc-content :: the actual text content to be transcluded"
  ;; Remove #+transclude keyword
  ;; Assume in the beginning of a link
  (when (org-transclusion--ok-to-transclude)
    (let* ((keyword-values (org-transclusion--get-keyword-values))
           (tc-type (plist-get tc-params :tc-type))
           (tc-arg (plist-get tc-params :tc-arg))
           (tc-fn (plist-get tc-params :tc-fn))
           (tc-payload (funcall tc-fn tc-arg))
           (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
           (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
           (tc-content (plist-get tc-payload :tc-content)))
      (if (or (string= tc-content "")
              (eq tc-content nil))
          (message "Nothing done. No content is found through the link.")
        ;; Do creation only when there is content to be transcluded
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
                   (beg (point)) ;; at the beginning of the text content before inserting it
                   (beg-mkr (point-marker))) ;; for source overlay
              (if (and
                   org-transclusion-use-paste-subtree
                   (org-kill-is-subtree-p tc-content))
                  ;; Deactivate org-adapt-indentation temporarlily.  This is
                  ;; necessary; otherwise, the transclusion links included the
                  ;; demoted subtree will have a space by adaptation. It
                  ;; disables further adding of transclusion links.
                  (let ((org-adapt-indentation nil)
                        (hlevel (plist-get keyword-values ':hlevel)))
                    (when hlevel (setq hlevel (string-to-number hlevel)))
                    (org-transclusion-paste-subtree hlevel tc-content t t)) ;; one line removed from original
                (insert tc-content))
              (let* ((sbuf (marker-buffer tc-beg-mkr)) ;source buffer
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
                (overlay-put ov-tc 'tc-keyword-values keyword-values)
                (overlay-put ov-tc 'help-echo
                             (substitute-command-keys
                              (concat "Original link: " tc-raw-link ". Visit with `\\[org-transclusion-open-src-buffer-at-point]'.")))
                (add-text-properties (overlay-start ov-tc) (overlay-end ov-tc) '(read-only t))
                ;; Put to the source overlay
                (overlay-put ov-src 'tc-by beg-mkr)
                (overlay-put ov-src 'evaporate t)
                (overlay-put ov-src 'face 'org-transclusion-source-block)
                (overlay-put ov-src 'tc-pair tc-pair)))))))))

(defun org-transclusion-remove-at-point (pos &optional mode stars)
  "Remove transclusion and the copied text around POS.
When MODE is 'detatch, remove the tranclusion overlay only,
keeping the copied text, and the original link.

 When MODE is 'stars, it is meant for
`org-transclusion-metaup-down'.  The the number of STARS are
added to the keyword when the transclusion is removed to make
headlines."
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (save-excursion
        (save-restriction
          ;; Bring back the transclusion link.
          ;; Show all the folded parts is needed
          ;; as the transcluded heading might have folded
          ;; texts outside the tranclusion overlay
          (widen)
          ;;(outline-show-all)
          (let* ((inhibit-read-only t)
                 (beg (overlay-start ov))
                 (raw-link (overlay-get ov 'tc-raw-link))
                 (keyword-values (mapconcat
                                  (lambda (v) (if (symbolp v) (symbol-name v) v))
                                  (overlay-get ov 'tc-keyword-values) " "))
                 (t-or-nil)
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
             ((eq mode 'detach)
              (setq t-or-nil "nil")
              (remove-text-properties new-beg new-end '(read-only)))
             (t ;; remove
              ;; TODO called-interactively-p is not correctly evaluated
              (let ((inhibit-read-only t))
                ;; for when detatch-at-point is called interactively
                ;; We want to stop adding it back again.
                (if (called-interactively-p 'interactive) (setq t-or-nil "nil")
                  (setq t-or-nil "t"))
                (delete-region new-beg new-end))))
            ;; Add back #+transclusion:
            (goto-char beg)
            (when (eq mode 'stars)
              (unless stars (setq stars 1))
              (insert (propertize (make-string stars ?*) 'tc-metamove t) " "))
            (insert (concat "#+transclude: " t-or-nil " " keyword-values "\n")))))
    ;; The message below is common for all the modes
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-detach-at-point (pos)
  "Detach the transclusion at POS, removing the overlay only.
It needs remove the link type as well, otherwise, when the tranclusion
is active, it will automatically bring the transclusion back."
  (interactive "d")
  (org-transclusion-remove-at-point pos 'detach))

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
  "Open source buffer link for the transclusion at POS."
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
  "Return t or nil.
It is like `org-not-nil', but when the V is non-nil or not
string \"nil\", return symbol t."
  (when (org-not-nil v) t))

(defun org-transclusion--get-keyword-values ()
  "Return the \"#+transcldue:\" keyword's values if any.
The first t/nil value to control transclusion is NOT returned.
The values are returned as plist.  Currently it only expects
:hlevel value from 1 to 9.
Others are ignored and removed."
    (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (let ((transclude-re "^[ \t]*#\\+transclude:")
          (plist))
      (when (looking-at-p transclude-re)
        ;; #+transclude: keyword exists.
        ;; Further checking the value
        (when-let ((value (org-element-property :value (org-element-at-point))))
          (when (string-match ":hlevel *\\([1-9]\\)" value)
            (setq plist
                  (plist-put plist :hlevel (match-string 1 value)))))))))

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
            (and (org-transclusion--not-nil (match-string 1 value))
                 ;; If inserting in or immediately after another overaly -
                 ;; read-onl, it transclusion fails with "Read-Only" error
                 (not (get-pos-property (point) 'read-only)))))))))

;; Not used. Candidate for removal
;; (defun org-transclusion--keyword-p ()
;;   "Return t if keyword #+transclusion: is present.
;; It assumes that this function is called in the beginning of a link.

;; TODO Add check it is indeed called in the beginning of a link"
;;   (save-excursion
;;     (forward-line -1)
;;     (beginning-of-line)
;;     (let ((transclude-re "^[ \t]*#\\+transclude:"))
;;       (when (looking-at-p transclude-re) t))))

(defun org-transclusion--buffer-org-file-p (&optional buf)
  "Check if BUF is visiting an org file.
When BUF is nil, use current buffer.  This function works for
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
   (t (org-transclusion-deactivate))))

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
  "Add all the transclusions in the current buffer.
As this function should be used only on the current buffer, no
argument is passed.

Adding transclusion inserts text contents after the link.  This
function avoids infinite recursion of transclusions.

It retains the `buffer-modified-p' status before transcluding any
linked contents.  This means transclusion is not considered
modification to the buffer for the auto save facilities, such as
`auto-save-mode' and `auto-save-visited-mode'.

The following conditions are checked before calling a function to work on
each link:

- Check if the link is preceded by a #+transclusion keyword with value t
- Check if the link at point is NOT within transclusion"
  (interactive)
  ;; Check the windows being worked on is in focus (selected)
  ;; This is to prevent background hook (e.g. save hooks) from updating
  ;; the transclusion buffer.
  (cond ((not org-transclusion-mode)
         (message "Org-transclusion mode is not active."))
        ((eq (current-buffer)(window-buffer (selected-window)))
         (setq org-transclusion-buffer-modified-p (buffer-modified-p))
         (org-with-wide-buffer
          (goto-char (point-min))
          ;; We do not need to consider the case where the link is at
          ;; point-min, because we need the #+transclusion keyword.
          ;; Skip this:
          ;; (when (org-element-link-parser)  ;; when a link is in the begging of buffer
          (while (eq t (org-next-link))
            ;; For `org-next-link', eq t is needed for this while loop to check
            ;; no link.  This is because fn returns a message string when there
            ;; is no further link.
            ;; Check if the link is in the beginning of a line
            ;; Check if the link immediately follows the keyword line #+transclude:
            ;; Check if the link at point is NOT within tranclusion
            (when (and (bolp)
                       (org-transclusion--ok-to-transclude)
                       (not (org-transclusion--is-within-transclusion)))
              (org-transclusion-link-open-at-point))))
         (set-buffer-modified-p org-transclusion-buffer-modified-p))))

(defun org-transclusion-remove-all-in-buffer (&optional buf add-stars)
  "Remove all the translusion overlay and copied text in current buffer.
Caller can pass BUF to specify which BUF needs to remove transclusions.
`org-transclusion-metaup-down' use this function with ADD-STARS.
The values can be 't or 'only-with-headline.

It retains the `buffer-modified-p' status before removing any
transclusions; this is not considered modification to the buffer
for the auto save facilities, such as `auto-save-mode' and
`auto-save-visited-mode'."
  (interactive)
  (when buf (set-buffer buf))
  (setq org-transclusion-buffer-modified-p (buffer-modified-p))
  (org-with-wide-buffer
   (dolist (ov (overlays-in (point-min) (point-max)))
     (let ((pos (overlay-start ov))
           (switch)
           (level))
       (when pos
         (goto-char pos)
         (when (or (eq add-stars t)
                   (and (eq add-stars 'only-with-headline)
                        (org-transclusion--has-headline-p)))
           (org-transclusion--move-to-root-hlevel-of-transclusion-at-point)
           (setq switch 'stars)
           (setq level (org-outline-level)))
         (org-transclusion-remove-at-point pos switch level)))))
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
    (advice-add 'org-metaup :around #'org-transclusion-metaup-down)
    (advice-add 'org-metadown :around #'org-transclusion-metaup-down)
    (advice-add 'org-shiftmetaup :around #'org-transclusion-shiftmetaup)
    (advice-add 'org-shiftmetadown :around #'org-transclusion-shiftmetadown)
    (advice-add 'org-metaleft :around #'org-transclusion-metaleft)
    (advice-add 'org-metaright :around #'org-transclusion-metaright)
    (advice-add 'org-shiftmetaleft :around #'org-transclusion-metaleft)
    (advice-add 'org-shiftmetaright :around #'org-transclusion-metaright)
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
        (advice-remove 'org-metaup #'org-transclusion-metaup-down)
        (advice-remove 'org-metadown #'org-transclusion-metaup-down)
        (advice-remove 'org-shiftmetaup #'org-transclusion-shiftmetaup)
        (advice-remove 'org-shiftmetadown #'org-transclusion-shiftmetadown)
        (advice-remove 'org-metaleft #'org-transclusion-metaleft)
        (advice-remove 'org-metaright #'org-transclusion-metaright)
        (advice-remove 'org-shiftmetaleft #'org-transclusion-metaleft)
        (advice-remove 'org-shiftmetaright #'org-transclusion-metaright)
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
             ;; TODO This cannot add transclusion when moving from one
             ;; transclusion buffer to another.  I think it needs to check the
             ;; condition if both from and to buffers have minior mode on
             (org-transclusion-remove-all-in-buffer buf)))))) ;; remove all

;;-----------------------------------------------------------------------------
;; Metaup/down; metaleft/right metashiftleft/right

(defun org-transclusion-shiftmetaup (_oldfn &optional _arg)
  (org-transclusion-up-down #'org-move-subtree-up t))

(defun org-transclusion-shiftmetadown (_oldfn &optional _arg)
  (org-transclusion-up-down #'org-move-subtree-down t))

(defun org-transclusion-metaup-down (oldfn &optional _arg)
  (org-transclusion-up-down oldfn 'only-with-headline))

(defun org-transclusion-up-down (oldfn add-stars-switch)
  "Move the transcluded region at point up and down as a unified chunk.
It only works on the headlines within a transclusion.
Otherwise, the original function (OLDFN with optional ARG) is used.
Need to add stars to the keyword for all the overlays (not a bit issue.)
Temporarily remove text-only attributes to allow for metaup/down to move headlines around.

meta -- subtree down
shiftmeta -- drag line down"
  (interactive)
  (if-let ((ov (org-transclusion--get-overlay-at-point)))
      ;; Only if you are in the transclusion overlay
      (progn
        ;; Call the normal metaup/down
        (org-transclusion--add-temporarly-headline-stars add-stars-switch)
        (when (org-at-heading-p)
          (ignore-errors (funcall oldfn)))
        ;; Remove heading on the keyword
        ;; Go through all the headlines, if tc-metamove is on, toggle headline off
        (org-transclusion--remove-all-temporarly-headline-stars)
        ;; After calll metaup/down
        (org-transclusion-add-all-in-buffer))
    ;; If not in the transclusion overlay.
    (org-transclusion--add-temporarly-headline-stars add-stars-switch)
    (ignore-errors (funcall oldfn))
    (org-transclusion--remove-all-temporarly-headline-stars)
    (org-transclusion-add-all-in-buffer)))

(defun org-transclusion-metaleft (oldfn &optional arg)
  (org-transclusion-metaleft-right oldfn 'left arg))

(defun org-transclusion-metaright (oldfn &optional arg)
  (org-transclusion-metaleft-right oldfn 'right arg))

(defun org-transclusion-metaleft-right (oldfn left-or-right &optional _arg)
  "Metashift/right, rather than metaleft/right.
This is because we want to treat the whole subtree as one unit.

Check if the point is on the heading < this check is probably not
necessary as the region is read only. Move to the highest, and
then call metashift, instead of meta."
  (interactive)
  (if-let* ((ov (org-transclusion--get-overlay-at-point))
            (ov-beg (overlay-start ov))
            (ov-end (overlay-end ov)))
      (progn ;; if you are in the transclusion overlay
        (let ((inhibit-read-only t))
          (add-text-properties ov-beg ov-end '(read-only nil)))
        (org-with-wide-buffer
         (let ((more-subtrees-mkr '()))
           ;; get the list of all the subtrees
           (save-excursion
             (goto-char ov-beg)
             (org-transclusion--move-to-root-hlevel-of-transclusion-at-point ov)
             (when (org-at-heading-p) (push (point-marker) more-subtrees-mkr))
             (while (or (org-forward-heading-same-level 1) ;; always nil                        (or ;; either didn't move or not
                        (and (org-transclusion--point-is-within-transclusion ov)
                        ;; I want to get out as soon as either it's outside or didn't move
                             ;; Keep the loop when both inside and moved
                             ;; no movement means no heading of the same level))
                             (not (eq (marker-position (car (last more-subtrees-mkr))) (point)))))
               (setq more-subtrees-mkr (append more-subtrees-mkr (list (point-marker))))))
           ;; Rerse the order of the list to do from top to bottom
           (dolist (m more-subtrees-mkr)
             (goto-char m)
             ;; (org-transclusion--move-to-root-hlevel-of-transclusion-at-point ov)
             (if (eq left-or-right 'left)
                 (ignore-errors (org-promote-subtree))
               ;; As this function is advised for only org-shiftmetaright/left
               ;; org-metaleft/right, the rest of the case must be either metaleft
               ;; or shiftmetaleft
               (ignore-errors (org-demote-subtree)))))
         (org-transclusion--update-hlevel-at-point))
        ;; After calll metaleft/right
        (add-text-properties ov-beg ov-end '(read-only t)))
    ;; If not in the transclusion overlay, do as normal.
    ;; ignore-errors needed to ignore "read-only"
    ;; when the headline subsumes a transcusion region
    (ignore-errors (funcall oldfn))))

(defun org-transclusion--get-overlay-at-point ()
  "Returns overlay object of the transclusion at point."
  (cdr (get-char-property-and-overlay (point) 'tc-type)))

(defun org-transclusion--has-headline-p ()
  "Returns t if the transclusion at point contains a headline."
  (if-let* ((ov (org-transclusion--get-overlay-at-point))
            (beg (overlay-start ov))
            (end (overlay-end ov)))
      (save-excursion
        (goto-char beg)
        (when (re-search-forward org-heading-regexp end t 1) t))))

(defun org-transclusion--add-temporarly-headline-stars (add-stars-switch)
  "Add temporary headline stars to the \"#+transclude:\" keyword.
This function is meant to be used for
`org-transclusion-metaup-down'."
  (org-transclusion-remove-all-in-buffer (current-buffer) add-stars-switch))

(defun org-transclusion--remove-all-temporarly-headline-stars ()
  "Remove temporary headline stars from the \"#+transclude:\" keyword.
This function is meant to be used for
`org-transclusion-metaup-down'."
  (org-with-wide-buffer
   (org-show-all)
   (goto-char (point-min))
   (or (and (bobp)(org-at-heading-p))
       (or (org-next-visible-heading 1) t))
   (while (or (and (bobp)(org-at-heading-p))
              (and (not (eobp))(org-at-heading-p)))
     (when (get-text-property (point) 'tc-metamove)
       (org-toggle-heading))
     (org-next-visible-heading 1))))

(defun org-transclusion--point-is-within-transclusion (&optional ov pos)
  "Return non-nil if POS is within transclusion OV.
If OV is not passed, it will be the overlay at the current point.
If POS is not passed it will be the current point."
  (let ((ov (progn (if ov ov (org-transclusion--get-overlay-at-point)))))
     ;; only when the OV is a transclusion overlay.
    (when (overlay-get ov 'tc-type)
      (let ((beg (overlay-start ov))
            (end (overlay-end ov))
            (p (progn (if pos pos (point)))))
        ;; Check Point is between beg (inclusive) and end (exclusive) end
        ;; check can be inclusive, but the point is assumed to be in the
        ;; beginning of a line. And overlay does not end there.
        (and (<= beg p)
             (> end p))))))

(defun org-transclusion--move-to-root-hlevel-of-transclusion-at-point (&optional ov)
  "Move to the root of subtree within the transclusion at point.
Optionally OVerlay can be passed. If not passed, this function will get one at point."
  (when-let* ((ov (progn (if ov ov (org-transclusion--get-overlay-at-point))))
         (ov-beg (overlay-start ov))
         (ov-end (overlay-end ov)))
    ;; This function can be in a while loop and need to have a clear exit
    ;; criteria It should not "initialize" when the point is already out of
    ;; the OVerlay.
    ;; NO IT NEEDS TO BE GUARANTEED that this is called at the beginning
    ;; (when (org-transclusion--point-is-within-transclusion ov) (goto-char ov-beg))
    ;; Check if the point is at heading or first section before the first
    ;; headline.  If not a heading, it can be a paragraph, block element,
    ;; etc. with no heading, or first section.  Move to the next heading. If
    ;; it is the first section followed by a heading, continue.  If not go
    ;; back to the beginning of the overlay and process as normal.
    (unless (org-at-heading-p)
      (org-next-visible-heading 1)
      (unless (org-transclusion--is-within-transclusion) (goto-char ov-beg)))
    (while (save-excursion
             ;; Check the destination is within the current transclusion
             ;; before actually moving the point
             (and (org-up-heading-safe)
                  (org-transclusion--point-is-within-transclusion ov)))
      ;; Move point
      (org-up-heading-safe))))
      ;;(beginning-of-line)

(defun org-transclusion--update-hlevel-at-point ()
  "Assume the point is on the headline."
  (let* ((level (org-current-level))
         (ov (cdr (get-char-property-and-overlay (point) 'tc-type)))
         (plist (overlay-get ov 'tc-keyword-values)))
    (setq plist (plist-put plist ':hlevel (number-to-string level)))
    (overlay-put ov 'tc-keyword-values plist)))

;;-----------------------------------------------------------------------------
;; Definition of org-transclusion-paste-subtree
;;

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
