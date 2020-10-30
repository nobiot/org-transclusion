;;; org-transclusion.el --- transclude text contents of linked target -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Noboru Ota

;; Author: Noboru Ota <me@nobiot.com>
;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing
;; Version: 0.0.4
;; Package-Requires: ((emacs "26.3") (org "9.3")

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
;; TODO Most of these should be defcustom

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
  "Disables the toggle of transclusions.
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

(defcustom org-transclusion-link "otc"
  "Defines custom org link type used for transclusion links."
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

(defun org-transclusion-link-open (orgfn link &optional arg)
  "Override Org Mode's default `org-link-open' (ORGFN) for LINK.
Meant to be used with add-advice/remove-advice in activate/deactivate.

If the link type is not supported by org-transclusion, or \\[universal-argument]
is used (ARG is non-nil), then use `org-link-open'."

  (let ((tc-params nil))
    (cond (arg (apply orgfn link arg))
          
          ((string= "id" (org-element-property :type link))
           (let ((tc-payload (org-transclusion--get-org-content-from-link orgfn link arg)))
             (setq tc-params (list :tc-type "org-id"
                                   :tc-fn (lambda ()
                                            tc-payload)))))
          ((org-transclusion--org-file-p (org-element-property :path link))
           (let ((tc-payload (org-transclusion--get-org-content-from-link orgfn link arg)))
             (setq tc-params (list :tc-type "org-link"
                                   :tc-fn (lambda ()
                                            tc-payload)))))
          
          ((setq tc-params (org-transclusion--get-custom-tc-params link)))
          
          ;; If arg is not added, do nothing.
          ;; This is used by transclude-all-in-buffer; you don't want to
          ;; navigate to these files.
          (t "No transclusion added."))
    (when tc-params (org-transclusion--create-at-point tc-params))))

(defun org-transclusion--get-org-content-from-link (orgfn link &rest _arg)
  "Return tc-beg-mkr, tc-end-mkr, tc-content from LINK using ORGFN."
  (save-window-excursion
    (funcall orgfn link)
    (let* ((el (org-element-context))
           (type (org-element-type el))
           (beg)(end)(tc-content)(tc-beg-mkr)(tc-end-mkr))
      (when (and (string= "target" type)
                 (string= "paragraph" (org-element-type (org-element-property :parent el))))
        (setq el (org-element-property :parent el)))
      (setq beg (org-element-property :begin el))
      (setq end (org-element-property :end el))
      (setq tc-content (buffer-substring beg end))
      (setq tc-beg-mkr (progn (goto-char beg) (point-marker)))
      (setq tc-end-mkr (progn (goto-char end) (point-marker)))
      (list :tc-content tc-content
            :tc-beg-mkr tc-beg-mkr
            :tc-end-mkr tc-end-mkr))))

;;-----------------------------------------------------------------------------
;; Functions to support non-Org-mode link types

(defun org-transclusion--get-custom-tc-params (link)
  "Return PARAMS with TC-FN if link type is supported for LINK object.

TODO This is a little ugly inthat it takes an Org Mode's link object, and
for `org-transclusion-link' link type (default `otc'), it takes the raw-link.
For others, it requires path."
  
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

  ;; Remove the link
  (when-let ((link-loc (org-transclusion--get-link-location))
             (link-beg (plist-get link-loc ':begin))
             (link-end (plist-get link-loc ':end))
             (raw-link (buffer-substring-no-properties link-beg link-end)))
    
    (delete-region link-beg link-end)
    ;; Delete a char after the link has been removed to remove the line
    ;; the link used to occupy. Without this, you end up moving one line
    ;; every time add operation is called.
    (delete-char 1)
    ;; TODO You need to check if the link is at the bottom of buffer
    ;; If it is, then yank won't work.

    ;; Add content and overlay
    (let* ((tc-raw-link raw-link)
           (tc-type (plist-get tc-params :tc-type))
           (tc-fn (plist-get tc-params :tc-fn))
           (tc-payload (funcall tc-fn))
           (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
           (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
           (tc-content (plist-get tc-payload :tc-content)))
      (save-excursion
        ;;(org-paste-subtree 2 tc-content nil))
        (insert tc-content))
      (let* ((sbuf (marker-buffer tc-beg-mkr))
             (pt-end (+ (point) (- tc-end-mkr tc-beg-mkr)))
             (ov-src (make-overlay tc-beg-mkr tc-end-mkr sbuf t nil)) ;; source-buffer
             (ov-tc (make-overlay (point) pt-end nil t nil)) ;; transclusion-buiffer
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
        (add-text-properties (overlay-start ov-tc) (overlay-end ov-tc) '(read-only t))
        ;; Put to the source overlay
        (save-excursion
          (goto-char (overlay-start ov-tc))
          (overlay-put ov-src 'tc-by (point-marker)))
        (overlay-put ov-src 'evaporate t)
        (overlay-put ov-src 'face 'org-transclusion-source-block)
        (overlay-put ov-src 'tc-pair tc-pair)))))

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
                 (new-beg (progn
                            (goto-char beg)
                            (newline)
                            (forward-line -1)
                            (insert raw-link)
                            (forward-line)
                            (point)))
                 (new-end (overlay-end ov))
                 (tc-pair (overlay-get ov 'tc-pair)))
            (dolist (ol tc-pair)
              (delete-overlay ol))
            ;; TODO
            ;; Also ensure to delete all the possible orphan overlays from the source
            ;; When remove fn, delete the copied texts
            (unless detach
              (let ((inhibit-read-only t))
                (delete-region new-beg new-end))))))
    ;; The message below is common for remove and detach
    (message "Nothing done. No transclusion exists here.")))

(defun org-transclusion-detach-at-point (pos)
  "Detach the transclusion at POS, removing the overlay only.
It needs remove the link type as well, otherwise, when the tranclusion
is active, it will automatically bring the transclusion back."
  (interactive "d")
  (org-transclusion-remove-at-point pos t)
  ;; this OR is necessary to check if the function is called at
  ;; the beginning of the overlay.
  (let* ((link (or (org-element-link-parser)
                   (progn (org-next-link t)
                          (org-element-link-parser))))
         (end (org-element-property :end link))
         (link-type (org-element-property :type link))
         (type (concat org-transclusion-link ":")))
    (when (string= link-type org-transclusion-link)
      (search-forward type end t 1)
      (delete-char (- 0 (length type))))))
  
(defun org-transclusion-open-edit-src-buffer-at-point (pos)
  "Open a clone buffer of transclusions source at POS for editting."
  
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (let ((from-mkr (point-marker))
            (to-mkr (overlay-get ov 'tc-beg-mkr)))
        (with-current-buffer (marker-buffer to-mkr)
          (org-with-wide-buffer
           (setq org-transclusion-edit-src-at-mkr from-mkr)
           (goto-char to-mkr)
           (if (org-up-heading-safe);; if non-nil, it's before the first subtree
               (org-tree-to-indirect-buffer)
             (org-transclusion--src-indirect-buffer)))
          ;; Only one edit buffer globally at a time
          (when (buffer-live-p org-transclusion-last-edit-src-buffer)
            (kill-buffer org-transclusion-last-edit-src-buffer))
          (setq org-transclusion-last-edit-src-buffer org-last-indirect-buffer)
          (pop-to-buffer org-transclusion-last-edit-src-buffer)
          (rename-buffer (concat "*" (buffer-name) "*"))
          (org-transclusion-edit-src-mode)))
    ;; The message below is common for remove and detach
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

(defun org-transclusion--transclusion-link-p ()
  "Check if the link at point is a tranclusion link."

  (when-let ((link (plist-get (org-element-context) 'link)))
    (let ((type (plist-get link ':type)))
      (string= type org-transclusion-link))))

(defun org-transclusion--src-indirect-buffer ()
  "Clones current buffer for editing transclusion source.
It is meant to be used within
`org-transclusion-open-edit-buffer-at-point'.
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
            (define-key map (kbd "C-c n e")
              'org-transclusion-open-edit-buffer-at-point)
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
Meant to be for `after-save-hook'.
It adds all the transcluded copies back into the current buffer.
And then saves all the transclusion source buffers."
  (org-transclusion-add-all-in-buffer) ; put all tranclusions back in
  (goto-char org-transclusion-original-position)
  (setq org-transclusion-original-position nil)
  (set-buffer-modified-p nil))

(defun org-transclusion-add-all-in-buffer ()
  "Add all the transclusions in the current buffer.
As this should be used only when the buffer is current, no argment passed.

As transclusing adds text after the link, the loop needs to process from top to
bottom one by one.  The transcluded text may contrain transclusion link.

TODO check needs to be added back in? to avoid recursion."
  
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
             ;; is no further link.  The OR condition supports when a link is in
             ;; the begging of buffer
             (when (org-element-link-parser)
               (when (eq (line-beginning-position)(point))
                 (org-open-at-point)))
             (while (eq t (org-next-link))
               ;; Check if the link at point is tranclusion link
               ;; Check if the link is in the beginning of a line
               (when (eq (line-beginning-position)(point))
                 (org-open-at-point)))))
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
    (advice-add 'org-link-open :around #'org-transclusion-link-open)
    (when org-transclusion-activate-persistent-message
      (setq header-line-format
	    "Transclusion active in this buffer")))
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
        (advice-remove 'org-link-open #'org-transclusion-link-open)
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

(provide 'org-transclusion)
;;; org-transclusion.el ends here
