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
There should be only one edit source buffer at a time.  This is so that you
avoid opening too many clone buffers. It is also used to close the edit
source buffer when minor mode is turned off.

Note that the minor mode is buffer local, but this variable is global.
This is deliberte design choice. You may activate Org-transclusion for
multiple buffers at a time. But editing their sources should be focused,
and thus one edit buffer can be open at a time.

Killing a clone buffer is assumed to be safe in general, as its original
buffer is in sync and the content is reflected there.")

;;;; Customization variables
(defgroup org-transclusion nil
  "Insert text contents by way of link references."
  :group 'org
  :prefix "org-translusion-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-translusion"))

(defcustom org-transclusion-auto-add-on-activation t
  "Define whether or not add all the transclusion contents on activation.
If true, add text contents for all the transclusion links where possible.
Default to true."
  :type 'boolean
  :group 'org-transclusion)

(defvar org-transclusion-link "otc")
(defvar org-transclusion-activate-persistent-message t)

;; ort-translusion-add-at-point-functions is a list of
;; "link types" org-tranclusion supports.
;; In addtion to a element in the list, there must be two corresponding
;; functions with specific names
;; 
;; The functions must conform to take specific arguments, and to returnbvalues.
;; 
;; org-transclusion-match-<org-id>
;; org-transclusion-add-<org-id>
;;
;; See the functions delivered within org-tranclusion for the API signatures.

(defvar org-transclusion-add-org-link-at-point-functions
  (list "org-link-headline"))

(defvar org-transclusion-add-at-point-functions
  (list "org-id" "org-headline" "paragraph-org-dedicated-target"))

;;-----------------------------------------------------------------------------
;; Faces
;; (WIP)

(defface org-transclusion-source-block
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#fff3da" :extend t))
  "Face for transcluded block.")

(defface org-transclusion-block
  '((((class color) (min-colors 88) (background light))
     :background "#f3f3ff" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#f3f3ff" :extend t))
  "Face for transcluded block.")

;;-----------------------------------------------------------------------------
;; Custom link parameter
;; :follow fn should be a one that can do-list for functions, where
;; each function support different type of links: e.g. file, ID, etc.

;; (org-link-set-parameters org-transclusion-link
;;  :follow #'org-transclusion-call-add-at-point-functions)

;;-----------------------------------------------------------------------------
;; Functions to override org-link-open
;; Support different link types

(defun org-transclusion-link-open (oldfn &optional link arg)
  "Override Org Mode's default `org-link-open' for LINK.
Meant to be used with add-advice/remove-advice in activate/deactivate.

If the link type is not supported by org-transclusion, or \\[universal-argument]
is used (ARG is non-nil), then use `org-link-open'."
  (save-window-excursion
    (funcall oldfn link)
    (let* ((el (org-element-context))
           (disp (buffer-substring
                  (org-element-property :begin el)
                  (org-element-property :end el))))
      disp)))
  ;; (let ((tc-params nil))
  ;;   (if (or arg
  ;;           (not (setq tc-params (org-transclusion--org-link-tc-params-p link))))
  ;;       nil ;; Call the original `org-link-open'
  ;;     (org-transclusion--create-at-point tc-params)
  ;;     t))) ;; return t so that advice-add :before-until won't call the orignal fn

;; (defun org-transclusion-match-org-link-headline (path)
;;   "Return t if PATH if for the link type to be transcluded.
;; org-transclusion-add-<tc-type> function needs to be also defined."
  
;;   (let ((type (plist-get path ':type))
;;         (search (plist-get path ':search-option)))
;;     (and (string= type "file")
;;          (string-prefix-p "*" search))))

;; (defun org-transclusion-add-org-link-headline (path)
;;   "Return the text content of the subtree of an Org headline for PATH.
;; PATH is assumed to be of the form: file:path/to/file.org::*headline."
;;   (let ((file-path (org-element-property :path path))
;;         (headline (org-element-property :search-option path)))
;;     (when-let ((buf (find-file-noselect file-path)))
;;       (with-current-buffer buf
;;         (org-with-wide-buffer
;;          (org-link-search headline)
;;          (org-narrow-to-subtree)
;;          (let ((content (buffer-string))
;;                (beg (point-min-marker))
;;                (end (point-max-marker)))
;;            (list :tc-content content
;;                  :tc-beg-mkr beg
;;                  :tc-end-mkr end)))))))
  
;;-----------------------------------------------------------------------------
;; Functions to support different link types

;; (defun org-transclusion-match-paragraph-org-dedicated-target (path)
;;   "Return t if PATH if for the link type to be transcluded.
;; org-transclusion-add-<tc-type> function needs to be also defined.

;; Note that the regex is broader than necessary.  It also matches
;; ::*headline form as well as ::dedicated-link.
;; It is assumed that match for org-headline has been done before
;; this match function."
  
;;   (and (string-prefix-p "file:" path)
;;        (when (string-match "\\(.org::\\)\\(\\.*\\)" path) t)))

;; (defun org-transclusion-add-paragraph-org-dedicated-target (path)
;;   "Return the text content of a paragraph in an Org file.

;; The target paragraph must be identifiable by a dedicated link with
;; a <<paragraph-id>>: e.g.

;;    Lorem ipsum dolor sit amet, consectetur adipiscing elit.
;;    Suspendisse ac velit fermentum, sodales nunc in,
;;    tincidunt quam.  <<paragraph-id>>

;; It is generally assumed that the paragraph-id is placed after its content,
;; but it is not an absolute requirement; it can be in the beginning (before
;; the content) or in the middle of it.  It uses `org-link-search' to locate
;; the <<dedicated-link>> and use `mark-paragraph' to select the content.

;; PATH is assumed to be of the form: file:path/to/file.org::dedicted-link."
;;   (let ((file-path nil)
;;         (paragraph-id nil))
;;     (string-match "\\(file:\\)\\(.*\\)::\\(.*\\)" path)
;;     (setq file-path (match-string-no-properties 2 path))
;;     (setq paragraph-id (match-string-no-properties 3 path))
;;     (when-let ((buf (find-file-noselect file-path)))
;;       (with-current-buffer buf
;;         (org-with-wide-buffer
;;          (org-link-search paragraph-id)
;;          (mark-paragraph)
;;          (let* ((beg (point-marker)) ;; it is the beginning of the paragraph
;;                 (end (mark-marker))
;;                 (content (buffer-substring beg end)))
;;            (deactivate-mark)
;;            (list :tc-content content
;;                  :tc-beg-mkr beg
;;                  :tc-end-mkr end)))))))

;; (defun org-transclusion-match-org-headline (path)
;;   "Return t if PATH if for the link type to be transcluded.
;; org-transclusion-add-<tc-type> function needs to be also defined."
;;   (and (string-prefix-p "file:" path)
;;        (when (string-match "\\(.org::\\)\\(\\*.*\\)" path) t)))

;; (defun org-transclusion-add-org-headline (path)
;;   "Return the text content of the subtree of an Org headline for PATH.
;; PATH is assumed to be of the form: file:path/to/file.org::*headline."
;;   (let ((file-path nil)
;;         (headline nil))
;;     (string-match "\\(file:\\)\\(.*\\)::\\(*.*\\)" path)
;;     (setq file-path (match-string-no-properties 2 path))
;;     (setq headline (match-string-no-properties 3 path))
;;     (when-let ((buf (find-file-noselect file-path)))
;;       (with-current-buffer buf
;;         (org-with-wide-buffer
;;          (org-link-search headline)
;;          (org-narrow-to-subtree)
;;          (let ((content (buffer-string))
;;                (beg (point-min-marker))
;;                (end (point-max-marker)))
;;            (list :tc-content content
;;                  :tc-beg-mkr beg
;;                  :tc-end-mkr end)))))))
  
;; (defun org-transclusion-match-org-id (path)
;;   "Return t if PATH if for the link type to be transcluded.
;; org-transclusion-add-<tc-type> function needs to be also defined."
;;   (string-prefix-p "id:" path))

;; (defun org-transclusion-add-org-id (path)
;;   "Return the text content of the subtree identified by Org-ID.
;; PATH is assumed to be in form `id:uuid'."
;;   (let* ((id (progn
;;                (string-match "\\(id:\\)\\([[:alnum:]|-]*\\)" path)
;;                (match-string 2 path)))
;;          (marker (org-id-find id 'marker))
;;          (buf (marker-buffer marker)))
;;     (with-current-buffer buf
;;       (org-with-wide-buffer
;;        (goto-char marker)
;;        (org-narrow-to-subtree)
;;        (let ((content (buffer-string))
;;              (beg (point-min-marker))
;;              (end (point-max-marker)))
;;          (list :tc-content content
;;                :tc-beg-mkr beg
;;                :tc-end-mkr end))))))

;;-----------------------------------------------------------------------------
;; Core Functions
;; - Core operations: create-, save-, remove-, detach-at-point
;; - Supporting functions for these core operations

;; (defun org-transclusion--org-link-tc-params-p (link)
;;   "Return PARAMS with TC-FN if link type is supported for LINK.
;; Link type for this function is the standard Org Mode ones."
;;   (let ((types org-transclusion-add-org-link-at-point-functions)
;;         (params nil))
;;     (while (and (not params)
;;                 types)
;;       (let* ((type (pop types))
;;              (match-fn
;;               (progn (intern (concat "org-transclusion-match-" type))))
;;              (add-fn
;;               (progn (intern (concat "org-transclusion-add-" type))))
;;              (path (plist-get link ':path )))
;;         (when (and (functionp match-fn)
;;                    (funcall match-fn link)
;;                    (functionp add-fn))
;;           (setq params (list :tc-type type :tc-fn add-fn :tc-path path)))))
;;     params))

;; (defun org-transclusion--custom-tc-params-p (str)
;;   "Return PARAMS with TC-FN if link type is supported for STR."
;;   (let ((types org-transclusion-add-at-point-functions)
;;         (params nil))
;;     (while (and (not params)
;;                 types)
;;       (let* ((type (pop types))
;;              (match-fn
;;               (progn (intern (concat "org-transclusion-match-" type))))
;;              (add-fn
;;               (progn (intern (concat "org-transclusion-add-" type)))))
;;         (when (and (functionp match-fn)
;;                    (funcall match-fn str)
;;                    (functionp add-fn))
;;           (setq params (list :tc-type type :tc-fn add-fn :tc-path str)))))
;;     params))
  
;; (defun org-transclusion--get-tc-params (str)
;;   "Return TC-TYPE, TC-FN and TC-PATH by parsing STR.
;; Fallback to default if not supported.

;; Call the matcher function corresponding to each link type in
;; `org-transclusion-add-at-point-functions` from left to right.

;; Use the first match (thus, the left has more priority), and then
;; return its corresponding TC-FN function, TC-TYPE, and TC-PATH.
;; If none of the matchers finds a match, use default."

;;   (let ((params nil))
;;     (setq params (org-transclusion--org-link-tc-params-p str))
;;     (when (not params)
;;       (setq params (org-transclusion--custom-tc-params-p str)))
;;     (when (not params) ;fallback to default
;;       (setq params (list :tc-type "default"
;;                          :tc-fn #'org-transclusion-add-default
;;                          :tc-path str)))
;;     params))

;; (defun org-transclusion-add-default (path)
;;   "Use PATH to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.

;; TODO need to handle when the file does not exist."

;;   (let ((buf (find-file-noselect path)))
;;     (with-current-buffer buf
;;         (org-with-wide-buffer
;;          (let ((content (buffer-string))
;;                (beg (point-min-marker))
;;                (end (point-max-marker)))
;;            (list :tc-content content
;;                  :tc-beg-mkr beg
;;                  :tc-end-mkr end))))))

;; (defun org-transclusion-call-add-at-point-functions (str &rest _prefix)
;;   "Call functions to insclude source text for PATH in current buffer.
;; It is meant to be used as a :follow function in the custom Org Mode link type.

;; It is parametarized to make it easy to add support for additional link types.
;; Refer to `org-transclusion-add-org-id' as a sample implementation.

;; The FN must:
;;     1. Take arguments: (str) or (str &optional prefix)
;;     2. Return nil or TC-PARAMS

;; TC-PARAMS is a plist with the following params:
;;  :tc-content  := text string to be trancluded
;;  :tc-beg-mkr  := marker pointing to the beginning of the content in the src buf
;;  :tc-end-mkr  := marker pointing to the end of the content in the src buf

;; FN should decode STR as a link and determine the TC-TYPE.
;; eg. id:uuid-1234-xxxx, for Org-ID.
;; Return the content TC-CONTENT and markers TC-BG-MKR and TC-END-MKR.

;; Default to deal with link otc:./path/to/file.txt

;; A conditon check to avoid recursion happens in this function.

;; TODO You need to check if the link is at the bottom of buffer."
  
;;   (if (cdr (get-char-property-and-overlay (point) 'tc-type)) nil
;;          ;; The link is within a transclusion overlay.
;;          ;; Do nothing to avoid recurrsive transclusion.
;;     (let* ((tc-params (org-transclusion--get-tc-params str)))
;;       (org-transclusion--create-at-point tc-params))))

;; (defun org-transclusion--create-at-point (tc-params)
;;   "Create transclusion by unpackng TC-PARAMS."

;;   (when-let ((link-loc (org-transclusion--get-link-location))
;;              (link-beg (plist-get link-loc ':begin))
;;              (link-end (plist-get link-loc ':end))
;;              (raw-link (buffer-substring-no-properties link-beg link-end)))
;;     ;; Remove the link
;;     ;;(remove-text-properties link-beg link-end '(invisible 'org-link))
    
;;     ;;(add-text-properties link-beg link-end '(comment t face highlight invisible t))
;;     ;;(add-text-properties link-beg link-end '(invisible t))
;;     (delete-region link-beg link-end)
;;     ;; Delete a char after the link has been removed to remove the line
;;     ;; the link used to occupy. Without this, you end up moving one line
;;     ;; every time add operation is called.
;;     (delete-char 1)
;;     ;; TODO You need to check if the link is at the bottom of buffer
;;     ;; If it is, then yank won't work.

;;     ;; Add content and overlay
;;     (let* ((tc-raw-link raw-link)
;;            (tc-type (plist-get tc-params :tc-type))
;;            (tc-fn (plist-get tc-params :tc-fn))
;;            (tc-path (plist-get tc-params :tc-path))
;;            (tc-payload (funcall tc-fn tc-path))
;;            (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
;;            (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
;;            (tc-content (plist-get tc-payload :tc-content)))
;;       (save-excursion
;;         ;;(move-beginning-of-line nil)
;;         (insert tc-content))
;;       (when-let
;;           ((dups (org-transclusion--text-clone-create tc-beg-mkr tc-end-mkr))
;;            (ov (car (cdr dups)))
;;            (ov-src (car dups)))
;;         ;; Put to target overlay
;;         (overlay-put ov 'tc-type tc-type)
;;         (overlay-put ov 'tc-raw-link tc-raw-link)
;;         (overlay-put ov 'tc-beg-mkr tc-beg-mkr)
;;         (overlay-put ov 'tc-end-mkr tc-end-mkr)
;;         (overlay-put ov 'priority -50)
;;         ;;(overlay-put ov 'display tc-content)
;;         ;; Put to the source overlay
;;         (save-excursion
;;           (goto-char (overlay-start ov))
;;           (overlay-put ov-src 'tc-by (point-marker)))))))


(defun org-transclusion--get-org-content-from-link (open-link-fn link)
  "Return tc-beg-mkr, tc-end-mkr, tc-content from LINK using open-link-fn."
  (save-window-excursion
    (funcall open-link-fn link)
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

(defun org-transclusion--add-from-link (open-link-fn link &rest arg)
  "New and simple."
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
           (tc-type "org-link") ; change this
           (tc-path nil)
           (tc-payload (org-transclusion--get-org-content-from-link open-link-fn link))
           (tc-beg-mkr (plist-get tc-payload :tc-beg-mkr))
           (tc-end-mkr (plist-get tc-payload :tc-end-mkr))
           (tc-content (plist-get tc-payload :tc-content)))
      (save-excursion
        ;;(org-paste-subtree 2 tc-content nil))
        (insert tc-content))
      (when-let
          ((dups (org-transclusion--text-clone-create tc-beg-mkr tc-end-mkr))
           (ov (car (cdr dups)))
           (ov-src (car dups)))
        ;; Put to target overlay
        (overlay-put ov 'tc-type tc-type)
        (overlay-put ov 'tc-raw-link tc-raw-link)
        (overlay-put ov 'tc-beg-mkr tc-beg-mkr)
        (overlay-put ov 'tc-end-mkr tc-end-mkr)
        (overlay-put ov 'priority -50)
        ;;(overlay-put ov 'display tc-content)
        ;; Put to the source overlay
        (save-excursion
          (goto-char (overlay-start ov))
          (overlay-put ov-src 'tc-by (point-marker)))))))

(defun org-transclusion-save-src-at-point (pos)
  "Save the transclusion source buffer with the tranclusion at POS.
It can be used interactively.

It will create a backup file if this is the first backup for
the source buffer in this session."
  
  (interactive "d")
  ;; Check POS has an tranclusion overlay
  ;; If not, get out the function with a message telling the user.
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (let ((src_buf (marker-buffer (overlay-get ov 'tc-beg-mkr))))
        (with-current-buffer src_buf
          (unless make-backup-files (setq-local make-backup-files t))
          (save-buffer)))
    (message "Nothing done. No transclusion exists here.")))

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
          (let* ((beg (overlay-start ov))
                 (raw-link (overlay-get ov 'tc-raw-link))
                 (new-beg (progn
                            (goto-char beg)
                            (newline)
                            (forward-line -1)
                            (insert raw-link)
                            (forward-line)
                            (point)))
                 (new-end (overlay-end ov))
                 (dups (overlay-get ov 'text-clones)))
            (dolist (ol dups)
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

(defun org-transclusion-open-edit-buffer-at-point (pos)
  (interactive "d")
  (if-let ((ov (cdr (get-char-property-and-overlay pos 'tc-type))))
      (let ((from-mkr (point-marker))
            (to-mkr (overlay-get ov 'tc-beg-mkr)))
        (with-current-buffer (marker-buffer to-mkr)
          (org-with-wide-buffer
           (setq org-transclusion-edit-src-at-mkr from-mkr)
           (goto-char to-mkr)
           (org-narrow-to-subtree)
           (org-tree-to-indirect-buffer))
          ;; Only one edit buffer globally at a time
          (when (buffer-live-p org-transclusion-last-edit-src-buffer)    
            (kill-buffer org-transclusion-last-edit-src-buffer))
          (setq org-transclusion-last-edit-src-buffer org-last-indirect-buffer)
          (pop-to-buffer org-transclusion-last-edit-src-buffer)
          (rename-buffer (concat "*" (buffer-name) "*"))
          (org-transclusion-edit-src-mode)))
    ;; The message below is common for remove and detach
    (message "Nothing done. No transclusion exists here.")))

;;-----------------------------------------------------------------------------
;; Utility functions used in the core functions above

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

(defun org-transclusion--transclusion-org-link-p ()
  "Check if the link at point is a tranclusion link."

  (when-let ((link (plist-get (org-element-context) 'link)))
    (org-transclusion--org-link-tc-params-p link)))

(defun org-transclusion--transclusion-link-p ()
  "Check if the link at point is a tranclusion link."

  (when-let ((link (plist-get (org-element-context) 'link)))
    (let ((type (plist-get link ':type)))
      (string= type org-transclusion-link))))
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
  (org-transclusion-save-all-src-in-buffer) ; save to file
  (goto-char org-transclusion-original-position)
  (setq org-transclusion-original-position nil)
  (set-buffer-modified-p nil))

(defun org-transclusion-save-all-src-in-buffer ()
  "Save all transclusion sources from the current buffer."

  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (outline-show-all)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (org-transclusion-save-src-at-point (overlay-start ov))))))

(defun org-transclusion-add-all-in-buffer ()
  "Add all the transclusions in the current buffer.
As this should be used only when the buffer is current, no argment passed.

As transclusing adds text after the link, the loop needs to process from top to
bottom one by one.  The transcluded text may contrain transclusion link.

Check is done within `org-transclusion-call-add-at-point-functions'
to avoid recursion."
  
  (interactive)
  ;; Check the windows being worked on is in focus (selected)
  ;; This is to prevent background hook (e.g. save hooks) from updating
  ;; the transclusion buffer.
  (when (eq (current-buffer)(window-buffer (selected-window)))
    (setq org-transclusion-buffer-modified-p (buffer-modified-p))
    (save-excursion
      (save-restriction
        (widen)
        (outline-show-all)
        (goto-char (point-min))
        ;; For `org-next-link', eq t is needed for this while loop to check no
        ;; link.  This is because fn returns a message string when there is no
        ;; further link.
        (while (eq t (org-next-link))
          ;; Check if the link at point is tranclusion link
          ;; Check if the link is in the beginning of a line
          (when (eq (line-beginning-position)(point))
            (org-open-at-point)))))
    (set-buffer-modified-p org-transclusion-buffer-modified-p)))
            ;; (when (org-transclusion--transclusion-org-link-p)
            ;;   ;; the tc link is for standard org mode ones
            ;;   (let* ((link (org-element-link-parser))
            ;;          (raw-link (org-element-property :raw-link link)))
            ;;     (org-transclusion-call-add-at-point-functions raw-link)))
            ;; (when (org-transclusion--transclusion-link-p)
            ;;   ;; the tc link is for otc: custom ones
            ;;   (let* ((link (org-element-link-parser))
            ;;          (path (org-element-property :path link)))
            ;;     (org-transclusion-call-add-at-point-functions path)))))))))

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
    ;;(advice-add 'org-link-open :before-until #'org-transclusion-link-open)
    (advice-add 'org-link-open :around #'org-transclusion--add-from-link)
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
        ;;(advice-remove 'org-link-open #'org-transclusion-link-open)
        (advice-remove 'org-link-open #'org-transclusion--add-from-link)
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
           (org-transclusion-remove-all-in-buffer buf))))) ;; remove all

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
;; Text Clone
;; Based on StackExchange user Tobias' code; adapted by nobiot
;; https://emacs.stackexchange.com/questions/56201/is-there-an-emacs-package-which-can-mirror-a-region/56202#56202
;; Since I'm not using SPREADP argument (or margin), I can simplify
;; the code much more.
;; Not sure if I would like to keep regex (TEXT-CLONE-SYNTAX)
;; I think this should be handled with the add functions above.
;; that is, leaning towards removing.

(defun org-transclusion--text-clone-create (start end &optional spreadp syntax)
  "Create a text clone of START...END at point.
Text clones are chunks of text that are automatically kept identical:
changes done to one of the clones will be immediately propagated to the other.

  The buffer's content at point is assumed to be already identical to
  the one between START and END.
  If SYNTAX is provided it's a regexp that describes the possible text of
  the clones; the clone will be shrunk or killed if necessary to ensure that
  its text matches the regexp.
  If SPREADP is non-nil it indicates that text inserted before/after the
  clone should be incorporated in the clone."
  ;; To deal with SPREADP we can either use an overlay with `nil t' along
  ;; with insert-(behind|in-front-of)-hooks or use a slightly larger overlay
  ;; (with a one-char margin at each end) with `t nil'.
  ;; We opted for a larger overlay because it behaves better in the case
  ;; where the clone is reduced to the empty string (we want the overlay to
  ;; stay when the clone's content is the empty string and we want to use
  ;; `evaporate' to make sure those overlays get deleted when needed).
  ;;
  (let* ((clone-buf (or (and (markerp start) (marker-buffer start))
                        (current-buffer)))
         (pt-end (+ (point) (- end start)))
         (start-margin (if (or (not spreadp) (bobp) (with-current-buffer clone-buf (<= start (point-min))))
                           0 1))
         (end-margin (if (or (not spreadp)
                             (>= pt-end (point-max))
                             (with-current-buffer clone-buf (>= start (point-max))))
                         0 1))
         ;; FIXME: Reuse overlays at point to extend dups!
         (ol1 (make-overlay (- start start-margin) (+ end end-margin) clone-buf t nil)) ;;< Tobia ;; added rea-advance

         (ol2 (make-overlay (- (point) start-margin) (+ pt-end end-margin) nil t nil))
         (dups (list ol1 ol2)))
;;    (overlay-put ol1 'modification-hooks '(org-transclusion--text-clone--maintain)) ;;< nobiot
    (when spreadp (overlay-put ol1 'text-clone-spreadp t))
    (when syntax (overlay-put ol1 'text-clone-syntax syntax))
    ;;(overlay-put ol1 'face 'underline)
    (overlay-put ol1 'evaporate t)
    (overlay-put ol1 'face 'org-transclusion-source-block) ;; < nobiot
    (overlay-put ol1 'text-clones dups)
    ;;
;;    (overlay-put ol2 'modification-hooks '(org-transclusion--text-clone--maintain)) ;;< Tobias
    (when spreadp (overlay-put ol2 'text-clone-spreadp t))
    (when syntax (overlay-put ol2 'text-clone-syntax syntax))
    ;;(overlay-put ol2 'face 'underline)
    (overlay-put ol2 'evaporate t)
    (overlay-put ol2 'face 'org-transclusion-block) ;; < nobiot
    (overlay-put ol2 'text-clones dups)
    (add-text-properties (overlay-start ol2) (overlay-end ol2) '(read-only t))
    dups)) ;; < nobiot return dups

(provide 'org-transclusion)
;;; org-transclusion.el ends here
