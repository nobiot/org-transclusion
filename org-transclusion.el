;;; org-transclusion.el --- Transclude text content via links -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024  Free Software Foundation, Inc.

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

;; Author:        Noboru Ota <me@nobiot.com>
;; Created:       10 October 2020
;; Last modified: 01 January 2025

;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing

;; Version: 1.4.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library is an attempt to enable transclusion with Org Mode.
;; Transclusion is the ability to include content from one file into
;; another by reference.

;;; Code:

;;;; Requirements
(require 'org)
(require 'org-element)
(require 'org-id)
(require 'text-clone)
(require 'text-property-search)
(require 'seq)

;;;; Customization

(defgroup org-transclusion nil
  "Insert text contents by way of link references."
  :group 'org
  :prefix "org-transclusion-"
  :link '(url-link :tag "Github" "https://github.com/nobiot/org-transclusion")
  :package-version '("Org-transclusion" . "1.0.0"))

(defun org-transclusion-set-extensions (var value)
  "Set VAR to VALUE and `org-transclusion-load-extensions-maybe'.
Intended for :set property for `customize'."
  (set var value)
  (when (featurep 'org-transclusion)
    (org-transclusion-load-extensions-maybe 'force)))

(defcustom org-transclusion-extensions
  '(org-transclusion-src-lines org-transclusion-font-lock)
  "Extensions to be loaded with org-transclusion.el."
  :set #'org-transclusion-set-extensions
  :type
  '(set :greedy t
        (const :tag "src-lines: Add :src and :lines for non-Org files"
               org-transclusion-src-lines)
        (const :tag "font-lock: Add font-lock for Org-transclusion"
               org-transclusion-font-lock)
        (const :tag "indent-mode: Support org-indent-mode"
               org-transclusion-indent-mode)
        (const :tag "html: Transclude HTML converted to Org with Pandoc"
               org-transclusion-html)
        (repeat :tag "Other packages" :inline t (symbol :tag "Package"))))

(defcustom org-transclusion-add-all-on-activate t
  "Define whether to add all the transclusions on activation.
When non-nil, automatically add all on `org-transclusion-activate'."
  :type 'boolean)

(defcustom org-transclusion-exclude-elements (list 'property-drawer)
  "Define the Org elements that are excluded from transcluded copies.
It is a list of elements to be filtered out.
Refer to variable `org-element-all-elements' for names of elements accepted."
  :type '(repeat symbol))

(defcustom org-transclusion-include-first-section t
  "Define whether or not transclusion for Org files includes \"first section\".
If t, the section before the first headline is
transcluded. Default is t."
  :type 'boolean)

(defcustom org-transclusion-open-source-display-action-list '(nil . nil)
  "Action list used to open source buffer to display.

See `display-buffer' for example options."
  :type display-buffer--action-custom-type
  :risky t)

(defcustom org-transclusion-mode-lighter
  " OT"
  "Mode-line indicator for minor-mode variable `org-transclusion-mode'."
  :type '(choice (const :tag "No lighter" "") string)
  :safe #'stringp)

(defcustom org-transclusion-after-add-functions nil
  "Functions to be called after a transclusion content has been added.
The hook runs after the content and the read-only text property
have been added so it is not supposed to manipulate the content
but to add further text properties.  For example, it is used by
the `org-transclusion-indent-mode' extension to support
`org-indent-mode'.  The functions are called with arguments beg
and end, pointing to the beginning and end of the transcluded
content."
  :type '(repeat function))

;;;; Faces

(defface org-transclusion-source-fringe
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark)))
    (t ))
  "Face for source region's fringe being transcluded in another buffer.")
 (defface org-transclusion-source
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark)))
    (t ))
  "Face for source region being transcluded in another buffer.
The default is no color specification (transparent).")
 (defface org-transclusion-source-edit
  '((((class color) (min-colors 88) (background light))
     :background "#fff3da" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#221000" :extend t)
    (t
     :background "chocolate4" :extend t))
  "Face for element in the source being edited by another buffer.")
 (defface org-transclusion-fringe
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark)))
    (t ))
  "Face for transcluded region's fringe in the transcluding buffer.")
 (defface org-transclusion
  '((((class color) (min-colors 88) (background light)))
    (((class color) (min-colors 88) (background dark)))
    (t ))
  "Face for transcluded region in the transcluding buffer.
The default is no color specification (transparent).")
 (defface org-transclusion-edit
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t)
    (t
     :background "forest green" :extend t))
  "Face for element in the transcluding buffer in the edit mode.")

;;;; Variables

(defvar org-transclusion-extensions-loaded nil
  "Have the extensions been loaded already?")

(defvar-local org-transclusion-remember-point nil
  "This variable is used to remember the current just before `save-buffer'.
It is meant to be used to remember and return to the current
point after `before-save-hook' and `after-save-hook' pair;
`org-transclusion-before-save-buffer' and
`org-transclusion-after-save-buffer' use this variable.")

(defvar-local org-transclusion-remember-transclusions nil
  "Remember the active transclusions before `save-buffer'.
It is meant to be used to keep the file the current buffer is
visiting clear of the transcluded text content.  Instead of
blindly deactivate and activate all transclusions with t flag,
this variable is meant to provide mechanism to
deactivate/activate only the transclusions currently used to copy
a text content.

`org-transclusion-before-save-buffer' and
`org-transclusion-after-save-buffer' use this variable.")

(defvar-local org-transclusion-remember-window-config nil
  "Remember window config (the arrangement of windows) for the current buffer.
This is for live-sync.  Analogous to
`org-edit-src-code'.")

(defvar org-transclusion-add-functions
  '(org-transclusion-add-org-id
    org-transclusion-add-org-file
    org-transclusion-add-other-file)
  "Define a list of functions to get a payload for transclusion.
These function take two arguments: Org link and keyword plist,
and return a payload.  The payload is defined as a property list
that consists of the following properties:

- :tc-type
- :src-buf
- :src-beg
- :src-end
- :src-content

Otherwise, the payload may be a named or lambda function which
will be called with the following arguments:

- \\+`link'
- \\+`keyword-plist'
- \\+`copy'

In order for the transclusion to be inserted into the buffer, the
payload function should generate a payload plist, then call
`org-transclusion-add-payload', passing in the payload as well as
the \\+`link', \\+`keyword-plist', and \\+`copy' arguments.")

(defvar org-transclusion-keyword-value-functions
  '(org-transclusion-keyword-value-link
    org-transclusion-keyword-value-level
    org-transclusion-keyword-value-disable-auto
    org-transclusion-keyword-value-only-contents
    org-transclusion-keyword-value-exclude-elements
    org-transclusion-keyword-value-expand-links
    org-transclusion-keyword-current-indentation)
  "Define a list of functions used to parse a #+transclude keyword.
These functions take a single argument, the whole keyword value
as a string.  Each function retrieves a property with using a
regexp from the string.")

(defvar org-transclusion-keyword-plist-to-string-functions '())

(defvar org-transclusion-content-format-functions
  '(org-transclusion-content-format-org
    org-transclusion-content-format))

(defvar org-transclusion-open-source-marker-functions
  '(org-transclusion-open-source-marker))

(defvar org-transclusion-live-sync-buffers-functions
  '(org-transclusion-live-sync-buffers-org
    org-transclusion-live-sync-buffers-others-default))

(defvar org-transclusion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "e") #'org-transclusion-live-sync-start)
    (define-key map (kbd "g") #'org-transclusion-refresh)
    (define-key map (kbd "d") #'org-transclusion-remove)
    (define-key map (kbd "C-d") #'org-transclusion-detach)
    (define-key map (kbd "P") #'org-transclusion-promote-subtree)
    (define-key map (kbd "D") #'org-transclusion-demote-subtree)
    (define-key map (kbd "o") #'org-transclusion-open-source)
    (define-key map (kbd "O") #'org-transclusion-move-to-source)
    map)
  "It is the local-map used within a transclusion.
As the transcluded text content is read-only, these keybindings
are meant to be a sort of contextual menu to trigger different
functions on the transclusion.")

(defvar org-transclusion-live-sync-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-c") #'org-transclusion-live-sync-exit)
    (define-key map (kbd "C-y") #'org-transclusion-live-sync-paste)
    map)
  "It is the local-map used within the live-sync overlay.
It inherits `org-mode-map' and adds a couple of org-transclusion
specific keybindings; namely:

- `org-transclusion-live-sync-paste'
- `org-transclusion-live-sync-exit'")

(defvar org-transclusion-yank-excluded-properties
  '(org-transclusion-type org-transclusion-id org-transclusion-pair
    org-transclusion-orig-keyword wrap-prefix line-prefix
    :parent front-sticky rear-nonsticky))

(defvar org-transclusion-yank-remember-user-excluded-props '())

(define-fringe-bitmap 'org-transclusion-fringe-bitmap
  [#b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000
   #b11000000]
  nil nil '(center t))

;;;; Macro
;;;; Defining macros before they are used in the rest of package
;;;; Flycheck warns with "macro X defined too late"
(defmacro org-transclusion-with-inhibit-read-only (&rest body)
  "Run BODY with `'inhibit-read-only` t.
This macro is used instead of `with-silent-modifications' because
Org mode's caching relies upon modification hooks to function."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (inhibit-read-only t))
       (unwind-protect
           (progn
             ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

;;;; Commands

;;;###autoload
(define-minor-mode org-transclusion-mode
  "Toggle Org-transclusion minor mode."
  :init-value nil
  :lighter org-transclusion-mode-lighter
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if org-transclusion-mode
      (progn
        (org-transclusion-activate)
        (when org-transclusion-add-all-on-activate
          (org-transclusion-add-all)))
    (org-transclusion-deactivate)))

;;;###autoload
(defun org-transclusion-activate ()
  "Activate Org-transclusion hooks and other setups in the current buffer.
This function does not add transclusions; it merely sets up hooks
and variables."
  (interactive)
  (add-hook 'before-save-hook #'org-transclusion-before-save-buffer nil t)
  (add-hook 'after-save-hook #'org-transclusion-after-save-buffer nil t)
  (add-hook 'kill-buffer-hook #'org-transclusion-before-kill nil t)
  (add-hook 'kill-emacs-hook #'org-transclusion-before-kill nil t)
  (add-hook (if (version< org-version "9.6")
                'org-export-before-processing-hook
              'org-export-before-processing-functions)
            #'org-transclusion-inhibit-read-only nil t)
  (org-transclusion-yank-excluded-properties-set)
  (org-transclusion-load-extensions-maybe))

(defun org-transclusion-deactivate ()
  "Deactivate Org-transclusion hooks and other setups in the current buffer.
This function also removes all the transclusions in the current buffer."
  (interactive)
  (org-transclusion-remove-all)
  (remove-hook 'before-save-hook #'org-transclusion-before-save-buffer t)
  (remove-hook 'after-save-hook #'org-transclusion-after-save-buffer t)
  (remove-hook 'kill-buffer-hook #'org-transclusion-before-kill t)
  (remove-hook 'kill-emacs-hook #'org-transclusion-before-kill t)
  (remove-hook (if (version< org-version "9.6")
                   'org-export-before-processing-hook
                 'org-export-before-processing-functions)
               #'org-transclusion-inhibit-read-only t)
  (org-transclusion-yank-excluded-properties-remove))

;;;###autoload
(defun org-transclusion-make-from-link (&optional arg)
  "Make a transclusion keyword from a link at point.

The resultant transclusion keyword will be placed in the first
next empty line.  If there is no empty line until the bottom of
the buffer, this function adds a new empty line.

When minor-mode `org-transclusion-mode' is active, this function
automatically transcludes the text content; when it is inactive,
it simply adds the \"#+transclude\" keyword before the link and
inserts the whole line.

If you pass a `universal-argument', this function reverses this:
if the mode is active, the keyword gets inserted; if the mode is
inactive, the transclusion gets added.

You can pass a prefix argument (ARG) with using
`digit-argument' (e.g. C-1 or C-2; or \\[universal-argument] 3,
so on) or `universal-argument' (\\[universal-argument]).

If you pass a positive number 1-9 with `digit-argument', this
function automatically puts the :level property to the resultant
transclusion keyword."
  (interactive "P")
  (let* ((context (org-element-lineage
                   (org-element-context)'(link) t))
         (auto-transclude-p (if (or (not arg) (numberp arg))
                                org-transclusion-mode
                              ;; if `universal-argument' is passed,
                              ;; reverse nil/t when
                              (not org-transclusion-mode))))
    (let* ((contents-beg (org-element-property :contents-begin context))
           (contents-end (org-element-property :contents-end context))
           (contents (and contents-beg
                          (buffer-substring-no-properties contents-beg
                                                          contents-end)))
           (link (org-element-link-interpreter context contents)))
      (save-excursion
        (org-transclusion-search-or-add-next-empty-line)
        (insert (format "#+transclude: %s\n" link))
        (forward-line -1)
        (when (and (numberp arg)
                   (> arg 0)
                   (<= arg 9))
          (end-of-line)
          (insert (format " :level %d" arg)))
        (when auto-transclude-p (org-transclusion-add))))))

;;;###autoload
(defun org-transclusion-add (&optional copy)
  "Transclude text content for the #+transclude at point.
When minor-mode `org-transclusion-mode' is inactive in the
current buffer, this function toggles it on.

With using `universal-argument' (\\[universal-argument]) or
non-nil COPY argument, you can copy the transcluded content into
the buffer instead of transclusion.

Examples of acceptable formats are as below:

- \"#+transclude: [[file:path/file.org::search-option][desc]]:level n\"
- \"#+transclude: [[id:uuid]] :level n :only-contents\"

The file path or id in the transclude keyword value are
translated to the normal Org Mode link format such as
[[file:path/to/file.org::*Heading]] or [[id:uuid]] to copy a piece
of text from the link target.

TODO: id:uuid without brackets [[]] is a valid link within Org
Mode. This is not supported yet.

A transcluded text region is read-only. You can use a variety of
commands on the transcluded region at point. Refer to the
commands below. You can customize the keymap with
using `org-transclusion-map'.

For example, `org-transclusion-live-sync-start' lets you edit the
part of the text at point.  This edit mode is analogous to Occur
Edit for Occur Mode.

TODO: that for transclusions of Org elements/buffer, live-sync
does not support all the elements.

\\{org-transclusion-map}"
  (interactive "P")
  (when (progn (org-transclusion-fix-common-misspelling)
               (org-transclusion-check-add))
    ;; Turn on the minor mode to load extensions before staring to add.
    (unless org-transclusion-mode
      (let ((org-transclusion-add-all-on-activate nil))
        (org-transclusion-mode +1)))
    (let* ((keyword-plist (org-transclusion-keyword-string-to-plist))
           (link (org-transclusion-wrap-path-to-link
                  (plist-get keyword-plist :link)))
           (payload (run-hook-with-args-until-success
                     'org-transclusion-add-functions link keyword-plist)))
      (if (functionp payload)
          ;; Allow for asynchronous transclusion
          (funcall payload link keyword-plist copy)
        (org-transclusion-add-payload payload link keyword-plist copy)))))

(defun org-transclusion-add-payload (payload link keyword-plist copy)
  "Insert transcluded content with error handling.

PAYLOAD should be a plist according to the description in
`org-transclusion-add-functions'.  LINK should be an org-element
context object for the link.  KEYWORD-PLIST should contain the
\"#+transclude:\" keywords for the transclusion at point.  With
non-nil COPY, copy the transcluded content into the buffer.

This function is intended to be called from within
`org-transclusion-add' as well as payload functions returned by
hooks in `org-transclusion-add-functions'."
  (let ((tc-type (plist-get payload :tc-type))
        (src-buf (plist-get payload :src-buf))
        (src-beg (plist-get payload :src-beg))
        (src-end (plist-get payload :src-end))
        (src-content (plist-get payload :src-content)))
    (if (or (string= src-content "")
            (eq src-content nil))
        ;; Keep going with program when no content `org-transclusion-add-all'
        ;; should move to the next transclusion
        (prog1 nil
          (message
           "No content found with \"%s\".  Check the link at point %d, line %d"
           (org-element-property :raw-link link) (point) (org-current-line)))
      (let ((beg (line-beginning-position))
            (end))
        (org-transclusion-with-inhibit-read-only
          (when (save-excursion
                  (end-of-line) (insert-char ?\n)
                  (org-transclusion-content-insert
                   keyword-plist tc-type src-content
                   src-buf src-beg src-end copy)
                  (unless (eobp) (delete-char 1))
                  (setq end (point))
                  t)
            ;; `org-transclusion-keyword-remove' checks element at point is a
            ;; keyword or not
            (org-transclusion-keyword-remove)))
        (run-hook-with-args 'org-transclusion-after-add-functions beg end))
      t)))

;;;###autoload
(defun org-transclusion-add-all (&optional narrowed)
  "Add all active transclusions in the current buffer.

By default, this function temporarily widens the narrowed region
you are in and works on the entire buffer.  Note that this
behavior is important for `org-transclusion-after-save-buffer' in
order to clear the underlying file of all the transcluded text.

For interactive use, you can pass NARROWED with using
`universal-argument' (\\[universal-argument]) to get this
function to work only on the narrowed region you are in, leaving
the rest of the buffer unchanged."
  (interactive "P")
  (save-restriction
    (let ((marker (point-marker)))
      (unless narrowed (widen))
      (goto-char (point-min))
      (let ((regexp "^[ \t]*#\\+TRANSCLUDE:"))
        (while (re-search-forward regexp nil t)
          ;; Don't transclude if within a transclusion to avoid infinite
          ;; recursion
          (unless (or (org-transclusion-within-transclusion-p)
                      (plist-get (org-transclusion-keyword-string-to-plist)
                                 :disable-auto))
            ;; Demoted-errors so that one error does not stop the whole process
            (with-demoted-errors
                "Not transcluded. Continue to next: %S"
              (when (org-transclusion-add)
                (message "Transcluded at point %d, line %d"
                         (point) (org-current-line)))))))
      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      t)))

(defun org-transclusion-remove ()
  "Remove transcluded text at point.
When success, return the beginning point of the keyword re-inserted."
  (interactive)
  (if-let*
      ((beg-end (plist-get (org-transclusion-at-point) :location))
       (beg (car beg-end))
       (end (cdr beg-end))
       (keyword-plist (get-char-property (point)
                                         'org-transclusion-orig-keyword))
       (indent (plist-get keyword-plist :current-indentation))
       (keyword (org-transclusion-keyword-plist-to-string keyword-plist))
       (tc-pair-ov (get-char-property (point) 'org-transclusion-pair)))
      (prog1
          beg
        (when (org-transclusion-within-live-sync-p)
          (org-transclusion-live-sync-exit))
        (delete-overlay tc-pair-ov)
        (org-transclusion-with-inhibit-read-only
          (save-excursion
            (delete-region beg end)
            (when (> indent 0) (indent-to indent))
            (insert-before-markers keyword)))
        (goto-char beg))
    (message "Nothing done. No transclusion exists here.") nil))

(defun org-transclusion-detach ()
  "Make the transcluded region normal copied text content."
  (interactive)
  ;; Make sure the transclusion is removed first so that undo can be used
  ;; to go back to the #+transclusion before detach.
  (org-transclusion-refresh 'detach))

(defun org-transclusion-remove-all (&optional narrowed)
  "Remove all transcluded text regions in the current buffer.
Return the list of points for the transclusion keywords
re-inserted.  It is assumed that the list is ordered in
descending order from the bottom of the buffer to the top.  The
list is intended to be used in
`org-transclusion-before-save-buffer'.

By default, this function temporarily widens the narrowed region
you are in and works on the entire buffer.  Note that this
behavior is important for `org-transclusion-before-save-buffer'
and `org-transclusion-before-kill' to clear the underlying file
of all the transcluded text.

For interactive use, you can pass NARROWED with using
`universal-argument' (\\[universal-argument]) to get this
function to work only on the narrowed region you are in, leaving
the rest of the buffer unchanged."
  (interactive "P")
  (save-restriction
    (let ((current-marker (move-marker (make-marker) (point)))
          match list)
      (unless narrowed (widen))
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'org-transclusion-id))
        (goto-char (prop-match-beginning match))
        (push (org-transclusion-remove) list))
      (goto-char current-marker)
      (move-marker current-marker nil) ; point nowhere for GC
      list)))

(defun org-transclusion-refresh (&optional detach)
  "Refresh the transcluded text at point.
With using `universal-argument' (\\[universal-argument]), you can
pass DETACH, which copies the source instead of transclusion.

TODO: Support asynchronous transclusions (set point correctly)."
  (interactive "P")
  (when (org-transclusion-within-transclusion-p)
    (let ((pos (point)))
      (org-transclusion-remove)
      (org-transclusion-add detach)
      (goto-char pos))
    t))

(defun org-transclusion-promote-subtree ()
  "Promote transcluded subtree at point."
  (interactive)
  (org-transclusion-promote-or-demote-subtree))

(defun org-transclusion-demote-subtree ()
  "Demote transcluded subtree at point."
  (interactive)
  (org-transclusion-promote-or-demote-subtree 'demote))

(defun org-transclusion-open-source (&optional arg)
  "Open the source buffer of transclusion at point.
When ARG is non-nil (e.g. \\[universal-argument]), the point will
remain in the source buffer for further editing.

TODO: Support asynchronous transclusions when source buffer
doesn't exist."
  (interactive "P")
  (unless (overlay-buffer (get-text-property (point) 'org-transclusion-pair))
    (org-transclusion-refresh))
  (let* ((type (get-text-property (point) 'org-transclusion-type))
         ;; Temporary marker to be discarded at the end of this function
         (src-mkr (run-hook-with-args-until-success
                   'org-transclusion-open-source-marker-functions type))
         (src-buf (marker-buffer src-mkr))
         (buf (current-buffer))
         (pos (point)))
    (if (not src-buf)
        (user-error
         (format "No paired source buffer found here: at %d" (point)))
      (unwind-protect
          (progn
            (when (display-buffer
                   src-buf
                   org-transclusion-open-source-display-action-list)
              (pop-to-buffer src-buf)
              (goto-char src-mkr)
              (recenter-top-bottom)))
        (unless arg
          (progn (pop-to-buffer buf)
                 (goto-char pos)))))
    (move-marker src-mkr nil))) ; point nowhere for GC

(defun org-transclusion-move-to-source ()
  "Open the source buffer and move point to it.
It's a function only to enable a keymap to call
`org-transclusion-open-source' with an argument."
  (interactive)
  (org-transclusion-open-source t))

(defun org-transclusion-live-sync-start ()
  "Start live-sync edit on the transclusion at point.

While live-sync is on, before- and after-save-hooks to remove/add
transclusions are also temporarily disabled.  This prevents
auto-save from getting in the way of live-sync.

For transclusions of Org elements or a buffer, live-sync works
only on the following elements: center-block, drawer,
dynamic-block, latex-environment, paragraph, plain-list,
quote-block, special-block table, and verse-block.

It is known that live-sync does not work for the other Org
elements: comment-block, export-block, example-block,
fixed-width, keyword, src-block, and property-drawer.

`org-transclusion-live-sync-map' inherits `org-mode-map' and adds
a couple of org-transclusion specific keybindings; namely:

- `org-transclusion-live-sync-paste'
- `org-transclusion-live-sync-exit'

\\{org-transclusion-live-sync-map}

TODO: Support asynchronous transclusions."
  (interactive)
  (if (not (org-transclusion-within-transclusion-p))
      (progn (message (format "Nothing done. Not a translusion at %d" (point)))
             nil)
    ;; Delete the other live-sync and refresh its transclusion
    ;; There should be only one pair of transclusion-source in live-sync
    (when-let* ((deleted-live-sync-ovs (text-clone-delete-overlays))
                (deleted-tc-ov (cadr deleted-live-sync-ovs)))
      (org-transclusion-live-sync-refresh-after-exit deleted-tc-ov))
    (org-transclusion-refresh)
    (let* ((remember-pos (point))
           (ovs (org-transclusion-live-sync-buffers))
           (src-ov (car ovs))
           (tc-ov (cdr ovs))
           (tc-beg (overlay-start tc-ov))
           (tc-end (overlay-end tc-ov)))
      ;; Check the length of both overlays
      ;; if different, abort live-sync
      (if (not (= (- (overlay-end tc-ov) (overlay-start tc-ov))
                  (- (overlay-end src-ov) (overlay-start src-ov))))
          (progn
            (user-error
             (concat
              "No live-sync can be started.  "
              "Lengths of transclusion and source are not identical"
              (format " - tc: [%s] src: [%s]"
                      (- (overlay-end tc-ov) (overlay-start tc-ov))
                      (- (overlay-end src-ov) (overlay-start src-ov)))))
            nil) ; return nil
        (org-transclusion-live-sync-modify-overlays
         (text-clone-set-overlays src-ov tc-ov))
        (org-transclusion-live-sync-display-buffer (overlay-buffer src-ov))
        (goto-char remember-pos)
        (remove-hook 'before-save-hook #'org-transclusion-before-save-buffer t)
        (remove-hook 'after-save-hook #'org-transclusion-after-save-buffer t)
        (with-silent-modifications
          (remove-text-properties (1- tc-beg) tc-end '(read-only)))
        t))))

(defun org-transclusion-live-sync-exit ()
  "Exit live-sync at point.
It attempts to re-arrange the windows for the current buffer to
the state before live-sync started."
  (interactive)
  (if (not (org-transclusion-within-live-sync-p))
      (user-error "Not within a transclusion in live-sync")
    (text-clone-delete-overlays)
    (let* ((src-ov (car (org-transclusion-live-sync-buffers)))
	   (src-buf (overlay-buffer src-ov)))
      (with-current-buffer src-buf
	(org-element-cache-reset)))
    ;; Re-activate hooks inactive during live-sync
    (org-transclusion-activate)
    (org-transclusion-refresh)
    (when org-transclusion-remember-window-config
      (unwind-protect
          (set-window-configuration org-transclusion-remember-window-config)
        (progn
          (setq org-transclusion-remember-window-config nil))))))

(defun org-transclusion-live-sync-paste ()
  "Paste text content from `kill-ring' and inherit the text props.
This is meant to be used within live-sync overlay as part of
`org-transclusion-live-sync-map'"
  (interactive)
  (insert-and-inherit (current-kill 0)))

;;;;---------------------------------------------------------------------------
;;;; Private Functions
;;;; Functions for Activate / Deactivate / save-buffer hooks

(defun org-transclusion-before-save-buffer ()
  "Remove transclusions in `before-save-hook'.
This function is meant to clear the file clear of the
transclusions.  It also remembers the current point for
`org-transclusion-after-save-buffer' to move it back."
  (setq org-transclusion-remember-point (point))
  (setq org-transclusion-remember-transclusions
        (org-transclusion-remove-all)))

(defun org-transclusion-after-save-buffer ()
  "Add transclusions back as they were `before-save-buffer'.
This function relies on `org-transclusion-remember-transclusions'
set in `before-save-hook'.  It also move the point back to
`org-transclusion-remember-point'."
  (unwind-protect
      (progn
        ;; Assume the list is in descending order.
        ;; pop and do from the bottom of buffer
        (dolist (p org-transclusion-remember-transclusions)
          (save-excursion
            (goto-char p)
            (org-transclusion-add)))
        ;; After save and adding all transclusions, the modified flag should
        ;; be set to nil.
        (restore-buffer-modified-p nil)
        (when org-transclusion-remember-point
          (goto-char org-transclusion-remember-point)))
    (progn
      (setq org-transclusion-remember-point nil)
      (setq org-transclusion-remember-transclusions nil))))

(defun org-transclusion-before-kill ()
  "Remove transclusions before `kill-buffer' or `kill-emacs'.
Intended to be used with `kill-buffer-hook' and `kill-emacs-hook'
to clear the file of the transcluded text regions.  This function
also flags the buffer modified and `save-buffer'.  Calling the
second `org-transclusion-remove-all' ensures the clearing process
to occur.  This is required because during live-sync, some hooks
that manage the clearing process are temporarily turned
off (removed)."
  ;; Remove transclusions first. To deal with an edge case where transclusions
  ;; were added for a capture buffer -- e.g. `org-capture' or `org-roam-catpure'
  ;; --, check is done for `buffer-file-name' to see if there is a file visited
  ;; by the buffer. If a "temp" buffer, there is no file being visited.
  (when (and (org-transclusion-remove-all)
             (buffer-file-name))
    (org-transclusion-remove-all)))

;;;;---------------------------------------------------------------------------
;;;; Functions for Transclude Keyword

(defun org-transclusion-keyword-string-to-plist ()
  "Return the \"#+transclude:\" keyword's values if any at point."
  (save-excursion
    (beginning-of-line)
    (let ((plist))
      (when (string= "TRANSCLUDE"
                     (org-element-property :key (org-element-at-point)))
        ;; #+transclude: keyword exists.
        ;; Further checking the value
        (when-let ((str (org-element-property :value (org-element-at-point))))
          (dolist (fn org-transclusion-keyword-value-functions) plist
                  (setq plist (append plist (funcall fn str)))))
        plist))))

(defun org-transclusion-keyword-value-link (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by
`org-transclusion-get-string-to-plist'.  It needs to be set in
`org-transclusion-keyword-value-functions'."
  (if (string-match "\\(\\[\\[.+?\\]\\]\\)" string)
      (list :link (org-strip-quotes (match-string 0 string)))
    ;; link mandatory
    (user-error "Error.  Link in #+transclude is mandatory at %d" (point))
    nil))

(defun org-transclusion-keyword-value-disable-auto (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-keyword-value-functions'."
  (when (string-match ":disable-auto" string)
    (list :disable-auto
          (org-strip-quotes (match-string 0 string)))))

(defun org-transclusion-keyword-value-level (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-keyword-value-functions'."
  (when (string-match ":level *\\([1-9]\\)" string)
    (list :level
          (string-to-number (org-strip-quotes (match-string 1 string))))))

(defun org-transclusion-keyword-value-only-contents (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-keyword-value-functions'."
  (when (string-match ":only-contents?" string)
    (list :only-contents
          (org-strip-quotes (match-string 0 string)))))

(defun org-transclusion-keyword-value-exclude-elements (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double quotations are mandatory."
  (when (string-match ":exclude-elements +\"\\(.*\\)\"" string)
    (list :exclude-elements
          (org-trim (org-strip-quotes (match-string 1 string))))))

(defun org-transclusion-keyword-current-indentation (_)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-keyword-value-functions'."
  (list :current-indentation (current-indentation)))

(defun org-transclusion-keyword-value-expand-links (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-keyword-value-functions'."
  (when (string-match ":expand-links" string)
    (list :expand-links
          (org-strip-quotes (match-string 0 string)))))

(defun org-transclusion-keyword-remove ()
  "Remove the keyword element at point.
Returns t if successful.  It checks if the element at point is a
keyword.  If not, returns nil."
  (let* ((elm (org-element-at-point))
         ;; Ignore and keep affiliated keywords before #+transclusion
         ;; Fix issue #115
         (beg (org-element-property :post-affiliated elm))
         (end (org-element-property :end elm))
         (post-blank (org-element-property :post-blank elm)))
    (when (string= "keyword" (org-element-type elm))
      (delete-region beg (- end post-blank))
      t)))

(defun org-transclusion-keyword-plist-to-string (plist)
  "Convert a keyword PLIST to a string."
  (let (;;(active-p (plist-get plist :active-p))
        (link (plist-get plist :link))
        (level (plist-get plist :level))
        (disable-auto (plist-get plist :disable-auto))
        (only-contents (plist-get plist :only-contents))
        (exclude-elements (plist-get plist :exclude-elements))
        (expand-links (plist-get plist :expand-links))
        (custom-properties-string nil))
    (setq custom-properties-string
          (dolist (fn org-transclusion-keyword-plist-to-string-functions
                      custom-properties-string)
            (let ((str (funcall fn plist)))
              (when (and str (not (string-empty-p str)))
                (setq custom-properties-string
                      (concat custom-properties-string " " str ))))))
    (concat "#+transclude: "
            link
            (when level (format " :level %d" level))
            (when disable-auto (format " :disable-auto"))
            (when only-contents (format " :only-contents"))
            (when exclude-elements (format " :exclude-elements \"%s\""
                                           exclude-elements))
            (when expand-links (format " :expand-links"))
            custom-properties-string
            "\n")))

(defun org-transclusion-keyword-plist-to-exclude-elements (plist)
  "Return list of symbols from PLIST when applicable.
If PLIST does not have :exclude-elements, return nil.

This function also attempts to remove empty string that gets
inserted when more than one space is inserted between symbols."
  (let ((str (plist-get plist :exclude-elements)))
    (when str
      (let ((list (split-string str " "))
            elements)
        (dolist (s list elements)
          (unless (string= s "")
            (when (memq (intern s) org-element-all-elements)
              (push (intern s) elements))))))))

;;-----------------------------------------------------------------------------
;;;; Add-at-point functions
(defun org-transclusion-add-org-id (link plist)
  "Return a list for Org-ID LINK object and PLIST.
Return nil if not found."
  (when (string= "id" (org-element-property :type link))
    ;; when type is id, the value of path is the id
    (let* ((id (org-element-property :path link))
           (mkr (ignore-errors (org-id-find id t)))
           (payload '(:tc-type "org-id")))
      (if mkr
          (append payload (org-transclusion-content-org-marker mkr plist))
        (message
         "No transclusion done for this ID. Ensure it works at point %d, line %d"
         (point) (org-current-line))
        nil))))

(defun org-transclusion-add-org-file (link plist)
  "Return a list for Org file LINK object and PLIST.
Return nil if not found."
  (and (string= "file" (org-element-property :type link))
       (org-transclusion-org-file-p (org-element-property :path link))
       (append '(:tc-type "org-link")
               (org-transclusion-content-org-link link plist))))

(defun org-transclusion-add-other-file (link plist)
  "Return a list for non-Org file LINK object and PLIST.
Return nil if not found."
  (and (string= "file" (org-element-property :type link))
       (append '(:tc-type "others-default")
               (org-transclusion-content-others-default link plist))))

;;-----------------------------------------------------------------------------
;;;; Functions for inserting content

(defun org-transclusion-content-insert ( keyword-values type content
                                         sbuf sbeg send copy)
  "Insert CONTENT at point and put source overlay in SBUF.
Return t when successful.

This function formats CONTENT with using one of the
`org-transclusion-content-format-functions'; e.g. align a table
for Org.

This function is intended to be used within
`org-transclusion-add'.  All the arguments should be
obtained by one of the `org-transclusion-add-functions'.

This function adds text properties required for Org-transclusion
to the inserted content.  It also puts an overlay to an
appropriate region of the source buffer.  They are constructed
based on the following arguments:

- KEYWORD-VALUES :: Property list of the value of transclusion keyword
- TYPE :: Transclusion type; e.g. \"org-link\"
- CONTENT :: Text content of the transclusion source to be inserted
- SBUF :: Buffer of the transclusion source where CONTENT comes from
- SBEG :: Begin point of CONTENT in SBUF
- SEND :: End point of CONTENT in SBUF"
  (let* ((beg (point)) ;; before the text is inserted
         (end) ;; at the end of text content after inserting it
         (id (org-id-uuid))
         (tc-buffer (current-buffer))
         (ov-src (text-clone-make-overlay sbeg send sbuf)) ;; source-buffer overlay
         (tc-pair ov-src)
         (content content))
    (when (org-transclusion-type-is-org type)
      (with-temp-buffer
        ;; This temp buffer needs to be in Org Mode
        ;; Otherwise, subtree won't be recognized as a Org subtree
        (delay-mode-hooks (org-mode))
        (insert content)
        (org-with-point-at 1
          (let* ((to-level (plist-get keyword-values :level))
                 (level (org-transclusion-content-highest-org-headline))
                 (diff (when (and level to-level) (- level to-level))))
            (when diff
              (cond ((< diff 0) ; demote
                     (org-map-entries (lambda ()
                                        (dotimes (_ (abs diff))
                                          (org-do-demote)))))
                    ((> diff 0) ; promote
                     (org-map-entries (lambda ()
                                        (dotimes (_ diff)
                                          (org-do-promote))))))))
          (setq content (buffer-string)))))
    (insert
     (run-hook-with-args-until-success
      'org-transclusion-content-format-functions
      type content (plist-get keyword-values :current-indentation)))
    (setq end (point))
    (unless copy
      (add-text-properties
       beg end
       `( local-map ,org-transclusion-map
          read-only t
          front-sticky t
          ;; rear-nonticky seems better for
          ;; src-lines to add "#+result" after C-c
          ;; C-c
          rear-nonsticky t
          org-transclusion-id ,id
          org-transclusion-type ,type
          org-transclusion-pair ,tc-pair
          org-transclusion-orig-keyword ,keyword-values))
      ;; Put to the source overlay
      (overlay-put ov-src 'org-transclusion-by id)
      (overlay-put ov-src 'org-transclusion-buffer tc-buffer)
      (overlay-put ov-src 'evaporate t)
      (overlay-put ov-src 'face 'org-transclusion-source)
      (overlay-put ov-src 'modification-hooks
                   '(org-transclusion-source-overlay-modified))
      (overlay-put ov-src 'priority -60)

      ;; Always apply fringe indicators (works with or without org-indent-mode)
      (org-transclusion-add-fringe-to-region
       sbuf sbeg send 'org-transclusion-source-fringe)
      (org-transclusion-add-fringe-to-region
       (current-buffer) beg end 'org-transclusion-fringe)

      ;; TODO this should not be necessary, but it is at the moment
      ;; live-sync-enclosing-element fails without tc-pair on source overlay
      (overlay-put ov-src 'org-transclusion-pair tc-pair))
    t))

(defun org-transclusion-content-highest-org-headline ()
  "Return the highest level as an integer of all the headlines in buffer.
Returns nil if there is no headline.  Note that level 1 is the
highest; the lower the number, higher the level of headline.

This function assumes the buffer is an Org buffer."
  (let ((tree (org-element-parse-buffer))
        list)
    (org-element-map tree 'headline
      (lambda (h)
        (push (org-element-property :level h) list)))
    (when list (seq-min list))))

(defun org-transclusion-content-format-org (type content _indent)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).

This function is the default for org-transclusion-type (TYPE)
\"org-*\". Currently it only re-aligns table with links in the
content."
  (when (org-transclusion-type-is-org type)
    (with-temp-buffer
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode))
        (insert content)
        ;; Fix table alignment
        (let ((point (point-min)))
          (while point
            (goto-char (1+ point))
            (when (org-at-table-p)
              (org-table-align)
              (goto-char (org-table-end)))
            (setq point (search-forward "|" (point-max) t))))
        ;; Fix indentation when `org-adapt-indentation' is non-nil
        (org-indent-region (point-min) (point-max))
        ;; Return the temp-buffer's string
        (buffer-string)))))

(defun org-transclusion-content-format (_type content indent)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).

This is the default one.  It only returns the content as is.

INDENT is the number of current indentation of the #+transclude."
  (with-temp-buffer
    (insert content)
    ;; Return the temp-buffer's string
    (set-left-margin (point-min)(point-max) indent)
    (buffer-string)))

(defun org-transclusion-content-org-marker (marker plist)
  "Return a list of payload from MARKER and PLIST.
This function is intended to be used for Org-ID.  It delegates the
work to
`org-transclusion-content-org-filtered'."
  (if (and marker (marker-buffer marker)
           (buffer-live-p (marker-buffer marker)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (if (org-before-first-heading-p)
               (org-transclusion-content-org-filtered
                nil plist)
             (org-transclusion-content-org-filtered
              'only-element plist)))))
    (message "Nothing done. Cannot find marker for the ID.")))

(defun org-transclusion-content-org-link (link plist)
  "Return a list of payload from Org LINK object and PLIST.
This function is intended to be used for Org-ID. It delegates the
work to
`org-transclusion-content-org-filtered'."
  (save-excursion
    ;; First visit the buffer and go to the relevant element if
    ;; search-option is present.
    (let* ((path (org-element-property :path link))
           (search-option (org-element-property :search-option link))
           (buf (find-file-noselect path))
           (org-link-search-must-match-exact-headline
            ;; Don't ever prompt to create a headline when transcluding
            (if (eq 'query-to-create org-link-search-must-match-exact-headline)
                t  ;; Less surprising default than nil - fuzzy search
              org-link-search-must-match-exact-headline)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (if search-option
             (progn
               (org-link-search search-option)
               (org-transclusion-content-org-filtered
                'only-element plist))
           (org-transclusion-content-org-filtered
            nil plist)))))))

(defvar org-transclusion-content-filter-org-functions '())

(add-hook 'org-transclusion-content-filter-org-functions
          #'org-transclusion-content-filter-org-only-contents-function)

(add-hook 'org-transclusion-content-filter-org-functions
          #'org-transclusion-content-filter-org-expand-links-function)

(make-obsolete 'org-transclusion-content-org-buffer-or-element
               'org-transclusion-content-org-filtered "1.4.1")

(defun org-transclusion-content-org-filtered (only-element plist)
  "Return a list of payload for transclusion.
This function assumes the point is at the beginning of the org
element to transclude.

The payload is a plist that consists of the following properties:
- :src-content
- :src-buf
- :src-beg
- :src-end

When ONLY-ELEMENT is non-nil, this function looks at only the element
at point; if nil, the whole buffer.

This function applies multiple filters on the Org elements before
constructing the payload based on PLIST. It is the
\"keyword-plist\" for the transclusion being worked on; each
property controls the filter applied to the transclusion."
  (let* ((el (org-element-context))
         (type (when el (org-element-type el))))
    (if (or (not el)(not type))
        (message "Nothing done")
      ;; For dedicated target, we want to get the parent paragraph,
      ;; rather than the target itself
      (when (and (string= "target" type)
                 (string= "paragraph"
                          (org-element-type (org-element-property :parent el))))
        (setq el (org-element-property :parent el)))
      (let ((beg (org-element-property :begin el))
            (end (org-element-property :end el))
            obj)
        (when only-element
          (narrow-to-region beg end))
        (setq obj (org-element-parse-buffer))
        (setq obj (org-transclusion-content-org-filter only-element obj plist))
        (list :src-content (org-element-interpret-data obj)
              :src-buf (current-buffer)
              :src-beg (point-min)
              :src-end (point-max))))))

(defun org-transclusion-content-org-filter (only-element obj plist)
  ;; Apply `org-transclusion-exclude-elements'
  ;; Appending exclude-elements can duplicate symbols
  ;; But that does not influence the output
  (let ((org-transclusion-exclude-elements
         (append (org-transclusion-keyword-plist-to-exclude-elements plist)
                 org-transclusion-exclude-elements)))
    (setq obj (org-element-map obj org-element-all-elements
                #'org-transclusion-content-filter-org-exclude-elements
                nil nil org-element-all-elements nil)))
  ;; First section
  (unless only-element ;only-element is nil when it is a first section
    (setq obj (org-element-map obj org-element-all-elements
                #'org-transclusion-content-filter-org-first-section
                nil nil org-element-all-elements nil)))
  ;; Apply other filters
    (dolist (fn org-transclusion-content-filter-org-functions)
      (let ((obj-returned (funcall fn obj plist)))
        ;; If nil is returned, do not change the org-content (obj)
        (when obj-returned (setq obj obj-returned))))
  obj)

(defun org-transclusion-content-filter-org-expand-links-function (obj plist)
  (when (plist-get plist :expand-links)
    (org-element-map obj 'link #'org-transclusion-content-filter-org-expand-links)
    obj))

(defun org-transclusion-content-filter-org-expand-links (link)
  "Convert LINK to an absolute filename.
LINK is assumed to be an Org element. This function does nothing
to LINK if the link is already absolute.

The current buffer is assumed to be the source buffer for the
transclusion."
  (when (string-equal "file" (org-element-property :type link))
    (let ((path (org-element-property :path link)))
      (unless (file-name-absolute-p path)
        (org-element-put-property
         link :path
         (expand-file-name
          path
          (file-name-directory (buffer-file-name (current-buffer)))))))))

(defun org-transclusion-content-filter-org-exclude-elements (data)
  "Exclude specific elements from DATA.
The elements to be excluded are defined by customizing variable
`org-transclusion-exclude-elements' globally, and adjusted by
:exclude-elements property of the transclusion keyword."
  (org-element-map data org-transclusion-exclude-elements
    (lambda (d) (org-element-extract-element d)))
  data)

(defun org-transclusion-content-filter-org-first-section (data)
  "Exclude the first section from DATA.
The first section is the part before the first headline of an Org
file.  Include it when `org-transclusion-include-first-section'
is non-nil."
  (if (and (eq (org-element-type data) 'section)
           (not org-transclusion-include-first-section))
      nil
    data))

(defun org-transclusion-content-filter-org-only-contents-function (obj plist)
  (when-let ((only-contents (plist-get plist :only-contents)))
    (org-element-map obj org-element-all-elements
      #'org-transclusion-content-filter-org-only-contents
      nil nil 'section nil)))

(defun org-transclusion-content-filter-org-only-contents (data)
  "Exclude headlines from DATA to include only contents."
  (if (eq (org-element-type data) 'headline)
      nil
    data))

;;;;---------------------------------------------------------------------------
;;;; Functions to support non-Org-mode link types

(defun org-transclusion-content-others-default (link _plist)
  "Use Org LINK element to return SRC-CONTENT, SRC-BEG, and SRC-END."
  (let* ((path (org-element-property :path link))
         (buf (find-file-noselect path)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (list :src-content (buffer-string)
             :src-buf buf
             :src-beg (point-min)
             :src-end (point-max))))))

;;-----------------------------------------------------------------------------
;;; Utility Functions

(defun org-transclusion-append-fringe-to-prefix (existing-prefix face)
  "Append fringe indicator to EXISTING-PREFIX, preserving it.
FACE determines the fringe color (org-transclusion-source-fringe or
org-transclusion-fringe).
Returns concatenated string suitable for `line-prefix' or `wrap-prefix'."
  (let ((fringe-indicator
         (if (display-graphic-p)
             (propertize "x" 'display
                         `(left-fringe org-transclusion-fringe-bitmap ,face))
           (propertize "| " 'face face))))
    (if existing-prefix
        (concat existing-prefix fringe-indicator)
      fringe-indicator)))

(defun org-transclusion--make-fringe-indicator (face)
  "Create fringe indicator string for FACE.
Handles both graphical and terminal display modes."
  (if (display-graphic-p)
      (propertize "x" 'display
                  `(left-fringe org-transclusion-fringe-bitmap ,face))
    (propertize "| " 'face face)))

(defun org-transclusion-prefix-has-fringe-p (prefix)
  "Return non-nil if PREFIX already contains a fringe indicator.
Checks for both graphical fringe display property and terminal text."
  (when (stringp prefix)
    (let ((pos 0)
          (found nil))
      (while (and (not found) (< pos (length prefix)))
        (let ((display-prop (get-text-property pos 'display prefix)))
          (when (and (consp display-prop)
                     (eq (car display-prop) 'left-fringe)
                     (eq (nth 1 display-prop) 'org-transclusion-fringe-bitmap))
            (setq found t)))
        (setq pos (1+ pos)))
      (or found
          ;; Also check for terminal fallback "| "
          (string-match-p (regexp-quote "| ") prefix)))))

(defun org-transclusion-add-fringe-to-region (buffer beg end face)
  "Add fringe indicator to each line in BUFFER between BEG and END.
FACE determines the fringe color.

When org-indent-mode is active (line-prefix/wrap-prefix properties exist),
appends fringe to existing indentation. When org-indent-mode is inactive,
adds fringe-only prefix."
  (with-current-buffer buffer
    (with-silent-modifications
      (save-excursion
        (goto-char beg)
        (while (< (point) end)
          (let* ((line-beg (line-beginning-position))
                 (line-end (min (1+ line-beg) end))
                 (line-prefix (get-text-property line-beg 'line-prefix))
                 (wrap-prefix (get-text-property line-beg 'wrap-prefix))
                 (fringe-only (org-transclusion--make-fringe-indicator face)))

            ;; Handle line-prefix
            (if line-prefix
                ;; org-indent-mode case: append to existing prefix
                (unless (org-transclusion-prefix-has-fringe-p line-prefix)
                  (put-text-property line-beg line-end 'line-prefix
                                     (org-transclusion-append-fringe-to-prefix
                                      line-prefix face)))
              ;; Non-indent case: add fringe-only prefix
              (put-text-property line-beg line-end 'line-prefix fringe-only))

            ;; Handle wrap-prefix
            (if wrap-prefix
                ;; org-indent-mode case: append to existing prefix
                (unless (org-transclusion-prefix-has-fringe-p wrap-prefix)
                  (put-text-property line-beg line-end 'wrap-prefix
                                     (org-transclusion-append-fringe-to-prefix
                                      wrap-prefix face)))
              ;; Non-indent case: add fringe-only prefix
              (put-text-property line-beg line-end 'wrap-prefix fringe-only)))
          (forward-line 1))))))

(defun org-transclusion-source-overlay-modified (ov after-p _beg _end &optional _len)
  "Update source overlay OV indentation after modification.
Called by overlay modification hooks. AFTER-P is t after modification.
This ensures fringe indicators stay synchronized with org-indent-mode's
dynamic updates."
  (when (and after-p (overlay-buffer ov))
    (let ((ov-beg (overlay-start ov))
          (ov-end (overlay-end ov)))
      (org-transclusion-add-fringe-to-region
       (overlay-buffer ov) ov-beg ov-end 'org-transclusion-source-fringe))))

(defun org-transclusion-find-source-marker (beg end)
  "Return marker that points to source begin point for transclusion.
It works on the transclusion region at point.  BEG and END are
meant to be transclusion region's begin and end used to limit the
`text-property-search' -- as it does not have an argument to
limit the search, this is done by looking at the output point and
compare it with BEG and END.

Return nil when :parent text-prop cannot be found.

This function critically relies on the fact that `org-element'
puts a \":parent\" text property to the elements obtained by
using `org-element-parse-buffer' and
`org-element--parse-elements' Some elements such as comment-block
does not seem to add :parent, which makes live-sync not working
for them.

Text properties are added by `org-element-put-property' which in
turn uses `org-add-props' macro. If any of this substantially
changes, the logic in this function will need to reviewed."
  (let ((parent (get-text-property (point) ':parent))
        (src-buf (overlay-buffer
                  (get-text-property (point) 'org-transclusion-pair)))
        (m))
    (unless parent
      (save-excursion
        (when-let ((match (or (text-property-search-forward
                               ':parent)
                              (text-property-search-backward
                               ':parent))))
          ;; Point must be between beg and end (inclusive)
          (when (and (<= beg (point)) (<= (point) end))
            (setq parent (prop-match-value match))))))
    (when parent
      (setq m (set-marker (make-marker)
                          (or (org-element-property :contents-begin parent)
                              (org-element-property :begin parent))
                          src-buf)))
    m))

(defun org-transclusion-search-or-add-next-empty-line ()
  "Move point to the next empty line.
If no empty line exists before the next org heading or the end of
the buffer, stop there and add a newline."
  (forward-line)
  (while (not (looking-at-p "^[ \t]*$"))
    (if (or (org-at-heading-p) (eobp))
        (progn (insert "\n")
               (backward-char))
      (forward-line))))

(defun org-transclusion-wrap-path-to-link (path)
  "Return Org link object for PATH string."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert path)
    (org-element-context)))

(defun org-transclusion-org-file-p (path)
  "Return non-nil if PATH is an Org file.
It does so by confirming that the extension is either `org' or `org.gpg'.
The latter form of extension ending with .gpg means it is an encrypted org file.
`file-name-extension' is used to ascertain that PATH is valid."
  (when (file-name-extension path)
    (let* ((path-substrings (split-string path "\\."))
           (last-two-substings (last path-substrings 2))
           (last-substring (car (last last-two-substings)))
           (org-file (string= last-substring "org"))
           (encrpyted-org-file (and (string= (car last-two-substings) "org")
                                    (string= last-substring "gpg"))))
      (or org-file encrpyted-org-file))))

(defun org-transclusion-not-nil (v)
  "Return t or nil.
It is like `org-not-nil', but when the V is non-nil or not
string \"nil\", return symbol t."
  (when (org-not-nil v) t))

(defun org-transclusion-check-add ()
  "Return t if `org-transclusion-add' should work on the point.
Error if transclusion is not allowed.

Currently the following cases are prevented:

Case 1. Element at point is NOT #+transclude:
        Element is in a block - e.g. example
Case 2. #+transclude inside another transclusion"
  (cond
   ;; Case 1. Element at point is NOT #+transclude:
   ((not (org-transclusion-at-keyword-p))
    (user-error
     "Not at a transclude keyword or transclusion in a block at point %d, line %d"
     (point) (org-current-line)))
   ;; Case 2. #+transclude inside another transclusion
   ((org-transclusion-within-transclusion-p)
    (user-error
     "Cannot transclude in another transclusion at point %d, line %d"
     (point) (org-current-line)))
   (t
    t)))

(defun org-transclusion-fix-common-misspelling ()
  "Fix \"#+transclude\" by appending a colon \":\".

When `org-element-at-point' is a paragraph and the first string
of the line after spaces and tabs is \"transclude\", this
function appends a colon \":\". This function does not change the
case, so both \"#+TRANSCLUDE\" and \"#+transclude\" work and the
case will be kept unchanged.

It is a common mistake for users to omit the colon. It is a
workaround to minimize the chance for users experience the known
infinite issue. Refer to issue #177 on the GitHub repository:
https://github.com/nobiot/org-transclusion/issues/177."
  (let ((elm (org-element-at-point)))
    (when (string-equal "paragraph" (org-element-type elm))
      (save-excursion
        (save-match-data
          (let ((bol (line-beginning-position))
                (eol (line-end-position))
                (case-fold-search t))
            (goto-char bol)
            (when (and (re-search-forward "^[[:blank:]]*#\\+\\(\\S-*\\)" eol t)
                       (string-equal-ignore-case
                        "transclude" (match-string-no-properties 1)))
              (replace-match
               (concat (match-string-no-properties 1) ":")
               t nil nil 1)
              ;; return t when the string replaced
              (message "A colon \":\" added to \"#+TRANSCLUDE\" keyword")
              t)))))))

(defun org-transclusion-at-point (&optional point)
  "Return plist representing the transclusion at point.
This function returns a plist of this form:

   (:id ID-STRING :location (BEG . END))

With Elisp, POINT can be passed. Otherwise, the current point is
used."
  (save-excursion
    (and-let* ((pt (or point (point)))
               ;; If the ID is present, the current point is within a
               ;; transclusion.
               (id (get-text-property pt 'org-transclusion-id))
               ;; We need to get both BEGINNING and END of the transclusion at
               ;; point. `prop-match-forward' sets BEGINNING as the current
               ;; point, rather than the beginning of the current transclusion,
               ;; so `prop-match-backward' is also used.
               (prop-match-forward
                (text-property-search-forward 'org-transclusion-id))
               ;; Because the cursor (or POINT) is unlikely to be at the
               ;; beginning, find the END point first.
               (end (prop-match-end prop-match-forward))
               (value (prop-match-value prop-match-forward))
               (prop-match-backward
                ;; As the call to `text-property-search-backward' needs to match
                ;; VALUE, t needs to be passed to PREDICATE unlike
                ;; `text-property-search-forward' a few lines above.
                (text-property-search-backward 'org-transclusion-id value t))
               (beg (prop-match-beginning prop-match-backward)))
      (list :id id :location (cons beg end)))))

(defun org-transclusion-at-keyword-p ()
  "Return non-nil if the current line is on #+TRANSCLUDE: keyword."
  ;;
  ;; BUG (I believe): The following edge case is considered part of keyword
  ;; where "|" is the cursor.
  ;;
  ;; Avoid the following situation to be recognized as "t"
  ;;
  ;;   #+transclude: [[link]]
  ;;   |
  ;;   New paragraph starts
  (let ((edge-case-p
         (save-excursion
           (and (looking-at-p "$")
                (not (bobp))
                (progn (forward-char -1)
                       (looking-at-p "$")))))
        (element (org-element-at-point)))
    ;; If edge-case, do not transclude.
    (unless edge-case-p
      (and (string-equal "keyword" (org-element-type element))
           (string-equal "TRANSCLUDE" (org-element-property :key element))))))

(defun org-transclusion-within-transclusion-p ()
  "Return t if the current point is within a transclusion region."
  (when (get-char-property (point) 'org-transclusion-type) t))

(defun org-transclusion-within-live-sync-p ()
  "Return t if the current point is within a transclusion in live-sync."
  (when (and (org-transclusion-within-transclusion-p)
             (get-char-property (point) 'text-clones))
    t))

(defun org-transclusion-type-is-org (type)
  "Return non-nil if TYPE begins with \"org\".
TYPE is assumed to be a text-property \"org-transclusion-type\"
and is a string."
  (string-prefix-p "org" type 'ignore-case))

;;-----------------------------------------------------------------------------
;;;; Functions for open-source

(defun org-transclusion-open-source-marker (_type)
  "Return a marker pointing to the position and source buffer.
It is intended to be used for `org-transclusion-open-source' and
`org-transclusion-move-to-source'.

This function relies on `org-transclusion-find-source-marker' to
locate the position in the source buffer; thus, the same
limitation applies.  It depends on which org elements whether or
not this function can identify the beginning of the element at
point.  If it cannot, it will return the beginning of the
transclusion, which can be far away from the element at point, if
the transcluded region is large."
  (let* ((tc-elem (org-element-context))
         (tc-beg (org-element-property :begin tc-elem))
         (tc-end (org-element-property :end tc-elem))
         (src-beg-mkr
          (or (org-transclusion-find-source-marker tc-beg tc-end)
              (move-marker (make-marker)
                           (overlay-start (get-text-property
                                           (point)
                                           'org-transclusion-pair))
                           (overlay-buffer (get-text-property
                                           (point)
                                           'org-transclusion-pair))))))
    src-beg-mkr))

;;-----------------------------------------------------------------------------
;;;; Functions for live-sync

(defun org-transclusion-live-sync-source-range-markers (beg end)
  "Find and return source range based on transclusion's BEG and END.
Return \"(src-beg-mkr . src-end-mkr)\"."
  (let ((src-buf (overlay-buffer (get-text-property (point)
                                                    'org-transclusion-pair)))
        (src-search-beg (org-transclusion-find-source-marker beg end)))
    (if (not src-search-beg)
        (user-error "No live-sync can be started at: %d" (point))
      (with-current-buffer src-buf
        (goto-char src-search-beg)
        (when-let* ((ov (get-char-property (point)
                                           'org-transclusion-pair))
                    (src-elem (org-transclusion-live-sync-enclosing-element
                               (overlay-start ov) (overlay-end ov)))
                    (src-beg (org-element-property :begin src-elem))
                    (src-end (org-element-property :end src-elem)))
          (cons
           (move-marker (make-marker) src-beg)
           (move-marker (make-marker) src-end)))))))

(defun org-transclusion-live-sync-source-content (beg end)
  "Return text content between BEG and END.
BEG and END are assumed to be markers for the transclusion's source buffer."
  (when (markerp beg)
    (with-current-buffer (marker-buffer beg)
      (buffer-substring-no-properties beg end))))

(defun org-transclusion-live-sync-modify-overlays (overlays)
  "Add overlay properties specific Org-transclusion for OVERLAYS.
This must be done after `text-clone-set-overlays'.
Org-transclusion always works with a pair of overlays."
  (let ((src-ov (car overlays))
        (tc-ov (cadr overlays)))
    ;; Source Overlay
    (overlay-put src-ov 'face 'org-transclusion-source-edit)
    ;; Transclusion Overlay
    (overlay-put tc-ov 'face 'org-transclusion-edit)
    (overlay-put tc-ov 'local-map org-transclusion-live-sync-map)))

(defun org-transclusion-live-sync-enclosing-element (beg end)
  "Return an enclosing Org element between BEG and END.
This function is intended for live-sync.

This function first looks for elements other than paragraph:

If none of them found, this function identifies the paragraph at
point to return.

Note that live-sync is known to work only for the following elements:
  center-block drawer dynamic-block latex-environment plain-list
  quote-block special-block table verse-block

It is known that live-sync does not work for the other elements
as `org-element' does not add :parent prop to them:
  comment-block export-block example-block fixed-width keyword
  src-block property-drawer not work well

This function works in a temporary org buffer to isolate the
transcluded region and source region from the rest of the
original buffer.  This is required especially when transclusion is
for a paragraph, which can be right next to another paragraph
without a blank space; thus, subsumed by the surrounding
paragraph."
  (let* ((content (buffer-substring beg end))
         (pos (point)))
    (if (length< content 0)
        (user-error (format "Live sync cannot start here: point %d" (point)))
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        ;; Calibrate the start position "Move" to the beg - 1 (buffer position
        ;; with 1, not 0)
        (insert-char ?\n (1- beg))
        (insert content)
        (goto-char pos)
        (let ((context
               (or (org-element-lineage (org-element-context)
                                        '(center-block
                                          comment-block
                                          drawer
                                          dynamic-block
                                          example-block
                                          export-block fixed-width
                                          keyword
                                          latex-environment
                                          plain-list
                                          property-drawer
                                          quote-block special-block
                                          src-block
                                          table
                                          verse-block) 'with-self)
                   ;; For a paragraph
                   (org-element-lineage
                    (org-element-context) '(paragraph) 'with-self))))
          (if context context
            (user-error (format "Live sync cannot start here: point %d"
                                (point)))))))))

(defun org-transclusion-live-sync-refresh-after-exit (list)
  "Refresh the transclusion after live-sync has ended.
This must be done before starting a new live-sync.  LIST is
assumed to be a list that represents the deleted overlay for
transclusion in this structure:
    (buf (beg . end))"
  (when list
    (let ((buf (car list))
          (beg (caadr list))
          (current-p (point)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (goto-char beg)
         (org-transclusion-refresh))
        (goto-char current-p)))))

(defun org-transclusion-live-sync-display-buffer (buffer)
  "Display the source buffer upon entering live-sync edit.
It remembers the current arrangement of windows (window
configuration), deletes the other windows, and displays
BUFFER (intended to be the source buffer being edited in
live-sync.)

This is analogous to `org-edit-src-code' -- by default, it
layouts the edit and original buffers side-by-side.

Upon exiting live-sync, `org-transclusion-live-sync-exit'
attempts to bring back the original window configuration."
  (setq org-transclusion-remember-window-config (current-window-configuration))
  (delete-other-windows)
  (let ((win (selected-window)))
    (pop-to-buffer buffer
                   '(display-buffer-pop-up-window . '(inhibit-same-window)))
    (recenter-top-bottom)
    (select-window win)))

(defun org-transclusion-live-sync-buffers ()
  "Return cons cell of overlays for source and transclusion.
The cons cell to be returned is in this form:

   (SRC-OV . TC-OV)

This function looks at transclusion type property and delegates
the actual process to the specific function for the type.

Assume this function is called with the point on an
org-transclusion overlay."
  (let ((type (get-text-property (point) 'org-transclusion-type)))
    (run-hook-with-args-until-success
     'org-transclusion-live-sync-buffers-functions type)))

(defun org-transclusion-live-sync-buffers-org (type)
  "Return cons cell of overlays for source and transclusion.
The cons cell to be returned is in this form:

    (SRC-OV . TC-OV)

This function uses TYPE to identify Org files to work on only Org
links and IDs."
  (when (org-transclusion-type-is-org type)
    (let* ((beg-end (plist-get (org-transclusion-at-point) :location))
           (beg (car beg-end))
           (end (cdr beg-end))
           (tc-elem (org-transclusion-live-sync-enclosing-element beg end))
           (tc-beg (org-element-property :begin tc-elem))
           (tc-end (org-element-property :end tc-elem))
           (src-range-mkrs (org-transclusion-live-sync-source-range-markers
                            tc-beg tc-end))
           (src-beg-mkr (car src-range-mkrs))
           (src-end-mkr (cdr src-range-mkrs))
           (src-buf (marker-buffer src-beg-mkr))
           (src-content (org-transclusion-live-sync-source-content
                         src-beg-mkr src-end-mkr))
           (src-ov (text-clone-make-overlay
                    src-beg-mkr src-end-mkr src-buf))
           (tc-ov))
      ;; Replace the region as a copy of the src-overlay region
      (save-excursion
        (let* ((inhibit-read-only t)
               (props)
               (beg tc-beg)
               (end tc-end))
          (goto-char beg)
          (setq props (text-properties-at tc-beg))
          (delete-region tc-beg tc-end)
          ;; Before marker is needed
          ;; for an adjacent transclusion
          (insert-before-markers src-content)
          (setq end (point))
          (add-text-properties beg end props)
          ;; Need to move marker that indicate the range of transclusions (not
          ;; live-sync range) when it is for an single element like paragraph
          (setq tc-ov (text-clone-make-overlay beg end))))
      (cons src-ov tc-ov))))

(defun org-transclusion-live-sync-buffers-others-default (_type)
  "Return cons cell of overlays for source and transclusion.
The cons cell to be returned is in this format:

    (SRC-OV . TC-OV)

This function is for non-Org text files."
  ;; Get the transclusion source's overlay but do not directly use it; it is
  ;; needed after exiting live-sync, which deletes live-sync overlays.
  (when-let*
      ((beg-end (plist-get (org-transclusion-at-point) :location))
       (tc-beg (car beg-end))
       (tc-end (cdr beg-end))
       (tc-ov (text-clone-make-overlay tc-beg tc-end))
       (tc-pair (get-text-property (point) 'org-transclusion-pair))
       (src-ov (text-clone-make-overlay
                (overlay-start tc-pair)
                (overlay-end tc-pair)
                (overlay-buffer tc-pair))))
    (cons src-ov tc-ov)))

;;-----------------------------------------------------------------------------
;;;; Functions for yank/paste a region within transclusion

(defun org-transclusion-yank-excluded-properties-set ()
  "Set `yank-excluded-properties' for pasting transcluded text.
This way, the pasted text will not inherit the text props that
are required for live-sync and other transclusion-specific
functions.

List variable
`org-transclusion-yank-remember-user-excluded-props' is used to
ensure the settings revert to the user's setting prior to
`org-transclusion-activate'."
  ;; Ensure this happens only once until deactivation
  (unless (memq 'org-transclusion-type yank-excluded-properties)
    (let ((excluded-props))
    ;; Return t if 'wrap-prefix is already in `yank-excluded-properties'
    ;; if not push to elm the list
    ;; wrap-prefix, etc.
    (dolist (sym org-transclusion-yank-excluded-properties)
      (if (memq sym yank-excluded-properties)
        (push sym org-transclusion-yank-remember-user-excluded-props)
        ;; Avoid duplicate
        (push sym excluded-props)))
    (setq yank-excluded-properties
          (append yank-excluded-properties excluded-props)))))

(defun org-transclusion-yank-excluded-properties-remove ()
  "Remove transclusion-specific text props from `yank-excluded-properties'.
List variable
`org-transclusion-yank-remember-user-excluded-props' is used to
ensure the settings revert to the user's setting prior to
`org-transclusion-activate'."
  ;; Ensure it's called only once until next activation
  (when (memq 'org-transclusion-type yank-excluded-properties)
    (dolist (obj org-transclusion-yank-excluded-properties)
      ;; 'line-prefix and 'wrap-prefix need to be set to the user's set values
      ;; Ensure `yank-excluded-properties' will revert to the user's setting
      ;; for line-prefix, wrap-prefix, etc.
      (unless (memq obj org-transclusion-yank-remember-user-excluded-props)
        (setq yank-excluded-properties (delq obj yank-excluded-properties))))
    (setq org-transclusion-yank-remember-user-excluded-props '())))

;;-----------------------------------------------------------------------------
;;;; Functions for promote/demote a transcluded subtree

(defun org-transclusion-promote-adjust-after ()
  "Adjust the level information after promote/demote."
  (let* ((pos (next-property-change (point) nil (line-end-position)))
         (keyword-plist (get-text-property pos
                                           'org-transclusion-orig-keyword))
         (level (car (org-heading-components))))
    ;; adjust keyword :level prop
    (setq keyword-plist (plist-put keyword-plist :level level))
    (put-text-property (point) (line-end-position)
                       'org-transclusion-orig-keyword keyword-plist)
    ;; refresh to get the text-prop corrected.
    (save-excursion
      (goto-char pos)
      (org-transclusion-refresh))))

(defun org-transclusion-promote-or-demote-subtree (&optional demote)
  "Promote or demote transcluded subtree.
When DEMOTE is non-nil, demote."
  (unless (org-transclusion-within-transclusion-p)
    (user-error "Not in a transcluded headline."))
  (let* ((inhibit-read-only t)
         (beg (car (plist-get (org-transclusion-at-point) :location)))
         (pos (point)))
    (save-excursion
      (goto-char beg)
      (when (org-at-heading-p)
        (if demote (org-demote-subtree) (org-promote-subtree))
        (org-transclusion-promote-adjust-after)))
    (goto-char pos)))

;;-----------------------------------------------------------------------------
;;;; Functions to support Org-export

(defun org-transclusion-inhibit-read-only (&rest _args)
  "Set `inhibit-read-only' to t for Org export functions.
Org export may need the buffer not to contain read-only elements.
This function is meant to be added to
`org-export-before-processing-hook' to temporarily inhibit
read-only."
  (setq-local inhibit-read-only t))

;;-----------------------------------------------------------------------------
;;;; Functions for extensions
;;   It's based on `org-modules'

(defun org-transclusion-load-extensions-maybe (&optional force)
  "Load all extensions listed in `org-transclusion-extensions'.
FORCE will let this function ignore
`org-transclusion-extensions-loaded' and load extensions again."
  (when (or force (not org-transclusion-extensions-loaded))
    (dolist (ext org-transclusion-extensions)
      (condition-case nil (require ext)
        (error (message "Problems while trying to load feature `%s'" ext))))
    (setq org-transclusion-extensions-loaded t)))

;; Load extensions upon loading this file
(org-transclusion-load-extensions-maybe)

(provide 'org-transclusion)
;;; org-transclusion.el ends here
