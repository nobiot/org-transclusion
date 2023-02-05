;;; org-transclusion.el --- Transclude text content via links -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023  Free Software Foundation, Inc.

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
;; Last modified: 05 February 2023

;; URL: https://github.com/nobiot/org-transclusion
;; Keywords: org-mode, transclusion, writing

;; Version: 1.3.2
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
(require 'org-transclusion-font-lock)
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

 (defcustom org-transclusion-extensions '(org-transclusion-src-lines org-transclusion-font-lock)
  "Extensions to be loaded with org-transclusion.el."
  :set #'org-transclusion-set-extensions
  :type
  '(set :greedy t
        (const :tag "src-lines: Add :src and :lines for non-Org files" org-transclusion-src-lines)
        (const :tag "font-lock: Add font-lock for Org-transclusion" org-transclusion-font-lock)

        (const :tag "indent-mode: Support org-indent-mode" org-transclusion-indent-mode)
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
  :safe 'stringp)

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
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t)
    (t
     :foreground "darkgray"))
  "Face for source region being transcluded in another buffer.")
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
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t)
    (t ))
  "Face for transcluded region in the transcluding buffer.")
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
  "Have the exntensions been loaded already?")

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
  "Remember window config (the arrangment of windows) for the current buffer.
This is for live-sync.  Analogous to
`org-edit-src-code'.")

(defvar org-transclusion-add-functions
  '(org-transclusion-add-org-id
    org-transclusion-add-org-file
    org-transclusion-add-other-file)
  "Define a list of functions to get a payload for transclusion.
These function take two arguments: Org link and keyword plist,
and return a playload.  The payload is defined as a property list
that consists of the following properties:

- :tc-type
- :src-buf
- :src-beg
- :src-end")

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
  '(org-transclusion-type org-transclusion-beg-mkr
  org-transclusion-end-mkr org-transclusion-pair
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
;;;; Definining macros before they are used in the rest of package
;;;; Flycheck warns with "macro X defined too late"
(defmacro org-transclusion-with-inhibit-read-only (&rest body)
  "Run BODY with `'inhibit-read-only` t."
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
  (cond
   (org-transclusion-mode
    (org-transclusion-activate)
    (when org-transclusion-add-all-on-activate
      (org-transclusion-add-all)))
   (t (org-transclusion-deactivate))))

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
  (add-hook 'org-export-before-processing-hook
            #'org-transclusion-inhibit-read-only nil t)
  (org-transclusion-yank-excluded-properties-set)
  (org-transclusion-load-extensions-maybe))

(defun org-transclusion-deactivate ()
  "Dectivate Org-transclusion hooks and other setups in the current buffer.
This function also removes all the transclusions in the current buffer."
  (interactive)
  (org-transclusion-remove-all)
  (remove-hook 'before-save-hook #'org-transclusion-before-save-buffer t)
  (remove-hook 'after-save-hook #'org-transclusion-after-save-buffer t)
  (remove-hook 'kill-buffer-hook #'org-transclusion-before-kill t)
  (remove-hook 'kill-emacs-hook #'org-transclusion-before-kill t)
  (remove-hook 'org-export-before-processing-hook
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

You can pass a prefix argument (ARG) with using
`digit-argument' (e.g. C-1 or C-2; or \\[universal-argument] 3,
so on) or `universal-argument' (\\[universal-argument]).

If you pass a positive number 1-9 with `digit-argument', this
function automatically puts the :level property to the resultant
transclusion keyword.

If you pass a `universal-argument', this function automatically
triggers transclusion by calling `org-transclusion-add' even when
`org-transclusion-mode' is inactive in the current buffer."
  (interactive "P")
  (let* ((context (org-element-lineage
                   (org-element-context)'(link) t))
         (type (org-element-property :type context)))
    (when (or (string= type "file")
              (string= type "id"))
      (let* ((contents-beg (org-element-property :contents-begin context))
             (contents-end (org-element-property :contents-end context))
             (contents (when contents-beg
                         (buffer-substring-no-properties contents-beg contents-end)))
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
          (when (or (equal arg '(4)) org-transclusion-mode)
            (org-transclusion-add)))))))

;;;###autoload
(defun org-transclusion-add ()
  "Transclude text content for the #+transclude at point.
When minor-mode `org-transclusion-mode' is inactive in the
current buffer, this function toggles it on.

Examples of acceptable formats are as below:

- \"#+transclude: [[file:path/file.org::search-option][desc]]:level n\"
- \"#+transclude: [[id:uuid]] :level n :only-contents\"

The file path or id in the transclude keyword value are
translated to the normal Org Mode link format such as
[[file:path/tofile.org::*Heading]] or [[id:uuid]] to copy a piece
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
  (interactive)
  (when (org-transclusion-check-add)
    ;; Turn on the minor mode to load extensions before staring to add.
    (unless org-transclusion-mode
      (let ((org-transclusion-add-all-on-activate nil))
        (org-transclusion-mode +1)))
    (let* ((keyword-plist (org-transclusion-keyword-string-to-plist))
           (link (org-transclusion-wrap-path-to-link
                  (plist-get keyword-plist :link)))
           (payload (run-hook-with-args-until-success
                     'org-transclusion-add-functions link keyword-plist))
           (tc-type (plist-get payload :tc-type))
           (src-buf (plist-get payload :src-buf))
           (src-beg (plist-get payload :src-beg))
           (src-end (plist-get payload :src-end))
           (src-content (plist-get payload :src-content)))
      (if (or (string= src-content "")
              (eq src-content nil))
          ;; Keep going with program when no content `org-transclusion-add-all'
          ;; should move to the next transclusion
          (progn (message
                  (format
                   "No content found with \"%s\".  Check the link at point %d, line %d"
                   (org-element-property :raw-link link) (point) (org-current-line))
                  nil))
        (let ((beg (line-beginning-position))
              (end))
          (org-transclusion-with-inhibit-read-only
            (when (save-excursion
                    (end-of-line) (insert-char ?\n)
                    (org-transclusion-content-insert
                     keyword-plist tc-type src-content
                     src-buf src-beg src-end)
                    (unless (eobp) (delete-char 1))
                    (setq end (point))
                    t)
              ;; `org-transclusion-keyword-remove' checks element at point is a
              ;; keyword or not
              (org-transclusion-keyword-remove)))
          (run-hook-with-args 'org-transclusion-after-add-functions beg end))
        t))))

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
    (let ((marker (move-marker (make-marker) (point))))
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
                (message (format "Transcluded at point %d, line %d" (point) (org-current-line))))))))
      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      t)))

(defun org-transclusion-remove ()
  "Remove transcluded text at point.
When success, return the beginning point of the keyword re-inserted."
  (interactive)
  (if-let* ((beg (marker-position (get-char-property (point)
                                                     'org-transclusion-beg-mkr)))
            (end (marker-position (get-char-property (point)
                                                     'org-transclusion-end-mkr)))
            (keyword-plist (get-char-property (point)
                                              'org-transclusion-orig-keyword))
            (indent (plist-get keyword-plist :current-indentation))
            (keyword (org-transclusion-keyword-plist-to-string keyword-plist))
            (tc-pair-ov (get-char-property (point) 'org-transclusion-pair)))
      (progn
        ;; Need to retain the markers of the other adjacent transclusions
        ;; if any.  If their positions differ after insert, move them back
        ;; beg or end
        (let ((mkr-at-beg
               ;; Check the points to look at exist in buffer.  Then look for
               ;; adjacent transclusions' markers if any.
               (when (>= (1- beg)(point-min))
                 (get-text-property (1- beg) 'org-transclusion-end-mkr))))
          ;; If within live-sync, exit.  It's not absolutely
          ;; required. delete-region below will evaporate the live-sync
          ;; overlay, and text-clone's post-command correctly handles the
          ;; overlay on the source.
          (when (org-transclusion-within-live-sync-p)
            (org-transclusion-live-sync-exit))
          (delete-overlay tc-pair-ov)
          (org-transclusion-with-inhibit-read-only
            (save-excursion
              (delete-region beg end)
              (when (> indent 0) (indent-to indent))
              (insert-before-markers keyword))
            ;; Move markers of adjacent transclusions if any to their original
            ;; potisions.  Some markers move if two transclusions are placed
            ;; without any blank lines, and either of beg and end markers will
            ;; inevitably have the same position (location "between" lines)
            (when mkr-at-beg (move-marker mkr-at-beg beg))
            ;; Go back to the beginning of the inserted keyword line
            (goto-char beg))
          beg))
    (message "Nothing done. No transclusion exists here.") nil))

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
    (let ((marker (move-marker (make-marker) (point)))
          match point list)
      (unless narrowed (widen))
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'org-transclusion-type))
        (goto-char (prop-match-beginning match))
        (setq point (org-transclusion-remove))
        (when point (push point list)))
      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      list)))

(defun org-transclusion-refresh ()
  "Refresh the transcluded text at point."
  (interactive)
  (when (org-transclusion-within-transclusion-p)
    (let ((pos (point)))
      (org-transclusion-remove)
      (org-transclusion-add)
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
remain in the source buffer for further editing."
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
fixed-width, keyword, src-block, and property-drawerd.

`org-transclusion-live-sync-map' inherits `org-mode-map' and adds
a couple of org-transclusion specific keybindings; namely:

- `org-transclusion-live-sync-paste'
- `org-transclusion-live-sync-exit'

\\{org-transclusion-live-sync-map}"
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
              "Lengths of transclusion and source are not identical"))
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
It attemps to re-arrange the windows for the current buffer to
the state before live-sync started."
  (interactive)
  (if (not (org-transclusion-within-live-sync-p))
      (user-error "Not within a transclusion in live-sync")
    (text-clone-delete-overlays)
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
;;;; Functions for Activate / Deactiveate / save-buffer hooks

(defun org-transclusion-before-save-buffer ()
  "Remove translusions in `before-save-hook'.
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
        ;; After save and adding all transclusions, the modified flag should be
        ;; set to nil
        (restore-buffer-modified-p nil)
        (when org-transclusion-remember-point
          (goto-char org-transclusion-remember-point))
    (progn
      (setq org-transclusion-remember-point nil)
      (setq org-transclusion-remember-transclusions nil)))))

(defun org-transclusion-before-kill ()
  "Remove transclusions before `kill-buffer' or `kill-emacs'.
Intended to be used with `kill-buffer-hook' and `kill-emacs-hook'
to clear the file of the transcluded text regions.  This function
also flags the buffer modified and `save-buffer'.  Calling the
second `org-transclusion-remove-all' ensures the clearing process
to occur.  This is reqiured because during live-sync, some hooks
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
  "Return the \"#+transcldue:\" keyword's values if any at point."
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
Double qutations are mandatory."
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

This function also attemps to remove empty string that gets
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
         (format "No transclusion done for this ID. Ensure it works at point %d, line %d"
                 (point) (org-current-line)))
        nil))))

(defun org-transclusion-add-org-file (link plist)
  "Return a list for Org file LINK object and PLIST.
Return nil if not found."
  (when (org-transclusion-org-file-p (org-element-property :path link))
    (append '(:tc-type "org-link")
            (org-transclusion-content-org-link link plist))))

(defun org-transclusion-add-other-file (link plist)
  "Return a list for non-Org file LINK object and PLIST.
Return nil if not found."
  (append '(:tc-type "others-default")
          (org-transclusion-content-others-default link plist)))

;;-----------------------------------------------------------------------------
;;;; Functions for inserting content

(defun org-transclusion-content-insert (keyword-values type content sbuf sbeg send)
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
         (beg-mkr (set-marker (make-marker) beg))
         (end) ;; at the end of text content after inserting it
         (end-mkr)
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
    (setq end-mkr (set-marker (make-marker) end))
    (add-text-properties beg end
                         `(local-map ,org-transclusion-map
                                     read-only t
                                     front-sticky t
                                     ;; rear-nonticky seems better for
                                     ;; src-lines to add "#+result" after C-c
                                     ;; C-c
                                     rear-nonsticky t
                                     org-transclusion-type ,type
                                     org-transclusion-beg-mkr
                                     ,beg-mkr
                                     org-transclusion-end-mkr
                                     ,end-mkr
                                     org-transclusion-pair
                                     ,tc-pair
                                     org-transclusion-orig-keyword
                                     ,keyword-values
                                     ;; TODO Fringe is not supported for terminal
                                     line-prefix
                                     ,(org-transclusion-propertize-transclusion)
                                     wrap-prefix
                                     ,(org-transclusion-propertize-transclusion)))
    ;; Put to the source overlay
    (overlay-put ov-src 'org-transclusion-by beg-mkr)
    (overlay-put ov-src 'evaporate t)
    (overlay-put ov-src 'line-prefix (org-transclusion-propertize-source))
    (overlay-put ov-src 'wrap-prefix (org-transclusion-propertize-source))
    (overlay-put ov-src 'priority -50)
    ;; TODO this should not be necessary, but it is at the moment
    ;; live-sync-enclosing-element fails without tc-pair on source overlay
    (overlay-put ov-src 'org-transclusion-pair tc-pair)
    t))

(defun org-transclusion-content-highest-org-headline ()
  "Return the highest level as an integer of all the headlines in buffer.
Returns nil if there is no headline.  Note that level 1 is the
highest; the lower the number, higher the level of headline.

This function sssumes the buffer is an Org buffer."
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
This function is intended to be used for Org-ID.  It delates the
work to
`org-transclusion-content-org-buffer-or-element'."
  (if (and marker (marker-buffer marker)
           (buffer-live-p (marker-buffer marker)))
      (progn
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (if (org-before-first-heading-p)
               (org-transclusion-content-org-buffer-or-element
                nil plist)
             (org-transclusion-content-org-buffer-or-element
              'only-element plist)))))
    (message "Nothing done. Cannot find marker for the ID.")))

(defun org-transclusion-content-org-link (link plist)
  "Return a list of payload from Org LINK object and PLIST.
This function is intended to be used for Org-ID.  It delates the
work to
`org-transclusion-content-org-buffer-or-element'."
  (save-excursion
    ;; First visit the buffer and go to the relevant elelement if
    ;; search-option is present.
    (let* ((path (org-element-property :path link))
           (search-option (org-element-property :search-option link))
           (buf (find-file-noselect path)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (if search-option
             (progn
               (org-link-search search-option)
               (org-transclusion-content-org-buffer-or-element
                'only-element plist))
           (org-transclusion-content-org-buffer-or-element
            nil plist)))))))

(defun org-transclusion-content-org-buffer-or-element (only-element plist)
  "Return a list of playload for transclusion.
Tis function assumes the point is at the beginning of the org
element to transclude.

The payload is a plist that consists of the following properties:
- :src-content
- :src-buf
- :src-beg
- :src-end

When ONLY-ELEMENT is non-nil, this function looks at only the element
at point; if nil, the whole buffer.

This function applies multiple filters on the Org elements before
construting the payload based on PLIST. It is the
\"keyword-plist\" for the transclusion being worked on; each
property controls the filter applied to the transclusion."
  (let* ((el (org-element-context))
         (type (when el (org-element-type el))))
    (if (or (not el)(not type))
        (message "Nothing done")
      ;; For dedicated target, we want to get the parent paragraph,
      ;; rather than the target itself
      (when (and (string= "target" type)
                 (string= "paragraph" (org-element-type (org-element-property :parent el))))
        (setq el (org-element-property :parent el)))
      (let ((beg (org-element-property :begin el))
            (end (org-element-property :end el))
            (only-contents (plist-get plist :only-contents))
            (exclude-elements
             (org-transclusion-keyword-plist-to-exclude-elements plist))
            (expand-links (plist-get plist :expand-links))
            obj)
        (when only-element
          (narrow-to-region beg end))
        (setq obj (org-element-parse-buffer))
        ;; Apply `org-transclusion-exclude-elements'
        ;; Appending exclude-elements can duplicate symbols
        ;; But that does not influence the output
        (let ((org-transclusion-exclude-elements
               (append exclude-elements org-transclusion-exclude-elements)))
          (setq obj (org-element-map obj org-element-all-elements
                      #'org-transclusion-content-filter-org-exclude-elements
                      nil nil org-element-all-elements nil)))
        ;; First section
        (unless only-element ;only-element is nil when it is a first section
          (setq obj (org-element-map obj org-element-all-elements
                      #'org-transclusion-content-filter-org-first-section
                      nil nil org-element-all-elements nil)))
        ;; Only contents
        (when only-contents
          (setq obj (org-element-map obj org-element-all-elements
                      #'org-transclusion-content-filter-org-only-contents
                      nil nil '(section) nil)))

        ;; Expand file names in all the links
        (when expand-links
          (org-element-map obj 'link #'org-transclusion-content-filter-expand-links))

        (list :src-content (org-element-interpret-data obj)
              :src-buf (current-buffer)
              :src-beg (point-min)
              :src-end (point-max))))))

(defun org-transclusion-content-filter-expand-links (link)
  "Convert LINK to an absolute filename.
LINK is assumed to be an Org element. This function does nothing
to LINK if the link is already absolute.

The current buffer is assumed to be the source buffer for the
transclusion."
  (when (string-equal "file" (org-element-property :type link))
    (let ((raw-link (org-element-property :raw-link link)))
      (unless (file-name-absolute-p raw-link)
        (org-element-put-property
         link :path
         (expand-file-name
          raw-link
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

(defun org-transclusion-content-filter-org-only-contents (data)
  "Exclude headlines from DATA to include only contents."
  (if (eq (org-element-type data) 'headline)
      nil
    data))

;;;;---------------------------------------------------------------------------
;;;; Functions to support non-Org-mode link types

(defun org-transclusion-content-others-default (link _plist)
  "Use Org LINK element to return TC-CONTENT, TC-BEG-MKR, and TC-END-MKR.
TODO need to handle when the file does not exist."
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

Text properties are addeb by `org-element-put-property' which in
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
  "Search the next empty line.
Start with the next line.  If the current line is the bottom of
the line, add a new empty line."
  ;; beginning-of-line 2 moves to the next line if possible
  (beginning-of-line 2)
  (if (eobp)(insert "\n")
    (while (not (looking-at-p "[ \t]*$"))
      (beginning-of-line 2))
    (if (eobp)(insert "\n"))))

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
   ((let ((elm (org-element-at-point)))
      (not (and (string= "keyword" (org-element-type elm))
                (string= "TRANSCLUDE" (org-element-property :key elm)))))
    (user-error (format "Not at a transclude keyword or transclusion in a block at point %d, line %d"
                        (point) (org-current-line))))
   ;; Case 2. #+transclude inside another transclusion
   ((org-transclusion-within-transclusion-p)
    (user-error (format "Cannot transclude in another transclusion at point %d, line %d"
                        (point) (org-current-line))))
   (t
    t)))

(defun org-transclusion-within-transclusion-p ()
  "Return t if the current point is within a tranclusion region."
  (when (get-char-property (point) 'org-transclusion-type) t))

(defun org-transclusion-within-live-sync-p ()
  "Return t if the current point is within a transclusion in live-sync."
  (when (and (org-transclusion-within-transclusion-p)
             (get-char-property (point) 'text-clones))
    t))

(defun org-transclusion-propertize-transclusion ()
  "."
  (if (not (display-graphic-p))
      (propertize "| " 'face 'org-transclusion)
    (propertize
     "x"
     'display
     '(left-fringe org-transclusion-fringe-bitmap
                   org-transclusion-fringe))))

(defun org-transclusion-propertize-source ()
  "."
  (if (not (display-graphic-p))
      (propertize "| " 'face 'org-transclusion-source)
    (propertize
     "x"
     `display
     `(left-fringe empty-line
                   org-transclusion-source-fringe))))

(defun org-transclusion-type-is-org (type)
  "Return non-nil if TYPE begins with \"org\".
TYPE is assumed to be a text-property \"org-transclusion-type\"
and is a string."
  (string-prefix-p "org" type 'ignore-case))

;;-----------------------------------------------------------------------------
;;;; Functions for open-source

(defun org-transclusion-open-source-marker (_type)
  "Return a marker pointing to the position and soure buffer.
It is intended to be used for `org-transclusion-open-source' and
`org-transclusion-move-to-source'.

This function relies on `org-transclusion-find-source-marker' to
locate the position in the source buffer; thus, the same
limitation applies.  It depends on which org elements whether or
not this function can identify the beginnning of the element at
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
        (when-let* ((src-elem (org-transclusion-live-sync-enclosing-element))
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

(defun org-transclusion-live-sync-enclosing-element ()
  "Return an enclosing Org element for live-sync.
This assumes the point is within the element (at point).

This function first looks for elements other than paragraph:

If none of them found, this function identifies the paragraph at
point to return.

Note that live-sync is known to work only for the following elements:
  center-block drawer dynamic-block latex-environment plain-list
  quote-block special-block table verse-block

It is known that live-sync does not work for the other elements
as `org-element' does not add :parent prop to them:
  comment-block export-block example-block fixed-width keyword
  src-block property-drawerd not work well

This function works in a temporary org buffer to isolate the
transcluded region and source region from the rest of the
original buffer.  This is required especially when translusion is
for a paragraph, which can be right next to another paragraph
without a blank space; thus, subsumed by the surrounding
paragraph."
  (let* ((beg (or (when-let ((m (get-char-property (point)
                                                   'org-transclusion-beg-mkr)))
                    (marker-position m))
                  (overlay-start (get-char-property (point)
                                                    'org-transclusion-pair))))
         (end (or (when-let ((m (get-char-property (point)
                                                   'org-transclusion-end-mkr)))
                    (marker-position m))
                  (overlay-end (get-char-property (point)
                                                  'org-transclusion-pair))))
         (content (buffer-substring beg end))
         (pos (point)))
    (if (or (not content)
            (string= content ""))
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
It rembembers the current arrangement of windows (window
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
  "Return cons cell of overlays for source and trasnclusion.
The cons cell to be returned is in this format:

   (src-ov . tc-ov)

This function looks at transclusion type property and delegates
the actual process to the specific function for the type.

Assume this function is called with the point on an
org-transclusion overlay."
  (let ((type (get-text-property (point) 'org-transclusion-type)))
    (run-hook-with-args-until-success
     'org-transclusion-live-sync-buffers-functions type)))

(defun org-transclusion-live-sync-buffers-org (type)
  "Return cons cell of overlays for source and trasnclusion.
The cons cell to be returned is in this format:

    (src-ov . tc-ov)

This function uses TYPE to identify Org files to work on only Org
links and IDs."
  (when (org-transclusion-type-is-org type)
    (let* ((tc-elem (org-transclusion-live-sync-enclosing-element))
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
               (end tc-end)
               ;; Only applicable if there is another transclusion
               ;; immediately before the one starting to live-sync
               (end-mkr-at-beg
                (get-text-property (1- beg) 'org-transclusion-end-mkr)))
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
          (let ((beg-mkr (get-text-property beg 'org-transclusion-beg-mkr))
                (end-mkr (get-text-property beg 'org-transclusion-end-mkr)))
            (when (> beg-mkr beg)
              (move-marker beg-mkr beg))
            (when (< end-mkr end)
              (move-marker end-mkr end))
            ;; deal with the other transclusion immediately before this.
            (when (and end-mkr-at-beg
                       (not (eq end-mkr-at-beg end-mkr)))
              (move-marker end-mkr-at-beg beg)))
          (setq tc-ov (text-clone-make-overlay beg end))))
      (cons src-ov tc-ov))))

(defun org-transclusion-live-sync-buffers-others-default (_type)
  "Return cons cell of overlays for source and trasnclusion.
The cons cell to be returned is in this format:

    (src-ov . tc-ov)

This function is for non-Org text files."
  ;; Get the transclusion source's overlay but do not directly use it; it is
  ;; needed after exiting live-sync, which deletes live-sync overlays.
  (when-let* ((tc-pair (get-text-property (point) 'org-transclusion-pair))
              (src-ov (text-clone-make-overlay
                       (overlay-start tc-pair)
                       (overlay-end tc-pair)
                       (overlay-buffer tc-pair)))
              (tc-ov (text-clone-make-overlay
                      (get-text-property (point) 'org-transclusion-beg-mkr)
                      (get-text-property (point) 'org-transclusion-end-mkr))))
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
  ;; find org-transclusion-beg-mkr. If the point is directly on the starts,
  ;; you need to find it in the headline title.  Assume point at beginning of
  ;; the subtree after promote/demote
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
  (if (not (org-transclusion-within-transclusion-p))
      (message "Not in a transcluded headline.")
    (let ((inhibit-read-only t)
          (beg (get-text-property (point) 'org-transclusion-beg-mkr)))
      (let ((pos (point)))
        (save-excursion
          (goto-char beg)
          (when (org-at-heading-p)
            (if demote (org-demote-subtree) (org-promote-subtree))
            (org-transclusion-promote-adjust-after)))
        (goto-char pos)))))

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

(provide 'org-transclusion)
;;; org-transclusion.el ends here
