;;; org-transclusion-transient.el --- Transient menu for org-transclusion -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  Free Software Foundation, Inc.

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

;; Author: Noboru Ota <me@nobiot.com>
;; Created: 1 January 2025
;; Last modified: 03 January 2026

;;; Commentary:

;;  Transient menu for `org-transclusion'. To use the menu, simply call
;;  `org-transclusion-transient-menu'. This command will call a different menu
;;  depending on whether the point is at a transcluded content.

(require 'org-transclusion)
(require 'transient) ; Need more recent than that comes with 29.4; tested on
                     ; transient-20241224.2234

;;; Code:

;; Variables

(defvar org-transclusion-transient-repeat-mode-was-active-p nil)

(defvar org-transclusion-transient-src-languaes '(python emacs-lisp)
  "History for `completing-read' when selecting a src language.")

(defvar org-transclusion-transient-src-history nil
  "History for `completing-read' when selecting a src language.")


;; Helper macro

;; This macro is originally shared by GitHub user stardiviner via
;; https://github.com/nobiot/org-transclusion/issues/169
(defmacro org-transclusion-transient--detect-transclude-at-point-wrapper (body)
  "Evaluate BODY at the end of #+transclude: keyword.
This macro checks if the current line is a transclude keyword line. If
it is not, it emits a user error."
  `(let ((position (point))
         (end-of-line (line-end-position)))
     (if (org-transclusion-at-keyword-p)
         (save-excursion
           (unless (eq position end-of-line) (end-of-line))
           (insert " ")
           ,body
           (pulse-momentary-highlight-region end-of-line (line-end-position)
                                             'pulse-highlight-start-face))
       (user-error "You're not on #+transclude: [[link]] line.?"))))

;; Transient menus

(transient-define-prefix org-transclusion-transient--buffer-menu ()
  "Transient menu when point is not at transcluded content.
In general, users should use command `org-transclusion-transient-menu',
which automatically calls the appropriate transient-prefix."
  [[:description "Add/Remove"
                 ("a" "Add at point" org-transclusion-transient--add
                  :inapt-if-not org-transclusion-at-keyword-p)
                 ("A" "Add all in buffer" org-transclusion-add-all)
                 ("R" "Remove all in buffer" org-transclusion-remove-all)]

    [:description "Options for #+TRANSCLUDE keyword"
                  (:info "Select options. Keep adding")
                  ("i" "insert from link at point or current line"
                   org-transclusion-transient--insert
                   :inapt-if org-transclusion-at-keyword-p)
                  ("l" "level" org-transclusion-transient--level
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("o" "only-contents" org-transclusion-transient--only-contents
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("ex" "exclude-elements"
                   org-transclusion-transient--exclude-elements
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("el" "expand-links" org-transclusion-transient--expand-links
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("no" "no-first-heading" org-transclusion-transient--no-first-heading
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("da" "disable-auto" org-transclusion-transient--disable-auto
                   :inapt-if-not org-transclusion-at-keyword-p)]

    [:description "Addiitonal options: :src and :lines"
                  ;; TODO check the extension to be active
                  :inapt-if-not org-transclusion-at-keyword-p
                  (:info "For extension src-lines")
                  ("ss" "src"
                   org-transclusion-transient--src
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("sr" "rest"
                   org-transclusion-transient--rest
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ""
                  ("sl" "lines (eg 3-5, 6-, -6)"
                   org-transclusion-transient--lines
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("se" "end"
                   org-transclusion-transient--end
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("st" "thing-at-point"
                   org-transclusion-transient--thingatpt
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("sn" "noweb-chunk"
                   org-transclusion-transient--noweb-chunk
                   :inapt-if-not org-transclusion-at-keyword-p)]]

  [[:description "Undo / Redo"
                 ("<left>" "Undo" undo-only :transient t)
                 ("<right>" "Redo" undo-redo :transient t)]
   [:description "Return"
                 ("<RET>" "Quit" transient-quit-all)]]
  (interactive)
  (org-transclusion-transient--setup)
  (transient-setup 'org-transclusion-transient--buffer-menu))

(transient-define-prefix org-transclusion-transient--at-point-menu ()
  "Transient menu when point is at transcluded content.
In general, users should use command `org-transclusion-transient-menu',
which automatically calls the appropriate transient-prefix."
  [:description "Operation on Transclusion at Point"
    [:description "Remove"
                  ("d" "Remove at point"   org-transclusion-remove)
                  ("C" "Detach at point"   org-transclusion-detach)
                  ("R" "Remove all in buffer" org-transclusion-remove-all)]
    [:description "Other at-point functions"
                  ("P" "Promote" org-transclusion-promote-subtree :transient t)
                  ("D" "Demote"  org-transclusion-demote-subtree :transient t)
                  ("o" "Open the source buffer" org-transclusion-open-source)
                  ("O" "Move to the source buffer" org-transclusion-move-to-source)]])

;;;###autoload
(defun org-transclusion-transient-menu ()
  "Call a transient menu for `org-transclusion'.
It calls different menu depending on whether the point is at a
transcluded content or not."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "`org-transclusion' works only in `org' buffer"))
  (let ((org-transclusion-buffer (current-buffer)))
    (if (org-transclusion-within-transclusion-p)
        (org-transclusion-transient--at-point-menu)
      (org-transclusion-transient--buffer-menu))))

;; Private functions

(defun org-transclusion-transient--setup ()
  "Temporarily deactivate Repeat mode, which interferes with transient.
Only when `repeat-mode' is active when calling the transient menu. This
function also sets `org-transclusion-transient--teardown' to
`transient-exit-hook' to automatically turn `repeat-mode' back on."
  (when (and (require 'repeat nil t) repeat-mode)
    (message "Temporarily deactivating Repeat mode")
    (setq org-transclusion-transient-repeat-mode-was-active-p t)
    (repeat-mode -1)
    (add-hook 'transient-exit-hook #'org-transclusion-transient--teardown)))

(defun org-transclusion-transient--teardown ()
  "Turn `repeat-mode' on in `transient-exit-hook'.
See `org-transclusion-transient--setup'"
  (when org-transclusion-transient-repeat-mode-was-active-p
    (repeat-mode +1)
    (setq org-transclusion-transient-repeat-mode-was-active-p nil)))

(defun org-transclusion-transient--read-level (&rest _)
  "Read a string from the minibuffer, restricted to the range 1 to 9 or an empty value."
  (cl-loop for result =
           (read-string "Enter org-transclusion content headline\
level (1-9) or leave empty: ")
           if (or (string= result "")
                  (string-match-p "^[1-9]$" result))
           return result
           else do (progn
                     (message "Invalid input. Number 1-9 or leave empty")
                     (sit-for 1))))

(defun org-transclusion-transient--read-lines (&rest _)
  "Read a string from the minibuffer, restricted to eg 5-10, 6-, -6."
  (cl-loop for result =
           (read-string "Enter :lines option values (eg 5-10, 6-, -6): ")
           if (string-match-p "\"?[0-9]*-[0-9]*\"?" result)
           return result
           else do (progn
                     (message "Invalid input. The format must be eg 5-10, 6-, -6")
                     (sit-for 1))))

;; Transient suffix

(transient-define-suffix org-transclusion-transient--insert ()
  "Call `org-transclusion-insert', which see."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-insert)
  (org-transclusion-transient--buffer-menu))

(transient-define-suffix org-transclusion-transient--level ()
  "Add :level property to transclude keyword.
The command prompts for a number or empty without a number,
which automatically adjust headline levels."
  :transient 'transient--do-stay
  (interactive)
  (let ((level-string (org-transclusion-transient--read-level)))
    (org-transclusion-transient--detect-transclude-at-point-wrapper
     (insert (if (string-empty-p level-string)
                 ":level"
               (format ":level %s" level-string))))))

(transient-define-suffix org-transclusion-transient--only-contents ()
  ":only-content will exclude titles of headlines of a subtree (headline).
With this property, transclude only the contents."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-transient--detect-transclude-at-point-wrapper
   (insert ":only-contents")))

(transient-define-suffix org-transclusion-transient--expand-links ()
  ":expand-links expand the file names in links to absolute file names."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-transient--detect-transclude-at-point-wrapper
   (insert ":expand-links")))

(transient-define-suffix org-transclusion-transient--exclude-elements ()
  "Add org-elements to be excluded.
The command prompts for elements and lets you select multiple items when
you type a certain character (typically a comma). See `crm-separator'."
  :transient 'transient--do-stay
  (interactive)
  (and-let* ((list-elements (completing-read-multiple
                             "Select elements to exclude: "
                             org-element-all-elements))
             (elements-string (mapconcat #'identity list-elements "\s")))
    (org-transclusion-transient--detect-transclude-at-point-wrapper
     (insert (format ":exclude-elements %S" elements-string)))))

(transient-define-suffix org-transclusion-transient--no-first-heading ()
  ":no-first-heading will remove the first headline of a subtree.
This is useful when you wish to merge a subtree into another headline."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-transient--detect-transclude-at-point-wrapper
   (insert ":no-first-heading")))

(transient-define-suffix org-transclusion-transient--disable-auto ()
  "`org-transclusion-add-all' will skip transclusions with :disable-auto."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-transient--detect-transclude-at-point-wrapper
   (insert ":disable-auto")))

(transient-define-suffix org-transclusion-transient--src ()
  ":src property lets you wrap the content in a src-block.
Choose a language from items in
`org-transclusion-transient-src-languaes' or type a language."
  :transient 'transient--do-stay
  (interactive)
  (let ((string (completing-read "Enter language for :src option: "
                                 org-transclusion-transient-src-languaes
                                 nil nil nil
                                 org-transclusion-transient-src-history)))
    (when string
      (org-transclusion-transient--detect-transclude-at-point-wrapper
       (insert (format ":src %s" string))))))

(transient-define-suffix org-transclusion-transient--rest ()
  ":rest for additional properties for the src-block."
  :transient 'transient--do-stay
  (interactive)
  (let ((string (read-string "Enter :rest option values: ")))
    (when string
      (org-transclusion-transient--detect-transclude-at-point-wrapper
       (insert (format ":rest %S" string))))))

(transient-define-suffix org-transclusion-transient--lines ()
  ":lines for range of lines to transclude from a source and text file."
  :transient 'transient--do-stay
  (interactive)
  (let ((string (org-transclusion-transient--read-lines)))
    (when string
      (org-transclusion-transient--detect-transclude-at-point-wrapper
       (insert (format ":lines %s" string))))))

(transient-define-suffix org-transclusion-transient--end ()
  ":end for a search term as the end of content to be transcluded."
  :transient 'transient--do-stay
  (interactive)
  (let ((string (read-string "Enter :end option value: ")))
    (when string
      (org-transclusion-transient--detect-transclude-at-point-wrapper
       (insert (format ":end %S" string))))))

(transient-define-suffix org-transclusion-transient--thingatpt ()
  ":thingatpt to specify a \"thing\" to transclude from the source.
Choose one of the things \"sentence\" \"paragraph\" \"defun\" \"sexp\"."
  :transient 'transient--do-stay
  (interactive)
  (let ((string (completing-read "Enter :thingatpt option value: "
                                 '("sentence" "paragraph" "defun" "sexp"))))
    (when string
      (org-transclusion-transient--detect-transclude-at-point-wrapper
       (insert (format ":thingatpt %s" string))))))

(transient-define-suffix org-transclusion-transient--noweb-chunk ()
  ":noweb-chunk lets you transclude named chunks of noweb.
The name of the chunk is appended to the file name, separated by `::'
like this example:

    #+transclude: [[./file.nw::chunk-A]] :noweb-chunk"
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-transient--detect-transclude-at-point-wrapper
   (insert ":noweb-chunk")))

(transient-define-suffix org-transclusion-transient--add ()
  "Call `org-transclusion-add'.
This will not exit the transient menu. You will navigate to another menu
for the transcluded content."
  :transient 'transient--do-stay
  (interactive)
  (org-transclusion-add)
  (org-transclusion-transient--at-point-menu))

(provide 'org-transclusion-transient)

;;; org-transclusion-transient.el ends here
