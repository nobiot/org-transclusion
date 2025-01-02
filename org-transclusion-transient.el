;; -*- lexical-binding: t; -*-

;;    https://github.com/nobiot/org-transclusion/issues/169

(require 'org-transclusion)
(require 'transient) ; Need more recent than that comes with 29.4; tested on
                    ; transient-20241224.2234

;; Utilities

(defmacro hydra-org-transclusion--detect-transclude-at-point-wrapper (body)
  `(let ((line-text (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
         (position (point))
         (end-of-line (line-end-position)))
     (if (string-match-p "#\\+transclude:" line-text)
         (save-excursion
           (unless (eq position end-of-line) (end-of-line))
           (insert " ")
           ,body
           (pulse-momentary-highlight-region end-of-line (line-end-position)
                                             'pulse-highlight-start-face))
       (user-error "You'r not on #+transclude: [[link]] line."))))

(defun org-transclusion--transient-read-level (&rest _)
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

(defun org-transclusion--transient-read-lines (&rest _)
  "Read a string from the minibuffer, restricted to eg 5-10, 6-, -6."
  (cl-loop for result =
           (read-string "Enter :lines option values (eg 5-10, 6-, -6): ")
           if (string-match-p "\"?[0-9]*-[0-9]*\"?" result)
           return result
           else do (progn
                     (message "Invalid input. The format must be eg 5-10, 6-, -6")
                     (sit-for 1))))

;; You can add `org-roam-node-insert' as an example.
(defvar org-transclusion-insert-link-functions '(org-insert-link))

(defun org-transclusion-insert-org-link ()
  (let ((function (if (length> org-transclusion-insert-link-functions 1)
                      (intern (completing-read
                               "Choose a function: "
                               org-transclusion-insert-link-functions))
                    (car org-transclusion-insert-link-functions))))
    (when function
      (with-temp-buffer
        (funcall function)
        (buffer-string)))))

(defun org-transclusion-insert-from-link (&optional insert-below)
  "Insert #+TRANSCLUDE: keyword from a link.
If you pass a `universal-argument' via \\[universal-argument]
 \(INSERT-BELOW is non-nil\), the keyword is added to the line
 below current one. Otherwise, to the line above."
  (interactive "P")
  (let* ((link-elem-at-pt
          (or (org-element-lineage (org-element-context) 'link t) ; at-point
              ;; if not at-point, find the first one in the current line
              (save-excursion
                (beginning-of-line)
                (re-search-forward org-link-bracket-re (line-end-position) t)
                (org-element-lineage (org-element-context) 'link t))))
         (blank-line-p (save-excursion
                         (beginning-of-line)
                         (looking-at-p "^[ \t]*$")))
         (link-string (cond
                       (link-elem-at-pt
                        (buffer-substring (org-element-begin link-elem-at-pt)
                                          (org-element-end link-elem-at-pt)))
                       (blank-line-p
                        (org-transclusion-insert-org-link)))))
    (when link-string
      ;; When the current line is not blank, open a line above or below the
      ;; current.
      (unless blank-line-p
        (when insert-below (progn (forward-line 1) (unless (bolp) (insert "\n"))))
        (beginning-of-line)
        (open-line 1))
      (insert (format "#+transclude: %s" link-string))
      (beginning-of-line)
      (pulse-momentary-highlight-region
       (point) (line-end-position) 'pulse-highlight-start-face))))

(transient-define-prefix org-transclusion--buffer-transient ()
  "Prefix that waves at the user"
  [[:description "Add/Remove"
                 ("a" "Add at point"
                  org-transclusion-add
                  :transient transient--do-return)
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
                  ("sl" "end"
                   org-transclusion-transient--end
                   :inapt-if-not org-transclusion-at-keyword-p)
                  ("st" "thing-at-point"
                   org-transclusion-transient--thingatpt
                   :inapt-if-not org-transclusion-at-keyword-p)]]
  [:description "Setting"
                (:info ".")
                ("-m" "Show more" test/set-level)
                ("-l" "show less" test/set-level-less)])

(transient-define-suffix test/set-level ()
  :transient t
  (interactive)
  (transient-set-level 'org-transclusion--buffer-transient
                       4)
  (org-transclusion-transient-menu))

(transient-define-suffix test/set-level-less ()
  :transient t
  (interactive)
  (transient-set-level 'org-transclusion--buffer-transient
                       3)
  (org-transclusion-transient-menu))

(transient-define-prefix org-transclusion--at-point-transient ()
  "Prefix that waves at the user"
  [:description
    "Operation on Transclusion at Point"
    [:description "Remove"
                  ("r" "Remove at point"   org-transclusion-remove)
                  ("d" "Detach at point"   org-transclusion-detach)
                  ("R" "Remove all in buffer" org-transclusion-remove-all)]
    [:description "Other at-point functions"
                  ("P" "Promote" org-transclusion-promote-subtree)
                  ("D" "Demote"  org-transclusion-demote-subtree)
                  ("o" "Open the source buffer" org-transclusion-open-source)
                  ("O" "Move to the source buffer" org-transclusion-move-to-source)]]
  [:description ""
                (:info ".")])

(transient-define-suffix org-transclusion-transient--insert ()
  :transient 'transient--do-return
  (interactive)
  (org-transclusion-insert-from-link)
  (org-transclusion--buffer-transient))

(transient-define-suffix org-transclusion-transient--level ()
  :transient 'transient--do-stay
  (interactive)
  (let ((level-string (org-transclusion--transient-read-level)))
    (hydra-org-transclusion--detect-transclude-at-point-wrapper
     (insert (if (string-empty-p level-string)
                 ":level"
               (format ":level %s" level-string))))))

(transient-define-suffix org-transclusion-transient--only-contents ()
  :transient 'transient--do-stay
  (interactive)
  (hydra-org-transclusion--detect-transclude-at-point-wrapper
   (insert ":only-contents")))

(transient-define-suffix org-transclusion-transient--expand-links ()
  :transient 'transient--do-stay
  (interactive)
  (hydra-org-transclusion--detect-transclude-at-point-wrapper
   (insert ":expand-links")))

(transient-define-suffix org-transclusion-transient--exclude-elements ()
  :transient 'transient--do-stay
  (interactive)
  (and-let* ((list-elements (completing-read-multiple
                             "Select elements to exclude: "
                             org-element-all-elements))
             (elements-string (mapconcat #'identity list-elements "\s")))
    (hydra-org-transclusion--detect-transclude-at-point-wrapper
     (insert (format ":exclude-elements %S" elements-string)))))

(transient-define-suffix org-transclusion-transient--src ()
  :transient 'transient--do-stay
  (interactive)
  (let ((string (read-string "Enter language for :src option: ")))
    (when string
      (hydra-org-transclusion--detect-transclude-at-point-wrapper
       (insert (format ":src %s" string))))))

(transient-define-suffix org-transclusion-transient--rest ()
  :transient 'transient--do-stay
  (interactive)
  (let ((string (read-string "Enter :rest option values: ")))
    (when string
      (hydra-org-transclusion--detect-transclude-at-point-wrapper
       (insert (format ":rest %S" string))))))

(transient-define-suffix org-transclusion-transient--lines ()
  :transient 'transient--do-stay
  (interactive)
  (let ((string (org-transclusion--transient-read-lines)))
    (when string
      (hydra-org-transclusion--detect-transclude-at-point-wrapper
       (insert (format ":lines %s" string))))))

(transient-define-suffix org-transclusion-transient--end ()
  :transient 'transient--do-stay
  (interactive)
  (let ((string (read-string "Enter :end option value: ")))
    (when string
      (hydra-org-transclusion--detect-transclude-at-point-wrapper
       (insert (format ":end %S" string))))))

(transient-define-suffix org-transclusion-transient--thingatpt ()
  :transient 'transient--do-stay
  (interactive)
  (let ((string (completing-read "Enter :thingatpt option value: "
                                 '("sentence" "paragraph" "defun" "sexp"))))
    (when string
      (hydra-org-transclusion--detect-transclude-at-point-wrapper
       (insert (format ":thingatpt %s" string))))))


(defun org-transclusion-transient-menu ()
  (interactive)
  (let ((org-transclusion-buffer (current-buffer)))
    (if (org-transclusion-within-transclusion-p)
        (org-transclusion--at-point-transient)
      (org-transclusion--buffer-transient))))
