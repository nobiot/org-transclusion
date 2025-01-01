;; -*- lexical-binding: t; -*-

;;    https://github.com/nobiot/org-transclusion/issues/169

(require 'org-transclusion)

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

(defun org-transclusion-insert-from-link ()
  (interactive)
  (and-let* ((link-elem-at-pt
              (or (org-element-lineage (org-element-context) 'link t) ; at-point
                  ;; if not at-point, find the first one in the current line
                  (save-excursion
                    (beginning-of-line)
                    (re-search-forward org-link-bracket-re (line-end-position) t)
                    (org-element-lineage (org-element-context) 'link t))))
             (beg (org-element-begin link-elem-at-pt))
             (end (org-element-end link-elem-at-pt))
             (link-string (buffer-substring beg end)))
    (beginning-of-line)
    (open-line 1)
    (insert (format "#+transclude: %s" link-string))
    (beginning-of-line)
    (pulse-momentary-highlight-region beg (line-end-position) 'pulse-highlight-start-face)))

(transient-define-prefix org-transclusion--buffer-transient ()
  "Prefix that waves at the user"
  [[:description "Add/Remove"
                  ("a" "Add at point"   org-transclusion-add)
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
                   :inapt-if-not org-transclusion-at-keyword-p)]]
  [:description ""
                (:info ".")])

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

(defun org-transclusion-transient-menu ()
  (interactive)
  (let ((org-transclusion-buffer (current-buffer)))
    (if (org-transclusion-within-transclusion-p)
        (org-transclusion--at-point-transient)
      (org-transclusion--buffer-transient))))

