;;; org-transclusion-src-lines.el --- Extension -*- lexical-binding: t; -*-

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

;; Author: Noboru Ota <me@nobiot.com>
;; Created: 24 May 2021
;; Last modified: 23 May 2023

;;; Commentary:
;;  This is an extension to `org-transclusion'.  When active, it adds features
;;  for non-Org files such as program source and text files

;;; Code:

(require 'org-element)
(declare-function text-clone-make-overlay "text-clone")
(declare-function org-transclusion-live-sync-buffers-others-default
             "org-transclusion")
(declare-function org-transclusion-org-file-p "org-transclusion")
(declare-function org-transclusion-content-format "org-transclusion")

;;;; Setting up the extension

;; Add a new transclusion type
(add-hook 'org-transclusion-add-functions
          #'org-transclusion-add-src-lines)
;; Keyword values
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-lines)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-src)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-rest)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-end)
(add-hook 'org-transclusion-keyword-value-functions
          #'org-transclusion-keyword-value-thing-at-point)
;; plist back to string
(add-hook 'org-transclusion-keyword-plist-to-string-functions
          #'org-transclusion-keyword-plist-to-string-src-lines)

;; Transclusion content formating
(add-hook 'org-transclusion-content-format-functions
          #'org-transclusion-content-format-src-lines)

;; Open source buffer
(add-hook 'org-transclusion-open-source-marker-functions
          #'org-transclusion-open-source-marker-src-lines)
;; Live-sync
(add-hook 'org-transclusion-live-sync-buffers-functions
          #'org-transclusion-live-sync-buffers-src-lines)

;;; Functions

(defun org-transclusion--bounds-of-n-things-at-point (thing count)
  "Return the bounds of COUNT THING (s) -at-point."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point thing)))
      (when bounds
        (push-mark (car bounds) t t)
        (goto-char (cdr bounds))
        (while (and (> count 1) bounds)
          (setq bounds (bounds-of-thing-at-point thing))
          (when bounds
            (if (> count 1)
                (forward-thing thing)
              (goto-char (cdr bounds)))
            (setq count (1- count))))
        (car (region-bounds))))))

(defun org-transclusion-add-src-lines (link plist)
  "Return a list for non-Org text and source file.
Determine add function based on LINK and PLIST.

Return nil if PLIST does not contain \":src\" or \":lines\" properties."
  (cond
   ((plist-get plist :src)
    (append '(:tc-type "src")
            (org-transclusion-content-src-lines link plist)))
   ;; :lines needs to be the last condition to check because :src INCLUDE :lines
   ((or (plist-get plist :lines)
        (plist-get plist :end)
        ;; Link contains a search-option ::<string>
        ;; and NOT for an Org file
        (and (org-element-property :search-option link)
             (not (org-transclusion-org-file-p (org-element-property :path link)))))
    (append '(:tc-type "lines")
            (org-transclusion-content-range-of-lines link plist)))))

(defun org-transclusion--get-lines-num (lines max-num)
  "Return a list of line numbers from :lines value"
  (when lines
    (flatten-list
     (mapcar (lambda (str)
               (let* ((line (split-string str "-"))
                      (lbeg (if (string-match "^-[0-9]+" str)
                                1
                              (string-to-number (car line))))
                      (lend (if (string-match "[0-9]+-$" str)
                                max-num
                              (let ((last-line-num (string-to-number (car (last line)))))
                                (if (> last-line-num max-num)
                                    max-num
                                  last-line-num)))))
                 (number-sequence lbeg lend
                                  (if (> lbeg lend)
                                      -1
                                    1))))
             (split-string lines "," t)))))

(defun org-transclusion-content-range-of-lines (link plist)
  "Return a list of payload for a range of lines from LINK and PLIST.

You can specify a range of lines to transclude by adding the :line
property to a transclusion keyword like this:

    #+transclude: [[file:path/to/file.ext]] :lines -10,15,12,11,17-

This is taken from Org Export (function
`org-export--inclusion-absolute-lines' in ox.el) with one
exception.  Instead of :lines 1-10 to exclude line 10, it has
been adjusted to include line 10.  This should be more intuitive
when it comes to including lines of code.

One of the numbers can be omitted.  When the first number is
omitted (e.g. -10), it means from the beginning of the file or
element to line 10. Likewise, when the second number is
omitted (e.g. 10-), it means from line 10 to the end of file.

Also, the transcluded lines will conform to the :lines order.
"
  (let* ((path (org-element-property :path link))
         (search-option (org-element-property :search-option link))
         (type (org-element-property :type link))
         (entry-pos) (buf)
         (lines (plist-get plist :lines))
         (end-search-op (plist-get plist :end))
         (thing-at-point (cadr (split-string (plist-get plist :thing-at-point))))
         (thing-at-point (when thing-at-point (make-symbol thing-at-point))))
    (pcase type
      ("id"
       (let ((filename-pos (org-id-find path)))
         (setq buf (find-file-noselect (car filename-pos)))
         (setq entry-pos (cdr filename-pos))))
      ("file"
       (setq buf (find-file-noselect path)))
      ("fuzzy"
       (setq buf (find-file-noselect (buffer-file-name)))
       (setq search-option path))
      (_ nil))
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (let* ((start-pos (cond
                            (entry-pos)
                            ((when search-option
                               (save-excursion
                                 (ignore-errors
                                   ;; FIXME `org-link-search' does not
                                   ;; return postion when eithher
                                   ;; ::/regex/ or ::number is used
                                   (if (org-link-search search-option)
                                       (line-beginning-position))))))
                            ((point-min))))
                (bounds (when thing-at-point
                          (let ((count (if end-search-op
                                           (string-to-number end-search-op) 1)))
                            (save-excursion
                              (goto-char start-pos)
                              (back-to-indentation)
                              (org-transclusion--bounds-of-n-things-at-point thing-at-point count)))))
                (end-pos (cond (thing-at-point (cdr bounds))
                               (end-search-op (save-excursion
                                                (ignore-errors
                                                  ;; FIXME `org-link-search' does not
                                                  ;; return postion when either ::/regex/
                                                  ;; or ::number is used
                                                  (when (org-link-search end-search-op)
                                                    (line-beginning-position)))))
                               (search-option (save-excursion
                                                (ignore-errors
                                                  (when (org-link-search search-option)
                                                    (let* ((element (org-element-context))
                                                           (type (org-element-type element)))
                                                      (when (string= "target" type)
                                                        (let ((parent (org-element-property :parent element)))
                                                          (when (string= "paragraph" (org-element-type parent))
                                                            (setq element parent)))
                                                        (let ((parent (org-element-property :parent element)))
                                                          (when (string= "item" (org-element-type parent))
                                                            (setq element parent))))
                                                      (org-element-property :end element))))))
                               (t (point-max))))
                (src-element-lines (count-lines start-pos end-pos))
                (line-nums (org-transclusion--get-lines-num lines src-element-lines))
                (lbeg (if line-nums (apply #'min line-nums)
                        1))
                (lend (if line-nums (apply #'max line-nums)
                        src-element-lines))
                (content (buffer-substring-no-properties start-pos end-pos))
                (filtered-content
                 (let ((lines (split-string content "\n")))
                   (string-join (mapcar (lambda (slice) (nth (1- slice) lines)) line-nums)
                                "\n"))))
           (list :src-content filtered-content
                 :src-buf (current-buffer)
                 :src-beg start-pos
                 :src-end end-pos)))))))

(defun org-transclusion-content-src-lines (link plist)
  "Return a list of payload from LINK and PLIST in a src-block.
This function is also able to transclude only a certain range of
lines with using :lines n-m property.  Refer to
`org-transclusion-content-range-of-lines' for how the notation
for the range works."
  (let* ((payload (org-transclusion-content-range-of-lines link plist))
         (src-lang (plist-get plist :src))
         (rest (plist-get plist :rest)))
    ;; Modify :src-content if applicable
    (when src-lang
      (setq payload
            (plist-put payload :src-content
                       (let ((src-content (plist-get payload :src-content)))
                         (concat
                          (format "#+begin_src %s" src-lang)
                          (when rest (format " %s" rest))
                          "\n"
                          (org-transclusion-ensure-newline src-content)
                          "#+end_src\n")))))
    ;; Return the payload either modified or unmodified
    payload))

(defun org-transclusion-keyword-value-lines (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional \"1-10\"."
  (when (string-match ":lines +\\(\"?[0-9]*[0-9-,]*[0-9]*\"?\\)" string)
    (list :lines (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-value-src (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional :src \"python\".  The regex should
match a name of language that is one word (e.g. \"python\"), or
two words connected with a hyphen (e.g. \"emacs-lisp\"); however,
it does not match any name with two or more hyphens."
  (when (string-match ":src +\\(\"?\\w*-?\\w*\"?\\)" string)
    (list :src (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-value-rest (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in
`org-transclusion-get-keyword-values-hook'.
Double qutations are mandatory."
  (when (string-match ":rest +\"\\(.*\\)\"" string)
    (list :rest (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-value-end (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
...
Double qutations are mandatory"
  (when (string-match ":end +\"\\(.*\\)\"" string)
    (list :end (org-strip-quotes (match-string 1 string)))))

(defun org-transclusion-keyword-plist-to-string-src-lines (plist)
  "Convert a keyword PLIST to a string.
This function is meant to be used as an extension for function
`org-transclusion-keyword-plist-to-string'.  Add it to the
abnormal hook
`org-transclusion-keyword-plist-to-string-functions'."
  (let ((lines (plist-get plist :lines))
        (src (plist-get plist :src))
        (rest (plist-get plist :rest))
        (end (plist-get plist :end))
        (thing-at-point (plist-get plist :thing-at-point)))
    (concat
     (when lines (format ":lines %s" lines))
     (when src (format " :src %s" src))
     (when rest (format " :rest \"%s\"" rest))
     (when end (format " :end \"%s\"" end))
     (when thing-at-point (format " %s" thing-at-point)))))

(defun org-transclusion-src-lines-p (type)
  "Return non-nil when TYPE is \"src\" or \"lines\".
Return nil if neither."
  (or (string= type "src")
      (string= type "lines")))

(defun org-transclusion-open-source-marker-src-lines (type)
  "Return marker for `org-transclusion-open-source'.
Use TYPE to check relevance."
  (when (org-transclusion-src-lines-p type)
    (get-text-property (point) 'tc-src-beg-mkr)))

(defun org-transclusion-live-sync-buffers-src-lines (type)
  "Return cons cell of overlays for source and trasnclusion.
The cons cell to be returned is in this format:

    (src-ov . tc-ov)

This function uses TYPE to identify relevant files; it's meant
for non-Org text files including program source files."
  (when (org-transclusion-src-lines-p type)
    (cl-destructuring-bind
        (src-ov . tc-ov) (org-transclusion-live-sync-buffers-others-default nil)
      (save-mark-and-excursion
          (org-babel-mark-block)
          (let* ((src-ov-length (- (overlay-end src-ov) (overlay-start src-ov)))
                 (region-length (- (region-end) (region-beginning)))
                 (overlay-has-extra-newline (= 1 (- region-length src-ov-length)))
                 (newline-offset (if overlay-has-extra-newline 1 0)))
            (move-overlay tc-ov
                          (region-beginning)
                          (- (region-end) newline-offset))))
        (cons src-ov tc-ov))))

;;; Thing-at-point
(defun org-transclusion-keyword-value-thing-at-point (string)
  "It is a utility function used converting a keyword STRING to plist.
It is meant to be used by `org-transclusion-get-string-to-plist'.
It needs to be set in `org-transclusion-get-keyword-values-hook'.
Double qutations are optional :thing-at-point \"sexp\".  The regex should
match any valid elisp symbol (but please don't quote it)."
  (when (string-match "\\(:thing-at-point\\|:thingatpt\\) \\([[:alnum:][:punct:]]+\\)" string)
    (list :thing-at-point (org-strip-quotes (match-string 0 string)))))

(defun org-transclusion-content-format-src-lines (type content indent)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).

This is the default one.  It only returns the content as is.

INDENT is the number of current indentation of the #+transclude."
  (when (org-transclusion-src-lines-p type)
    (let ((content (org-transclusion-ensure-newline content)))
      (org-transclusion-content-format type content indent))))

(defun org-transclusion-ensure-newline (str)
  (if (not (string-suffix-p "\n" str))
      (concat str "\n")
    str))

(provide 'org-transclusion-src-lines)
;;; org-transclusion-src-lines.el ends here
