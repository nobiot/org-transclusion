;;; org-transclusion-src-lines.el --- Extension -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Free Software Foundation, Inc.

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
;; Last modified: 03 January 2026

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

;;;###autoload
(define-minor-mode org-transclusion-src-lines-mode ()
  :lighter nil
  :global t
  :group 'org-transclusion
  (if org-transclusion-src-lines-mode
      (org-transclusion-extension-functions-add-or-remove
       org-transclusion-src-lines-extension-functions)
    (org-transclusion-extension-functions-add-or-remove
     org-transclusion-src-lines-extension-functions :remove)))

(defvar org-transclusion-src-lines-extension-functions
  (list
   ;; Add a new transclusion type
   (cons 'org-transclusion-add-functions #'org-transclusion-add-src-lines)
    ;; Keyword values
   (cons 'org-transclusion-keyword-value-functions
         '(org-transclusion-keyword-value-lines
           org-transclusion-keyword-value-src
           org-transclusion-keyword-value-rest
           org-transclusion-keyword-value-end
           org-transclusion-keyword-value-noweb-chunk
           org-transclusion-keyword-value-thing-at-point))
   ;; plist back to string
   (cons 'org-transclusion-keyword-plist-to-string-functions
         #'org-transclusion-keyword-plist-to-string-src-lines)
   ;; Transclusion content formatting
   (cons 'org-transclusion-content-format-functions
         #'org-transclusion-content-format-src-lines)
   ;; Open source buffer
   (cons 'org-transclusion-open-source-marker-functions
         #'org-transclusion-open-source-marker-src-lines)
   ;; Live-sync
   (cons 'org-transclusion-live-sync-buffers-functions
         #'org-transclusion-live-sync-buffers-src-lines))
  "Alist of functions to activate `org-transclusion-src-lines'.
CAR of each cons cell is a symbol name of an abnormal hook
\(*-functions\). CDR is either a symbol or list of symbols, which
are names of functions to be called in the corresponding abnormal
hook.")

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
    ;; FIXME :lines can be combined with ID links now, but cannot be with file
    ;; links to org files. The original design for :lines was to be used only
    ;; for non-Org files. But this design has not been enforced. We should
    ;; re-consider :lines. The reason why :tc-type "org-lines" is required here
    ;; is for `org-transclusion-content-format-functions'.
    (append (if (string-equal "id" (org-element-property :type link))
                '(:tc-type "org-lines")
              '(:tc-type "lines"))
            (org-transclusion-content-range-of-lines link plist)))))

(defun org-transclusion-content-range-of-lines (link plist)
  "Return a list of payload for a range of lines from LINK and PLIST.

You can specify a range of lines to transclude by adding the :line
property to a transclusion keyword like this:

    #+transclude: [[file:path/to/file.ext]] :lines 1-10

This is taken from Org Export (function
`org-export--inclusion-absolute-lines' in ox.el) with one
exception.  Instead of :lines 1-10 to exclude line 10, it has
been adjusted to include line 10.  This should be more intuitive
when it comes to including lines of code.

In order to transclude a single line, have the the same number in
both places (e.g. 10-10, meaning line 10 only).

One of the numbers can be omitted.  When the first number is
omitted (e.g. -10), it means from the beginning of the file to
line 10. Likewise, when the second number is omitted (e.g. 10-),
it means from line 10 to the end of file."
  (let* ((src-mkr (org-transclusion-add-source-marker link))
         (search-option (org-element-property :search-option link))
         (type (org-element-property :type link))
         (buf (and src-mkr (marker-buffer src-mkr)))
         (lines (plist-get plist :lines))
         (end-search-op (plist-get plist :end))
         (noweb-chunk (plist-get plist :noweb-chunk))
         (thing-at-point (plist-get plist :thing-at-point))
         (thing-at-point (when thing-at-point
                           (make-symbol (cadr (split-string thing-at-point))))))
    (when buf
      (with-current-buffer buf
        (org-with-wide-buffer
         (let* ((start-pos (cond
                            ;; org-element only finds search-option only when
                            ;; type=file. This condition is only for noweb now
                            ((and (equal type "file") search-option noweb-chunk)
                             (save-excursion
                               (org-transclusion--goto-noweb-chunk-beginning search-option)))
                            ;; for others, non-file types, assume that the
                            ;; position in the marker is the intended point
                            (t (marker-position src-mkr))))
                (bounds (when thing-at-point
                          (let ((count (if end-search-op
                                           (string-to-number end-search-op) 1)))
                            (save-excursion
                              (goto-char start-pos)
                              (back-to-indentation)
                              (org-transclusion--bounds-of-n-things-at-point thing-at-point count)))))
                (end-pos (cond ((when thing-at-point (cdr bounds)))
                               ((when end-search-op
                                  (save-excursion
                                    (ignore-errors
                                      ;; FIXME `org-link-search' does not
                                      ;; return position when either ::/regex/
                                      ;; or ::number is used
                                      (when (org-link-search end-search-op)
                                        (line-beginning-position))))))
                               ((when noweb-chunk
                                    (goto-char (1+ start-pos))
                                    (org-transclusion--goto-noweb-chunk-end)
                                    (point)))))
                (range (when lines (split-string lines "-")))
                (lbeg (if range (string-to-number (car range))
                        0))
                (lend (if range (string-to-number (cadr range))
                        0))
                ;; This means beginning part of the range
                ;; can be mixed with search-option
                ;;; only positive number works
                (beg  (progn (goto-char (or start-pos (point-min)))
                             (when (> lbeg 0)(forward-line (1- lbeg)))
                             (point)))
                ;;; This `cond' means :end prop has priority over the end
                ;;; position of the range. They don't mix.
                (end (cond
                      ((when noweb-chunk
                         (org-transclusion--goto-noweb-chunk-:lines-end start-pos end-pos lend)
                         (point)))
                      ((when thing-at-point end-pos))
                      ((when (and end-pos (> end-pos beg))
                         end-pos))
                      ((if (zerop lend) (point-max)
                         (goto-char start-pos)
                         (forward-line (1- lend))
                         (end-of-line);; include the line
                         ;; Ensure to include the \n into the end point
                         (1+ (point))))))
                (content (buffer-substring-no-properties beg end)))
           (list :src-content content
                 :src-buf (current-buffer)
                 :src-beg beg
                 :src-end end)))))))

(defun org-transclusion--goto-noweb-chunk-beginning (chunk-name)
  "Go to the beginning of chunk CHUNK-NAME."
  (goto-char (point-min))
  (re-search-forward (format "<<%s>>=" chunk-name) nil t)
  (forward-line 1)
  (line-beginning-position))

(defun org-transclusion--goto-noweb-chunk-end ()
  "Go to the end of the current chunk.
POINT shall be inside the current chunk."
  ;; A noweb chunk ends at the beginning of the next text chunk ("@")
  ;; or the beginning of the next code chunk ("<<.*>>=").
  (if (re-search-forward "^\\(?:[[:blank:]]*\n\\)*\\(?:@\\|<<.*?>>=\\)" nil t)
      (progn
        (goto-char (match-beginning 0))
        (line-beginning-position))
    ;; Else the chunk ends at the end of the buffer.
    (when (re-search-forward "\\(?:[[:blank:]\n]*\\)*\\'" nil t)
      (goto-char (match-beginning 0)))))

(defun org-transclusion--goto-noweb-chunk-:lines-end (start-pos end-pos lend)
  "Go to the end of the `:lines' range.
START-POS is the beginning of the chunk, END-POS the end of the chunk.
LEND is the end line of the `:lines' range."
  (if (eql lend 0) ; magic number 0 meaning end value of range is missing
      (goto-char end-pos)
    (goto-char start-pos)
    (forward-line lend)
    (when (> (point) end-pos)
        (goto-char end-pos))))

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
  (when (string-match ":lines +\\(\"?[0-9]*-[0-9]*\"?\\)" string)
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

(defun org-transclusion-keyword-value-noweb-chunk (string)
  "Convert keyword STRING to a plist."
  (when (string-match ":noweb-chunk" string)
    (list :noweb-chunk (org-strip-quotes (match-string 0 string)))))

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
        (noweb-chunk (plist-get plist :noweb-chunk))
        (thing-at-point (plist-get plist :thing-at-point)))
    (concat
     (when noweb-chunk ":noweb-chunk")
     (when lines (format " :lines %s" lines))
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
    (let ((ov (get-char-property (point)
                                 'org-transclusion-pair)))
      (move-marker (make-marker) (overlay-start ov) (overlay-buffer ov)))))

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

(defun org-transclusion-content-format-src-lines (type content keyword-values)
  "Format text CONTENT from source before transcluding.
Return content modified (or unmodified, if not applicable).

This is the default one.  It only returns the content as is.

KEYWORD-VALUES is a plist of transclusion properties."
  (when (org-transclusion-src-lines-p type)
    (let ((content (org-transclusion-ensure-newline content)))
      (org-transclusion-content-format type content keyword-values))))

(defun org-transclusion-ensure-newline (str)
  (if (not (string-suffix-p "\n" str))
      (concat str "\n")
    str))

(provide 'org-transclusion-src-lines)
;;; org-transclusion-src-lines.el ends here
