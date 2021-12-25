((emacs-lisp-mode
  (fill-column . 80)
  (indent-tabs-mode . nil)
  (time-stamp-format . "%02d %:B %Y")
  (time-stamp-start . "modified:[       ]+\\\\?")
  (time-stamp-end . "$")
  ;; "Last modified in source code files are at line 20"
  (time-stamp-line-limit . 20)
  ;; Need this locale to be "C" or "en_US.UTF-8" or something to standardize the
  ;; time stamp with English
  (system-time-locale . "C")))
