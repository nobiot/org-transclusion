;; -*- lexical-binding: t; -*-

(require 'org-transclusion)

(ignore-error
    (org-transclusion-add-all))

(let ((inhibit-read-only t))
  (org-texinfo-export-to-info))

