;; -*- lexical-binding: t; -*-

(require 'org-transclusion)

;; Need to fix this
;; Currently it does not work
(org-transclusion-add-all)

(let ((inhibit-read-only t))
  (org-texinfo-export-to-info))

