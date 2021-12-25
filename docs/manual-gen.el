;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;;; Commentary:

;; A helper to generate user manual for Org-transclusion


(require 'org-transclusion)

;; Need to fix this
;; Currently it does not work
(org-transclusion-add-all)

(let ((inhibit-read-only t))
  (org-texinfo-export-to-info))

