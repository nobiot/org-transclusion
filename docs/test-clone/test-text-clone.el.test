(defun my/add-overlays (ovs)
  (let ((overlays ovs))
    (overlay-put (pop overlays) 'face '(:background "#3f2210" :extend t))
    (dolist (overlay overlays)
      (overlay-put overlay 'face '(:background "#203448" :extend t)))))

(defun test-text-clone ()
  (interactive)
  (let ((src-buf (find-file-noselect "./test-clone-original.txt"))
        (clone-buf1 (find-file-noselect "./test-clone-1.txt"))
        (clone-buf2 (find-file-noselect "./test-clone-2.txt"))
        src-beg src-end src-content src-len
        src-ov clone-ov1 clone-ov2)
    (with-current-buffer src-buf
      (setq src-beg (point-min))
      (setq src-end (point-max))
      (setq src-len (- src-end src-beg))
      (setq src-content (buffer-string))
      (setq src-ov (text-clone-make-overlay src-beg src-end)))
    (with-current-buffer clone-buf1
      (let ((beg (point)))
        (insert src-content)
        (setq clone-ov1 (text-clone-make-overlay beg (+ beg src-len)))))
    (with-current-buffer clone-buf2
      (insert-char ?\n 10)
      (let ((beg (point)))
        (insert src-content)
        (setq clone-ov2 (text-clone-make-overlay beg (+ beg src-len)))))
    (my/add-overlays
     (text-clone-set-overlays src-ov clone-ov1 clone-ov2))))

(defun test-text-clone-finish ()
  (interactive)
  (let ((clone-buf1 (find-file-noselect "./test-clone-1.txt"))
        (clone-buf2 (find-file-noselect "./test-clone-2.txt")))
    (text-clone-delete-overlays)
    (with-current-buffer clone-buf1
      (delete-region (point-min)(point-max)))
    (with-current-buffer clone-buf2
      (delete-region (point-min)(point-max)))))
