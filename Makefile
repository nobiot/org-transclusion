clean:
	find . -name "*.elc" -delete


test-compile:
	emacs --quick --batch --eval "(progn (add-to-list 'load-path default-directory) \
	                                     (byte-compile-file \"org-transclusion.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path default-directory) \
	                                     (byte-compile-file \"text-clone.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path default-directory) \
	                                     (byte-compile-file \"org-transclusion-indent-mode.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path default-directory) \
	                                     (byte-compile-file \"org-transclusion-src-lines.el\"))"
