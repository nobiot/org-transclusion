docs/org-transclusion.org: docs/org-transclusion-manual.org
	emacs --batch -L "$$(pwd)" -l org-transclusion $< \
	      --eval '(progn (org-transclusion-add-all) (write-region nil nil "org-transclusion.org"))'

test-compile:
	emacs --batch --eval "(add-to-list 'load-path default-directory)" \
              -f batch-byte-compile ./*.el
        # Check declare-function
	emacs --batch --eval "(check-declare-directory default-directory)"

clean:
	find . -name "*.elc" -delete
