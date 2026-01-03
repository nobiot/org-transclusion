.POSIX:
EMACS = emacs
MAKEINFO = makeinfo
INSTALLINFO = install-info
MV = mv
RM = rm
MANUAL_HTML_ARGS =--html --no-split --footnote-style=separate --css-ref=resources/manual.css

## Build #############################################################

gh-html: index.html clean

index.html: org-transclusion.texi
	@printf "\n\n### Generating manual .html files \n\n"
	$(MAKEINFO) $(MANUAL_HTML_ARGS) $< -o index.html

org-transclusion.texi: org-transclusion-manual.org
	@printf "\n\n### Generating manual .texi and .info files \n\n"
	$(EMACS) --batch -L ../ --file $< \
		 --eval="(progn (require 'org-transclusion)(org-transclusion-add-all)(org-texinfo-export-to-texinfo))"

.PHONY: clean
clean:
	@printf "\n\n### Clear .texi file \n\n"
	$(RM) org-transclusion.texi*
