.POSIX:
EMACS = emacs
MAKEINFO = makeinfo
INSTALLINFO = install-info
MV = mv
RM = rm
MANUAL_HTML_ARGS =--html --no-split --footnote-style=separate --css-ref=resources/manual.css

## Build #############################################################

all: manual html install clear

manual:
	@printf "\n\n### Generating manual .texi and .info files \n\n"
	$(EMACS) -Q --batch -L ../ --file org-transclusion.org -l manual-gen.el

html:
	@printf "\n\n### Generating manual .html files \n\n"
	$(MAKEINFO) $(MANUAL_HTML_ARGS) org-transclusion.texi -o index.html

install:
	@printf "\n\n### Installing manual files \n\n"
	$(INSTALLINFO) org-transclusion.info ../dir
	$(MV) org-transclusion.info ../

clear:
	@printf "\n\n### Clear .texi file \n\n"
	$(RM) org-transclusion.texi*
