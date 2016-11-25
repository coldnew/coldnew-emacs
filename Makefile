EMACS ?= emacs

all: init.el compile

test: clean
	${MAKE} all

bootstrap:
	${MAKE} clean
	${MAKE} init.el

clean:
	$(RM) init.el
	$(RM) *.elc
	$(RM) */*.elc

# Use function defiled in scripts/makefile-script.el to build the init file.
init.el:
	${EMACS} -Q --script "scripts/makefile-script.el" -f make-init-el

compile: init.el
	${EMACS} -Q --script "scripts/makefile-script.el" -f byte-compile-configs

doc: init.el
	${EMACS} -Q -l init.el \
			--script "assets/export-script.el" -f generate-doc-files

.PHONY: all bootstrap init.el compile doc
