EMACS ?= emacs

all: init.el byte-compile native-compile

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

byte-compile: init.el
	${EMACS} -Q --script "scripts/makefile-script.el" -f byte-compile-init-el

native-compile: init.el
	${EMACS} -Q --script "scripts/makefile-script.el" -f native-compile-init-el

doc: init.el
	${EMACS} -Q -l init.el \
			--script "assets/export-script.el" -f generate-doc-files

.PHONY: all bootstrap init.el compile doc
