EMACS ?= emacs
CASK ?= cask

all: compile

test: clean
	${MAKE} all

bootstrap:
	${MAKE} clean
	${MAKE} init.el
	${CASK} install

Cask:
	${RM} Cask
	${MAKE} init.el

clean:
	$(RM) init.el Cask
	$(RM) *.elc
	$(RM) */*.elc

# Use emacs to generate init.el from init.org
# we first tangle the org-mode file to init.el~ the rename it.
# this can make use use async task to create another init.el after save.
init.el:
	${EMACS} -Q --script "scripts/makefile-script.el" -f make-init-el

compile: init.el
	${CASK} exec ${EMACS} -Q -batch \
		--eval '(setq byte-compile-error-on-warn nil)' \
		--eval '(byte-recompile-directory (expand-file-name (getenv "PWD")) 0)'

doc: init.el
	${CASK} exec ${EMACS} -Q -l init.el \
		--script "assets/export-script.el" -f generate-doc-files

.PHONY: all bootstrap init.el compile doc
