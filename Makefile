EMACS ?= emacs

# Detect native-compile availability
NATIVE_AVAILABLE := $(shell ${EMACS} --batch --eval "(prin1 (fboundp 'native-compile))" 2>/dev/null)

# Use native compile by default if available (Emacs 28+)
ifeq ($(NATIVE_AVAILABLE),t)
COMPILE_TARGET := native-compile
else
COMPILE_TARGET := byte-compile
endif

all: $(COMPILE_TARGET)

# Direct elisp workflow - early-init.el and init.el are now the source of truth
# No org-mode generation needed anymore

test: clean
	${MAKE} all

# Byte-compile elisp files
byte-compile:
	${EMACS} -Q -l early-init.el -l init.el --batch \
		--eval "(progn (byte-compile-file \"early-init.el\") (byte-compile-file \"init.el\"))"

# Native compile elisp files (Emacs 28+)
# Uses synchronous native-compile to ensure files are generated
native-compile:
	${EMACS} -Q -l early-init.el -l init.el --batch \
		--eval "(progn (require 'native-compile) (native-compile \"early-init.el\") (native-compile \"init.el\"))"

# Clean compiled files
clean:
	$(RM) *.elc
	$(RM) */*.elc
	$(RM) */*/*.elc
	$(RM) -rf eln-cache
	$(RM) -rf .eln-cache

# Verify outshine structure
verify-outshine:
	@echo "=== Outshine Heading Statistics ==="
	@echo "init.el H1 headings: $$(grep -c '^;; \* ' init.el || echo 0)"
	@echo "init.el H2 headings: $$(grep -c '^;; \*\* ' init.el || echo 0)"
	@echo "early-init.el H1 headings: $$(grep -c '^;; \* ' early-init.el || echo 0)"
	@echo ""
	@echo "Files are ready for outshine-mode navigation."
	@echo "Use: M-x outline-minor-mode or M-x outshine-mode"

.PHONY: all test byte-compile native-compile clean verify-outshine
