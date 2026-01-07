# AGENTS.md - Guide for Coding Agents

## Build Commands

- `make` or `make all` - Compile all .el files (uses native-compile if available, Emacs 28+)
- `make compile` - Compile all .el files (byte-compile or native-compile)
- `make byte-compile` - Force byte-compilation
- `make native-compile` - Force native compilation (Emacs 28+)
- `make clean` - Remove generated files (*.elc, *.eln-cache)
- `make test` - Clean and rebuild configuration
- `make verify-outshine` - Show outshine heading statistics

## Test Commands

- `emacs -batch -l ert -f ert-run-tests-batch-and-exit` - Run all ERT tests
- `emacs -batch -l FILE.el -l ert -f ert-run-tests-batch-and-exit` - Run single test file
- `make -C modules/emacs/test check` - Run Emacs test suite (if available)

## Code Style Guidelines

### Emacs Lisp
- Use `lexical-binding: t` in file headers
- Follow standard Emacs Lisp naming: `package-function-name`, `package--private-function`
- Use `cl-lib` functions instead of deprecated `cl` functions
- Include proper file headers with Commentary and Code sections
- Use `defvar` for configuration variables, `defcustom` for user-customizable options

### Imports/Requires
- Group requires at top of file: built-ins first, then third-party packages
- Use `require` with feature symbols, not file paths
- Use `use-package` for package configurations

### Formatting
- 2-space indentation for Emacs Lisp
- Line length ~80 characters (soft limit)
- Use `electric-pair-mode` for balanced parentheses
- Format function calls: `(func arg1 arg2)` on single line when reasonable

### Error Handling
- Use `condition-case` for error handling
- Provide meaningful error messages with `user-error` for user mistakes
- Use `ignore-errors` for non-critical operations

### Documentation
- Document all public functions with docstrings
- Use `;;;###autoload` for interactive commands
- Include Commentary section explaining file purpose
- Use proper function signatures with argument types in docstrings

### Outshine Format (init.el)
- Use outshine headings instead of org-mode tangling: `;; * Section`, `;; ** Subsection`
- Navigation: `C-c @` to toggle outline-minor-mode, `C-c n/p/f/b` to navigate
- All configuration is in init.el directly (no org-mode tangling)

## Architecture

### Package Management
- Uses built-in `package.el` with MELPA/GNU ELPA
- Use `:ensure t` in use-package for package installation
- No straight.el - removed for simplicity
- Global setting: `(setq use-package-always-defer t)` - packages load lazily by default
- Use `:demand t` for packages needed at startup (evil, helm→vertico, which-key, etc.)

### Completion Framework (Vertico Stack)
- **vertico** - Vertical completion UI
- **consult** - Useful commands (consult-find, consult-buffer, consult-line, etc.)
- **marginalia** - Rich annotations for completions
- **orderless** - Flexible completion style (space-separated components)

### Keybindings
- All keybindings are **centralized** in the "Centralized Keybindings" section at end of init.el
- Organized by category:
  - Global Keybindings (non-evil)
  - Minibuffer Keybindings
  - Evil Normal State Keybindings
  - Evil Insert State Keybindings
  - Evil Ex Commands
  - Major Mode Specific Keybindings (org-mode, markdown, c-mode, etc.)
- Do NOT scatter keybindings throughout package configurations - put them in the centralized section

### Configuration Structure
- `early-init.el` - Early startup settings (before package initialization)
- `init.el` - Main configuration with outshine headings
- `modules/load-modules.el` - Load additional modules
- `.personal.el` - Local personal settings (loaded at end)

### Language Server Support
- Various language modes configured (go-mode, rust-mode, python, etc.)
- Check existing patterns in init.el before adding new language configurations