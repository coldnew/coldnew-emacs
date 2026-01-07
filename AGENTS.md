# AGENTS.md - Guide for Coding Agents

## Build Commands

- `make` or `make all` - Generate init.el from init.org and compile
- `make init.el` - Generate init.el from org-mode files only
- `make compile` - Compile all .el files (byte-compile or native-compile)
- `make byte-compile` - Force byte-compilation
- `make native-compile` - Force native compilation (Emacs 28+)
- `make clean` - Remove generated files (*.elc, init.el)
- `make test` - Clean and rebuild configuration

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
- Use `use-package` for complex package configurations in init.org

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

### Org-mode Configuration
- Use `:tangle yes` for code blocks that should generate .el files
- Organize with proper heading structure (* ** ***)
- Use `:noweb yes` for code reuse between blocks
- Include `:results silent` for side-effect code blocks