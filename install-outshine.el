;;; install-outshine.el --- Install outshine package for coldnew's Emacs
;;; Commentary:
;;; This script installs outshine package which provides better navigation
;;; for Emacs configuration files using outshine-style headings.

;;; Code:

(require 'package)

;; Add MELPA if not already present
(unless (package-installed-p 'outshine)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

;; Refresh package list
(package-refresh-contents)

;; Install outshine
(unless (package-installed-p 'outshine)
  (package-install 'outshine))

(provide 'install-outshine)
;;; install-outshine.el ends here