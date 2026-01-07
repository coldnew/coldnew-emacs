;; early-init.el --- -*- lexical-binding: t; -*-
(with-eval-after-load 'comp
  (setq-default comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))
  ;; This variable is obsolete since 29.1
  (setq-default native-comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))
  ;; introduce in emacs  28.1
  (setq-default native-comp-jit-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)")))
(with-eval-after-load 'comp
  (setq native-comp-async-report-warnings-errors 'silent))
(setq gc-cons-threshold most-positive-fixnum)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq package-enable-at-startup nil)
(when (>= emacs-major-version 27)
  (setq-default package-quickstart t))
(setq frame-inhibit-implied-resize t)
(add-to-list 'warning-suppress-types '(org-element org-element-parser))
(provide 'early-init)
;;; early-init.el ends here
