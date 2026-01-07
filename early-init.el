;;; early-init.el --- Emacs Early Initialization Configuration -*- lexical-binding: t -*-

;; Copyright (C) 2010-2026 Yen-Chin, Lee <coldnew.tw@gmail.com>

;;; Commentary:

;; This file contains early initialization code that runs before Emacs
;; starts up.  Since Emacs 27, we can add setup here.
;;
;; Outshine headings are supported - use org-mode navigation keys.
;; Press C-c @ to toggle outline-minor-mode for navigation or use outshine-mode directly.
;;

;; This file is NOT part of GNU Emacs.

;;; Code:

;; * Disable native-compilation for some packages
;;
;;   Adding regexes to `comp-deferred-compilation-deny-list' to make
;;   some packages not build by native-compilation when enabled.
;;
;;   Here I just disable native-compile auto loads.  There's something
;;   wrong with them.

(with-eval-after-load 'comp
  (setq-default comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))
  ;; This variable is obsolete since 29.1
  (setq-default native-comp-deferred-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)"))
  ;; introduce in emacs  28.1
  (setq-default native-comp-jit-compilation-deny-list '("\\(?:[^z-a]*-autoloads\\.el$\\)")))

;; * Prevent native-compilation warning window popup
;;
;;   native-compilation will async report warning or error info, it's
;;   so annoying, set it to silent mode so these warning will just
;;   logging but not pop up the window.

(with-eval-after-load 'comp
  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors 'silent)))

;; * Defer garbage collection further back in the startup process

(setq gc-cons-threshold most-positive-fixnum)

;; * Prevent the glimpse of un-styled Emacs by disabling these UI elements early.

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; * Prevent package.el to load packages prior to init.el loading
;;
;;   We plan to handle our package initialization in our configs, we
;;   must prevent Emacs from doing it at early stage.

(setq package-enable-at-startup nil)

;; * Allow loading from the package cache

(when (>= emacs-major-version 27)
  (setq-default package-quickstart t))

;; * Do not resize the frame at this early stage.

(setq frame-inhibit-implied-resize t)

;; * Drop 'org-element-at-point' cannot be used in non-Org buffer warning
;;
;;   Not sure why always shows ='org-element-at-point' cannot be used
;;   in non-Org buffer= in *Warning* buffer, follow below link to fix
;;   problem
;;
;;   https://github.com/syl20bnr/spacemacs/issues/16575

(add-to-list 'warning-suppress-types '(org-element org-element-parser))

;; * Prevent load outdated .elc files
;;
;;   Since emacs 24.4, new option =load-prefer-newer= has been
;;   introduce, which make me never accidentally using outdated
;;   compiled files.

(setq load-prefer-newer t)


(provide 'early-init)
;;; early-init.el ends here
