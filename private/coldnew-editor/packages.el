;;; coldnew-editor.el --- coldnew-editor Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;; Copyright (c) 2015 Yen-Chin, Lee
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; URL: https://github.com/coldnew/coldnew-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar coldnew-editor-packages
  '(
    ;; package coldnew-editors go here
    evil
    evil-leader
    hungry-delete
    undo-tree
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar coldnew-editor-excluded-packages '()
  "List of packages to exclude.")

;;; hungry-delete
(defun coldnew-editor/init-hungry-delete ()
  "Initialize hungry-delete"
  (use-package hungry-delete
               :defer t
               :init (global-hungry-delete-mode)
               :config
               (progn
                 ;; only horizontal whitespace
                 (setq-default hungry-delete-chars-to-skip " \t\f\v"))))

;; For each package, define a function coldnew-editor/init-<package-coldnew-editor>
;;
;; (defun coldnew-editor/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
