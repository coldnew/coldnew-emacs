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

(defun my/set-key (keymap key def &rest bindings)
  (evil-leader--def-keys keymap key def bindings))

;; evil
(defun coldnew-editor/init-evil ()
  "Initialize my evil mode"

  ;; ;;; Copy all default emacs keymap to insert-mode
  ;; ;; Vim's insert mode is useless, copy emacs's config
  ;; ;; to it
  ;; (setcdr evil-insert-state-map nil)
  ;; (define-key evil-insert-state-map
  ;;   (read-kbd-macro evil-toggle-key) 'evil-emacs-state)

  ;; Bind my familier emacs keybinding
  (my/set-key evil-insert-state-map
              "C-a" 'beginning-of-line
              "C-e" 'end-of-line
              "C-o" 'evil-execute-in-normal-state
              "C-d" 'hungry-delete-forward
              "C-l" 'hungry-delete-backward
              "C-/" 'undo-tree-undo
              "M-/" 'undo-tree-redo
              "C-n" 'evil-next-line
              "C-p" 'evil-previous-line
              "C-f" 'evil-forward-char
              "C-b" 'evil-backward-char
              )
  )


;; For each package, define a function coldnew-editor/init-<package-coldnew-editor>
;;
;; (defun coldnew-editor/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
