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

(defun my/set-key (keymap key def &rest bindings)
  (evil-leader--def-keys keymap key def bindings))

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
