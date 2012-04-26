;;; evil-paredit.el --- paredit-mode for evil

;; Copyright 2012 Yen-Chin,Lee
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords:
;; X-URL:
(defconst evil-paredit-version "0.1")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Display relative line numbers for the current buffer.
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;; (require 'evil-paredit)

;;; Code:

(eval-when-compile (require 'cl))
(require 'paredit)
(require 'evil)

(define-minor-mode evil-paredit-mode
  "Buffer-local minor mode to emulate paredit.vim"
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

(defun turn-on-evil-paredit-mode ()
  "Enable evil-paredit-mode in the current buffer."
  (evil-paredit-mode 1))

(defun turn-off-evil-paredit-mode ()
  "Disable evil-paredit-mode in the current buffer."
  (evil-paredit-mode -1))

(define-globalized-minor-mode global-evil-paredit-mode
  evil-paredit-mode turn-evil-paredit-mode
  "Global minor mode to emulate paredit.vim")

;;;; keybinding
;; insert
(evil-define-key 'insert evil-paredit-mode-map "(" 'paredit-open-round)
(evil-define-key 'insert evil-paredit-mode-map ")" 'paredit-close-round)
(evil-define-key 'insert evil-paredit-mode-map "[" 'paredit-open-square)
(evil-define-key 'insert evil-paredit-mode-map "]" 'paredit-close-square)
(evil-define-key 'insert evil-paredit-mode-map "{" 'paredit-open-curly)
(evil-define-key 'insert evil-paredit-mode-map "}" 'paredit-close-curly)
(evil-define-key 'insert use-paredit-mode-map (kbd "\"")  'paredit-doublequote)
(evil-define-key 'insert evil-paredit-mode-map (kbd "<delete>") 'paredit-forward-delete)
(evil-define-key 'insert evil-paredit-mode-map (kbd "<backspace>") 'paredit-backward-delete)

;; normal


(provide 'evil-paredit)
;; evil-paredit.el ends here.
