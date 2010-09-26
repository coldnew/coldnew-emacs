;;; vim-popup-kill-ring.el --- 

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: vim-popup-kill-ring.el,v 0.0 2010/08/31 13:20:21 coldnew Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'vim-popup-kill-ring)

;;; Code:

(provide 'vim-popup-kill-ring)
(eval-when-compile
  (require 'cl))


(require 'vim-modes)
(require 'popup-kill-ring)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(vim:defcmd vim:popup-kill-ring (nonrepeatable)
  "Interactively insert selected item from `key-ring' by `popup.el'
and `pos-tip.el"
  (popup-kill-ring))


;; keybinding
(vim:imap (kbd "M-p") 'vim:popup-kill-ring) 
;(local-set-key popup-kill-ring-keymap (kbd "<TAB>") 'popup-kill-ring-select)

(setq popup-kill-ring-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap "\r" 'popup-kill-ring-select)
    (define-key keymap "\t" 'popup-kill-ring-select)
    (define-key keymap "\C-n" 'popup-kill-ring-next)
    (define-key keymap "\C-p" 'popup-kill-ring-previous)
    (define-key keymap "\C-f" 'popup-kill-ring-current)
    (define-key keymap "\C-b" 'popup-kill-ring-hide)
    keymap))


;;; vim-popup-kill-ring.el ends here
