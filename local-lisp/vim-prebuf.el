;;; vim-prebuf.el --- Switch to previous buffer

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords: buffer, switch buffer
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/vim-prebuf.el
(defconst vim-prebuf-version "0.1")

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
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'vim-prebuf)

;;; Code:

(eval-when-compile (require 'cl))


(defvar vim:prebuf-ignore-list nil
  "")

(defvar vim:prebuf-last-buf "*scratch*"
  "This variable save previous buffer, default is *scratch*.")

(defun vim:cmd-prebuf-switch ()
  "switch to previous active buffer"
  (interactive)
  (if (not (boundp 'vim:prebuf-last-buf))
      (message "No previous buffer to switch"))
  (let ((buf (get-buffer vim:prebuf-last-buf)))
    (if (not buf)
	(message "Invalid buffer")
      (switch-to-buffer buf))))

(defadvice switch-to-buffer (before vim:prebuff-add-buf act)
  ""
  (setq vim:prebuf-last-buf (buffer-name)))



(vim:nmap (kbd "gb") 'vim:cmd-prebuf-switch)



(provide 'vim-prebuf)
;; vim-prebuf.el ends here.
