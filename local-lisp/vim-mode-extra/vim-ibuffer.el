;;; vim-buffer.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: vim-ibuffer.el,v 0.0 2010/09/01 10:58:55 coldnew Exp $
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
;;   (require 'vim-cscope)

;;; Code:

(provide 'vim-ibuffer)
(eval-when-compile
  (require 'cl))

(require 'ibuffer)
(require 'vim)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (vim:local-imap (kbd "i") nil) ; it's no need to got to insert-mode in ibuffer
	    (vim:local-nmap (kbd "l") 'nil)
	    (vim:local-nmap (kbd "h") nil)
	    (vim:local-nmap (kbd "j") 'next-line)
	    (vim:local-nmap (kbd "p") 'previous-line)
	    (vim:local-nmap (kbd "q") 'quit-window)
	    ))












;;; vim-ibuffer.el ends here
