;;; vim-cscope.el --- 

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: vim-cscope.el,v 0.0 2010/09/01 10:58:55 coldnew Exp $
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

(provide 'vim-cscope)
(eval-when-compile
  (require 'cl))

(require 'vim)
(require 'xcscope)

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(vim:nmap (kbd "C-\\ s") 'cscope-find-this-symbol)
(vim:nmap (kbd "C-\\ g") 'cscope-find-global-definition)
(vim:nmap (kbd "C-\\ c") 'cscope-find-called-functions)
(vim:nmap (kbd "C-\\ t") 'cscope-find-this-text-string)
(vim:nmap (kbd "C-\\ e") 'cscope-find-egrep-pattern)
(vim:nmap (kbd "C-\\ f") 'cscope-find-this-file)
(vim:nmap (kbd "C-\\ i") 'cscope-find-files-including-file)
(vim:nmap (kbd "C-\\ d") 'cscope-find-called-functions)


;;; vim-cscope.el ends here
