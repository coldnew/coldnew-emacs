;;; elscreen-restore.el --- Restore elscreen buffers and windows

;; Copyright 2011 Yen-Chin,Lee
;;
;; Author: coldnew coldnew.tw@gmail.com
;; Keywords: elscreen, restore, windows,
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/elscreen-restore.el
(defconst elscreen-restore-version "0.1")

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

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'elscreen-restore)

;;; Code:

(eval-when-compile (require 'cl))

(require 'elscreen)
(require 'revive)


(defvar elscreen-configuration-file "~/.emacs.d/.elscreen-conf"
  "file to save elscreen windows configuration.")

(defvar elscreen-restore-config-loaded nil
  "flag if config loaded or not")


(defun elscreen-restore-save-layout ()
  "save the frame and window layout to elscreen-configuration-file"
  (let ((frames (frame-list))
	))
  )








(provide 'elscreen-restore)
;; elscreen-restore.el ends here.
