;;; plurk-mode.el --- Major mode for plurk

;; Copyright 2011
;;
;; Author: [yas] elisp error! Symbol's value as variable is void: user-nickname coldnew@Fevia
;; Keywords: plurk web
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/plurk-mode.el
(defconst plurk-mode-version "0.1")

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
;; plurk-mode.el is a major mode for Plurk.
;; You can check friends timeline, and update your status on Emacs.

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'plurk-mode)

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'parse-time)









(provide 'plurk-mode)
;; plurk-mode.el ends here.
