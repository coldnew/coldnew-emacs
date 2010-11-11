;;; rc-fonts.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: rc-fonts.el,v 0.0 2010/08/10 11:34:37 coldnew Exp $
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
;;   (require 'rc-fonts)

;;; Code:

(provide 'rc-fonts)
(eval-when-compile
    (require 'cl))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(message "* --[ loading rc-fonts.el ]-- *")


;;;; 建立新的 fontset
(create-fontset-from-fontset-spec
    "-*-liberation mono-medium-r-*--14-*-*-*-*-*-fontset-coldnew")

;;;; 設定其他編碼的字型
(set-fontset-font "fontset-coldnew"     ; 中文字體
    'han (font-spec :family "LiHei Pro" :size 16))

(set-fontset-font "fontset-coldnew"     ; 符號
    'symbol (font-spec :family "Monaco" :size 20 ))

;;; 使用自己建立的 fontset
(set-frame-font "fontset-coldnew")

;;;; 讓新開的 frame 使用特定的fontset
(add-to-list 'default-frame-alist '(font . "fontset-coldnew"))

;;;; 字型顯示樣本
(setq-default list-faces-sample-text
    "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 11223344556677889900
     ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz 壹貳參肆伍陸柒捌玖零")



;;; rc-fonts.el ends here



