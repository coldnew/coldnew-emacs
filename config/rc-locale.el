;;; rc-locale.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: rc-locale.el,v 0.0 2010/08/13 17:17:53 coldnew Exp $
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
;;   (require 'rc-locale)

;;; Code:

(provide 'rc-locale)
(eval-when-compile
    (require 'cl))


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(message "* --[ loading rc-locale.el ]-- *")

(prefer-coding-system          'utf-8 ) ; 設定系統編碼
(set-language-environment      'utf-8 ) ; 設定語言環境
(set-buffer-file-coding-system 'utf-8 ) ; 文件保存時的編碼設置
(set-keyboard-coding-system    'utf-8 ) ; 鍵盤編碼設定
(set-terminal-coding-system    'utf-8 ) ; 設定終端機的編碼
(set-buffer-file-coding-system 'utf-8 ) ; buffer內文字的編碼
(set-selection-coding-system   'utf-8 ) ; 選擇區域內編碼
(set-clipboard-coding-system   'utf-8 ) ; 剪貼簿編碼設定
(set-file-name-coding-system   'utf-8 ) ; 使用 utf-8 編碼顯示文件名
(setq system-time-locale      "en_US" ) ; 設定時間顯示使用英文


;;; rc-locale.el ends here
