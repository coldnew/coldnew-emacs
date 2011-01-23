;;; rc-backup.el ---

;; Copyright 2010 Yen-Chin,Lee
;;
;; Author: Yen-Chin,Lee <coldnew.tw@gmail.com>
;; Version: $Id: rc-backup.el,v 0.0 2010/08/07 02:52:40 coldnew Exp $
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
;;   (require 'rc-backup)

;;; Code:

(provide 'rc-backup)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(message "* --[ loading rc-backup.el ]-- *")

(setq-default auto-save-interval         50 )   ; 擊鍵50次就保存
(setq-default auto-save-timeout          30 )   ; 空閒30秒就保存
(setq-default auto-save-visited-file-name t )   ; 當前buffer關聯一個已存在的文件名時才保存
(setq-default delete-by-moving-to-trash nil )   ; 刪除的檔案不移至系統的垃圾桶
(setq-default delete-auto-save-files      t )   ; 成功儲存後移除自動儲存的檔案

;;;; 檔案備份設定
(defvar emacs-backup-directory "~/.emacs.d/var/backups/") ; 設定備份用資料夾位置
;; 如果備份用的資料夾不存在就建立他
(if (not (file-exists-p emacs-backup-directory))
  (make-directory emacs-backup-directory t))

;; 將文件備份至備份用資料夾
(setq-default backup-directory-alist `(("."  . ,emacs-backup-directory)))
(setq-default version-control     t ) ; 啟用版本控制功能（可備份多次）
(setq-default backup-by-copying   t ) ; 使用直接拷貝來備份檔案
(setq-default kept-old-versions   3 ) ; 備份最原始的版本3次
(setq-default kept-new-versions   12) ; 備份最新的版本12次
(setq-default delete-old-versions t ) ; 刪除不屬於以上15種版本的備份


;;; rc-backup.el ends here
