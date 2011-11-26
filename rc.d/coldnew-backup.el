;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

(setq-default auto-save-interval         50 )   ; 擊鍵50次就保存
(setq-default auto-save-timeout          30 )   ; 空閒30秒就保存
(setq-default auto-save-visited-file-name t )   ; 當前buffer關聯一個已存在的文件名時才保存
(setq-default delete-by-moving-to-trash nil )   ; 刪除的檔案不移至系統的垃圾桶
(setq-default delete-auto-save-files      t )   ; 成功儲存後移除自動儲存的檔案
(setq-default auto-save-list-file-name    t )
(setq-default auto-save-default           t )

;; ;;;; 檔案備份設定
;; 如果備份用的資料夾不存在就建立他
(if (not (file-exists-p emacs-backup-dir))
    (make-directory emacs-backup-dir t))

;; 將文件備份至備份用資料夾
(setq-default backup-directory-alist `(("."  . ,emacs-backup-dir)))
(setq-default version-control     t ) ; 啟用版本控制功能（可備份多次）
(setq-default backup-by-copying   t ) ; 使用直接拷貝來備份檔案
(setq-default kept-old-versions   5 ) ; 備份最原始的版本5次
(setq-default kept-new-versions   19) ; 備份最新的版本12次
(setq-default delete-old-versions t ) ; 刪除不屬於以上24種版本的備份


;; I don't like auto-save-list directory show upder ~/.emacs.d/
;; move it to ~/.emacs.d/var/auto-save-list/
(defvar emacs-auto-save-list-directory "~/.emacs.d/var/auto-save-list/")
(make-directory emacs-auto-save-list-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,emacs-auto-save-list-directory)
	(,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat emacs-auto-save-list-directory "auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-auto-save-list-directory t)))

;;;;;;;; Backup kill-ring
(when (require* 'savekill)
  (setq save-kill-file-name (concat emacs-cache-dir "kill-ring.cache")))



(provide 'coldnew-backup)
;; coldnew-backup.el ends here.
