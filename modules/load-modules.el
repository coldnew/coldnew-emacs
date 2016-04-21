;;; Add all modules to path
;; This file will be load in my emacs config.


;;;; Spacemacs
;;; https://github.com/syl20bnr/spacemacs

;; Latest spacemacs can setup `SPACEMACSDIR' to load customize spacemacs init.el file.
(setenv "SPACEMACSDIR" (concat user-emacs-directory "spacemacs.d"))

;; Make spacemacs not remove my packages.
(defadvice configuration-layer/delete-orphan-packages (around null-func activate)
  "Overwrite the spacemacs's `configuration-layer/delete-orphan-packages'
  to make it not remove any orphan packages.")

;; Spacemacs no need to check newer version
(defadvice spacemacs/check-for-new-version (around null-func activate)
  "Overwrite the spacemacs's `spacemacs/check-for-new-version' to
  makt it useless since I use git submodule to bundle spacemacs with my emacs.")

(require 'f)
(let* ((spacemacs-dir
	(directory-file-name (f-join user-emacs-directory "modules" "spacemacs")))
       (spacemacs-init
	(concat (file-name-as-directory spacemacs-dir) "init.el"))
       (user-emacs-directory (file-name-directory spacemacs-init)))
  ;; Initial spacemacs, our emacs run on top of it
  (load spacemacs-init))
