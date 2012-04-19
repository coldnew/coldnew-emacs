;;; coldnew-session.el ---
(eval-when-compile (require 'cl))


;;;; ---------------------------------------------------------------------------
;;;; savehist
;;;; ---------------------------------------------------------------------------
(require 'savehist)
;; keep minibuffer history between session
(setq savehist-file (concat emacs-cache-dir "savehist.dat"))
(savehist-mode 1)

;;;; ---------------------------------------------------------------------------
;;;; saveplace
;;;; ---------------------------------------------------------------------------
(require 'saveplace)
(setq save-place-file (concat emacs-cache-dir "saveplace.dat"))
(setq-default save-place t)

;;;; ---------------------------------------------------------------------------
;;;; recentf
;;;; ---------------------------------------------------------------------------
(require 'recentf)
;; Setting cache file for recentf
(setq recentf-save-file (concat emacs-cache-dir "recentf"))
;; Following file won;t contain in recentf
(setq recentf-exclude '("\\.elc$" "\\.pyc$" "\\.recentd$" "^/tmp/"))



(provide 'coldnew-session)
;; coldnew-session.el ends here.
