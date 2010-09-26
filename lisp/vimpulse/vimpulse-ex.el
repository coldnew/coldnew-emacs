;;;; Ex commands

(require 'vimpulse-dependencies) ; ex-token-alist, v-want-quit-like-Vim

(defvar vimpulse-extra-ex-commands
  '(("b" "buffer")
    ("bdelete" (vimpulse-kill-current-buffer))
    ("bnext" "next")
    ("clo" "close")
    ("close" (delete-window))
    ("on" "only")
    ("only" (delete-other-windows))
    ("split" (split-window))
    ("syntax" (global-font-lock-mode))
    ;; Emacs and Vim use inverted naming conventions for splits
    ("vsplit" (split-window-horizontally)))
  "Extra Ex commands, added to `ex-token-alist' when Vimpulse loads.")

(when vimpulse-want-quit-like-Vim
  (add-to-list 'vimpulse-extra-ex-commands
               '("quit" (save-buffers-kill-emacs))))

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

(dolist (entry vimpulse-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (push entry ex-token-alist))

(provide 'vimpulse-ex)
