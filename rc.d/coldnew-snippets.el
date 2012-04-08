(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)

;;;;;;;; Loding libraries
(require 'yasnippet)
(require 'dropdown-list)

;; Set my snippet-dirs, do not load original yasnippet-dir
(setq-default yas/snippet-dirs emacs-snippets-dir)
(yas/initialize)
(yas/load-directory emacs-snippets-dir)

(setq yas/prompt-functions
      '(yas/dropdown-prompt
    yas/ido-prompt
    yas/completing-prompt))

;; ;; TODO: ???
;; (setq yas/buffer-local-condition
;;	'(or (not (or (string= "font-lock-comment-face"
;;                 (get-char-property (point) 'face))
;;            (string= "font-lock-string-face"
;;                 (get-char-property (point) 'face))))
;;       '(require-snippet-condition . force-in-comment)))


;; TODO: After finish, move this two line to lang-snippet
(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yas\\'" . snippet-mode))

;; FIXME: How to remove the annoying message `progn: end of buffer'. ?
;; Auto add HEADER in new file
(add-hook 'find-file-hook
      '(lambda ()
         (when (and (buffer-file-name)
            (not (file-exists-p (buffer-file-name)))
            (= (point-max) 1))
           (let ((header-snippet "HEADER"))
         (insert header-snippet)
         ;; if can't expand snippet, delete insert string
         (if (not (yas/expand))
             (backward-delete-char (1+ (length header-snippet))))))))


;;;; Functions
(defun yas/dir ()
  (file-name-directory (buffer-file-name)))
(defun yas/file ()
  (file-name-nondirectory (buffer-file-name)))
(defun yas/file-sans ()
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-ext ()
  (file-name-extension (file-name-nondirectory (buffer-file-name))))
(defun yas/file-sans-upcase ()
  (upcase (yas/file-sans)))
(defun yas/year ()
  (format-time-string "%Y"))
(defun yas/user-name ()
  (insert user-full-name))
(defun yas/login-name ()
  (insert user-login-name))
(defun yas/user-email ()
  (insert user-mail-address))
(defun yas/user-nickname ()
  (insert user-nickname))

;; ;;; yasnippet展開中はflymakeを無効にする
;; (defvar flymake-is-active-flag nil)
;; (defadvice yas/expand-snippet
;;   (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
;;   (setq flymake-is-active-flag
;;         (or flymake-is-active-flag
;;             (assoc-default 'flymake-mode (buffer-local-variables))))
;;   (when flymake-is-active-flag
;;     (flymake-mode-off)))
;; (add-hook 'yas/after-exit-snippet-hook
;;           '(lambda ()
;;              (when flymake-is-active-flag
;;                (flymake-mode-on)
;;                (setq flymake-is-active-flag nil))))


(provide 'coldnew-snippets)
;; coldnew-snippets.el ends here.
