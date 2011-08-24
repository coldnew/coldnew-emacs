;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'verilog-mode)

;;;;;;;; verilog-mode extensions
(add-to-list 'auto-mode-alist '("\\.v$" . verilog-mode))

;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (require* 'auto-complete-clang)
  ;; Setting my c-mode auto-complete source
  (defun ac-verilog-mode-setup ()
    "auto-complete settings for verilog-mode."
    (setq ac-sources '(ac-source-dictionary
		       ac-source-abbrev
		       ac-source-filename
		       ac-source-files-in-current-dir
		       ac-source-words-in-same-mode-buffers
		       ))
    ))

;;;;;;;; Coding-Style Setting
(add-hook 'verilog-mode-hook
	  '(lambda ()
	     (setq verilog-indent-level             8)
	     (setq verilog-indent-level-module      8)
	     (setq verilog-indent-level-declaration 8)
	     (setq verilog-indent-level-behavioral  8)
	     (setq verilog-indent-level-directive   1)
	     (setq verilog-case-indent              8)
	     (setq verilog-auto-newline             nil)
	     (setq verilog-auto-indent-on-newline   nil)
	     (setq verilog-tab-always-indent        t)
	     (setq verilog-auto-endcomments         t)
	     (setq verilog-minimum-comment-distance 40)
	     (setq verilog-indent-begin-after-if    t)
	     (setq verilog-auto-lineup              '(all))
	     ))

;;;;;;;; Hooks
(add-hook 'verilog-mode-hook
	  '(lambda ()

	     ;; Enable Auto Complete
	     (when (require* 'auto-complete)
	       (ac-verilog-mode-setup))

	     ;; Use global programming mode
	     (programming-mode)

	     ;; Use paredit in elisp
	     (use-paredit-mode)

	     ))

;;;;;;;; Keybindings
(add-hook 'verilog-mode-hook
	  '(lambda ()

	     (vim:local-nmap (kbd "C-x C-s") 'verilog-mode:save-buffer-always)
	     (vim:local-imap (kbd "C-x C-s") 'verilog-mode:save-buffer-always)

	     (vim:local-imap (kbd "M-[") 'verilog-insert-block)
	     (vim:local-imap (kbd "M-a") 'verilog-sk-always)
	     (vim:local-imap (kbd "M-c") 'verilog-sk-case)
	     ))


;;;;;;;; Function
(defun verilog-mode:save-buffer-always ()
  "Expand AUTO staments and save buffer."
  (interactive)
  (verilog-auto)
  (save-buffer-always))




(provide 'coldnew-lang-verilog)
;; coldnew-lang-verilog.el ends here.
