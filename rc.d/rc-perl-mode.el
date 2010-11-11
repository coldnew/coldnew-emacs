;; init for perl mode

;; use cperl-mode instead of perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (setq cperl-indent-level 8)
	     (setq cperl-continued-statement-offset 0)
	     (setq cperl-extra-newline-before-brace t)
	     ;; hook for cpp-mode
	     (programming-common-hook)	; programming common hook
	     (insert-equal-char-smart)  ; insert `=' more easily
	     ))
