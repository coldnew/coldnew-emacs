
(defun cc-mode-common-hook ()
  "The meta functions fo cc-mode, like c-mode c++-mode and objc-mode."
  (high-light-functions)
  ;;;; Keybindings
  ;; Insert smart char
  (vim:local-imap (kbd "=")   'cc-mode:insert-equal)
  (vim:local-imap (kbd ".")   'cc-mode:insert-pointer)
  (vim:local-imap (kbd ">")   'cc-mode:insert-greater-or-shift)
  (vim:local-imap (kbd "<")   'cc-mode:insert-lesser-or-shift)
  )

                    ;(font-lock-add-keywords 'nil
                    ;           '(("\\(\\w+\\)\\s-*\("
                    ;             (1 rumpsteak-font-lock-function-call-face))) t)


                    ;(defun rumpsteak-match-function-call (&optional limit)
                    ; (while (and (search-forward-regexp "\\(\\w+\\)\\s-*\(" limit 'no-error)
                    ;            (not (save-match-data
                    ;                  (string-match c-keywords-regexp (match-string 1))))
                    ;          (not (save-excursion
                    ;                (backward-char)
                    ;               (forward-sexp)
                    ;              (c-skip-whitespace-forward)
                    ;             (or (eobp) (= ?\{ (char-after (point)))))))))
                    ;(font-lock-add-keywords 'c-mode
                    ;           '((rumpsteak-match-function-call
                    ;             (1 rumpsteak-font-lock-function-call-face))))


;;;; Functions
(defun high-light-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((high-light-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


(defun high-light-functions ()
  "hightlight functions if it's regexp match func() "
  (font-lock-add-keywords 'nil
              '(("\\<\\([a-zA-Z_]*\\) *("  1 font-lock-keyword-face))))

;; Insert char smart
(defcmd cc-mode:insert-equal ()
  "insert eaual with extra space."
  (if (eq this-command real-last-command)
      (cond ((in-string-p) (insert "="))
        ((search-backward " = "  nil t) (delete-char 3) (insert " == "))
        ((search-backward " == " nil t) (delete-char 4) (insert " = "))
        (t (insert " = ")))
    (insert " = ")))

(defcmd cc-mode:insert-pointer ()
  "insert . or -> if not in string."
  (if (eq this-command real-last-command)
      (cond ((in-string-p) (insert "."))
        ((search-backward "->" nil t) (delete-char 2) (insert "."))
        ((search-backward "."  nil t) (delete-char 1) (insert "->"))
        (t (insert ".")))
    (insert ".")))

(defcmd cc-mode:insert-greater-or-shift ()
  "insert > or >> if not in string."
  (if (eq this-command real-last-command)
      (cond ((in-string-p) (insert ">"))
        ((search-backward ">"   nil t) (delete-char 1) (insert ">>"))
        ((search-backward ">>"  nil t) (delete-char 2) (insert ">"))
        (t (insert ">")))
    (insert ">")))

(defcmd cc-mode:insert-lesser-or-shift ()
  "insert < or << if not in string."
  (if (eq this-command real-last-command)
      (cond ((in-string-p) (insert "<"))
        ((search-backward "<"   nil t) (delete-char 1) (insert "<<"))
        ((search-backward "<<"  nil t) (delete-char 2) (insert "<"))
        (t (insert "<")))
    (insert "<")))


(provide 'rc-ccmode-common)
;; rc-ccmode-common.el ends here.
