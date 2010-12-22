
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
