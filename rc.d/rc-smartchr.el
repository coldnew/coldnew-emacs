;; init for smartchr
(provide 'rc-smartchr)

(require 'smartchr)

(defun smartchr-insert-eol (s)
  (lexical-let ((s s))
    (smartchr-make-struct
     :insert-fn (lambda ()
		  (save-excursion
		    (goto-char (point-at-eol))
		    (when (not (string= (char-to-string (preceding-char)) s))
		      (insert s))))
     :cleanup-fn (lambda ()
		   (save-excursion
		     (goto-char (point-at-eol))
		     (delete-char (- 0 (length s))))))))


(defun smartchr:insert-if-not-in-string (s)
  (interactive)
  (lexical-let ((s s))
    (smartchr-make-struct
     :insert-fn (lambda ()
		  (save-excursion
		    (when (not (string= (char-to-string (preceding-char)) s))
		      (insert s))))
     :cleanup-fn (lambda ()
		   (save-excursion
		     (delete-char (- 0 (length s))))))))


(defun smartchr-insert-semicolon-eol ()
  (smartchr-insert-eol ";"))

(defun insert-char-smart-common-type ()
  "insert character more smart."
  (when (featurep 'smartchr)
    (vim:imap (kbd "(")  (smartchr '("(`!!')" "(")))
    ;; (vim:imap (kbd "[")  (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    ;; (vim:imap (kbd "{")  (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
    ;; (vim:imap (kbd "`")  (smartchr '("\``!!''" "\`")))
    (vim:imap (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    ;; (vim:imap (kbd ">")  (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
    ;; (vim:imap (kbd "F")  (smartchr '("F" "$" "$_" "$_->" "@$"))) ;
    ;; (vim:imap (kbd "=")  (smartchr '(" = " " == "  "="))) ;
    ;; (vim:imap (kbd ";")  (smartchr '(";" smartchr-insert-semicolon-eol)))
    ))

(defun insert-equal-char-smart ()
  "insert character more smart."
  (when (featurep 'smartchr)
    (vim:imap (kbd "=") (smartchr '(" = " " == "  "=")))))
