;;; auto-complete-clang.el ---

;; copyright 2011
;;
;; author: coldnew
;; keywords:
;; x-url: http://www.emacswiki.org/cgi-bin/wiki/download/auto-complete-clang.el
(defconst auto-complete-clang-version "0.1")

;; this program is free software; you can redistribute it and/or modify
;; it under the terms of the gnu general public license as published by
;; the free software foundation; either version 2, or (at your option)
;; any later version.
;;
;; this program is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the
;; gnu general public license for more details.
;;
;; you should have received a copy of the gnu general public license
;; along with this program; if not, write to the free software
;; foundation, inc., 675 mass ave, cambridge, ma 02139, usa.

;;; commentary:
;;
;;

;;; bug report:
;;
;; if you have problems, send a bug report via m-x auto-complete-clang-send-bug-report.
;; i implemented bug report feature because i want to know your current state.
;; it helps me to solve problems easily.
;; the step is:
;;  0) setup mail in emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "your full name")
;;       (setq smtpmail-smtp-server "your.smtp.server.tw")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) be sure to use the latest version of anything.el.
;;  2) enable debugger. m-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) use lisp version instead of compiled one: (load "auto-complete-clang.el")
;;  4) do it!
;;  5) if you got an error, please do not close *backtrace* buffer.
;;  6) m-x auto-complete-clang-send-bug-report (outside)
;;     then m-x insert-buffer *backtrace* (if you got error)
;;  7) describe the bug using a precise recipe.
;;  8) type c-c c-c to send.
;;  # if you are a taiwanese, please write in taiwanese :p

;;; change log:
;;
;;

;;; usage:
;; put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-clang)

;;; code:

(eval-when-compile (require 'cl))
(require 'auto-complete)

;;;;##########################################################################
;;;;  user options, variables
;;;;##########################################################################


;;https://github.com/mikeandmore/auto-complete-clang/blob/master/auto-complete-clang.el
;;https://github.com/brianjcj/auto-complete-clang/blob/master/auto-complete-clang.el


(defcustom ac-clang-executable (executable-find "clang")
  "location of clang executable"
  :type 'file
  :group 'auto-complete)

(defcustom ac-clang-flags nil
  "Extra flags to pass to the Clang executable.
   This variable will typically contain include paths, e.g., -I~/MyProject."
  :type '(repeat (string :tag "Argument" ""))
  :group 'auto-complete
  )

(defcustom ac-clang-prefix-header nil
  "The prefix header to use eith Clang code completion."
  :type '(string)
  :group 'auto-complete)

;;;;;;;; faces
(defface ac-clang-candidate-face
  '((t (:inherit ac-candidate-face)))
  "face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:inherit ac-selection-face)))
  "face for the clang selected candidate."
  :group 'auto-complete)

(defface ac-clang-function-name-face
  '((t (:inherit font-lock-function-name-face)))
  "clang doc for function-name"
  :group 'auto-complete)

(defface ac-clang-type-face
  '((t (:inherit 'font-lock-type-face)))
  "clang doc for type"
  :group 'auto-complete)

(defface ac-clang-variable-name-face
  '((t (:inherit 'font-lock-variable-name-face)))
  "clang doc for variable-name"
  :group 'auto-complete)

;; Set the Clang prefix header
(defun ac-clang-set-prefix-header (ph)
  "Set the clang prefix header."
  (interactive
   (let ((prefix-header (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header "
			      ;; show information if there's an old pch file
			      (if (stringp ac-clang-prefix-header)
				  "(current: " ac-clang-prefix-header ") :"))
		      (if prefix-header
			  (file-name-directory prefix-header))
		      prefix-header nil
		      (if prefix-header
			  (file-name-nondirectory prefix-header))))))
  (cond ((string-match "^[\s\t]*$" ph)
	 (setq ac-clang-prefix-header nil))
	(t
	 (setq ac-clang-prefix-header)))
  )

;; FIXME: nouse?
;; (defun ac-clang-prefix-symbol ()
;;   "Return the result of (ac-prefix-symbol) if there is a `<' or `.' or
;;    `->' or `::' between current point and the nearest space or tab backward."
;;   (let ((p (or (save-excursion
;;		(re-search-backward "[ \t]"
;;                                  (save-excursion (beginning-of-line) (point))
;;                                  t))
;;             (save-excursion (beginning-of-line) (point)))))
;;     (when (save-excursion (re-search-backward "<\\|\\.\\|->\\|::" p t))
;;       (ac-prefix-symbol))))

(defun ac-clang-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
	(when (or (eq ?\. c)
		  ;; ->
		  (and (eq ?> c)
		       (eq ?- (char-before (1- (point)))))
		  ;; ::
		  (and (eq ?: c)
		       (eq ?: (char-before (1- (point))))))
	  (point)))))

;; FIXME: not only functions but also variables
(ac-define-source clang
		  '((candidates . (clang-get-completions nil ac-point))
		    (candidate-face . ac-clang-candidate-face)
		    (selection-face . ac-clang-selection-face)
		    (prefix . ac-clang-prefix)
		    (requires . 0)
		    (document . ac-clang-document)
		    (cache)
		    (symbol . "c")
		    ))







;;;;;;;;;; MODIFIED ABOVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: rewrite below.


;; TODO: remove folowing
(defalias 'ac-clang-function-name-face 'clang-completion-plain-face)
(defalias 'ac-clang-type-face 'clang-completion-type-face)
(defalias 'ac-clang-variable-name-face 'clang-completion-variable-face)



;;;;;;;; local variables
(defvar clang-completion-pch ac-clang-prefix-header)
(defvar clang-completion-suppress-error nil)
(defvar clang-completion-doc-table (make-hash-table :test 'equal))


;;;;;;;; functions
;; read-file-name prompt &optional directory default require-match initial predicate


(defun clang-process-exec (command)
  (with-output-to-string
    (with-current-buffer standard-output
			 (unless (or (eq (apply 'call-process (car command) nil
						'(t ".clang-completion-error") nil (cdr command)) 0)
				     clang-completion-suppress-error)
			   (let ((last-command compile-command))
			     (compile "cat .clang-completion-error")
			     (setq compile-command last-command))))))

(defun clang-parse-completion-line (line)
  (cond ((string-match "^COMPLETION: Pattern" line) nil)  ;; exclude patterns
	((string-match "^COMPLETION: \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
	 (list (match-string 1 line) (match-string 2 line)))
	((string-match "^OVERRIDE:  \\([^ ]*\\)\\(?: : \\([^\"]*\\)\\)$" line)
	 (list (match-string 1 line) (match-string 2 line)))
	(t nil))
  )

(defun clang-process (buffer point)
  (unless (buffer-file-name buffer)
    (return ""))
  (let* ((filename (buffer-file-name buffer))
	 (col      (1+ (- point (point-at-bol))))
	 (row      (count-lines point (point-min)))
	 (cmd      (list ac-clang-executable "-cc1"
			 filename "-fsyntax-only" "-code-completion-at"
			 (format "%s:%s:%s" filename row col))))

    ;; eval the config file under buffer locations
    (let* ((filedir  (file-name-directory filename))
	   (config-filename (concat filedir ".clang-completion-config.el")))
      (when (file-readable-p config-filename)
	(with-temp-buffer
	 (insert-file-contents config-filename)
	 (eval-buffer))))

    (when (listp ac-clang-flags)
      (setq cmd (append cmd ac-clang-flags)))
    (when (stringp clang-completion-pch)
      (setq cmd (append cmd (list "-include-pch " clang-completion-pch))))

    (message (format "complete at %s:%s:%s" filename row col))
    (clang-process-exec cmd))

  )

(defun clang-get-process-result (string)
  (let* ((completion-lines (split-string string "\n")))
    (delq nil (mapcar 'clang-parse-completion-line completion-lines))))

(defun clang-get-process-completion-result (string)
  (mapcar 'car (clang-get-process-result string)))

(defun clang-get-process-prototype-table (string)
  (let* ((lines (clang-get-process-result string))
	 (result-table (make-hash-table :test 'equal)))
    (dolist (line lines)
	    (let* ((key (first line))
		   (value (gethash key result-table)))
	      (setq value (append value (list (second line))))
	      (puthash key value result-table))
	    )
    (setq clang-completion-doc-table result-table)))

(defun clang-get-completions (&optional buffer point)
  ;; save all modified buffers
  (or buffer (setq buffer (current-buffer)))
  (or point (setq point (point)))
  (save-some-buffers t)
  (let* ((output (clang-process buffer point)))
    (clang-get-process-prototype-table output)
    (clang-get-process-completion-result output)))

(defun filter-doc-buffer ()
  (while (re-search-backward "\\[#.*?::#\\]" nil t)
    (replace-match ""))
  (goto-char (point-max))

  (while (re-search-backward "\\[#\\|#\\]" nil t)
    (replace-match " "))
  (goto-char (point-max))
  (while (re-search-backward "{#\\|#}\\|<#\\|#>" nil t)
    (replace-match ""))
  )

;; (font-lock-add-keywords 'lisp-interaction-mode
;;			'(("(\\(require\*\\*\\)\\s [ \t']*\\(\\sw+\\)?"
;;                         (1 font-lock-keyword-face)
;;                         (2 font-lock-constant-face nil t))))

;;void gtk_init(int *argc, char ***argv)

(defun ac-clang-document (symbol)
  (let ((reslist (gethash symbol clang-completion-doc-table)))
    (with-temp-buffer
     (font-lock-add-keywords nil '(("\\[#\\(.*?\\)#\\]" 1
				    'ac-clang-type-face t)))
     (font-lock-add-keywords nil '(("<#\\(.*?\\)#>" 1
				    'ac-clang-variable-name-face t)))
     (font-lock-add-keywords nil '(("\\(.*\\)" 1
				    'clang-completion-plain-face t)))
     (font-lock-mode t)

     (insert (reduce '(lambda (x y) (concat x "\n" y)) reslist))
     (font-lock-fontify-buffer)
     (filter-doc-buffer)

     (message (buffer-string))))
  (return nil))







(provide 'auto-complete-clang)
;; auto-complete-clang.el ends here.
