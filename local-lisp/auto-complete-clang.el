;;; auto-complete-clang.el ---

;; Copyright 2011
;;
;; Author: coldnew
;; Keywords:
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/download/auto-complete-clang.el
(defconst auto-complete-clang-version "0.1")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;;

;;; Bug Report:
;;
;; If you have problems, send a bug report via M-x auto-complete-clang-send-bug-report.
;; I implemented bug report feature because I want to know your current state.
;; It helps me to solve problems easily.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.tw")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of anything.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "auto-complete-clang.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x auto-complete-clang-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Taiwanese, please write in Taiwanese :P

;;; Change Log:
;;
;;

;;; Usage:
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'auto-complete-clang)

;;; Code:

(eval-when-compile (require 'cl))

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


;;https://github.com/mikeandmore/auto-complete-clang/blob/master/auto-complete-clang.el
;;https://github.com/brianjcj/auto-complete-clang/blob/master/auto-complete-clang.el


(defcustom ac-clang-executable
  (executable-find "clang")
  "Location of clang executable"
  :group 'auto-complete
  :type 'file)


;;;; face
(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)








;;TODO:rewrite following
;; faces
(defface clang-completion-plain-face
  '((t (:inherit default :family "Verdana")))
  "clang completion hint base font" :group 'clang-completion-faces)

(defface clang-completion-type-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#729FCF" :weight bold :family "Verdana")))
  "clang completion hint font for types" :group 'clang-completion-faces)

(defface clang-completion-variable-face
  '((t (:inherit 'clang-completion-plain-face :foreground "#73D216" :family "Verdana")))
  "clang completion hint font for variables" :group 'clang-completion-faces)


;; extra flags
(defvar clang-completion-pch nil)
(defvar clang-completion-flags nil)
(defvar clang-completion-suppress-error nil)


(defvar clang-completion-doc-table (make-hash-table :test 'equal))

(defun clang-process-exec (command)
  (with-output-to-string
    (with-current-buffer standard-output
                         (unless (or (eq (apply 'call-process (car command) nil '(t ".clang-completion-error") nil (cdr command)) 0) clang-completion-suppress-error)
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

    (when (listp clang-completion-flags)
      (setq cmd (append cmd clang-completion-flags)))
    (when (stringp clang-completion-pch)
      (setq cmd (append cmd (list "-include-pch" clang-completion-pch))))
    (message (format "complete at %s:%s:%s" filename row col))
    (clang-process-exec cmd)))

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

(defun clang-get-doc (symbol)
  ;;(setq symbol (symbol-name (intern-soft symbol)))
  (let ((reslist (gethash symbol clang-completion-doc-table)))
    (with-temp-buffer
     (font-lock-add-keywords nil '(("\\[#\\(.*?\\)#\\]" 1
                                    'clang-completion-type-face t)))
     (font-lock-add-keywords nil '(("<#\\(.*?\\)#>" 1
                                    'clang-completion-variable-face t)))
     (font-lock-add-keywords nil '(("\\(.*\\)" 1
                                    'clang-completion-plain-face t)))
     (font-lock-mode t)

     (insert (reduce '(lambda (x y) (concat x "\n" y)) reslist))
     (font-lock-fontify-buffer)
     (filter-doc-buffer)

     (message (buffer-string))))
  ;;(with-temp-buffer
  ;;  (dolist (proto reslist)
  ;;    (insert proto)
  ;;    (insert "\n\n"))
  ;;  (filter-doc-buffer)
  ;;  (buffer-string))
  ;; display nothing
  (return nil))

(defvar ac-source-clang
  '((candidates . (clang-get-completions nil ac-point))
    (prefix "[^a-zA-Z0-9_]\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . clang-get-doc)
    (requires . 0)
    (symbol . "C")
    (cache)))

;;(defvar ac-source-clang-static-complete
;;  '((candidates . (clang-get-completions nil ac-point))
;;    (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
;;    ;;(document . 'clang-get-doc)
;;    (requires . 0)
;;    (symbol . "M")
;;    (cache)))

(defun ac-complete-clang ()
  (interactive)
  (auto-complete '(ac-source-clang)))




(provide 'auto-complete-clang)
;; auto-complete-clang.el ends here.
