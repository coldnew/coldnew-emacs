;;; emacs-goodies-build.el --- emacs-goodies-el maintance code
;; Copyright (C) 2003 Peter S. Galbraith

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ----------------------------------------------------------------------------

;;; Commentary:
;;
;; A few very rough functions to help with the maintenance of the
;; emacs-goodies-el pacakge.  None of this is meant for everyday use.
;;
;; I created `insert-missing-autoloads' to see what files didn't have
;; `;;;###autoload' markers for autoloads in emacs-goodies-el.el.
;;
;; Then I created `delete-tagged-autoloads' the remove a bunch of autoloads
;; that were entered manually in emacs-goodies-el.el since they can be
;; automatically generated.
;;
;; `insert-defgroup' and `insert-defgroup-dired' are used to automatically
;; make the file `emacs-goodies-custom.el' which loads a modified version
;; of all `defgroup' declarations from all files.  Go into dired for this
;; directory and run `insert-defgroup-dired' from the line before the first
;; listed file.  It will generate a buffer with `emacs-goodies-custom.el'
;; from all files.
;;
;; `document-tagged-autoloads' extracts a texinfo table from autoload
;; tagged commands.  I should probably make sure they are interactive.

;; Kevin Ryde writes:
;;   Incidentally, I believe an ;;;###autoload cookie on a defgroup copies
;;   it into the loaddefs, if that's easier to maintain, esp if respective
;;   authors were persuaded to put it in upstream.  That'd just leave
;;   emacs-goodies-el.info doc links perhaps done by custom-add-link.

;;; History:
;;

;;; Code:
(defun insert-missing-autoloads ()
  "Scan emacs-goodies-el.el for autoloads and check if there are in files."
  (interactive)
  (while (re-search-forward "(autoload '\\([^ ]+\\) \"\\(.*\\)\"" nil t)
    (let* ((command (match-string-no-properties 1))
           (efile (concat (match-string-no-properties 2) ".el")))
      (save-excursion
        (find-file efile)
        (goto-char (point-min))
        (when (re-search-forward (concat "^(defun " command "[ (]") nil t)
          (forward-line -1)
          (when (not (looking-at "^;;;###autoload"))
            (end-of-line)
            (insert "\n;;;###autoload")
            (message "Inserted for %s in %s" command efile)
            (save-buffer)))))))

(defun delete-tagged-autoloads ()
  "Scan emacs-goodies-el.el for autoloads and delete those that are marked.
Those that already have a ;;###autoload marker string are deleted from
emacs-goodies-el.el because a autoload file is automatically generated
(i.e. emacs-goodies-loaddefs.el)."
  (interactive)
  (while (re-search-forward "(autoload '\\([^ ]+\\) \"\\(.*\\)\"" nil t)
    (let* ((start (match-beginning 0))
           (command (match-string-no-properties 1))
           (efile (concat (match-string-no-properties 2) ".el"))
           (deleteit))
;;           (the-buffer (create-file-buffer efile)))
      (save-excursion
;;        (set-buffer the-buffer)
;;        (insert-file-contents efile)
        (find-file efile)
        (goto-char (point-min))
        (when (re-search-forward (concat "^(defun " command "[ (]") nil t)
          (forward-line -1)
          (when (looking-at "^;;;###autoload")
            (setq deleteit t))))
      (when deleteit
        (goto-char start)
        (forward-sexp 1)
        (forward-line 1)
        (delete-region start (point))
        (message "***Deleting %s in %s" command efile)))))

(defun insert-defgroup ()
  "Scan buffer for defgroup statements and merge in emacs-goodies-custom.el.
Add a :link '
Add a :group 'emacs-goodies-el"
  (interactive)
  (save-excursion
    (when (re-search-forward "^(defgroup \\([^ ]+\\)" nil t)
      (beginning-of-line)
      (let ((filename (file-name-nondirectory (buffer-file-name)))
            (defname (match-string 1))
            (text (buffer-substring (point)(progn (forward-sexp 1)(point)))))
        (if (string-match "^\\(.*\\)\\.el$" filename)
            (setq filename (match-string 1 filename)))
        (find-file "emacs-goodies-custom.el")
        (goto-char (point-max))
        (narrow-to-region (point)(point))
        (insert (format ";; %s\n" filename))
        (insert text)
        (delete-backward-char 1)
        (insert (format "\n;;:link '(custom-manual \"(emacs-goodies-el)%s\")\n"
                        filename))
        (insert (format "  :load '%s\n" filename))
;;      (insert (format "  :require '%s\n" filename))
        (insert         "  :group 'emacs-goodies-el)\n\n")
        (widen)
        (save-buffer)))))

(defun insert-defgroup-dired ()
  "Run through list of elisp files in dired."
  (interactive)
  (while (= 0 (forward-line 1))
    (when (and (looking-at ".*el$")
               (not (looking-at ".*emacs-goodies-el.el$"))
               (not (looking-at ".*emacs-goodies-custom.el$")))
      (save-excursion
        (dired-find-file)
        (goto-char (point-min))
        (emacs-lisp-mode)
        (insert-defgroup))))
  (find-file "emacs-goodies-custom.el")
  (goto-char (point-min))
  (insert ";;; emacs-goodies-custom.el --- Automatically harvested defgroups\n")
  (insert ";;\n")
  (insert ";;  Peter S Galbraith <psg@debian.org>\n")
  (insert ";;  License of copied code applies to this combined work (GPL V2)\n")
  (insert ";;\n")
  (insert ";;; Code:\n\n")
  (goto-char (point-max))
  (insert "(provide 'emacs-goodies-custom)\n"))

(defun document-tagged-autoloads ()
  "Scan for autoloads and extract texinfo doc string."
  (interactive)
  (let ((entries "")(function)(string))
    (while (re-search-forward "^;;;###autoload" nil t)
      (forward-line 1)
      (when (looking-at "^(defun \\(.+\\) (")
        (setq function (match-string-no-properties 1))
        (forward-line 1)
        (if (or (looking-at "  \"\\(.*\\)\"$")
                (looking-at "  \"\\(.*\\)$"))
            (setq string (match-string-no-properties 1))
          (setq string ""))
        (setq entries (concat entries "@item " function "\n" string "\n"))))
    (setq entries (concat "@noindent Commands:\n\n@table @samp\n"
                          entries "@end table\n"))
    (with-output-to-temp-buffer "*Help*"
      (princ entries))))

(provide 'emacs-goodies-build)

;;; emacs-goodies-build.el ends here
