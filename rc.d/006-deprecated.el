;;
(eval-when-compile (require 'cl))

;;;; Macros

(defmacro partial (f &rest args)
  `(lambda (&rest more)
     (apply ',f ,@args more)))

(defmacro lexdef (name args &rest body)
  "Defun with lexically-scoped parameters. Could also be called lexical-defun."
  `(defun ,name ,args
     (lexical-let ,(->> args
			(remove-if (partial equal '&rest))
			(mapcar (lambda (arg) (list arg arg))))
       ,@body)))

;; (defmacro require-maybe (feature &optional file)
;;   "*Try to require FEATURE, but don't signal an error if `require' fails."
;;   `(require ,feature ,file 'noerror))



;; Math

(defun mean (values)
  (/ (reduce '+ values)
     (float (length values))))

(defun square (x)
  (* x x))

(defun variance (values)
  (- (->> values (mapcar 'square) mean)
     (square (mean values))))

;; Sequences

(defun sequence (maybe-seq)
  "Returns the value wrapped in a sequence if it is not a sequence already."
  (if (sequencep maybe-seq) maybe-seq
    (list maybe-seq)))

(defun random-elt (sequence)
  (elt sequence
       (-> sequence length random)))

(defun seq-difference (lseq rseq)
  (remove-if (lambda (element) (find element rseq :test 'equal))
	     lseq))

;; Strings

(defun string-empty-p (str)
  (if str
      (string= "" str)
    t))

(defun string-not-empty-p (str)
  (not (string-empty-p str)))

(defun string-blank-p (str)
  (if (string-empty-p str)
      t
    (not (null (string-match "^\\(?:\s*\n\\)*$" str)))))

(defun string-not-blank-p (str)
  (not (string-blank-p str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undestructive alist functions

(defun alist-get (alist key &optional default)
  (or (assoc-default key alist)
      default))

(defun alist-remove (alist key)
  "Doesn't change the original alist, returns a new one instead."
  (remove-if (lambda (x) (equal key (car x)))
	     alist))

(defun alist-set (alist key value)
  "Doesn't change the original alist, returns a new one instead."
  (cons (cons key value) (alist-remove alist key)))

;;;;
;; (defun require* (feature &optional force)
;;   (when (or force (not (featurep feature)))
;;     (setq feature (symbol-name feature))
;;     (let ((path load-path)
;;	  (found-filename nil)
;;	  head el-attribs elc-attribs)
;;       (while (and (not found-filename) path)
;;	(setq head (pop path))
;;	(let ((el-filename (format "%s/%s.el" head feature))
;;	      (elc-filename (format "%s/%s.elc" head feature)))
;;	  ;; if .el and .elc both exist, pick the newest
;;	  ;; otherwise pick the one that exists if any
;;	  (cond ((and (file-exists-p el-filename)
;;		      (file-exists-p elc-filename))
;;		 (if (file-newer-than-file-p el-filename elc-filename)
;;		     (setq found-filename el-filename)
;;		   (setq found-filename elc-filename)))
;;		((file-exists-p el-filename)
;;		 (setq found-filename el-filename))
;;		((file-exists-p elc-filename)
;;		 (setq found-filename elc-filename)))
;;	  ;; load file if found
;;	  (when found-filename
;;	    (message (format "Found: [%s]" found-filename))
;;	    (let ((load-suffixes ()))
;;	      (load found-filename)))))
;;       (unless found-filename (error "Unable to find %s" feature)))))


;;(setq-default header-line-format mode-line-format) ; Copy mode-line to top
;;(setq-default mode-line-format nil) ; Remove mode-line
;; (defcmd show-mode-line ()
;;   (if mode-line-format
;;       (setq mode-line-format nil)
;;     (setq mode-line-format	t)))


;;;;;;;; 將指定目錄裡的東西全部加入清單
;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir "~/.emacs.d/")
;;	   (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))





;;;; Functions
(defun find-file:find-proper-mode ()
  ;; only run on .h files
  (when (string-match "\\.h\\'" (buffer-file-name))
    (save-window-excursion
      (save-excursion
	(let* ((alist (append auto-mode-alist nil))  ;; use whatever auto-mode-alist has
	       (ff-ignore-include t)                 ;; operate on buffer name only
	       (src (ff-other-file-name))            ;; find the src file corresponding to .h
	       re mode)
	  ;; go through the association list
	  ;; and find the mode associated with the source file
	  ;; that is the mode we want to use for the .h file
	  (while (and alist
		      (setq mode (cdar alist))
		      (setq re (caar alist))
		      (not (string-match re src)))
	    (setq alist (cdr alist)))
	  (when mode (funcall mode)))))))


;; ;; 該資料夾內沒有 Tags 檔案時自動建立,若有時則更新 Tags 檔
;; (defun etags-create-or-update ()
;;   "create or update the etag file"
;;   (interactive)
;;   ;; tagfile doesn't exist?
;;   (if (not (= 0 (call-process "global" nil nil nil " -p")))
;;       (let ((olddir default-directory)
;;	    (topdir (read-directory-name
;;		     "gtags: top of source tree:" default-directory)))
;;	(cd topdir)
;;	(shell-command "gtags && echo 'created tagfile'")
;;	(cd olddir)) ; restore
;;     ;;  tagfile already exists; update it
;;     (shell-command "global -u && echo 'updated tagfile'")))


;; (defvar my-auto-update-tags-alist
;;   (list '("/some/path/to/TAGS" "command_to_build_tags")
;;         '("/another/path/to/TAGS" "another_build_command")))

;; (defun my-auto-update-tags ()
;;   "Automatically update TAGS files"
;;   (tags-table-check-computed-list)
;;   (let ((filename (buffer-file-name))
;;         build-cmd)
;;     (mapc (lambda (tag-file)
;;             (set-buffer tag-file)
;;             (when (member filename (tags-table-files))
;;               (setq build-cmd (cdr (assoc tag-file my-auto-update-tags-alist)))
;;               (when build-cmd
;;                 (call-process build-cmd nil 0))))
;;           tags-table-computed-list)))

;; (add-hook 'after-save-hook 'my-auto-update-tags)



;; FIXME: I dont's like after fc-eval-and-replace, nil or t will show
;; templary us m eval-and-replace instead
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))






(provide '006-deprecated)
;; 006-deprecated.el ends here.
