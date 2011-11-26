;;; coldnew-deprecated.el
;;
;; This file contains some elisp I grabbed from the Internet which I may not use now,
;; maybe some time :P
;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)


(defvar hexcolor-keywords
  '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   ’face (list :background
					(match-string-no-properties 0)))))))
(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))




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

					; load given package if directory exists
(defmacro load-if-dir (dir-name &rest body)
  (let ((path-to-load (make-symbol "path-to-load")))
    `(let ((,path-to-load ,dir-name))
       (when (file-directory-p ,path-to-load)
	 (add-to-list 'load-path ,path-to-load)
	 ,@body))))

					; load given file if exists
(defmacro load-if-file (file-name &rest body)
  (let ((file-to-load (make-symbol "file-to-load")))
    `(let ((,file-to-load ,file-name))
       (when (file-readable-p ,file-to-load)
	 (load ,file-to-load)
	 ,@body))))

					; load given file if exists, and add dir to path
(defmacro load-if-dir-and-file (dir-name file-name &rest body)
  `(progn
    (load-if-dir ,dir-name)
    (load-if-file (concat ,dir-name ,file-name))
    ,@body))





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





(defun resolve-sym-link ()
  "Replace the string at the point with the true path."
  (interactive)
  (beginning-of-line)
  (let* ((file (buffer-substring (point)
				 (save-excursion (end-of-line) (point))))
	 (file-dir (file-name-directory file))
	 (file-true-dir (file-truename file-dir))
	 (file-name (file-name-nondirectory file)))
    (delete-region (point) (save-excursion (end-of-line) (point)))
    (insert (concat file-true-dir file-name))))


(defun show-dot-emacs-structure ()
  "Show the outline-mode structure of ~/.emacs"
  (interactive)
  (occur "^;;;;+"))


(defun lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
This command generates a url for Wikipedia.com and switches you
to browser. If a region is active (a phrase), lookup that phrase."
  (interactive)
  (let (myword myurl)
    (setq myword
	  (if (and transient-mark-mode mark-active)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	      (thing-at-point 'symbol)))

    (setq myword (replace-regexp-in-string " " "_" myword))
    (setq myurl (concat "http://en.wikipedia.org/wiki/" myword))
    ;;(browse-url myurl)
    (if (featurep 'w3m)
	(w3m-browse-url myurl)
	(browse-url myurl))
    ))



;; ------------------------------------------------------------------------------
;; execute-keyboard-macro-here
;; ------------------------------------------------------------------------------
;; When clicked, move point to the location clicked and execute the last
;; defined keyboard macro there.  Very handy for automating actions which
;; must be done many times but at user controlled places. (e.g. lowercasing
;; HTML tags.)
;;
(defun execute-keyboard-macro-here (event)
  "Move point and execute the currently defined macro."
  (interactive "e")
  (mouse-set-point event)
  (call-last-kbd-macro))

;; ------------------------------------------------------------------------------
;; kill-other-buffers
;; ------------------------------------------------------------------------------
;; I find that Emacs buffers multiply faster than rabbits.  They were
;; regenerating faster than I could kill them so I wrote this.  (The
;; original version was my first code in ELisp!)  Run this macro to kill
;; all but the active buffer and the unsplit the window if need be.
;;
(defun kill-other-buffers ()
  "Kill all buffers except the current and unsplit the window."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))   ; Delete other buffers
  (delete-other-windows)                                      ; And then unsplit the current window...
  (delete-other-frames))                                      ; ...and remove other frames, too.


;; remove duplicate lines in a buffer
(defun remove-duplicate-lines()
  "Remove duplicate lines in a buffer"
  (interactive)
  (save-excursion
   (let
       ((lines_hash (make-hash-table :test #'equal))
	(numlines (count-lines 1 (progn (end-of-buffer)(point)))))

     ;; Make a hash table with key=line
     ;;     and value=the smallest line number that contains a line.
     (loop for i from numlines downto 1 do
	   (let ((line nil))
	     (goto-line i)
	     (setf line (get-current-line))
	     ;; Want to store the smallest line number for
	     ;;     a particular line.
	     (setf (gethash line lines_hash) i)))
     ;; If a line has a line number not equal to the smallest line, kill it.
     (loop for i from numlines downto 1 do
	   (let ((line nil))
	     (goto-line i)
	     (setf line (get-current-line))
	     (beginning-of-line)
	     (if (not (equal line ""))
		 (if (not (=
			   (let ((min-line (gethash line lines_hash)))
			     (if (null min-line)
				 -1
				 min-line))
			   i))
		     (kill-line 1))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dead emacs Config <- some old emacs config I use, all in comment and won't use again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq-default debug-on-error     nil )
;;;;;;;; 將指定目錄裡的東西全部加入清單
;; (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;     (let* ((my-lisp-dir "~/.emacs.d/")
;;	   (default-directory my-lisp-dir))
;;       (setq load-path (cons my-lisp-dir load-path))
;;       (normal-top-level-add-subdirs-to-load-path)))
;;;
;;;; 移除掉的預設功能
;;(setq-default ring-bell-function (lambda () t)) ; 關閉 console 下的螢幕閃爍
;;;; 基本外觀設置
;;(menu-bar-mode          t ) ; 移除菜單欄
;;(transient-mark-mode    t )    ; 高亮顯示要拷貝的區域



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Deprecated Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmacro require-maybe (feature &optional file)
;;   "*Try to require FEATURE, but don't signal an error if `require' fails."
;;   `(require ,feature ,file 'noerror))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Deprecated Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun byte-compile-when-save()
;;   "When save, recompile it"
;;   (make-local-variable 'after-save-hook)
;;   (add-hook 'after-save-hook
;;	    (lambda ()
;;	      (if (buffer-file-name)
;;		  (byte-compile-file buffer-file-name)))))
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
;;;
;; (defcmd show-mode-line ()
;;   (if mode-line-format
;;       (setq mode-line-format nil)
;;     (setq mode-line-format	t)))
;;;
;; FIXME: I dont's like after fc-eval-and-replace, nil or t will show templary us m eval-and-replace instead
;; (defun fc-eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;;	     (current-buffer))
;;     (error (message "Invalid expression")
;;	   (insert (current-kill 0)))))

;; BUG: when use sort-lines, this will prompt
;; (defadvice kill-buffer (around my-kill-buffer-check activate)
;;   "Prompt when a buffer is about to be killed."
;;   (let* ((buffer-file-name (buffer-file-name))
;;	 backup-file)
;;     ;; see 'backup-buffer
;;     (if (and (buffer-modified-p)
;;	     buffer-file-name
;;	     (file-exists-p buffer-file-name)
;;	     (setq backup-file (car (find-backup-file-name buffer-file-name))))
;;	(let ((answer (completing-read (format "Buffer modified %s, (d)iff, (s)ave, (k)ill? " (buffer-name))
;;				       '("d" "s" "k") nil t)))
;;	  (cond ((equal answer "d")
;;		 (set-buffer-modified-p nil)
;;		 (let ((orig-buffer (current-buffer))
;;		       (file-to-diff (if (file-newer-than-file-p buffer-file-name backup-file)
;;					 buffer-file-name
;;				       backup-file)))
;;		   (set-buffer (get-buffer-create
;;				(format "%s last-revision" (file-name-nondirectory file-to-diff))))
;;		   (buffer-disable-undo)
;;		   (insert-file-contents file-to-diff nil nil nil t)
;;		   (set-buffer-modified-p nil)
;;		   (setq buffer-read-only t)
;;		   (ediff-buffers (current-buffer) orig-buffer)))
;;		((equal answer "k")
;;		 (set-buffer-modified-p nil)
;;		 ad-do-it)
;;		(t
;;		 (save-buffer)
;;		 ad-do-it)))
;;       ad-do-it)))


;; (defadvice find-file (around find-file-set-trigger-variable protect activate)
;;   "bind a variable so that history command can do special behavior for find-file"
;;   (interactive (let (inside-find-file-command) (find-file-read-args "Find file: " nil)))
;;   ad-do-it)

;; (defadvice next-history-element (around next-history-element-special-behavior-for-find-file protect activate)
;;   "when doing history for find-file, use the buffer-list as history"
;;   (if (boundp 'inside-find-file-command)
;;       (let ((find-file-history (delq nil (mapcar 'buffer-file-name (buffer-list))))
;;             (minibuffer-history-variable 'find-file-history))
;;         ad-do-it)
;;     ad-do-it))

;;;;;;;; textmate
;; TextMate has some very nice to use bindings on quotes, brackets,
;; parentheses etc, which Emacs lacks out of the box.
;; You can use skeleton-pairs to insert pairs, but what about deleting
;; them? What if you (or a colleague) press the closing bracket key
;; accidentally (muscle memory and all)?
;; textmate-mode provides more sensible behaviour for the keys
;; ", ', (, [, {, and their closing pairs.
;;
;; (when (require* 'textmate)
;;   ;; (tm/initialize)
;;   )


(provide 'coldnew-deprecated)
;; coldnew-deprecated.el ends here.
