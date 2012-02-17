;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'coldnew-project)

(require 'flymake)
(require 'flymake-cursor)
(require 'rfringe)


(defun global-compilation-hook ()
  "Global compilation setting."

  ;; Make compilaction buffer always scrolls to follow output as it comes in.
  (setq compilation-scroll-output t)

  ;; Auto jump to the first error.
  (setq compilation-auto-jump-to-first-error t)

  ;;

  )


(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
   close the *compilation* buffer if the compilation is successful,
   and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
       (delete-windows-on buffer)
       (message (propertize "COMPILATION SUCCESSFUL :-) " 'face 'font-lock-warning-face))
       ;;	(tooltip-show "\n Compilation Successful :-) \n ")
       )
      (tooltip-show "\n Compilation Failed :-( \n "))
  ;; FIXME: When I use dualscreen, following functiokn will make error,
  ;;        after compilation, current frame will jump to another DISPLAY
  ;;  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  ;; (select-frame-set-input-focus current-frame)
  )

(add-to-list 'compilation-finish-functions 'notify-compilation-result)

;;;;;;;; Emacs
;; ignore byte-compile warnings
(setq byte-compile-warnings '(not nresolved
				  free-vars
				  callargs
				  redefine
				  obsolete
				  noruntime
				  cl-functions
				  interactive-only
				  ))



;;;;;;;; Flymake

;; disable annoying flymake dialog
(setq flymake-gui-warnings-enabled nil)

(defun flymake-display-err-popup-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (menu-data           (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
	(popup-tip (mapconcat #'(lambda (e) (nth 0 e))
			      (nth 1 menu-data)
			      "\n")))
    ))

(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
	 (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (count (length line-err-info-list))
	 )
    (while (> count 0)
      (when line-err-info-list
	(let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
	       (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
	       (text (flymake-ler-text (nth (1- count) line-err-info-list)))
	       (line (flymake-ler-line (nth (1- count) line-err-info-list))))
	  (message "[%s] %s" line text)
	  )
	)
      (setq count (1- count)))))

;; TODO: combine flymake with CMake
(defun flymake-generic-init (cmd &optional opts)
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
		       temp-file
		       (file-name-directory buffer-file-name))))
    (list cmd (append opts
		      (list local-file)))))

(defun flymake-generic-init-makefile (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
      (flymake-generic-init cmd opts)))

;; (defun flymake-display-err-minibuf-for-current-line ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (let* ((line-no             (flymake-current-line-no))
;;	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;;	 (count               (length line-err-info-list)))
;;     (while (> count 0)
;;       (when line-err-info-list
;;	(let* ((text       (flymake-ler-text (nth (1- count) line-err-info-list)))
;;	       (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;;	  (message "[%s] %s" line text)))
;;       (setq count (1- count)))))



(provide 'coldnew-build)
;; coldnew-build.el ends here.
