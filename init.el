
(eval-when-compile (require 'cl))

;; (defvar *emacs-load-start* (current-time))

;;;; Load Path Setting
(let* ((emacs-dir "~/.emacs.d/")
       (lisp-dir '("lisp/" "local-lisp/" "rc.d/")))
  (dolist (lisp-path lisp-dir)
          (let* ((load-dir (concat emacs-dir lisp-path))
                 (default-directory load-dir))
            (setq load-path (cons load-dir load-path))
            (normal-top-level-add-subdirs-to-load-path))))

;;;; Loading emacs configures
(let* ((emacs-dir "~/.emacs.d/")
       (init-folder (concat emacs-dir "rc.d/"))
       (authinfo-file (concat emacs-dir ".authinfo.gpg")))
  (if (file-readable-p init-folder)
      (dolist (config-file (directory-files init-folder t ".*\.elc?$"))
              (let* ((feature (file-name-sans-extension (file-name-nondirectory config-file)))
                     (loading-result (require (intern feature) nil 'noerror)))
                (with-current-buffer (get-buffer-create "*Loading Config Log*")
                                     (insert (format "\t Loading %-20s\t \n" (concat init-folder config-file))))
                )))

  ;; Show about how many time we waste on open emacs
  ;; (message "All .emacs config loadded in %ds"
  ;;       (destructuring-bind (hi lo ms) (current-time)
  ;;                           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

  ;; After loading allemacs config file, readauthorization file
  (if (file-exists-p authinfo-file) (load-file authinfo-file))
  (message "\t --- Loading All Emacs Confis Finish ---")
  )
