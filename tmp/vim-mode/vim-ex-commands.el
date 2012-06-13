;;; vim-ex-commands.el - Implementation of some ex-mode commands.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'vim-ex)

(defun* vim:save-buffer (file-name &key begin end force append)
  "Saves the lines from `begin' to `end' to file `file-name'."
  (with-current-buffer vim:ex-current-buffer
    (when (null file-name)
      (setq file-name (buffer-file-name))
      (unless file-name
        (error "Please specify a file-name for this buffer!")))

    (let (beg-pos end-pos)
      (when begin
        (setq beg-pos (save-excursion
                        (goto-line begin)
                        (line-beginning-position)))
        (setq end-pos (if end
                          (save-excursion
                            (goto-line end)
                            (line-end-position))
                        beg-pos)))

      (cond
       ((and (null beg-pos)
             (string= file-name (buffer-file-name)))
        (save-buffer))
       ((and (null beg-pos)
             (null (buffer-file-name)))
        (write-file file-name (not force)))
       (t
        (write-region beg-pos end-pos file-name append nil nil (not force)))))))


(vim:defcmd vim:cmd-write (motion (argument:file file) force nonrepeatable)
  "Saves file `file'."
  (vim:save-buffer file
                   :begin (and motion (vim:motion-first-line motion))
                   :end (and motion (vim:motion-last-line motion))
                   :force force))

(vim:defcmd vim:cmd-write-all (force nonrepeatable)
  "Saves all buffers."
  (save-some-buffers force))

(vim:defcmd vim:cmd-edit ((argument:file file) nonrepeatable)
  "Visits a certain file."
  (if file
      (find-file file)
    (when (buffer-file-name)
      (find-file (buffer-file-name)))))

(vim:defcmd vim:cmd-show-buffers (nonrepeatable)
  "Shows the buffer-list."
  (let (message-truncate-lines message-log-max)
    (message "%s"
             (mapconcat #'buffer-name (buffer-list) "\n"))))

(vim:defcmd vim:cmd-buffer ((argument:buffer buffer) nonrepeatable)
  "Switches to another buffer."
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(vim:defcmd vim:cmd-next-buffer (count nonrepeatable)
  "Goes to the `count'-th next buffer in the buffer list."
  (dotimes (i (or count 1))
    (next-buffer)))

(vim:defcmd vim:cmd-prev-buffer (count nonrepeatable)
  "Goes to the `count'-th prev buffer in the buffer list."
  (dotimes (i (or count 1))
    (previous-buffer)))

(vim:defcmd vim:cmd-split-buffer ((argument:buffer buffer) nonrepeatable)
  "Splits window and switches to another buffer."
  (vim:window-split)
  (vim:cmd-buffer :argument buffer))

(vim:defcmd vim:cmd-split-next-buffer (count nonrepeatable)
  "Splits window and goes to the `count'-th next buffer in the buffer list."
  (vim:window-split)
  (vim:cmd-next-buffer :count count))

(vim:defcmd vim:cmd-split-prev-buffer (count nonrepeatable)
  "Splits window and goes to the `count'-th prev buffer in the buffer list."
  (vim:window-split)
  ((vim:cmd-prev-buffer :count count)))

(vim:defcmd vim:cmd-delete-buffer ((argument:buffer buffer) force nonrepeatable)
  "Deletes a buffer."
  (when force
    (if buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
      (set-buffer-modified-p nil)))
  (kill-buffer buffer))

(vim:defcmd vim:cmd-quit (force nonrepeatable)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error
        (if force
            (kill-emacs)
          (save-buffers-kill-emacs)))))))

(vim:defcmd vim:cmd-quit-all (force nonrepeatable)
  "Exits Emacs, asking for saving."
  (if force
      (kill-emacs)
    (save-buffers-kill-emacs)))

(vim:defcmd vim:cmd-save-and-quit (nonrepeatable)
  "Exits Emacs, without saving."
  (save-buffers-kill-emacs 1))

(vim:defcmd vim:cmd-save-and-close ((argument:file file) force nonrepeatable)
  "Saves the current buffer and closes the window."
  (vim:cmd-write :argument file :force force)
  (vim:cmd-quit))


(defun vim:ex-complete-mode-argument (mode predicate flag)
  "Completes a registered vim-mode submode."
  (when mode
    (let ((modes (mapcar #'cdr vim:mode-alist)))
      (with-current-buffer vim:ex-current-buffer
	(case flag
	  ((nil) (try-completion mode modes predicate))
	  ((t) (all-completions mode modes predicate))
	  ((lambda) (vim:test-completion mode modes predicate)))))))
  
(vim:define-arg-handler 'mode
			:complete 'vim:ex-complete-mode-argument)

(vim:defcmd vim:cmd-setmode ((argument:mode mode) nonrepeatable)
  "Changes the default start mode of the current major-mode."
  (when (zerop (length mode)) (setq mode nil))
  (let ((valid-mode (rassoc mode vim:mode-alist))
	(start-mode (and mode (intern mode))))
    (if (and start-mode (not valid-mode))
	(error "No such vim-mode submode: %s" start-mode)
      
      (with-current-buffer vim:ex-current-buffer
	(let* ((mmode (assoc major-mode vim:initial-modes))
	       (current-mode (cdr-safe mmode)))
	  (unless (eq current-mode start-mode)
	    ;; only if we selected a new mode
	    (when (y-or-n-p (format "Major-mode `%s' has initial mode `%s'. Change to `%s'? "
				    major-mode
				    (or current-mode "DEFAULT")
				    (or start-mode "DEFAULT")))
	      (setq vim:initial-modes (assq-delete-all major-mode vim:initial-modes))
	      (when start-mode
		(push (cons major-mode start-mode) vim:initial-modes))
	      (when (y-or-n-p "Save setting in customization file? ")
		(customize-save-variable 'vim:initial-modes vim:initial-modes)))))))))


(provide 'vim-ex-commands)

;;; vim-ex-commands.el ends here
