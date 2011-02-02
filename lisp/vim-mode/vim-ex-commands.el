;;; vim-ex-commands.el - Implementation of some ex-mode commands.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(defun* vim:save-buffer (file-name &key begin end mustbenew append)
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
        (write-file file-name))
       (t
        (write-region beg-pos end-pos file-name append nil nil mustbenew))))))


(vim:defcmd vim:cmd-write (motion (argument:file file) nonrepeatable)
  "Saves file `file'."
  (vim:save-buffer file
                   :begin (and motion (vim:motion-first-line motion))
                   :end (and motion (vim:motion-last-line motion))
                   :mustbenew t))

(vim:defcmd vim:cmd-write-q (motion (argument:file file) nonrepeatable)
  "Overwrites file `file'."
  (vim:save-buffer file
                   :begin (and motion (vim:motion-first-line motion))
                   :end (and motion (vim:motion-last-line motion))
                   :mustbenew nil))

(vim:defcmd vim:cmd-write-all (nonrepeatable)
  "Saves all buffers."
  (save-some-buffers nil))

(vim:defcmd vim:cmd-write-all-q (nonrepeatable)
  "Overwrites all buffers."
  (save-some-buffers t))

(vim:defcmd vim:cmd-edit ((argument:file file) nonrepeatable)
  "Visits a certain file."
  (if file
      (find-file file)
    (when (buffer-file-name)
      (find-file (buffer-file-name)))))

(vim:defcmd vim:cmd-buffer ((argument:buffer buffer) nonrepeatable)
  "Switches to another buffer."
  (if buffer
      (when (or (get-buffer buffer)
                (y-or-n-p (format "No buffer with name \"%s\" exists. Create new buffer? " buffer)))
        (switch-to-buffer buffer))
    (switch-to-buffer (other-buffer))))

(vim:defcmd vim:cmd-delete-buffer ((argument:buffer buffer) nonrepeatable)
  "Deletes a buffer."
  (kill-buffer buffer))

(vim:defcmd vim:cmd-delete-buffer-q ((argument:buffer buffer) nonrepeatable)
  "Deletes a buffer without saving."
  (if buffer
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
    (set-buffer-modified-p nil))
  (kill-buffer buffer))

(vim:defcmd vim:cmd-quit (nonrepeatable)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error (save-buffers-kill-emacs))))))

(vim:defcmd vim:cmd-quit-q (nonrepeatable)
  "Closes the current window, exits Emacs if this is the last window."
  (condition-case nil
      (delete-window)
    (error
     (condition-case nil
         (delete-frame)
       (error (kill-emacs))))))

(vim:defcmd vim:cmd-quit-all (nonrepeatable)
  "Exits Emacs, asking for saving."
  (save-buffers-kill-emacs))

(vim:defcmd vim:cmd-quit-all-q (nonrepeatable)
  "Exits Emacs, without saving."
  (kill-emacs))

(vim:defcmd vim:cmd-save-and-quit (nonrepeatable)
  "Exits Emacs, without saving."
  (save-buffers-kill-emacs 1))

(vim:defcmd vim:cmd-save-and-close ((argument:file file) nonrepeatable)
  "Saves the current buffer and closes the window."
  (vim:cmd-write :argument file)
  (vim:cmd-quit))

(vim:defcmd vim:cmd-save-and-close-q ((argument:file file) nonrepeatable)
  "Saves the current buffer and closes the window."
  (vim:cmd-write-q :argument file)
  (vim:cmd-quit))

(provide 'vim-ex-commands)

;;; vim-ex-commands.el ends here
