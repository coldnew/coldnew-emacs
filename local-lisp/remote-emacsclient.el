(require 'tramp)
(require 'tramp-sh)

(setq server-use-tcp t)
(server-start)

(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun update-tramp-emacs-server-port-forward (method-name)
  "Update the specified TRAMP's method to forward the Emacs
 server port to the local host. This lets emacsclient on the
 remote host open files in the local Emacs server."
  (let* ((method (assoc method-name tramp-methods))
         (ssh-args (cadr (assoc 'tramp-login-args method))))
    (put-alist 'tramp-login-args
      (list (put-alist "-R" (let ((port
                                   (process-contact server-process :service)))
        ;; put-alist makes a dotted pair for the key/value, but tramp-methods
        ;; needs a normal list, so put the value inside a list so that the
        ;; second part of the dotted pair (ie the cdr) is a list, which
        ;; converts it from a dotted pair into a normal list.
                              (list (format "%s:127.0.0.1:%s" port port)))
                       ssh-args))
      method)))

(defun tramp-make-tramp-file-name-from-vec (vec file)
  "Convenience function for making a TRAMP path, since this
apparently didn't already exist."
  (tramp-make-tramp-file-name
    (tramp-file-name-method vec)
    (tramp-file-name-user vec)
    (tramp-file-name-host vec)
    file))

(defcustom tramp-default-remote-emacsclient-auth-file
  "~/.emacs.d/remote-server"
  "Default remote path at which to save the remote emacsclient
authentication file. This can be a string or nil to disable
saving an authentication file.

The authentication file is similar to the one written out by the
emacsclient TCP server, except it includes the prefix used for
the TRAMP connection to the remote server."
  :group 'tramp
  :type '(choice (const nil) string))

(defcustom tramp-remote-emacsclient-auth-file-alist nil
  "The remote emacsclient authentication file path to use for
specific host/user pairs. This is an alist of items (HOST USER
PATH). The first matching item specifies the path to use for a
connection which does not specify a method. HOST and USER are
regular expressions or nil, which is interpreted as a regular
expression which always matches. If no entry matches, the
variable `tramp-default-remote-emacsclient-auth-file' takes
effect.

If the connection does not specify the user, lookup is done using
the empty string for the user name.

See `tramp-default-remote-emacsclient-auth-file' for an
explanation of the auth file path."
  :group 'tramp
  :type '(repeat (list (choice :tag "Host regexp" regexp (const nil))
		       (choice :tag "User regexp" regexp (const nil))
		       (choice :tag "emacsclient auth path" string (const nil)))))

(defun tramp-get-remote-emacsclient-auth-file (vec)
  "Determine the full TRAMP path for the remote emacsclient
authentication file, given a connection vector."
  (let
      ((auth-file
        (let ((choices tramp-remote-emacsclient-auth-file-alist)
              (host (or (tramp-file-name-host vec) ""))
              (user (or (tramp-file-name-user vec) ""))
              lfile item matched)
          (while choices
            (setq item (pop choices))
            (when (and (string-match (or (nth 0 item) "") host)
                       (string-match (or (nth 1 item) "") user))
              (setq lfile (nth 2 item)
                    choices nil
                    matched t)))
          (if matched lfile tramp-default-remote-emacsclient-auth-file))))
    (if auth-file
        (tramp-make-tramp-file-name-from-vec vec auth-file))))

(defun tramp-save-remote-emacsclient-auth-file (&optional vec)
  "Write the remote emacsclient authentication file for a given
connection buffer, or, if used interactively, for the TRAMP
connection of the current buffer."
  (interactive)
  (let ((vec (or vec (tramp-dissect-file-name default-directory))))
    (condition-case err
        (let ((auth-file (tramp-get-remote-emacsclient-auth-file vec))
              (server (process-contact server-process :local)))
          (if auth-file
              (with-temp-file auth-file
                (insert
                 (format "127.0.0.1 %d\n" (elt server (- (length server) 1)))
                 (format "-auth %s\n" (process-get server-process :auth-key))
                 (server-quote-arg (tramp-make-tramp-file-name-from-vec vec ""))
                 "\n"))
            (when (called-interactively-p 'any)
              (message "No remote emacsclient auth file for %s"
                       default-directory))))
      (file-error (message "error saving remote emacsclient auth: %s" err)))))

(defadvice tramp-open-connection-setup-interactive-shell
  (after copy-server-file-by-tramp (proc vec) activate)
  "Automatically write out a remote emacsclient auth file after a
successful connection."
  (tramp-save-remote-emacsclient-auth-file vec))

(provide 'remote-emacsclient)
