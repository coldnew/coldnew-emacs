;;; nnnil.el: empty, read-only backend for Gnus              -*- emacs-lisp -*-
;;; <URL:http://multivac.cwru.edu/gnus/#nnnil>
;;; Written and placed in the public domain by Paul Jarc <prj@po.cwru.edu>.  
;;; This backend is suitable for use as the primary server when real servers
;;; are to be secondary or foreign.

(eval-and-compile (require 'gnus-start))

(gnus-declare-backend "nnnil" 'none)

(defvar nnnil-status-string "")

(defun nnnil-retrieve-headers (articles &optional group server fetch-old)
  'nov)

(defun nnnil-open-server (server &optional definitions)
  t)

(defun nnnil-close-server (&optional server)
  t)

(defun nnnil-request-close ()
  t)

(defun nnnil-server-opened (&optional server)
  t)

(defun nnnil-status-message (&optional server)
  nnnil-status-string)

(defun nnnil-request-article (article &optional group server to-buffer)
  (setq nnnil-status-string "No such group")
  nil)

(defun nnnil-request-group (group &optional server fast)
  (let (deactivate-mark)
    (save-excursion
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (insert "411 no such news group\n")))
  (setq nnnil-status-string "No such group")
  nil)

(defun nnnil-close-group (group &optional server)
  t)

(defun nnnil-request-list (&optional server)
  t)

(defun nnnil-request-post (&optional server)
  (setq nnnil-status-string "Read-only server")
  nil)

(provide 'nnnil)
