;;; makefile-script.el --- Building script for coldnew's emacs

;; Copyright (c) 2013 - 2016 Yen-Chin, Lee.
;;
;; Author: coldnew <coldnew.tw@gmail.com>
;; Keywords: converience
;; X-URL: http://github.com/coldnew/coldnew-emacs
;; Version: 0.1

;; This file is not part of GNU Emacs.

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
;; This file is desinged to used by Makefile.

;;; Code:
(require 'org)
(require 'subr-x)
(require 'find-lisp)

;;;; Basic Functions
(defun find-tmpfile-path ()
  "Find suitable tmpfile path."
  (let ((my-ramdisk "/Volumes/ramdisk"))
    ;; I always create `/Volumes/ramdisk' for ramdisk usage on OSX
    (if (and (eq system-type 'darwin) (file-exists-p my-ramdisk))
        my-ramdisk
        temporary-file-directory)))

(defun dirjoin (root &rest dirs)
  "Joins ROOT and DIRS together."
  (if (not dirs)
      root
      (apply 'dirjoin
             (expand-file-name (car dirs) root)
             (cdr dirs))))

;;;; Variables

(defconst *workdir* (dirjoin (find-tmpfile-path) ".emacs-init-build")
  "Working directory for building emacs-lisp configs.")

(defconst *pid* (number-to-string (emacs-pid))
  "Current process pid in string.")

(defconst *lockfile* (dirjoin *workdir* "lock")
  "A file store pid info.")

(defconst *configs-dir* "configs"
  "Other `org-mode' configs path.")

;;;; Functions

(defun mkdir-1 (dirname)
  "Create directory DIRNAME and it's parents."
  (when (not (file-exists-p dirname))
    (make-directory dirname :parents)))

(defun find-org-files (dirname)
  "Return a list store all .org file in DIRNAME."
  (thread-last
      (directory-files dirname nil "\\.org$")
    (mapcar (lambda (fn) (format "%s/%s" dirname fn)))))

(defun find-tmp-el-files ()
  "Return a list store all .pid files."
  (find-lisp-find-files *workdir* (concat "\\." *pid* "$")))

(defun delete-tmp-files ()
  "Delete all temp file generate by this script."
  (dolist (f (find-tmp-el-files))
    (delete-file f nil)))

(defun org->el (forg)
  "Convert `org-mode' file FORG to emacs-lisp file in *workdir*."
  (let* ((dir (dirjoin *workdir* (or (file-name-directory forg) "")))
         (base (file-name-base forg))
         (fel (dirjoin dir (concat base ".el" "." *pid*))))
    (mkdir-1 dir)
    (message (format "Building %s to %s ..." forg fel))
    (org-babel-tangle-file forg fel)))

(defun write-lockfile (pid)
  "Create lockfile with PID as it's contents."
  (mkdir-1 (file-name-directory *lockfile*))
  (with-temp-buffer
    (insert pid)
    (write-file *lockfile*)))

(defun read-lockfile ()
  "Return lockfile contents."
  (with-temp-buffer
    (insert-file-contents *lockfile*)
    (buffer-string)))

(defun update-config (forg)
  "Update FORG's relative .el file from *workdir*."
  (let* ((bdir (or (file-name-directory forg) ""))
         (base (file-name-base forg))
         (dir  (dirjoin *workdir* bdir))
         (fel  (dirjoin dir (concat base ".el" "." *pid*)))
         (fel2 (dirjoin bdir (concat base ".el"))))
    (message (format "Create %s from %s..." fel2 forg))
    (rename-file fel fel2 t)))

(defun find-all-org-configs ()
  "Return a list store all `org-mode' configs."
  (cons "init.org"
        (find-org-files "configs")))

(defun make-init-el ()
  "Build all .org configs to emacs-lisp file."
  (setq org-confirm-babel-evaluate nil)
  ;; Create lockfile with pid
  (write-lockfile *pid*)
  ;; tangle init.el and other configs
  (let ((configs (find-all-org-configs)))
    ;; tangle all .org files
    (dolist (config configs)
      (org->el config))
    ;; Since tnagle may use so many time, check if lock file still has
    ;; the same pid as this process.
    ;; If not, which means current process is really old, just delete the generate files and exit.
    ;; Move all generate .el files after success.
    (if (string= *pid* (read-lockfile))
        ;; when pid the same, create our .el files
        (progn
          (message "OK, time to update all .el files.")
          (dolist (config configs)
            (update-config config)))
        ;; else just kill old one
        (delete-tmp-files))))

;;; makefile-script.el ends here