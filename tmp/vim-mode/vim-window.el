;;; vim-window.el - Implementation of window commands.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains implementations for the window operations.
;; Window operations are usually just simple commands and should not
;; be repeatable.

;;; Code:

(eval-when-compile (require 'cl))
(require 'vim-macs)
(require 'vim-core)
(require 'vim-ex-commands)

(condition-case nil
    (require 'windmove)
  (error
   (message "vim-mode: Could not load 'windmove', window-commands not available.")
   nil))

(defun vim:resize-window (new-size &optional horizontal)   
  "Sets the current window's with or height to `new-size'."
  (let ((wincfg (current-window-configuration))
	(nwins (length (window-list)))
	(count (if horizontal
		   (- new-size (window-width))
		 (- new-size (window-height)))))

    (catch 'loop
      (save-window-excursion
	(while (not (zerop count))
	  (if (> count 0)
	      (progn (enlarge-window 1 horizontal) (decf count))
	    (progn
	      (shrink-window 1 horizontal)
	      (incf count)))
	  (if (= nwins (length (window-list)))
	      (setq wincfg (current-window-configuration))
	    (throw 'loop t)))))
    (set-window-configuration wincfg)))
         

(defun vim:get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'vim:get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))


(defun vim:restore-window-tree (win tree)
  "Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (vim:restore-window-tree win (cadr tree))
      (vim:restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t (set-window-buffer win tree))))


(vim:defcmd vim:window-split (count (argument:file file) nonrepeatable)
  "Splits the current window horizontally, `count' lines height, editing a certain `file'."            
  (let ((new-win (split-window (selected-window) count)))
    (when file
      (vim:cmd-edit :argument file))))


(vim:defcmd vim:window-vsplit (count (argument:file file) nonrepeatable)
  "Splits the current window vertically, `count' columns width, editing a certain `file'."            
  (let ((new-win (split-window (selected-window) count t)))
    (when file
      (vim:cmd-edit :argument file))))


(vim:defcmd vim:window-close (nonrepeatable)
  "Closes the current window."
  (delete-window))


(vim:defcmd vim:window-only (nonrepeatable)
  "Closes all but the current window."
  (delete-other-windows))


(vim:defcmd vim:window-left (count nonrepeatable)
  "Move the cursor to new `count'-th window left of the current one."
  (dotimes (i (or count 1))
    (windmove-left)))


(vim:defcmd vim:window-right (count nonrepeatable)
  "Move the cursor to new `count'-th window right of the current one."
  (dotimes (i (or count 1))
    (windmove-right)))


(vim:defcmd vim:window-up (count nonrepeatable)
  "Move the cursor to new `count'-th window above the current one."
  (dotimes (i (or count 1))
    (windmove-up)))


(vim:defcmd vim:window-down (count nonrepeatable)
  "Move the cursor to new `count'-th window below the current one."
  (dotimes (i (or count 1))
    (windmove-down)))


(vim:defcmd vim:window-bottom-right (nonrepeatable)
  "Move the cursor to bottom-right window."
  (do ((success t))
      ((not success))
    (setq success nil)
    (condition-case nil
        (progn
          (windmove-right)
          (setq success t))
      (error nil))
    (condition-case nil
        (progn
          (windmove-down)
          (setq success t))
      (error nil))))
     

(vim:defcmd vim:window-top-left (nonrepeatable)
  "Move the cursor to top-left window."
  (do ((success t))
      ((not success))
    (setq success nil)
    (condition-case nil
        (progn
          (windmove-left)
          (setq success t))
      (error nil))
    (condition-case nil
        (progn
          (windmove-up)
          (setq success t))
      (error nil))))


(vim:defcmd vim:window-lru (nonrepeatable)
  "Move the cursor to the previous (last accessed) window."
  (select-window (get-lru-window)))
            

(vim:defcmd vim:window-next (count nonrepeatable)
  "Move the cursor to the next window in the cyclic order.
With `count' go to the count-th window in the order starting from
top-left."
  (if (not count)
      (select-window (next-window))
    (vim:window-top-left)
    (other-window (1- (or count 1)))))


(vim:defcmd vim:window-prev (count nonrepeatable)
  "Move the cursor to the previous window in the cyclic order.
With `count' go to the count-th window in the order starting from
top-left."
  (if (not count)
      (select-window (previous-window))
    (vim:window-top-left)
    (other-window (1- (or count 1)))))


(vim:defcmd vim:window-new (count (argument:file file) nonrepeatable)
  "Splits the current window horizontally and opens a new buffer or edits a certain `file'."
  (split-window (selected-window) count)
  (if file
      (vim:cmd-edit :argument file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer (normal-mode)))))
  


(vim:defcmd vim:window-vnew (count (argument:file file) nonrepeatable)
  "Splits the current window vertically and opens a new buffer name or edits a certain `file'."
  (split-window (selected-window) count t)
  (if file
      (vim:cmd-edit :argument file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer (normal-mode)))))


(vim:defcmd vim:window-balance (nonrepeatable)
  "Balances all window sizes."
  (balance-windows))


(vim:defcmd vim:window-increase-height (count nonrepeatable)
  "Increase current window height by `count'."
  (vim:resize-window (+ (window-height) (or count 1))))
            

(vim:defcmd vim:window-decrease-height (count nonrepeatable)
  "Decrease current window height by `count'."
  (vim:resize-window (- (window-height) (or count 1))))


(vim:defcmd vim:window-increase-width (count nonrepeatable)
  "Increase current window width by `count'."
  (vim:resize-window (+ (window-width) (or count 1)) t))
            

(vim:defcmd vim:window-decrease-width (count nonrepeatable)
  "Decrease current window width by `count'."
  (vim:resize-window (- (window-width) (or count 1)) t))


(vim:defcmd vim:window-set-height (count nonrepeatable)
   "Sets the height of the current window to `count'."
   (vim:resize-window (or count (frame-height)) nil))


(vim:defcmd vim:window-set-width (count nonrepeatable)
   "Sets the width of the current window to `count'."
   (vim:resize-window (or count (frame-width)) t))


(vim:defcmd vim:window-rotate-upwards (nonrepeatable)
   "Rotates the windows according to the currenty cyclic ordering."
   (let ((wlist (window-list))
         (blist (mapcar #'(lambda (w) (window-buffer w))
                        (window-list))))
     (setq blist (append (cdr blist) (list (car blist))))
     (while (and wlist blist)
       (set-window-buffer (car wlist) (car blist))
       (setq wlist (cdr wlist)
             blist (cdr blist)))
     (select-window (car (last (window-list))))))
     
     
(vim:defcmd vim:window-rotate-downwards (nonrepeatable)
   "Rotates the windows according to the currenty cyclic ordering."
   (let ((wlist (window-list))
         (blist (mapcar #'(lambda (w) (window-buffer w))
                        (window-list))))
     (setq blist (append (last blist) blist))
     (while (and wlist blist)
       (set-window-buffer (car wlist) (car blist))
       (setq wlist (cdr wlist)
             blist (cdr blist)))
     (select-window (cadr (window-list)))))


(vim:defcmd vim:window-move-very-top (nonrepeatable)
   "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((newwin (selected-window))
               (subwin (split-window)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))


(vim:defcmd vim:window-move-far-left (nonrepeatable)
   "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((newwin (selected-window))
               (subwin (split-window-horizontally)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
     

(vim:defcmd vim:window-move-far-right (nonrepeatable)
   "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((subwin (selected-window))
               (newwin (split-window-horizontally)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
     

     

(vim:defcmd vim:window-move-very-bottom (nonrepeatable)
   "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
   (unless (one-window-p)
     (let ((b (current-buffer)))
       (delete-window)
       (let ((btree (vim:get-buffer-tree (car (window-tree)))))
         (vim:window-only)
         (let ((subwin (selected-window))
               (newwin (split-window)))
           (vim:restore-window-tree subwin btree)
           (set-window-buffer newwin b)
           (select-window newwin))))
     (balance-windows)))
        

(provide 'vim-window)

;;; vim-window.el ends here
