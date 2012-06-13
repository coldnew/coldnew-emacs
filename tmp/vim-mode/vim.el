;;; vim.el --- a VIM-emulation for Emacs

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; Maintainer: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;; URL: http://www.emacswiki.org/emacs/VimMode 
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Emacs 22, 23
;; Version: 0.4.0
;; Keywords: emulation, vim
;; Human-Keywords: vim, emacs
;;
;; This file is not part of GNU Emacs.

;;; Acknowledgements:

;; This package contains code from several other packages:
;;
;; - rect-mark.el
;; - viper
;; - vimpulse.el
;; - windmove.el
;;
;; Special thanks go to the authors of those packages.

;;; Commentary:

;; A simple VIM-mode for Emacs
;;
;; This project is in an early development state and many function
;; have not been implemented yet.
;;
;; If you want to try, open this file in your Emacs and evaluate the buffer.
;; The mode can be activated by 'M-x vim-mode'.
;;
;; Don't forget to disable Viper if you want to try vim-mode.
;;
;; The project is divided into many files. Each file implements some
;; almost-independent feature. If you want to learn how to implement
;; new commands or motions, look at the files vim-commands.el and
;; vim-motions.el.
;;
;; Here is a short description of the contents of each file:
;;
;;  - vim.el:  This file just sets up the mode and loads the other files.
;;
;;  - vim-compat.el: Compatibility layer for different Emacsen.
;;
;;  - vim-keymap.el: A few functions for defining keymaps for vim-mode.
;;
;;  - vim-macs.el: This file contains the macros for defining motions
;;                and commands.
;;
;;  - vim-defs.el: Global variables.
;;
;;  - vim-core.el: Controlling of active modes and execution of
;;                 motions and commands.
;;
;;  - vim-modes.el: Each VIM-mode (normal-mode, insert-mode, ...) corresponds
;;                  to an Emacs-minor-mode. This file contains some macros and
;;                  functions to define new vim-modes in this context.   
;;
;;  - vim-insert-mode.el: The implementation of insert-mode.         
;;                                                                    
;;  - vim-normal-mode.el: The implementation of normal-mode.         
;;                                                                    
;;  - vim-visual-mode.el: The implementation of visual-mode.         
;;
;;  - vim-ex.el: The implementation of ex-mode.         
;;                                                                    
;;  - vim-commands.el: The implementations of commands like 'delete', 
;;                     'yank', 'paste' and so on.               
;;
;;  - vim-motions.el: The implementations of motion commands like 'h',
;;                    'i', 'j', 'k', 'f', 'w', ...
;;
;;  - vim-scroll.el: The implementation of scrolling commands like
;;                   'zz', 'Ctrl-F'.
;;
;;  - vim-window-el: The implementation of window commands like 'C-w s'.
;;
;;  - vim-ex-commands.el: The implementations of commands like ':edit'
;;                        or ':buffer'.
;;
;;  - vim-search.el: The implementation of '/' and ':substitute'.
;;
;;  - vim-undo.el: Some variables and functions for undo/redo.
;;
;;  - vim-maps.el: The definition of the basic keymaps.  This file
;;                 connects the keymaps with the commands and motions
;;                 defined in vim-commands.el and vim-motions.el.

;;; Changelog:

;; version 0.5
;;     * add ]p and ]P commands
;;     * paste-pop works for all mixes of block/linewise/char and for
;;       paste-before and paste-behind
;;     * add :setmode ex-command for setting vim-mode's start-mode
;;       for the current major-mode
;;     * enable search commands /, ?, *, #, g*, g# in motion-mode,
;;       repeat search with C-n and C-N instead of n and N
;;     * : starts ex-mode in motion-mode
;;     * C-: starts ex-mode in window-mode
;;     * command can take an addition force argument which is set
;;       to non-nil iff an exclamation mark has been typed behind
;;       the command in ex-mode
;;     * ex-commands :bn, :bp 
;;     * ex-mode shows info about current command

;;; Code:

(eval-when-compile
  (require 'cl))

(let ((load-path (cons (expand-file-name ".") load-path)))
  (eval-when-compile
    (load "vim-core")
    (load "vim-compat")
    (load "vim-normal-mode")
    (load "vim-keymap")
    (load "vim-maps"))
  
  (require 'vim-core)
  (require 'vim-compat)
  (require 'vim-normal-mode)
  (require 'vim-keymap)
  (require 'vim-maps))


(defgroup vim-mode nil
  "A VIM emulation mode."
  :group 'emulations)

(defgroup vim-mode-general nil
  "General options for Vim-Mode"
  :group 'vim-mode)

(defcustom vim:default-initial-mode
  'normal
  "The default initial vim sub-mode."
  :type '(symbol :tag "vim-mode start mode")
  :group 'vim-mode-general)

(defcustom vim:initial-modes
  '((debugger-mode . window)
    (compilation-mode . window)
    (grep-mode . window)
    (gud-mode . window)
    (sldb-mode . window)
    (slime-repl-mode . window)
    (reftex-select-bib-mode . window)
    (completion-list-mode . window)
    (help-mode . motion)
    (Info-mode . motion))
  "Associated list of (major-mode . vim:mode) which specifies the
vim sub-mode in which vim-mode should start when a buffer with the
given major-mode is created."
  :type '(repeat (cons (symbol :tag "major mode") (symbol :tag "vim-mode start mode")))
  :group 'vim-mode-general)


(define-minor-mode vim-local-mode
  "VIM emulation mode."
  :lighter " VIM"
  :init-value nil
  :global nil

  (if vim-local-mode
      (progn
        (ad-enable-advice 'show-paren-function 'around 'vim:show-paren-function)
        (ad-activate 'show-paren-function)
        (make-local-variable 'vim:emulation-mode-alist)
        (vim:initialize-keymaps t))
    (progn
      (ad-disable-advice 'show-paren-function 'around 'vim:show-paren-function)
      (ad-activate 'show-paren-function)
      (vim:initialize-keymaps nil)
      (setq global-mode-string
            (delq 'vim:mode-string global-mode-string ))
      (vim:activate-mode nil))))
  
(define-globalized-minor-mode vim-mode vim-local-mode vim:initialize)

(defun vim:initialize ()
  (unless (vim:minibuffer-p)
    (let ((mode (cdr (or (assoc major-mode vim:initial-modes)
                         (cons t vim:default-initial-mode)))))
      (when mode
        (setq vim:active-mode nil)
        (vim-local-mode 1)
        (vim:intercept-ESC-mode 1)
        (vim:activate-mode mode)
        (unless (memq 'vim:mode-string global-mode-string)
          (setq global-mode-string
                (append '("" vim:mode-string) (cdr global-mode-string))))))))


(defcustom vim:show-paren-range
  0
  "The minimal distance between point and a parenthesis which
causes the parenthesis to be highlighted."
  :type 'integer
  :group 'vim-mode-general)


(defadvice show-paren-function (around vim:show-paren-function)
  "Advices show-paren-function so also parentheses near point are matched."
  (save-excursion
    (goto-char
     (or (catch 'end
           (save-excursion
             (dotimes (d (1+ (* 2 vim:show-paren-range)))
               (forward-char (if (evenp d) d (- d)))
               (let ((sc (syntax-class (syntax-after (point)))))
                 (case sc
                   (4 (throw 'end (point)))
                   (5 (throw 'end (1+ (point)))))))
             nil))
         (point)))
    ad-do-it))



(provide 'vim)

;;; vim.el ends here
