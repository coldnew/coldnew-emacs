
;;; completion-ui-tooltip.el --- hotkey user-interface for Completion-UI


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.1
;; Keywords: completion, user interface, hotkey
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.1.1
;; * changed `completion-[auto-show]-[de]activate-hotkeys' to cope with
;;   vectors in `completion-hotkey-list'
;;
;; Version 0.1
;; * initial version (split off from completion-ui.el)


;;; Code:

(eval-when-compile (require 'cl))

(provide 'completion-ui-hotkeys)
(require 'completion-ui)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-hotkeys nil
  "Completion-UI hotkeys user interface."
  :group 'completion-ui)


(defcustom completion-use-hotkeys t
  "*Enable completion hotkeys (single-key selection of completions).

If t, enable hotkeys whenever completions are available. If nil,
disable hotkeys entirely. If set to the symbol 'auto-show, only
enable hotkeys when the `completion-auto-show' interface is
active. (Note that because the completion menu steals keyboard
focus, enabling hotkeys when the menu is active has no effect. So
don't try to report this as a bug!)"
  :group 'completion-ui-hotkeys
  :type '(choice (const t)
                 (const auto-show)
                 (const nil)))


(defcustom completion-hotkey-list '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "*List of keys (vectors) to use for selecting completions
when `completion-use-hotkeys' is enabled."
  :group 'completion-ui-hotkeys
  :type '(repeat character))



;;; ============================================================
;;;                 Interface functions

(defun completion-activate-hotkeys (overlay)
  "Activate completion hotkeys for OVERLAY."
  ;; activate keys unless only active when auto-show interface is displayed
  (unless (eq completion-use-hotkeys 'auto-show)
    (dolist (key completion-hotkey-list)
      (define-key (overlay-get overlay 'keymap)
	(if (vectorp key) key (vector key))
	'completion-hotkey-select))))


(defun completion-auto-show-activate-hotkeys (overlay)
  "Activate completion hotkeys for OVERLAY
when auto-show interface is displayed."
  (when (eq completion-use-hotkeys 'auto-show)
    (dolist (key completion-hotkey-list)
      (define-key (overlay-get overlay 'keymap)
	(if (vectorp key) key (vector key))
	'completion-hotkey-select))))


(defun completion-deactivate-hotkeys (overlay)
  "Deactivate completion hotkeys for OVERLAY."
  (dolist (key completion-hotkey-list)
    (define-key (overlay-get overlay 'keymap)
      (if (vectorp key) key (vector key)) nil)))


(defun completion-hotkey-select ()
  "Select completion corresponding to the last input event
when hotkey completion is active.

Characters in `completion-hotkey-list' get bound to this
internally. It should *never* be bound in a keymap."
  (interactive)

  (let ((overlay (completion-ui-overlay-at-point))
	key n)
    ;; resolve any other old provisional completions
    (completion-ui-resolve-old overlay)
    ;; find completion index corresponding to last input event
    (setq unread-command-events (listify-key-sequence (this-command-keys))
	  key (read-key-sequence-vector ""))
    ;; FIXME: work around apparent bug where keys are doubled in vector
    (when (> (length key) 1) (setq key (vector (aref key 0))))
    (setq n
	  (completion--position key (mapcar 'vector completion-hotkey-list)))

    (cond
     ;; if there are no completions, run whatever would otherwise be bound to
     ;; the key
     ;; FIXME: could also do this if there are too few completions, but maybe
     ;;        that would be confusing to the user?
     ((or (null overlay) (null (overlay-get overlay 'completions)))
      (when completion--trap-recursion
	(error "Recursive call to `completion-hotkey-select'"))
      (completion-deactivate-hotkeys overlay)
      (let ((completion--trap-recursion t))
	(unwind-protect
	    (command-execute (key-binding key t))
	  (completion-activate-hotkeys overlay))))

     ;; if there are completions, select completion corresponding to hotkey
     (t (completion-select n overlay)))))



;;; =================================================================
;;;                    Register user-interface

(completion-ui-register-interface
 'completion-use-hotkeys
 :activate 'completion-activate-hotkeys
 :deactivate 'completion-deactivate-hotkeys
 :auto-show-helper 'completion-auto-show-activate-hotkeys)



;;; completion-ui-hotkeys.el ends here
