
;;; completion-ui-echo.el --- echo area user-interface for Completion-UI


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: completion, user interface, echo area, help-echo
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
;; Version 0.1
;; * initial version (split off from completion-ui.el)


;;; Code:

(eval-when-compile (require 'cl))

(provide 'completion-ui-popup-frame)
(require 'completion-ui)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-popup-frame nil
  "Completion-UI pop-up frame user interface."
  :group 'completion-ui)


(defcustom completion-use-popup-frame t
  "*Enable the completion pop-up frame."
  :group 'completion-ui-popup-frame
  :type 'boolean)


(defcustom completion-popup-frame-max-height 20
  "*Maximum height of a popup frame"
  :group 'completion-ui-popup-frame
  :type 'integer)


(defcustom completion-popup-frame-offset '(0 . 0)
  "Pixel offset for pop-up frame.
This sometimes needs to be tweaked manually to get the pop-up
frame in the correct position under different window systems."
  :group 'completion-ui-popup-frame
  :type '(cons (integer :tag "x") (integer :tag "y")))



;;; ============================================================
;;;               Other configuration variables

(defvar completion-popup-frame-function nil
  "Function to call to construct pop-up frame text.

The function is called with two arguments, PREFIX and
COMPLETIONS. It should return a list of strings, which are used
\(in order\) as the lines of text in the pop-up frame.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-popup-frame-map nil
  "Keymap used by completion pop-up frames.")


(defvar completion-popup-frame-mode-map nil
  "Keymap used in completion pop-up frames.")



;;; =================================================================
;;;                     Setup default keymaps

;; Set default keybindings added to the overlay keymap, unless it's already
;; been set (most likely in an init file).
(unless completion-popup-frame-map
  (setq completion-popup-frame-map (make-sparse-keymap))
  ;; C-<down> displayes the completion pop-up frame
  (define-key completion-popup-frame-map [C-down]
    'completion-activate-popup-frame))



;; Set default keybindings for the keymap used in completion pop-up
;; frames (actually, used by the completion-popup-frame major mode),
;; unless it's already been set (most likely in an init file).
(unless completion-popup-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map "\C-n" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map [down] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map "\M-p" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'previous-line arg)))
    (define-key map "\C-p" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'previous-line arg)))
    (define-key map [up] (lambda (&optional arg) (interactive)
                           (completion-popup-frame-motion
                            'previous-line arg)))
    (define-key map "\C-v" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-up arg)))
    (define-key map [next] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-up arg)))
    (define-key map "\M-v" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-down arg)))
    (define-key map [prior] (lambda (&optional arg) (interactive)
                              (completion-popup-frame-motion
                               'scroll-down arg)))
    (define-key map [home] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'beginning-of-buffer arg)))
    (define-key map "\M-<" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'beginning-of-buffer arg)))
    (define-key map [end] (lambda (&optional arg) (interactive)
                            (completion-popup-frame-motion
                             'end-of-buffer arg)))
    (define-key map "\M->" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'end-of-buffer arg)))
    (define-key map "\C-u" 'universal-argument)
    (define-key map [?\C--] 'negative-argument)
    (define-key map [C-up] 'completion-popup-frame-dismiss)
    (define-key map [M-up] 'completion-popup-frame-dismiss)
    (define-key map [?\M-\t] 'completion-popup-frame-toggle-show-all)
    (define-key map "\M-/" 'completion-popup-frame-toggle-show-all)
    (define-key map [t] 'completion-popup-frame-unread-key)
    (setq completion-popup-frame-mode-map map)))



;;; ============================================================
;;;                     Internal variables

(defvar completion-popup-frame-parent-frame nil
  "Stores the parent frame of a popup frame.")
(make-variable-buffer-local 'completion-popup-frame-parent-frame)


(defvar completion-popup-frame-parent-overlay nil
  "Stores the parent completion overlay of a popup frame.")
(make-variable-buffer-local 'completion-popup-frame-parent-overlay)


(defvar completion-popup-frame-overlay nil
  "Stores pop-up frame overlay used to highlight selected completion.")
(make-variable-buffer-local 'completion-popup-frame-overlay)


(defvar completion-popup-frame-show-all nil
  "Non-nil when all completions are shown in a pop-up frame.")
(make-variable-buffer-local 'completion-popup-frame-show-all)



;;; ============================================================
;;;                 Interface functions

(defun completion-activate-popup-frame (&optional overlay all)
  "Show the completion pop-up frame.
With a prefix argument, display all possible completions in the
pop-up frame, rather than just the first few."
  (interactive (list nil current-prefix-arg))
  ;; look for completion overlay at point, unless one was supplied
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; deactivate other auto-show interfaces and call auto-show-helpers
  (completion-ui-deactivate-auto-show-interface overlay)
  (completion-ui-call-auto-show-interface-helpers overlay)
  ;; show the completion menu
  (completion-popup-frame overlay)
  (when all (completion-popup-frame-toggle-show-all)))


(defun completion-update-popup-frame (overlay)
  "Update the completion pop-up frame if it's being displayed."
  (when (overlay-get overlay 'popup-frame) (completion-popup-frame overlay)))


(defun completion-activate-popup-frame-keys (overlay)
  "Enable pop-up frame key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) binding))
   completion-popup-frame-map))


(defun completion-deactivate-popup-frame-keys (overlay)
  "Disable tooltip key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) nil))
   completion-popup-frame-map))



(defun completion-popup-frame (&optional overlay)
  "Pop up a frame at point displaying the completions for OVERLAY.
The point had better be within OVERLAY or your aubergines will be
cursed for a hundred years \(that's eggplant for any Americans
out there\).

If no OVERLAY is supplied, tries to find one at point."
  (interactive)

  ;; if none was supplied, find overlay at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  (when (and overlay window-system)
    (let* ((parent-frame (selected-frame))
           (prefix (overlay-get overlay 'prefix))
           (completions (overlay-get overlay 'completions))
           (num (overlay-get overlay 'completion-num))
           (popup-fun (completion-ui-source-popup-frame-function nil overlay))
           (lines (funcall popup-fun prefix completions))
           (maxlen (if (null lines) 0 (apply 'max (mapcar 'length lines))))
           (pos (save-excursion
                  (goto-char (overlay-start overlay))
                  (completion-frame-posn-at-point)))
           ;; get or create pop-up frame
           (frame
            (or (overlay-get overlay 'popup-frame)
                (make-frame
                 `((name . "*completion-ui*")
		   (user-size . t)
                   (user-position . t)
                   (minibuffer . nil)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (menu-bar-lines . nil)
                   (tool-bar-lines . nil)
                   (unsplittable . t)
                   (cursor-type . nil)
                   (border-width . 0))))))

      ;; initialise pop-up frame
      (overlay-put overlay 'popup-frame frame)
      (set-frame-size
       frame
       (1+ maxlen)
       (1+ (min (length completions)
                completion-popup-frame-max-height)))
      (set-frame-position
       frame
       (+ (car pos) (car completion-popup-frame-offset))
       (+ (cdr pos) (cdr completion-popup-frame-offset)))
      (select-frame-set-input-focus frame)
      (switch-to-buffer " *completion-ui*")
      (completion-popup-frame-mode)
      (setq completion-popup-frame-parent-frame parent-frame)
      (setq completion-popup-frame-parent-overlay overlay)

      ;; insert completions
      (erase-buffer)
      (mapc (lambda (str) (insert str "\n")) lines)
      (backward-delete-char 1)

      ;; highlight current completion
      (goto-char (point-min))
      (when num (forward-line num))
      (let ((pos (point)))
        (end-of-line)
        (unless (overlayp completion-popup-frame-overlay)
          (setq completion-popup-frame-overlay
                (make-overlay pos (point)))
          (overlay-put completion-popup-frame-overlay
                       'face 'completion-highlight-face))
        (move-overlay completion-popup-frame-overlay pos (point)))
      )))



(defun completion-popup-frame-dismiss (&optional overlay)
  "Delete current pop-up frame."
  (interactive)

  ;; if overlay isn't supplied, pop-up frame must be selected, so get parent
  ;; overlay
  (unless overlay (setq overlay completion-popup-frame-parent-overlay))

  ;; if showing all completions, revert to storing just the first maxnum
  (when completion-popup-frame-show-all
    (let* ((prefix (overlay-get overlay 'prefix))
	   (cmpl-fun (or (completion-ui-source-completion-function
			  (overlay-get overlay 'completion-source))
			 (overlay-get overlay 'completion-source)))
	   (completions (funcall cmpl-fun prefix completion-max-candidates)))
      (overlay-put overlay 'completions completions)))
  ;; select main Emacs frame if we're in a pop-up frame
  (when completion-popup-frame-parent-frame
    (select-frame completion-popup-frame-parent-frame))
  ;; delete pop-up frame, if any
  (when (overlay-get overlay 'popup-frame)
    (delete-frame (overlay-get overlay 'popup-frame))
    (overlay-put overlay 'popup-frame nil))
  ;; reset global pop-up frame variables
  (setq completion-popup-frame-parent-frame nil)
  (setq completion-popup-frame-parent-overlay nil))




;; The major mode function
(defun completion-popup-frame-mode ()
  "Major mode used in completion-UI pop-up frames."
  (kill-all-local-variables)
  (setq major-mode 'completion-popup-frame-mode)
  (use-local-map completion-popup-frame-mode-map)
  (setq mode-line-format nil))

;; indicate mode is only appropriate in special circumstances
(put 'completion-popup-frame-mode 'mode-class 'special)



(defun completion-popup-frame-motion (command &optional arg)
  "Call COMMAND to move point, then select completion at point,
Selecting the completion inserts it in the pop-up frame's parent
buffer and highlights it in the pop-up frame.

If ARG is supplied, it is passed through to COMMAND."

  ;; call COMMAND with ARG
  (funcall command arg)

  ;; highlight selected completion
  (forward-line 0)
  (let ((pos (point)))
    (end-of-line)
    (move-overlay completion-popup-frame-overlay pos (point)))

  ;; insert selected completion in parent buffer
  (let ((num (1-(line-number-at-pos)))
	(frame (selected-frame))
	(overlay completion-popup-frame-parent-overlay))
    ;; update completion overlay properties and user-interfaces
    (select-frame completion-popup-frame-parent-frame)
    (set-buffer (overlay-buffer overlay))
    (completion-ui-deactivate-interfaces-pre-update overlay)
    (overlay-put overlay 'completion-num num)
    (completion-ui-activate-interfaces overlay)
    (select-frame-set-input-focus frame)))



(defun completion-popup-frame-unread-key ()
  "Unread last key sequence, then kill popup frame.
The focus is returned to the parent buffer, which will then
receive the unread key sequence."
  (interactive)
  (setq unread-command-events (listify-key-sequence (this-command-keys)))
  (select-frame completion-popup-frame-parent-frame))



(defun completion-popup-frame-toggle-show-all ()
  "Toggle between showing some completions and all completions.
Initially, only the first `completion-max-candidates' completions
are shown in a pop-up frame, as with all the other completion
methods. Toggling will show all possible completions."
  (interactive)

  (let ((prefix (overlay-get completion-popup-frame-parent-overlay
                             'prefix))
	(cmpl-fun (or (completion-ui-source-completion-function
		       (overlay-get completion-popup-frame-parent-overlay
			       'completion-source))
		      (overlay-get completion-popup-frame-parent-overlay
			       'completion-source)))
	completions lines maxlen)

    (cond
     ;; if we weren't showing all completions, get all completions and
     ;; update completion overlay properties
     ((null completion-popup-frame-show-all)
      (message
       "Finding all completions (C-g to cancel if taking too long)...")
      (save-excursion
        (set-buffer (overlay-buffer completion-popup-frame-parent-overlay))
        (setq completions (funcall cmpl-fun prefix)))
      (overlay-put completion-popup-frame-parent-overlay
                   'completions completions))

     ;; if we were showing all completions, get list of best completions and
     ;; update completion overlay properties
     (completion-popup-frame-show-all
      (save-excursion
        (set-buffer (overlay-buffer completion-popup-frame-parent-overlay))
        (setq completions
	      (funcall cmpl-fun prefix completion-max-candidates)))
      (overlay-put completion-popup-frame-parent-overlay
                   'completions completions)))

    ;; reset pop-up frame properties
    (erase-buffer)
    (setq lines
          (completion-construct-popup-frame-text prefix completions))
    (setq maxlen (if (null lines) 0 (apply 'max (mapcar 'length lines))))
    (set-frame-size (selected-frame) (1+ maxlen) (frame-height))
    ;; insert completions in pop-up frame
    (mapc (lambda (str) (insert str "\n")) lines)
    (delete-backward-char 1)
    ;; highlight first completion
    (goto-char (point-min))
    (let ((pos (point)))
      (end-of-line)
      (move-overlay completion-popup-frame-overlay pos (point)))
    ;; toggle flag
    (setq completion-popup-frame-show-all
          (not completion-popup-frame-show-all))))



(defun completion-construct-popup-frame-text (prefix completions)
  "Construct the list of lines for a pop-up frame."
  (let ((maxlen
	 (if (null completions) 0
	   (apply 'max
		  (mapcar
		   (lambda (cmpl)
		     (if (stringp cmpl) (length cmpl) (length (car cmpl))))
		   completions))))
        (lines nil)
	str)
    (dotimes (i (length completions))
      (setq str (nth i completions))
      (unless (stringp str) (setq str (car str)))
      (setq lines
            (append lines
                    (list
                     (concat
		      str
                      ;; pad to same length
                      (make-string (- maxlen (length str)) ? )
                      ;; add hotkey for current completion, if any
                      (if (and completion-use-hotkeys
                               (< i (length completion-hotkey-list)))
                          (format " (%s)"
                                  (key-description
                                   (vector (nth i completion-hotkey-list))))
                        ""))))))
    lines))  ; return pop-up frame lines



;;; =================================================================
;;;                    Register user-interface

(completion-ui-register-interface
 'completion-use-popup-frame
 :activate 'completion-activate-popup-frame-keys
 :deactivate 'completion-popup-frame-dismiss
 :update 'completion-update-popup-frame
 :auto-show 'completion-popup-frame
 :after t)



;;; completion-ui-popup-frame.el ends here
