
;;; completion-ui-tooltip.el --- tooltip user-interface for Completion-UI


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: completion, user interface, tooltip
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


;;; Commentary:
;;


;;; Change Log:
;;
;; Version 0.2
;; * use pos-tip.el library to display tooltips instead of `x-show-tip'
;;   (thanks to Seweryn Kokot)
;;
;; Version 0.1.1
;; * bug-fix to `completion-cancel-tooltip' to prevent
;;   `completion-tooltip-map' keys from being incorrectly disabled
;;
;; Version 0.1
;; * initial version (split off from completion-ui.el)


;;; Code:

(eval-when-compile (require 'cl))

(provide 'completion-ui-tooltip)
(require 'completion-ui)
(require 'pos-tip)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-tooltip nil
  "Completion-UI tooltip user interface."
  :group 'completion-ui)


(defcustom completion-use-tooltip t
  "*When non-nil, enable the tooltip Completion-UI interface."
  :group 'completion-ui-tooltip
  :type 'boolean)


(defcustom completion-tooltip-timeout -1
  "*Number of seconds for wihch to display completion tooltip.
A negative value means don't hide the tooltip automatically."
  :group 'completion-ui-tooltip
  :type 'integer)


;; (defcustom completion-tooltip-offset '(0 . 0)
;;   "Pixel offset for tooltip.
;; This sometimes needs to be tweaked manually to get the tooltip in
;; the correct position under different window systems."
;;   :group 'completion-ui-tooltip
;;   :type '(cons (integer :tag "x") (integer :tag "y")))


(defface completion-tooltip-face
  `((t . ,(or (and (stringp (face-attribute 'menu :background))
		   (stringp (face-attribute 'menu :foreground))
		   (list :background (face-attribute 'menu :background)
			 :foreground (face-attribute 'menu :foreground)))
	      '(:background "light yellow" :foreground "black"))))
  "*Face used in tooltip. Only :foreground, :background and :family
attributes are used."
  :group 'completion-ui-tooltip)



;;; ============================================================
;;;                 Other configuration variables

(defvar completion-tooltip-function nil
  "Function to call to construct the tooltip text.

The function is called with three arguments, the prefix,
completions, and index of the currently active completion. It
should return a string containing the text to be displayed in the
tooltip.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-tooltip-map nil
  "Keymap used when a tooltip is displayed.
These key bindings also get added to the completion overlay keymap.")


(defvar completion-tooltip-active-map nil
  "Keymap used when a tooltip is being displayed.")



;;; ============================================================
;;;                   Internal variables

(defvar completion-tooltip-active nil
  "Used to enable `completion-tooltip-active-map'
when a tooltip is displayed.")



;;; ============================================================
;;;                   Setup default keymaps

;; Note on key bindings:
;;
;; `completion-tooltip-active' is reset by `pre-command-hook' (see end of
;; file), so the keymap below is disabled before every command is
;; executed. However, the key bindings are looked up before `pre-command-hook'
;; runs, so the first key sequence after displaying a tooltip has a chance of
;; running something from here. This is exactly what we want, since Emacs
;; hides tooltips after every command and we only want this keymap to be
;; active if a tooltip is visible.
;;
;; The cycling commands bound below re-display the completion tooltip, which
;; causes `completion-tooltip-active' to be set to non-nil again. So after
;; they've run, the keymap is left active again for the next key sequence.
;;
;; Unfortunately, a bug in all known versions of Emacs prevents keymaps from
;; being activated by timers, so this doesn't work when the tooltip is
;; auto-displayed. To work around this, we also bind the cycling commands in
;; `completion-overlay-map'.


;; Set default key bindings for the keymap used when a completion tooltip
;; is displayed, unless it's already been set (most likely in an init
;; file). This keymap is active when `completion-tooltip-active' is
;; non-nil.
(unless completion-tooltip-active-map
  (setq completion-tooltip-active-map (make-sparse-keymap))
  ;; <up> and <down> cycle completions, which appears to move selection
  ;; up and down tooltip entries
  (define-key completion-tooltip-active-map
    [down] 'completion-tooltip-cycle)
  (define-key completion-tooltip-active-map
    [up] 'completion-tooltip-cycle-backwards))

;; make sure completion-tooltip-active-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-tooltip-active minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-tooltip-active-map)
    (push (cons 'completion-tooltip-active completion-tooltip-active-map)
          minor-mode-map-alist)))


;; Set default key bindings for the keymap whose key bindings are added to the
;; overlay keymap when the tooltip interface is enabled
(unless completion-tooltip-map
  (setq completion-tooltip-map (make-sparse-keymap))
  ;; S-<down> displays the completion tooltip
  (define-key completion-tooltip-map [S-down] 'completion-show-tooltip)
  ;; S-<up> cancels it
  (define-key completion-tooltip-map [S-up] 'completion-cancel-tooltip))


;; <up> and <down> cycle the tooltip
;; Note: it shouldn't be necessary to bind them here, but the bindings in
;;       completion-tooltip-active-map don't work when the tooltip is
;;       auto-displayed, for mysterious reasons (note that we can't use
;;       `completion-run-if-condition' in `completion-overlay-map', hence
;;       binding them here)
(define-key completion-map [down]
  (lambda () (interactive)
    (completion--run-if-condition
     'completion-tooltip-cycle
     'completion-ui--activated
     completion-tooltip-active)))
(define-key completion-map [down]
  (lambda () (interactive)
    (completion--run-if-condition
     'completion-tooltip-cycle-backwards
     'completion-ui--activated
     completion-tooltip-active)))



;;; ============================================================
;;;                 Interface functions

(defun completion-activate-tooltip (&optional overlay)
  "Show the completion tooltip.

If no overlay is supplied, tries to find one at point.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for thirteen years."
    (interactive)
    ;; if no overlay was supplied, try to find one at point
    (unless overlay (setq overlay (completion-ui-overlay-at-point)))
    ;; activate tooltip key bindings
    (completion-activate-tooltip-keys overlay)
    ;; if tooltip has been displayed manually, re-display it
    (when (overlay-get overlay 'completion-interactive-tooltip)
      (completion-show-tooltip overlay)))



(defun completion-show-tooltip (&optional overlay interactive)
  "Show completion tooltip for completion OVERLAY.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for fourteen years.

If OVERLAY is not supplied, try to find one at point. If
INTERACTIVE is supplied, pretend we were called interactively."
  (interactive)

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; deactivate other auto-show interfaces
  (completion-ui-deactivate-auto-show-interface overlay)
  ;; if we can display a tooltip and there are completions to display in it...
  (when (and overlay (overlay-get overlay 'completions)
             window-system (fboundp 'x-show-tip))
    ;; if called manually, flag this in overlay property and call
    ;; auto-show-helpers, since they won't have been called by
    ;; `completion-ui-auto-show'
    (when (or (interactive-p) interactive)
      (overlay-put overlay 'completion-interactive-tooltip t)
      (completion-ui-call-auto-show-interface-helpers overlay))

    ;; calculate tooltip parameters
    (let ((fg (face-attribute 'completion-tooltip-face :foreground))
          (bg (face-attribute 'completion-tooltip-face :background))
          text w-h)

      ;; construct the tooltip text
      (setq text (funcall (completion-ui-source-tooltip-function nil overlay)
                          (overlay-get overlay 'prefix)
                          (overlay-get overlay 'completions)
                          (overlay-get overlay 'completion-num)))
      (when (string= (substring text -1) "\n")
        (setq text (substring text 0 -1)))
      (setq w-h (pos-tip-string-width-height text))
      ;; work-around an apparent bug in tooltips under windows, which appears
      ;; to add an extra space to the end of each line
      (when (eq window-system 'w32) (incf (car w-h)))

      ;; show tooltip
      ;; (let ((pos-tip-border-width 0)
      ;; 	    (pos-tip-internal-border-width 0))
	(pos-tip-show-no-propertize
	 text 'completion-tooltip-face nil nil completion-tooltip-timeout
	 (pos-tip-tooltip-width (car w-h) (frame-char-width))
	 (pos-tip-tooltip-height (cdr w-h) (frame-char-height)))
	 ;; (car completion-tooltip-offset)
	 ;; (+ (frame-char-height) (cdr completion-tooltip-offset)))
      ;; )

      ;; set flag to indicate tooltip is active for this overlay (this should
      ;; enable tooltip-related key bindings, but doesn't when tooltip is
      ;; auto-displayed, so we also add them to overlay's keymap)
      (setq completion-tooltip-active overlay)
      )))


(defun completion-activate-tooltip-keys (overlay)
  "Enable tooltip key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) binding))
   completion-tooltip-map))


(defun completion-deactivate-tooltip-keys (overlay)
  "Disable tooltip key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) nil))
   completion-tooltip-map))


(defun completion-cancel-tooltip (&optional overlay)
  "Hide the completion tooltip and cancel timers."
  (interactive)
  ;; unset manually displayed tooltip flag
  (when (or overlay (setq overlay (completion-ui-overlay-at-point)))
    (overlay-put overlay 'completion-interactive-tooltip nil))
  ;; cancel timer
  (completion-ui-cancel-auto-show)
  ;; cancel tooltip
  (when (and window-system (fboundp 'x-show-tip))
    (tooltip-hide)
    ;; (when (overlayp completion-tooltip-active)
    ;;   (completion-deactivate-tooltip-keys completion-tooltip-active))
    (setq completion-tooltip-active nil)))


(defun completion-construct-tooltip-text
  (prefix completions &optional num)
  "Function to return completion text for a tooltip.
Optional argument NUM specifies the number of the currently
inserted dynamic completion."

  (let* ((text "") str
         (maxlen (if (null completions) 0
                   (apply 'max (mapcar (lambda (cmpl)
					 (if (stringp cmpl)
					     (length cmpl)
					   (length (car cmpl))))
				       completions)))))

    (dotimes (i (length completions))
      ;; pad all strings to same length
      (setq str (nth i completions))
      (unless (stringp str) (setq str (car str)))
      (setq str (concat str (make-string (- maxlen (length str)) ? )))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (when (and completion-use-hotkeys
                 (< i (length completion-hotkey-list)))
        (setq str
              (concat str " "
                      (format "(%s)"
                              (key-description
                               (vector (nth i completion-hotkey-list)))))))
      ;; if current completion is the inserted dynamic completion, use
      ;; `completion-highlight-face' to highlight it
      (when (and num (= i num))
        ;; setting 'face attribute to 'completion-highlight-face
        ;; doesn't seem to work with defface using display classes
        (put-text-property
         0 (length str) 'face
         `((foreground-color . ,(face-attribute 'completion-highlight-face
                                                :foreground))
           (background-color . ,(face-attribute 'completion-highlight-face
                                                :background)))
         str))
      (setq text (concat text str "\n")))

    ;; return constructed text
    text))


(defun completion-tooltip-cycle (&optional n overlay)
  "Cycle forwards through N completions and redisplay the tooltip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
cow."
  (interactive)
  (completion-cycle n overlay)
  (completion-show-tooltip nil t))


(defun completion-tooltip-cycle-backwards (&optional n overlay)
  "Cycle backwards through N completions and redisplay the tooltip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
sheep."
  (interactive)
  (completion-cycle (if n (- n) -1) overlay)
  (completion-show-tooltip nil t))



;;; =================================================================
;;;                      Set hook functions

;; (add-hook 'after-change-functions
;;           (lambda (&rest unused) (completion-cancel-tooltip)))

;; we reset tooltip flag after any command because Emacs hides tooltips
;; after any command
(add-hook 'pre-command-hook 'completion-cancel-tooltip)



;;; =================================================================
;;;                    Register user-interface

(completion-ui-register-interface
 'completion-use-tooltip
 :activate 'completion-activate-tooltip
 :deactivate 'completion-cancel-tooltip
 :auto-show 'completion-show-tooltip)


;; set default auto-show interface to tooltip
(setq completion-auto-show 'completion-show-tooltip)

;; (Note: this is kinda ugly, but setting the default in the defcustom seems
;; to conflict with refreshing the defcustom in
;; `compeltion-ui-register-interface'. Since Completion-UI is almost
;; invariably loaded before a user's customization settings, theirs will
;; still take precedence.)



;;; completion-ui-tooltip.el ends here
