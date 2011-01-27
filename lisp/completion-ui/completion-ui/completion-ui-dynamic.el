
;;; completion-ui-tooltip.el --- dynamic user-interface for Completion-UI


;; Copyright (C) 2009-2010 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.3
;; Keywords: completion, user interface, dynamic, hippie
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
;; Version 0.1.3
;; * Minor change to `completion-deactivate-dynamic' to make it more robust
;;   against the overlay being in an inconsistent state
;;
;; Version 0.1.2
;; * changed behaviour when `completion-accept-or-reject-by-default' is set to
;;   'accept-common
;;
;; Version 0.1.1
;; * added missing code to activate and deactivate key bindings from
;;   `completion-dynamic-map'
;; * removed obsolete `auto-completion-dynamic-map'
;; * support new `completion-auto-update' customization option
;;
;; Version 0.1
;; * initial version (split off from completion-ui.el)


;;; Code:

(eval-when-compile (require 'cl))

(provide 'completion-ui-dynamic)
(require 'completion-ui)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-dynamic nil
  "Completion-UI dynamic completion user interface."
  :group 'completion-ui)


(defcustom completion-use-dynamic t
  "*Enable dynamic completion.
Dynamic completion directly inserts the first completion into the
buffer without further action required by the user. It is still a
provisional completion, so until it is accepted all the usual
mechanisms for selecting completions are still available."
  :group 'completion-ui-dynamic
  :type 'boolean)


(defcustom completion-dynamic-highlight-common-substring t
  "*Highlight the longest common prefix in dynamic completions."
  :group 'completion-ui-dynamic
  :type 'boolean)


(defcustom completion-dynamic-highlight-prefix-alterations t
  "*Highlight alterations to the prefix in dynamic completions."
  :group 'completion-ui-dynamic
  :type 'boolean)


(defface completion-dynamic-common-substring-face
  '((((class color) (background dark))
     (:background "dodger blue" :foreground "white"))
    (((class color) (background light))
     (:background "gold" :foreground "black")))
  "*Face used to highlight the common prefix in dynamic completions."
  :group 'completion-ui-dynamic)


(defface completion-dynamic-prefix-alterations-face
  '((((class color) (background dark))
     (:background "slate blue" :foreground "white"))
    (((class color) (background light))
     (:background "yellow" :foreground "black")))
  "*Face used to highlight prefix alterations in dynamic completions."
  :group 'completion-ui-dynamic)



;;; ============================================================
;;;               Other configuration variables

(defvar completion-dynamic-map nil
  "Keymap used when dynamic completion is enabled.
These key bindings get added to the completion overlay keymap.")



;;; ============================================================
;;;                 Interface functions

(defun completion-activate-dynamic (overlay)
  "Insert dynamic completion and update completion OVERLAY
accordingly. The point had better be within OVERLAY or
cauliflower will start growing out of your ears."

  (let ((pos (make-marker)))
    (let ((prefix (overlay-get overlay 'prefix))
	  (completions (overlay-get overlay 'completions))
	  (prefix-len (overlay-get overlay 'prefix-length))
	  (non-prefix-cmpl
	   (overlay-get overlay 'non-prefix-completion))
	  cmpl len)
      (when completions
	;; For some unknown reason, the delete-region or insert (below) can
	;; sometimes delete or move the completion overlay, so we mark its
	;; start position with marker `pos' before doing anything else, in
	;; order to move the completion overlay into the correct new position
	;; later.
	(move-marker pos (overlay-start overlay) (overlay-buffer overlay))
	(set-marker-insertion-type pos nil)
        ;; delete prefix if `non-prefix-completion' is non-nil and
        ;; `auto-completion-mode' is disabled
        (delete-region
	 (- pos
	    (if (or (null non-prefix-cmpl)
		    (and (not (overlay-get overlay 'prefix-replaced))
			 (or (eq completion-accept-or-reject-by-default
				 'accept)
			     (eq completion-accept-or-reject-by-default
				 'accept-common))
			 (overlay-put overlay 'prefix-replaced t)))
		(overlay-get overlay 'prefix-length) 0))
	 (overlay-end overlay))
        ;; get length of prefix part of new completion
	(setq cmpl (nth (overlay-get overlay 'completion-num) completions)
	      len  (length prefix))
	(unless (stringp cmpl)
	  (setq len  (cdr cmpl)
		cmpl (car cmpl)))
	;; insert new completion
        (let ((overwrite-mode nil)) (insert cmpl))
        (move-overlay overlay
		      (+ pos (if non-prefix-cmpl 0 len))
		      (+ pos (length cmpl)))
        (overlay-put overlay 'prefix-length len)
	;; highlight alterations to prefix, if enabled
	(when (and completion-dynamic-highlight-prefix-alterations
		   (not non-prefix-cmpl))
	  (completion--highlight-prefix-alterations prefix cmpl pos len))
        ;; highlight common substring, if enabled
        (when completion-dynamic-highlight-common-substring
	  (completion--highlight-common-substring
	   prefix cmpl pos len overlay)))

      ;; move point to appropriate position in the overlay
      (completion--position-point-in-overlay overlay))

    ;; enable key-bindings
    (when completion-dynamic-map
      (map-keymap
       (lambda (key binding)
	 (define-key (overlay-get overlay 'keymap) (vector key) binding))
       completion-dynamic-map))

    ;; delete temporary marker
    (set-marker pos nil)))



(defun completion-deactivate-dynamic (overlay)
  "Deactivate dynamic completions for comletion OVERLAY."
  ;; delete dynamic completion and prefix, unless prefix has been replaced
  (when (overlay-buffer overlay)
    (goto-char (overlay-start overlay))
    (let (pos)
      (when (> (setq pos (- (overlay-start overlay)
			    (if (and (overlay-get
				      overlay 'non-prefix-completion)
				     (overlay-get overlay 'prefix-replaced)
				     (not (overlay-put
					   overlay 'prefix-replaced nil)))
				0 (overlay-get overlay 'prefix-length))))
	       0)
      (delete-region pos (overlay-end overlay))))
    ;; restore original prefix
    (let ((overwrite-mode nil)) (insert (overlay-get overlay 'prefix)))
    ;; reset overlay
    (move-overlay overlay (point) (point)))

  ;; disable key-bindings
  (when completion-dynamic-map
    (map-keymap
     (lambda (key binding)
       (define-key (overlay-get overlay 'keymap) (vector key) nil))
     completion-dynamic-map)))



(defun completion--highlight-prefix-alterations (prefix cmpl pos len)
  "Highlight altered characters in PREFIX.
CMPL is the current dynamic completion, and POS is the position
of the end of PREFIX in the buffer."
  (let ((p-len (length prefix)))
    ;; compare characters of prefix and highlight differences
    (dotimes (i (min len p-len))
      (unless (eq (aref cmpl i) (aref prefix i))
	(put-text-property
	 (+ pos i) (+ pos i 1)
	 (if font-lock-mode 'font-lock-face 'face)
	 'completion-dynamic-prefix-alterations-face)))
    ;; if prefix length has been altered, highlight all the remaining altered
    ;; prefix
    (when (< len p-len)
      (put-text-property
       (+ pos len) (+ pos p-len)
       (if font-lock-mode 'font-lock-face 'face)
       'completion-dynamic-prefix-alterations-face))))



(defun completion--highlight-common-substring (prefix cmpl pos len overlay)
  "Highlight longest common substring of all completions of PREFIX.
CMPL is the current dynamic completion, POS is the position of
the end of PREFIX in the buffer, LEN is the length of the prefix
inserted in the buffer, and OVERLAY is the dynamic completion
overlay."
  (let* ((non-prefix-cmpl
	  (overlay-get overlay 'non-prefix-completion))
	 (substr (try-completion
		  "" (mapcar
		      (lambda (cmpl)
			(if (stringp cmpl)
			    (if non-prefix-cmpl
				cmpl
			      ;; avoid throwing error for nonsense completions
			      (when (<= (length prefix) (length cmpl))
				(substring cmpl (length prefix))))
			  (if non-prefix-cmpl
			      (car cmpl)
			    ;; avoid throwing error for nonsense completions
			    (when (<= (cdr cmpl) (length (car cmpl)))
			      (substring (car cmpl) (cdr cmpl))))))
		      (overlay-get overlay 'completions)))))

    ;; create common substring overlay if doesn't already exist
    (unless (overlay-get overlay 'common-substring)
      (let ((o (make-overlay (point) (point))))
        (overlay-put overlay 'common-substring o)
        (overlay-put o 'face 'completion-dynamic-common-substring-face)
        (overlay-put o 'priority 101)))

    ;; note: try-completion returns t if there's only one completion
    (move-overlay (overlay-get overlay 'common-substring)
		  (+ pos (if non-prefix-cmpl 0 len))
		  (if (eq substr t)
		      (+ pos (if non-prefix-cmpl 0 len))
		    (+ pos (if non-prefix-cmpl 0 len)
		       (length substr))))))



(defun completion--position-point-in-overlay (overlay)
  "Move point to appropriate position in OVERLAY,
the start for `auto-completion-mode' or if it is to be rejected,
the end of the common prefix for `accept-common' or
the end if it is to be accepted."
  (cond
   ;; reject, auto-completion-mode, or completion-auto-update
   ((or (eq completion-accept-or-reject-by-default 'reject)
	(and auto-completion-mode
	     (eq (overlay-get overlay 'completion-source)
		 auto-completion-source)))
    (goto-char (overlay-start overlay)))

   ;; accept-common
   ((eq completion-accept-or-reject-by-default 'accept-common)
    (if (overlayp (overlay-get overlay 'common-substring))
        ;; if the common-substring overlay already exists, goto the end of it
        (goto-char (overlay-end (overlay-get overlay 'common-substring)))
      ;; if the common-substring hasn't already been found, find it
      (let* ((prefix (overlay-get overlay 'prefix))
	     (completions
	      (mapcar
	       (lambda (c)
		 (if (stringp c)
		     (substring c (length prefix))
		   (substring (car c) (cdr c))))
	       (overlay-get overlay 'completions)))
	     (str (try-completion "" completions)))
	;; (try-completion returns t if there's only one completion)
	(when (eq str t) (setq str (car completions)))
	(goto-char (+ (overlay-start overlay) (length str))))))

   ;; accept
   (t (goto-char (overlay-end overlay)))))



;;; =================================================================
;;;                    Register user-interface

(completion-ui-register-interface
 'completion-use-dynamic
 :activate 'completion-activate-dynamic
 :deactivate 'completion-deactivate-dynamic)



;;; completion-ui-dynamic.el ends here
