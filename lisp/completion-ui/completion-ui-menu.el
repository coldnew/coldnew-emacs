
;;; completion-ui-menu.el --- menu user-interface for Completion-UI


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: completion, user interface, menu
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

(provide 'completion-ui-menu)
(require 'completion-ui)



;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui-menu nil
  "Completion-UI menu user interface."
  :group 'completion-ui)


(defcustom completion-use-menu t
  "*Enable the completion menu and browser."
  :group 'completion-ui-menu
  :type 'boolean)


(defcustom completion-menu-offset '(0 . 0)
  "*Pixel offset for completion menus.
This sometimes needs to be tweaked manually to get completion
menus in the correct position under different window systems."
  :group 'completion-ui-menu
  :type '(cons (integer :tag "x") (integer :tag "y")))


(defcustom completion-browser-max-items 25
  "*Maximum number of completions to display
in any one completion browser submenu."
  :group 'completion-ui-menu
  :type 'integer)


(defcustom completion-browser-recurse-on-completions t
  "If non-nil, the completion browser will recursively list
completions of completions (of completions of completions...).
If nil, it will only display the original list of completions,
organised hierarchically.

Note that setting `non-prefix-completion' makes the browser
act as though this variable is set to nil, regardless of its
actual value, since recursing only makes sense for prefix
completion."
  :group 'completion-ui-menu
  :type 'boolean)


(defcustom completion-browser-buckets 'balance
  "*Policy for choosing number of \"buckets\" in completion browser
when there are more than `completion-browser-max-items' to
display:

balance:  balance number of buckets and size of content
maximize: maximize number of buckets, minimize size of contents
mininize: minimize number of buckets, maximize size of contents"
  :group 'completion-ui-menu
  :type '(choice (const :tag "balance" balance)
                 (const :tag "maximize" max)
                 (const :tag "minimize" min)))




;;; ============================================================
;;;                 Other configuration variables

(defvar completion-menu-map nil
  "Keymap used when the completion menu is enabled.
These key bindings get added to the completion overlay keymap.")

(unless completion-menu-map
  (setq completion-menu-map (make-sparse-keymap))
  ;; M-<down> displayes the completion menu
  (define-key completion-menu-map [M-down] 'completion-activate-menu)
  ;; clicking on a completion displays the completion menu
  (define-key completion-menu-map [mouse-2] 'completion-activate-menu))




;;; ============================================================
;;;                 Interface functions

(defun completion-activate-menu (&optional overlay browser)
  "Show the completion menu.
With a prefix argument, show the completion browser."
  (interactive (list nil current-prefix-arg))
  ;; look for completion overlay at point, unless one was supplied
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))
  ;; deactivate other auto-show interfaces
  (completion-ui-deactivate-auto-show-interface overlay)
  ;; show the completion menu
  (if browser
      (completion-show-browser-menu overlay)
    (completion-show-menu overlay)))


(defun completion-activate-menu-keys (overlay)
  "Enable menu key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) binding))
   completion-menu-map))


(defun completion-deactivate-menu-keys (overlay)
  "Disable menu key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) nil))
   completion-menu-map))



(defun completion-show-menu (&optional overlay menu)
  "Show completion menu for completion OVERLAY.
The point had better be within OVERLAY or you'll have a sneezing
fit.

If no OVERLAY is supplied, one is found at point (this only
happens when this function is called interactively).

If MENU is supplied, use that to construct the menu, unless an
overlay overrides it. It is called with one argument, the
completion OVERLAY. MENU defaults to the \"overlay local\"
binding of 'completion-menu, or `completion-menu' if there is
none."
  (interactive)
  (unless overlay (setq overlay (completion-ui-overlay-at-point)))

  (when overlay
    (unless menu
      (setq menu (completion-ui-source-menu-function nil overlay)))

    (let ((prefix (overlay-get overlay 'prefix))
	  keymap result)
      (cond
       ;; if `menu' is a function, evaluate it to get menu
       ((functionp menu)
        (setq keymap (funcall menu overlay))
        ;; throw error if return value has wrong type
        (unless (or (null keymap) (keymapp keymap))
          (error "`completion-menu' returned wrong type:null or keymapp, %s"
                 (prin1-to-string keymap))))

       ;; if `menu' is a keymap, use that
       ((keymapp menu) (setq keymap menu))

       ;; otherwise, throw an error
       (t (error "Wrong type in `completion-menu': functionp or keymapp, %s"
		 (prin1-to-string menu))))


      ;; if we've constructed a menu, display it
      (when keymap
        (setq result
              (x-popup-menu
	       (save-excursion
		 (goto-char (overlay-start overlay))
		 (list
		  (let ((pos (completion-window-posn-at-point
			      nil nil
			      (car completion-menu-offset)
			      (+ (frame-char-height) 3
				 (cdr completion-menu-offset)))))
		    (list (car pos) (cdr pos)))
		  (selected-window))
		 ;; (completion-posn-at-point-as-event
		 ;;  nil nil
		 ;;  (car completion-menu-offset)
		 ;;  (+ (frame-char-height) 3
		 ;;     (cdr completion-menu-offset)))
		 )
	       keymap))

        ;; if they ain't selected nuffin', don't do nuffin'!
        (when result
          ;; convert result to a vector for key lookup
          (setq result (apply 'vector result))

          (cond
           ;; if they selected a completion from the menu...
           ((string-match "^completion-insert"
                          (symbol-name (aref result (1- (length result)))))
            ;; insert selected completion
	    (destructuring-bind (cmpl len)
		(funcall (lookup-key keymap result))
	      ;; run accept hooks
	      (run-hook-with-args 'completion-accept-functions prefix cmpl)
	      ;; deactivate interfaces, delete original prefix, and insert
	      ;; selected completion
	      (completion-ui-deactivate-interfaces overlay)
	      (delete-region (- (point) (length prefix)) (point))
	      (let ((overwrite-mode nil)) (insert cmpl)))
            (completion-ui-delete-overlay overlay))

           ;; otherwise, run whatever they did select
           (t (funcall (lookup-key keymap result))))
          )))))



(defun completion-show-browser-menu (&optional overlay menu)
  "Show completion browser menu for completion OVERLAY.
The point had better be within OVERLAY or you'll get hives.

If no OVERLAY is supplied, one is found at point.

If MENU is supplied, use that to construct the menu, unless an
overlay overrides it. Defaults to the appropriate completion
source setting, or `completion-construct-browser-menu' if there
is none.

Note: can be overridden by \"overlay local\" binding of
'completion-browser-menu-function."
  (interactive)
  ;; this function is really just a call to `completion-show-menu' but passing
  ;; the browser menu function as the menu argument
  (completion-show-menu
   overlay (or menu (completion-ui-source-browser-function nil overlay))))



(defun completion-construct-menu (overlay)
  "Construct and return menu keymap defining the completion menu
for a completion OVERLAY."

  (let* ((menu (make-sparse-keymap))
	 (prefix (overlay-get overlay 'prefix))
	 (completions (overlay-get overlay 'completions))
	 (num (length completions))
	 n)

    ;; construct menu keymap from available completions
    (dotimes (i num)
      (setq n (- num i 1))
      (define-key menu
        (vector (intern (concat "completion-insert-" (number-to-string n))))
        (list 'menu-item
	      (if (stringp (nth n completions))
		  (nth n completions)
		(car (nth n completions)))
	      `(lambda ()
		 (list ,(if (stringp (nth n completions))
			    (nth n completions) (car (nth n completions)))
		       ,(if (stringp (nth n completions))
			    (length prefix) (cdr (nth n completions)))))
	      ;; if a hotkey is associated with completion, show it in menu
	      :keys (when (and completion-use-hotkeys
			       (< n (length completion-hotkey-list)))
		      (key-description
		       (vector (nth n completion-hotkey-list)))))))

    ;; add entry to switch to completion browser
    (define-key-after menu [separator-browser] '(menu-item "--"))
    (define-key-after menu [completion-browser-menu-function]
      (list 'menu-item "Browser..." 'completion-show-browser-menu))

    ;; return the menu keymap
    menu))



(defun completion-construct-browser-menu
  (overlay &optional menu-item-func sub-menu-func)
  "Construct the completion browser menu keymap
for a completion OVERLAY.

MENU-ITEM-FUNC and SUB-MENU-FUNC override the default functions
for creating the sub-menus and menu items. Both functions are
passed 4 arguments: a list of completions, or a single completion
in the case of MENU-ITEM-FUNC, MENU-ITEM-FUNC, SUB-MENU-FUNCT,
and OVERLAY. They should return menu keymaps."

  ;; FIXME: could we speed this up by using :filter menu entry functions to
  ;;        construct do  just-in-time construction of submenus? This didn't
  ;;        use to work, but maybe in new Emacs versions it does...

  ;; inform user it's in progress, as it can take a while
  (message "Creating predictive completion browser\
 (C-g to cancel if taking too long)...")

  ;; default menu creation functions
  (unless menu-item-func
    (setq menu-item-func 'completion-browser-menu-item))
  (unless sub-menu-func
    (setq sub-menu-func 'completion-browser-sub-menu))

  ;; main browser menu is just a browser submenu...
  (let* ((completions
	  (funcall (completion-ui-source-completion-function nil overlay)
		   (overlay-get overlay 'prefix)))
	 (menu (funcall sub-menu-func completions
			menu-item-func sub-menu-func overlay)))
    ;; ... with an item added for switching to the basic completion
    ;; menu
    (define-key-after menu [separator-basic] '(menu-item "--"))
    (define-key-after menu [completion-menu]
      (list 'menu-item "Basic..." 'completion-show-menu))

    ;; return keymap
    menu))



;; Note:
;;
;; I should probably use some `imenu' function to create the menu,
;; since `imenu' already deals with "bucketising" menus (an ugly
;; necessity which should anyway be replaced with menu scrollbars,
;; preferably with just-in-time calculation of menu entries --
;; heads-up Emacs devs!).
;;
;; My excuses are that `imenu--mouse-menu' etc. are undocumented,
;; rolling my own was easier, and anyway I think my buckets are better
;; (they're optimal in the information-theoretic sense that you need
;; to make the least number of choices to get to the entry you want).
;;
;; One day I might patch the `imenu' "bucketising" code, and use
;; `imenu' here instead. Don't hold your breath.

(defun completion-browser-sub-menu
  (completions menu-item-func sub-menu-func overlay)
  "Construct a predictive completion browser sub-menu keymap."

  (let ((prefix (overlay-get overlay 'prefix))
	(menu (make-sparse-keymap))
	(num-completions (length completions)))
    (cond

     ;; if there's only 1 entry, don't bother with sub-menu, just set keymap
     ;; to be the item itself
     ((= num-completions 1)
      (let* ((cmpl (car completions))
	     (entry (funcall menu-item-func
			     cmpl menu-item-func sub-menu-func overlay)))
	(cond
	 ;; if entry is a menu keymap, use it as the menu, adding completion
	 ;; itself to the top
	 ((keymapp entry)
	  (define-key entry [separator-item-sub-menu] '(menu-item "--"))
	  (define-key entry [completion-insert-root]
	    (list
	     'menu-item cmpl
	     `(lambda ()
		(list ,(if (stringp cmpl) cmpl (car cmpl))
		      ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))
	  (setq menu entry))

	 (t  ;; if entry is a single item, add it to the menu
	  (define-key menu [completion-insert-0]
	    (list
	     'menu-item cmpl
	     `(lambda ()
		(list ,(if (stringp cmpl) cmpl (car cmpl))
		      ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))))
	))


     ;; if menu does not need to be divided into buckets, just add the
     ;; completions themselves to the keymap
     ((<= num-completions completion-browser-max-items)
      (dotimes (i num-completions)
	(define-key-after menu
	  (vector (intern (concat "completion-insert-"
				  (number-to-string i))))
	  (list 'menu-item
		(if (stringp (nth i completions))
		    (nth i completions)
		  (car (nth i completions)))
		(funcall menu-item-func
			 (nth i completions) menu-item-func sub-menu-func
			 overlay))
	  )))


     ;; if menu needs to be divided into buckets, construct a menu keymap
     ;; containing the bucket menus
     (t
      (let* ((num-buckets
              (cond
               ;; maximize number of buckets, minimize size of
               ;; contents
               ((eq completion-browser-buckets 'max)
                completion-browser-max-items)
               ;; minimize number of buckets, maximize size of
               ;; contents
               ((eq completion-browser-buckets 'min)
                (min completion-browser-max-items
		     (1+ (/ (1- num-completions)
			    completion-browser-max-items))))
               ;; balance number of buckets and size of contents
               (t
                (min completion-browser-max-items
                     (round (sqrt num-completions))))))
             (num-per-bucket (/ num-completions num-buckets))
             (num-large-buckets (% num-completions num-buckets))
             (num-small-buckets (- num-buckets num-large-buckets))
             i j)

        (dotimes (b num-buckets)
          ;; if bucket has only 1 entry, don't bother with bucket
          ;; menu, just add completion itself to keymap
          (if (and (= 1 num-per-bucket) (< b num-small-buckets))
              (define-key-after menu
		(vector (intern (concat "completion-insert-"
					(number-to-string i))))
		(list 'menu-item
		      (if (stringp (nth i completions))
			  (nth i completions)
			(car (nth i completions)))
		      (funcall menu-item-func
			       (nth i completions)
			       menu-item-func sub-menu-func overlay)))

            ;; if bucket has more than 1 entry...
            ;; get index of first completion in bucket
            (setq i (+ (* (min b num-small-buckets) num-per-bucket)
                       (* (max 0 (- b num-small-buckets))
                          (1+ num-per-bucket))))
            ;; get index of last completion in bucket
            (setq j (1- (+ i num-per-bucket
			   (if (< b num-small-buckets) 0 1))))
            ;; add bucket menu to keymap
            (define-key-after menu
              (vector (intern (concat "bucket-" (number-to-string b))))
              (list 'menu-item
                    (concat "From \""
                            (nth i completions)
                            "\" to \""
                            (nth j completions) "\"")
                    ;; call function to generate sub-menu
                    (funcall sub-menu-func
			     (completion--sublist completions i (1+ j))
			     menu-item-func sub-menu-func overlay))))
          ))))

    ;; return constructed menu
    menu))



(defun completion-browser-menu-item
  (cmpl menu-item-func sub-menu-func overlay)
  "Construct predictive completion browser menu item."

  (let* ((prefix (overlay-get overlay 'prefix))
	 (cmpl-function (or (completion-ui-source-completion-function
			     (overlay-get overlay 'completion-source))
			    (overlay-get overlay 'completion-source)))
	 (cmpl-prefix-function
	  (overlay-get overlay 'completion-prefix-function))
	 (non-prefix-completion (overlay-get overlay 'non-prefix-completion))

	 ;; If `non-prefix-completion' is null, get completions for entry,
	 ;; dropping the empty string which corresponds to the same entry
	 ;; again (which would lead to infinite recursion). It makes no sense
	 ;; to get completions of completions (of completions of
	 ;; completions...) when doing something other than prefix-completion,
	 ;; so the entry is just the original completion itself if
	 ;; `non-prefix-completion' is non-nil.
	 (completions
	  (and completion-browser-recurse-on-completions
	       (not non-prefix-completion)
	       (not (string= (if (stringp cmpl) cmpl (car cmpl)) ""))
	       ;; note :have to replace any prefix length data in completions
	       ;; list with prefix length data from original prefix
	       (mapcar
		(if (stringp cmpl)
		    (lambda (c) (if (stringp c) c (car c)))
		  (lambda (c) (cons (if (stringp c) c (car c)) (cdr cmpl))))
		(cdr (funcall cmpl-function cmpl))))))
    ;; if there are no completions (other than the entry itself), create a
    ;; selectable completion item
    (if (null completions)
        `(lambda ()
	   (list ,(if (stringp cmpl) cmpl (car cmpl))
		 ,(if (stringp cmpl) (length prefix) (cdr cmpl))))
      ;; otherwise, create a sub-menu containing them
      (let ((menu (funcall sub-menu-func
			   completions menu-item-func sub-menu-func overlay)))
	;; add completion itself to the menu
        (define-key menu [separator-item-sub-menu] '(menu-item "--"))
        (define-key menu [completion-insert-root]
          (list 'menu-item
		cmpl
		`(lambda ()
		   (list ,(if (stringp cmpl) cmpl (car cmpl))
			 ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))
        ;; return the menu keymap
        menu))))



;;; =================================================================
;;;                    Register user-interface


(completion-ui-register-interface
 'completion-use-menu
 :activate 'completion-activate-menu-keys
 :deactivate 'completion-deactivate-menu-keys
 :auto-show 'completion-show-menu)



;;; completion-ui-menu.el ends here
