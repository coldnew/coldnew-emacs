;;;; Redefinitions of some of Viper's functions

(require 'vimpulse-dependencies)
(eval-when-compile (require 'vimpulse-utils))   ; vimpulse-unquote

(defalias 'viper-digit-argument 'digit-argument)

;; Ensure that counts are always echoed immediately, since they might
;; alter the command's behavior profoundly (e.g., 5i repeats the
;; insertion four times).
(defadvice digit-argument (around echo-keystrokes activate)
  "Echo keystrokes immediately."
  (setq echo-keystrokes 0.01)
  ad-do-it)

(defadvice ensure-overriding-map-is-bound (after echo-keystrokes activate)
  "Echo keystrokes immediately."
  (setq echo-keystrokes 0.01))

(defvar vimpulse-saved-echo-keystrokes echo-keystrokes)

(defadvice restore-overriding-map (after echo-keystrokes activate)
  "Restore `echo-keystrokes'."
  (setq echo-keystrokes vimpulse-saved-echo-keystrokes))

(defadvice viper-change
  (around vimpulse-want-change-state activate)
  "Disable Replace state if `vimpulse-want-change-state' is nil."
  (cond
   (vimpulse-want-change-state
    ad-do-it)
   (t
    ;; we don't want Viper's Replace mode when changing text;
    ;; just delete and enter Insert state
    (setq viper-began-as-replace t)
    (kill-region beg end)
    (goto-char beg)
    (viper-change-state-to-insert))))

(defadvice viper-exit-insert-state (before vimpulse activate)
  "Refresh `vimpulse-exit-point'."
  (viper-move-marker-locally 'vimpulse-exit-point (point)))

(defun vimpulse-set-replace-cursor-type ()
  "Display a horizontal bar cursor."
  (unless (featurep 'xemacs)
    (setq cursor-type '(hbar . 4))))

(set-face-foreground viper-replace-overlay-face nil)
(set-face-background viper-replace-overlay-face nil)

(unless (featurep 'xemacs)
  (setq viper-replace-overlay-cursor-color
        viper-vi-state-cursor-color)
  (add-hook 'viper-replace-state-hook
            'vimpulse-set-replace-cursor-type)
  (remove-hook 'viper-replace-state-hook
               'viper-restore-cursor-type))

;;; Marks

;; the following makes lowercase marks buffer-local
(defun vimpulse-mark-point ()
  "Set Vimpulse mark at point."
  (interactive)
  (let ((char (read-char)))
    (cond
     ;; local marks
     ((and (<= ?a char) (<= char ?z))
      (vimpulse-mark char))
     ;; global marks
     ((and (<= ?A char) (<= char ?Z))
      (vimpulse-mark char t))
     ;; < > . , ^
     (t
      (add-to-list 'unread-command-events char)
      (viper-mark-point)))))

(defun vimpulse-mark (char &optional global)
  "Set mark CHAR at point.
Mark is buffer-local unless GLOBAL."
  (let* ((marks-alist (if global
                          'vimpulse-global-marks-alist
                        'vimpulse-local-marks-alist))
         (mark  (assq char (symbol-value marks-alist)))
         (value (cons buffer-file-name (point-marker))))
    (if mark
        (setcdr mark value)
      (set marks-alist (cons (cons char value)
                             (symbol-value marks-alist)))))
  (add-hook 'kill-buffer-hook 'vimpulse-mark-swap-out nil t))

(defun vimpulse-mark-swap-out ()
  "Cf. `register-swap-out'."
  (and buffer-file-name
       (dolist (marks-alist '(vimpulse-local-marks-alist
                              vimpulse-global-marks-alist))
         (dolist (elt (symbol-value marks-alist))
           (and (markerp (cddr elt))
                (eq (marker-buffer (cddr elt)) (current-buffer))
                (setcdr (cdr elt) (marker-position (cddr elt))))))))

(defun vimpulse-get-mark (char)
  (or (cdr (assq char (if (< char ?Z)
                          vimpulse-global-marks-alist
                        vimpulse-local-marks-alist)))
      (error "No such mark: %c" char)))

(defun vimpulse-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char))
        (com (viper-getcom arg)))
    (vimpulse-goto-mark-subr char com nil)))

(defun vimpulse-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white character on line."
  (interactive "P")
  (let ((char (read-char))
        (com (viper-getCom arg)))
    (vimpulse-goto-mark-subr char com t)))

(defun vimpulse-goto-mark-subr (char com skip-white)
  (cond
   ((viper-valid-register char '(letter Letter))
    (let* ((buff (current-buffer))
           (pos (vimpulse-get-mark char))
           (file (car pos))
           (marker (cdr pos)))
      (if (and file (equal buffer-file-name file))
          (goto-char marker)
        (if (null file)
            (if (marker-buffer marker)
                (progn (switch-to-buffer (marker-buffer marker))
                       (goto-char marker))
              (error "Cannot jump to non-existent buffer"))
          (and (or (find-buffer-visiting file)
                   (y-or-n-p (format "Visit file %s again? " file)))
               (find-file file)
               (goto-char marker))))
      (when com
        (viper-move-marker-locally 'viper-com-point (point)))
      (if (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
          (push-mark viper-last-jump t)
        (push-mark nil t))
      (setq viper-last-jump (point-marker))
      (when skip-white
        (back-to-indentation)
        (setq viper-last-jump-ignore (point)))
      (when com
        (if (equal buff (current-buffer))
            (viper-execute-com (if skip-white
                                   'viper-goto-mark-and-skip-white
                                 'viper-goto-mark)
                               nil com)
          (switch-to-buffer buff)
          (goto-char viper-com-point)
          (viper-change-state-to-vi)
          (error "Viper bell")))))
   ((and (not skip-white) (viper= char ?`))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (when (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
      (goto-char viper-last-jump))
    (when (null (mark t))
      (error "Mark is not set in this buffer"))
    (when (= (point) (mark t))
      (pop-mark))
    (push-mark (prog1 (point)
                 (goto-char (or (mark t) (point)))) t)
    (setq viper-last-jump (point-marker)
          viper-last-jump-ignore 0)
    (when com
      (viper-execute-com 'viper-goto-mark nil com)))
   ((and skip-white (viper= char ?'))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (when (and (viper-same-line (point) viper-last-jump)
               (= (point) viper-last-jump-ignore))
      (goto-char viper-last-jump))
    (when (= (point) (mark t))
      (pop-mark))
    (push-mark (prog1 (point)
                 (goto-char (or (mark t) (point)))) t)
    (setq viper-last-jump (point))
    (back-to-indentation)
    (setq viper-last-jump-ignore (point))
    (when com
      (viper-execute-com 'viper-goto-mark-and-skip-white nil com)))
   (t
    (error viper-InvalidTextmarker char))))

;;; Code for adding extra states

;; state index variables: for keeping track of which modes
;; belong to which states, et cetera
(defvar vimpulse-state-vars-alist
  '((vi-state
     (id . viper-vi-state-id)
     (auxiliary-modes . viper-vi-auxiliary-modes)
     (change-func . viper-change-state-to-vi)
     (basic-mode . viper-vi-basic-minor-mode)
     (basic-map . viper-vi-basic-map)
     (diehard-mode . viper-vi-diehard-minor-mode)
     (diehard-map . viper-vi-diehard-map)
     (modifier-mode . viper-vi-state-modifier-minor-mode)
     (modifier-alist . viper-vi-state-modifier-alist)
     (kbd-mode . viper-vi-kbd-minor-mode)
     (kbd-map . viper-vi-kbd-map)
     (global-user-mode . viper-vi-global-user-minor-mode)
     (global-user-map . viper-vi-global-user-map)
     (local-user-mode . viper-vi-local-user-minor-mode)
     (local-user-map . viper-vi-local-user-map)
     (need-local-map . viper-need-new-vi-local-map)
     (intercept-mode . viper-vi-intercept-minor-mode)
     (intercept-map . viper-vi-intercept-map))
    (insert-state
     (id . viper-insert-state-id)
     (auxiliary-modes . viper-insert-auxiliary-modes)
     (change-func . viper-change-state-to-insert)
     (basic-mode . viper-insert-basic-minor-mode)
     (basic-map . viper-insert-basic-map)
     (diehard-mode . viper-insert-diehard-minor-mode)
     (diehard-map . viper-insert-diehard-map)
     (modifier-mode . viper-insert-state-modifier-minor-mode)
     (modifier-alist . viper-insert-state-modifier-alist)
     (kbd-mode . viper-insert-kbd-minor-mode)
     (kbd-map . viper-insert-kbd-map)
     (global-user-mode . viper-insert-global-user-minor-mode)
     (global-user-map . viper-insert-global-user-map)
     (local-user-mode . viper-insert-local-user-minor-mode)
     (local-user-map . viper-insert-local-user-map)
     (need-local-map . viper-need-new-insert-local-map)
     (intercept-mode . viper-insert-intercept-minor-mode)
     (intercept-map . viper-insert-intercept-map))
    (replace-state
     (auxiliary-modes . viper-replace-auxiliary-modes)
     (id . viper-replace-state-id)
     (change-func . viper-change-state-to-replace)
     (basic-mode . viper-replace-minor-mode)
     (basic-map . viper-replace-map))
    (emacs-state
     (id . viper-emacs-state-id)
     (auxiliary-modes . viper-emacs-auxiliary-modes)
     (change-func . viper-change-state-to-emacs)
     (modifier-mode . viper-emacs-state-modifier-minor-mode)
     (modifier-alist . viper-emacs-state-modifier-alist)
     (kbd-mode . viper-emacs-kbd-minor-mode)
     (kbd-map . viper-emacs-kbd-map)
     (global-user-mode . viper-emacs-global-user-minor-mode)
     (global-user-map . viper-emacs-global-user-map)
     (local-user-mode . viper-emacs-local-user-minor-mode)
     (local-user-map . viper-emacs-local-user-map)
     (need-local-map . viper-need-new-emacs-local-map)
     (intercept-mode . viper-emacs-intercept-minor-mode)
     (intercept-map . viper-emacs-intercept-map)))
  "Alist of Vimpulse state variables.
Entries have the form (STATE . ((VAR-TYPE . VAR) ...)).
For example, the basic state keymap has the VAR-TYPE `basic-map'.")

(defvar vimpulse-state-modes-alist
  '((vi-state
     (viper-vi-intercept-minor-mode . t)
     (viper-vi-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-vi-local-user-minor-mode . t)
     (viper-vi-auxiliary-modes . t)
     (viper-vi-global-user-minor-mode . t)
     (viper-vi-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-vi-state-modifier-minor-mode . t)
     (viper-vi-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-vi
                 (viper-is-in-minibuffer))))
     (viper-vi-basic-minor-mode . t))
    (insert-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-auxiliary-modes . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (replace-state
     (viper-insert-intercept-minor-mode . t)
     (viper-replace-minor-mode . (eq state 'replace-state))
     (viper-replace-auxiliary-modes . t)
     (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
     (viper-insert-local-user-minor-mode . t)
     (viper-insert-auxiliary-modes . t)
     (viper-insert-global-user-minor-mode . t)
     (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-insert-state-modifier-minor-mode . t)
     (viper-insert-diehard-minor-mode
      . (not (or viper-want-emacs-keys-in-insert
                 (viper-is-in-minibuffer))))
     (viper-insert-basic-minor-mode . t))
    (emacs-state
     (viper-emacs-intercept-minor-mode . t)
     (viper-emacs-local-user-minor-mode . t)
     (viper-emacs-auxiliary-modes . t)
     (viper-emacs-global-user-minor-mode . t)
     (viper-emacs-kbd-minor-mode . (not (viper-is-in-minibuffer)))
     (viper-emacs-state-modifier-minor-mode . t)))
  "Alist of Vimpulse state mode toggling.
Entries have the form (STATE . ((MODE . EXPR) ...)), where STATE
is the name of a state, MODE is a mode associated with STATE and
EXPR is an expression with which to enable or disable MODE.
The first modes get the highest priority.")

(defvar vimpulse-state-maps-alist
  '((viper-vi-intercept-minor-mode . viper-vi-intercept-map)
    (viper-vi-minibuffer-minor-mode . viper-minibuffer-map)
    (viper-vi-local-user-minor-mode . viper-vi-local-user-map)
    (viper-vi-global-user-minor-mode . viper-vi-global-user-map)
    (viper-vi-kbd-minor-mode . viper-vi-kbd-map)
    (viper-vi-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-vi-state-modifier-alist)))
           (cdr (assoc major-mode viper-vi-state-modifier-alist))))
    (viper-vi-diehard-minor-mode . viper-vi-diehard-map)
    (viper-vi-basic-minor-mode . viper-vi-basic-map)
    (viper-insert-intercept-minor-mode . viper-insert-intercept-map)
    (viper-insert-minibuffer-minor-mode . viper-minibuffer-map)
    (viper-insert-local-user-minor-mode . viper-insert-local-user-map)
    (viper-insert-global-user-minor-mode . viper-insert-global-user-map)
    (viper-insert-kbd-minor-mode . viper-insert-kbd-map)
    (viper-insert-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-insert-state-modifier-alist)))
           (cdr (assoc major-mode viper-insert-state-modifier-alist))))
    (viper-insert-diehard-minor-mode . viper-insert-diehard-map)
    (viper-insert-basic-minor-mode . viper-insert-basic-map)
    (viper-replace-minor-mode . viper-replace-map)
    (viper-emacs-intercept-minor-mode . viper-emacs-intercept-map)
    (viper-emacs-local-user-minor-mode . viper-emacs-local-user-map)
    (viper-emacs-global-user-minor-mode . viper-emacs-global-user-map)
    (viper-emacs-kbd-minor-mode . viper-emacs-kbd-map)
    (viper-emacs-state-modifier-minor-mode
     . (if (keymapp (cdr (assoc major-mode viper-emacs-state-modifier-alist)))
           (cdr (assoc major-mode viper-emacs-state-modifier-alist)))))
  "Alist of Vimpulse modes and keymaps.
Entries have the form (MODE . MAP-EXPR), where MAP-EXPR is an
expression for determining the keymap of MODE.")

;; state-changing code: this uses the variables above
(defun vimpulse-normalize-minor-mode-map-alist ()
  "Normalize state keymaps."
  (let (local-user-mode map mode modes)
    ;; refresh `viper--intercept-key-maps'
    (setq viper--intercept-key-maps nil)
    (dolist (mode vimpulse-state-vars-alist)
      (add-to-list 'viper--intercept-key-maps
                   (cons (cdr (assq 'intercept-mode mode))
                         (eval (cdr (assq 'intercept-map mode)))) t))
    ;; refresh `viper--key-maps'
    (setq viper--key-maps (vimpulse-make-keymap-alist))
    ;; make `minor-mode-map-alist' buffer-local in older Emacs versions
    ;; lacking `emulation-mode-map-alists'
    (unless (and (fboundp 'add-to-ordered-list)
                 (boundp 'emulation-mode-map-alists))
      (set (make-local-variable 'minor-mode-map-alist)
           (viper-append-filter-alist
            (append viper--intercept-key-maps viper--key-maps)
            minor-mode-map-alist)))))

(defalias 'viper-normalize-minor-mode-map-alist 'vimpulse-normalize-minor-mode-map-alist)

(defun vimpulse-normalize-auxiliary-modes ()
  "Normalize `vimpulse-auxiliary-modes'.
Order the modes on the basis of `minor-mode-map-alist'
and remove duplicates."
  (let ((temp vimpulse-auxiliary-modes) result)
    (dolist (mode minor-mode-map-alist)
      (setq mode (car mode))
      (when (memq mode temp)
        (setq temp (delq mode temp))
        (add-to-list 'result mode t 'eq)))
    (dolist (mode temp)
      (add-to-list 'result mode t 'eq))
    (setq vimpulse-auxiliary-modes result)))

(defun vimpulse-make-toggle-alist (&optional state &rest excluded-states)
  "Make toggle alist for STATE (current if not specified)."
  (let (mode result toggle)
    (setq state (or state viper-current-state 'vi-state))
    (unless (memq state excluded-states)
      (dolist (entry (cdr (assq state vimpulse-state-modes-alist)))
        (setq toggle (cdr entry)
              entry  (car entry))
        (mapc
         (lambda (var)
           (unless (assq (car var) result)
             (if toggle
                 (add-to-list 'result var t)
               (add-to-list 'result (cons (car var) nil)))))
         (cond
          ;; state reference
          ((assq entry vimpulse-state-modes-alist)
           (apply 'vimpulse-make-toggle-alist entry state excluded-states))
          ;; auxiliary modes
          ((rassq entry vimpulse-auxiliary-modes-alist)
           (let (aux result)
             (setq entry (symbol-value entry))
             (dolist (mode vimpulse-auxiliary-modes)
               (when (and (boundp mode)
                          (symbol-value mode)
                          (assq mode entry))
                 (setq aux (cdr (assq mode entry)))
                 (unless (assq aux result)
                   (add-to-list 'result (cons aux toggle) t))))
             (when (memq major-mode vimpulse-auxiliary-modes)
               (setq aux (cdr (assq major-mode entry)))
               (unless (assq aux result)
                 (add-to-list 'result (cons aux toggle) t)))
             result))
          ;; regular mode
          (t
           (unless (assq entry result)
             (list (cons entry toggle))))))))
    result))

(defun vimpulse-make-keymap-alist (&optional state)
  "Make keymap alist for STATE (current if not specified)."
  (let (result map)
    (setq state (or state viper-current-state 'vi-state)
          result (mapcar (lambda (entry)
                           (cons (car entry)
                                 (eval (cdr (assq (car entry)
                                                  vimpulse-state-maps-alist)))))
                         (vimpulse-make-toggle-alist state)))
    (dolist (entry vimpulse-state-modes-alist)
      (dolist (mode (cdr entry))
        (setq mode (car mode))
        (unless (or (assq mode result)
                    (assq mode vimpulse-state-modes-alist)
                    (rassq mode vimpulse-auxiliary-modes-alist))
          (add-to-list 'result
                       (cons mode
                             (eval (cdr (assq mode vimpulse-state-maps-alist))))
                       t))))
    result))

(defvar viper-mode-string)
(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Refresh mode line tag for Vimpulse states."
  (let ((id (assq viper-current-state vimpulse-state-vars-alist)))
    (setq id (eval (cdr (assq 'id (cdr id)))))
    (when id
      (set (make-local-variable 'viper-mode-string) id)
      (force-mode-line-update))))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Toggle Vimpulse state modes."
  (let (enable disable)
    ;; determine which modes to enable
    (setq enable (vimpulse-make-toggle-alist state))
    ;; determine which modes to disable
    (dolist (entry vimpulse-state-modes-alist)
      (dolist (mode (mapcar 'car (cdr entry)))
        (unless (or (assq mode enable)
                    (assq mode vimpulse-state-modes-alist)
                    (rassq mode vimpulse-auxiliary-modes-alist))
          (add-to-list 'disable mode t))))
    (dolist (entry vimpulse-auxiliary-modes-alist)
      (dolist (aux (mapcar 'cdr (symbol-value (cdr entry))))
        (unless (assq aux enable)
          (add-to-list 'disable aux t))))
    ;; enable modes
    (dolist (entry enable)
      (when (boundp (car entry))
        (set (car entry) (eval (cdr entry)))))
    ;; disable modes
    (dolist (entry disable)
      (when (boundp entry)
        (set entry nil)))))

(defadvice viper-change-state (before vimpulse-states activate)
  "Update `viper-insert-point'."
  (let (mark-active)
    (unless (mark t)
      (push-mark nil t nil)))
  (when (and (eq new-state 'insert-state)
             (not (memq viper-current-state '(vi-state emacs-state))))
    (viper-move-marker-locally 'viper-insert-point (point))))

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return the empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state (assq state vimpulse-state-vars-alist))
  (setq state (eval (cdr (assq 'modifier-alist (cdr state)))))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    (copy-keymap viper-empty-keymap)))

(defun vimpulse-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then
modifications to this major mode may not take effect until the
buffer switches state to Vi, Insert or Emacs. If this happens,
add `viper-change-state-to-emacs' to this major mode's hook.
If no such hook exists, you may have to put an advice on the
function that invokes the major mode. See `viper-set-hooks'
for hints.

The above needs not to be done for major modes that come up in
Vi or Insert state by default."
  (let (alist elt)
    (setq alist (cdr (assq state vimpulse-state-vars-alist)))
    (setq alist (cdr (assq 'modifier-alist alist)))
    (if (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(defalias 'viper-modify-major-mode 'vimpulse-modify-major-mode)

(defun vimpulse-add-local-keys (state alist)
  "Override some vi-state or insert-state bindings in the current buffer.
The effect is seen in the current buffer only.
Useful for customizing  mailer buffers, gnus, etc.
STATE is 'vi-state, 'insert-state, or 'emacs-state
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
      (viper-add-local-keys state '((key-str . func) (key-str . func)...))"
  (let (local-user-map need-local-map)
    (setq local-user-map (cdr (assq state vimpulse-state-vars-alist)))
    (when local-user-map
      (setq need-local-map
            (cdr (assq 'need-local-map local-user-map)))
      (setq local-user-map
            (cdr (assq 'local-user-map local-user-map)))
      (when (symbol-value need-local-map)
        (set local-user-map (make-sparse-keymap))
        (set need-local-map nil))
      (viper-modify-keymap (symbol-value local-user-map) alist)
      (viper-normalize-minor-mode-map-alist)
      (viper-set-mode-vars-for viper-current-state))))

(defalias 'viper-add-local-keys 'vimpulse-add-local-keys)

;; Macro for defining new Viper states. This saves us the trouble of
;; defining and indexing all those minor modes manually.
(defmacro vimpulse-define-state (state doc &rest body)
  "Define a new Viper state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:id ID                  Mode line indicator.
:hook LIST              Hooks run before changing to STATE.
:change-func FUNC       Function to change to STATE.
:basic-mode MODE        Basic minor mode for STATE.
:basic-map MAP          Keymap of :basic-mode.
:diehard-mode MODE      Minor mode for when Viper wants to be vi.
:diehard-map MAP        Keymap of :diehard-mode.
:modifier-mode MODE     Minor mode for modifying major modes.
:modifier-alist LIST    Keymap alist for :modifier-mode.
:kbd-mode MODE          Minor mode for Ex command macros.
:kbd-map MAP            Keymap of :kbd-mode.
:global-user-mode MODE  Minor mode for global user bindings.
:global-user-map MAP    Keymap of :global-user-mode.
:local-user-mode MODE   Minor mode for local user bindings.
:local-user-map MAP     Keymap of :local-user-mode.
:need-local-map VAR     Buffer-local variable for :local-user-mode.
:intercept-mode         Minor mode for vital Viper bindings.
:intercept-map          Keymap of :intercept-mode.
:enable LIST            List of other modes enabled by STATE.
:prefix PREFIX          Variable prefix, default \"vimpulse-\".
:advice TYPE            Toggle advice type, default `after'.

It is not necessary to specify all of these; the minor modes are
created automatically unless one provides an existing mode. The
only keyword one should really specify is :id, the mode line tag.
For example:

    (vimpulse-define-state test
      \"A simple test state.\"
      :id \"<T> \")

The basic keymap of this state will then be
`vimpulse-test-basic-map', and so on.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. This is stored in a `defadvice'
of `viper-change-state'. :advice specifies the advice type
\(default `after'). The advice runs :hook before completing."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let (advice auxiliary-modes basic-map basic-mode change-func
        diehard-map diehard-mode enable global-user-map
        global-user-mode hook id intercept-map intercept-mode kbd-map
        kbd-mode keyword local-user-map local-user-mode modifier-alist
        modifier-mode name name-string need-local-map prefix
        prefixed-name-string state-name state-name-string)
    ;; collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq keyword :prefix)
        (setq prefix (vimpulse-unquote (pop body))))
       ((eq keyword :enable)
        (setq enable (vimpulse-unquote (pop body))))
       ((eq keyword :advice)
        (setq advice (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-id :id))
        (setq id (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-hook :hook))
        (setq hook (vimpulse-unquote (pop body))))
       ((memq keyword '(:change-func :change))
        (setq change-func (vimpulse-unquote (pop body))))
       ((memq keyword '(:basic-mode :basic-minor-mode))
        (setq basic-mode (vimpulse-unquote (pop body))))
       ((eq keyword :basic-map)
        (setq basic-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-mode
                        :local-mode
                        :local-user-minor-mode))
        (setq local-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-map :local-map))
        (setq local-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:need-new-local-map
                        :need-local-map
                        :need-map))
        (setq need-local-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-mode
                        :global-mode
                        :global-user-minor-mode))
        (setq global-user-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-map :global-map))
        (setq global-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-minor-mode
                        :state-modifier-mode
                        :modifier-minor-mode
                        :modifier-mode))
        (setq modifier-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-alist :modifier-alist))
        (setq modifier-alist (vimpulse-unquote (pop body))))
       ((memq keyword '(:diehard-mode :diehard-minor-mode))
        (setq diehard-mode (vimpulse-unquote (pop body))))
       ((eq keyword :diehard-map)
        (setq diehard-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:kbd-mode :kbd-minor-mode))
        (setq kbd-mode (vimpulse-unquote (pop body))))
       ((eq keyword :kbd-map)
        (setq kbd-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:intercept-mode :intercept-minor-mode))
        (setq intercept-mode (vimpulse-unquote (pop body))))
       ((eq keyword :intercept-map)
        (setq intercept-map (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    ;; set up the state name etc.
    (setq name-string (replace-regexp-in-string
                       "-state$" "" (symbol-name state)))
    (setq name (intern name-string))
    (setq state-name-string (concat name-string "-state"))
    (setq state-name (intern state-name-string))
    (when (and prefix (symbolp prefix))
      (setq prefix (symbol-name prefix)))
    (setq prefix (or prefix "vimpulse-"))
    (setq prefix (concat (replace-regexp-in-string
                          "-$" "" prefix) "-"))
    (setq prefixed-name-string (concat prefix name-string))
    (setq advice (or advice 'after))
    (setq auxiliary-modes (intern (concat prefixed-name-string
                                          "-auxiliary-modes")))
    (unless (and change-func (symbolp change-func))
      (setq change-func
            (intern (concat prefix "change-state-to-" name-string))))
    ;; macro expansion
    `(progn
       ;; define change function
       (defun ,change-func ()
         ,(format "Change Viper state to %s." state-name)
         (viper-change-state ',state-name))
       ;; define state variables etc.
       (let* ((advice ',advice)
              (auxiliary-modes ',auxiliary-modes)
              (change-func ',change-func)
              (doc ',doc)
              (enable ',enable)
              (name ',name)
              (name-string ',name-string)
              (prefix ',prefix)
              (prefixed-name-string ',prefixed-name-string)
              (state-name ',state-name)
              (state-name-string ',state-name-string)
              (basic-map (vimpulse-define-symbol
                          ',basic-map (concat prefixed-name-string
                                              "-basic-map")
                          (make-sparse-keymap) 'keymapp
                          (format "The basic %s keymap.\n\n%s"
                                  state-name doc)))
              (basic-mode (vimpulse-define-symbol
                           ',basic-mode
                           (concat prefixed-name-string
                                   "-basic-minor-mode")
                           nil nil
                           (format "Basic minor mode for %s.\n\n%s"
                                   state-name doc) t))
              (diehard-map (vimpulse-define-symbol
                            ',diehard-map
                            (concat prefixed-name-string
                                    "-diehard-map")
                            (make-sparse-keymap) 'keymapp
                            (format "This keymap is in use when the \
user asks Viper to simulate vi very closely.
This happens when `viper-expert-level' is 1 or 2.  \
See `viper-set-expert-level'.\n\n%s" doc)))
              (diehard-mode (vimpulse-define-symbol
                             ',diehard-mode
                             (concat prefixed-name-string
                                     "-diehard-minor-mode")
                             nil nil
                             (format "This minor mode is in effect \
when the user wants Viper to be vi.\n\n%s" doc) t))
              (global-user-map (vimpulse-define-symbol
                                ',global-user-map
                                (concat prefixed-name-string
                                        "-global-user-map")
                                (make-sparse-keymap) 'keymapp
                                (format "Auxiliary map for global \
user-defined keybindings in %s.\n\n%s" state-name doc)))
              (global-user-mode (vimpulse-define-symbol
                                 ',global-user-mode
                                 (concat prefixed-name-string
                                         "-global-user-minor-mode")
                                 nil nil
                                 (format "Auxiliary minor mode for \
global user-defined bindings in %s.\n\n%s" state-name doc) t))
              (hook (vimpulse-define-symbol
                     ',hook (concat prefixed-name-string
                                    "-state-hook")
                     nil 'listp
                     (format "*Hooks run just before the switch to %s \
is completed.\n\n%s" state-name doc)))
              (id (vimpulse-define-symbol
                   ',id (concat prefixed-name-string "-state-id")
                   (format "<%s> " (upcase name-string)) 'stringp
                   (format "Mode line tag indicating %s.\n\n%s"
                           state-name doc)))
              (intercept-map (vimpulse-define-symbol
                              ',intercept-map
                              (concat prefixed-name-string
                                      "-intercept-map")
                              viper-vi-intercept-map 'keymapp
                              (format "Keymap for binding Viper's \
vital keys.\n\n%s" doc)))
              (intercept-mode (vimpulse-define-symbol
                               ',intercept-mode
                               (concat prefixed-name-string
                                       "-intercept-minor-mode")
                               nil nil
                               (format "Mode for binding Viper's \
vital keys.\n\n%s" doc)))
              (kbd-map (vimpulse-define-symbol
                        ',kbd-map
                        (concat prefixed-name-string "-kbd-map")
                        (make-sparse-keymap) 'keymapp
                        (format "This keymap keeps keyboard macros \
defined via the :map command.\n\n%s" doc)))
              (kbd-mode (vimpulse-define-symbol
                         ',kbd-mode
                         (concat prefixed-name-string
                                 "-kbd-minor-mode")
                         nil nil
                         (format "Minor mode for Ex command macros \
in Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.\n\n%s" doc) t))
              (local-user-map (vimpulse-define-symbol
                               ',local-user-map
                               (concat prefixed-name-string
                                       "-local-user-map")
                               (make-sparse-keymap) 'keymapp
                               (format "Auxiliary map for per-buffer \
user-defined keybindings in %s.\n\n%s" state-name doc) t))
              (local-user-mode (vimpulse-define-symbol
                                ',local-user-mode
                                (concat prefixed-name-string
                                        "-local-user-minor-mode")
                                nil nil
                                (format "Auxiliary minor mode for \
user-defined local bindings in %s.\n\n%s" state-name doc) t))
              (modifier-alist (vimpulse-define-symbol
                               ',modifier-alist
                               (concat prefixed-name-string
                                       "-state-modifier-alist")
                               nil 'listp))
              (modifier-mode (vimpulse-define-symbol
                              ',modifier-mode
                              (concat prefixed-name-string
                                      "-state-modifier-minor-mode")
                              nil nil
                              (format "Minor mode used to make major \
mode-specific modifications to %s.\n\n%s" state-name doc) t))
              (need-local-map (vimpulse-define-symbol
                               ',need-local-map
                               (concat prefix "need-new-"
                                       name-string "-local-map")
                               t (lambda (val) (eq val t)) nil t))
              enable-modes-alist enable-states-alist
              modes-alist vars-alist)
         (put need-local-map 'permanent-local t)
         (defvar ,auxiliary-modes nil)
         (add-to-list 'vimpulse-auxiliary-modes-alist
                      (cons ',state-name ',auxiliary-modes) t)
         ;; remove old index entries
         (dolist (entry (list basic-mode
                              diehard-mode
                              modifier-mode
                              kbd-mode
                              global-user-mode
                              local-user-mode
                              intercept-mode))
           (setq vimpulse-state-maps-alist
                 (assq-delete-all entry vimpulse-state-maps-alist)))
         (setq vimpulse-state-modes-alist
               (assq-delete-all state-name vimpulse-state-modes-alist))
         (setq vimpulse-state-vars-alist
               (assq-delete-all state-name vimpulse-state-vars-alist))
         ;; index keymaps
         (add-to-list 'vimpulse-state-maps-alist
                      (cons basic-mode basic-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons diehard-mode diehard-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons modifier-mode
                            `(if (keymapp
                                  (cdr (assoc major-mode
                                              ,modifier-alist)))
                                 (cdr (assoc major-mode
                                             ,modifier-alist)))))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons kbd-mode kbd-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons global-user-mode global-user-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons local-user-mode local-user-map))
         (add-to-list 'vimpulse-state-maps-alist
                      (cons intercept-mode intercept-map))
         ;; index minor mode toggling: first, sort lists from symbols
         ;; in :enable
         (unless (listp enable)
           (setq enable (list enable)))
         (dolist (entry enable)
           (let ((mode entry) (val t))
             (when (listp entry)
               (setq mode (car entry)
                     val  (cadr entry)))
             (when (and mode (symbolp mode))
               (add-to-list 'enable-modes-alist (cons mode val) t))))
         ;; then add the state's own modes to the front if they're not
         ;; already there
         (dolist (mode (list (cons basic-mode t)
                             (cons diehard-mode
                                   '(not (or viper-want-emacs-keys-in-vi
                                             (viper-is-in-minibuffer))))
                             (cons modifier-mode t)
                             (cons kbd-mode '(not (viper-is-in-minibuffer)))
                             (cons global-user-mode t)
                             (cons auxiliary-modes t)
                             (cons local-user-mode t)
                             (cons intercept-mode t)))
           (unless (assq (car mode) enable-modes-alist)
             (add-to-list 'enable-modes-alist mode)))
         ;; add the result to `vimpulse-state-modes-alist'
         (add-to-list 'vimpulse-state-modes-alist
                      (cons state-name enable-modes-alist) t)
         (viper-normalize-minor-mode-map-alist)
         ;; index state variables
         (setq vars-alist
               (list (cons 'id id)
                     (cons 'hook hook)
                     (cons 'auxiliary-modes auxiliary-modes)
                     (cons 'change-func change-func)
                     (cons 'basic-mode basic-mode)
                     (cons 'basic-map basic-map)
                     (cons 'diehard-mode diehard-mode)
                     (cons 'diehard-map diehard-map)
                     (cons 'modifier-mode modifier-mode)
                     (cons 'modifier-alist modifier-alist)
                     (cons 'kbd-mode kbd-mode)
                     (cons 'kbd-map kbd-map)
                     (cons 'global-user-mode global-user-mode)
                     (cons 'global-user-map global-user-map)
                     (cons 'local-user-mode local-user-mode)
                     (cons 'local-user-map local-user-map)
                     (cons 'need-local-map need-local-map)
                     (cons 'intercept-mode intercept-mode)
                     (cons 'intercept-map intercept-map)))
         (add-to-list 'vimpulse-state-vars-alist
                      (cons state-name vars-alist) t)
         ;; make toggle-advice
         (eval `(defadvice viper-change-state (,advice ,state-name activate)
                  ,(format "Toggle %s." state-name)
                  ,',@body
                  (when (eq new-state ',state-name)
                    (run-hooks ',hook))))
         ',state))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(vimpulse-define-[-[:word:]]+\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t)))))

(defun vimpulse-define-symbol
  (sym-or-val varname varval &optional val-p doc local)
  "Accept a symbol or a value and define a variable for it.
If SYM-OR-VAL is a symbol, set that symbol's value to VARVAL.
If SYM-OR-VAL is a value, set VARNAME's value to SYM-OR-VAL.
VAL-P checks whether SYM-OR-VAL's value is \"valid\", in which
case it is kept; otherwise we default to VARVAL. DOC is the
docstring for the defined variable. If LOCAL is non-nil,
create a buffer-local variable. Returns the result."
  (cond
   ((and sym-or-val (symbolp sym-or-val)) ; nil is a symbol
    (setq varname sym-or-val))
   ((or (not val-p) (funcall val-p sym-or-val))
    (setq varval sym-or-val)))
  (when (stringp varname)
    (setq varname (intern varname)))
  (unless (and (boundp varname) val-p
               (funcall val-p (eval varname)))
    (eval `(defvar ,varname (quote ,varval) ,doc))
    (set varname varval)
    (when local
      (make-variable-buffer-local varname)))
  varname)

;;; Viper bugs (should be forwarded to Michael Kifer)

;; `viper-deflocalvar's definition lacks a `declare' statement,
;; so Emacs tends to mess up the indentation. Luckily, the
;; relevant symbol properties can be set up with `put'.
;; TODO: E-mail Michael Kifer about updating the definition.
(put 'viper-deflocalvar 'lisp-indent-function 'defun)
(put 'viper-loop 'lisp-indent-function 'defun)
(put 'viper-deflocalvar 'function-documentation
     "Define VAR as a buffer-local variable.
DEFAULT-VALUE is the default value and DOCUMENTATION is the
docstring. The variable becomes buffer-local whenever set.")

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(viper-deflocalvar\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
     ("(\\(viper-loop\\)\\>" 1 font-lock-keyword-face))))

;; search bug: `viper-search' flashes twice when search wraps
(defun vimpulse-search
  (string forward arg
          &optional no-offset init-point fail-if-not-found dont-flash)
  (if (not (equal string ""))
      (let ((val (viper-p-val arg))
            (com (viper-getcom arg))
            (offset (not no-offset))
            (start-point (or init-point (point))))
        (viper-deactivate-mark)
        ;; smartcase searching: upper-case chars disable case folding
        (when search-upper-case
          (setq case-fold-search
                (isearch-no-upper-case-p
                 viper-s-string viper-re-search)))
        (if forward
            (condition-case nil
                (progn
                  (if offset (viper-forward-char-carefully))
                  (if viper-re-search
                      (progn
                        (re-search-forward string nil nil val)
                        (re-search-backward string))
                    (search-forward string nil nil val)
                    (search-backward string))
                  (if (not (equal (point) start-point))
                      (push-mark start-point t)))
              (search-failed
               (if (and (not fail-if-not-found)
                        viper-search-wrap-around)
                   (progn
                     (message "Search wrapped around BOTTOM of buffer")
                     (goto-char (point-min))
                     (viper-search string forward (cons 1 com)
                                   t start-point 'fail)
                     (setq dont-flash t)
                     ;; don't wait in macros
                     (or executing-kbd-macro
                         (memq viper-intermediate-command
                               '(viper-repeat
                                 viper-digit-argument
                                 viper-command-argument))
                         (sit-for 2))
                     ;; delete the wrap-around message
                     (message ""))
                 (goto-char start-point)
                 (error "`%s': %s not found"
                        string
                        (if viper-re-search "Pattern" "String")))))
          ;; backward
          (condition-case nil
              (progn
                (if viper-re-search
                    (re-search-backward string nil nil val)
                  (search-backward string nil nil val))
                (if (not (equal (point) start-point))
                    (push-mark start-point t)))
            (search-failed
             (if (and (not fail-if-not-found) viper-search-wrap-around)
                 (progn
                   (message "Search wrapped around TOP of buffer")
                   (goto-char (point-max))
                   (viper-search string forward (cons 1 com)
                                 t start-point 'fail)
                   (setq dont-flash t)
                   ;; don't wait in macros
                   (or executing-kbd-macro
                       (memq viper-intermediate-command
                             '(viper-repeat
                               viper-digit-argument
                               viper-command-argument))
                       (sit-for 2))
                   ;; delete the wrap-around message
                   (message ""))
               (goto-char start-point)
               (error "`%s': %s not found"
                      string
                      (if viper-re-search "Pattern" "String"))))))
        ;; pull up or down if at top/bottom of window
        (viper-adjust-window)
        ;; highlight the result of search;
        ;; don't wait and don't highlight in macros
        (or dont-flash
            executing-kbd-macro
            (memq viper-intermediate-command
                  '(viper-repeat
                    viper-digit-argument
                    viper-command-argument))
            (viper-flash-search-pattern)))))

(defalias 'viper-search 'vimpulse-search)

;; e/E bug: on a single-letter word, ce may change two words
(defun vimpulse-end-of-word-kernel ()
  (when (viper-looking-at-separator)
    (viper-skip-all-separators-forward))
  (cond
   ((viper-looking-at-alpha)
    (viper-skip-alpha-forward "_"))
   ((not (viper-looking-at-alphasep))
    (viper-skip-nonalphasep-forward))))

(defun vimpulse-end-of-word (arg &optional careful)
  "Move point to end of current word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point))
      (when (and (not (viper-looking-at-alpha))
                 (not (viper-looking-at-alphasep)))
        (setq val (1+ val))))
     ((viper-end-of-word-p)
      (setq val (1+ val))))
    (viper-loop val (viper-end-of-word-kernel))
    (if com
        (viper-execute-com 'viper-end-of-word val com)
      (viper-backward-char-carefully))))

(defun vimpulse-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (cond
     (com
      (viper-move-marker-locally 'viper-com-point (point)))
     ((save-excursion
        (viper-forward-char-carefully)
        (or (eolp) (memq (char-syntax (char-after)) '(?\  ?- nil))))
      (setq val (1+ val))))
    (viper-loop val
      (viper-end-of-word-kernel)
      (viper-skip-nonseparators 'forward))
    (if com
        (viper-execute-com 'viper-end-of-Word val com)
      (viper-backward-char-carefully))))

(defalias 'viper-end-of-word-kernel 'vimpulse-end-of-word-kernel)
(defalias 'viper-end-of-word 'vimpulse-end-of-word)
(defalias 'viper-end-of-Word 'vimpulse-end-of-Word)

(provide 'vimpulse-viper-function-redefinitions)
