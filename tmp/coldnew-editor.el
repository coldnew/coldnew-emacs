;;; coldnew-editor.el --- enhanced core editing experience.
(eval-when-compile (require 'cl))


(require 'coldnew-complete)




;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; View-mode
;;;; ---------------------------------------------------------------------------

;;; view-mode is a `read-only' mode
(setq view-read-only t)

(define-key view-mode-map "i" 'coldnew/switch-to-emacs-mode)
(define-key view-mode-map "h" 'backward-char)
(define-key view-mode-map "j" 'next-line)
(define-key view-mode-map "k" 'previous-line)
(define-key view-mode-map "l" 'forward-char)
(define-key view-mode-map (kbd "C-f") 'View-scroll-page-forward)
(define-key view-mode-map (kbd "C-b") 'View-scroll-page-backward)


;;;; ---------------------------------------------------------------------------
;;;; center-cursor
;;;; ---------------------------------------------------------------------------
;;(require* 'centered-cursor-mode)
;;(global-centered-cursor-mode +1)

;;;; ---------------------------------------------------------------------------
;;;; minimap
;;;; ---------------------------------------------------------------------------
;; minimap is really funny :)
(require* 'minimap)

;;;; ---------------------------------------------------------------------------
;;;; zone
;;;; ---------------------------------------------------------------------------

;; Uncomment this if you'd like your Emacs session to do amusing
;; things after 3 minutes of idle time.  Hitting a key will stop the
;; madness :^) .
;;
(require 'zone)
(setq zone-idle (* 60 300 1000))
(zone-when-idle zone-idle)



;;;; ---------------------------------------------------------------------------
;;;; auto-indent-mode
;;;; ---------------------------------------------------------------------------

;; (require* 'auto-indent-mode)
;; (auto-indent-global-mode)
;; (setq auto-indent-on-save-file t)
;; (setq auto-indent-untabify-on-save-file t)
;; (setq auto-indent-delete-trailing-whitespace-on-save-file t)

;;;; ---------------------------------------------------------------------------
;;;; key-chord
;;;; ---------------------------------------------------------------------------
(require 'key-chord)
;;(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)



;;;; ---------------------------------------------------------------------------
;;;; dtrt-indent
;;;; ---------------------------------------------------------------------------
(require 'dtrt-indent)
(add-hook 'coldnew-editor-hook '(lambda () (dtrt-indent-mode t)))



;;;; ---------------------------------------------------------------------------
;;;; linum-ace
;;;; ---------------------------------------------------------------------------
;; (require* 'linum-ace)
;; (setq linum-format 'linum-ace)



;;;; ---------------------------------------------------------------------------
;;;; doxymacs
;;;; ---------------------------------------------------------------------------
(require 'doxymacs)





;;;; ---------------------------------------------------------------------------
;;;; Common language setting
;;;; ---------------------------------------------------------------------------



;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------











(provide 'coldnew-editor)
;; coldnew-editor.el ends here.
