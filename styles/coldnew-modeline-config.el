;;; coldnew-modeline-config.el --- coldnew's modeline-config.

;; Copyright (C) 2015 - 2016 Yen-Chin, Lee.

;; Author: coldnew <coldnew.tw@gmail.com>
;; Kyewords: themes
;; Version: 0.3
;; X-Original-Version: 0.3
;; Package-Requires: ((emacs "24.3"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
;; (require 'spaceline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package telephone-line
  :ensure t
  :config
  ;; Need to create custom segments
  (require 'telephone-line-utils)

  ;; Set default separators: choose either of them
  (setq telephone-line-primary-left-separator 'telephone-line-identity-left)
  (setq telephone-line-primary-right-separator 'telephone-line-identity-right)
  ;; OR
  ;; (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
  ;; (setq telephone-line-primary-right-separator 'telephone-line-cubed-right)

  ;; Set subseparator
  (if window-system
      (progn
        (setq telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
        (setq telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right)))

;;;; Custom segments

  ;; Example of color string segment
  ;; (telephone-line-defsegment* my-color-segment
  ;;   (propertize "some-string" 'face `(:foreground "green")))


  ;; TODO: Rewrite using assoc and defvar
  ;; Display major mode
  (telephone-line-defsegment* my-major-mode-segment
                              (let ((mode (cond
                                           ((string= mode-name "Fundamental") "Text")
                                           ((string= mode-name "Emacs-Lisp") "Elisp")
                                           ((string= mode-name "Javascript-IDE") "Javascript")
                                           (t mode-name))))
                                (propertize mode 'face `(:foreground "#835d83"))))

  ;; Display evil state
  (telephone-line-defsegment* my-evil-segment
                              (if (telephone-line-selected-window-active)
                                  (let ((tag (cond
                                              ((string= evil-state "normal") ":")
                                              ((string= evil-state "insert") ">")
                                              ((string= evil-state "replace") "r")
                                              ((string= evil-state "visual") "v")
                                              ((string= evil-state "operator") "=")
                                              ((string= evil-state "motion") "m")
                                              ((string= evil-state "emacs") "Emacs")
                                              ((string= evil-state "multiedit") "Multi")
                                              (t "-"))))
                                    tag)))

  ;; Display buffer name
  (telephone-line-defsegment* my-buffer-segment
                              `(""
                                ,(telephone-line-raw mode-line-buffer-identification t)))


  ;; Display current position in a buffer
  (telephone-line-defsegment* my-position-segment
                              (if (telephone-line-selected-window-active)
                                  (if (eq major-mode 'paradox-menu-mode)
                                      (telephone-line-trim (format-mode-line mode-line-front-space))
                                      '(" %3l,%2c "))))

  ;; Ignore some buffers in modeline
  (defvar modeline-ignored-modes nil
    "List of major modes to ignore in modeline")

  (setq modeline-ignored-modes '("Dashboard"
                                 "Warnings"
                                 "Compilation"
                                 "EShell"
                                 "REPL"
                                 "Messages"))

  ;; Display modified status
  (telephone-line-defsegment* my-modified-status-segment
                              (if (and (buffer-modified-p) (not (member mode-name modeline-ignored-modes)))
                                  (propertize "+" 'face `(:foreground "#85b654"))
                                  ""))

  ;; Display encoding system
  (telephone-line-defsegment* my-coding-segment
                              (if (telephone-line-selected-window-active)
                                  (let* ((code (symbol-name buffer-file-coding-system))
                                         (eol-type (coding-system-eol-type buffer-file-coding-system))
                                         (eol (cond
                                               ((eq 0 eol-type) "unix")
                                               ((eq 1 eol-type) "dos")
                                               ((eq 2 eol-type) "mac")
                                               (t ""))))
                                    (concat eol " "))))

  ;; Left edge
  (setq telephone-line-lhs
        '((accent . "  ")
          (evil   . (my-evil-segment))
          (nil    . (my-buffer-segment))
          (nil    . (my-modified-status-segment))))

  ;; Right edge
  (setq telephone-line-rhs
        '((nil     . (telephone-line-misc-info-segment))
          (accent  . (my-position-segment))
          (nil     . (my-major-mode-segment))
          (accent  . (my-coding-segment))))

  (telephone-line-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'coldnew-modeline-config)
;;; coldnew-modeline-config.el ends here
