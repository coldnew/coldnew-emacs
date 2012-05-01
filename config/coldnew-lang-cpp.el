;;; coldnew-lang-cpp.el ---
(eval-when-compile (require 'cl))

;;;; c++-mode extensions
(add-to-list 'auto-mode-alist '("\\.hpp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
(add-to-list 'magic-mode-alist
             `(,(lambda ()
                  (and (string= (file-name-extension buffer-file-name) "h")
                       (re-search-forward "\\W\\(class\\|template\\namespace\\)\\W"
                                          magic-mode-regexp-match-limit t)))
               . c++-mode))


;;;; ---------------------------------------------------------------------------
;;;; Hooks
;;;; ---------------------------------------------------------------------------

;; use my cc-mode-common-setting
(add-hook 'c++-mode-hook 'coldnew-cc-mode-common-setting)

;; my auto-complete for cpp
(add-hook 'c++-mode-hook 'ac-cpp-mode-setup)

;; Enable c-eldoc
(require 'c-eldoc)
(setq c-eldoc-includes "`pkg-config gtk+-3.0 opencv --cflags --libs` -I./ -I../")
(c-turn-on-eldoc-mode)

;;;; CodingStyle
(add-hook 'c++-mode-hook
          '(lambda ()

             ;; TODO: add comment here
             (setq c-macro-shrink-window-flag t)
             (setq c-macro-preprocessor "cpp")
             (setq c-macro-cppflags " ")
             (setq c-macro-prompt-flag t)

             ;; Use linux-kernel style
             (c-set-style "linux")

             ;; Setting indentation lvel
             (setq c-basic-offset 4)

             ;; Make TAB equivilent to 4 spaces
             (setq tab-width 4)

             ;; Use spaces to indent instead of tabs.
             (setq indent-tabs-mode nil)

             ;; Indent the continuation by 2
             (setq c-continued-statement-offset 2)

             ;; Brackets should be at same indentation level as the statements they open
             ;; for example:
             ;;                 if (0)        becomes        if (0)
             ;;                     {                        {
             ;;                        ;                         ;
             ;;                     }                        }
             (c-set-offset 'substatement-open '0)
             ;; make open-braces after a case
             (c-set-offset 'case-label '+)

             ))


;;;; ---------------------------------------------------------------------------
;;;; Keybindings
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Auto Complete
;;;; ---------------------------------------------------------------------------
(require 'auto-complete-clang)
(defun ac-cpp-mode-setup ()
  "auto-complete settings for c-mode."
  (setq ac-sources '(
                     ac-source-clang
                     ac-source-dictionary
                     ac-source-abbrev
                     ac-source-semantic
                     ac-source-filename
                     ac-source-files-in-current-dir
                     ac-source-words-in-same-mode-buffers
                     ))
  ;; Default clang completion flags
  ;;    (setq clang-completion-flags
  (setq ac-clang-flags
        (split-string
         (concat
          "-pthread -I./ -I../ "
          (shell-command-to-string "pkg-config --cflags-only-I opencv gtk+-3.0"))))
  )

;;;; ---------------------------------------------------------------------------
;;;; Flymake
;;;; ---------------------------------------------------------------------------

;;;; ---------------------------------------------------------------------------
;;;; Functions
;;;; ---------------------------------------------------------------------------


;;;; ---------------------------------------------------------------------------
;;;; Commands
;;;; ---------------------------------------------------------------------------


(provide 'coldnew-lang-cpp)
;; coldnew-lang-cpp.el ends here.
