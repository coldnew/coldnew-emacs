;;
(eval-when-compile (require 'cl))


;;;;;;;; Packages Import
(require 'coldnew-editor)
(require 'cc-mode)

;;;;;;;; c-mode extensions
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))


;;;;;;;; Auto Complete Settings
(when (require* 'auto-complete)
  (require* 'auto-complete-clang)
  ;; Setting my c-mode auto-complete source
  (defun ac-c-mode-setup ()
    "auto-complete settings for c-mode."
    (setq ac-sources '(ac-source-clang-complete
                       ac-source-dictionary
                       ac-source-abbrev
                       ac-source-semantic
                       ac-source-filename
                       ac-source-files-in-current-dir
                       ac-source-words-in-same-mode-buffers
                       ))
    )
  ;; Default clang completion flags, generate by "pkg-config --cflags gtk+-3.0"
  (setq clang-completion-flags
        (split-string "-pthread
                       -I/usr/include/gtk-3.0  -I/usr/include/gtk-3.0/include
                       -I/usr/include/glib-2.0 -I/usr/lib64/glib-2.0/include
                       -I/usr/include/gdk-pixbuf-2.0
                       -I/usr/include/atk-1.0
                       -I/usr/include/cairo
                       -I/usr/include/pango-1.0
                       -I/usr/include/gio-unix-2.0/
                       -I/usr/include/pixman-1
                       -I/usr/include/freetype2"
                      ))
  )

;;;;;;;; Coding-Style Setting
(add-hook 'c-mode-hook
          '(lambda ()

             ;; Use linux-kernel style
             (c-set-style "linux")

             ;; Setting indentation lvel
             (setq c-basic-offset 8)

             ;; Make TAB equivilent to 8 spaces
             (setq tab-width 8)

             ;; Use spaces to indent instead of tabs.
             (setq indent-tabs-mode nil)

             ;; Indent the continuation by 2
             (setq c-continued-statement-offset 2)

             ;; Brackets should be at same indentation level as the statements they open
             (c-set-offset 'substatement-open '0)

             ;; TODO:test this function
             ;; Handle longname argument in functions
             (c-set-offset 'arglist-intro '+)

             ))

;;;;;;;; Hooks
(add-hook 'c-mode-hook
          '(lambda ()

             ;; Enable Auto Complete
             (when (require* 'auto-complete)
               (ac-c-mode-setup))

             ;; Enable c-eldoc
             (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../")
             (when (require* 'c-eldoc)
               (c-turn-on-eldoc-mode))

             ;; Automatically determine c-basic-offset
             (when (require* 'guess-offset))

             ;; Use global programming mode
             (programming-mode)


             ))

;;;;;;;; make cedet integrated with c
(when (require 'cedet)
  (semantic-add-system-include "/usr/include" 'c-mode)
  )


(provide 'coldnew-lang-c)
;; coldnew-lang-c.el ends here.
