;; init org mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


;;  Hooks
(add-hook 'org-mode-hook
	  '(lambda ()
	     (setq org-hide-leading-stars t)
	     (setq org-log-done t)
	     (setq org-log-done 'time)	; 對已完成事項加上時間
	     (setq org-tag-alist '(
				   ("Programming" . ?p)
				   ("Lab"         . ?l)
				   ("Home"        . ?h)
				   ))
	     (setq org-todo-keywords
		   '("TODO(T)" "STARTED(S)" "WAITING(W)" "|" "CANCELED(C)" "DONE(D)"))
	     (org-turn-on-iimage-in-org) ; display image on load
	     ))

;;;;; Keybinding
(add-hook 'org-mode-hook
	  '(lambda ()
	     (when (require 'rc-vim nil 'noerror)
	       (vim:nmap "\C-l" 'org-store-link) ;
	       (vim:nmap "\C-a" 'org-agenda)	 ; 進入日程表
	       (vim:nmap "\C-b" 'org-iswitchb)

	       (vim:imap (kbd "M-t") 'org-insert-todo-heading)
	       (vim:imap (kbd "C-t") 'org-insert-todo-heading-respect-content)
	       )))


;;;; Extra Settings
;; allow for export=>beamer by placing
;; see http://emacs-fu.blogspot.com/2009/10/writing-presentations-with-org-mode-and.html
;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
	     ;; beamer class, for presentations
	     '("beamer"
	       "\\documentclass[11pt]{beamer}\n
		\\mode<{{{beamermode}}}>\n
      		\\usetheme{{{{beamertheme}}}}\n
  		\\usecolortheme{{{{beamercolortheme}}}}\n
		\\beamertemplateballitem\n
		\\setbeameroption{show notes}
		\\usepackage[utf8]{inputenc}\n
		\\usepackage[T1]{fontenc}\n
		\\usepackage{hyperref}\n
		\\usepackage{color}
		\\usepackage{listings}
		\\lstset{numbers=none,language=[ISO]C++,tabsize=4,
			frame=single,
			basicstyle=\\small,
			showspaces=false,showstringspaces=false,
			showtabs=false,
			keywordstyle=\\color{blue}\\bfseries,
			commentstyle=\\color{red},
			}\n
		 \\usepackage{verbatim}\n
		 \\institute{{{{beamerinstitute}}}}\n
		  \\subject{{{{beamersubject}}}}\n"

	       ("\\section{%s}" . "\\section*{%s}")

	       ("\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}"
		"\\begin{frame}[fragile]\\frametitle{%s}"
		"\\end{frame}")))

;; letter class, for formal letters

(add-to-list 'org-export-latex-classes

	     '("letter"
	       "\\documentclass[11pt]{letter}\n
		\\usepackage[utf8]{inputenc}\n
		\\usepackage[T1]{fontenc}\n
		 \\usepackage{color}"

	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;;;;;; Functions

;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))








(provide 'rc-org-mode)
;; rc-org-mode.el ends here
