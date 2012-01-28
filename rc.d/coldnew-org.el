;; Use for org-mode
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)
(require 'org-install)
(require 'org-latex)
(require 'xml-rpc)
(require 'org2blog)

;;;;;;;; org-mode extensions
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

;;;;;;;; Configure

(add-hook 'org-mode-hook
	  '(lambda ()

	     ;; do not show leading stars
	     (setq org-hide-leading-stars t)

	     ;; Latex Setting
	     (setq org-latex-to-pdf-process
		   '("xelatex -interaction nonstopmode -shell-escape %f")
		   )
	     ;;(setq org-latex-to-pdf-process '("texi2dvi --pdf --clean --verbose --batch %f"))
	     (setq org-src-window-setup 'current-window)
	     ))
;;;;;;;; Hooks
(add-hook 'org-mode-hook
	  '(lambda ()

	     ;; Use global programming mode
	     (programming-mode)
	     ))

;;;;;;;; Keybindings
(add-hook 'org-mode-hook
	  '(lambda ()
	     (define-key evil-normal-state-local-map (kbd "TAB") 'org-cycle)
	     (define-key evil-normal-state-local-map (kbd "C-t") 'org-tod)
	     (define-key evil-normal-state-local-map (kbd "C-k") 'outline-previous-visible-heading)
	     (define-key evil-normal-state-local-map (kbd "C-j") 'outline-next-visible-heading)
	     (define-key evil-normal-state-local-map (kbd "C-l") 'org-forward-same-level)
	     (define-key evil-normal-state-local-map (kbd "C-h") 'org-backward-same-level)
	     (define-key evil-normal-state-local-map (kbd "C-c k") 'org-shiftup)
	     (define-key evil-normal-state-local-map (kbd "C-c j") 'org-shiftdown)
	     (define-key evil-normal-state-local-map (kbd "C-c l") 'org-shiftright)
	     (define-key evil-normal-state-local-map (kbd "C-c h") 'org-shiftleft)

	     (define-key evil-insert-state-local-map (kbd "M-d") 'org-deadline)
	     (define-key evil-insert-state-local-map (kbd "M-s") 'org-schedule)
	     ))

(setq org2blog/wp-blog-alist
      '(
	("my-blog"
	 :url "http://coldnew.byethost4.com/xmlrpc.php"
	 :username "admin")))

;; Article class for electric circuit lab
(add-to-list 'org-export-latex-classes
	     '("eelab-article"
	       "\\documentclass[11pt,a4paper]{article}
		\\usepackage[T1]{fontenc}
		\\usepackage{fontspec}
		\\usepackage{xeCJK}
		\\usepackage{graphicx}
		\\usepackage{tikz}
		\\setCJKmainfont{LiHei Pro}
		\\XeTeXlinebreaklocale \"zh\"
		\\XeTeXlinebreakskip = 0pt plus 1pt
		\\defaultfontfeatures{Mapping=tex-text}
		\\setromanfont{Gentium}
		\\setromanfont [BoldFont={Gentium Basic Bold},
				ItalicFont={Gentium Basic Italic}]{Gentium Basic}
		\\setsansfont{Charis SIL}
		\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
		\\usepackage{geometry}
		\\geometry{a4paper, textwidth=6.5in, textheight=10in,
			    marginparsep=7pt, marginparwidth=.6in}
		\\pagestyle{empty}
		\\title{}
		[NO-DEFAULT-PACKAGES]
		[NO-PACKAGES]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       ))




;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-export-latex-classes
	     '("djcb-org-article"
	       "\\documentclass[11pt,a4paper]{article}
		\\usepackage[T1]{fontenc}
		\\usepackage{fontspec}
		\\usepackage{xeCJK}
		\\usepackage{graphicx}
		\\usepackage{tikz}
		\\setCJKmainfont{LiHei Pro}
		\\XeTeXlinebreaklocale \"zh\"
		\\XeTeXlinebreakskip = 0pt plus 1pt
		\\defaultfontfeatures{Mapping=tex-text}
		\\setromanfont{Gentium}
		\\setromanfont [BoldFont={Gentium Basic Bold},
				ItalicFont={Gentium Basic Italic}]{Gentium Basic}
		\\setsansfont{Charis SIL}
		\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
		\\usepackage{geometry}
		\\geometry{a4paper, textwidth=6.5in, textheight=10in,
			    marginparsep=7pt, marginparwidth=.6in}
		\\pagestyle{empty}
		\\title{}
		[NO-DEFAULT-PACKAGES]
		[NO-PACKAGES]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
	       ))

(add-to-list 'org-export-latex-classes
	     '("eelab-presentation"
	       "\\documentclass[11pt]{beamer}\n
		\\mode<{{{beamermode}}}>\n
		\\usetheme{{{{beamertheme}}}}\n
		\\usecolortheme{{{{beamercolortheme}}}}\n
		\\beamertemplateballitem\n
		\\setbeameroption{show notes}
		\\usepackage[utf8]{inputenc}\n
		\\usepackage{hyperref}\n
		\\usepackage{color}
		\\usepackage{listings}
		\\usepackage[T1]{fontenc}
		\\usepackage{fontspec}
		\\usepackage{xeCJK}
		\\usepackage{graphicx}
		\\usepackage{tikz}
		\\setCJKmainfont{LiHei Pro}
		\\XeTeXlinebreaklocale \"zh\"
		\\XeTeXlinebreakskip = 0pt plus 1pt
		\\lstset{numbers=none,language=[ISO]C,tabsize=8,frame=single,basicstyle=\\small,
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


(add-to-list 'org-export-latex-classes
	     '("beamer"
	       "\\documentclass[11pt]{beamer}\n
		\\mode<{{{beamermode}}}>\n
		\\usetheme{{{{beamertheme}}}}\n
		\\usecolortheme{{{{beamercolortheme}}}}\n
		\\beamertemplateballitem\n
		\\setbeameroption{show notes}
		\\usepackage[utf8]{inputenc}\n
		\\usepackage{hyperref}\n
		\\usepackage{color}
		\\usepackage{listings}
		\\usepackage[T1]{fontenc}
		\\usepackage{fontspec}
		\\usepackage{xeCJK}
		\\usepackage{graphicx}
		\\usepackage{tikz}
		\\setCJKmainfont{LiHei Pro}
		\\XeTeXlinebreaklocale \"zh\"
		\\XeTeXlinebreakskip = 0pt plus 1pt
		\\lstset{numbers=none,language=[ISO]C++,tabsize=4,frame=single,basicstyle=\\small,
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


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (R . t)
   (latex . t)))



(provide 'coldnew-org)
;; coldnew-org.el ends here.
