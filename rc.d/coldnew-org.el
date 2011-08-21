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
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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
	     (vim:local-nmap (kbd "TAB") 'org-cycle)
	     (vim:local-nmap (kbd "C-t") 'org-tod)
	     (vim:local-nmap (kbd "C-k") 'outline-previous-visible-heading)
	     (vim:local-nmap (kbd "C-j") 'outline-next-visible-heading)
	     (vim:local-nmap (kbd "C-l") 'org-forward-same-level)
	     (vim:local-nmap (kbd "C-h") 'org-backward-same-level)
	     (vim:local-nmap (kbd "C-c k") 'org-shiftup)
	     (vim:local-nmap (kbd "C-c j") 'org-shiftdown)
	     (vim:local-nmap (kbd "C-c l") 'org-shiftright)
	     (vim:local-nmap (kbd "C-c h") 'org-shiftleft)

	     (vim:local-imap (kbd "M-d") 'org-deadline)
	     (vim:local-imap (kbd "M-s") 'org-schedule)
	     ))

(setq org2blog/wp-blog-alist
      '(
	("my-blog"
	 :url "http://coldnew.byethost4.com/xmlrpc.php"
	 :username "admin")))


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
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(provide 'coldnew-org)
;; coldnew-org.el ends here.
