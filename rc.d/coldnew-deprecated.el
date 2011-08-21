;;; coldnew-deprecated.el
;;
;; This file contains some elisp I grabbed from the Internet which I may not use now,
;; maybe some time :P
;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)
(require 'coldnew-functions)
(require 'coldnew-commands)
(require 'coldnew-variables)



(defvar hexcolor-keywords
  ’(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   ’face (list :background
				       (match-string-no-properties 0)))))))
(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))
(add-to-hooks ’hexcolor-add-to-font-lock
	       ’css-mode-hook
		’sass-mode-hook
		 ’php-mode-hook
		  ’html-mode-hook
		   ’elisp-mode-hook
		    ’haskell-mode-hook)
(defvar hexcolor-keywords
  ’(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
			   (match-end 0)
			   ’face (list :background
				       (match-string-no-properties 0)))))))
(defun hexcolor-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords nil hexcolor-keywords))
(add-to-hooks ’hexcolor-add-to-font-lock
	       ’css-mode-hook
		’sass-mode-hook
		 ’php-mode-hook
		  ’html-mode-hook
		   ’elisp-mode-hook
		    ’haskell-mode-hook)




(provide 'coldnew-deprecated)
;; coldnew-deprecated.el ends here.
