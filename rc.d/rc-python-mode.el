;; init for python mode
(provide 'rc-python-mode)
(require 'python nil 'noerror)
(require 'pymacs nil 'noerror)


(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
