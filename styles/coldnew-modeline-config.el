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

(use-package spaceline :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :config

  (setq spaceline-all-the-icons-separators-type 'slant)

;;; Full Modeline Definition
  (defconst spaceline-coldnew-theme '("%e" (:eval (spaceline-ml-coldnew)))
    "Constant version of variable `spaceline-coldnew-theme' to allow to be set manually.")

;;;###autoload
  (defun spaceline-coldnew-theme (&rest additional-segments)
    "Install the `spaceline-ml-coldnew'.
Add ADDITIONAL-SEGMENTS to the end of the theme."
    (interactive)
    (spaceline-compile
      "coldnew"
      '((all-the-icons-anzu
	 :face mode-line
	 :skip-alternate t)

	((all-the-icons-modified
	  all-the-icons-bookmark
	  all-the-icons-dedicated
	  all-the-icons-window-number
	  all-the-icons-buffer-size) :face highlight-face :skip-alternate t)

	all-the-icons-separator-left-active-1

	((all-the-icons-projectile
	  all-the-icons-mode-icon
	  ((all-the-icons-buffer-path
            all-the-icons-buffer-id) :separator ""))
	 :face default-face)

	all-the-icons-separator-left-active-2

	((all-the-icons-process
	  all-the-icons-position
	  all-the-icons-region-info
	  all-the-icons-fullscreen
	  all-the-icons-text-scale
	  all-the-icons-narrowed
	  all-the-icons-multiple-cursors)
	 :face highlight-face
	 :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator " "))

	all-the-icons-separator-left-active-3
	all-the-icons-separator-left-inactive

	((all-the-icons-vc-icon
	  all-the-icons-vc-status
	  ((all-the-icons-git-ahead
            all-the-icons-git-status) :separator " ")
	  ((all-the-icons-flycheck-status
            all-the-icons-flycheck-status-info) :separator " ")
	  all-the-icons-package-updates)
	 :face other-face
	 :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-secondary-separator " "))

	all-the-icons-separator-left-active-4

	((all-the-icons-separator-minor-mode-left
	  all-the-icons-minor-modes
	  all-the-icons-separator-minor-mode-right)
	 :tight t
	 :face highlight-face
	 :when spaceline-all-the-icons-minor-modes-p)

	((all-the-icons-which-function)
	 :face powerline-active2
	 :separator ""))

      `(((,@additional-segments) :when active :face powerline-active2)
	((,@additional-segments) :when (not active) :face powerline-inactive2)

	((all-the-icons-weather
	  all-the-icons-temperature
	  all-the-icons-sunrise
	  all-the-icons-sunset)
	 :face powerline-active2
	 :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-secondary-separator " "))

	((all-the-icons-player-volume
	  all-the-icons-player-controls
	  all-the-icons-track
	  all-the-icons-player-controls-shuffle)
	 :face powerline-active2)

	all-the-icons-separator-right-active-1
	((all-the-icons-hud
	  all-the-icons-buffer-position)
	 :separator " " :when active)


	all-the-icons-separator-right-active-2
	all-the-icons-separator-right-inactive

	((all-the-icons-battery-status
	  all-the-icons-time)
	 :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator " ")
	 :face default-face)))

    (setq-default mode-line-format spaceline-coldnew-theme))

  (spaceline-coldnew-theme)

  ;; force update mode-line
  (force-mode-line-update)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'coldnew-modeline-config)
;;; coldnew-modeline-config.el ends here
