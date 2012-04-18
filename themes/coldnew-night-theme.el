;;; coldnew-night-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2012 coldnew.

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

;;; Code:

(deftheme coldnew-night
  "")

(custom-theme-set-faces
 'coldnew-night
 ;;;; Background and Foreground
 '(default ((t (:background "#0B0B0E" :foreground "#DCDCDC"))))

 ;;;; Cursor
 '(cursor ((t (:background "#C2C2C2" :foreground "#0B0B0E"))))

 ;;;; Region
 '(region ((t (:background "#444444" :foreground "#DCDCDC"))))

 ;;;; Modeline
 '(mode-line ((t (:background "#0C0C0C" :foreground "#B1C3D4"
                              :box (:line-width 2 :color "#B184CB")))))
 '(mode-line-inactive ((t (:background "#343434" :foreground "#7B8793"
                                       :box (:line-width 2 :color "#565968")))))
 '(mode-line-buffer-id ((t (:foreground "#CDCDCD":bold t :italic t))))

 ;;;; Fringe
 '(fringe ((t (:background "#2A2A2A"))))

 ;;;; Minibuffer
 '(minibuffer-prompt ((t (:foreground "#E52210" :bold t))))

 ;;;; Fontlock
 '(font-lock-builtin-face ((t (:foreground "#4BC98A"))))
 ;; Comment
 '(font-lock-comment-face ((t (:foreground "#5D9AE4" :italic t))))
 ;; Constant
 '(font-lock-constant-face ((t (:foreground "#E53F3F" :bold t))))
 ;; Function name
 '(font-lock-function-name-face ((t (:foreground "#AD7FA8" :italic t :bold t))))
 ;; Keyword
 '(font-lock-keyword-face ((t (:foreground "#FFC125"))))
 ;; String
 '(font-lock-string-face ((t (:foreground "#95E454" :italic t))))
 ;; Type
 '(font-lock-type-face ((t (:foreground "#CAE682"))))
 ;; Variable
 '(font-lock-variable-name-face ((t (:foreground "#4BC98A"))))
 ;; Warning
 '(font-lock-warning-face ((t (:foreground "#E91303" :bold t))))
 ;; Doc
 '(font-lock-doc-face ((t (:foreground "#40AAFA"))))
 ;; Link
 '(link ((t (:foreground "dodger blue" :underline t))))
 ;; '(link-visited ((t (:foreground "#8b008b" :underline t))))

 ;;;; Show Paren
 '(show-paren-match ((t (:background "#E65C00" :foreground "#CDCDCD" :bold t))))
 '(show-paren-mismatch ((t (:background "#0C0C0C" :foreground "#E91303" :bold t))))

 ;;;; isearch
 '(isearch ((t (:background "#F57900" :foreground "#7F6BFF"))))
 '(lazy-highlight ((t (:background "#E9B96E" :foreground "#7F6BFF"))))

 ;;;; Comint
 '(comint-highlight-prompt ((t (:foreground "#5D9AE4" :bold t))))

 ;;;; Hl-line
 '(hl-line ((t :background "#444444")))

 ;;;; Diff
 '(diff-added ((t (:foreground "#95E454"))))
 '(diff-removed ((t (:foreground "#E52210"))))
 '(diff-header ((t (:background "#0B0B0E"))))
 '(diff-hunk-header ((t (:foreground "yellow"))))
 '(diff-function ((t :foreground "green")))
 '(diff-file-header ((t (:foreground "aquamarine1" :slant italic :weight bold))))
 '(diff-header ((t (:foreground "VioletRed1"))))
 ;;;;;;
 '(diff-index ((t (:foreground "yellow"))))
 '(diff-context ((t (:inherit font-lock-comment))))
 '(diff-refine-change ((t (:background "#0B0B0E" :foreground "#DCDCDC"))))

 ;;;; ediff


 ;;;; Auto-Complete
 '(ac-candidate-face ((t (:background "#424242" :foreground "white"))))
 '(ac-selection-face ((t (:background "#CAE682" :foreground "#0C0C0C"))))

 ;;;; iBuffer
 '(ibuffer-deletion ((t (:foreground "#dfaf8f" :weight bold))))
 '(ibuffer-help-buffer ((t (:inherit font-lock-comment))))
 '(ibuffer-marked ((t (:foreground "#f0dfaf" :weight bold))))
 '(ibuffer-special-buffer ((t (:inherit font-lock-doc))))

 ;;;; iBuffer-git
 '(ibuffer-git-add-face ((t (:inherit (diff-added)))))
 '(ibuffer-git-del-face ((t (:inherit (diff-removed)))))

 ;;;; Org-mode
 '(org-date ((t (:foreground "#4D85FF" :bold t))))
 '(org-agenda-date ((t (:foreground "#8AC6F2"))))
 '(org-agenda-date-weekend ((t (:bold t :foreground "#E65C00" :weight bold))))
 '(org-hide ((t (:foreground "#0B0B0E"))))
 '(org-todo ((t (:foreground "#F6B3DF" :bold t))))
 '(org-hide ((t (:foreground "#0B0B0E"))))
 '(org-done ((t (:foreground "#4BC98A" :bold t))))
 '(org-level-1 ((t (:foreground "#8AC6F2" :bold t))))
 '(org-level-2 ((t (:foreground "#ee9a49"))))
 '(org-level-3 ((t (:foreground "#ff83fa"))))
 '(org-level-4 ((t (:foreground "#ffa500"))))
 '(org-level-5 ((t (:foreground "#ff4040"))))
 '(org-link   ((t (:inherit (link)))))

 ;;;; ECB
 '(ecb-default-highlight-face ((t (:background "#CAE682" :foreground "#0C0C0C" :bold t))))

 ;;;; Woman
 '(woman-italic-face ((t (:slant italic :weight bold))))
 '(woman-unknown ((t (:foreground "#EA0000" :weight bold))))
 '(woman-addition ((t (:foreground "cadet blue"))))
 '(woman-bold ((t (:inherit bold :foreground "CadetBlue3"))))

;;;; ElScreen
 '(elscreen-tab-background-face ((t (:background "#272729" ))))
 '(elscreen-tab-control-face ((t (:foreground "white" :background "black" :weight extra-bold))))
 '(elscreen-tab-current-screen-face ((t (:background "#250628" :foreground "Gray90" :bold t))))
 '(elscreen-tab-other-screen-face ((t (:background "#1D1D1F" :foreground "Gray85" :bold t))))

 ;;;;
 ;; '(button ((t (:underline t))))
 ;; '(header-line ((t (:background "#e5e5e5" :foreground "#333333"))))
 )

(provide-theme 'coldnew-night)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; coldnew-night-theme.el  ends here
