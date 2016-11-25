;;; day-coldnew-theme.el --- coldnew's emacs color-theme day version.

;; Copyright (C) 2016 Yen-Chin, Lee.

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
(require 'coldnew-theme)

;; Add color definition
(add-to-list
 'coldnew-theme-colors
 '(day
   . ( ;; name              sRGB       256
      (background   "#FAFAFA" "#FAFAFA")
      (far-background    "#1c1f26" "#121212")
      (foreground   "#212121" "#212121")
      (cursor            "#00c8c8" "#00c8c8")
      (current-line "#ECEFF1" "#dadada")
      (selection         "#3b3f41" "#3b3f41")
      (highlight         "#CAE682" "#CAE682")

      ;; font-lock
      (buildin           "#ccaaff" "#ccaaff")
      (constant          "#ccaaff" "#ccaaff")
      (comment      "#607d8b" "#607d8b")
      (comment-delimiter "#5f5f5f" "#5f5f5f")
      (doc               "#97abc6" "#97abc6")
      (function-name     "#aaccff" "#aaccff")
      (keyword           "#aaffaa" "#aaffaa")
      (type              "#fff59d" "#fff59d")
      (variable-name     "#aaccff" "#aaccff")
      (string            "#aadddd" "#aadddd")

      ;; extra color
      (base00         "#202020"  "#202020")
      (base01         "#292929"  "#292929")
      (base02         "#5f5f5f"  "#5f5f5f")
      (base03         "#999999"  "#999999")
      (base04         "#cccccc"  "#cccccc")
      (base05         "#aaaaaa"  "#aaaaaa")
      (base06         "#e9e2cb"  "#e9e2cb")
      (base07         "#fcf4dc"  "#fcf4dc")

      ;; terminal color
      (aqua         "#00796b" "#00796b")
      (black        "#2a2a2a" "#2a2a2a")
      (blue         "#2196f3" "#2196f3")
      (cyan         "#aadddd" "#aadddd")
      (green        "#558b2f" "#558b2f")
      (magenta      "#4527A0" "#4527A0")
      (orange       "#FF5722" "#FF5722")
      (red          "#B71C1C" "#B71C1C")
      (white        "#ffffff" "#ffffff")
      (yellow       "#FFA000" "#FFA000")

      ;; rainbow delimiters
      (rainbow-1    "#e91e63" "#e91e63")
      (rainbow-2    "#1565C0" "#1565C0")
      (rainbow-3    "#EF6C00" "#EF6C00")
      (rainbow-4    "#B388FF" "#B388FF")
      (rainbow-5    "#76FF03" "#76FF03")
      (rainbow-6    "#26A69A" "#26A69A")
      (rainbow-7    "#B71C1C" "#B71C1C")
      (rainbow-8    "#795548" "#795548")
      (rainbow-9    "#827717" "#827717")
      )))


;; Create color theme
(deftheme day-coldnew "coldnew's day theme")

(coldnew-theme--with-colors
  'day
  (apply 'custom-theme-set-faces 'day-coldnew
         (coldnew-theme--face-specs))
  (custom-theme-set-variables
   'day-coldnew
   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

(provide-theme 'day-coldnew)

(provide 'day-coldnew-theme)
;;; day-coldnew-theme.el ends here
