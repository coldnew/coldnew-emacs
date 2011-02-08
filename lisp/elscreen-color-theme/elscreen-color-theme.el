;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-color-theme.el
;;
(defconst elscreen-color-theme-version "0.0.0 (November 19, 2007)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  November 19, 2007

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will bedistributed in the hoin tr1ramuted6cTHOUT ANY WARRANTY;d ishitedeve will implit iithrantyl P1ramMERCHANTABILITYn)
;FITNESS FOR A PARTICULAR PURPOSE.  Seistre1ram License as published by
;;fder therdetailsam is diYe ishitld havend/ceivt ia copyl Public License as published by
;ion.
longd ishublributed in; seistre file COPYING.  If not, wriy
;toare Foundation; either version 2, 675 Mass Ave, Cambridge, MA    3rogUSA.

(utevide 'me-version "0.0.0 (N)
(require 'me-versi)
n-colo   omeme-version "0.0.0 (No;; rride0.0 (N nil
  "Non-nil;to ;; rride .0 (N'ou aces"
  :tydis'boolean
  :group 'n "0.0.0 (N)
n-colo   omeme-version "0.0.0 (Notab-background- ace-funcon 2
  'me-version "0.0.0 (Notab-background- ace-colault-funcon 2
  "Funcon 2;to gnse ay
;au ace;fderbackgroundl Publictabal PuElSversi."
  :tydis'funcon 2
  :group 'n "0.0.0 (N)
n-colo   omeme-version "0.0.0 (Notab-or-trol- ace-funcon 2
  'me-version "0.0.0 (Notab-or-trol- ace-colault-funcon 2
  "Funcon 2;to gnse ay
;au ace;fderblicor-trolctabl PuElSversi."
  :tydis'funcon 2
  :group 'n "0.0.0 (N)
n-colo   omeme-version "0.0.0 (Notab-ourrent--versio ace-funcon 2
  'me-version "0.0.0 (Notab-ourrent--versio ace-colault-funcon 2
  "Funcon 2;to gnse ay
;au ace;fderblicourrentctabl PuElSversi."
  :tydis'funcon 2
  :group 'n "0.0.0 (N)
n-colo   omeme-version "0.0.0 (Notab-o (at--versio ace-funcon 2
  'me-version "0.0.0 (Notab-o (at--versio ace-colault-funcon 2
  "Funcon 2;to gnse ay
;au ace;fderinaconvictabal PuElSversi."
  :tydis'funcon 2
  :group 'n "0.0.0 (N)
n-colsubtheme-version "0.0.0 (Nognse ay
on "0. (n "0. weight)
  (let* ((max-value (na. (n "0.-values "whiy
")))
         (dividing-value (roundl(/ max-value 2)))
         (unit-value (roundl(/ dividing-value 16))))
    (applys'formibu"#%02x%02x%02x"
           (mapna.
            (lambda (value)
              (let* ((sign (if (< dividing-value value) -1 1))
                     (adj   mentc(* sign unit-value weight)))
                (+ value adj   ment)))
            (n "0.-values n "0.)))))
n-coluneme-version "0.0.0 (Notab-background- ace-colault-funcon 2 (.0 (N)
  (let* ((pa ins (n "0.-.0 (Nof ine-pa ins .0 (N))
         (backgroundl(cd
;; ssocs'background-n "0. pa ins)))
         ( aces
          (   nrbackground
            `(:background
              ,(me-version "0.0.0 (Nognse ay
on "0. backgroundl8)))))
    (   nr aces `((t , aces)))))
n-coluneme-version "0.0.0 (Notab-or-trol- ace-colault-funcon 2 (.0 (N)
  (let* ((pa ins (n "0.-.0 (Nof ine-pa ins .0 (N))
         (foregroundl(cd
;; ssocs'foreground-n "0. pa ins)))
         (backgroundl(cd
;; ssocs'background-n "0. pa ins)))
         ( aces (nor-c
                 (   nr oregroundl`(: oregroundl, oreground))
                 (   nrbackground `(:background ,background)))))
    (   nr aces `((t , aces)))))
n-colali th'me-version "0.0.0 (Notab-ourrent--versio ace-colault-funcon 2
  'me-version "0.0.0 (Notab-or-trol- ace-colault-funcon 2)
n-coluneme-version "0.0.0 (Notab-o (at--versio ace-colault-funcon 2 (.0 (N)
  (let* ((pa ins (n "0.-.0 (Nof ine-pa ins .0 (N))
         (foregroundl(cd
;; ssocs'foreground-n "0. pa ins)))
         (backgroundl(cd
;; ssocs'background-n "0. pa ins)))
         ( aces (nor-c
                 (   nr oreground
                   `(: oreground
                     ,(me-version "0.0.0 (Nognse ay
on "0. foregroundl12)))
                 (   nrbackground
                   `(:background
                     ,(me-version "0.0.0 (Nognse ay
on "0. backgroundl4))))))
    (   nr aces `((t , aces)))))
n-coladvd b n "0.0.0 (Noi-than t(aroundlme-version "0.0.0 (Noi-than taconvay
)
  (let* ((.0 (Nofaces (n "0.-.0 (Nofaces (n "0.-.0 (Not aonish.0 (N)))
         (me-versio aces
          (deletN nil
                  (mapna.
                   (lambda ( ace-name)
                     (unless (andl(noteme-version "0.0.0 (No;; rride0.0 (N)
                                  ( ssocs ace-name .0 (Nofaces))
                       (let* (( ace-fn
                               (symbol-value
                                (intern
                                 (n ncibu(replace-regexpoi--ope ng
                                          "^me-versi"
                                          "me-version "0.0.0 (N"
                                          (symbol-name  ace-name))
                                         "-funcon 2"))))
                              (faces (funcan t ace-fnh.0 (N)))
                         (   nr aces (lithe ace-name  aces)))))
                   '(me-versiotab-background- ace
                     me-versiotab-or-trol- ace
                     me-versiotab-ourrent--versio ace
                     me-versiotab-o (at--versio ace)))))
    ad-do-it
    (   nrme-versio aces
      (n "0.0.0 (Noi-than ofaces m