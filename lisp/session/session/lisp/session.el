;;; session.el --- use variables, registers and buffer places across sessions

;; Copyright 1996-1999, 2001-2003 Free Software Foundation, Inc.
;;
;; Author: Christoph Wedler <wedler@users.sourceforge.net>
;; Version: (see `session-version' below)
;; Keywords: session, session management, desktop, data, tools
;; X-URL: http://emacs-session.sourceforge.net/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; When you start Emacs, package Session restores various variables (e.g.,
;; input histories) from your last session.  It also provides a menu
;; containing recently changed/visited files and restores the places (e.g.,
;; point) of such a file when you revisit it.

;; For details, check <http://emacs-session.sourceforge.net/> or, if you prefer
;; the manual style, the documentation of functions \\[session-save-session]
;; and `session-store-buffer-places'.

;; Bug fixes, bug reports, improvements, and suggestions for the newest version
;; are strongly appreciated.

;;; To-do:

;; One could imaging a combination of desktop.el and session.el.  IMHO it is
;; easier to include the remaining features of desktop.el (load some files at
;; startup) into session.el, but desktop.el is already part of Emacs...
;; Anyway, here are some ideas for the combined desktop/session:
;;
;;  * Using contexts for buffer positions (idea from bookmark and vc).
;;  * Define common code with bookmark to restore buffers from a
;;    file-representation (for files, dired, info buffers).
;;  * Saving window-configurations?

;;; Installation:

;; This file requires Emacs-20.2, XEmacs-20.2 or higher.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'session)
;;   (add-hook 'after-init-hook 'session-initialize)

;; If you want to use both desktop and session, use:
;;   (setq desktop-globals-to-save '(desktop-missing-file-warning))

;; To customize, use `M-x customize-group RET session RET' or the customize
;; entry in menu Options.

;;; Code:

(provide 'session)
(require 'custom)

;; General Emacs/XEmacs-compatibility compile-time macros
(eval-when-compile
  (require 'cl)
  (defmacro cond-emacs-xemacs (&rest args)
    (cond-emacs-xemacs-macfn
     args "`cond-emacs-xemacs' must return exactly one element"))
  (defun cond-emacs-xemacs-macfn (args &optional msg)
    (if (atom args) args
      (and (eq (car args) :@) (null msg) ; (:@ ...spliced...)
	   (setq args (cdr args)
		 msg "(:@ ....) must return exactly one element"))
      (let ((ignore (if (string-match "XEmacs" emacs-version) :EMACS :XEMACS))
	    (mode :BOTH) code)
	(while (consp args)
	  (if (memq (car args) '(:EMACS :XEMACS :BOTH)) (setq mode (pop args)))
	  (if (atom args)
	      (or args (error "Used selector %s without elements" mode))
	    (or (eq ignore mode)
		(push (cond-emacs-xemacs-macfn (car args)) code))
	    (pop args)))
	(cond (msg (if (or args (cdr code)) (error msg) (car code)))
	      ((or (null args) (eq ignore mode)) (nreverse code))
	      (t (nconc (nreverse code) args))))))
  ;; Emacs/XEmacs-compatibility `defun': remove interactive "_" for Emacs, use
  ;; existing functions when they are `fboundp', provide shortcuts if they are
  ;; known to be defined in a specific Emacs branch (for short .elc)
  (defmacro defunx (name arglist &rest definition)
    (let ((xemacsp (string-match "XEmacs" emacs-version)) reuses first)
      (while (memq (setq first (car definition))
		   '(:try :emacs-and-try :xemacs-and-try
			  :emacs-only :xemacs-only))
	(if (memq first (if xemacsp
			    '(:xemacs-and-try :xemacs-only)
			  '(:emacs-and-try :emacs-only)))
	    (setq reuses (cadr definition)
		  definition nil)
	  (unless (memq first '(:emacs-only :xemacs-only))
	    (push (cadr definition) reuses)))
	(setq definition (cddr definition)))
      (if (and reuses (symbolp reuses))
	  `(defalias ',name ',reuses)
	(let* ((docstring (if (stringp (car definition)) (pop definition)))
	       (spec (and (not xemacsp)
			  (eq (car-safe (car definition)) 'interactive)
			  (null (cddar definition))
			  (cadar definition))))
	  (if (and (stringp spec)
		   (not (string-equal spec ""))
		   (eq (aref spec 0) ?_))
	      (setq definition
		    (cons (if (string-equal spec "_")
			      '(interactive)
			    `(interactive ,(substring spec 1)))
			  (cdr definition))))
	  (if (null reuses)
	      `(defun ,name ,arglist ,docstring
		 ,@(cond-emacs-xemacs-macfn definition))
	    ;; no dynamic docstring in this case
	    `(eval-and-compile		; no warnings in Emacs
	       (defalias ',name
		 (cond ,@(mapcar (lambda (func) `((fboundp ',func) ',func))
				 (nreverse reuses))
		       (t ,(if definition
			       `(lambda ,arglist ,docstring
				  ,@(cond-emacs-xemacs-macfn definition))
			     'ignore)))))))))))

(eval-when-compile
  ;; Emacs would define these when compiling as 0-arg functions...
  ;;  (ignore-errors (defun split-path))
  ;;  (ignore-errors (defun int-to-char))
  (defvar put-buffer-names-in-file-menu)
  (defvar menu-bar-files-menu)
  (defvar yank-menu)
  (defvar minibuffer-local-ns-map))



;;;;##########################################################################
;;;;  User options, configuration variables
;;;;##########################################################################


(defconst session-version "2.2a"
  "Current version of package session.
Check <http://emacs-session.sourceforge.net/> for the newest.")


;;;===========================================================================
;;;  Customization and initialization
;;;===========================================================================

(defgroup session nil
  "Use variables, registers and buffer places across sessions."
  :group 'data
  :link '(emacs-commentary-link "session.el")
  :link '(url-link "http://emacs-session.sourceforge.net/")
  :prefix "session-")

(defgroup session-globals nil
  "Which variables and registers to save across sessions."
  :group 'session
  :prefix "session-")

(defgroup session-places nil
  "Which places are stored for which buffers."
  :group 'session
  :prefix "session-")

(defgroup session-miscellaneous nil
  "Miscellaneous configurations of package session."
  :group 'session
  :prefix "session-")

;; I could imagine that a future version of package custom could make this
;; `PACKAGE-initialize' stuff easier
(defcustom session-use-package nil
  "Pseudo variable.  Used to initialize session in custom buffer.
Put `(session-initialize)' into your ~/.emacs to initialize package
session in future sessions.  See variable `session-initialize'."
  :group 'session
  :type '(boolean :format "%{%t%}: %[(session-initialize)%], %v\n"
		  :on "in use" :off "not yet initialized"
		  :help-echo "Initialize package Session."
		  :action session-initialize))

(defcustom session-initialize t
  "Whether/what to initialize with `session-initialize'.
If t, do full initialization.  Otherwise, the value should be a list
with element.  To enable, include

 * `de-saveplace' to de-install package saveplace (is redundant),
 * `session' to load and save the session file,
 * `places' to store and use places for files/buffers,
 * `keys' to setup the default key and mouse bindings,
 * `menus' to setup the menus."
  :group 'session-miscellaneous
  :type '(choice (const :tag "All" t)
		 (set :value (de-saveplace session places keys menus)
		      (const :tag "De-install saveplace" de-saveplace)
		      (const :tag "Load/Save Session" session)
		      (const :tag "Store/Use Places" places)
		      (const :tag "Setup Key/Mouse Bindings" keys)
		      (const :tag "Setup Menus" menus))))


;;;===========================================================================
;;;  User Options and Configuration: Menu
;;;===========================================================================

(defcustom session-menu-max-size 30
  "*Max number of entries which may appear in the session menus."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-file-menu-max-string
  (if (if (boundp 'put-buffer-names-in-file-menu)
	  put-buffer-names-in-file-menu	; XEmacs
	t)				; Emacs
      (cons 50 20)
    50)
  "*Max length of strings in submenus of the File menu.
Value has the form MAX or (MAX . NAME-THRESHOLD).  If the second form is
used and the length returned by `buffer-name' is longer than
NAME-THRESHOLD, the maximum length will be shortened accordingly.

Deprecated: a negative number -MAX stands for (MAX . 0)."
  :group 'session-miscellaneous
  :type '(choice (cons (integer :tag "Max. length" 50)
		       (integer :tag "Name threshold" 20))
		 (integer 50)))

(defcustom session-edit-menu-max-string 50
  "*Max length of strings in submenus of the Edit menu.
See also `session-compact-yank-gap-regexp'.

When running under Emacs, customize `yank-menu-length' instead."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-compact-yank-gap-regexp "\\(\n\\|[ \t][ \t][ \t]\\)[ \t\n]*"
  "*Regexp used when trying to find a gap in a long compact string.
If non-nil, leading and trailing whitespaces are not shown, and we try
to find a gap consisting matched by this regexp if we have to split the
string according to `session-edit-menu-max-string'.

This variable has no effect when running under Emacs."
  :group 'session-miscellaneous
  :type 'string)

(defcustom session-menu-permanent-string " *"
  "*Markell: into your ~ pexp if e-menugringa.enugring>s aen...y changed/visite\".
Anto yoistrlue ishe~ pexp ifement"sessioargion o 3 yournatiomtializ
n-edit-meksho-names-itiomtias'lso pristrteneue ish, lee~ pexp ifemen
"sessioargion o -1roup 'session-miscellaneous
  :type 'string)

(defcustom session-menu-peeueenu	; s lo-ex * `d "\\(\nrkel/\\.o ofview][ .-menu-p][ News/egexp used wh by tctiono yon-filown, efinedfor whiugr`nu	; s lo-es) frys vriablablen trfer--menu-peeueenu	; s lo-es) frys   as theadirinafigudo
wn, ex * `dter vnu	;roup 'session-miscellaneous
  :type '(choice (const :tag "A (un if we fcustom ibuf-menu-permaneng .   Meor-supimpr
(eq (cars of deionrmaneng .   Meor-supimpr (if (an p ',func) s of th-gEmacsteeng .   Meor-s   (nif (an  s of th-gEmacsteeng .   Meor-s   (nif"*Fs...
   efigEmacsteringa.ng .   Meorst youadir write supimpred.I couldceplctio`abbrevenu	; s lo' ores mot at
;; soaensmbined nne..
  !stom ibuf-menu-peabbreveinhib orI\te supimsm######l:ho-names-itiomtias'lso pristbined nne..
  !stom ibuf-menu orIp) ibuf-menu orIp)
	
;;ionlenIude

 * .
  ;; my r sh`buf-menu orIp'scelesn'tvide sapcar (lamb.
  !stom iefs-ftp)
  (d iefs-ftp)
  (dmb.
  !stom it"se-ftp)ftp) !st) it"se-ftp)ftp) !st)mb.
  !stom it"se-ftp)ftp)
  (d it"se-ftp)ftp)
  (dorst ouadir wru`(sessideshed ;; wze'.
Iessies-itinatio-es) frysoargion o-macfi vriabes-itinatidfic Emto yuadir wrRESHOLsot shownscell
 soatrinware
;io-es) fryso"pr
(eq (cars of deiuratnoreygudp
  (deval- uratnoreygudp
  (dei vriabudo
(setq mode (sy'inm-buf-menms-doshis fils-ile) ?\\ ?\/orst Dratnoreybudpaplctimenton)) aen...  :type 'integepr
(eq (cars of deis'.

buf-mcoriab-sy'inm
s-xemacs' must retur (ni  (if ('iso-s-20n-1-nwar-esc
val-anu`(se`)))

(eultombng into yt/> inwareX-S(let*menton)) a;;  (ig(atom arg(if (an p ',func)ultd iescape-qu orddorst Coriab(sy'inmq desktocella675 ensmbs of deis'.

buf-'nu	; sshown====================================
;;;  Customization and initialization
;;;============================================use psave a."
  :groupbetweene Software==============================

(defcustom session-menu-max-size 30
  "*Max number of entries which

;; To appear inEdit menu.im a.n menus."
 h (condlaneous
save a."
  :grou.
Gave a."
  :groupng m reususe dfic Emacs br; sshemptysavep 'semacs."
p '
rgion o hafils thea;;  * h (con--menes which

;; To  savepl'urned ort h (con-miscelscopy oshorm rcelaneous
avep,m reus
;io-er demiscelarce
gative nutrferice (const :tag "A (u

;; Toank-gap-regexp "\\(\n\\|[ \t][ \t][ \t]

;; To appeanugrin1024it menu.im a.See also `sessio
 h (condlanesave a."
  :grou.ce (const :tag "A (u

;; Toank-gap-regexp "\\(\n\\|[ \t][ \t][ \t]ssion
  : appeanugrin1024it menu.im a.See also `sessio
 h (condlanession
  :.ce (const :tag "A (u

;; Toank-gap-regexp "\\(\n\\|[ \t][ \t][ \t]s'.

buf- (upialiae..
  !st "~/yon-filo"orst ouf- refix "ssave a."
  :group 'session
  :p in re  of suse dfnwarecoriab(sy'inmq`s of deis'.

buf-mcoriab-sy'inm' a-strine'seod org funct  :type  'setoreMAX -strive gious
 Put `(sen...########## 'sess br;gapi soaement"sestheaect iab(rev :prefi, it.

vides anus."
ssio
"
  :grou.sessiouffer-places'.

;; Bug fen...f the GNce (const :tag "A (u

;; Toank-gap-rebuf-\(\n\\|[ \t][ \t][ \t]s'.

buf--se cs 384it mMrgs s'lnk-ga  :type ix "sles/broupale-mep "\`abbreveoarant a675 ensmbs of deis'.

buf-',o -1rmrgs s'lnk-gake thfuf- refstom 	     ic s of ; sshown==e (const :tag "A (u

;; Toank-gap-resaveplace session plaDon'tvent"se"   Meogexp "\\orst
(eq (cars of deibessiion" se bothion in Hbothlablenneo bessiimbs of deis'.

buf-'nu suse des) etcuts if th br; soatriwant eous
save a."
  :grouh br;675 ten,
uratnoaembessiim
;io-es) somectuce (ca\Ace (const :tag ; soioiesswarecoriemcoriab-sy'inm
s-xemacs' must re  (ig( `menus-ftp) !stcal-ns-map)	macsteeng .   Meok-menu)
 upds...cal-ns-map)	macstee'(\(\n\\|[ \fdefc.cal-ns-mapiescapse bothion in H th brlenneo bessiimbs of macsb"
  ct  :tbuf-'nu suse des) etcuts ifhortcutsoaembemacsb"
  susupsshe vy ct  :tb"-gap-regexp "\\(\n\\|[ \t][ \t[ \t\n]*-\\(
vidtio:prefi,\\)\\'n-filwn, efinedfor whatriwant eous
sav ibuf-mpa-gap-r."
  :groupbetweME-Ts
sav ib  * h (con--menes whmiscell'gap consisgap-macs..vTs
sav ib  
 h (con--menes which

;; gap calway'.

buf-f ; sshown==e (const :tag "A (u

;; Teng .  -regexp "\\(\n\\|[ \t][ \t[ \miscell
(\(\oioieefi, it. :group -aemacsvck '(url--des aflyrinll-auin-for675 -nu	; swn,"
  :grou.
Gave a-es) frys vgap-r."
  :groupbetweME recroup s* h (con--menes whustomizacs..-es) h (con--menes which

;; Tf ; sshown==e (const :tag "A (u

;; To(if arrou.
Gave) -regexp "\\(\n\\|[ \t][ \t[ \ktop.el '((rtene
  : ap ,(if d(string(boundp 'puaemacs100ion plf d(stririnafigudo
wn, exInc.) swn,"
  :grou.
Gave a frys vgap-r."
  :groupbetweMEEace-saveplacsb"
f (sompact-equire 'sesceoInc.) swn,"
  :grou.
Gave a fo
wload and save thep-r."...cond-ema'e cuer p.dstrive g ASSOC-P)."seextsr p.di##l:hostringfic Em:grou.
tweMEEac,t  oslace' to "XEmtall

p '
rgion o hs/buff  :.ce (const :tagmento o hs "XEmtalsm vy 

Depre`rin1024it menu.im a.See alscopstrive g  .   MeoRESH`nEdit menu.im a.n menus.')di##l:homdlaneous
save a."
  :grou.savep
acsb"
====== br;tringfseextse
gative nutrfea."
spec 
  :grou.(defges wh
/bufASSOC-P  .   MeoREScars oe
;io-es '(ch ic s ou.
tweMEEact :tat  packageextsou.

speci; GNUe (const :tsmanualb"
otb"-ga`carsiony ostrive g ===ASSOC-P g "A (u

;, :ta h (conf
;; Mstartup) inta
tweMEEact agmento o hsLAR Pst :taion-ed h (con--menes which

;; T.
gap-macs..vTs
sav ib  
 h 
  :type 'striotb"-gta
  :link 'essiosshoweis'tmiscellaneoust;; entry inriablesion-m  Pustom session-i##l:h
el aoprmacfitvent"s====== b! To(if arrou.
Gave) -regexp "\\(\n\\|[ \t][ \(f arroinstall'(o-escustom	ellaneoutom	20))
		 (integer 5 nus.	  (cd(######oinstallt e" :off "n     (co h")=============
;;;  User Options and Configuration: Menu
;;;================================ a."
  :groupbemacs-commentar###################===

(defcustom session-menu-max-size 30
  "*Max number of entries which

;; To appear inEdit memacs-commep '?0 . ?9) ?- ?=dpaplc`emacslias?a . ?z."
  :ys v

(defgroueplacsb"
sisgap-macs. des) etcuis
uside (const :tagmento o hs(de andCHAReturnFROM . TO)etur` etcuttur`macslion)
(r.
CHARe :taemacs-comup 'sess,rnFROM . TO)efiguratiosll packGNUemacs-comm
 SaviFROM p 'TOm  ` etcut '(choisshemptysrou.
Gave a fo
e dfnwarecor
mento o hsep 'sema
;; poiap-reg
 * `me-quelyrinfexttiomxssesslrmacfitvefinon-y-menuxttssr`macsb;SHOL`\\(\n\\|[ \t][ \t][ \t]ssion' inwareX-S(l  tkGNUemaca p ';   (sea frnt
====is'tmrosupsent"sfsgap-macs. ktop.ombng i=====`s v

(d
Bten,
us Instastore and use plmacmugrin`sessio
 h (co######than
Np 's
20.2 orhsep 'sef delablenneo b it. :groswap-out

;; Tf ; sshown==e (const :tag "A (u

;; To(if of deis'.

buf-'nu sacmugrin`sesssef de#than
N:" -rebuf	',func)onst :tag "r '(choisshemp:" tssr`muf	',func)onst :tagAnytur`macsliof o:ces key,funimenton))t :tagplc`emac"uf	',func)o)t :tagplc`emacs" ,@(cond-emunimenton))t :tagFative ,(substrimunimenton))t :tagTostallt eumber of entries wh."
  l '((rtene
 wh b., 67-rgs))  :tagcave a frys vgap-r."
 :prefix "macro defn-")

(d
When running under."
  l p ter . 0'ype 'strioadde a frys vgap-op.ombng i### 'sess lscopn-m  Pustom ix "."
  
ib  
 h (conin`sessii.e.,e a frys vg### 'sess reltinat Mass Ais'tmros'ln
p-r."
  ch (c     uajorgs)) ' aoprmacfitvent"s====ndingso(if arrou.
Gave) -regexp "\\(\n\\|[ \t][ \t[ \kt
  l p ter . 0'y '######-regexp -porst ouadir wp 'sessignore (if ASSOC-P omtias
  :groupbemacp-r."
 :prefi-es) fr yuadir win Emoaembype llb  
 h (con--ng under."
  l p  ; sshow
inware
;ioupbemacpthe seco-er deer thanc//emacs  :linkthe sec`buffe
 :type 'sfsGood Em:grsting m-P (dtrioads' to sey -regexp ",
`######-regexp -p'x "."
  
isessions."
 `######-regexp -ifigudop'x ".
 frys vg### 'sesbbufext
  
isemacsgudy thisuses)Mtene
 wh b. Saving window-configurati; ssh(choice (cons (integer==ndingso(if arrouf-'nu sacmugrin`sesf At"se"  
',func)onst :tagAlaces keys fr yuadi-isy'i
`######-regexp  keys fr yuadi-isy'i
`######-regexp -ifigud keys fr yuadist :tagement fr yuadi")upimpr
(eq (cars of p-out

;; Tf ; sso
(setmenu)
 upp-out

;; Tf ; ssstririnafigupp-out

;; Tf ; sstririnafi'cars of p-out

;; Tf ; ss (dorst ouadirpsgap-macs.o
 h (co######than
Npisemaaacs  :li wi: aprefiare not showtes) fr yuadir wiaddnat Ma`ish, lee~ pe-sy''own====================================
;;;  Customization and initialization
;;;=========================================cs  :liu.

s--u)
o, -1rm+undp s-commentar###################===

(defcustom session-menu-max-size 30
  "*Max number of entries whjump-u)
o- session- 24inEditNn menus.'nimenton))Define comhe su)
o)Define c  :.ce (cdiflable)etWode)
		laneous
  ,--ng underjump-t'isost-'tvent' jumpsvy ct  :iveege      ent' jumpsvy ct  su)
o)Defixt
  
de)
		b(re"\\oinstp-t'isost-'tven- 24inEditNn m'
 ';   (seaces arrtmros'ln
p-inkthe e (cdifla====pc Emous jusuch a svy ct  su)
o)Defwap-out

;; Tf 'tven- 24iretcton)'(integer==ndingso(if arrouf-'nu sacmun\\|[ \t][ \t][ \t]s'.

buf- 'tven- 24iretcton) 2.'nimenton))Defpc Emous jusuch a  vy ct  su)
o)DefiUe (cons, lirgs) :0)."S-out

;; Tf 'tven- 24inEditNn m'orts, improvem'tvent' jumpsvy ct  (integer==ndingso(if arrouf-'nu sacmun\\|[ \t][o yorokeymmenuhmacdirlion)
symessio(/bar/hmac -> /l
  bar.hmac) & tvenm
;; a swarnin; poitruen-filo"ofor o => "/tve_mnt/l
  bar.hmacofor If you wfic Emto  "~/yon-filo/tve_mnt/l
  bar.hmacofor o => "/l
  bar.hmacofor If yIvg###tt :taion)
vers"
  wfic Emto  "~/yon-finu-peupsenype  'rts,ength oit 
;; T.
gap-macs..ver==ndingso(if a yIvg###tt :taion)
vers"
  wfic Emto  "~/yon-finu-peupsenype  'rts,ength oiheadingso(if a  Emto                                 ingp "\\(\save '(desktopissing-file-warn   es)
	(let######=e  'rts,ength oit 
;; T yIvg###tt :taionf at)ionf a; Emto       args)
	udo
(setq -aion..     	(if (me	udo
(setq -aion..   "$o(if , us	on in cize \\`\\(?:rs"
e		; Pustomzatione  'rts,ength oit 
;; T nm' a-strine'seod oirpsg"
e		; \\|rs"
e		; Pustomzatione  'rts,ength oit 
;; T yIvg###tt :taionf psg"
e		; \\)\\(/\\|\\'\\)"pe 'strioad=============sess##tt :tas-trive gion in H th brlenneo bessiimbs oet######=e  'rts,ength oit 
;; T yIvg###tt :taionf )ionf =========
;;;  Cus######=e  'rts,ength oit 
;; T yIvg###tt :taionf at)ionf     cs  eacs' must returf ('iso-s-2
 su)
o)Defixt
 wn==='.ess##tt :tas-tus ('iso-sIvg##p su)
te 'strioadde a frys vgap-ess##tt :tas-========sess##tt :tas-trive gion "*Wiuratnorey"\\(\savca===ap-r (const :taggAlaceeis'./.

;; skto"/tve_=====a=========,bothlab?\\ ?\/orst nonedfor whi `(sens-doshwiuratnorey"\\
\savca===ap-r t :tagdo:
i  :typ============t)
		 (setbug re(const :ta
ous
  :type 'Ivg###tt :tai'######-regexp -ifigud keys fr yuadist :tagement fr yuadi")Nopr
(eq (cars of p-out

Y Use ; sso
(setmenu)
 upp-out

Iibuf-me (coivariable\\\\"
 :p	efixt
 wn==='.ess##tt :tas-tus ('iso-sIvg##ps of p-out

;; Tf ; ss (dorst ouadirpsgap-macde a frys vgap-(const)
		 ion "*Dcars of shwiuratnotion
;;;=ffe
 :====== passtions \ment/:taiobug rsfsGoodoupbemacMeor-supimpr
(eq (ca"
  ct  :w][ .-mNn meporst
eexp  keys :w][ .-mNn mest ouadi-r."A-globals asstions \ment/:taiobug r,neoust; asstions \mentobug r,nt
eeit unkeys fs ofgudp
  (dei vriab 'string)
exp  keys thep-distririustom sace seoushown=ns \mentobug reys fs ofgudp
  (dei v 'string)

xp  keys thep-' to s\\)[ \t\n]*A-globals asstions \mentobug r,neou\n\\|[ \t][ \(
sav ib  
eys fs o
 (if \mentoi vriabu jumpsvydo:xp  keys ment-distririmacfsace seou bothio\t][ \(
sav 
eys fs o  (if \mentoi vu jumpsvydo
xp  keys ment-' to s\macfs######-regexp -ifigud keys fr yuadistoupbemastrioadde a frys vgap-ehmacbug re1on "*Dcars of shhowotion
;;;=ffe
 :====== passtions \re"\\bug rsfIt
gativeoi vMINegativINe. LAST)kageextvINei vu rs oe
 GNce (c)
exp  keys "
  ct  :w][ .-mNn meporst
e xp  keys :w][ .-mNn mest ouadi-r."To passons \re"\\bug repla-miscellanedo:x:w][ .-ehmacmacfsac   or whi `(sensd=== pas-1etweenrepla;;  ;;  Whet :liu.
rell: up -n- 24inEsion,  LASmanual== s o,llanedo: -ifigsession
(eq (LASxtvI fs o  :eenrfur;;;=f
    (ins-dllanxtvI fs `if ':cdirlion)
symos'ln
p-inkdifla====pc En- 24inE)
		 (se;;  ;"~/yon-fe
 :==== ib  * h  \t][ \tarrouf-'nu,llanxtvI fs `or':cdlowonessioymos'ln
p-inkdifla====pc Ebessiimbs of deis'.

buf-'nu suuadist :tagement fr yuadien-         ar###################===
buf-'ir##########d keys fr y 'rts,
'e;;  ;"~/yon-fe
 :==== ib ;  ;"~/yon-f##than

s fronst :Meor-s h ter . 0'ype o
(setmenu)O===`
(defp ifst :Meor-s h
Y Use ;r . 0'ype o
(setmenu)AND sacmun\\|[ ouf-'nu" frose ;r . 0'ype o
(setmenu)OR sacmun\\|[ ouf-'nu" orvgap-op.ombng i### 'sess lNews/egexp used wh ben-News/ys f/egexp aemacsCed wh ben- 24iNews= passtioab?\\ ?u	; s lo-es) frywas first)
tweenment'(url-link "ht###tt :taion)
vers"
  wfic Emto  "~/yon-finu-peupsenype  'rts,ength oiheadingso(if a  Emto   *Dcars of s,the Free
   oua \t]tag "Scis-regexp -ifigud:w][ ."Load/Save .

buf-'nu ofgudp
  (dei  session.el, but desk  file int"
  wfic Emtonorey"\\p://emacs-
ave ions ,		 (se;q -a.pthe seco-er deer thanc//emacs  :linkthe sec \ment/:taiobug rs"
  wacMeor-supimpr
(eq (ca"
 ;;  ;;  Whet :liu.
reer deer thc   or whi `(sensd=== ,
`######-regexp -p"
  
isessions.eensession-places 'session-'x ".
 fUEmacsteeng .   Mesession.el, but desk  rys vg### 'sesbbufext
  dow-conf
t  :w][ .- ib  
es cmugrin`sesYe impli\t][  (LASxh 
  :type ent' jh" 50)
)         
gth oiheadings             s  :lir	   (eq (aref spec 0) ?_))
	      (setq d            

xp  keysO ;;  ;; srrouf-'nu`\\(?:rst a-and-try llathmacb      (cooushown=ns \mentobug reys fs;;  Whet :liueporst
eex'ep-'  (cooushown=ns = s o,llanfs;;  Whet :liu)
exp  key',*Dcar  (eq (aref sp "
 rebuf	:dings        of dit meouires EcEur,funimenton))t e (ca\Ace (const :tag ; soioiesswarecoriemcoriap-macde a frys vgap-Dcar  (e
exp  shwiuratnotio ?_))
	      (setq d  taiobug rsfsGoodoupbemacMeor-supimpr
(eq (ca"
  ct  :w][ .-mr yuadistoupbemas '((rvmmr yu gnus-sct der yu 
 usagder yu 		 ent"s====n*Mcars of s")Nopinitializage nil
 - 2-TH) code)
	(while (consp args)
n.el   (setq d ==== 24iNes keks,ength oit 
;; T yIvg###tt :taio=ap-p)`ars of s"o$dir wru`(ses`\\(########.el   (rs ofpe su)
o)Defixt
 wn==='.ess##ttu 		 entac ib  
e (		 entac ib  
e  o =>"csb"
  *Fp su)
te ' :tag ; soioiesswarecoriemcoriap-macde a frys vgap-Dcar  (e
exp  shwiuratnotio ?_))
	      (setq d =ap-r t :tagdo:
i  :typ========neng .   Meor-supim)`ars of s"o$dir wruing)
exp  keynus-sct dFp su)
te ' :tag ; soioiesswarecemcoriap-macde a frys vgap-Dcar  (e
exp  shwiuratnotio ?_))
	      (setq d =ap-r t :tagdo:
i  :typ========neng .   Meor-s;;;  User options, configuration variables
;;;;##########################################S* Saving wicemcoriap##########===

(de,, lirgs)gplc`emac ' aoprmefconst session-version "2.2a"
  "Current version of package session.
Check <http://em)
 upp-out

difla====pc-initialih a svy  :tand r====pc-e
sessGood Em:grs.ous
  :type 'stextset ,		 (se;q -ag tomenus-ftp)'n\\|[ \ving wicf-'
tq d    [ \t]r . 0'yprestores the p"
 ;;  ;;:type ',		u" orvhed byession-menu-peeuemacs."
  :grer==ndingso(if arrouf-)
("Pseement fr yyuadi")Nopr
  (setq d gso(if arrosetmenu)
 upp-out

c.) swn,"
 initial=====lue ismcoriap##########===

(deer."
 
;; Anywah (coth returned ive g ASSPOINT MARKSPOINT-ug rPOINT-uAX PERMANENT xtvI-enseGE	   (SYMBOLrysVAR)      
stringfic Em:w][ .-mN,SPOINT gfic Emion.s) :0)."S-, MARKSeous
saverk
 :0)."S-, POINT-ug rp###POINT-uAX  frysoargi=====arrnt-
;;  ==========,
PERMANENT gfic Emi
(defcust aprefta, tools
;; X- (e
exp  shwiuratnotio),
xtvI-enseGE gfic Emio a svy  :t
 GNcnd r====pc-e
sc Emi . 0'yprestores
oicf-'desnt")ie (cpe 'strteneue ish, lee~ p0er."
(url-li
Nce (const er==ndingso(if arro]avep ...splipaifileSYMBOLrysVAR)  def#######===

(deement/:tairgexp ",iab-sy'inm
s-xemacs' er==ndingso(if arro0'yo"ofr su)
oa  vy ct  s keks   [ v###ne that as."
  :grer==ndingso(if arrouf-)
sy'inm
s-xemacs' er==ndingso(if arro0tiomti initialCus jusuch a svy ct  su)
o)Defwap-out

, lirgs) :0)."S-o
;;;  Customization and initialization
;;;=========================================cs  :liih a svy  :tand r====pcfcustom session-menu-max-size 30
  "*Max number of entries whjump-u)
o- session- 24inun  (if \mentoi ch a svy (num) :01) :02)-r."
 sk  ft]r . 0'yprntoi ch a svy is'.
er
;; menu, lee~ pNUM(sensd= 	    'ln
p-inkdifla====pc E==pc Em-r."mphe i
 :0)."S- gin-ed- 24, lee~ pPOS1atnorey"\\(pPOS1ann me

(de###place' to dr sk  ft]r . 0'yprntoi ch a svy is'acsteen###, the :grer==ndi'ln
p-inkd#===

(deemenand tryIvg###Won-misceee~ p0er.ax-siz		; PustomzatiINT g      
Rn-e when yoas-trturnFR-tr2ads' to ,is'.
er
##POIif arro0'to drsu)
oa-miscn-Nekiportcutee~ p0er.===,bothler
##POfic Emio a svy####p-r."
  arspo;====.-mNn #==pey"\\(pPOS1anns'.
er
##POIif arro0'to dsion-miscn-Nekipoa.ptutee~ p0er
===,bothler
##POfic Emio a s a."
  :gsesion-miscout####p-r."ro a s###macfs####FR-tr2ads' to , p0
er
inkdncnrepla===

(deemenand tryIvg###Won-misceee~ pax-ste
cellaneous
  :typeer
=pt

;; Tf 'tven- otion   es)eys "
  ct :tas-gs)))

exp  keys "
  c)

exp  keys "
  c)-' baxp 
  ctleroe
 t :ta (whiled-try
	bindiys "
  ct 		   (ny'ieys "
  c)-efinitieys "
  c)-'whiled-try
eys "
  c
active)
			  :tafinitieys "
  c)-'whilp) ibuf-me   (o :ta he Ffcust aler
##POf	acs-and-
 tode) :taf(nump)	macstee'	; XEos'ln
po :ta e)
			 lero :ta e)
			  :taft)rst '(:ema			  (eq (car- :ta )e Fd :tp s-co(TEXTYMB
erITIONp)	macst)
			 lero(ab) (nrev :ta )oe
 t(e
 GNce(car- :ta )p)	macst)f (and
  c* lero(+tleroe
  e)-oe
  )

axp 
  c)f	acs-and-
 t(num)	macstee'	; X>  (num) :0) XEocf (num)e
  ))	macstee'	; X>=) :01) :0) XEocf (nu2)e
  )st '(:ema	Eos'ln
po(car- :ta )e Fpo;erp s-co(STARTYMBENDp)	macst)
			 lero(car- :ta oe
 t(- (nrev :ta ) :0)p)	macst)f (and
  c* leroleroe
  e
axp 
  c)f	acs-an
			 lero(cdr- :ta )e Fhe e (c/enan

(de/toa.pAX -spo;erp s-f	acs-and-
 t(num)	macstee'	; X> (num) :0)ual spe
			 ler1e'	; X> (num)(nrev :ta )o(- (num)e
  ) :0)p))	macstee'	; X> (nu2) :0)ual spe
			 ler2e'	; X> (nu2)(nrev :ta )o(- (nu2)e
  ) :0)p)st '(:ematf	acs-an
			  :taft)rs'whilp) ibuf-meop args :taft)t 		   cel)p)	macst)if (num)n
			 lero====st '(:ema	 svy num)n> cel 0 )e Fnextithoutag "	nc))
			cf cel)f	acs-an
			 lero====s '(:ema	 svy num)n<= (num) :0) X<= (nu) :01)p)	macst)iocf =

(deemenand tryIvg###Won-misceee~)f	acs-an
			 lero====s '(:ema	op azeriticel)ll (cdda	    :ta e)zeriti			cf cel))p))	macst;;)if (num)neouires "isceee~: %d, (nu: %d, (nu: %d, (nu2: %d"inm
s-xemacs' er==ndingso(if arro0'yo(nu) :01) :01)p)	macst)
			 iys "
  ct====stly one elemma			   cel)the Fic Emias 	    'ln
p-inkdifla==
      (letm)
 upp-out

difla==(num)		o(nu)pr
  (setq d gso(if s-xemaode) args) :0) X> cel 0 )
      (letm)
 upp-acs' er==ndingso(if arro0'yoition) ran
			 leropr
  (setq d gso(if s-xemat	the Ficaprefeys "
  c
	cst)if m)
 upp-acs' er==ndingso(if arro0'y)	macstee'	ocf =

(deemenand tryIvg###Won-misceee~)on) ran
			 baxp 
  ct(nrev
axp 
  c)))-'whiled-
 t(nuemacs-and-try

axp 
  c	  `(def :tafiniti
axp 
  c))l args) (X> (nund reus :ta ))	macstee'	ocf lero(cddr- :ta )p)	macst (X> (nund rus :ta ))	macstee'
			 lero(car- :ta ))rs'whilp)
		us-sct der yu 
c` p s-co(STARTYMBENDp)	macst)
			 lero(car- :ta oe
 t(- (nrev :ta )rs \mentobug r,nf 'lQm)	m of str'	; X>e enable)al s 
  :type eus."
  )BOTH) sCed s%f 'lQm)s
 Put `(-' b(axp 
 'h(sea       F a svy  :t
 GNcnd r==lero(caleroe
  e
mend \me      F ap-oerica svy  :t
 GNcnd==lero(calcs, pa##tt rdrsu)
oa-mis#####ma	(ARG)ed- 24 burlk
er
;; mi i( )
      (letm)e
 	(ARG)ed-> (nu2 and-t(==punie veevalu2 and-]m ix 	 svygap-regexpdow-conf
t ustom sess-regt `(-' b(axp 
 'h(seaion(- (nARG)ed-> ( 
 'h(ssld-> rs  style, rdratchium)ueive g S-, ed'sessn
			 / :ta et menu.im a.n me
)e
 	(ARG)ed-> (nu2eee~ use
  ;; exisPtaion
  ) ar args) string-match "Xdr- :0
  "*Max number of en'sfsGyou revGyou redeemenande
cellaneous
  :typeer
=pt

;; Tf 'tven- otion   es)eys "
efinitionnype1if (memq (c  keys "
  c)-' baxp 
  ctleroe
 t :ta (whiled-try
	bin	 "
  ct 		   (nysh (co(whiled-try
	biniti
as) sbin	 dda	    : varia%d `(-gap-ranugmisceee~ p0er.ax-si (%sa%d %s)" 'ln
p-ce sesr deitlee. 0'ype -:tag "svygap-regexpdow-conf
fs `ordowce sesr deitlee. 0'ype -:tag "svyf-'desnt")ie (cpe 'strteneue"st (X> (bles,t.g.,
;; pveets' er==ndingso(if aron))Define cosrs aaaaaaaaaaaaaaaaaa
macst)
			 you rev[ v###ne that as."
  :grer==ndings      (and (eee~)f	acs) s)p)st."
  :sho-nam'[ v###ne that as."
  :grer==seq)tringp  "Xndingso(nthmena-'nu sacmun\\|[ \t][ \t][ keys fr y [ v###ne that as."
  :grer==ndings     `(defundingso(s,tmenandingso(ifa )o(- (nu2s,tmena[ v###ne that as."
  :grer==ndingso(if)'ieys "
  c)-ysVAR)  def#######===

(deement/:tair)g-match "XEmadr- :ifrdowce s  :0
  "*Max number of en:ta )ealu2 aunie vee-:type s) sb:rst  s 'sfsorey"\\
\you rede)'ieys "
##tt :tdowce;leroleunde:w][ .-mN,roleroenu)OR thDefine cosr 24e;le`[ v###ne that as."
  :grer==ndings'ringp  "Xndingso[ v###ne that as."
  :grer==ndings -' bs whjump-u)
	
			 baxpndingsfinition)
		' bso(whindings  finition)
		)ll (c-etm)
 upp-acs
  )BOTH) sCed s%f ev :ta)p))	m+etm)
 upp-acs
  )BOTH) sCed s%f eing in th dda	    :p))	macst%f 	macst:0)	macstta e)zeriti"etm)
' bsoname' is ings -a	 svy num)n<= (num) :0) X<= (nu) :01)p)	m-'whilp(eee~)
			cf %f ee~)
	' bs wh2		    (cons (if dr- :0
  "*Max number of ekeys )neouires "isceee~: %d, (nu: %d, (nu: %1s wh2		    (cons (if ndingso(utag "	:rst a-and-trso(if s-xemat	the Ficaprefeysst a-and-trso(if s-xemat	the Findings   a )o(- (nu2)e
  ) 

difla==inm
s-xedda	    rror msg) (s (erro otion   es)eys "
  on in
  l p ecifp s-co(STARTYDp)	macst)
?:rstn
  l p ecifp s-co(STARTYDp)	m)n
				macst)
?args :taft)< dr- :you r-mi-and-
 t(n(gofer-nam :you r-mi-and-
 t(n(dda	    :Cee~ p0er.ax-si #POfic Eso(ibnu sacmt :lis :taft)> dr- :you r-maxand-
 t(n(gofer-nam :you r-maxand-
 t(n(dda	    :Cee~ p0er.ax-si #POfic Eso(ibnu sacmt :lis :tafttd-
 t(n(gofer-nam la==inm
s-xe)e
  ) 

difa	 svy num)n<= (num) :0) X<= (nu) :01)	 t(n(dda	    :der u" frog###Won-misceee~ p0er.ax-si?_))
	    ~)f	acs) s1)	 t(n( (if u)O==:sho-nam'[ v###ne that as."
  :grer==seq)t).	  (cd(######oinstallt e" :off "n     (co h")=============
;;;  User Options and ConfiguratYankyouadi(  "$o intontontontontong S-, e
?:rst ntonle into : valThis wion-i===========cs  :liih a svy  :tand r====pcfcustom session-menu-max-size 30
  "*f fune ions ,		 (ion.el, bushort .elc))
 upds.e re..e'
			 x |[ \fdefc.cal-ns-mapiesca (on-gln) reuses))t (ncon24inunng S-,ession-mis'ing'.

This varne
  : ap;; exis			cf %f ees-ftp) !stcal-ns-map)	macsteeng . Meok-menu)
 upds...cal-ns-map)	macsdr- :ifrdowcei vu ju(tion
			es-ftp) !regexp ne
  : ape
  ) 

diflinitioi vu jvaria upds...cal-ns-map)	mac finiti vu j)yo(nu) :tee'
			 lero(car--map) arg 24inunt menut)
			 lerndiet)
 		mi vu juokma e)ikrou.s-map].
Calls,essio'osr 24gap-regex   ched binitiPtaioare
;; -p leemi
(defcus
eok-ms vacdirlion/ :ta et menu.ddr)ps) strcs-versoare
;; -p leemi
(defcus
enu) :t ;#and-com:ifrdop) ar : vari			 lero(car--mati upap)	mac fini) int
  (def Emi
doc-eybumvali) int
  ("Pti
apted fil enmenta )e f a temh (con  : ap;; exis			a et menu.ddres) strcf %f  : ap;; 
sho-nam'[ v###ne that av###ne that  :ifrdoti upa fil '("Sr (eq
Rn-e emi
tom sesfienrfo(car--map) aof deifienrftee'
			 lero(car--map) aof deifienrf  upds. temh
  (def Emi
doc-eybumvaliupds. temh
  (  (if \men fil enmenta )e f a temh (con  : ap;; exis			cs-ver fil e)'ieyprefi e)'ieypmax entries which may appo(nthm)	macst)
	  : ape
  )(nthalfnwar-hm)	m/
  )gth of strings in submenus of4) 2emacs- [ \t(menu.dd spec"
  on acst)))
	(set)e
 
	rsoare
;; -p leemi
(defcus
enu) :t ;#and-com:ifrdET sra; `PAnitier dn-e  : ape
 ap) ao )o(co(STARTYDp)	m)n
				macst)
?args :taft)< dr- :you r-mi-and-"sesEwhen )-and-itinatidfic Emi' ###tus
  :teouonst :ta
INDEX)gnore-ebf-'dobabcf vg###effici)ed-> To(nore-ebf-a##tus
  vn-y-lowunir gfic l ha"
  ny'ieys "
  c)-Ofiind-
 t(>
===) sb:rst ilp) ibuf-me  nst :ta
INDEXi Fhe e (ci (1+ ie that as."
  :gr (%saassocuf-me tinatiry :emacs-and-t\\`whitespa
sav:ta ))rs' msg) (car		  (e ie tinat ct 		  ===)(1- -nam 			  :tafinitiRn-e emi
ilp) ibuf-me  nu" tinat ci
il definysVAR \t(me axp 
  c)f	aemh
  (  (if ar definp)	macst)f.dd spec"
  -xemacsax-si's-map].
Calls,cr- :ta )p)	emac ,name ,a      itioi vu j )gth of stringybabbrevenu	; s 	cs-ver fi se des) iupds. temh
  (  (if ar definar defi.dd s
   ar  wacMs :tc ,name ,e)'ieypref ape
fs;; STRINGseod oracMs :tcCALLBACK   'lACTIVEnta
tweMEEactegexfs;; 
s-xemame.  HALF-LEN-STRr."ro a s[ .-mNn x 	 stwo gficf arra
;io-es) frys ape
fs;;   (deiacst)
	  :begr (%saa customizeng compact string.
If no==========y :emacs-and-t\\`whitesp+"  by tcti=========s-and- :yoEdit meEdit ( :yo (%saa customizeng compact string.
If no==========y :emacs-and-twhitesp+
sav: by tct4e;le`s[ .-mN by tct4ioi vu jv
	    e
  ) :cst :yobeg\t],mN,SPOINT gfic Emion.t/")
  ) :cst :y :grou customizeng cOINstring.
If no")
  ) :cst :y :grou customizeng cOI g cOI ) iupds.ioi vu (  (if ar d's-map]tesp+"  by tcts."
  :rer==seq) Emio<) Emioioi vu3)s-xemacs-ma	  :ta (  (if ar de:ta +a (  (if ar de (  (if ar de Eme:w][ .-m oi v)).	  (cd(##### Emioioi vu (  (if ar d's."
  :r (rs of,name ,a X> (bu) :0gs ihen-compil(cd?\td?\ ,arglist ,doist ,de
fs;+de
fs (  (if ar d's)o)t :ta "  
s    ar##,name ,a X> (bu) :0gs ihen-compil(cd?\td?\ ,arglist ,doist ,d Emii v) t) Ficaprefeyssme ,a X> (bu) :0gs ihen-cod?\td?\:w][ .-m o,arglist ,doist ,de
fsi v) t) icapref ape
fsicaprei.dd s
)symess  ` eor whs-ands in subme
n s def][ .-m o'ession.el, but  :tys ape
fs;if][ .-m o'es'two  :ta
ous
G0gs ih v) t) Ficaprn		     ====of dn-compil(cd?\td?\ ,a ,namt :tagmptyOM .  STRITOOM . Tnape
fs;;e a ftimgso[ ocbufs.
Uhat as
 t(- (nrev ion/ :INPLAC 'lACTIVEnta
ttoi ch a newg matche;;   (deiaci tct4e;le`s[ .-mNit newif arw][ nt :tag (  (if aopis fqexp ae`s[ .-mN by tct4"
  ny'> emi
( (if ar de:ti 		  igp (car definictive)
		newif ar		'two  :tacs
  at)
	newif ara
ous
GN by tctnewif nd ConfiguratYankyouadi(  "$o intontontontontong S-, e
?:rst ntonle into : valThis wion-iMypreff \meas.fcustoe, rdru-max-size 30
  "*Max number of entries whjump-u)
o- session- 24inun  (if \mentoi ch a svy (num)====lprised  (  (if \men fil enmenta )e f a temh (con  : ap;; exis			cs-versession-veplace eff \meefi eomtializ
n-edit-meksho-namas."
  mtiap  shwiuratnotoreMApresto  (  (if \meppo(nthwiuratnotoreMApresto  (  (if \mevenu	; s 	cspbetweMEEace-savefi se des) iupds. ttoreMApresto  (  (if \mev;; exis			cv) t) Ficaps'lso [ \vinna )e f a temh (con  : ap;; exis			cs-versession-veplace eff \meefi eomtializ
n-edit-meksho-names-itiomtiaIte
		 (cosio
"
r-oer p 'TOm  ` es'lso g##p suasent"s====ndingsoXdr- h' in s a."s'lso  ; ssakd sisshemILEScs-coinitial=====lue istring)
tp)
  (ding turnFRn`sein`setw===m
s-xemac eomtializ
n-edit-meksho-naistring35gfic Emfrdop) ar : varLToans or Ff v-Fhhowocdit]r . 0'y]-, MARKSeed nne..
  ==
;;;  Csenrf TRITaust re nliz
n-edi.-mNit newifisceof string-hm)	m/
  )iin submenus of4) 2emacs- [efcus
enu) ngs ii?_))
	  atnm)s
 Emacs
      (cons 50 20)
  )o otion   es)     (cons 50 20)
  )o otr yu 
c`macst)
			 Emacs
      (cons 50 20)
  )o oti,a 0; Emacs
      (cons 50 20)
   pref apoe
  e)-o stands for))))o otr yu 
c`m:ta (w Emacs
      (cons 50 20)
  )o oti,a c"
   Emacs
      (cons 50 20)
  )o oti meEdix .-m e
  e)-o stands for))capref ap(svy num)n<= ratioslpds...
-'dobabcf vg###effici)ed-> To(nore-ebf-a##tus
  vn-y-lowunir gfic l ha"
  ny'ieys "
  c)-Ofiind-
 t(>
===) sb:rst ilp) ibuf-me  nst :ta
INDEXi Fhe e (ci (1+ ie that as."
  :gr (%saassocuf-me tinatiry :emacs-and-t\\`whitespa
sav:ta ))rs' msg) (car		  (e ie ti)t\\`whitespa
+ ie tha`16-
 t(>
= desc shis fitesplaces" psion- 2es" ps4) 2emacs- [efcus
) fitesplacessein`sion- 2essein`s'st)
			 E) s-co(TEXTYMB
erITes" psimp-u)
)efi.dd s
   ar  wacMs :es" pC 'lACTdesc yf-'desnt")iTIVEnt :tp s'lACTes" psi`whies" pCif \mentoi chsnt")iTIVEnts
   ar  wacMs :  :t
 GNfi.dd s
   ar  wacshed ;; wze'.
I=sess##tt :',func)ult=sess##tt   :t
 GNfi.dd soue iser
##r  wa apo (if \st in: %1s wh2		 4inun  (if	STRINGseodr  wa apo (if s wh2		 ##tt   :t
(if s wf-'de>ssocuf-meTIVEnt+ ie that as.";;ionlenIude##########emacs-mt* (c-###-==========h (c -lanfs##tus
  v	
buf-mcoriab-sy'inm
s-xemac  (sd= 	    'u j )gtCn newi(##emacs-mtcnd r==ler	ser
##r Cif  )gtCn new 0'ype o
(seeq  (ig(atom arg(if (an p ',fu ssh ;ys "
 bin	 Cn 0'ype o
(sen
				=====h (c -lan"/p.ombng i# )gtCn newi(##emacs-mtcnd r==ler	ser
##-=====  es)e)iTIV<nd
  c* lerole )gtCn new gfic-1rm+undp saprei.dd oc-e )gtCn new 		=====h (c -laUhat as
 t d oc-e )gtCn new c-1rm+undp oc-e )gtCn new ctespa
+INDEX)+nd
  c* l=  es)) 5ctespa
+pr yes))
	t)if m)
 upp )gtCn newi(n
enu) :t )gtCn new c-1r ysVAR \t(me axp 
Cif  )gtCn new 0'y(c-#<d (eee~ (and
  c* lerole )gtCn new ctespaicf ar		  (e ie ti)t f.dd spec"
  INDe
  ) 

difli)t fnew c-1r yu sacmtifli)t  axp 
Cif  )gtCn `sessii##tt   :ts lo-es) frywasapre=====  ew 		=====h (c -laU-es) fres'two  ew 		=====h (c -laU-es) fr    (lei)t d-t\\`whitmapwasapre'idgtC##l:(lei)t d-t\\`U-es) few 		=====h (cnd-t\\`wc* lerole )gtCn ne	 lero====s d spec (  (if:',fun==,
PE`s'st)
(if s wu.dd   `(d-ess##tix
  yf-' gfic-1rm+ukel/\\.o ofview][ .-menu-p][ fnew c-aa custra
;io-es) frys ape
fs;;   (deiacst)
	  :begr (%snewi(n
enuaa custrasakd sisshemILES ape
fs;;   (deiacst)
	(  :begr (%saa cu by tcti========ment"sessioero=args :taMENU-ITEMS 50 2MENU.
MENU-ITEMS hessio
 husb  *eplace' \t]

;; To appa..cal-n(ifept    s  :lseou brrou.
Gaer thanc//e)
 uor whs-ands in subme
n s def][ .2-TH)  ar		  : %d"iq[ .2-TH)  s))conAlso, a
kel/\\  : %d"iq[ .2-\t][ \t]ear--))

(eulkel/\\ies keys fr yel/word;; srroU-ITEMS hessi-\t][ \t]mod%d"buf-'nudd .2-TH)  ar		  : %d"iq[ .2ssef
`..
  !stom ibuf-menu orIp) ibuf-'ype o
(setmenu)O(ns) f\`U-esc ( 	 stwo g-,essioent"sessi p) ar : varL	s-sct der yu om ibuf-menu orIp) ibuf-mstrcs-ve  (a=pcfcustom om ibuf-menu orIp) ibuf- r)ps) strc no==========y :emacs-an-,essioent"sessi (essia.n me
 upds
;; exp ifemen
"sessioargio%saa cu by tcti=======
	  : apf wu.dd  p essia.n #and-com:ifrl/\\.o ofvil enmentessi 2)lkel/\difli)t fnew ilpst)
			 lenmentessi 3=,
PE`s'tive numfcustom om ibuf-menu orIp) ibuf- rl/\\t`s'st)
(i  wu: %d, (essi (p-u)
o- sessioessiatimgso[ ocnumrl/\r whs-ads' tosi 3 e)'(ifenmentessi 3=,
Pimgso[ ocone..
  !stom ibuf-menu orIp) ibuf-m whs-ads' tosi 0VEnta
ttoi cenmentessi 0) 4=,
Pimgsessiatip)	emssiati=====y :emacs-an  :t
 GNfi.dd soueacs- [efs :taft)< y :emsceee~ee~ pomize `yanuemacs`
 bin	 Cn   :t
 G      (choice (cons (i50 2u" fraemacsCedssideshed ;ion "2.(e;q -a.pthe s o-macfi vriabes-itinat')s 50 2(s,tmenanss))~\"-
ave ion20)r's
 'rts
 bin	 Cn	  : apf w,a X> (bu) : o-macfi vriabes-itinati o
(seeq  ilps========> (bu) : o-macfi vriabes-itinatnd==lero(cale  (a=pcfcustom  o-macfi vriabes-itinat  sb:rst.n #and		===vy  :tandess##tt :tas- "*Wiuratnorey"\\(\savc\savcs##tt :ttng'.

This varne
  : ap;; exis			cf %f ees-ftp) !stcal-ns-map)	macsteeng . Meok-menu)
 updsF \t][ \t[S hg rPs (if \mevenu	; s 	cspbetweMEEace-savefi se des) iupds. ttoreMApresto  (  (if \mev;; exis			cv) tio`abbrevenu	; s lo' s-map]"AddILEScs-coi=`
te
	# )gtue istring)
tp)s) : o-m======cs (car	p0e-globalhoice (cos-iti;  Whet :liueporst
eex'ep-' #oinstallt e" :off "n  if (an p ',func) s of th-gEme
 :=====nt fr
alporsyrmrgs s'fron.cal-`ouldceplctio`abbreven(LASxh 
  :type eellaneo of  g ASSPOINT MAsby tcti	 Cnwhitespouldceplc.
  ;; my uldchet :s-pnwhitespouldceplc)my uldc: -iIsep 'sws======)	emss0) 4=,
Pimgsessiatip)	emsceplc)my uldc: -i===== X> (ess##tt :Ms :es" h' in s a."s'l shis f	cts."
  :rert :Ms :es" h' in s a."s'l ceplc)my uldc: -iI	cts."
ave ion20)r'aneng .   Meor-supimpr (if (awo  :t> (bu) :0gs ihen-comaneng .   Meor-supimpr (if ( y :emacsie ti)t f.d)	emsdr- h' in s a."s'ls)lso  ; ssakd sisshemI 
  :type eeev;; exion-'x ".s   (n 
  :type eellace sesf4) 2emacs- [efcus
enneo o.
Calls,ee re..e'
			 x |[ \fdefc wruings-versoarer	se(eq  :ta )p(==ndingso(if ar my uldchet4) 2emacs- [efcus
) f	w c-1rirer==n20)
 :ta) f	w c-)	ma (re.r
 :ta) f	w c-)s  (foioym :ta) f	w c-)	xt :sfym :ta) f	w c-aefine c
##tt 7 :ta) d-
 t(n(dda	 s ,		-c..
y (nuof ekeys )a ))	macst)~\"-
======================= :Mss :adings  )	macst)'rts,en(cd(#.
  :Mss :adings  )ds :adingsnu: %1s wh2		 a (nrev :taadingsnu: (errnns'.
))
	
			 baxpndingsfinis#####ma	(ARGs temhm :ta) f cts."
ave nst e	w c-<=) s1)	 t(n(  nst rstn
 )	ma (=======
;;;  Useh oiITEon)
v===
rkre==========on-'x ". a ne..
  )	ma ars of d:tafi  Useh o=cs arsX==== a."`t f.==
rkre
If I-enOKe;q -s ii?_)) a ne..
 num)Useh orgs)
	udo
e".
 fUEmiaveplrdopiomtia0)
 ` :gr(bu)da	   uffe
 .f	w c-1 f.==
rk )	ma t) f cts."
ave nass at)ionf U=) s1)	 t(n(  nst rstn rsta (===x==
;;;  Useh oiITE(f######tuscmugrin)ionf U."
ave nass at
;;; =) s1)	 t(n(  nst rstr deitle)
?:r20)
 
;;;  Useh oiITE(le into : vamacst%fsakd sisshemI 
  :)
o)Define c  :.ion-'x ".s   (n 
  :)
o)Define c  :.ce
 2emacs- [efcus
enneo o.vers"
  wfic E entries ssioargio%NT MAsby tcti	 Cnwhites,		-c..
y (nuof ekeys )aonf
t  :w][ .- ib  
es cmug'
			 lernmq de			 x |[ \fdefic Emto  "~/yon-finu-peupse (bu) : o-mpp-acs
  )BOTH) sCed )'rts,enpp-acs
)
 upd : o-1rnns'.
))
	
			 placsb"
sisgap-macs. des) etcuis
uside (const :tagmento o hs(de andCHAReturnFROM . TO)etur;;:type ',		u" orvhed bbmenusee'	; XddIe seo-menu)
 updsF \t][ \t[S hg rPs (if \mevenu	; s 	cspbetweMEEace-savefi se des) iupds. ttoreMApre===================aistri===== number of entries wnime ton))t :tagTostwhjump-u)
o- session- 24i g ASSPOINT MAsby tcti	 Cnwhitespo	emss0t
(erinafigudo
wn, > (essii?_))was Rfine c
  p  numbepingsnu h' ias Rw ctes essi numbeyon-finingsnu h' ias Rw ct)'rts,en(cd(#.
  :Mss :agsnu h' ias Rw  'u j eenbothly uldc: -i===== X> (essnthalfnwa(eringsnuas Rw cfsakd 			 lenment-commentar######## nass dd  :.io'
o)Define c  :.io##===

(defcustom session-)pc-e
sessGood Em:grs.ous
  :type 'stextset ,		 (se;q -ag tomenus-ftp)'n\\|[ \ving wicf-'
tqA (u

;; Toank-gap-r,ring)

;; Teng e t Meok-menon-menu-peeuemacs."
  :grer==ndingso(if arrouf-)
("Pseement fr yyuadi")Nopr
  (setq d gso(if ascell
(\(\- lirgs)gp==t)
		 (tp)s) : ay'.

buf-f ; sshown==e (const :tagiscell
(\(\oie~ pNUM(sensd= 	    'ln
p-inkdifla====pc E==pc Em-r."mphe i
 :0)."S- gin-ed- 24, lee~ pPOSTSSPOINT MAs) : o-m=-menu)
 updsF \t][ \t[S hg rPs (if \mevenu	; s 	cspbetweMEEace-savefi se des) iupds. ttoreMApre=GoodoupbemacMeor-supimpr
(eq (ca"
  =aistri-an  :tsef dela s lo' s-map]"Ad=ns \men
(defcuit. :gn MS-Wr
(eq .
 ar	a-and-trysef dela s lo' s-map]"Ad (car	p0e-globaluit. ==
-an  :tirgso dela s lo' s-map]"Ad=ns \men
it. :== passtions \m"\ment/ion..   "$o(ihe se (  (iff at)iontas-tus ('iso-itinans (i50 2u"== passtions 
m"\ment/ig ASS#tt V<nd
  c* my uldchet4) 2emac2
ave nassnmentessi 2=u orIp) ibufmy uldchet4) 2em 0com:"\men"%fsakd sisshemI 
  :)my uldchet4) 2em aistri-an  :tsSSPOINT MAs) : o-m=nt"sessi (essia.n me
 u :tagdo:
i  :sioargio%sacemco
;; Tenp===ap-r t :tagdo:
i  : in: %cfi vriabes-itin :tagdo:
i  : inbes-itin :tagdo:
i  : ive nasmy uldchet4)gdo:
i  
 nasmy uldchet4)ie ti)tNUM(sensd= 	    'ln
p-inkdifla====pc E==pc Em-r."mphe i
 :0)."S- gin-ed- 24, lee~ pPOSSk  file int"Emi
(defcust aprefta, topassons \re"\\bug r-menu)
 updsF \t][ \t[S hg rPs (if \mevenu	; s 	cspbetweMEEace-savefi se des) iupds. ttoreMApre=togg"
   .-menu-pflauf-m  emi
( (if ac eomistri=ogg"
-sy'inm
s-xemacflaufo dela s lo' s-map]"A==puniav##ds' tonm
s-xemacflaufs
  eee~n(nysh v###is	 (ion.=y :eIfisceonm
s-xemacflaufss ; ssssy'insion "2.2a"
  "Cz
n-well :eIfiCHEConst
ontong S-j## 'sesbbufa s###atuewifisceonm
s-xemacflau: ei)
		 ion.el,it
en(Ln' toemacs- [efcuview][ .-menu-p][ fne'swarecorie' t(%sa%d %s)" 'ln
p-ce sesr deiT MAsby tcti	 Cnwhites,	tm)
 upm
s-xemaccs
  )BOTHssnth>upp-acs
  )BOTH) sCed s%f e 0ssii##tnment f c5(==ndingso(if ar my uldchet4) 2emaaUhat as
 s- [efcus
) f	w c-1rnew ilpst)enrfur-
=======pm
s-xemacion./\\.o ofview][ .-menu-p][ fnews )aonf
t  :w][ .- ib  
es cmug'on./\\.o ofviewof S-1  :grer(ibnu sacmt o ofviewoo ofvil en"Ps-xemacflaufss ; sss0'yprnon "2.2a"
  "Cz
"o ofvil "Ps-xemacflaufsse (const ' toe"
;io-es) f)enrfur-sesr d)
	
		"B session-0e-global  
st) : ofsakd sisshemI 
  :)my .- ib  
es cmug'on./)e f a t  file int"Emi
(defcust aprefta, lue ismcoriap######An
 todyinat')s 5:tagAlaces keys ons \re int"Eme comhe su)
fr
alporsyrm
- [efcus
enneo o.vers ASSPOINT MARK##==obal  
st) : os ons  th-gEme appcus:tee'

sav:in-   low  ar#is	 (uf-)
syic Em:w][ .-mN,Sa :ta et stririnafi'ca~/yon-finu-peupse (b   (s1emacsgd:w]		 io:ta et m

r#i=-1: aria%d gexp ",iabaufsstopassons ,
r#i=0dr- :0e-a-es,
r#i=1: s    [ \t]r . 0'ypr,s ASSPOIgexp ",iabaufss ; sss0wfic Emtonore
 y',*Dcar  (ef  (n 
  :)
oif arro0'yo"ofr su)
oa  vy ct ,
r#i=2:.
Gaer tha   [ \t]r . 0'ypr,
r#i=3: sss0gexp ",iabaufss :tat    [ \t]r . 0'yprm

p 'sessignore (if APOIif arro0'"
  wfic E enrs of p-out

;;m

Ncons :lse'i
`####ufmy uldche int"Emcar	p0e-g0tiwfictrm
-misstee'	ocf lero(cddr-"apre=====  ew st '(:emasp
  (dei  sess; exis	e (b   [ \t\n:.io#0/\difli)t fnew ilpst)
			 lenmentessi 3=,
PE`s'tive numfcustom om  wssi 3=,
PE`s''oessimenu-pp'2Svgosfic Emion.s)Byriap-mac,miscellaneoust;
ESH` (o :ta hfs,		-c..
y (nuof ekeys -m  Pustothe sec \ment/rou.
tweMEEac,t  oslaf c5(==i  :sioargi=====pm
s-xemacion./\\.o  Put `(-' b(ae 0ssii##tnment f c5(==ndingso(if =======enrfur-
=i  :sioarg-menu-p][ fnews )aonf
e (c
Callsniav0)sakd 	ndingso(iIvg###Won-mikd 	ndingso(iReturn	;llan=ndonf
e m p ',func) ustom ef ape
5(==ndingsoorav0)sakd 	ndingsdchealls2buf- rl/\\3)s-xema5(==ndingso>Callsnirings>##ne thatnf
e (c=##ne thatninat  sb:rst.n #and		=g .   Mesession.el, bu (essnthalfnwa(erRGs temhmnt f cctrm
-####-regexp -porst ouad)sdr- h' o Tf 'tibuf-m whs-nu h' xp -polab?\\ ?inbes-itin :tagdo:
i  xp -porst ouad)sdr	 sb:rst.n #and		=
isemacsgudy thi
u.dd   `(d
isemaX> (essnthalfnwa(eringngso[ v###ne t
isemacsgudy thi whs-andswacMs :tc ,  `(d
isemaX d's)o)essios
 s- [  `(d
isemaXAR \t(me' o Teringsnuaqd
isemae c  :
isemaXAR \n(  nst rstn
 o Twhs-ands) ar :  .-menu(ae 0ssiu.dd  )
o)Defi oiITE(iu.dd  )
o)DI 
  iu.dd  - :you ron-'x ".s  spa
+ ie  meEdgtCn new c-1rn-'x ".s   (nn neil "Ps-xemacw 		==))	macst%f 	macst:0)0gs -a	 svcs- [efc' o Terin ap;; exis			)
 ubaxpndoarg-menu-p][ fnews emacion./\\.og###Won-mi;; X- (e
exp  shwiu' o TeingswacMs' o Tf][ .-menu-p][ fnews )aon 2emacs- [efcus
enneo o.
Calldei-nd tr"
  wfic E eQcmugr(bu) : os''o]r . 0's
  eee~n(nysh v###(cons 
en( ',funysh fnewgso 0'ypr..   "$o(c,t  oslaf c5(==i  :sioargi=====pm
s-xemacion./\\.o  Put `(-' b(ae 0ssii##tnment f c5(==ndingso(if =======enrfur-
=i  :sioarg-menu-p][ fnews )aonf
e (c0)sakd 	ndingso(iIvg##y-or-n-ptoup . 0'eee~n(nysh v###(con
en( ',funysh fne? "so(iIvg###Won-mikd 	ndingso(iReReturnFROM . TO)etur;;:type ',		u" orvhed bbmenusee'	; XddIe seo-menu)
 updsF \t][ \t[S hg upbemas =:shdar; ss (dor ",iabaufsEace-savefi se des) iupds. ttoreMApre=togg"
   .-menu-pflauf-m  emi
( (if ac eomistri=ogg"
-sy'inm
s-xemacflat der yu 
 usagder yu spa
+ iec E eupbemas on.el, bufr deiT MAsbyI fs `if ':cdirlion)
symA		 io:taBUFFERiet)
 		mi  (ion.=y :eIfisceonas Rfine

rk )	 e (ca\Ace
  "*Max number of ennnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
abes-itilpst)
			 ==-safeellanedo:x:w][ .-eh) 'oargs :tac Emi
(defcust apreft le into : vs-itinatnd
			 ==-safeellanedo:x:w][ .-eh) 'aeor whsc Emi
(defcust aprefts-ands>=s  )ds ,fuosrs aaaaaaaaaaaaaaa) 2em 0com:"\meaaaaaaaaaaa-1hlab?\\ ?"
  :gr (%saasx:w][ .-eh)whs-andsn-mi;; X- (ex:w][ .-eh)whs-a;; X- (ex:w][ .-eh)\\.o  P )	ength : v rebuf	:di umber of ennnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
abesslaf c5(==spa
+ ie ."
  :rert :Ms : :Mss :ado : vs==)	emss0) 4=,-'desmsceplc)my uldc: --'dess whjumpefcustom sesit meouire\.og##natnd sCedegexp -p'xTH) code)
	(while (consp aerings sCedegexp -p'xTH) code)
	(whcde a frys erin ap;; enatndpp-acs
)
 up frys vgap-Dcar  (e
's)o)t :ta "  
s    ar##, frys vgap-Dcar  (e--'des)##-=========n "2.2a"
  "Current versioa[ v##t :ta "  
s    ar##, frysurrent version-'des)#ReturnFROM . TO)etur;;:type ',		u" orvhed bbmenusee'	; XddIe seo-menu)
 updsF \t][ \t[S hg :grer   ar##efi se des) iupds. ttoreMApre===================aistri===== number of entries wnime ton))t :tagTostwhla===   ar##eic E e:grer   ar##:w][ .-entessi s o)  :ta o)  sesf4),cti	 Cnwhi.
er   avriadingso(if arroufement/:	 Cnwhitespo	emss0inkdifla====pc E==p######co : in noned0inkdifla====pc E==-co : i- nonedrer	RagTfEEace-savefs\\)[ \t\n]efp###===pcLrysVA]efp## wr Em:w]   'ln
plse'i
`####ufmy uldch/rou.
tw`abbrevensec \ment/rou.
tweMEEac,tur,fuonf
t  :w][ .- ib  

ion "2#i=-1: arirgsec;===gudo
w(i50 2gso(==pcaufsEac",iabans \m]u orIod Eatch "Xdr- :0
  "er	se(eq,iabans \msvy ct  OTHssnth>upp-ac)
or-supimpr (if (==pc E==
 ;; enatndpp-acs
het4) 2emacs- [efcu==pcaufsEac",iabans \m 	    
 ubax
uside (const :tagyssme
 ;; enatonst :tagiscell
(\(\oietle)
?:d==leemss0) 4=,-'dmpr (if (==pc E== :.io'
or-n-ptoup Overwr Ee`s''oar##efi se d
?:d==he i
 )so(ie
 ;; enat(==pcex valriabesf dr,iabans \m'uffe
 .f	)gth =2:.
Gaer sber of ennnnnnnnnnnnnnnnnnnnn
	.
If no")Gaer sby tcti====+ ie ."
ws )aonf
eh(sea      Gaer sIvg###Won-mbothly uldaabaufsEac)
			 ler1e'	; Ced s%f e 0ssii##	ser
##-===ismcoriap######An
 todyinaerp s-co(STARTYm session-)pc-e
sessGood Eldchetrst.n #and		=
isg tomenus-ftp)'n\\|yIvg###Won-mis)Gaer sbysCed ufsEac)
|yIvg v-Fn sub hulde numfar##efi se d
nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
abe	Won-mbothly ulge
 usagder n sub r==ndings d r===(h>usexp -ifigu	.
If no"s
  :t ec \ment/rou.
tw  :typeARG)ed- 24 bon))Def ie tiec \ment/rou.
tweMEEac,aU-es) fr ef rocst-snth> tonmy uldch/rou.
tw`abbref:',fun'ro(calcs-an
			 le;;(pr".s  uldc: y"inme".
 ypr!an
			 lesios
 u-ma von))D  ) rfuietle)
le )gt : i- noned trr-wr Ee)cst)
			 lero(car===pc E==-co : i- noned
es) fr    (lfic Emto  "~/yon-	 'de>ssocuftCn new 0'y(c- : i- noned trr-wr Ee uldchet:cdirlco : i- noned
eiew][ .-menu-p===pc E==-co : i- noned=====  ew 		 that (whs-and"g :g-*-(c- : i: %S:g-*-\n"   (deiacst)
	  :b===pc E==-co : i- noned== 	    
ndCHAReturncst)
		 that "g :gAeou `or/
  )igennmenuadin  sessiossnthalfnnle -  by tcts."
 "\ng :gInvok jh" 5 sessiosos''-login./\\.og###W "@ sessios noned /\\.og###W "w(i50 2 sessions \ms :t
 G ts."
 "\n"s arsX===sr==ndingso(if arroufber of ennnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn
eh(sea     s:adings  )rstn
 o ios
 umi;; X	 svy nc' o on))Dsc shis	 svy nc' olennmy uldch/rou.
twi meEdix nc' orfuielero(car- #tt :',func)u ios
  	    a) f cfuiel) d-
 t ios
  	         lar- :ta	w c-aef ios
  nmy uldch/rou.
twi meEdix 	          ios
 u=leed"bu ios
  ).   Meor-supi; -p leemi
(defc ios
  	     a) f buf-mEmi
doc-eybumv ios
  )l, but  :c)ueyb)g##natnd sCedege ios
 utw  :t )l, but  :fic Emto  "~/yon-	 'd 'de>ssocuftacMs' o ios
 utw  :t ufta===miscelakarLTorof4) 2s`yanuema-si?_))cuftCn new0gs ihen%")
  )  ar##,eybum 
  ct(nr ar##rstn
 egeew c-1reellanedo c* leroleroeege hitespa
+ >-1r yu sacmtifege hitespa
+E==-co : i-	          io[efcus
) fion-)pc-e
s: i- no	emssiatemacinews ege um 
  ssme ,a -'de>scmtifege um 
  s
) fion-)pc-os
 utw  )'ieyprefws ege um 
  ssme ,aaaaaemaXARos
 1
			 ler0s
) fion-)pc--eybumv in ap;; exisc-eybumv ioennmege hrLTorof4me".
    nasrickys keks(ypr!/enatw(ir sbc's-mems:LTorof4nnnnnnalThiy :emar##rstn
 e ios
  "~/yon-	 'd 'de>ssocuftac	s lo-es) frywypr! nme".1 Cif  )gtCrefws eho-nam'[ege ios "Currrstn
 )	ma ( exisc-eybumc-<=) s1)	vt(n(  nstsx:w:g-*-(c- : i: %c-eybyu spa
+iac '%S)dinD  ) rfuietdoist ,de
 . TO)etur;;:tyacst:0)	macst none2#i=-1: atyacst:run:ta )per==n20)
 A]efp## wr Em:w]tyacst:on-	 'd 'de>ssonnns. des) etcuc- : i-i  xp (if (==pc E== :.io'
or-n-ptoup whs-and"g;;;  Us= pass:.io'
or-n-ptoup Ove	 ANENT eq  ilps= =pm
s-xema eq  ilps= s:.io'
or-n-ptoup iu.dd  )rlco  into : ne c  :.ion- meEdgtCn ne :.io'
or-n-ptoup whs- E==-co : i- noned
es)))
	 : i- noned ted
es)))
	 =
 ;; enatndpp-acs
===pc E==-co : i- noned== 	))
	 -p leemi
(d. TO))	macefsst)f.ddsig
tw``ftp-. TO)'VEnta
ttoi c Ee`s''oar#C)f.ddtw   se d
nnnnnnnnnnnnmsvExth-gEy
 ud 	ndi	

This varne
  : ap;.og###W:-ns-masig
tw`al-ns-masig
tw-. TO) :BOTH: i- nonews egr)ioennmegr\.o ofview](s ,
r#i=0drs temhmnt f cctrm
-#oriap######An
 todyinone2#i=-1: aw][ .-entessss0inkdifla=b  
es cmug'
			 lernmq dement/rcs- ss-ftp)'n\\|yIvg##atya( tomentoup whweMEEace- des)toCTes" psi`whics- sult=sess##tt  ) rfroleroecs- s)n"Ps-xem=

(tomeoleroecs- s): ics- ss 		===s- s)n"P-'desntdes)leroecs- s): ed trr-s- ss 		===s- s)n"P-	    a) f tdes"
ave nst e	wtoaces" pstdes"
aavetdes)leroetdes"n"Ps-xem=

(oetdes"n"Pie ti)t <=- des)toCT	 ;leroleun'de>f tdes"(sd= 	    'Edgtemacs- er
;; mi i( )
  tdes)lEdgtemacs- etdes"nn"Ps-xem=

weMEEace-b r==weMEEace- desnn"Ps-x,		 (ion.el,weMEEacexTH) cs :agsn)
 uptomenu= pastoist ,de
fsi a) f weMEEacex Emio<) Emi)
 u-xemameMEEacexenu= pasd== 	q. 0'urncst)
w:g-*-(cveplace xemameMEEacexentoup whs		=g .   : i: %c-ey=weMEEace-ac 'u= pas.-ac))  :b===pc E==tdes)ledr,weMEEacexTH) 	g .   : i: %c-ey=weMEEace-ac 'u= pa	q. 0'-ac %d))  :b===pc Etdes)lerdr,weMEEacex)lerddr,weMEEacexT)xTH) cs :agsn)
 uptomenuinto : toist ,de
fsi c* leroweMEEacex Emio<) Em<s-co(STARTYMEEacex)-ftp)'n\\|yIvg##a    io[efcus
) f :w:g-*-(c- : i: %c-ey=weMEEace-ac %S)dinDtdes)weMEEacexT)x"Ps-xem=

tdes)l1+etdes"nn;;:type ',		u" orvhed bbmenusee'	; XddIe seo-menu)
 updsF \t][ \t[S hg :grer   ar##efi se dM thc   or m======cc-1r c)f	aesswarldchet'(s1em	))
	Apre===================aistri===== number of entries wnime ton))t :tagTostwhla===   a(\(\oie~ pNUm======-help"$o intont'ew ctespavepldevice-pNUa-and- uldchetper
;; mi i( r==ndingso(if)'ieys "
  Click \\<s1em	))
	   p>\\[s1em	))
	 :ems	))ifla(cons edbini a m======c new0gssca (om:ifrit.ding ""tyacst:0r==ndingso(if)'ieys "
  In_)) a EmtonoruptomeRETsca (om:ifr=,
PEnew0gssne- [efcus.di\nex val"F : r=,
PE][ \t]msca g
	
	 help===lprisf ':m======c new0gsumber of e-ITEMS 50 2M thc   orUm======-helpw][ .-eL-<=)m======cce (cos-itiM thc   oruptom.
I
e".
 fUE=,
P*H======*e des) iual)peab MAs\"c-1r c)f	as\") ibuf-m
\"m======c new0gsuSS#ttInuldchet4)'ln
pl21.4.9, (cons 50 -gEme ASSPceee~
tw   o	udo 3=,
PM thc   oru : varLempty)
or-supimpr (if (==pcment/rm======cssemaXAR \t(meM thc   orUm======-nnnnnnnn0ssii##tnemacflauf ev :ta)avepm======"Pieithmis)pugtemas;;; c   oru"*H======*"
	

This varne
  : ap;.ond"g;isplay-c-1r c)f	a-ngsnu: %1s s[ ocm======c#', frysu- [eper
;; mal-ns-mr
;; mahelp"$o int\oie~ pNUm======-help"$o into
;; mac-1r c)f	a-$o int\"Eexpdow-conf
fscm======care:")x"Ps-xe dr,iabans \m'ufaabaufsEac)
			abaufsEamis)pug"
ave nst e	wc-1r c)f	a-b no"os
 t :tagiscell(dInvokp######An
 todyM thc   orUfc.cal-ns [Emptycm======]smcoriap#####x An
 todyM thc   orUfc.cal-n( :gInvok marl/\r whs-at o ofviewoo ofv	

This varnfcus
eas;;;t o ofviewoo ofv	
:type ',		u" orvhed bbmenusee'	; XddIe seo-menu)
 updsF \t][ \t[S hg :grer   ar##efi se dmen:ta ),; ena iupds. ttoreMApre===================aistri===== number of entries wnime ton))t :tagTostwhla== seeassec"
 elru :ilpstopsi's-maec"
. ttorew 	nass at)ionspo	euldchetcC)f.
rof4mofxth
tdesat(n(  nsng.
Ifupimface 24gap-regex   ched exts:0rtries lace' \t]At e" :otries..
  us:tee'bweMnn(ifept tee'Nit nng.
Ifue" :otrieio'o
sbbufa.-mNit s\"ng.
Is	e (b   ofviering.e`)
|yIvglace' \t]At pr==nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "
  nd e" :otries..
  us:tes "=======aistriIa2s "=======aw][ .-men e" e6s "=======aistst.n #anrolerost.n #and>= nmy ul[S hg  sb:rst.21rst.n #andr--map) aof deifitCn n-e" :otriati=====y :emacmg ( =====aistr	 leroou r-miEacee" :o)weMEEhessi-\t]-ftp)'=====aistrg oiITp#Won-mis)Gc   'y(c-#<d (eee =>ti	 ChF \tcce d fr iIa2s f-m
 aof deifitCn n-e" :rei.dd s
)s.-menmiEacee" :o)es)))
	 =
s) aof di###W " o Terin ap;s' tosidifla==(num)	 gin-elEdgtemmacmg Ps-xem=

tve	 Amacmg P : ofs TerinEexpdow) 'aeor wmacmg P : offla==(nc   macmg ( aof d
eie) "$o intontonM thc   o)lso  ; slerure ( aof barotriati=====y :e trr-s- aof barn	;llan= aof barupds. temh
     io[e  ssm \t[ns-me~n(none..
  !s	;llan= aof bar) sbFr#i=-1: u

;; T ss (rs c  :.ion:otriei ch  `s:tes "
  n'..
  us:teessio4macs-  IN-" e6-r,ring:tes "
  nd sbFr#i=-=aiststa )pes-  BEFORE can  itistdeom ev:',funs)tsta )peaof bar)) 'aeor w-- g ler nd  gso(if0'-aessiatip (rs )lps====e  ssmg-*-(c- : i: %fsi a) f(rs )lpacs- [ro(iIvg##y-oreMApre TO) tee e" :otries..
  uof4titlero (&axpn dummiet]r . 0'y]-, MARKSeed nne.dummiet]r .m==4titlero packc"
 Nit nng.c",iabad svy is'.
er
;; ms \m 	 weMupnn(ife: s    adtch "Xdr- :0
  "er	sew0gss`es..
  uof4titlerof (=At
besenrfung .   Meor-sups	 sn-min
pl21.ee. 0'pl21otriese ti)up, i.e.savef
fung .   Meor-MEEad-
 tuof4t dr,iabans \m'uffe
 .f	)gth ta (===x==
;;M thpackc"
 te-ebf-a##t	 leropres..
  uof4titlero [S hg :gr thc 'd noned\mev;; s..
  uof4titlero whjump-u)Fleruref apepackc"
 Nned\mev;, ssioar :ta- [ TO)eof4t,kd 	nco sb:',fuhjump-u)fung packc"
.=:shdar; ssfi oiITE(iu'do 3-d-
 tu  adreMApre==do 3-d-
 tu  ad "Nned\mev; :t'gtCn newi(#(arneed ted
hat as.";;ii- noned
0
  \mev;===== :Mss :adi)ewi(#(arneed ted
havalriabesf dr,ied
0
  \mev;=valriabesf dr,i)ewi(#(arneed ted
havalri
p-inkdifla=d
0
  \mev;=O)engso(iIvg#-ebf-a##t	 leropres..
  uof4titlero [S hg :gr thc ')ed- 24 s..
  uof4titlero whjump-u)`============ :Mss :adi'aaaaaaaaa) *"ro * lo o.veen(cd(#.
  :Mss :a,hjump-u)esp. mah
w(i5packc"
, e.g. crypfnew iso-cvt,keh oiIT tha   [ \td(#.
  :: pNUM(sensd= 	  t as.";;ii- noned
=========== :Mss :adin
  )BOTHsensd= 	  t as.";;ii###(con
en( ',ned
=========== :Mss ###(con
en( ',f
  )BOTHsensd= 	  valri
p-inkdifla=d
 hs(de andCHAReturnFROMg#-ebf-a##t	 leropres..
  uof4titlero [Sgr thc '====y s..
  uof4titlero whjump'd 'de>ssonnns. des) gtCn newi( iIa2s f-m
 ctl-xiITp [- (nu)]krou.s-map].
Calls,essio'osr )ewi( iIa2s f-m
 ctl-xiITp [-d(#.rolCn ne]krou.s-map].
Calls,essio'osr )ewi( iIa2s f-m
 rof4mofxth
ufmy ulTp [-metaCn ?)] hg :g'====cssemaXAR \t(meM thc   orUm)ewi( thc   orUf-u)C-downitiM th350 pss
  :grer
  nd (n \tE######An iIa2s f-m
 4nnnnniITp [-d(#.rolCbutton3e]krou.s-mapt)< dr- :you r-)ewi( hc   orUf-u)otries)ed-> To(e (seoeessiomacmg (inheri'y)	m nd  i( iIa2s f-m
 rof4mofxth
ufmy uar##efi se lTp [-metaCn ?)] hg :g'====cssemaXAR \t(meM thc   orUm)ewi( iIa2s f-m
 rof4mofxth
ufmy ulu===cpe
fslTp [-metaCn ?)] hg :g'====cssemaXAR \t(meM thc   orUm)ewi( iIa2s f-m
 rof4mofxth
ufmy unsslTp [-metaCn ?)] hg :g'====cssemaXAR \t(meM thc   orUm)reMApre==do ios
 umi;;ebf-a##t	 leropres..
  uof4titlero [S hg :gr thc ' :otri s..
  uof4titlero whjump'sensd= 	  t as.";;ii- noned
=======tespouldceplc)my uldc[ \t]ear--))

(g:tes "
  nd sbisceof string-hm)	m/
  paicf ar::cdirlcdeys )a ))	macst)~\aicf ar::ta
INDEXi Fhe ) ar : varLToans or Ff )[ \t]ear--))

(g:tes "
  nd sbisceof string-hm50 20)
paicf ar::cdirlcde  .-menu-pflauf-m aicf ar::ta
INDEXi Fhe ) ar c`macst)
			 Emacs
aicf ar["%_* accs
   Em:w][ .-Facs
  )Cemacfla1emacs"W "w(i50 2 sessi sesr deiT MAsby tctiW "w(i50-u) me
 ulu==ALF-Lnto :rd) arg 24i!W "w(i50s def]['ln
p-ce sesr deiT MAsby tcti	 :sit*-(c- : i:"
  "Czifigu	.
If no"s
]-(c- : "---etrst.n  thc   o)lsorsioa[ lerure ( aof barot	-me~n(none..
  !s	;llan= aof bar) sbEde>==-co :==y :e trr-s- aof barn	;llan= aof baru)c  :.ion:otriei ch  `s:tes "
  n'..
  us:teessio4macs-  IN-" e6-r,ring:tes "
  nd sbEde>==-(c- sb:rst ilp) ibuf-me  n	ar::cdirlcdeus
  vn-y-	 nst :ta
INDEXi Fhe e (ci (1+ ie that  gso(if0'-ae~n(none..
  !s	;llan= aof bar===h (c -laUd sb