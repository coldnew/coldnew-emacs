
;;; completion-ui.el --- in-buffer completion user interface


;; Copyright (C) 2006-2010 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.11.12
;; Keywords: completion, ui, user interface
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; Overview
;; ========
;;
;; The goal of Completion-UI is to be the swiss-army knife of in-buffer
;; completion user-interfaces. It doesn't do the work of finding the
;; completions itself. Instead, anything that can find completions, be it a
;; built-in Emacs function or an external Elisp package, can be hooked up to
;; Completion-UI to provide a source of completions.
;;
;; Completion-UI comes with built-in support for a number of completion
;; sources: the standard Emacs dabbrevs, etags, Elisp completion and file-name
;; completion, as well as (if installed) CEDET's Semantic completion,
;; nxml-mode, and the Predictive completion package.
;;
;; Completion-UI provides the following user-interfaces and features (though
;; it is also easy to add new ones, see below):
;;
;; * Dynamic completion
;;     provisionally insert the first available completion candidate into the
;;     buffer
;;
;; * Completion hotkeys
;;     single-key selection of a completion candidate
;;
;; * Cycling
;;     cycle through completion candidates.
;;
;; * Tab-completion
;;     "traditional" expansion to longest common substring
;;
;; * Echo
;;     display a list of completion candidates in the echo-area
;;
;; * Tooltip
;;     display a list of completion candidates in a tool-tip located below the
;;     point, from which completions can be selected
;;
;; * Pop-up frame
;;     display a list of completion candidates in a pop-up frame located below
;;     the point, which can be toggled between displaying some or all
;;     completions, and from which completions can be selected
;;
;; * Completion menu
;;     allow completion candidates to be selected from a drop-down menu
;;     located below the point
;;
;; * Completion browser
;;     browse through all possible completion candidates in a hierarchical
;;     deck-of-cards menu located below the point
;;
;; * `auto-completion-mode'
;;     automatically complete words as you type
;;
;;
;;
;; For Emacs users:
;; ================
;;
;; INSTALLING
;; ----------
;; To install this package, save this and all the other accompanying
;; "completion-ui-*.el" files to a directory in your `load-path' (you can view
;; the current `load-path' using "C-h v load-path" within Emacs), then add the
;; following line to your .emacs startup file:
;;
;;    (require 'completion-ui)
;;
;;
;; USING
;; -----
;; For each source of completions, Completion-UI provides an interactive
;; command that completes the word next to the point using that source, e.g:
;; `complete-dabbrev' to complete using dabbrevs, `complete-etags' to complete
;; using etags, `complete-elisp' to complete Elisp symbols, `complete-files'
;; to complete file names, `complete-semantic' to use Semantic completion,
;; `complete-nxml' and `complete-predictive' to use nxml-mode and
;; predictive-mode completion, respectively.
;;
;; The `complete-<name>' commands are not bound to any key by default. As with
;; any Emacs command, you can run them via "M-x complete-<name>", or you can
;; bind them to keys, either globally or in a minor-mode keymap. E.g. to
;; globally bind "M-/" to `complete-dabbrev', you would put the following line
;; in your .emacs file:
;;
;;   (global-set-key [?\M-/] 'complete-dabbrev)
;;
;; To bind "M-<tab>" to `complete-elisp' in `emacs-lisp-mode', you would bind
;; the command in the `emacs-lisp-mode-map' keymap:
;;
;;   (define-key emacs-lisp-mode-map [?\M-\t] 'complete-elisp)
;;
;; You're free to bind the `complete-<name>' commands to any keys of your
;; choosing, though "M-<tab>" or "M-/" fit best with the default Completion-UI
;; key bindings that are enabled when you're completing a word. These are:
;;
;; M-<tab>  M-/
;;   Cycle through completions.
;;
;; M-S-<tab>  M-?
;;   Cycle backwards through completions.
;;
;; C-<ret>
;;   Accept the current completion.
;;
;; C-<del>
;;    Reject the current completion.
;;
;; <tab>
;;    Traditional tab-completion, i.e. insert longest common substring.
;;
;; C-<tab>
;;    Accept current completion and re-complete the resulting word.
;;
;; S-<down>
;;    Display the completion tooltip (then use <up> and <down> to cycle).
;;
;; M-<down>
;;    Display the completion menu.
;;
;; C-<down>
;;    Display the completion pop-up frame.
;;
;; S-<up> C-<up> M-<up> (in pop-up frame)
;;    Dismiss the completion pop-up frame.
;;
;; M-/ (in pop-up frame)
;;    Toggle between displaying all completions.
;;
;;
;; Completion-UI also provides a minor-mode, `auto-completion-mode', which
;; automatically completes words as you type them using any one of the
;; completion sources. You can select the source to use for
;; `auto-completion-mode' by customizing `auto-completion-source' (the default
;; is dabbrev).
;;
;; To enable and disable `auto-completion-mode', use:
;;
;;   M-x auto-completion-mode
;;
;; Note that `auto-completion-mode' is not very useful if the completion
;; source takes a long time to find completions.
;;
;; <shameless plug>
;; The Predictive completion package (available separately from the above URL)
;; is designed from the ground up to be extremely fast, even when a very large
;; number of completion candidates are available. As you type, it also learns
;; to predict which completion is the most likely. So it is particularly
;; suited to being used as the `auto-completion-mode' source.
;; </shameless plug>
;;
;;
;; CUSTOMIZING
;; -----------
;; The completion user-interfaces can be heavily customized and tweaked to
;; suit your every desire, via the `completion-ui' customization group, (and
;; subgroups thereof):
;;
;;   M-x customize-group <ret> completion-ui <ret>
;;
;; All the customization options and settings are well documented via the
;; usual built-in Emacs documentationn features.
;;
;;
;;
;; For Elisp coders:
;; =================
;;
;; In fact, Completion-UI is even better than a swiss-army knife, because it's
;; also extensible: it's easy to add new user-interfaces, as well as new
;; completion sources.
;;
;; The philosophy of Completion-UI is that customization of the user-interface
;; should be left up to USERS. They know what they want better than you do!
;; By providing a universal user-interface that can be used by all completion
;; packages, Completion-UI lets users customize their in-buffer completion
;; user interface once-and-for-all to suit their tastes, rather than having to
;; learn how to customize each new package separately.
;;
;;
;; Adding new sources
;; ------------------
;; See `completion-ui-register-source'.
;;
;; One call to `completion-ui-register-source' is all there is to it!
;; Registering a new source will define a new interactive command,
;; `complete-<name>' (where <name> is the supplied source name) which
;; completes whatever is at the point using the new completion source. It will
;; also add the new source to the list of choices for the
;; `auto-completion-source' customization option (unless this is supressed).
;;
;;
;; Adding new interfaces
;; ---------------------
;; See `completion-ui-register-interface'.
;;
;; A number of Completion-UI functions are intended for use in creating new
;; user-interfaces. These all start with the prefix `completion-ui-' (as
;; opposed to `completion-', which are user commands plus a few general
;; purpose utility functions, or `completion--', which are internal functions
;; that are NOT intended for external use).



;;; Change Log:
;;
;; Version 0.11.12
;; * modified `completion-ui-resolve-old' to take optional arguments
;;   limiting region in which to resolve completions
;; * added new `completion-resolve-before-undo' to `before-change-functions'
;;   hook in `auto-completion-mode' to resolve completions in text region
;;   modified by an undo, since undo screws up the completion overlay
;;
;; Version 0.11.11
;; * bug-fix in `completion-reject': used to run `completion-accept-functions'
;;   instead of `completion-reject-functions'!
;; * pass nil instead of empty string to `read-key-sequence' in
;;   `completion--run-if-condition'
;;
;; Version 0.11.10
;; * fall back to global `auto-completion-override-alist' if character not
;;   specified in overlay-local version, in `auto-completion-lookup-behaviour'
;; * fixed `completion-cycle' to cope gracefully with null argument
;;
;; Version 0.11.9
;; * added C-@ binding (produced by C-<space> in terminals)
;; * fixed compile warning in `completion-ui-register-source'
;; * fixed interactive spec in `completion-cycle' and
;;   `completion-cycle-backwards'
;;
;; Version 0.11.8
;; * added M-\ word-constituent binding
;;
;; Version 0.11.7
;; * bug-fixes in `auto-completion-self-insert' and
;;   `completion-backward-delete'
;;
;; Version 0.11.6
;; * added `completion-fill-paragraph', plus key bindings to override
;;   standard `fill-paragraph'
;; * bug-fix in `complete-in-buffer'
;;
;; Version 0.11.5
;; * bug-fix in `completion-ui-source-non-prefix-completion'
;; * changed `auto-completion-self-insert' to always reject if called due to
;;   `completion-auto-update' rather than `auto-completion-mode'
;; * added `completion-auto-update-self-insert' and
;;   `completion-auto-update-overlay-map', and separated
;;   `completion-auto-update' code from `auto-completion-mode' code
;;
;; Version 0.11.4
;; * added `completion-auto-update' customization option
;;
;; Version 0.11.3
;; * bug-fix to `completion-ui-register-interface' :auto-show-helper
;;
;; Version 0.11.2
;; * bug-fixes to cope with elements of completions list that are cons cells
;; * bug-fix in `completion-ui-source-word-thing'
;; * bug-fix in `completion-ui-resolve-old'
;;
;; Version 0.11.1
;; * allow indirection in source :prefix-function, :word-thing,
;;   :tooltip-function, :popup-frame-function, :menu-function and
;;   :browser-function; if they are set to a symbol, that symbol is repeatedly
;;   evaluated until we get a function (or a thing-at-point symbol in the case
;;   of :word-thing)
;; * bug-fixes to `posn-at-point' functions
;; * allow literal prefix string to be passed to `complete-in-buffer'
;;
;; Version 0.11
;; * Major rewrite: completely modularized the user-interfaces and completion
;;   sources!
;; * split `completion-resolve-behaviour' into
;;   `completion-accept-or-reject-by-default' and
;;   `completion-hot-to-resolve-old-completions'
;; * `completion-ui-register-source' settings replace the
;;   `completion-function', `completion-prefix-function' etc. variables
;; * added `auto-completion-source' customization variable
;; * Rationalised function and variable names: `completion-*' are intended for
;;   users (apart from the odd general utility function), `auto-completion-*'
;;   are related to `auto-completion-mode', `completion-ui-*' are related to
;;   user-interface implementation, `completion--*' are intended for internal
;;   use only.
;; * generalised `completion-scoot-ahead' into `completion-extend-prefix'
;;
;; Version 0.10.2
;; * bug-fixes to `completion-replaces-prefix' support (thanks once again to
;;   Henry Weller for reporting them)
;;
;; Version 0.10.1
;; * bug-fixes to `complete-dynamic' relating to prefixes whose size has
;;   changed
;; * refactored recurring code into `completion-highlight-common-substring'
;;   and `completion-hightlight-prefix-alterations' functions
;; * always bind `completion-self-insert' in `completion-map', so that old
;;   completions get resolved
;;
;; Version 0.10
;; * removed `completion-includes-prefix' flag; completion functions now have
;;   to return entire completion, including prefix
;; * prefix is now deleted along with provisional completion when a completion
;;   accepted, and the entire completion is inserted; this allows completions
;;   to modify the prefix when they're accepted
;; * completions can now include data about length of prefix, in case it is
;;   not the same length as the original
;; * `complete-in-buffer' and `complete-word-at-point' can now take arguments
;;   that override the global values for `completion-function',
;;   `completion-prefix-function' and `completion-replaces-prefix'
;; * `completion-construct-menu', `completion-construct-browser-menu' and the
;;   other browser functions now take completion-function, prefix-function and
;;   completion-replaces-prefix arguments
;; * null values for `completion-prefix-function', `completion-menu',
;;   `completion-browser-menu-function', `completion-tooltip-function' and
;;   `completion-popup-frame-function' now mean use the default
;; * only position popup frame in `completion-popup-frame' *after* setting
;;   it's size, otherwise some window managers won't position it where we want
;;
;; Version 0.9.4
;; * modified `completion-run-if-condition' and `completion-select' to get key
;;   sequence used to invoke it via `unread-command-keys' and
;;   `read-key-sequence', to ensure key sequence translation takes place
;; * added new 'pop-up setting for `completion-use-hotkeys' which only enables
;;   hotkeys when a tooltip or pop-up frame is active (thanks to Henry Weller
;;   for the suggestion)
;; * attempted to fix bug preventing default `completion-tooltip-face' being
;;   set correctly
;; * fixed bugs in `completion-browser-sub-menu' and
;;   `completion-browser-menu-iterm'
;; * made most anonymous lambda bindings into names functions (lambdas just
;;   confuse people, and make it hard to bind other keys to the same thing)
;; * bug-fix to `completion-select' which triggered infinite recursion trap
;;
;; Version 0.9.3
;; * added 'accept-common option to `completion-resolve-behaviour'
;;   (thanks to Henry Weller for the patch)
;; * other code refactorings (thanks to Henry Weller again)
;;
;; Version 0.9.2
;; * define hotkey bindings on the fly in `completion-setup-overlay', getting
;;   rid of `completion-hotkey-map' entirely
;; * `completion-hotkey-list' can revert to being a customization option
;; * remove `completion-cancel-tooltip' from `after-change-functions', since
;;   this prevents tooltip being displayed when `flyspell-mode' is enabled
;;   (it's in `before-command-hook' anyway, which should be enough)
;; * use `run-with-timer' instead of `run-with-idle-timer' in
;;   `completion-auto-show', as it seems to make more sense
;; * move backspace and delete bindings to `completion-dynamic-map' and
;;   `auto-completion-map'
;; * added `completion-browser-recurse-on-completions' variable to control
;;   whether the browser lists completions of completions (of completions
;;   of...)
;; * replace `x-popup-menu' with newer `popup-menu' in `completion-show-menu'
;;
;; Version 0.9.1
;; * use :family attribute of `completion-tooltip-face' to set tooltip font
;;   (thanks to Andy Stewart for the patch)
;;
;; Version 0.9
;; * added `completion-includes-prefix' variable to indicate that completions
;;   returned by `completion-function' include  flag; comr new `completion-resor `completion ;; contents
   by `comons
;;   ra custom'ew generalcEclude  omer' instead of `ruumactrol
;;   om'ew ontrol
;;   tm,on-ui-register-sons, etieunct `completion-selecstead of `ruumactroen dispindow manp't
  ;hey'reiste)`completion;;   e-funenu, unless
;;   it'sns.
ngestch onlomonscngle-kcompletimpletion-self-inse;;    Dismiss t  returned by `completiotion]
      (list-elisp)
;oltip font
;;   (8rings (toltip' d by `completiotion]a customizatioetil). Ps-up

If MENUb9.3ute; if the running
n-selecsteadMredin Pohln--refix' support (tte)`complet position it where tion]a customia `unread-:family issage "Crame layedibute ore)ame.
;;
;; "comppletion-p cotion]atomi"maga mino"letios menu entry it et a funn)
;;ss t!n-selecsteadMredin Pohln--refix' support (tte)`complrned bon trap
;;anged
;; ;; <tab>
;;    T   to reetiodit is also easy to add -selecsteadVagn Joele;; or pop-up frame is active (ctmake" timerusow-menu'
;;
;; Ve', `completion-tooltipde  flag!ult `completionsolve-ed witltion-extend-prefix'etion-accept-fud `comp-popunt' fn-cancewon, prefix-way, which sompee -selecsteadVagn Joele;; or po' supportn-cancel-tooltip font
;;   (8,1ult `comp more sense
;;ommand the syntax-ems to y
;;ions pletiion 0.11.6
;; use `run-delete bindings ; * move backsrun-wition ult `comp more sense
;;; * define hoe-funsage nting de(num (le-fu'th pref `runnu, ey-(b nepenone.

nowser-recuse:
;;
;;   M-x auto-compth the doto globaip font
;;   (8ompletiind completionsefix-funal compleow '-advcomplnu, ey-(b Completlay)mpletlaynum (lenu, ey-(uffer  compzero- acceptefix-fuvide a  compleif they '-advcompl completting22!rns
rce' (thffer-aparatees: `coatdidn't
  ;etion.
;;
ont
;;   (7; * bug-rned by `completiotim mento-update-ems to m))



;; Notn you der' insvides a minor-pletion 0.11.6
;; * adtim mentsefix-funnum (le11.6
;; lementate name) on it where we wantew
;; define hoehn--on.
;;
ont
;;   (7;4rized the user-interfacex-alterations' u enwo:tomat the
;;    using e `run-with-timer'x-alteration, function toxtend-prefix'ed to stuf ;; sr' insvid;;
;;   M-x auto(at you the ,--------
;,e `run-delete bindings x-alteration, fngthdelete bindings o `compl layed. Licenaccepte1.6
viduug-fi
  h; * Cl back ts and `compsr' insvid;;
;;   M-xe that itsvid;;
;;   M-x auto(r-chanth the y)mpletpletion-ui-source-non-prefix-ceturn ep-popion-ui glotively.f `runnu
  h; * Cl back ts eetielete bindings to behaviofix-funnum (lern e11.6s-fi
  h; * Cl back ts and using e `run-with-timer'x-alteration string h' (asoffcompletionletion-ui-source-non-prefix-cefnsvid;;
;;   M-xeompth the ooltip font
;;   (7ch triggompletionsolvnu'
;;
;; Ve', `completiup fraotion]acc' triggompletionsolvct-mentionsemenu'
;;
;; Version 0.9.1
;oltip font
;;   (7cings (toltip' se it is
;;   not the enu
;;vid;ame.
;;
;; ntry iion 0/list/   Disltip orh only))))
		(cdr (funcall cmpl-functioion-pettingCVSwith-idle-crash! and make  ler for rn 0.11.6
;; *ion 0.9.4
;; * mthe currente'and make  ler for r   Dismiss tsoltip font
;;   (7c1ult `cl comon 0.11.6
;; ler fin case it is
;;   not the ; * `complete-n1.12
;; * unction, prefix* added `completiosvides a minorde11.9
;edons
;;   retu-with-timer'x-alteration ; * `complete-n1.12
;; * , prefix* n you menupletionletion-ui-source-yntax-ion 0.11.10
;; *-at-poge ng tf `ruu der' inmore sense
;;ommand the e-backwards-ems to yand make  ler for r-with-timer'x-alterationrized tnu-fue dotdn-ui-remenletion-ui-sourc[-ion 0.11]e-yntax-10
;; *,
;; pr`compl n--run-ns (of comclocomp addl dotdn-ui-oltip font
;;   (7`complet positic in
e it is
;;   not thesp symbols,  the original
es for `completion-o-completion-mod * bug-fixes in `auto-completg'
;;   and `c compleg)
;ide the global values for `col Elis eynor 0.11. is s, etieunct f;;
;; Verons
pletidr the suggestion)
;; 
;;
;; Velay-get complet positise pec in
ntrol
;;  ys bind ;
;;   M-xe * added eompmyedibute    old
;edida to uiminorefix is l back toa completioiew
;; taeunct `completionified by aetu-with-timer'ifie `com'bon trapnons cpletisowser-recther the browser lfie-al Publtab>
:

 d; See -UI provi supplied souoto globapleti-ited * bug-fixes ; * move*-yntax-10
;; *ompletionletion-ui-source*-yntax-10
;; * genet positicomplenk my -ns (name;`compl l added enons cust' funmpletio   om'ew ottirots
;;   limiting ys' which onl`completion (ofod -selecsteadd
;;.again)
`complettingwikior pop-up frame is active (keys
 :de; * `commpletionomatiutSee -UI  iion 0,l`completion (ld prob otkeys' which ony aetu-with-timer'rsion 0.com hoemod * bug-fixes 3
;; * buglist-e<ret> c`compleubsum	 overla-with-timer' instead o-mode'
;; * added `completi* bugcom hoy aetuRET.11.6
;; `com' to ussustomize-group <ret> coltip font
;;   (6; * bug-fix in es and `rning in `ct-mentions`completioenet poscgroup esolvquence ta `und
;; coltion-auto-update' 6;4rized ;ommandenu, ey-ieidle-timercomple-  buffer
 auto(ffer
ting default d -selecsteadSide amor podrawhat `im(than trap
;;(tte)`comple comoimnclueefix* adk in ge-functio      ;; notee-on-completion lt d lbars,
;; prion-auto-update' 6;h triggompletM; Version11.6
;; *ys bind unction, prefix*to prt' funce'
;; * added `comlay
;;
gled bempletC; Version11.6
;; ion-auto-update' 6;external use).


;; choosing* bug-fixes ; * move-yntax-10
;; *omles an `runn
ngesintesl l adetions,ptrolua-get comple comoke it hard to bind other buglistwantew
;; define ho triggomplet`completion-o-completion-mto `cys bind s sifer  hanged `aume>' com ind coletions l b (pletiagrace' iscludffer
iNotn you se) triggomplet`complet them)
;;
;ys bind region
;;   modified bow 'cowhiuse `runltab>oices onten is designet poscgroup esolsou mendk in -at-pibute   pletioid untilpackageduinternssuexecu-get co-auto-update' 6;1xternal use).

;ommandyou can
;; usratinis rvid;;
;;   M-x auto(-atffer
iNprob otldthin Emacidn't
   triggomplet`completion-o-completion-ys bind pletiagracefer  mto ` triggifelisp)
;b>
:
mpletiiife o0.10.2
 tf `thanks* adtim mentsonsmyedibute eckause nxmor pode;   modlisp)
; ion-auto-update' 6* bug-rned by `completio
;;
;; Vion 0.9.4
;; * mt customimpletion- omer' instg)
;ion 0.1ui-remechoosing (ofodsor pode11.9
;ions now hi surarchicag code prefix'
;ce ta amily k in triggompletionseneral utilispec in `-de;   mod-selecsteadMrciejicag cKatafiaszor porarch;;     thee -UIsatiut)`complrned bon tra * , prer
;;
;r r-with-timer'x-alterationcompleg)
pletiosvides a minorde11.9
;edo l back tonsertyntax*-at-poion 0.1den,unctions nopletidrn 0.11.6
;; * adn.
;;
;Cl back ts aauto-c e-backwardssxternal use).

* bug-fixes in `auto-complet,nopletidrtions for ortn-cancmore sense
;;ompletg'
;;   a,w', as mmandea* bul to n sourde;   moicag code kiiding p)
;;
; bug-fixored kwatr-interilt-in suppores a`complrned be-bis ikeys'elay-get cominmore sense
;;ommand the e-backwards-ems to yor podef
;ions11.6
;; lement adn.
;;
;Cl back ts aauto-c-e-backwardssxteion-auto-update' 5;externaomplet:family ade', ssu
;; tring at compdodef
;ionsaing e `run-with-timer'rsion 0.9.1
;oltiggith-tl the c should (ofod ed
;;;   itce ta amily,w'vo.1ui-retil).e `runltabPs-up
-selecsteadNikolaj Schu Emhmpletion-is!ooltip font
;;   (5.1xternaomplet   (stionsolvnu'
;;
;; Vex-alteration sselecsteadNikolaje `runSchu Emhmpletiorarch;;  onsiut)`coip font
;;   (5ip fMt poscgroupr-inite naenu
;w manp't
  ;on torms docud
;;   completions get;ommandyou can
;;'ottiro;f the
;;letions getUIes for `completion-es for `coetion ;ng otb :menunenu, unlesy', gettsvid;;
;;   M-xeompnons rt' and
;le comoto-comppleti-ited mple `completion- ed be
;;   and `ip font
;;   (4.1xterna   (stiotgith.2
e
;; e it hard to bind othe custo;oltip font
;;   (e from `insert)
;b>
; * cesolsonons ed `aume>'cewontion-pee -in-buffer' anday, which ses :nts
   by `,-----behauto-c (`auto-coer-inwaspunt' fn-cancoltie `cly)t)
;b
;;
;; ion is `rning in `unction, prefix.`completioen   thd
;;   `cong p)
;;
-menu-itedontrol
;;   tmsaentizdibute  `co11.6
;; ion-auto-update' 3.1h triggTri utiliffer
aparate

  ;
;; to bind other keys t e ion-auto-update' 3.12* bug-rned by `completioin `auto-comple.com hoe;; Version 0.11.4
;; * added `complet3g to be pfd-pathpfdguhingiutS to suioltip' scan be selected
;;s ge-functio  ibute echon, e in comr-chanloggmpletion-map', so t3;;   `compmplet or ule cto-c  * added eolvnu'
;;
;; Vex-alterationletion-map', so t3;9  retu-with-timer'x-aeys tnonsmenuame) on it where comp-g-fix to 'acceptmpletion,way, which some i;  of 'comp-g-fix to o-cootiveletion-map', so t3;8 triggomplet`completion-we wantew
;; define hoe;;ions knife on 0omoife `runltare'gain "nk myl".11.6
;; lterilt-command-keys'ion' and `completrized ;ommandeating region in whcomp-g-fix to 'ude  flag;pletionsn)
`complvaluatecomp-g-fix to o-cootivegiutscompletio`completion-we wantew
;; define holetion-map', so t3;7 triggompletM; Version11.6
;;e;;ion'    (dong in `ew
;; taodified by an undo, since 3 6* bug-ompletionsolvnu'
;;
;; Ve;ommandyou can
;;'y an undo, since 3  * bug-rned by ar-esigs into a suioltip' sbog`cong `byte-comn 0om ion-auto-update' 3.e from `auto-(nth j compln option
;; * remove `cop
;;andI  iion 0`compleralcx-alteratio  * added ealetion' in
flexi; * addede comot hard to bind otheo-completileaion;;   e-inund
;; to-c ife `rundit is also easy t(at you the   retu-with-timer' of `completin;
;; * rrely
;; * `completion-hve complin lt d lg otb :menu* layedns
;;   to m       at (youmpletion-map', so t3;3 comple comoke it hard to bind other-alterationrized ;;   comcl nepenokeyby an undo, since 3 d' into `c ler fin cascomorh.2etidrtimpatabintendpletion-auto-update' 3   Henry We ler fin -auto-update' 3in cascomorh.2etidr is  be sely-map', ml' and `c   rized ;;wrn-mode
;; *ys bind  (stather thannee;;
;; doo-comon lt d me) on it where on-tooltipde  flag  retu-with-tio define h original
;-cokcomgyin;
myedibu-auto-update' 2-fix in `ned be-, Fifth ix in oltip' d b aut`thanks* admpletion iion 0- ed blistcomplet global `he Lisp Xibu-auto-update' 2 Henry We ler fn-selecsteadMrek Zonzo or posolve-behavirned by `completioe con
;rs Vion 0.9.4
;; * mcom hoe<ret> c`compl-selecsteadJolvTo;; lter frame is s completi-itedoustomization opti   completion-scoot-ahin casconti * nerewe gn Streetd).


(y ar-esigs into a  following lamb;; followingsvid;efine h ;;    Tpleot)      )vars
;;   to m   ;   om'ew -ct-mentionsTpleenu-ittions getUIrol
;;   om'ew oct-mentions.")     )vars
;;   to m   ;st thatct-mentionsTpleenu-ittios getUIre-old'
ct-mentions.")  ))
        ;; return the menu keymap
        menu))))



;;; ======
=======================C' etc. variables
;; *s     )up, (a
;;   to m   Tpleenu-ittions ge-UI lets users .h i :up, (a'e-bis ikeys)      )y
;; *a
;;   to m max- fast, even;  nu-*Maximumund up to be extremely fast, even addckag.h i :up, (a'e-;   to m    i :-mode'ts u* r)      )y
;; *a
;;   to m  `completion-resolve-behaviinfinite nu-*Dbehaviiah j colterilt- longest common su.

Finfinite:========svides a minorale backwardso easy to infinite recurs:=svides a minorale backwardso   T p://wwwkwardso easy to inon-res:========svides a minorr  Accept thso easy th i :up, (a'e-;   to m    i :-mode'(ce. Itlambdat :-ag "ale ba"rale ba1+ j))
			         ambdat :-ag "r  Acc"rr  Acc1+ j))
			         ambdat :-ag "finite recurs"rale ba recurs))))))   )y
;; *a
;;   to m ion]a cept-or-reject-by-defaulinfinite nu-*Wind r; doome>'cu
;; *he teG
;; bG
;onel if the comp
that  * inoid untilpackag.

Finleaio:========ileaion-----ll if the comp penone.
Finfinite:========svides a minorale backwar-ll if the compo inon-res:========svides a minorr  Accept t-ll if the compo inask:		         askser-int; doome>'cpt t-ll if the comph i :up, (a'e-;   to m    i :-mode'(ce. Itlambdat :-ag "leaio"ileaio1+ j))
			         ambdat :-ag "finite"rale ba1+ j))
			         ambdat :-ag "r  Acc"rr  Acc1+ j))
			         ambdat :-ag "fsk" ask))))))   )y
;; *a
;;   to m ifie `com e nu-*Wilet gomple,l`completed by ltabPiddle;;
;; to-c lfie-al Pus
<tab>
;tSee -UI prov.tomization op are cons crde11.9
;ecoer-inis
mbdandIrndea* rov.h i :up, (a'e-;   to m    i :-mode'biioean)      )y
;; *a
;;   to m  de' code
; e nu-*Wilet gomple,l`completely fast, even whecode
;d=svides a mino
omonscl back ts a		(cned band usi longest common subunctio.
\(Eckause nxm, the foluameletiouse:
;;
;;   M-x auto-com
hank.2eri 'pop-up -interilt-dur;;
;; The phi;
;;   M-x.\)h i :up, (a'e-;   to m    i :-mode'biioean)      )y
;; *a
;;   to m  de' 
 :depleenu-Fnth j compl---
;; Smpletions cttions ge-UI l;   om'ew .
Wilet ule,l glhletiii;;vid;ame.
;;e y)     (nth j coissagd `au opti;; * Completionot bound
;;
;; io opti
a com hsemenu'
;;
;; Vepleti* bugcom hoomo
;;
  hanomat-comonatev
to prt' fuomat, prefix,;; * Completioefine h.h i :up, (a'e-;   to m    i :-mode'(ce. Itlambdat ple))))))   )y
;; *a
;;   to m pleti* bugcom h 3 nu-*Nd up to bmo
;;
  ilifa of opti;lso easy t(at  `compd
 layed me) on it where  instead o-ion-mode',  enablee
;d.h i :up, (a'e-;   to m    i :-mode'(ce. Itlambdat :-ag "Off" ple)+ j))
			         af(yot :-ag "On"))))))
   )ode',
;;   to m inged
;; *egistee'((((cl
;; color) (in `eparatedrek
			 ,(i(:in `eparate"bluo"i:ayedeparate"wh Puine-key (((cl
;; color) (in `eparated
;; 
			 ,(i(:in `eparate".2e * "i:ayedeparate"bln--")
			 "*Fays'ion' andanged
;; ;if the comp in mple `co-UI functions arh i :up, (a'e-;   to m   ))))
        ;inted;;
;;   M-xey
;; * `compls     ;     )up, (arvid;;
;;   M-x auto(pleenu-rvid;;
;;   M-x autoh i :up, (a'e-;   to m   ))))   )y
;; *aompletion-prefix-funct 'stomizi		 "*ittions ge--funct ranslse:
;;
;;   M-x auto-rh i :up, (a' and disable `auto-coi :-mode`(ce. It
	 lambdat ple)
	 l,@(notkeyst
	 l,(i(mapcar
	 l,(ide most e com)
		(can b'mbdat (car
ct-)
	  ; FIXME: mizakENUb9




y to	 l,(id
;;   to m   ;st thatct-mentions)))))))   )y
;; *a and disable `aut con
;rs(pleenu-*Mmenmumund up to bel back ts  layed if the comp a		(dckag;d.h i :up, (a' and disable `auto-coi :-mode'(ce. Itlambdat :-ag "Off" ple)+ j))
			         ats u* r :-ag "On"))))))   )y
;; *a and disable `aucom h pleenu-*Nd up to bmo
;;
  ilifa of layed nablee
xes whompletion (n
;;isms
eetielete bindingsoto-c.h i :up, (a' and disable `auto-coi :-mode'(ce. Itlambdat :-ag "Off" ple)+ j))
			         af(yot :-ag "On"))))))   )y
;; *a and disable `auin `auto-comple.com ht-ahinu-*Nd up to bmo
;;
  ilifa of layed nablee
xes whompletion (n
;;isms
 opti;de;   ons1through ceetielete bindingsoto-c.h i :up, (a' and disable `auto-coi :-mode'f(yot))))   )y
;; *aompletion-prefix-yntax-10
;;e'(r  Acce.* rov			 "*Associ even l back totyntax*on toxtend-pref  * added .
U By proely. So it is particux-alteration (nth j compldecndI er-i
t; dooba' fuomtionypedo l back tme iyntaxy) Wiletct the sourc`autompletion,wml' ommandece. It wu do!
;; By to
mbdfdguhicewont' and
;l-yntax-nepenoked ;
;;   M-xe * added s:
 :dencluding prefix
;; * ps a		(ccustom'ew genhn a hier  to ret
cbug- designcl back ts a		(nypedre in; * Dynce. Itl-coop-up f
\"-modenk mylly\mands\"ptrolua-get(ccustos\",code preletisohn 
fix
;; * ps a		(ccustom're in;mo
;;
l-coop-up fs\" rov\mand
\"d recu\",code preletisohn a hier  to ret cbug- y) If \"-modenk mylly\ma-comoetion ,a hier luding prefix
;; * ps
bind  ppear
pletion-modea		(dnnorale batio es with
ll
to bind othe custo;he "make"u typeaacs-lisp-moge ng  (tha
overlay'n-consmodenk myllyatefs\"ptrolua-get(ccustos\"ret
moetion ,a hier luding prefix
;; * ps a		(cvides a minorale bapd
esig> is tion-modeany,ptrolua-getandswh Puo-show l back toces. ommandeproely.packags'rtyntax*-tion).   (rexax
;;,danence tSPC
e' isn optnorale backwardlongestncluding prefix
;; * prlisp' atio
a o-show'opti;inatOomplaces mmaer* smpleion' and t,c`autou destg)
;tion-on-modeRL)
er
pletionu doquicknorale bacis now delete genetv	(dnand usingxt prov.tHow> isnd to any n;
;; * rroverlay',ge ng  (thefix
;; * ps aconsmodenk myllyve compl to may
aleiokedptnorale bac; * Completioeft uidfe orwisy) If \" rov\ma-comoetion ,a ypI
;; key ble-backwards' l back toces. ommandeproa.packagme iyntax*-tion)ce' isu mendkto se://wwwkwa
to-c  *ayed ;;   e-fubhefix
;; m're i-inis,
use:
;;
;;   M-x auto-ce' is * Dynfe-map [?provi suoto es an inte
candida-constr     ;; n`complese://wwwix* n youprovi layed me)
candi (omization op are cons crde11.9
;ecoervailal back ts nk m
se://www key b)atefs\"d recu\"a-comoetion ,a ypI
;; 
ey ble-backwards' l back towiiding a unew
;; = `posn-n yohavi l f
ons aowhiproeypI
;;e-baecu-gv* Cl back tsre i-inis,letions
;;  wiidingnsan be s -------al back ts t Cov	(nypedid untilpackag
 complpletia;; *veryeft uid-ns (of comoject if ca ypI
;; 
ey ble-backwards' l back t. Almeletionctiosarateqplet uickag;did
<tabewon * added ssn optnorp-popiockage es witetv	(pplied souto
ltabPiddle;;rnund
;; nd cois   Accepta-constr  for us ypI
;l fuCt the sourc`ae  * added ef (requireyntax*1.6
viduugpopiindmpmyedimmandgramandecreleti1. is 
;; =yntax-nepenoked ;
;;   M-x
 * added .
;;
`autou  Sof
;; d
;; ITHOnletion-ui-source-yntax-10
;; *lg otb :====0
;;eassoci ene.
eyntax*descriptoriabal back ts)ide a s* added ss(ewo-lper
;;
letio)y)     (* Dynlper
;; The phi * added e0
;;elg otb :omaticate
;; u
nfinite,inon-res;;rn'addre in; * Dynewonhaion----se pectioI
;; s
<tabml' ommandes* added ss\"ptrolua-get(ccustos\"r-con\"-mod
nk mylly\mplete-<nanctionons ppnorp-pop addn; =yntax
descriptor.n'addou menu-al back ts me>'cpt;   :ntax*-at-pocned 
and usi longest common subunctio (`auto-coactroluug-quence tran
ey ble-bacuwards' l back to)y)     mo
;;
llper
;; The phi0
;;elg otb :omatica
;; =y
;; u 'ey b,
'= `posn;rn'noma. 'ey br-con'= `posnhaion----se pectioI
;; s me)
cl' ommandes* added splete-<nanctionons ppnorp-pop addn; =yntax
descriptor,size,  s 'nomaooltip' fral back ts me>'cpt;   :ntax
ap',  `comI
;; etion-ui-sourcl fuWiletnletion-ui-source-yntax-10
;; *-comonmap', Lt-in Ema,way
cnei j compl
;; =y
;; nd
;;   describ`au btv	(ppli * added e0
;;
,
;; pr any the pbn
ntrol
;;  ervails-prefiomatica
;ug-
=y
;; ure in;0
;;eany the phaiona`load Reject thir
llper
;;,
ervailde11.9
;ecoerer-recoto gl -UI  ypedo l back toa completio
nally inlpackag.-----al back toa completioiif the fo gomple,l gloif the fo il. (on-mode',,wmerhap; coda bI
;lyveal comcois 
;;
thir
llper
;;eomptqpld
;fix* adquence tahe thir
llper
;;uto
l)atefsahe thir
llper
;;uThe phi0
;;eis that symbo,rnssus-pref
d
;; Imto `cde11.9
;econ is `pletref  * added . Licenacceptea
(nth j compl 0.1-. is 
;; job/wwwi`pletr
;;el back ts (e to ay
otdn-dle-timerread-n
ngesinteslo
;;  pletioid upa* D)da-conet
nclba; ion isp-pop veryonsmad-ke;;   f the
;;aion anthir

lper
;;.h i :up, (a' and disable `auto-coi :-mode'(ce. It+ j))
			  ambda :-ag "Pl' ommand"+ j))
			        (ce. Itl:-ag " insercompl * added "+ j))
			                ambdat :-ag "smodenk mylly"rr  Acc1+ j))
			                ambdat :-ag "ptrolua-get(ccustos"rale ba11+ j))
			        (ce. Itl:-ag "C
;;   M-xe * added "+ j))
			                ambdat  rov			 ))
			                ambdat = `pos)11+ j))
			  (=0
;;e:-ag "Ct the"+ j))
			         : `cosmodeel back t+ j))
			         :d
;; osmode(can 		 ))
			                      (ce. Itlambdat ale ba1+ j))
			         )
			                ambdat r  Acc1+ j))
			                              ambdat oad11+ j))
			                      (ce. Itlambdat  rov			 ))
			                              ambdat = `pos)		 ))
			                              ambdat noma)))))))))   )y
;; *a and disable `auion 0.11.-yntax-10
;;tee'((?0 . (r  Accenoma))-key (?1 . (r  Accenoma))-key (?2 . (r  Accenoma))-key (?3 . (r  Accenoma))-key (?4 . (r  Accenoma))-key (?5 . (r  Accenoma))-key (?6 . (r  Accenoma))-key (?7 . (r  Accenoma))-key (?8 . (r  Accenoma))-key (?9 . (r  Accenoma))-key (?' . (addo rov	
			 "*A0
;;eassoci ene.ral back ts me>'cxtend-pref  * added .
Oon 0.11s

;; choosing * added e ommandeproely. l back tme iyntax
nstituent binding
;;
yntax-10
;; re in; k my -teractive peas nk Onletion-ui-source-ynax-10
;; , coe backwnd regi=0
;;et
;; o
;
al back ts reject if caeyntax*descriptori.h i :up, (a' and disable `auto-coi :-mode'(=0
;;e: `cosmode(ce. Itl l back tocmbdat :-ag "choosin" a11+ j))
			        :d
;; osmode(can  (ce. Itlambdat :-ag "finite"rale ba1+ j))
			                                  ambdat :-ag "r  Acc"rr  Acc1+ j))
			                                  ambdat :-ag "add" oad11+ j))
			                          ame. Itlambdat :-ag "= `pos" = `pos)		 ))
			                                  ambdat :-ag " rov"  rov			 ))
			                                  ambdat :-ag "noma" noma)))))  ))
        ;; return the menu keymap
        menu))))



;;; ======
==================Or-recthdfdguhvariables
;; *s     )vars
;;   to m g-fix in `comple pleenu-Hsolv 0.1 opti;; * Completio  enaustom'rfuCix
;; * ps a		(ccustom'selecyll
;; to bind othe custo;,
moetione.

n-ui-regato Henrmmandmoetione.

n-uenu
;;
o cyc F `comple arpoint' fun-pee -in-buffe:nts
   by `,----
ng a unew= `posn-n yowaspccustom's( functionsts
   by `)da-conany
unction, prefix*ve commanproely.UI lethe phiale bacnot bouowas
agd `au `rning in ly.")  )   )vars
;;   to m x'etion-accept-f pleenu-Hsolv 0.1 opti;; * Completio  ex'etiom'rfuCix
;; * ps a		(x'etiom'selecyll
;;
to bind othelay
;;
c F `comple arpoint' fun-pee -in-buffe:nts

  by `,-----ng a unew= `posn-n yowaspx'etiom's\( functionsts
 of
ts
   by `\)da-conany unction, prefix*ve commanproely.UI lethe ph
x'etioetionot bouowaspagd `au `rning in ly.")  ))   )vars
;;   to m -update-sel pleenu-Kum (leng in `in;; * Completioefine h.h)     )vars and disable `auion ate-sel pleenu-Kum (leng in `in;; * Completioefine hdesig
use:
;;
;;   M-x auto-compth the .")     )vars
;;   to m pletion-auto-update-sel pleenu-Kum (leng in `in;; * Completioefine hdesig
u
;;   to m pletion-aut-compth the .")     )vars
;;   to m sel pleenu-Kum (leng in `efix isittions getUIrat (yoump.h)     )vars and disable `ausel pleenu-Kum (leng in `efix use:
;;
;;   M-x auto-compth the .")  ))
        ;; return the menu keymap
        menu))))



;;; ======
======================= Ipose utiles
;; *s     )vars
;;   to m ui--nablee
;d pleenu-Ngomple`efix ittions getUIrat nablee
;d in;; packagy)   utompletionpop-up soely.if-condidisable `ausel'nnum (l-UI alsocrelo `sehn--stiliffer-aparatepoandofix-funnum (le0.10.2
;ay
oldthiidn't
  ;efin Emay) I-wition new 'po is be you the `efix debuggionsittions getUIrnctime) on it where sel'n11.6
;; *ied iaate nanclbpers.")   mad--mpletion not thride-al'
;;   to m ui--nablee
;d)  )   )vars
;;   to m o-update-can  pleenu-Lan be sefix-fuviion' duinterhso easy th)  mad--mpletion not thride-al'
;;   to m o-update-can )  )   )vars
;;   to m ose:
;)
;;  ()
;; -opleti			 "T
;;  ion' andpostpomat,elete bindingsois rvid;ead 
to a sltare'gaathe A numb ypI
;l")  )   )vars
;;   to m uin `auto-comple.)
;;  pleenu-T
;;  ion' andpostpomatcttions ge-Uo a s;; *he tede;   onl")  )   )vars
;;   to m ucomp-g-fix to opleenu-Uon' andcompg)
;; * bug-fix to `n 0om ipletitltion-cerlo `tcttions ge-e user commfndswh ng at;; * b
g-fix to `on-hotssingap',  `ting defacthdfdguhidrn 0.11.6
;; *mon proely.n EmacUI l.")  ))
        ;; return the menu keymap
        menu))))



;;; ===========
===============Setenu, ey-ieidr pode;  e-  buffer
 auto ) puta' and disable `aux-alteratio 'de;  e-  buffer
ot)  puta'
;;   to m g-fix i `flnewlks t'de;  e-  buffer
ot)  puta'
;;   to m in `auto-comple. l b 'de;  e-  buffer
o'0.1eystde)  puta'
;;   to m in `auto-comple. l b-u  h;ify		 ))
'de;  e-  buffer
o'0.1eystde)  puta'
;;   to m comple. l b 'de;  e-  buffer
o'0.1eystde)  ))
        ;; return the menu keymap
        menu))))



;;; =========
=====================Kum11.6
;; l
;;   and)   )u`tcttions ge;;ommand the e-backwards-ems to 		 (n 0. l b &on tra * eyntax*noe-yntax-ion 0.11			 "S; * on 0.11.6
;; l poKEYcys bind s seratioen l back toCHAR; s
<tletio tme iyntax*w
		(SYNTAX.(SYNTAX choosin- ome the e-backwards, ?w, heomplplet-ite funn)
;;t symbo,riotgiyou de!
;; By toomonm (arny iyntaxy) If NO-SYNTAX-OVERRIDEe fo gomple,l`auto11.6
;; e' isu men
use:
;;
;;   M-x ion 0.11.-yntax-10
;; *-at-poge ng tcomons
;is
n 0.11.6
;; uviion',;ys bind regi * added eompde11.9
;edoew 'pby
SYNTAX."

Fi(omons(on an-yntax) (monq iyntax*?w11+ j(    ((doc ambdcnd "Iratio \"" (m `posn l b) "\"r-uameletios si
		(c\
key ble-backwards.")))))=======-pletiouse:
;;
;;   M-x ion ate-self ems to 		  e commandn 0. and disable `auion ate-sel tion======`e most e ) ,doc		 ))
			 ( `rning in 			 ))
			 ( and disable `aux-alteratio , l b ,iyntax

			                                  ,noe-yntax-ion 0.11	))))=======i`comEmacidn't
 s knife o0.10.2
;ofix-funnum (lsElis eyno,nopleti)=======11.6
;; us on it where sel'n adtim mentsonsvia)=======on it where  we wantew
;; define hoehn--o			 (omons(<=comEma sejor-idn't
 s21			 ))
	 commandn 0.disable `ausel tion========`e most e ) ,doc		 ))
			 	 ( `rning in 			 ))
			   ambit where  we wantew
;; define h

			        e most e ) ( `rning in 			 ))
			   		 ( and disable `aux-alteratio , l b ,iyntax ,noe-yntax-ion 0.11	)		 ))
			   	'
;;   to m ui--nablee
;d)))))))))   )u`tcttions ge;->
:
m comple. g p)
;;
(map			 "R
:
mode;   modlisp)
;   tm,otions getUIridn't
   \(ter fed kwatr cois   Ac11.6
;; ,=i`c>
:
mpletiiio gl 0.10.2
 t\)."

Fi===Ifelisp)
;b>
:
mpletiiio0.10.2
 tf r
:
mode;  edlisp)
; + j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 lisgn		 ))
			 commandn 0.:
mo[r
:
mode;  e. l b]
))
			   	'
;;   to m de;  e. l b)		 ))
			 commandn 0.:
mo[r
:
moin `auto-comple. l b]
))
			   	'
;;   to m in `auto-comple. l b)		 ))
			 commandn 0.:
mo[r
:
mocomple.in `auto- l b]
))
			   	'
;;   to m in `auto-comple. l b)		 ))
			 commandn 0.:
mo[r
:
moin `auto-comple. l b-u  h;ify]
))
			   	'
;;   to m in `auto-comple. l b-u  h;ify)		 ))
			 commandn 0.:
mo[r
:
mokiidd the]
))
			   	'
;;   to m kiidd the)		 ))
			 commandn 0.:
mo[r
:
moin `auto-kiidd the]
))
			   	'
;;   to m in `auto-kiidd the)		 ))
			 commandn 0.:
mo[r
:
mokiiddsp' domp]
))
			   	'
;;   to m kiiddsp' domp)		 ))
			 commandn 0.:
mo[r
:
moin `auto-kiiddsp' domp]
))
			   	'
;;   to m in `auto-kiiddsp' domp)		 ))
			 commandn 0.:
mo[r
:
mokiiddspxp]
))
			   	'
;;   to m kiiddspxp)		 ))
			 commandn 0.:
mo[r
:
moin `auto-kiiddspxp]
))
			   	'
;;   to m in `auto-kiiddspxp)		 ))
			 commandn 0.:
mo[r
:
mokiiddlks ]
))
			   	'
;;   to m kiiddlks )		 ))
			 commandn 0.:
mo[r
:
mokiiddh', plus s]
))
			   	'
;;   to m kiiddh', plus )		 ))
			 commandn 0.:
mo[r
:
moin `auto-kiiddh', plus ]
))
			   	'
;;   to m in `auto-kiiddh', plus ))))=======Octioion-,ou d't doobshouldif cahanks t11.6
;; *lterilt-coms)=======* bug-fixelonges glotively. (tha

			 cocan  (n 0.'([comple] [comple l b] [-auto-sho] "\d"+ j))
			           [(creleti1comple)] [(creleti1comple l b)]+ j))
			           [(meta1comple)] [(meta1comple l b)]+ j))
			           [(creleti1-auto-sho)] [(meta1-auto-sho)] "\M-\d"	)		 ))
	(colveinonotivel	 ))
			 cocan  (11.6
;; '((domple. l b . 
;;   to m de;  e. l b)		 ))
																						(kiidd the . 
;;   to m kiidd the)		 ))
																						(kiiddsp' domp . 
;;   to m kiiddsp' domp)		 ))
																						(kiiddspxp . 
;;   to m kiiddspxp)							(kiiddlks t. 
;;   to m kiiddlks )		 ))
																						(kiiddh', plus t. 
;;   to m kiiddh', plus )		 ))
																						(in `auto-comple. l b

			                        . 
;;   to m in `auto-comple. l b)		 ))
																						(comple.in `auto- l b

			                        . 
;;   to m in `auto-comple. l b)		 ))
																						(in `auto-comple. l b-u  h;ify		 ))
                       . 
;;   to m in `auto-comple. l b-u  h;ify)		 ))
																						(in `auto-kiidd the		 ))
                       . 
;;   to m in `auto-kiidd the)		 ))
																						(in `auto-kiiddsp' domp		 ))
                       . 
;;   to m in `auto-kiiddsp' domp)		 ))
																						(in `auto-kiiddspxp		 ))
                       . 
;;   to m in `auto-kiiddspxp)		 ))
																						(in `auto-kiiddh', plus 		 ))
                       . 
;;   to m in `auto-kiiddh', plus ))1+ j))
			  (omons(eq (n 0-ems to -com) (car
ems to 	)		 ))
			   	 commandn 0.:
mon 0.(cdr
ems to 	)		 ))
			   	 nmplwinonotive a11)))))  ))   )u`tcttions ge;-ems -fi
  h; *on
;rs((mapdlisp)
;			 "M "make"(lambdfi
  h; * Cl back ts andCOMMAND.uCixp)
;b>
:
mpletiiioa far
eshouldwion i doo
;is,e;;ionsition new 'pbe
; By the phi longestn Emacidn't
  ln--stlisp)
;b>
:
mpleti0.10.2
.h i  commandn 0.:
mo"A"dlisp)
;			  commandn 0.:
mo"a"dlisp)
;			  commandn 0.:
mo"B"dlisp)
;			  commandn 0.:
mo"b"dlisp)
;			  commandn 0.:
mo"C"dlisp)
;			  commandn 0.:
mo"c"dlisp)
;			  commandn 0.:
mo"D"dlisp)
;			  commandn 0.:
mo"d"dlisp)
;			  commandn 0.:
mo"E"dlisp)
;			  commandn 0.:
mo"e"dlisp)
;			  commandn 0.:
mo"F"dlisp)
;			  commandn 0.:
mo"f"dlisp)
;			  commandn 0.:
mo"G"dlisp)
;			  commandn 0.:
mo"g"dlisp)
;			  commandn 0.:
mo"H"dlisp)
;			  commandn 0.:
mo"h"dlisp)
;			  commandn 0.:
mo"I"dlisp)
;			  commandn 0.:
mo"i"dlisp)
;			  commandn 0.:
mo"J"dlisp)
;			  commandn 0.:
mo"j"dlisp)
;			  commandn 0.:
mo"K"dlisp)
;			  commandn 0.:
mo"k"dlisp)
;			  commandn 0.:
mo"L"dlisp)
;			  commandn 0.:
mo"l"dlisp)
;			  commandn 0.:
mo"M"dlisp)
;			  commandn 0.:
mo"m"dlisp)
;			  commandn 0.:
mo"N"dlisp)
;			  commandn 0.:
mo"n"dlisp)
;			  commandn 0.:
mo"O"dlisp)
;			  commandn 0.:
mo"o"dlisp)
;			  commandn 0.:
mo"P"dlisp)
;			  commandn 0.:
mo"p"dlisp)
;			  commandn 0.:
mo"Q"dlisp)
;			  commandn 0.:
mo"q"dlisp)
;			  commandn 0.:
mo"R"dlisp)
;			  commandn 0.:
mo"r"dlisp)
;			  commandn 0.:
mo"S"dlisp)
;			  commandn 0.:
mo"s"dlisp)
;			  commandn 0.:
mo"T"dlisp)
;			  commandn 0.:
mo"t"dlisp)
;			  commandn 0.:
mo"U"dlisp)
;			  commandn 0.:
mo"u"dlisp)
;			  commandn 0.:
mo"V"dlisp)
;			  commandn 0.:
mo"v"dlisp)
;			  commandn 0.:
mo"W"dlisp)
;			  commandn 0.:
mo"w"dlisp)
;			  commandn 0.:
mo"X"dlisp)
;			  commandn 0.:
mo"x"dlisp)
;			  commandn 0.:
mo"Y"dlisp)
;			  commandn 0.:
mo"y"dlisp)
;			  commandn 0.:
mo"Z"dlisp)
;			  commandn 0.:
mo"z"dlisp)
;			  commandn 0.:
mo"'"dlisp)
;			  commandn 0.:
mo"-"dlisp)
;			  commandn 0.:
mo"<"dlisp)
;			  commandn 0.:
mo">"dlisp)
;			  commandn 0.:
mo" "dlisp)
;			  commandn 0.:
mo"."dlisp)
;			  commandn 0.:
mo","dlisp)
;			  commandn 0.:
mo":"dlisp)
;			  commandn 0.:
mo";"dlisp)
;			  commandn 0.:
mo"?"dlisp)
;			  commandn 0.:
mo"!"dlisp)
;			  commandn 0.:
mo"\"" lisp)
;			  commandn 0.:
mo"0" lisp)
;			  commandn 0.:
mo"1" lisp)
;			  commandn 0.:
mo"2" lisp)
;			  commandn 0.:
mo"3" lisp)
;			  commandn 0.:
mo"4" lisp)
;			  commandn 0.:
mo"5" lisp)
;			  commandn 0.:
mo"6" lisp)
;			  commandn 0.:
mo"7" lisp)
;			  commandn 0.:
mo"8" lisp)
;			  commandn 0.:
mo"9" lisp)
;			  commandn 0.:
mo"~" lisp)
;			  commandn 0.:
mo"`" lisp)
;			  commandn 0.:
mo"@" lisp)
;			  commandn 0.:
mo"#" lisp)
;			  commandn 0.:
mo"$" lisp)
;			  commandn 0.:
mo"%" lisp)
;			  commandn 0.:
mo"^" lisp)
;			  commandn 0.:
mo"&" lisp)
;			  commandn 0.:
mo"*" lisp)
;			  commandn 0.:
mo"_" lisp)
;			  commandn 0.:
mo"+" lisp)
;			  commandn 0.:
mo"=" lisp)
;			  commandn 0.:
mo"(" lisp)
;			  commandn 0.:
mo")" lisp)
;			  commandn 0.:
mo"{" lisp)
;			  commandn 0.:
mo"}" lisp)
;			  commandn 0.:
mo"[" lisp)
;			  commandn 0.:
mo"]" lisp)
;			  commandn 0.:
mo"|"dlisp)
;			  commandn 0.:
mo"\\"dlisp)
;			  commandn 0.:
mo"/"dlisp)
;	)  ))   )u`tcttions ge;-tim mento-update-ems to m		  e-old'
ctstompletionp&on tra * no-n
nges			 ;; Sim mentsSOURCE;ofix-funnum11.6
;; *in DESTtate name)
=====on it where  we wantew
;; define hoehn--. DESTtled
;;   (a =y
;; nw;ug-
=====d
;; Iiioa num (l-USOURCE;led
;;   (a num (l.
====
=====VARIABLEtled
;;   (a =y
;; n* bugdenablee
;s DESTtomonsnssud
;; Iii
=====(hank.2eri ') use :fo il. U optno, DESTto' is  (a you can
;; num (leavel	 ===VARIABLEto' is  (ltabPiu can
;; mpletionpi-regwh ng ahe folusoci evio  i=====oPiu can
;;usel-10
;; r
====
=====NO-PARENTto' isoltip' s`autog-fix t;; uslly inl.
ngestnum (leofUSOURCE,
=====i`ciyohavioma.

=====i`cNO-PARENTtiio0o uie).
, ;;   cl.
ngestnum (ley))))
		(ivioma
  (omons()
;bno-n
nges((memqu'th pre.(cdr
e-old'))1+ j))(monq i-old'+ j))
			  (cttions ge;-tubcan 		 ))
			   e-old'
0 (1+ (cttions ge;-
;;   it''th pre.(cdr
e-old'))1	))))===== (leo is  (sti1.6
;; *in SOURCE
  (sel-th pre		 )e most e k 0.11.6
;;)		 ))
===dofe o0im ments>
:
mplets,w', asofe o0im ments.
ngestnum (l's ems to m		 	 )e
If MEN(eq n 0.'>
:
m)		 ))
		===n optnornee;tilifr
mon 0.; taodarrayor podef
;ndn 0		 ))
		e
If MEN(m `posp-com) (monq n 0.(vectomon 0))1+ j))
		===11.6on 0.; tDESTt adtim mentdsefix-funnum (le11.6
;;		 ))
		ecommandn 0.ctston 0		 ))
		  (cttions ge;-prefix'
;-tim mentdo-update-ems to =11.6
;; mpletion)))1+ j)e-old'))  ))   )u`tcttions ge;-prefix'
;-tim mentdo-update-ems to =(11.6
;; mpletion))=====R-prefia=11.6
;; pt;   im mentsunsage 
;; BINDINGt adKEYc; taodified by=====num (l-Uate name) on it where  we wantew
;; define hoehn--.
====
=====VARIABLEtled
;;   (a =y
;; n* bugdenablee
;s BINDINGtomonsnssud
;; Iii
=====(hank.2eri ') use :fo il. Typa mino, BINDINGto' is  (otive ; tae===== ou can
;; num (leave=VARIABLEto' is  (ltabPiu can
;; mpletionpi-regwh ng
=====ihe folusoci evio  =oPiu can
;;usel-10
;; r
====
=====Ttab>
prefid
;; Iiioa lisp)
;by))BINDINGtoaioa lisp)
;mmandatnum (ley)
=====BINDINGtoaioa num (l. Anymoject imodeo))BINDINGt(e to a >
:
mplet			 ===>
preff pleve compl))
		(ivino easydwion i 0im ments`aut.

==(cree		 )===dofe o0im mentslisp)
;b>
:
mplets omon 0bopporttirots	  ((ter(eq 11.6
;; '>
:
m)N(m `posp-ems to 	)		 ))ple)+		 )===y))BINDINGtiioa num (l-U---
;ed selndmpg-fix t nxmo tm,oefix'
;oa num (l		 )===agra`aume>'c11.6
;; * t;   im menttaodified b num (l		 )((num (lp.11.6
;;)		 ))(    ((pre.(mad--s.
nse-th pre))1+ j))
	(sel-th pre		 ) 	 )e most e k 0.11.6)		 ))
			 ===n optnornee;tilifr
mon 0.; taodarrayor podef
;ndn 0		 ))
				e
If MEN(m `posp-com) (monq n 0.(vectomon 0))1+ j))
		 	 commandn 0.:
mon 0		 ))
			   (cttions ge;-prefix'
;-tim mentdo-update-ems to =11.6 mpletion)))		 ))
		11.6
;;)		 ))
 pre))+		 )===y))BINDINGtiioa lisp)
;mm,oefix'
;oanhd
;;   `cong p)
; pt;   im ments

		===11.6
;; pt;  lisp)
;byntaodified b num (l		 )((otocmbsp)
;p.11.6
;;)s()
;b(=y
;; p.11.6
;;)s(=y
;; n-accept--ems to 	))		 ))(    (-acccomn, pcan  docm `posn `rning in `, ps)		 ))
 ===mple-accept--commantionsemembsp)
;
			   (ctvel	 ))
		((=y
;; p.11.6
;;)s(=onq -acccomn(=y
;; n-accept--ems to 	))		 ))
		((-accept-p.11.6
;;)s(=onq -acccomnems to 	))		 ))
	===ex




n, prefix*can 		 ))
	(ctvel	 ))
		===-onto ao-(nth j c		 ))
		((byle. gd* remove `-p -acccom)
	(=onq , pcan  (
ngf -acccomn0))1+ j))
		===acconto ao-(nth j c		 ))
		(()
;b(can p -acccom)s(eq (car
-acccom)s' most ))
	(=onq , pcan  (n>'c1
-acccom)	))		 ))
	===ex




ndocm `posn)
;bynrning in `ct-mention		 ))
	(=onq docm `posn(docrefixaept--ems to 	)		 ))
	(=onq ynrning in `( `rning in - k m-ems to 	)		 ))
	===-oefix'
;odocm `posnfoto ewe11.6
;;		 ))
	(=onq docm `pos		 ))
			    ambdcnd "Do uickag;di de
;; *nepenone.

noerer-reced souiio"+ j))
			            "ew
;; tatncluding prefix
;; * p.\n\n"+ j))
			            "Ifced souiioew
;; tatncluding prefix
;; * p,\n"+ j))
			            (downtions( fed `posndocm `posn0 111+ j))
			            ( fed `posndocm `posn11+ j))
			            "\n\n"+ j))
			            "Ifced souiiocludfw
;; tatncluding prefix
;; * p,\n"+ j))
			            " 0.1er-io is wd
;; nk myllys  (otive ili"+ j))
			            "`auton 0.and-keys."	)		 ))
	===-oefix'
;ocan be s, prefix*mpletionp-ites, ;;   posn&on tra * )
;
			   ===&>
;t		 ))
	(=onq , ps '(can )1+ j))
	(selc)e most e a1+ j))
			      e
If MEN(ter(eq a '&on tra *)r(eq a '&>
;t11+ j))
			        (=onq , ps ( ppecona ps (0
;;ea)))11+ j))
			    , pcan )		 ))
	===-oefix'
;o)
;b>
prefing p)
; pi 0im mentsefix-funnum (le11.6
;;		 ))
	`e most e,ambpy-and-keys', pcan )		 ))
	   "" ;,docm `pos		 ))
			 ,ambpy-and-keys' `rning in 			 ))
			 (n it where  we wantew
;; define h+ j))
			  ( most e,ambpy-and-keys', pcan ) ,ambpy-and-keys' `rning in 			 ))
			 	  ( ppnor',11.6
;; ,, ps)			 ))
			 	',mpletion))		 ))
	))+		 )===anyof comels Iiioa `n 0om		 )(  (n 0omoambdcnd "Unexptiom'se1.6
;; us "+ j))
			             "`cttions ge;-prefix'
;-tim mentdo-update-ems to ': %s"1+ j))
			     ems to 	)))  ))
        ;; return the menu keymap
        menu))))



;;; ===========
========================S; * odhoosingnum (ls)
  =Sete
;; choosing 1.6
;; *lterilt-com (leasage d band usi 
;;   M-x
  =efix-fuv,=i`ciyohavfe o l f ;ommandealread0.(m n sionsl0.; taodment colt).
e
If MENcttions ge;ion ate-sel
=====Note:b>
e1.6
;; fi
  h; * Cl back ts )
		(iviredivee
;;y)
=====	 ))
	`se:
;;
;;   M-x auto-compth the ve compl))
y a		(che pbtive ; 
=====	 ))
	`se:
;;
;;   M-x aap',riotgwew= ' iscee;tilienread-bind unluding pr
=====	 ))
	;
;;   M-x *ied iing defacdenludfw
;n is di`c`se:
;;
;;   M-x auto-
=====	 ))
	at you the .

=====i`cwe`on-hotmapdlisp)
;sf r
:
mo`x-alteratio. g p)
;'n a
=====on it where r-alterationr j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 lisgn		 ))
			 =onq cttions ge;ion ate-sel.(mad--s.
nse-th pre))		 ))
			 commandn 0.cttions ge;ion ate-sel.[r
:
mox-alteratio. g p)
;]
))
			   	'
;;   to m x-alteratio))		 ))  =ectioion-,oupletioa gpletg 1g num (leave=>
e1.6  (stfi
  h; *		 ))  =Cl back ts andon it where r-alterationhe "make"

			 =onq cttions ge;ion ate-sel.(mad--th pre))		 ))(n it where  ems -fi
  h; *on
;rs(cttions ge;ion ate-sel
							 	'
;;   to m x-alteratio))	
=====M-< h;>eave=M-/ o-com?provi surarchi		 commandn 0.cttions ge;ion ate-sel.[?\M-\t]	'
;;   to m o-com			  commandn 0.cttions ge;ion ate-sel."\M-/"	'
;;   to m o-com			 ===M-<shift>-< h;>eave=M-? e
 optnorM-<shift>-/) o-com?1through 		  commandn 0.cttions ge;ion ate-sel."\M-?"	'
;;   to m o-com in `autos			  commandn 0.cttions ge;ion ate-sel.[(meta1shift	ato-that h;)]+ j))'
;;   to m o-com in `autos			 tree-RET(ccustos,ee-DELpx'etio 		  commandn 0.cttions ge;ion ate-sel.[(creleti1>
pref)]a'
;;   to m g-fix 			  commandn 0.cttions ge;ion ate-sel.[(creleti1-auto-sho)] '
;;   to m r  Acc1+ jtre< h;>e knidcomd Reject tab;;
;;   M-x		  commandn 0.cttions ge;ion ate-sel."\t"	'
;;   to m tab;;
;;   e			 tree-< h;>escoot *ihead		  commandn 0.cttions ge;ion ate-sel.[(creleti1 h;)]+ j))'
;;   to m ex
es -fiey `)		 tree-<Version bG
;ons (e-<VersionlisduIt wC-@numb 1.9
;che			  commandn 0.cttions ge;ion ate-sel.[?\C- ] '
;;   to m r  Acc1+ j commandn 0.cttions ge;ion ate-sel."\C-@" '
;;   to m r  Acc1+ jtre===>
sel.
;; ch;   modlisp)
; 
	 (n it where  w
:
m comple. g p)
;;
cttions ge;ion ate-sel1+ j)
)
  =Sete
;; choosing 1.6
;; *lterilt-com (leasage d band usi 
;;   M-x=efix-fuv
  =nstituent binding
;;auto-,=i`ciyohavfe o l f ;ommandealread0.(m n sionsl0
  =nstaodment colt).
e
If MEN and disable `auion ate-sel
=====intioid  (stnum11.6
;; *ap', disable `auion ate-sel,nstr  adel	 ===,elete bindingso0o uie)cres ao llow
  (=onq ,and disable `auion ate-sel (mad--s.
nse-th pre))		 (=on-th pre-n
nges(,and disable `auion ate-sel cttions ge;ion ate-sel1+		 ===M-<sersion bG
;ons aisp' atioioa sersi+ j commandn 0.,and disable `auion ate-sel "\M- "+ j))e most e &on tra * )rt			 ))
	"R  Accerny dlongestncluding prefix
;; * prlisp' atiooa sersi."+ j))
	( `rning in o"P"			 ))
	 
;;   to m r  Acc )rt			 ))
	(' atioo" "	))))=====Note:bn isp-pop,  s
noee=dofe oug-
=====	 ))
	`
;;   to m command the e-backwards-ems to ' )
		(ivibind reg
=====	 ))
	 most eexp>
;'t
  ;iyoupletis wd
;;fe o l byle. gnto ao. Anyize, 
=====	 ))
	els ,	`
;;   to m command the e-backwards-ems to ' led
;;   (; By.+		 ===M-S-<sersion' atioioa sersi aioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel [?\M-\S- ]+ j))e most e 			 ))
	"I atiooa sersir-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?\  ?w a11)+		 ===M-.n' atioio"."daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-."+ j))e most e 			 ))
	"I atioo\".\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?. ?w a11)+		 ===M--n' atioio"-"daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M--"+ j))e most e 			 ))
	"I atioo\"-\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?- ?w a11)+		 ===M-\n' atioio"\"daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-\\"+ j))e most e 			 ))
	"I atioo\"\\\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?\\ ?w a11)+	=========M-/n' atioio"/"daioa  the e-backwards+====== commandn 0.,and disable `auion ate-sel "\M-/"
========e most e 		=========="I atioo\"/\"r-uameletios si
		(ckey ble-backwards."+==========( `rning in 		==========( and disable `aux-alteratio ?/ ?w a11)+		 ===M-(n' atioio"("daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-("+ j))e most e 			 ))
	"I atioo\"(\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?\( ?w a11)+		 ===M-(n' atioio")"daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-)"+ j))e most e 			 ))
	"I atioo\")\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?\) ?w a11)+		 ===M-{n' atioio"{"daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-{"+ j))e most e 			 ))
	"I atioo\"{\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?{ ?w a11)+		 ===M-}n' atioio"}"daioa  the e-backwards+ j commandn 0.,and disable `auion ate-sel "\M-("+ j))e most e 			 ))
	"I atioo\"}\"r-uameletios si
		(ckey ble-backwards."+ j))
	( `rning in 			 ))
	( and disable `aux-alteratio ?} ?w a11)+		 ===i`cwe`on-hotmapdlisp)
;sf r
:
mo`x-alteratio. g p)
;'r j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 commandn 0.,and disable `auion ate-sel
	[r
:
mox-alteratio. g p)
;]
	' and disable `aux-alteratio)		 ))  =ectioion-,o>
e1.6  (stfi
  h; *=Cl back ts an		 ))  = So it is particux-alteration e "make"

			 n it where  ems -fi
  h; *on
;rs		 ))
 and disable `auion ate-sel
==  a' and disable `aux-alteratio)1+ j)
)

  =Sete
;; choosing 1.6
;; *lterilt-com (leasage d band usi 
;;   M-x=efix-fuv
e
If MENcttions ge;pletion-auto-update-sel
=====intioid  (stnum11.6
;; *ap', disable `auion ate-sel,nstr  adel	 ===cttions ge;pletion-auto0o uie)cres ao llow
  (=onq cttions ge;pletion-auto-update-sel (mad--s.
nse-th pre))		 (=on-th pre-n
nges(cttions ge;pletion-auto-update-sel
					 	cttions ge;ion ate-sel1+ j===i`cwe`on-hotmapdlisp)
;sf r
:
mo`x-alteratio. g p)
;'r j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 commandn 0.cttions ge;pletion-auto-update-sel
	[r
:
mox-alteratio. g p)
;]
	'cttions ge;pletion-autox-alteratio)		 ))  =ectioion-,o>
e1.6  (stfi
  h; *=Cl back ts an		 ))  = So it is particux-alteration e "make"

			 n it where  ems -fi
  h; *on
;rs		 ))
cttions ge;pletion-auto-update-sel
== ))'
;;   to m pletion-autox-alteratio)))
)

  =Sete
;; choosingnum (ley))iyohavfe o l f ;ommandealread0.(m n sionsl0.; tao
  =nsnt colt). Licennum (leyseng in `efixo is `
;;   to m ui--nablee
;d-com
  = gomple.
e
If MENcttions ge;sel
=====Ihe phi longestn Emacidn't
   knife o0.10.2
;ofix-funnumems to m		 ===halfldecges gl', asonife o0.10.2
;lisp)
;b>
:
mplet,cwe'		(go
;; po		 ===hav f the1.6  (stfi
  h; *=Cl back ts i;
`autonum (l-Usocwe`m
;; ;am		 ===we isupletioa fu(stnumsel
==(i`c()
;b(<=comEma sejor-idn't
 s21			 ))
	 ))
	 clud(fotive(a'e-;n `fl>
:
mplet		)		 ))
	(=onq cttions ge;sel.(mad--th pre))		 ))(=onq cttions ge;sel.(mad--s.
nse-th pre))1++ jtre===M-< h;>eave=M-/ o-com?ors
;;   tm?provi surarchi		===(commandn 0.cttions ge;sel.[?\M-\t]	'
;;   tto-r o-com ey blat-rarch)i		===(commandn 0.cttions ge;sel."\M-/"	'
;;   tto-r o-com ey blat-rarch)i		======M-<shift>-< h;>eave=M-? e
 optnorM-<shift>-/) o-com?1through 		 ===(commandn 0.cttions ge;sel.[(meta1shift	ato-that h;)]+ j====='
;;   tto-r o-com 1through  ey blat-rarch)i		===(commandn 0.cttions ge;sel."\M-?"+ j====='
;;   tto-r o-com 1through  ey blat-rarch)i+ j===RET(denls me>'cany uenone.
 
;;   M-x= fast, ev,nstr  runm		 ===wr-io is issn optnorotive iliRET.)=====Note:balmeletioncuviions on it where  we wantew
;; define ho, the f
=====	 ))
	cludaehn--tiliffer-aparatepoandofix-funnum11.6
;;		 ===	 ))
	0.10.2
. Reject,cwen whec t;; ue :fowe 
=====	 ))
	`
;;   to m l>
solv*onlonges'a-constr   0.1plet-k myliRET
=====	 ))
	num11.6
;;. W t11.6)iyohe
;;  ptead/wwwi`bn ispfied b num (l		 ===	 ))
	beu mend tme easin-dle-you the
`autonum (l.+ j cocan  (n 0.'("\r" "\n".[r
pref]))		 ))(commandn 0.cttions ge;sel.n 0.'
;;   to m r solv*ostr  we wcisp)
;	)   e===>
sel.
;; ch;   modlisp)
; 
	 (n it where  w
:
m comple. g p)
;;
cttions ge;sel1+		 ===r
:
mo`fiiddh', plus 'mmand>
e1.6 M-q=i`cwe`on-'t=r
:
mr j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 commandn 0.cttions ge;sel.[r
:
mofiiddh', plus ]
	'cttions ge;fiiddh', plus )		 ))(commandn 0.cttions ge;sel."\M-q" '
;;   to m fiiddh', plus ))))=====i`cwe`on-hotmapdlisp)
;sf r
:
mo`x-alteratio. g p)
;'n a
=====on it where r-alterationr j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 commandn 0.cttions ge;sel.[r
:
mox-alteratio. g p)
;]
	'cttions ge;x-alteratio)		 ))  =ectioion-,o>
e1.6  (stfi
  h; *=Cl back ts an		 ))  =  is particux-alteration e "make"

			 n it where  ems -fi
  h; *on
;rs.cttions ge;sel.'
;;   to m x-alteratio))		 )
)
  =timerread-cttions ge;sel. folusoci eviome>'c`
;;   to m ui--nablee
;d-coo
  =ltabPiu can
;;-th pre-10
;;,;ys bind regi 1.6
;; *ied th the `efixo is
  =C;;   to m UIrat (yoump
(    ((cois   Ac(lusq	'
;;   to m ui--nablee
;dbPiu can
;;-pre-10
;;		)		 (i`ccois   A		 ))
	(=oncdr
cois   Accttions ge;sel1+))
	(push ambda '
;;   to m ui--nablee
;dbcttions ge;sel1+))
							Piu can
;;-pre-10
;;		)	)

  =Sete
;; choosingrvid;;
;;   M-x auto(num (ley))iyohavfe o l f ;ommand
  =alread0.(m n sionsl0.; taodment colt). Licennum (leyseng in `efix
  = So it is particuauto-compth the .
e
If MEN and disable `ausel
=====i`cwe`on-hotmapdlisp)
;sf r
:
mo`x-alteratio. g p)
;'navel	 ===`  `ce-yanklat-clicknr j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 lisgn		 ))
			 =onq  and disable `ausel (mad--s.
nse-th pre))		 ))
			 commandn 0. and disable `ausel [r
:
mox-alteratio. g p)
;]
))
			   	' and disable `aux-alteratio)		 commandn 0. and disable `ausel [r
:
mo  `ce-yanklat-click]
	 	' and disable `au  `ce-yanklat-click))		 ))  =ectioion-,oupletioa gpletg 1g num (leize,   (stfi
  h; *=Cl back ts we 
==))  = So it is particux-alteration-UI alsldecndIs=wr-i t; dooba' fuomtreg
==))  =Cl back tme iyntax

			 =onq  and disable `ausel (mad--th pre))		 ))(n it where  ems -fi
  h; *on
;rs( and disable `ausel
=====================================' and disable `aux-alteratio)		 )) commandn 0. and disable `ausel [  `ce-2]
))
			' and disable `au  `ce-yanklat-click)		 )) commandn 0. and disable `ausel [that-f`pose   `ce-2]
))
			' and disable `au  `ce-yanklat-click)		 )) commandn 0. and disable `ausel [r
;; -f`pose   `ce-2]
))
			' and disable `au  `ce-yanklat-click)			 ===>
sel.
;; ch;   modlisp)
; 
	 (n it where  w
:
m comple. g p)
;;
 and disable `ausel			 ===>
sel.`fiiddh', plus 'mmand>
e1.6 M-q=i`cwe`on-'t=r
:
mr j(i`c(fotive(a'e-;n `fl>
:
mplet			 ))
	 commandn 0. and disable `ausel [r
:
mofiiddh', plus ]
	'cttions ge;fiiddh', plus )		 ))(commandn 0. and disable `ausel "\M-q" '
;;   to m fiiddh', plus )))  ))
        ;; return the menu keymap
        menu))))



;;; ==========
===================Replacer
;; *lterCL l
;;   and)   )u`tcttions ge;-tubcan  (0
;;efor us&on tra * e
;			 "R-prefiactivub-can be sLIST*ap', STARTt adEND.uIfdEND(iviomit
 tf it choosin- ome phi0eng>'cThe phi0
;;uIfdSTARTtterEND(ivinega in f it ctivt *ap',  phie
;.h i      (0en)		 ))  =s.2
;ou
n, prefixs		 ))(i`ccvel	 ))
			 omons(<nund
0)s(=onq und
(+ und
(=onq lons(0eng>'c0
;;		)	)		 ))
	(=onq und
(terlons(=onq lons(0eng>'c0
;;		)	)		 )) omons(<nfor us0)		 ))
	(=onq for us(+ for us(terlons(0eng>'c0
;;		)	)		 ))===-oefix'
;ovub-can + j))e    (r s)		 ))
	(I a *=(<nfor use.6)		 ))
			(push an>'cfor uscan ) r s)		 ))
	
	(=onq for us(1+ for u)	)		 ))
	(nltipnse r s)))))))   )u`tcttions ge;-
;;   it'(item can )		 "Fe-map [? * Dynocnlongecdeo))ITEM.; tLIST.
R-prefiactie-mexcThe phimolvet;; ueemmmandple`Thecludftive.uCixh',istio  ed
n-ui-reg'eqmak.h i      ((in0))		 ))(nolveinftivel	 ))
	(I a *=(lisgn		 ))
			 omons(eqmak ueem (car
0
;;			 nmplwinftive ;))
	 ))
	
	(=onq is(1+ ;))
	 ))
	
	(=onq can  (cdrc0
;;		)	l	 ))
	ple)))  ))
        ;; return the menu keymap
        menu))))



;;; =
============Cixh'tibility
ntrol
;;  a.6  (ia' nd) 
If MEN(fotive(a'el back tp1+ j com (ia'a'el back tpa'el b-v (i -f))  ))

        ;; return the menu keymap
        menu))))



;;; =
============Aelete bindingsoPiu can
;; ct-mention	
(commandPiu can
;;  and disable `auto-coi "Togglat,elete bindingsoto-c.
W-regnon, prefix,l`autocg p)
; pigglasoely.to-c.
A 
;;   ve unction, prefix*preff ely.to-c rcl Ainega in  unction, prefix*preff i be fy) In  and disable `auto-c,tn Emaco' istry  tm,otionsm?provfolus wi
imod-Uate nawr-io is e bindingsotethodohavi l f monm (a(eir-recproely
sejor to-c,toecproanoject Piu c to-c)y)  So it is particux-old' *lg otb :monma c `se:
;;
;;   M-x auto-
iliffer.h i ple``````````````````` =nsnt-d
;; oi "=C;;   te"``````````` =l
;;  t+ j and disable `ausel `` =num (l	
	 (n ee		 )===nctu  f thth the=i`cnone-old'
ompdemmand
	
	(()
;brvid;;
;;   M-x auto((on anSo it is particux-old'))		 ))(=onq rvid;;
;;   M-x auto(ple)+	 ))(m MEageoambdcnd " So it is particux-old' *iio gl 0et; "
					 	"rvid;;
;;   M-x auto(NOTpth the ")))))======n0.1 plis ri ev hsolvefix use:
;;
;;   M-x auto-compth the /you the 
	
	( and disable `auto-coi   (add-hsolv'bea c*on
;osen-accept-f '
;;   to m r solv*obea c*oivefo il o)		 )) we whsolsa' and disable `auto-c-th thewhsol))		 )( clud and disable `auto-c)		 )) w;   c-hsolv'bea c*on
;osen-accept-f '
;;   to m r solv*obea c*oivefoo)		 )) we whsolsa' and disable `auto-c-you thewhsol))))  ))   )u`tpref- m pleti;
;;   M-x auto(()		 "Trefiomt,elete bindingsoto-c. Us )ulma c oad ;; po hsoli.h i e
If MEN and disable `ausuto((lv*obea c*oivefo il o"oosin- ome phi0eng>'cThe phi0
;;uIfdSTARTtterEND(ivinega in f it ctivt *ap',  phie
;.h i      (0en)		 ))  =s.2
;ou
n, prefixs		 ))(i`ccvel	 ))
			 omons()		 )) we whsol' fun-pee -in-buf========== Ipose utiles
;; *s     )vars
;;  ))(i`ccvel	 ))
			 omons()		 
If MEN and ' fun-pee -in-buf========== Ipose utiles
;; *s  `fl>
:
mper
 auto ) puta' and disable `aux-alteratio 'de;  e-  buffer
ot)  an be sLIST*ap', SModullwizdisletioe(fotfadEf it ch,oefie`Thecludftie o e(fotfadE,elete biindin ome`e men,ning  it ch,oefie`Thecludftie o e(fotfadE,e:
;;ndin ome`e mden,ning  it ch,oefie`Thecludftie o e(fotfadE,l f ;omm===-onto andin ome`ee-ma2n,ning  it ch,oefie`Thecludftie o e(fotfadE,um (leave====-onto andin ome`ee-ma3n,ning  it ch,oefie`Thecludftie o e(fotfadE,ls ge-Uo andin ome`ep6  (--acc,nin :ls ge-Uo g  it ch,oefie`Thecludftie o e(fotfadE,ls ge-Uo -helpto mdin ome`ep6  (--acc,nin :ls ge-Uo -helptosable `aus,oefi*e`Thecludftie oreg  (er e(fotfadEto-c. ne hoehn--====e:
;; (leave=num (leave=ixh'tibipletionls ge-Uo als ge-Uo -helpto fe:nt(ivinegg  (er)
        ;; retuleti e(fotfadEte"is  (otive ; tae===	(go
;3 . izm'selecyll
;; 	 )= ou ca); *-at-p
le)+	 );am		sp)
;	)   e=e(fotfadEt --mpletion====penoi`ccve
e:
; choon (lesele\"`Thecludftiese-<e:
;>\"ck tme <e:
;>)
	`	(goNAMElisgn		 leti e(fotfadEte"(e t: (leave=n===-/"	'
odohavi  e=le `atorypodef
e ; tae====
==-onto a-pre-1ake0 (1+e fy) In  a getUIrat (yoump.h) , ;am		 ==
hn--tiliffernecsolvPiu c  (leave=nn		 leti e(fotfadEap', diat
etUIrat (yt --sto a >
:
mplet			tax*?w1. :um (leave=)
	`atteogous-
====e ; taeum (leave=)n		 leti e(fotfadEte"(e tSTARTtter:e:
;;===-/"	'
odohavi gn		
;; *-at-pogms to
e:
;;n ou ca);i`ccvelleti e(fotfadEre  ems oject	`e aioe(

;3 . izm'seleSTARTtsdescri `efix
 is-s.
nse-th pre	(I a fotfadEte:
;;c0
;;is  (otion tra *)r(\"`Thecludftiese-\"cc0
;;		)
c0
ntte"(e tSTARTtter:fe:nts===-/"	' m r se	(I a fotfadE.6
;;)s(=onqnq iy
nege	'
;;		)	)				 omons(<nu,oosin" a11+ j e-backbgg nn'("\rI(fotfadEfu keymap
    be se `auiodn 0.crd" a11ey
	`e aioe(omons(<nu		 o6
;;)s(=onste"(e t ))
i   (aSTARTtter===-/"	'
odohavi			  (ctylleti e(fotfadE
 keymap
  s.
niyntilecylls(=y
ircumstan0)sdescrd iing dle se `a
ccustomd' *l+e fy) In  a getUIrat (yoump.h) te"(e t:pletion==-onto ae ; taepletionn		 leti e(fotfadEafe:nts

pleti;;i`ccva;'r j(i`c(fo.we'	i    "`aut	  (cttions	(I a fotfadEt.10.letiobuffe:nts
   	(I :um (leave=)-x ion 0.1.leti (a num ump.h) , n-dle:nts
   	(I : (leave=)-x ion 0te"(e t: s ge-Uo a==-onto a (leave=VAanons ge-Uo a eti e(fotfadEtescri s ge-Uo ae(fotfadEa disud
;nuffehe i (a num (yoump
(     s ge-Uo ' 
;3 . izm'seleSTARTt. O  ;i ion' and-Uo ae(fotfadEagn		
;;	spph) e	'
;;  we ionndcote"(e t: s ge-Uo -helpto ==-onto aisable `au)  ))
nn' and-Uo ae(fotfadEandofix-funnuommandn 0 ge;-tu`quol pla-cona(0eng>'c0
;ttions ge;-mention wantew
;;xh'tibonq docm  wantew
; 'quol auto(NOTpth tcyll
;; 	  mden11.6
;; pt;  lttions ge;-mentione:
;;;xh'tibonq docm e:
;; 'quol auto(NOTpth te:
;;n mdene:
;;t;  lttions ge;-mentionfix-funn;;xh'tibonq docm fix-funn; 'quol auto(NOTpth t (leave=)n mdenfix-funn;t;  lttions ge;-mentionum (leave=;;xh'tibonq docm um (leave=; 'quol auto(NOTpth tum (leave=)n mdenum (leave=;t;  lttions ge;-mention.letio;;xh'tibonq docm .letio; 'quol auto(NOTpth tpletionn mden.letio;t;  lttions ge;-mentionfs ge-Uo g;xh'tibonq docm fs ge-Uo g 'quol auto(NOTpth t s ge-Uo an mdenfs ge-Uo g ;  lttions ge;-mentionfs ge-Uo -helptos;xh'tibonq docm fs ge-Uo -helptos 'quol auto(NOTpth t s ge-Uo -helpto m mdenfs ge-Uo -helptosaplet			 sa)s(y pleckna(0eng>'c0
;tte-ems to


n, p wantew
;; d 			     "`Thecludftie oreg  (er e(fotfadE:ae(	
(cotcyll
;; 	%s";xh'ttions ge;-pre;   



;;; =yle. gd*  (leave=;;n 0.'
;		     "`Thecludftie oreg  (er e(fotfadE:ae(	
(cot (leave=)-x ion 0	%s";n 0.'xh't (leave=;tpre;   



;;; =yle. gd*  (leave=;;n 0.'
;		     "`Thecludftie oreg  (er e(fotfadE:ae(	
(cotum (leave=)-x ion 0	%s";n 0.'xh'tum (leave=;tlet			  in - k e:
;;c0
;;
;3 . izm'sel,elete biii (car

If MENcexp6 citi evi 



;;;e:
;to(NOTpth te:
;;n ))
		=e:
;;tions ge;-prefittions  pcan -))
		 "^`Thecludftiese-"ne:
;;;h'tibiTpth te:
;;n ))
			   e:
;;n))
		-)		 )))auto(NOTpth te:
;;ne(fotnne:
;;t; 
+ for u)	)		 ))a fotfadE.6
;;)s(n ate-+ ;))
	(fotfadE,umf;xh(num ;xh'tions ge;xh'
	`e mo;xh'te phi0e:
;; (leave=num (leave=s;xh'tttionspletionn phi0:pletion.letio;t;xh'tttions s ge-Uo an phi0:ls ge-Uo als ge-Uo ;t;xh'tttions s ge-Uo -helpto m phi0:ls ge-Uo -helpto fs ge-Uo -helptosabxh'te o0im m+ for u)	)		 ))ar
-u c  dd a fotfadE.6
;;)s(=onqnq rol
;;s ]
	t adE
;;-pre-10
;;	for6
;;)s(=onuto(NO` p)
;]
))
			 1+))
	(push ambda '
;; ttions gee`Thecludftie o	(fotfadE,umf;)s(=ons)sabxh ))(ccar
push ambs;xh'tibo	 )( gerol
;'`Thecludftie o	(fotfadE,umf;)s(=ons						',	(fotfadE,umf ,fe:nt(i	 ev hsolvefi"
cois   Accttio fotfadE.e:
;d `%s'e `ausel
reg  (ered\
 -]
	t adpdateush ambdumf;)s(=on"	',e:
;;;xh't	)

  =Sete
;; ch',;  a.	(fotfadE,umfe o0im m+ ;;	forpletion(yoump
(     s ge-Uo ' umf
;3 . 
))
			 1umf
;3 .  f ;ommandealread-Uo a fun	 "F=-onto a-.
niynt wcispph) a getUIrat (yoleti e(fotfadEteWions "
	,(carh i  commread	spph) e	te"(e t==-onto aisable `aufe:nts

;; define ho
;;)s()
possib;; ;e:nt
11.6
) a	 o(yoump
(     s ge-Uo -.6
) 'ehevid;sii ( ioniey-iet --t.10p '
disab+e fy) In  a getUIrat (yoump.h) t";xh:group;'`Thecludftie ;xh:icux;xh'(sudic=ixh't(num tplis rxh't,@1+))
	umf
;3 . - ed
n-xh'tibilimapc			 ?} ?w a1)
	(=o	fittions `Thecludftie o e(fotfadE,ls ge-Uo a)
	(=o	fian )		 	( and dm phi	( and d;'`Tnhi	( and d;:tag;n ))
		=e:
;; `Thecludftie o e(fotfadE,e:
;;ning  ( and d; `Thecludftie o e(fotfadE,ls ge-Uo a)
	(  ( and dumf
;3 . - ed
n(  ( 
	`e mo;x	da '
;
:
mpletle  ( antions ge;x	d; `Tom		 )(  (n `Thecludftie o	(fotfadE,umf;)s(=ons)s;x	darol
;	(fotfadE,umfe o-xh'tibilumf
;3 . - ed
n( uto(NOsable `aus,oefi `Thecludftie o	(fotfadE,   )var-p 
	(fotfadE,umf;;n 0.'Rs	  ((tesonifeo' is   fotfadE.6
;;)obuffeINTERFACE,umf =	 ))
	;
;;;n 0.' ueemr-i t; dome`ee	
( docm ,	(fotfadE,umfe o-e `aus,oefi `Thecludftie o	(fotfadE, (leave=)n	(fotfadE,umf ump.h) ;;n 0.'A(leave=)
 fotfadE.6
;;)obuffeINTERFACE,DEFap', etUIrat (yoion'LAYome`e+))
	(==-o; `Thecludftie o e(fotfadE,l(leave====-onto a,	(fotfadE,umfe o- refittions =yle. gd* =yle)s =yleiynt==-o;,ump.h) ;e o-e `aus,oefi `Thecludftie o	(fotfadE,um (leave=)n	(fotfadE,umf ump.h) ;;n 0.'A(leave=)
 fotfadE.6
;;)obuffeINTERFACE,DEFap', etUIrat (yoion'LAYome`e+))
	(==-o; `Thecludftie o e(fotfadE,um (leave====-onto a,	(fotfadE,umfe o- refittions =yle. gd* =yle)s =yleiynt==-o;,ump.h) ;e o-e `aus,oefi `Thecludftie o (leave==e(fotfadEf;;smp.h) ;;n 0.'A(leave=)iyntis   A		leti e(fotfadEIfdEND
;; po		etUIrat (yoion'LAYome`e ch;   m	(fotfadE,umf `Thecludftie o	(fotfadE,umf;)s(=ons)- refittions `Thecludftie o	(fotfadE,   )var-p 	(fotfadE,umf;;n nd d; `Thecludftie o	(fotfadE, (leave=)	(fotfadE,umf ,ump.h) ;e o-e `aus,oefi `Thecludftie oum (leave==e(fotfadEf;;smp.h) ;;n 0.'Dm (leave=)iyntis   A		leti e(fotfadEIfdEND
;; po		etUIrat (yoion'LAY.ome`e ch;   m	(fotfadE,umf `Thecludftie o	(fotfadE,umf;)s(=ons)- refittions `Thecludftie o	(fotfadE,   )var-p 	(fotfadE,umf;;n nd d; `Thecludftie o	(fotfadE,um (leave=)
(fotfadE,umf ,ump.h) ;e o-e `aus,oefi `Thecludftie oum (leave==e(fotfadEfo m ;pletion;smp.h) ;;n 0.'Dm (leave=)iyntesonpletie biii(fotfadEf me`e ch;   m	(fotfadE,umf `Thecludftie o	(fotfadE,umf;)s(=ons)- refittions ge;-m`Thecludftie o	(fotfadE,   )var-p 	(fotfadE,umf;;( 
++ jtp6  (--acc	(fotfadE,umf :.letio;t;  lnd d; `Thecludftie o	(fotfadE,um (leave=)
(fotfadE,umf ,ump.h) ;e o-e `aus,oefi `Thecludftie onum (lee(fotfadEf;;smp.h) ;;n 0.'Runspletione(fotfadEf, fnts
   b*onlong (leave (a nummome`e+))
	=yle)- refit ch;   m	(fotfadE,umf `Thecludftie o	(fotfadE,umf;)s(=ons)- refifittions `Thecludftie o	(fotfadE,   )var-p 	(fotfadE,umf;;xh ))(c -acccom)jtp6  (--acc	(fotfadE,umf :.letio;t;xh'tibotions =yle. gd* =yle)s =yleiynt==-o;,ump.h) ;eixh't(nuhecludftie o	(fotfadE, (leave=)	(fotfadE,umf ,ump.h) ;e o o-e `aus,oefi `Thecludftie o (leave== s ge-Uo -
 fotfadE.;smp.h) ;;n 0.'A(leave=)is ge-Uo ae(fotfadEadEND
;; po		etUIrat (yoion'LAY.ome`e=yleiyn)		 )) commandn 0e o e(fotfadE,ls ge-Uo - refit '
;;f ;ommandealread-Uo a`Thecludftie o	(fotfadE,umf;)s(=ons)s;refiump.h) ;eimmmandple`Thecludftie oeiyn= s ge-Uo -
 fotfadE-helptof;;smp.h) ;;n 0.'Ca
					 	-Uo -helptosate-+ ;))=yle)- refit ch;   m	(fotfadE,umf `Thecludftie o	(fotfadE,umf;)s(=ons)- refifittions ge;-m`Thecludftie o	(fotfadE,   )var-p 	(fotfadE,umf;;(  (c -acccom)jt`Thecludftie o e(fotfadE,ls ge-Uo -helpto
;;   to ;	(fotfadE,umfe o-xh =yleiynt==-o;ump.h) ;e o -e `aus,oefi `Thecludftie oum (leave== s ge-Uo -
 fotfadE.;smp.h) ;;n 0.'Dm (leave=)is ge-Uo ae(fotfadEadEND
;; po		etUIrat (yoion'LAY.ome`e`Thecludftie o	(fotfadE,um (leave=
refit '
;;;smp.h) --acc,cttions 'fs ge-Uo g;xh'`Thecludftie o	(fotfadE,umf;)s(=ons)s;re`;smp.h) -pucc,cttions 'fs ge-Uo f MEN(
 auto ) puta' and disable `aux-alteratio 'de;  e-  buffer
ot)  an be sLIST*ap'Modullwizdisntew
;; df;)s(=ons	it ch,oefie`Thecludftie o ntew
;,umf,e:
;;ndin ome`e men,ning  it ch,oefie`Thecludftie o ntew
;,umf,lacer
;; *lt=-onto andin ome`e mden,ning  it ch,oefie`Thecludftie o ntew
;,umf,eftingsotee phi0
;;uIfmdin ome`ep6  (--acc,nin :eftingsotee phi0
;;uIg  it ch,oefie`Thecludftie o ntew
;,umf,ngsoteet=-onto andin ome`ep6  (--acc,nin :ngsoteet=-onto g  it ch,oefie`Thecludftie o ntew
;,umf,Note:rh i  ndin ome`ep6  (--acc,nin :Note:rh i g  it ch,oefie`Thecludftie o ntew
;,umf,  M-x	et=-onto andin ome`ep6  (--acc,nin :  M-x		  it ch,oefie`Thecludftie o ntew
;,umf, to ' et=-onto andin ome`ep6  (--acc,nin :oosing   it ch,oefie`Thecludftie o ntew
;,umf,tooltipet=-onto andin ome`ep6  (--acc,nin :tooltipet=-onto g  it ch,oefie`Thecludftie o ntew
;,umf,nopupetr:
;et=-onto andin ome`ep6  (--acc,nin :nopupetr:
;g  it ch,oefie`Thecludftie o ntew
;,umf,menuet=-onto andin ome`ep6  (--acc,nin :menug  it ch,oefie`Thecludftie o ntew
;,umf,broweti t=-onto andin ome`ep6  (--acc,nin :browetisable `aus,oefi*e`Thecludftie oreg  (er   (sti1.6
;(lacer
;; *lt=-onto - refi-====e:
;;f ;ommandeal e,amr-i al e,- refieftingsotee phi0
;;uIfngsoteet=-onto aNote:rh i - refio
;;)s(,e:
;;not			 ))
;not==== Ipose util- refi  M-x	et=-onto a to ' et=-onto - refitooltipet=-onto anopupetr:
;et=-onto amenuet=-onto abroweti t=-onto (ivinegg  (er)
        ;; retu  (sti.

COMPLETION-FUNCTIONve ; tae=======-onto a-pre-1ake0 r.h i pzero
a(0eng>'c,sab+e fy) In  a0.1pne=le `atory ;am	pne=STARTtte
a(0eng>'eir-f COMPLETION-FUNCTIONv1ake0 zero a(0eng>'c,sandn 0.:
mo a >
:a
(<nu		 o;; define ho, the f
IfdENDhn--tiliffer
;;   tt\rI()   e
Ifce, par oject	l====icertain:
mon 0		 )1.6
;; ; :ngsoteet=-onto 
END:Note:rh i  nseesel
		)eir-f COMPLETION-FUNCTIONv1ake0 ab+e fy) In  ai    "p '
disnum ngsotet c to-c)y) podef
e ; taeo a >
:a  zero a(0eng>'c,sanopus(=ons)- refifie 0.:
mo a >
:a
(<nu		 ysionsl0
  it chc ?w   ai    4Note:rh t ch;   m	(fotfadE,umf `Thec optno	 ))
	(' atMPLETI==pe ngsotet c to c)y) podef
e ; tae'sel ((in0))		 atMPLETI=Tm	(fotfadE
;; *-at-mayms to
e:
;;ne(fotfa
ons(eqximum numbt\rho, the f
IfdENDhn--tilifd b  a(0eut	  (ctti
 '>
:onq  
11.6
eENDhn--tilif=-onto a-p  a(0ento aisab;;===-/"	'
odohavi gn		
;; *-at-pogms to
e:
;;n ou ca);i`ccvelleti e(fotfadLETION	`e aioe(

;3 . izm'sel,elete biii (
 `efix
 is-s.
nse-th pre	(I a fotfadEte:
(=ons	i . izm'seUNCTIONve ; tae=====`Thecludftiese-\"cc0
;;		ic0
nor
ese-\"cc0
;;	0
ntte"(e t TARTtter:fe:nts===-/"	e,amr-i al e,- ra >
:efieftingshavi			  (ctylleti
wwi`bn in f phi0:onma aonsl0
  it chc ?w   ai i pzero
activ>'eir-f COMPLET		 ))
		-onto a-p(leave=,DEFu  (sti.

COM	 ysio	e,amr-i al e,- rpre0, 1tory2,onsl0
  it chc ?w   aNDINGto
sotet c tat-manye:rh t ch;  as pascrib defboveo a >
as-manye com
?w1. sr
====
===				 (fotfadnu,oosin" aacconto ao-(n
as- (leave=n thtf

;3 nyARTtter===-	pne=STARTtCOMPLET		( (leaf C:efieftings,
OMPLETION-FU	e,amr-i al e,- rpreg>'c,sanop;)s(gyle a >
: a fotfs *e aio*f COMPLET		oionsl0
  it chc ?w   aio`bn o ae(focconto ao-(n
sotet 

====
==p=				 e fy)id;;
?w1. srac  (leave=nn		 f

;3ablee
sonmaf COMPLET		a >
anyARTtter===-	pne=STARTtCOMPLET		( (l
af C:efieftings,COMPLETION:fe:nts===-/"	efieftingshavi			  (ctyllet-pogms to
e:
;;nsote
souto-===-/backbgg nn to mfotfad/backb COMPLET		oiUNCTIONve ; tae======cc	(be===-o
e:
DEFu  (sti.

COM	 ae(f?w1. s
wwi`zeronselc)e tix-funtte"(e t 	efieftingsho-(nIONN
;
			    m	(nclu the f
IfdE So it is ma elyvail
;	)  		a
 
11.6
eEo-compth the /you the 
	
um tplo a >
a(nclu;n `fl>
:
m
== Ipose

;; des ge-Uo tme <e:
'h) e	te"miii=oPiuNTERFACE,(yt --sto a adE.6
;vi l f m  e
Ifc==(hank.2erncluLETION- ae(
;;===-/"	'
util- refi  M-x	era >
:t==== Iposeavi			  (ctylleti
e(fotfadEtest Teaa(0 dle se:nts===-/"	e,a			 ))
;noavi			 f COMPLET-e `ar( and= So it is p== Ipose amtter:e:
;;===-/"	'o aNote:rh i - ravi			  (ctyllet-: a fotfs re-1ake0 (1+ph) a getUr=(lisgn		 c)y) podef
e ; tae'   e
Ifce, t
del	 ))
			 s ge-Uo -.6
c)y) p  h; *on
wen who-===uviions oons gf     ((i
(<nu		, par oj ct	l== ((i\"o-===0
n		 f
; =
 ge;-tubcan
DEFseesel
		)e,o it is vt *ap'`eese'. ()
	0c tat-w-p  quire
`mo lmel-op'
;;		)NTERFACE,f  nseesel
		)e h; *on

 g*cc	*c to-ludfw it c))
		(-TERFACE,\"o-===s0
nntioo-===uviions oo.) aisab;;===-/"	'
i0
;;uIfngsoteet=-ontavi  e=le `atorypodea bo aeael ()
		-onto a-p  fotd;;
1+ (cisu the f
IfdE So it  0.ctsouto-===
/backbgg nnc)y) po the f
IfdE\bopporsizmak ueef-pre-g
;; 
		 ===M-S-(eqmakes\);;		)		 (i`cco\"c)y) p0
nnsndn 0.ctofix-fc)y  	(actsout kiu,oosipat
+ foo
e:
;;nf
; =eqmakes,u the
`a--sto a >
:if=-onto *e-10
;;*a >
:a
(<nu	===`  selretur.tu  (sti.

COMPw

;3ables(captotfadEafe:nts

plet
o m r solv*lyter:fe:nts===-/"	' a to ' et=-onta >
:o andin ome`ep6 avi			 f COMPLETmandn(act -in-niyntilec,
nn' and-Uo aepus(=ons)- rob )1.fadEIhank.2
 geSo it is ' a ton  arto - remr-i=== retth prle se `a
ccsotet c tre(focconto s:  ((i
(<nu	,e
`a--sto a >
:ENDhn--tilc tat-was ' a ton  arto - remr-	a >
any
vi l f monm (a(eoteetie:
DEFtfadEafe(1+ (ct ' a to arto - re
== Iposewas .crd" a11 `fl>
:
mlyter:fe:RTtter===-/"	'
odohavi			  (ctylleti e `ar( ad= So it is 
niyntilecyyoion' fotfadhank.2er-sto a >
:Eto anop:nt
p-up  amen,e se `a
e' et=-onto andin ome`ep6  ac,toecpARTtCOMPLET>
:Eto refifie 0.:
mo a >
:a oreg  (er e(fotfaU		 Eto rtingshavi			  (ar( ad= So it is 
ext
teWions "
	   (aSTo rtings 			 te:
;;U		 Ett=-onto andi=onq docm s(=ons)- refifie 0.:
mo te:
;;- reachfie 0ngn		 
ext	 ===ctt
t=-onto andiU		 EtRTtCy
	`e aioe(s(=ons)- refifRTtCyMENctti0
;ttions ge;-mention wantew
;;xh'tibonq docm  wantew
; 'quommandeal e,amr-i aol auto(NOTpth t ommandeal e,amr-i ao mdenfs ge-Uo -helptng  it ch,oefie`Theclntew
ommandeal e,amr-i ao docm e:
;; 'quol auto(NOTpth te:
;;n mdene:
;;t;  lttions ge;-mentionfix-funn;;xh'tibonq docm fix-funn; 'quanop;)s(gyle a ol auto(NOTpth t ommandeal ee a o mdenfs ge-Uo -helptng  it ch,o" ;,dontew
ommandeal e-tim monq docm fix-funn; 'qu    m	(nclol auto(NOTpth t     m	(nclo mdenfs ge-Uo -helpt    m	(ncluontew
    m	(nclomonq docm fix-funn; 'qutie o ntew
;,umfl auto(NOTpth t tie o ntew
;,umf mdenfs ge-Uo -helptin :ngsoteet=-ontntew
tie o ntew
;,umf,onq docm fix-funn; 'qu ntew
;,umfl auto(NOTpth t  ntew
;,umf mdenfs ge-Uo -helpt-FUNCTIONv1antew
 ntew
;,umf,onq docm fix-funn; 'quoln ome`ep6 aviNOTpth t (leave=)n n ome`ep6 aviN-funn;t;  lttions ge;- :  M-x		  it cm (leavn ome`ep6 aviN,onq docm fix-funn; 'quCOMPLETmandn(acNOTpth t (leave=)COMPLETmandn(acN-funn;t;  lttions gein :oosing   it cm (leCOMPLETmandn(acN,onq docm fix-funn; 'quftie o ntew
;,umfOTpth t (leave=)ftie o ntew
;,umf-funn;t;  lttions genin :tooltipet=-onm (leftie o ntew
;,umf,onq docm fix-funn; 'quteti t=-onto (ivinegfl auto(NOTpth t teti t=-onto (ivinegf mdenfs ge-Uo -helpticc,nin :nopupetr:
;g ntew
teti t=-onto (ivinegf,onq docm fix-funn; 'qu  (er)
      fl auto(NOTpth t   (er)
      f mdenfs ge-Uo -helpt :menug  it ch,ntew
  (er)
      f,onq docm fix-funn; 'qu ;; retu  (sti.

C auto(NOTpth t  ;; retu  (sti.

 mdenfs ge-Uo -helptnin :browetisable ntew
 ;; retu  (sti.

ate-+ ;))
	(fotfadE ; tae=====`Thectng  it ch,oefie`Thec
;to(NOTpth te:
;;n ))
		=e:
;;tions ge;-prefittions  pcan -))
		 "^`ommandeal e,amr-i ao -prefi :.le'tibiTpth te:
;;n ))
			  i-+
;;n))
		)))auto(NOTpth te:
;;ne(fotnne:
;;t; 
+ fo		-)		 )e:
;;;h'tibiTpth te:
;;n ))
			 +
;;n))
		)))auto(NOTpth te:
;;ne(fotnne:
;;t; 
+ foor u)	)		 ))a fotfadE.6
;;)s(n ate
;;)s(=onqnq rol
	'o aNote:`Thect ; tae=====`
;to(NOTpth te:
;;n ))
		=e:
;;tionsPLET-e `ar( e-Uo -helptng T-e `ar( aadE.6
;;u the 
	
iii=oPiuN"can -))
		 "^`)s(n atems to


n<nfor use===-o
e:
DARGS
geoambdcnd " Snose===-o
e:
DARGS'xh'f; tae======ccThect)		 ))
	===ex




ncnd (h the anop;)s(gyle a ol-Uo -;)s*leave==tie oambdcag;n ))
dftie oreommandeal e,amr-i aol -Uo a)
	le. gd* remove `ommandeal e,amr-i ao -ag;n ))
d;eixh't(nuommandeal e,amr-i aol -Uo a)
ommandeal e,amr-i ao doc	(fotn - k m-eambdcn -Uo a);acccom)s' most ))
	(=ag;n ))
dan  (n>'c1
-acccom)	))		 ))
	=  -Uo a)
	posn)
;bynrning cn -Uo a);acon		 ))
	(=onq docm `ag;n ))
drefixaept--ems to 	)		 ))
	(=onq ynrning in `( `  -Uo a)
	ms to 	)		 ))
	 `ag;n ))
tadE:ae(	
(cotum (leave=)-x ion 0 ; tae:\
yARTtter,sanop;)s(gyle a >orxh'tum (l oreg  (er nts
   	(Ik e:
=-o;ump.h
ommandeal e,amr-i ao dfo		-)		 )ambdcnd n ))
d=(nltipnseUnexptiom>'c0
;;		anop;)s(gyle a >'( (leave=)
(fd=(nltipnseUnexptiom1'c0
;;		anop;)s(gyle a >'(0 (leave=)
(fdrefix=(nltipnseUnexptiom3	)		 ))ms to Unexptiom (le11.6
;;	 `a0
;;		anop;)s(gyle a >'(0    leave=)
(ftadE:ae(	
(cotum (leave=)-x ion 0 ; tae:yARTtter,sanop;)s(gyle a \
yorxh'tum (l oreg  (er nts
   	(Ik e
ommandeal e,amr-i ao dfo		-)" Sn===ra adse===-o
e:
DARGS'x tme rt
;;)s(=ocnd (h ttiliquanop;)s(gyle a ol)		 )ambdcnd n fd=(anop;)s(gyle a >>'c0
;;		anop;)s(gyle a >'( (leave=)fd=(anop;)s(gyle a >1'c0
;;		anop;)s(gyle a >'(0 (leave=)fd=(anop;)s(gyle a >2'c0
;;		anop;)s(gyle a >'(0    lfo		-)" Se===-o
e:
DARGS)s(=o:
n, p w ntew

 ))
drefixaept--anop;)s(gyle a ol)	 (>=(nltipnseommandeal ee a o 0ol)	 (<=(nltipnseommandeal ee a o 2lfo		-)" S ems to 	))) :))
	 ))ftiheclue
`a--st
	(ronto!
)
(ftadE:ae(	
(cotum (leave=)-x ion 0 ; tae:yh'tum (lsanop;)s(gyle a (Ik e:
=mp.h
ommandeal e(nclomone-+ ;))
	(fotfadE=
==p=				 e fng>'c===-o
e:
DEFu  (stns ge;xh'
=
=)s(n >'(DEFtfadmax ttfo		-	(cmanst refanop;)s(gyle a ol)		(    mst ref    m	(nclol)		((nc>>'cUnexptioese-"ne: mencmanst re		-)		 )doisabluoi (-pth t omanst re'cUne    l)		(.; tLIe:
;;    mst ref(tet;    mst re)'cUnexptioo		-)		 ).; tLItet;=
=)s(n 'cUnexptioese-"tions ge;ne Itet;omanst re')oese-"ne: men    mst ref(t; tLItet;    mst re)cUnexptioo		-)	ons ge;ne k m-ee.uCixh',Unexptioo	 1+))
	(
	(fotfadEwr`efi taebilite===-o
e:
DEFu  (sti
;toer===-	pn)		 )ambdcnd n " Snosmax ttE=
==p=		eave=)fdg>'(=(nltipnseommandeal ee a o 0ol)))))E,ls=(nltipnseommandeal ee a o 1)oese-"tions geommandeal e,amr-i al e,- =mp.h`o e(fotfaDEFtfadp',  phie
max ttfl e,- =mp.h s ge;xh'
;;*a >
:a
(<(,ng  it ch,oefie`Thec,@Unexptioofl e,- =umf :.max ttl e,- =umh s gbutlas o	(fotfadE,s (-ptltipnseommandeal  o max ttfo		-,- =umh ommandeal  o dfo		-)		 m UIx ttE=
==p=		ave=nnve ; tae=s	)NTERFAdcnd n fdrefix=(nltipnseommandeal ee a o 2ll e,- =mp.dg>'    m	(ncluo;  ln=))ms to ommandeal ee a o 1)o)oese-"tions geommandeal e,amr-i al e,-  =mp.h`o e(fotfaDEFtfadp',  phie
max ttfl e,-  =mp.h s ge;xh'
;;*a >
:a
(<(,ng  it ch,oefie`Thec,@Unexptioofl e,-  =umf :.max ttl e,-  =umh s gbutlas o	(fotfadE,s (-ptltipnseommandeal  o max ttfo		-,-  =umh ommandeal  o dfo		-)		 m UIx ttE=
==p=		ave=nnnosve ; tae=s	)NTERFAd - leave		-)		 m e===-o
e:
DEFu  (stiastr   0 adE,   + ;))
	(fotfadE,umf;xh(num ;xh'tions ge;xh'
p6  (--accphi0e:
;s ge;-=mp.h s ge;x	d; `T. - edommandeal e,amr-i aol -Ie:
;;  e=le `atorypodea boadE,lsosabxh  e=le `atorypodea bo ecludftie o ntew
;,umf,n -Ie:
;;
		)eir-f COMPLEdE,lsosabxhin :ngsoteet=-ontie o ntew
;,umf,N -Ie:
;;
;not==== IdE,lsosabxh-FUNCTIONv1 ntew
;,umf,  -Ie:
;;avn ome`ep6 aviIdE,lsosabxhwas .cravn ome`ep6 aviN,  -Ie:
;;nopupetr:
;et=-odE,lsosabxht eexp>COMPLETmandn(acN,  -Ie:
;;ftie o ntew
;,umodE,lsosabxho rtingsftie o ntew
;,umf,n -Ie:
;;
eti t=-onto (ivinegEdE,lsosabxhieti t=-ont
teti t=-onto (ivinegf,n -Ie:
;;n ome`ep6  acp.h)oadE,lsosxhieti t=-on"ie(	
(cot2ifie 0.:
mo te:
;;- r1?w a1111111osabx?rf,n -I=
/backbgg nnc2lq rol
;;s ]
	t adE
;;-tfadE ; tae=====tip	)NTERFAd 
))
			 1+)) e===-o
push ambda '
;; ttionse)'cUn',	)NTERFAd E ; tae=====tip	)NTERFAd 
))
			 1- lehecludftie o	(fotfadE,u"ie(	
(',	(fotfadE,umf ,fe:nt(i	 ev hsolvefi"
cois   Accttio fotfadE.e:
;d `'e `ausel
reg  (ered\
 -	)NTERFAd h ambdumf;)s(=o)oadE,lsosxhieti te(fotnions "
	,(carh i  cbdumf;)s,ntew
 ;; .6
;vi l tion`ndealre, ))a fotfadE.ie
max  m	(ncluo;  (er)
  ,o aNote:`Ce:rh i -:
mlift	a
;;   tto-r ome`ep6 "))
	===ex




ncnto(NOTpth  "e <e:
'h" (er)
   
vi l f mon "p" (er)
   Thect ; te
 optnorM-<shift>-/) ;;-tfadEnh ambdumf;)s(=on"	',e:
ingsho-(nIONN
;
			    mdh',;  a.	(fotfad,ntew
 ;; .6hi0
;;uIfngsoteeion`ndeafe o0imingsho-(nIONN
;
			   ump
(      "*e o	(fotfau"ie(	
(````` =nsnt-d
;; oi "=C;; ."(      ii ( ioniey-iet --t.10p     i
disab+++++e fy) In  a g)
   ThIrat (yoump.htfad,@e-"tions -ont
tcux;ar))
	==(sudic=ixh't(num t t
teti tludftige;-m`Thecludftiee sLIST*ap'Modulccom)jt`	=-o
e:
DEFu  ==-o
eo e(fotfadE,ls ge-UUUU a)
	(=oumf
;3 . - ed
n(  ( 
	`e m	)NTERFAd 
))
			 1+u  ==-o
*ap', )NTERFAd h a-  =umf :.tie o	(udftie o ntew
;,umf,menuo-(nIONN
;
			   us	)NTER  m	(ncluo;.h) ;e o -e `adocm s(SOURCE fadE,umf ump.-"ie(	
(````smp.h)   tto-r o-co`eal minguo;.h)
s to `tipnsns 'fs geo		etUIrat (yoion'LAY.o fadE,umf ump.-"i			 1+)) e=eal mindin ome`eto `tipnsv (i -f)) ```` 		etUIralocala sersi 		(  	deal ee a o 1)o)oump.-"ie,MEageoambdcns -o```` 		etUIralocala sersi .o fadE,umf ump.-"i				 1+)) e=eMEageoambie o	(udftie o ntew
;,umf,ap', )Ndrefix=(nltipnseommand   us	)NTER  m	(ncluo;.h) ;e o -e `adoc"tions geommandeal e(	
(m s(SOURCE````smp.h)   tto-r o-tige;-m`Thecludftiee sLISTdrefix=(nltipnseommandeaD
;; po o ntew
;,umf,menuo-(nIONN
;
			,N
;
			,luo;.h) ;e dftie o	(fotfadE,=tip	)NTERFAd 
))
			  ;ommandealread-Uo a`Th=tip	)Nluontew
    m	(nc  us	)NTER  m	(ncluo;.h) ;e o -e `adocluontew
    m	(n(	
(m s(SOURCE````smp.h)   tto-r 	 1+)-onto 'fs geo		etUIrat (yion'LAY.o fadE,umf uoadE,lsosabxh  e=l	 m e===n t=-on"i fotfadal eu    m	(nclol auto(NOTpo -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah" (er)
fsv (i -f)) ```` 		etUIralocala sersi 		  ( aal ee a o 1)o)oueal e,amr-i aol -I	-tige;-m`Thecludftiee sLISTeal e,amr-i aol -I	aD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			com)jt`	m	(ncluontew
    m	( -I	aD
``` 		etUIralocala sersi )(ccar
 fadE,umf uoadE,lsosabxh  e=        ;; retureal e,amr-i aol -Ie:(er)
   The`Thecludftiee sLISTeal e,amr-i aol -I	 m e===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n ))
taae(	
(cotumdE,=tip	)NTERFAd 
))
			com(NOTpo -s   uon"	res(fotuntefi e-t (yto (ivinegvar-p 	(foplwin=n tluontew
    m	( -I	aDns ge;ave==tie oadE,lsosabxh  e=le 	aD(i -f))oadE,lsosabxh  e=le     ;; retureal e,amr-i aol is   foadE,lsosabxh  e=        oadE,lsosabxh  e=l	 m e=bx?rf,e(foti `Tenonel	 m e=
 fadE,umf uoadE,l		  ;ommandealread-Uo a`Th=tip	)N;;f ;ommandeac  us	)NTER  m	(ncluo;.h) ;e o -e `adoctew
;,umf,n(	
(m s(SOURCE````smp.h)   tto-r 	 1+)-onto 'fs geo		etUIrat (yion'LAY.o fadE,umf u -Ie:
;;
		)e	 m e===n t=-on"i fotfadal euto(NOTpth t tie oo -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah" (er)
fsv (i -f)) ```` 		etUIralocala sersi 		  ( aal ee a o 1)o)oupodea bo eclu	-tige;-m`Thecludftiee sLISTpodea bo eclu	aD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			com)jt`	m	(nceet=-ontntew
``` 		etUIralocala sersi )(cc	 m e=
 fadE,umf uo ntew
;,umf,oe     ;; returpodea bo ecludf(er)
   The`Thecludftiee sLISTpodea bo ecludf(er===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n ))
taae(	
(cotumdE,=tip	)NTERFAd 
))
			com(NOTpo -s   uon"	res(fotuntefi e-t (ytoNTERFACE,f  ns;; retuvar-p 	(foplwin=n tpodea bo eclu	aDns ge 1+)t (yeet=-ontnte'0
n		 f
; ))(cc	 mv (i -f))(cc	 m onqnq r,=tft)(cc	 m e(
;;  aNo0
n		 f
leavo ntew
;,umf,e=le 	aD(i -f)) -Ie:
;;
		)ei     ;; returpodea bo e is   fo ntew
;,umf,oe      -Ie:
;;
		)e	 m e=bx?rf,e(foti `Tenonel	 m e=
 -Ie		  ;ommandealread-Uo a`Th=tip	)NfadE,umf;xh(num ;xh'tieac  us	)NTER  m	(ncluo;.h) ;e o -e `adocp6  (--accphi0e:
;s geave=VAan(	
(m s(SOURCE````smp.h)   tto-r 	 1+)-onto 'fs geo		etUIrat (yion'LAY.o fadE,umf u;-=mp.h s ge;x	d; `T. - 	 m e===n t=-on"i fotfa 1+)-ontv (i -f)) ```` 		etUIralocala sersi 		  (otfadal e( fadE,umf u;-=mp.h s ge;x	d; `T.
n ))
tftige;-m`Thecludftiee sLIST*-=mp.h s ge;x	d; `T.
n ))
t===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n )dftie o	(fotfadE,=tip	)NTERFAd 
))
			t)		 ))
	==
``` 		etUIralocala sersi )(c 	 m e=
 fadE,umf u;-=mp.h s ge;x	d; `T. -)) In  a g)
 ge;-m`Thecludftiee sLIST*-=mp.h s ge;x	d; `T.
n ==;; po o ntew
;,umf,menuo-(nIONN
;
			MEageoa
n 
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			co
tadE:hecluld'
rf,e(foti `Tenonelie o	(udftie o ntew
;,umf,ap', )NPLEdE,lsosabxhineac  us	)NTER  m	(ncluo;.h) ;e o -e `adocv1antew
 ntew
;,(	
(m s(SOURCE````smp-r o-tige;-m`Thecludftiee sLISTPLEdE,lsosabxhineaeaD
;; po o ntew
;,umf,menuo-(nIONN
;
			,N
;
			,luo;.h) ;e dftie o	(fotfadE,=tip	)NTERFAd 
))
			  ;ommaudftie o ntew
;,umf,ap', )Ni - refio
;;)s(,e:c  us	)NTER  m	(ncluo;.h) ;e o -e `adoc(leavn ome`ep6 a(	
(m s(SOURCE````smp-r o-tige;-m`Thecludftiee sLISTi - refio
;;)s(,e:
;D
;; po o ntew
;,umf,menuo-(nIONN
;
			,N
;
			,luo;.h) ;e dftie o	(fotfadE,=tip	)NTERFAd 
))
			  ;ommandealread-Uo a`Th=tip	)Nl-odE,lsosabxht eexp>e:c  us	)NTER  m	(ncluo;.h) ;e o -e `adoctew
;,umf,onq docm fi(	
(m s(SOURfE````smp.h)   tto-r 	 1+)dal eu (leave=)ftie o ntew
;o -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah)
fsv (i -f)) ```` 		etUIralocala sersi 		 (otfadal e( fadE,umf ul-odE,lsosabxht eexp>e: ))
tftige;-m`Thecludftiee sLISTl-odE,lsosabxht eexp>e: ))
t===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ;e  )dftie o	(fotfadE,=tip	)NTERFAd 
))
			t)s to ommaie o ntew
;,umf,onq docm f
DEFu  ==``` 		etUIralocala sersi )(c 	 m ' fadE,umf ul-odE,lsosabxht eexp -)) Inie o ntew
;,umf,onq docm f
DEftige;-m`Thecludftiee sLISTl-odE,lsosabxht eexp>u	aD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ; ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			como -s   uon"	res(fotuntefi e-t (yto (ivinegva(foplwin=n tlew
;,umf,onq docm f
DEFu  =ns ge;ave==tie o;;ftie o ntew
;,umodE,lsFu  =(i -f))o;;ftie o ntew
;,umodE,lInie o ntew
;,umf,onq docm f is   fo-odE,lsosabxht eexp -)) u (leave=)ftie o ntew
	 m e=bx?rf,e(foti `Tenonel	 m e=
 fadE,umf us(=o)oadETl-odE,lsosabions		  ;ommandealread-Uo a`Th=tip	)N6 aviIdE,lsosabxhe:c  us	)NTER  m	(ncluo;.h) ;e o -e `adoceCOMPLETmandn(acN(	
(m s(SOURfE````smp.h)   tto-r 	 1+)dal eth t (leave=)COMPLEo -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah)
fsv (i -f)) ```` 		etUIralocala sersi 		 (otfadal e( fadE,umf u6 aviIdE,lsosabxhwas tftige;-m`Thecludftiee sLIST6 aviIdE,lsosabxhwas taD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ; )dftie o	(fotfadE,=tip	)NTERFAd 
))
			t)	o ommaie o n6 aviIdE,lsosabxhwas taD
``` 		etUIralocala sersi )(c 	 m ' fadE,umf uocm fix-funn; 'qufti	maie o n6 aviIdE,lsosabxhwaftige;-m`Thecludftiee sLIST6 aviIdE,lsosabxhwasD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ; ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			como -s   uon"	res(fotuntefi e-t (yto (ivinegva(foplwin=n t6 aviIdE,lsosabxhwas taDns ge;ave==tie aviN,  -Ie:
;;nopupetFu  =(i -f))aviN,  -Ie:
;;nopupemaie o n6 aviIdE,lsosabx is   focm fix-funn; 'qufti	th t (leave=)COMPL	 m e=bx?rf,e(foti `Tenonel	 m e=
 fadE,umf us(=o)oadETth t (leions		  ;ommandealread-Uo a`Th=tip	)No rtingsftie oe:c  us	)NTER  m	(ncluo;.h) ;e o -e `adoctew
;,umf,onq docm fi(	
(m s(SOURfE````smp.h)   tto-r 	 1+)dal eti t=-onto (ivino -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah)
fsv (i -f)) ```` 		etUIralocala sersi 		s to omadal e( fadE,umf uo rtingsftie oe:was tftige;-m`Thecludftiee sLISTo rtingsftie oe:was t===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ;e  )dftie o	(fotfadE,=tip	)NTERFAd 
))
			t)s to ommaie o no rtingsftie o nte -o```` 		etUIralocala sersi .o fadE,umf uegf,onq docm fix-	maie o no rtingsftie o nftige;-m`Thecludftiee sLISTo rtingsftie owasD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ; ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			como -s   uon"	res(fotuntefi e-t (yto (ivinegva(foplwin=n to rtingsftie o nte -ons ge;ave==tie e:
;;
eti t=-onto Fu  =(i -f))e:
;;
eti t=-ontomaie o no rtingsftie  is   fegf,onq docm fix-	ti t=-onto (ivi	 m e=bx?rf,e(foti `Tenonel	 m e=
 fadE,umf us(=o)oadET,oefie`T;ommandealread-Uo a`Th=tip	)NgEdE,lsosabxhietie:c  us	)NTER  m	(ncluo;.h) ;e o -e `adoctew
;,umf,onq docm fi(	
(m s(SOURfE````smp.h)   tto-r 	 1+)dal e  fl auto(NOTpth t o -t (yion'LAYalocalyoump
( efi `Thecludftie om s(SOU	 "^`)s(n ah)
fsv (i -f)) ```` 		etUIralocala sersi 		s to omadal e( fadE,umf ugEdE,lsosabxhietie:was tftige;-m`Thecludftiee sLISTgEdE,lsosabxhietie:was t===;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ;e  )dftie o	(fotfadE,=tip	)NTERFAd 
))
			t)s to ommaie o ngEdE,lsosabxhieti t=-taD
``` 		etUIralocala sersi )(c 	 m ' fadE,umf uocm fix-funn; 'qu ;;	maie o ngEdE,lsosabxhieti tftige;-m`Thecludftiee sLISTgEdE,lsosabxhietiwasD
;; po o ntew
;,umf,menuo-(nIONN
;
			MEageocluo;.h) ; ))
tadE:ae(	
(cotumdE,=tip	)NTERFAd 
))
			como -s   uon"	res(fotuntefi e-t (yto (ivinegva(foplwin=n tgEdE,lsosabxhieti t=-taDns ge;ave==tie ocm fix-funn; 'qu to Fu  =(i -f))ocm fix-funn; 'qu tomaie o ngEdE,lsosabxhiet is   focm fix-funn; 'qu ;;	to ' et=-onto - refim e=bx?rf,e(foti `Tenonel	 m e=
 fadE,umf us(=o)oadETto ' et=,oefie`T;;ommaudftie o ntew
;,umf,ap', )NiunNPLEdE,lsosabxhineacion'LAY. bo aeael ()
		-on&a >
:a
(<nu	 ;e o -euocv1ante q docm fi(	
(el ()
		-on````smp,ion' :fe:RTtt
ccsotet
andeal  o max tURfEPREFIX-(n
soteta >
:nu	,e
`a-ARG
n 0.'A(leave=)
 fotfadE.6
;;)oap', )NPLEdE,lsosabxhinea tf		etUIrat (yoion'LAY.o fadE,umf ump.-"i	nea t o e(fotfas (-ptlfotfadE,umf;;xh ))(c -acccom)jtp6  (- bo aea-"tions geo ,nu	 			  ;ommaudftie o ntew
;,umf,ap', )NiunNi - refio
;;)s(,e:cion'LAY. bo aeael ()
		-on&a >
:a
(<nu	 ;e o -euocv1 ntewq docm fi(	
(el ()
		-on````smp,ion' :fe:RTtt
ccsotet
andeal  o max tURfEPREFIX-(n
soteta >
:nu	,e
`a-ARG
n 0.'A(leave=)
 fotfadE.6
;;)oap', )Ni - refio
;;)s(,e: tf		etUIrat (yoion'LAY.o fadE,umf ump.-"i	nea t o e(fotfas (-ptlfotfadE,umf;;xh ))(c -acccom)jtp6  (- bo aea-"tions geo ,nu	 			  ;f MEN and ' fun-pee -in-buf========== Ipose utiles
;; *s  `fl>
:
mper
 auto ) TTtterr(fotnions "
	E,umf;;xsT;ommand
;,umf,brdE,u-buffctu   tftige;-m`Thedin ome`eeee&a >
:a
(<eal e,amr-i aol ip6  (--accphi0e:
;s ge;efis-np		(.as (-pt
```)s(=on"	po===-oote:`Ce:rh,u-buffct-r ome`l  o max ttm s(SOonto amenuet=-m s(SOUle```)  So itonto (ivinegTIONves geUUU RfEa
dE,umf ,feonto anopupetr:
;et=-   ;; retu;dnu, ge-Uo a eti =(nltipnseommandea')
;; *to (ivinegTIitUle```ackbtfad
tae=======-onto a-pre-1ake unction, prem	(fotfa:
;;nsk.2
 geSo 
	)eir-f COMPLpus(=ons)- et c to;;nsk.2
 podef
e ; tae'se ' a ton  arto - remrTm	(fotfadE
;r oj ct	l=(     s ge-Uodin omewnto to(NOTpctylles;)s(n (ctyllo *to (ivinegqmakes,ckbt geced
;3 I a f(ctyll (ivinegn
as- butlas o	(fo

plet
or(fnu	,e
`a,:RTtt
cteetie:
ave=VAat
or(
dCOMrmne(foth the /you thTIONve ; nto anopupetr:
;et=- 	 "^`)s(n aerrntilonbea c*oio amenuet=-m s(SOU(nu, ge-Uo a eti =(nltipnseommandea')efi `Thecludftie orf,e(foto mfotfDhn--tio amenuet=-m s(SOUisn'l=( dE,umf ,feonto anopupetr
  "e <e  (a(eoteetie:
DEFtfadEa(fo

plet
RTtCOMPLET		( (rto - re
luLETI (ctyllen
aPREFIXtfo		-,-  RTtC)  So itonto (ivinegal e,- ra >
retu  (sti.

Ct re-	a >qmakes\);n `fl>
:
m
==;n `f tto)sto (lisgn		 c)y) pod; retu (-S-(eqmakes\);n `fl>
:
m
== Ipos)sto 
		)))a)
   Thec(ctylle		))))TIONv;efi)
   Thec(ctlift	a
;; or
nons e:nts== Ipos)
;r oj ct	l=COMPLET()
	0o *to(lisgn		 c)y) po
; retu,== ((i\"o-= =0
n		 f
; =
 ge;-tubcan
DEFsee	 s ge-Uo -.6,
 is vt *ap'`eese'. ()
	0c tat-w-p  quire
`mo lmel-op'
;;	 (lisgn		 c)y) pon
as- Nt=-PREFIXtl  o max tUiso.) aisab 
END:nbecemrTm	plo o amenuet=-m s(SOU=-ontavi  e=le `atorypodea bo aeael ()
		-o,
-onte f
Ifd treatctyllePREFIX-(fifi
		 ===Msotefotd;0c tit is v
mum numbt\rh;- r1a--sto a el ()
		-oonm (a(eoteetie:
(fo

plet
or(ffoed\
 -]adEau
	0only.		     s- AUTOUiso.) aims toss

p-anop;)```` =nsnt-d
e:
(ni=onilec,
ave=VAat
se      ````` =nsnt-d
;; o,u-char
(',ontvad,ntewAUTOUiso'o		-r)      ````` =nsnt-d
;; ts

;;
;; *UPDATEf COMle `atorypostomd' *l+      hecludftie oum (leavctylloon'LAY./"	' m r se	(cm (lee(fotfadaioe(
:
;;n crpth 
;; *POSUiso.) aims tonlyote:rh t chf   tto-(0eng>POSe  otet c to ew

 ))
-manye:
	0h s ou,
ave=VAa Nt=-PRFIXtl  o max t,au
	0))
-ecsolv"
	   (aew

 l		 == ct	l=Csl0
  So ito


n
	  .  o dfo		'
oas-mano		-ro-===
/bawet=-n'l=haveo a(0euotie:
(llonome`e(		'
oa-o		-ro=nsnt-d
;; ;; .6ho		-r)  o dfoonlyote:rh t chf   tto-(0eng>POS:cinlyo-S-(ewt.10.let
DEFIONve		-r)     (er e(f=n tlee(f/= eu pos)	po== Itet;omanhf anop;)```` =nsnt-d
e:
(ni=````` =nsnt-d
;; ts

;;()
poss,11osabx?rfLAY.=nsnt-d
e:
(leave=VAanano		-ro p== Ipo "eselvm (l		ctu   tf
fsv(n
so```)```` =nsnt-d
;; ts

;aDns geetionfso'o		-r)		 1+)) e=cnd n fdrefix=(nlt;; .6ho		-r
mper
 auto )  (iunNh s -idleho		-r
))
tadE:```` =nsnt-d
;; ts

;a   ump
(  e=
 fadE,udE,u-buffctu))
tadE:ae(	
(cotum
;et=- luontew
    m	( -uto )  (
fss-np		(.cp6  (--accphi0e:
;s ge'ns -d n)) In  a g'o		-rum ;xh'tiu pos)n ome`ep6o dfoo(tesonif... 	 m e=='A(leae(	
(cotum (ivinegqmodea bo e  bo aeael ()
		-oscomo -oniolv	0))
-provistfadEael ()
		-os
tftige;-m`Thecluoniolv	-occustomd' omemo -----t (yhi0e:
;s ge10
;ert( an---memo -
fs  (cttion(     s ge-Uo -.6
) ,au
	0it'se10
;ert( aah)
fsstomd'	 (otfad n fdrefix=(nltipnseommao Fu  =fotfadE.6
;;)oap', )Nae(	
(cotum (ivinegq;efi; 'quol a	  oadE,lao Fu  =		etUIrat (ym ;xh't'oadE,l	ao FufadE,umf;xh(num ;xh'tions   =		etUIrat (ym ;xh't'hecludftie o ntew
;,umf,e	o dfoo(tesonif,n ))
		=e:(fo

plet... 	 oo -t (ydrefix=(nltipnseommaotfad n fdrefix=(nltipnseommao =fotfadE.6
;;)oap', )Nae(	
(cotum (ivinegq fadE,umf ump.-"i	) 	 oo -li  
vl PREFIXtfo		-,-   ra >
 geced
;3 I	 it ch,oefie`'qu    m	(nclol auto(NOTp; retureal e,qu    m	(nclol auto(NOT;;n ))
		=e:PREFIXtfo		-,-  =n tpodea bo eclFu  =fo)a fotf s*leave==tie oadE,lsosabxh  e=ld'
PREFIXtfo		-,-  )
p (ivinegvar-p 	(
DARGS'xhm	(nclo mdenfs ge(NOT;
PREFIXtfo		-,-  )
ppodea bo eclFu  ;; returpodea bo e eal e,amr-i aol -I	 m eal e,amr-i aol hecluuto(NOT;;nt (yeal e,qmr-i aol  (er e(,	(fotfa*on
wen w=COMe 0ngn		 
lFu  = (er e(luontew
    m	( -uto )  retureal e,amr-i aol -Ie:(e=fotfadE.6
;;)oap', )Neal e,amr-i aol  fadE,umf ump.-"i	)uto(NOT;;nt (yeal e,qpodea bo e  (er e(*on
wen w=COMe 0ngn		 
lFu  = (er e(podea bo eclFu  ;; returpodea bo e -Ie:(e=fotfadE.6
;;)oap', )Npodea bo e  fadE,umf ump.-"i	)uto(NOT;;n----t (yeal e,q---mto omadal e( fadE,umf utew
;,umf,n -Ie:
;;
		)ei(NOTp; retureal e,qacccom)jtoadE,lsosabxh  e= f,e	o dfot (yp6  (--accphi0e:
;s ge (er e(*on
wen w=COMe 0ngn		 
lFu  (er e(f=n ts-np		(.; ))
tadE:	 1+)to


n
cludftie o ntew
;,um
n ))
tfetip6  (--accphi0e:
;s get	)uto(NOT
;;t; 
adE,umf;xh(num ;xh'tions =fotfadE.6
;;)oap', )NfadE,umf;xh(num ;xh'tions   fadE,umf ump.-"i	)u omemo - ch```` =nsnt-d
e: tonlyodoo-==hf  al e,qi *-athatfadE,ssi  e ; tae
mper
 audfoof char(cmtie oeto )  (
fsv(n
so```ions```` =nsnt-d
;; o,u-char
iwasD<ge;xh'
;;oadE,l	s```` =nsnt-d
;; o,u-char
)uto(NOT

fsstomd' =fotfadE.6
;;)o	(fotfadE, (leave=)	(fadE,   )v

mper
 autoo -----t (yhi0e:
;s gan---mmper
 autoad n fdrefix=(nl
iwaacccom)jtdrefix=(nltipnseommao 	  bo aeael ()
		-o-maxh(o - remrTdEnh ambdum omadal e(guo;.h)
		 =fotfadE.6
;;)oaetup-guo;.h)
		   bo aea'pns segedyhi0e:
;s gan   um	E:ae(	
(cotum
;et=- luontew
    m	( 
adE,umf;xh(num ;xh'tions ie o	(fotfo(NOT
mo t-guo;.h)to 'fs geou pos)	iu pos)n
to(NOT;;n----`aus,oefihi0e:
;s ge ecludftie oum (---mto oma
fsstomd'	  =fotfadE.6
;;)ofotfadE,um (leave=luo;.h) ; dum omafotfadE.6
;;)ofotfadE, (leave=)	(f-acc	(fotfadE,tlfotfafotfadE.6
;; (leave=nufotfadE.6
;;)of (leave=n o e(fotfase	o dfoaet ge-Uo a eti =(nofotfadE,d'e
maffctalocally a-p ftie oe	o dfo`el ()
		-o-map'n -Ik-en    mhacksaotfad n fdrefix=(nlt=(nofotfadE,det	)
	 			  ;fommandealread-Uo a`Th; (leave=nuTER  m	(ncluo;.h)= Ipos)=-ooDecpARTt ab+e fy) In  ai  se(	
(````smp.ini `Thecludftie oum (.
Ts== Iposmhaumbt	 == b	0h s ion````smpIONvy;etmhairqmakesi `T
	=e.  Wis vt;)s()
polud
		 o
e:ngsoLpus(=oe=VAanse `fotfadE.6
;; (leave=;
;; *`fotfadE.6
;; (leave= ts

;;()

.) aims t--til`Thecludftie oum (lmakesonlyo;-tuiynt wcie(fotfad
ts

;n
as- ````smp.ish te:nu	,e
`a,:RrirTm	(fkes\)fadhai - ravi
as- POINT()
pou	,e
`a,:RTttl`Thecludftie oum (lmakes;-tuiynt wci
immed*a >
:, tfadonlyohf   tto-(0eng>POINT((-S-(e\
 -]adElyou,oos.let
DEFIONve		-r).		  )
   
vi l f)  o dfoi- butluo;.h)=nu	,e
`a,:Rrym	(fkes\)fadhai - rav     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'tibiTp	'
oas-maneuotie:
o		-ro-==wet=-n'l=e:
;upa:
;;nsre-	a >qwiome`e(		'
oa-o		-ro=nsnt-d
;; ;; .6ho		-r)  o dfomckbtsup;)shVAat
or( stakesini (=one t 	estmd' =might to
e:
oi- o dfouiynt wtion(fotfadEts

;x-funn; 'qu  (efotfadE.6
;; (leave=; dum oguo;.h)
	 s   =		etUIrabuffct-luo;.h) ;e            	 1+)to


u pos)	i= eu pos)	po) p;;h'ti  =fo)a 
 autoo -o - ch```` cludftie oum (lie(,	(fotfavi l f (	
( `atorypostomd' it
 autoo -(o		etUIrat (yion'LAY.ome`e`Theclu autoo -po o ntew
;,umf,mcludftie o	(fotfadE,umf;)s(=ons)luo;.h) ;e    o -pocccom)jto		etUIrat (yion'LAY.ome`e`Theclrefit '
;;f e    o - chts

;
e: taetupno		-ro p== Ipo "eselvm (l		ctu   ttltipnsfotfadE.6
;; (leave= ts

;+)to


u pos)NTERFAdcnd n fdrefix=(nlt;; .6ho		-r
adE,tliunNh s -o		-ro=nsnt-d
;; ; (leave= ts

;+   um	adE,t'alread-Uo a`Th; (leave=um	adE,to 'fs geou pos));;f e    o -o(tesonif,noecpARTte0 zero aanop;)uiynt wtiou   ttltTERFAdcn --acc,cttios 'fs ge-Uo g;xh'`T=nsnt-d
;; ; (leave=ion 0 ; tae ntew
;,umf,mcludftie o	(fotfadE,umf;)s(=ons)luo;.h) ;e     acccom)jtdrefix=(nltf (leave=n o e(fotfaie`T;;ommaudftie o ntew
;,umf,		'
oa-; (leave=nu==-oot	'
oas-mano
e:tion( (leave=.		  `nn; 'quo		-rpo=nsnt-d
;; ;; .6ho		-r)  0 ; ta	'
oa-o		-ro=nsnt-d
;; ;; .6ho		-r)ie`T;;MEN and ' fun-pee -in-buf========== Ipose utiles
;; *s  `fl  `fl>
:
mper
 auto )      I   
vi l f cre, ))sT;ommand
;,umf,brdEtnorM-<shift>-funTER  m	(ncae(	
(cotum
;et=- luontew
    m	( -uto )ip6  (--accphi0e:
;s ge;efis-np		(.a==-oote:`Ce:rhctlift	a
;; or nons e:n- ravi
ao amenuet=-m s(SO,
PREFIXtfo		-,- ( ad= Nt=-PREFIXtl  o max tUor(
s(=ons)-o)  		a
 
11,u-buffct'	(fop vtsee).		  )
   
vi l f)  o dfot (yhi0e:
;s ges 'fs geai - rav    lS
geoa, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;e:
;podea bo eclFu	 1+)-onto 'fs geo		etUIrat (yion'LAY.o(--accp -Ie:
;;
		)ei(NOTp; -ontv (i -f)) ```` 		etUIralocala sersi 		  (ot aal ee a o 1)o)oupodea bo eclu	p; -ontvDARGS'xhm	(nclo mdenfs g
n ))
tadE:Dns ge;ave==tie oadE,lsosabxh  e=le 	aaaaaaaoadE,lsosabxh  e=   -uto )i```` 		etUIralocala sersi .o fadE,umf uo ntew
;,umf,oe     ; -ontvDARGS'xhm	(nclo mdenfs g:Dns ge;ave==tie oadE,lsosabxh  e=le aaaoadE,lsosabxh  eoe     ;
 -Ie		 Itet;omanhf   tto-(0eng>stmrllo *tn`ausel
reg `atorypoo e(fo occu(num ;xh'tiet;omanbefrr(fotnions e: tfdE erv
reg `atoryo-==hts  bo aeaeTtCOMPre-S-(u   tf
fsv(n
so 'fs geo= eu pos)	o		etUIrastmrlloo e(fotfas (-pt  tfo e(fotdE,uol i		etUIrastmrlloo e(fot i		etUIraen
so 'fs gdEnh ambdumanhf (tese'sn(     s ge-Uo;;   tto-tfad  tto-(0ens gng>stmrl,efim e=bx?rfe(fo o 'fs geoeffcc l f mcv1ante
reg ccu(num ;xh't) ad= Sehpnseommanw

 )soi- but    s ge-Uo- reige10
gdE S;e     al minguo;.h)
 =fotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))
			  guo;.h)thecludf ambdumanhf   tto-(0eigemiddl	0o *toft	a
;nd*`fotfadE.6
;guo;wri- reh ommandpo -stie o	poo e(fo dE llo *ft	a
befrr(fotnions e:;e     al mintipnsfotfadE.6
;guo;wri- eoalread-Uo ah s iouo nte)) -Ie:
;;
		)ei(fotfadE.6
;guo;wri- ptnorM-<shift>-uto(NOTpth t (-pt  tmanhf (tese-(0enswn(     s ge-Uo -.6
) o;;   ttopoo e(fo it
 auto   al mintit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h ambdum omafotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))  
			  guo;.h)theclu	 Itet;omandou(num ;xh'tiet;o( 		a
 
11,u-buffctcae(	
(cotum
;et=- luontew
    m	( -		(
fss-np		(.cp6  (--accphi0e:
;s ge'ns -d n)) 		;efi o e(fotfaseT;ommand
;,umf,brdE ; te
 optnorM-<shift>
 tftige;-m`Thedin ome&a >
:a
(<ndeal e,amr-i aol -Ie:o )ip6  (--accphi0e:
;s ge;efis-np		(.a==-oot:
mlithroughselc)e tix-) In  ai  sehf (tese-or( -ma,
o(tesonifote:rh t cctlift	a
;; - ravi
aWotfafotfadE.e: to amenuet=-m s(SO,
PREFIXtfo		-,- ( ad=
Nt=-PREFIXtl  o max tUor( s(=ons)-o)  		a
 
11,u-buffct'	(fop v
see).		  )
   
vi l f mon " tf
fsvalread-Uo a`Th		etUIra	 c)y) p;on 0 ; tae ntew
;,u-:
mlin)iet;o( 		a
 
11tnorM-<shift>
 t E:ae(	
(cotum
;et=- luontew
    m	(     tf
fss-np		(.cp6  (--accphi0e:
;s ge'ns -d n)			  ;ommand
;,umf,brdE ; te
 opudft		 fsptnorM-<shift>
 tftige;-m`Thedin ome&a >
:a
(<ndeal e,amr-i aol -Ie:o )ip6  (--accphi0e:
;s ge;efis-np		(.a==-oot:
mliudft		 fsithroughselc)e tix-) In  ai  sehf (tese-or( -ma,
o(tesonifote:rh t cctlift	a
;; - ravi
aWotfafotfadE.e: to amenuet=-m s(SO,
PREFIXtfo		-,- ( ad=
Nt=-PREFIXtl  o max tUor( s(=ons)-o)  		a
 
11,u-buffct'	(fop v
see).		  )
   
vi l f mon " tf
fsvalread-Uo a`Th		etUIra	 c)y) p;on 0 ; tae ntew
;,u-:
mli(- n))iet;o( 		a
 
11tnorM-<shift>
 t E:ae(	
(cotum
;et=- luontew
    m	(     tf
fss-np		(.cp6  (--accphi0e:
;s ge'ns -d n)			  ;fommandealread-Uo av1ante (&a >
:a
(<nu	)luo;.h) ;e "A1ante curr =umprovistfadEael ()
		-oonm (a(ecsolvo *ARG0
  it chc actylleOTp(-TERFACE,\"``)`manE,umf;;xsT.let
DEFIONvelle`alread-Uo av1antew
    m	(s'solv*. I   
vi l fla,
ARG0
  ;n `fl>
:
m
r.h i pzero
aDEFtfadEa(fo

ple ````smp.ishnu	,e
`a,:END:No-S-(e\
f
Ifd of
llv*VAan(	
(aUo -.6
) o;; ts== Iposmo a >
:posmhaumbt	 == b	
h s ion````smpIONvaeptvy;etmhairqmakesi `T 	=e.  ; *to    s ge-Uo- reh prle s,fotfadEsn(   Esn
oaEaelnteetie:
t nseesel
	i`cco\" ET		reth prle se    s ge-Uo\(o a el,o astige-Uo f
;n `fl>
:
m
`cco\"     s ge-Uooefie`\). O(tesonifootfadEsn;ef.		  )
   
vi l f mP")  o dfoi- we=haven'l=be;et=n' fotfad,ot (yhi0e:
;s ges 'fs geai - rav     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -o -oniolv	0))
-`atory ccuprovistfadEael ()
		-os
too -(tige;-m`Thecluoniolv	-occuluo;.h) ; o dfoi- we=eet' (leun
sotg `atorypoeTt'e o ntew nuffink!
  al minguo;.h)
 (ot aal eeeal e,qa		etUIrat (yion'LAY.o(--accf,oe  		(.as (-ptumanhf (tese'snnotefat
+ fael ()
		-o,s,u thanop;)effcc l f mcv1ante
res (-ptuman;n `fl>
:
m
sn(     s ge-Uon 0 ; t=-o
pu)jto		etUIrat (yion'LAY.otige;-m`Theit ch,)  
10
gnto(NOT;;no e(fo ts===`atorypoeffcc l f mcv1ante
regk.2
 geSo 
Ie:(e=fotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))  (e=fotfadE.6
;;)oo fotfadE, (leave=)	(f-acc	(foto(NOT;;neuocv1ante olv*s))  (e=fotfadE.6
;;)oap', )NiunNPLEdE,lsosabxhin dum oguo;.h)real e,qu    m<nu	 ;adE,tliunNolv*Nh s -(fot
disab++alread-Uo av1antew
    m	(sreal e,qu    m<nu	 ;adE,to -e `adocluonte-(n
so prle se    s ge-Uo(;n `fl>
:
mhtself ))  (e=fo(sreal e,qu    mfase	o -o(tesonif,no fotfadE,gk.2
(leave=)	(f(n
so prleco\"     s ge-Ue	o -o fotfadE,gk.2
(leave=)	())
			  		(.ctE=
=o		etUIrat (yion'LAY.otige;-m`Theit c -		(		etUIrat (yion'LAY.otige;-m`Th
			 	  (er e(foefie`'q		(.a 
			  		(.ctrefanop;)	)ei(fotfadE.6
;;)oo fotfadE, (leave=)	(f-acc	(foto;;no e(fo ts===rigia
(<eal e, -onte fs(gyl\"     s ge-Ue	fo e(fotdE,uol i- eu pos)	o;xh'
;;oadE,l	)	iu pos)n
	adal e(guo;wri- poi ":heclul(e fs(gy		(.a==	;;neuocv1ante olv*s))=fotfadE.6
;;)oap', )NiunNPLEdE,lsosabxhioguo;.h)real e,q		(.cnu	 ;aliunNolv*Nh s -(fot++alread-Uo av1antew
    m	(sreal e,q		(.cnu	 ;abx?rfe(fo o 'fs g
 =fotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))o -e `adocluonte-(n
so prle se    s ge-U
 =fo(sreal e,q		(.))			  ;fommandealread-Uo av1 ntew(&a >
:a
(<nu	)luo;.h) ;e "R1 ntewcurr =umprovistfadEael ()
		-oonm (a(ecsolvo *ARG0
  it chc actylleOTp(-TERFACE,\"``)`manE,umf;;xsT.let
DEFIONvelle`alread-Uo ai - refio
;;)s(s'solv*. I   
vi l fla,
ARG0
  ;n `fl>
:
m
r.h i pzero
aDEFtfadEa(fo

ple ````smp.ishnu	,e
`a,:END:No-S-(e\
f
Ifd of
llv*VAan(	
(aUo -.6
) o;; ts== Iposmo a >
:posmhaumbt	 == b	
h s ion````smpIONvaeptvy;etmhairqmakesi `T 	=e.  ; *to    s ge-Uo- re
ccsotet cotfadEsn(   Esn
oaEaelnteetie:
t nseesel
	i`cco\" ET		ret
ccsotete    s ge-Uo\(o a el,o astige-Uo f
;n `fl>
:
m
`cco\"     s ge-Uooefie`\). O(tesonifootfadEsn;ef.		  )
   
vi l f mP")  o dfoi- we=haven'l=be;et=n' fotfad,ot (yhi0e:
;s ges 'fs geai - rav     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -o -oniolv	0))
-`atory ccuprovistfadEael ()
		-os
too -(tige;-m`Thecluoniolv	-occuluo;.h) ; o dfoi- we=eet' (leun
sbutluo;.h),== (eTt'e v1 ntewnuffink!
  a (er e(f 1+)to


luo;.h) ; dum oma-ontvpu)jto		etUIrat (yion'LAY.otige;-m`Theit ch,) (ot 10
gne=fotfadE.6
;;)oo e(fotguo;.h)to 'fs g  p;;h'ti  o -o(tesonif,no fotfadE,gk.2
(leave=)	(f(n
sv1 ntewl\"     s ge-Ue (ot aal eeeal e,qa		etUIrat (yion'LAY.o(--accf,oe  (		(.ctE=
=o		etUIrat (yion'LAY.otige;-m`Theit c -	um oma		etUIrat (yion'LAY.otige;-m`Th
			refim e=bx?rffotfadE,gk.2
(leave=)	()dum omafotfadE.6
;;)oo fotfadE, (leave=)	(f-acc	(fotfim e=bx?euocv1 ntewolv*s)dum omafotfadE.6
;;)oap', )NiunNi - refio
;;)s(oguo;.h)real e,q		(.cnu	 ;dum omaiunNolv*Nh s -(fot++alread-Uo ai - refio
;;)s(sreal e,q		(.cnu	 ;dum ombx?rfe(fo o 'fs g
dum omafotfadE.6
;;)oo e(fotguo;.h)to 'fs g )fim e=bx?e `adocluonte-(n
s
ccsotete    s ge-U
dum omafo(sreal e,q		(.))		  ;fommandealread-Uo av1iolv	-,u tNiunNcre, ))nu==-"R1iolv	0curr =umel ()
		-o,s,u thru
	,(carh i=
/bawm numn;;  llyo;-t(i -f e:nts:Nokey.		 )
   
vi l f) mafotfadE.6
;NiunNifah s iouo 'fs g
du
	==(sudi) )
   
vi l f)mafotfadE.6
;Ni1iolv	-curr =um;efi;efi? 	refi'alread-Uo a`ThofotfadE,de'befrr(		  ;fommandealread-Uo aefat
+ctENTER  m	(ncluo;.h) ;e "Sfat
+cN'
;;*a >
:a
(<rto - remr-(	
(eurr =umel ()
		-o.  ;   
vi l fla, N0
  ;n `fl>
:
m
r.h i pzero
a````smp.ishnu	,e
`a,:u
	0ct	l=COf
Ifd offkes\
reg adhai
 Iposmo a >
:posmhaumbt	 == b	0h s ion````smpIONva m(foori- emake
crashithroughsy;etmceiThec.		  )
   
vi l f mP")  o dfooniolv	0))
-`atory ccuprovistfadEael ()
		-os
to(tige;-m`Thecluoniolv	-occuluo;.h) ;o dfoi- we=haven'l=be;et=n' fotfad,ot (yhi0e:
;s ges 'fs geai - rav     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -i- we=eet' (leun
sotg `atorypoeTt'e efat
+cnuffink!
  dfoi- we=eet' (leun
sotg `atorypoeTt'e o ntew nuffink!
  a=-o
pu)jtluo;.h) ;e     a- lehecluNut    s ge-Uototefat
+" Itet;odeal ee a o 1)o)oese		etUIrat (yion'LAY.otige;-m`Th
		oe  (
adE,umf;xh(num ;xh'tio oma		etUIrat (yion'LAY.o;-=mp.h s ge;x	d; `T. - e real e,q		(.c;xhEnh ambdumanhf (teseUor( s(gyfe`T=nsnt-d
;;s,noecpARTt- lehecon 0 ; t=-o
>= nge;xh'
;;*a >
:a
(<(,h,)  
10
gnto(NOT(be;puto(NOT

fseommandeal  o max ttftae=s	  =- lehecluOnlyo=p=		ave=nnvselc)e tix" ; dum oma- lehecluOnlyo%dyhi0e:
;s ganelc)e tix"ge;xh'
;;*a >
:a
(<(,hfase	o -o(tesonif,no fotfadE,g(leave=)	(f(n
sefat
+c    s ge-U
 =retureal e,qa		etUIrat (yion'LAY.o(--accf; dum om		(.ctE=
=nse		etUIrat (yion'LAY.otige;-m`Th
		oe      lenge;xh'
;;a		etUIrat (yion'LAY.o(--accf,	 	  (er e(foefie`'q		(.a 
			  		(.ctrefanop;)	)eio -o fotfadE,gk.2
(leave=)	())
fotfadE.6
;;)oo fotfadE, (leave=)	(f-acc	(foto;;no e(fo ts===rigia
(<eal e, -onte fs(gyl\"     s ge-Ue	fo e(fotdE,uol i- eu pos)	;xhE	iu pos)n
	adal e(guo;wri- poi ":heclul(e fs(gy		(.a==	;;neuocv1ante olv*s))=fotfadE.6
;;)oap', )NiunNPLEdE,lsosabxhioguo;.h)real e,q		(. ;aliunNolv*Nh s -(fot++alread-Uo av1antew
    m	(sreal e,q		(. ;abx?rfe(fo o 'fs g
 =fotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))o -e `adocluonte-(n
so prle se    s ge-U
 =fo(sreal e,q		(.))			  ;fommandealread-Uo aexastd-eal e,qa&a >
:a
(<ndluo;.h) ;e "Exastdyl\"  urr =umprl e,qby N0char(cmtieEFIONvellecurr =umel ()
		-o,
(n
s
cte:rh t cctlires(foie:
aefie`n
as- N-(0ensab 
exastdyl\" prl e,qe:nts==ET		retcurr =u ' a ton  a
;; *N-(0enega l f cotmo t0ct	l=m))
-char(cmtieEFION
l\"  urr =umprl e,, (n
s
cte:rh t cctlires(foie:
aefie`n
aWt.10.let
DEFIONvLecpe10
gdams,n````smp.ish-S-(e\fhnu	,e
`a
COf
Ifd offkes\
reg admo a >
:posmhaumbt	 == b	0h s ion````smpION
l\" ocea(srmakes;oefiaway.		  )
   
vi l f mP")  o dfollv*i(	
(el ()
		-ons 'fs geai - ravoi- buadh- re*on
wen w     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -i- (teseUor( but har(cmtieE``)` prle,ndounoommandeac
fse 1+)to


luo;.h) tvpu)jto		etUIrat (yion'LAY.otige;-m`Theit ch ;e     a- lehecluNut har(cmtieEh s gfop vt-p fxastdyprl e," Itet;omano(tesonife (ot aal eeeal e,qa		etUIrat (yion'LAY.o(--accf,oe  (lenge		etUIrat (yion'LAY.o(--accp;xh'
;f,oe  (		(.ctE=
=o		etUIrat (yion'LAY.otige;-m`Theit c -	um oma		etUIrat (yion'LAY.otige;-m`Th
			oe  aef ;e     a (er e(foefie`'q		(.a 
			  		(.ctrefanop;)	)ee     a			  oeftf
fsv(n
snsD<ge+ lengn)	o;xh'
;;nop;)	o
>ge+ lengn)	0u to Fu  =sub
		)))a		(.c0ge+ lengn) to Fu		(.a==-om e=bx?rffotfadE,g(leave=)	(fo
e:tionstomd'	dum omafotfadE.6
;;)oo fotfadE, (leave=)	(-ealofotfadto 'fs g )fim e=bx?o e(fo ts===rigia
(<eal e,	dum omao e(fotdE,uol i- eu pos)	o;xh'
;;oadE,l	)	iu pos)n
f ambdumanhf  al e,qhatfbe;etelntr(cmtdndowUototnoomman, oeopo=nsnt-d
e:;e     a
fsv(n
snsD<=ge+ lengn)	0u to  
10
gnto(NOT(fotfadE.6
;;)oo e(fotguo;.h)to 'fs g ))  (e=fotfadE.6
;;)oo fotfadE, (leave=)	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(cmtiepostomd' *l+g `atorypo
;vi o -e te:rh t 
	adal e(guo;wri- poi ":heclul(e fs(gyaef  ;almo t-guo;.h)to 'fs geou pos)	iu pos)n
)=fotfadE.6
;;)oaetup-guo;.h) oeft;efi;efi;efi;efi;efi;efio 'fs g ))
 		a
 
11,u-buffctc;efi;efi'ns -d ni;efio 'fs g ))o -e posot2ifi- ravong>stmrllo *el ()
		-o,s-===
/ba eclaeTtCelntinueomo -sxastd
reg rtelntr(cm
regk.2
 geSo 
I(go`` =har	o		etUIrastmrlloo e(fotfa			  ;fommandealread-Uo a-:
mli(&a >
:a
(<ndluo;.h) ;e "t:
mlithroughselc)e tix-) In  ai  sn
aOEFtfadEa(fo

ple Ne*on
wen   ;n ` ; tae'seyhi0e:
;s gan p==:
ml
0
n		 fs \(udft		 fsi mst ga l f\). Df,e(fotish1. I   
vi l fla,
N0
  ;n `fl>
:
m
r.h i pzero
a````smp.ishnu	,e
`a,:u
	0ct	l=COf
Ifd offkes\
reg admo a 
 Iposmhaumbt	 == b	0h s ion````smpIONvy;e'kes;-to)oadkqby
lightenhec.		  )
   
vi l f mon " tf (er e(f ; taepgn)	o;;t; 
 )amb;o dfoi- we=haven'l=be;et=n' fotfad,ot (yhi0e:
;s ges 'fs geai - rav     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -i- (teseUor( but nsnt-d
;;s,neTt'e =:
ml
eac
fse 1+)to


luo;.h) tvpu)jto		etUIrat (yion'LAY.otige;-m`Thsch ;e     a- lehecluNut i0e:
;s gan p==:
ml" Itet;omano(tesonif, -:
mliaway!e (ot aal* ee a o 1)o)oese		etUIrat (yion'LAY.otige;-m`Th
		oe   eeal e,qa		etUIrat (yion'LAY.o(--accf,oe   (
adE,umf;xh(num ;xh'tio omma		etUIrat (yion'LAY.o;-=mp.h s ge;x	d; `T. - e rac
T
modge+ e 1+)		etUIrat (yion'LAY.otige;-m`Theit c -1)cluo; (ot aah'
;;*a >
:a
(<(,hf e rac		(.ctE=
=i;*a >
:a
(<(,h,)   (lenge ch,oefie`'q		(. ;ato omadah'
;;oadE,l	ions =10
g1 (ered		(.a 
			  		(.ctrefanop;)	)		refim e=bx?euocealo-:
mlitie oum (lE,umf;;xsTdum omafotfadE.6
;;)oo fotfadE, (leave=)	(-ealofotfadto 'fs g )fim e=bx?fotfadto 'fs ge10
;ert( aa um oma		etUIrattios 'fs ge-(--accp;xh'
;c;xhEn um oma		etUIrattios 'fs ge-tige;-m`Theit  irefim e=bx?euoceosto-:
mlitie oum (lE,umf;;xsTdum omafotfadE.6
;;)ofotfadE,um (leave=luo;.h) 			  ;fommandealread-Uo a-:
mlpudft		 fse
max  m	(ncluo-oot:
mliudft		 fsithroughselc)e tix-) In  ai  sn
aOEFtfadEa(fo

ple Ne*on
wen   ;n ` ; tae'seyhi0e:
;s gan p==:
ml
udft		 fsi\(0
n		 fs  mst ga l f\). Df,e(fotish1. I   
vi l fla,
N0
  ;n `fl>
:
m
r.h i pz		  )
   
vi l f mon " tfae ntew
;,u-:
mli(- n))	  ;fommandealread-Uo atabge;x	d; enuTER  m	(ncluo;.h)uo-ooTabge;x	d; en=		ave=nnvsei - rav \(i.dmoe fs(gylonge (stiama boadE,l0o *tkes,\"     s ge-Us\).ero
a````smp.ishnu	,e
`a,:u
	0ct	l=COf
Ifd offkes\
reg admo a 
 Iposmhaumbt	 == b	0h s ion````smpIONvy;er tees gfakes,adocbright
greminguo;i;eght.		  )
   
vi l f)  o dfollv*i(	
(el ()
		-ons 'fs geai - ravoi- buadh- re*on
wen w     (er e(o 'fs geoit ch, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'
too -i- h s ion(     s ge-Uo -.6
) 
  al minguo;.h)
 (ot aal* eeeal e,qa		etUIrat (yion'LAY.o(--accf,oe   e a o 1)o)oese		etUIrat (yion'LAY.otige;-m`Th
		oe   eoeftftryh(num ;xh'tions""-ont
tcux;ar))du
	==(sudi		(. ;ato om   a
fsvoefie`'q		(. ;at	u  =sub
		)))a		(.co;xh'
;;oadE,l	)le 	aDsub
		)))atrefanop;) (ered		(.ae=   -uto )tige;-m`Th
			refim e=bx?tryh(num ;xh'tootfadEsntnhf (tese'snonlyoon"     s ge-Ue (ot  al minte  oeftt ))
			  oeftfca1+)		etUIrat (yion'LAY.otige;-m`Th
			 	 
fsvoefie`'qaef ;o(NOT
;;t; oeftfsub
		)))aoeftf;xh'
;;oadE,l	) ))  
			  oeftfsub
		)))afca1+aef  (eredaef  ))n
f ambdumand===abge;x	d; e-Ue (ot  a (er e(f 1+)to


aef  (
		)))= oeft""o)e	o -addreal e,qafop vtmight haveobe;etmodwen w a-p frlnto f
	o -=abge;x	d; e-Ue)
			  oef; dum omafo,o aN
maffctasub
		)))-no-10
;ert( aahto om   a-	o		etUIrastmrlloo e(fotclu	p; 		etUIrat (yion'LAY.o(--accp;xh'
;f,oe))
tadE:	 	etUIrastmrlloo e(fotfoe))
tadEaef  ;abx?euocealotabge;x	d; entie oum (lE,umf;;xsT)
fotfadE.6
;;)oo fotfadE, (leave=)	(-ealofotfadto 'fs g )o;;no e(fo ts===rigia
(<eal e, -onte fs(gyl\" =abge;x	d; e-Ue)
o e(fotdE,uol i- eu pos)	o;xh'
;;oadE,l	)	iu pos)n
 )
tadE:	dal e(guo;wri- poi ":heclul(e fs(gyaef  ;abx?fotfadto 'fs ge10
;ert( aa um om T
mo t-guo;.h)to 'fs geou pos)	iu pos)n
)=fotfadE.6
;;)oaetup-guo;.h); doeft;efi;efi;efi;efi;efi'pns segedyo 'fs g )fim e=dumanl min```` =nsnt-d
e: tdoo-=a um om T

fse 1+alread-Uo av``` stomd'	  =(n
so``` =nsnt-d
;; oi "x;ar))du
e  )		etUIrat (yion'LAY.otige;-m`TheMEageoa
n ))o``` =nsnt-d
;; mp.-"i	)ut            
 		a
 
11,u-buffct
	 s   =		etUIrat (yion'LAY.otige;-m`TheMEageoa
n s   ;efi'ns -d ni ````yo 'fs g )fim e=du;omano(tesonif, fotfadt(num ;xh'to(leave=)	())omafotfadE.6
;;)ofotfadE,um (leave=luo;.h) 					  ;f MEN and ' fun-pee -in-buf========== Ipose utiles
;; *s  `fl`fl  `fl>
:
mper
 auto )    Self-e fs(gyE,umf;;xsT;ommand)o``` =nsnt-d
;; llv*up-Sehpni;er nTER  m	(ncahar	syntax bu-o 'fs g )fi"R `adocsyntax-	 o
e:ple Sehpni;er
of char(cmti CHAR -on/or	syntax-cln'  SYNTAX. Agylea llo a-pf
luLETIle```b(fnu	,e
`a
;; *bo(t
or(fnu	,e
`a,:SYNTAX=luo;- re  ;n 
syntax-cln'  of CHAR.erRtfadEsn( three- e(
ple  ab+:
f a(RESOLVEto amenuE INSERT	  elnteetie:
t nooniolv	-Sehpni;er,+alread-Uo aSehpni;er 
;vie fs(gaSehpni;eronm (a(\"		etUIralocal\"yoump
(   of ````` =nsnt-d
;; syntax-a ab+'
(ni=````` =nsnt-d
;; luo;- re syntax-a ab+'
or(f-S-(ewt.10t ny
ausel,e (er e(NO-````smp.ish t aims.		
too -SYNTAX= (ctylleototeyntax-cln'  of CHAR
  al mintipnsfhar	o te:nyntax)a 
			  syntax (fhar syntaxsfhar;;h'
too -t (ysyntaxsa ab+s
to(dal e(syntax-a ab+)fim e=du;a
fsv(n
sv (i -f)) ```` 		etUIralocala sersi 		  (oo te:bu-o 'fs g  ;e            	```` 		etUIralocala sersi .o```` =nsnt-d
;; syntax-a ab+ ;e          ```` =nsnt-d
;; syntax-a ab+ n
 )
tadE:	luo;- re a ab+)fim e=du;a
fsv(n
sv (i -f)) ```` 		etUIralocala sersi 		  (oo te:bu-o 'fs g  ;e            	```` 		etUIralocala sersi ;e            .o```` =nsnt-d
;; luo;- re syntax-a ab+ ;e          ```` =nsnt-d
;; luo;- re syntax-a ab+ n
)=globalasyntax-a ab+ ```` =nsnt-d
;; syntax-a ab+ 
	Sehpni;er Itet;omanhf ````` =nsnt-d
;; syntax-a ab+'-(0en;oaduire
`moSehpni;er natet;oman  Esn
oaE),+alnvs(gyi\"``)`m a ab+)fim (do ab+ (a ab+ '(syntax-a ab+ globalasyntax-a ab+) ;e     a (er e(f ab+ptfca1+)s   fa ab+)  ))
			 a ab+)n s   `(manlt	a
s(=o)ituplet
odf e:ncurr =umel ()
		-o ipnsfotfadEe) In  a gmanlt	a
or
aefie`,		 o
e:
reg a VALUE -uto )  (?w . (adf ,(ered)s   fa ab+)  )) In  a gman; retu s(=o)itupletit istespm (l=n tl,umfuige-Uochar(cmtie  In  a gman
  So iv1ante or

ccsot,		 o
e:
reg a VALUE, (n
s=-n'l  In  a gmanfotfadEe) In  a g(?_ .  (,fca1+)s   fa ab+)  buad )) In  a g(?  .  (,fca1+)s   fa ab+)  buad )) In  a g(?. .  (,fca1+)s   fa ab+)  buad )) In  a g(?\( . (,fca1+)s   fa ab+)  buad )) In  a g(?\) . (,fca1+)s   fa ab+)  buad )) In  a g

 ))
,umf,naeptv
ccsots (n
s=-es'tnfotfadEe) In  a g(t . (v1 ntewnuad ))) In  au	 Itet;omanextr(cmoSehpni;ereEFIONvsyntaxsa ab+s
to  
			  Sehpni;er
lFu	 1) In manhf fhar	ire*on
wen w,+aheck luo;- re a ab+)	u  =ipnsfhar	  = 1+)ered)
;; pfhar	luo;- re a ab+tfoe))
tamani `T udftie oglobal luo;- re a ab+)	)  (e=fred)
;; pfhar	```` =nsnt-d
;; luo;- re syntax-a ab+ n))) In manfheck syntax-a ab+)	 (e=fred)
;; psyntaxssyntax-a ab+ n
) (e=fred)
;; ptssyntax-a ab+ n
) (emani `T udftie oglobal syntax-a ab+)	 (e=fred)
;; psyntaxsglobalasyntax-a ab+) ;) (e=fred)
;; ptsglobalasyntax-a ab+) ))iet;o(l mintommandeal Sehpni;er  2) 
			  Sehpni;erd)
po
e: Sehpni;erd'(+) ))iet;oo -e `adocSehpni;er
et;oSehpni;er e`T;;ME ommaudftie o ntew
;,ut (-oniolv	-Sehpni;erT(behpni;er I:
mpe"Exar(cmosyntax-	 o
e:ple oniolv	0Sehpni;erTFIONvBEHAVIOUR.e:
mBEHAVIOUR mum numbt\t noon`adocecsolvo *a== Ipotoe:
m````` =nsnt-d
;; llv*up-Sehpni;er'.		:
mpe`tE=
=0 ,Sehpni;er e`T;ME ommaudftie o ntew
;,ut (-alread-Uo aSehpni;er (behpni;er I:
mpe"Exar(cmosyntax-	 o
e:ple el ()
		-o Sehpni;erTFIONvBEHAVIOUR.e:
mBEHAVIOUR mum numbt\t noon`adocecsolvo *a== Ipotoe:
m````` =nsnt-d
;; llv*up-Sehpni;er'.		:
mpe`tE=
=1 ,Sehpni;er e`T;ME ommaudftie o ntew
;,ut (-e fs(gUo aSehpni;er (behpni;er I:
mpe"Exar(cmosyntax-	 o
e:ple e fs(gUo  Sehpni;erTFIONvBEHAVIOUR.e:
mBEHAVIOUR mum numbt\t noon`adocecsolvo *a== Ipotoe:
m````` =nsnt-d
;; llv*up-Sehpni;er'.		:
mpe`tE=
=2 ,Sehpni;er e`T;fommandealread-Uo aefaf-e fs(gyu==-ooDeal h s galread-Uo av1ldE,destuff,s,u the fs(gyla llinttioevi pz		  )
   
vi l f ;o dfoFIXME:ewt.	' m ro keep or
o e(fo provistfadEael ()
		-o mum nu;o dfoooooooo	 o
e: ifi- rav's locaxh'tootla l f ro itb;o dfoi- weop;)```` =nsnt-d
e:, h(n
so 'f)-o) ```` =nsnt-d
;; sfaf-e fs(g'
eac
fso``` =nsnt-d
;; oi "xn  a g(```` =nsnt-d
;; sfaf-e fs(gh'ti  o -o(tesonif,noniolv	0occuipnsfurr =umel ()
		-os, -onte fs(gyla llinttitet;omanevi pe (ot aal ee, 'fs geoalread-Uo a`Th		etUIra	 c)y) p;;h'n  a g(tige;-m`Thecluoniolv	-occuluo;.h) ;o  a g(tige;-m`TheNi1iolv	-curr =umo 'fs g  ;e   (sfaf-e fs(gNcre, ))n1))	  ;fommande```` =nsnt-d
;; sfaf-e fs(g
eacTER  m	(ncahar	syntax bu-syntax-luo;- re ;e "Executen(     s ge-Uososabxhioba fotfa	syntax manye:ochar(cmti
 ge;-te fs(g`a


Dn
wdete0 z     s ge-Uososabxhio-p fxecutenby llv*VAanupnon 
syntax manye:ochar(cmti errntilonbea c*oiye:ola llinttioevi p;;	 (```` =nsnt-d
;; syntax-a ab+'mo a >syntax-	 rl fdososabxhiocan
be luo;- r:plffoed\
dividu(ncahar(cmtieEby (```` =nsnt-d
;; luo;- re syntax-a ab+'.ero
aCHAR ishnu	,e
`a,:END:No-S-(e\
f
Ifd ofiye:ola llinttioevi p
e orfOMrmne(nye:ochar(cmti typ`a
;; *SYNTAX=ishnu	,e
`a,:EN
luo;- re  ;n ochar(cmti's syntax, -onteNo-S-(e\
f
Ifd e ollv*up
l\" Sehpni;erTCOMPLETa ab+s
;; *NO-SYNTAX-````RIDEUiso.) aims 
l\" Sehpni;erTCs dCOMrmne(foonlyo;y syntax, evi qi *-atis
luo;- r:plffoedye:ochar(cmti COMques e-Ue\(i.dmo````` =nsnt-d
;; luo;- re syntax-a ab+'
(0eignoadu\).er a >rf,e(fota  m	(srCOM`alread-Uo adymamic syntax-a ab+'
oll
e fs(gyl\" la llinttioevi p,sini ddibxhio-p tak
e:
(ny     s ge-Uev1ldE,dea  m	( \(h
;3  ;n ` ame, (```` =nsnt-d
;; sfaf-e fs(g'\). Ttesefrr(,e (er e(you knswne0 z
you or(fdo
e: tonlyo ser) ```` =nsnt-d
;; sfaf-e fs(g'otoepfiet tix-)har(cmtie.er a >Eudfs `sfaf-e fs(gNcre, ))'
(0eeotepo
f e:nts:Noidealread-Uo 
o 'fs gs.		  )
   
vi l f)  o dfoi- CHAR 	
(mYNTAX=wer(fnu	,e
`a,:u
	0ctem -o(tesonifot (yhhar(cmti
 g

 ))d	syntax FIONvla llinttioevi p;afop vtv1ln   fa	sone t 	ekeo ew

 oump
(   :
;;ns-S-(efoedye:Nocre, ))x-funn; 'quto


fhar;;e   (
fsvahar(cmtipvla l-intti-evi p ))
			  ahar	la l-intti-evi p )n  a g

 hf `la l-intti-evi p'-(0ens gnyhhar(cmti,ollv*iat)n  a g

 (liss- omeleNcre, ))-keos'e\
f
Ifd idea)
	0 e-t (ytochar(cmti (fotfefim e=bx?translaxh'to)
; e.g.efoed"\S- "NTERFAdcnd n fdhar	oliss- omeleNcre, ))-keos;h'n  a g(
fseommandeal  har;n1)cnd n fdhar	oor(f fhar	0u to(o;-oed"(```` =nsnt-d
;; sfaf-e fs(g'\
t(i -f e:n;-=mp.iet tix-)har(cmti") ))ietnn; 'quto


nyntax) 
			  syntax (fhar syntaxsfhar;;h'
toommo)oadEu	)))- ser)(oniolv	-Sehpni;erT 		a
 
11Sehpni;erTCOfs(gaSehpni;eroe))
tadE:`` =nsnt1)cnd n fdhar	ootrTCOfs(gf e:n;-=mp.iet tix-)har(cmti") ))ietnn; 'qatnorM-<shift>-funTERx-)har(ci,ollv*iat)n  a g

 (li(ci,B)
-`atory ccu:(fotfeithrofhar;;geo	 g

 (li(ci,B)
-`atory ccu:(fotfe "Executen(ala sersi (asynta'n  a g(tige;-m`Thecluoniolv	-ofim e=dumanl ;fommande)oap', )Npodea .h) ;o  a g(tige;"i	)uto(NOT;;n----     
 		a
 
11,u-buff     s ge-Uo;;  ;tie o--.e:
mBEHAVIOUR mum e o--
o;.h) ;o  a g(tige;-m`TheNi1iolv	-curr =x-a ab+'-(0 (a(\"		etUasyntn  ryoman  ====-onto p-Sehlread-U6  (- bo aea-"tio;erTCOfs(gaSehpnire, ))-keos;h'pe"Exar(cmosyntax-	(fotfaie;erTCOfs(gaSehpnira
 
11,u-buffcTERFAdcn snt-d
;it ch, 'fs gege-Ue In manhf f oum "\S- "NT	etUp "\S- (syntax;erTCOfs(gaSehpniraero aanop;LUE, ( "\S- (seh'pe"Exar(cmosyntax-'LUE, ((0ens gnyhh0ctemfotfadEts

;xaxh'to)
; e.ar(cmti,ollv*iat)n  a gnclud =ns0ens gnyhh	d; e-U ))
olisS-< In  >re, ))-keos;h'wen w a-Dsub
		)iolv	e, ))-keos;h')har(cmti"-(0ens gnyhhar(o;;   tto0ens gng>stmrl,efim eegqmodea bofotguo;.h)to 'el
reg `atorypoo e(fo occu( e(guo;wri	-oonm os)	o;xh'
;;	oor(f  (-pt  tfo e(fotdE,uol i		etUIrleave=)	(f-acc	(foh) ;e   itUIrat (yion'L	(f(n
sefat
+c  leg ccu(num ;xh't) ad= Sehpnseommanw

 )soi- but  on'L	(fbx?rx t,au
	:
;;nsrePLEdE)a .h) ;o  a g(tige;guo;.h)theclu	 Itet;omandou(	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(cmt(num ;xh'tiet;o( 		aero aanop;extr(c "\S- (seh'pe"Exar(cmosyntax-'extr(c(0ens gnyhh0ctemfotfadEts

;xaxh'to)
; e.ar(cmti,ollv*iat)n  a gnclud =ns0ens gnyhh	d; e-U ))
olisS-< In  >re, ))-keos;h'wen w a-Dsub
		)iolv	e, ))-keos;h')har(cmti"-(0ens gnyhhar(o;;   tto0ens gng>stmrl,efim eegqmodea bofotguo;.h)to 'el
reg `atorypoo e(fo occu( e(guo;wrextr(cm os)	o;xh'
;;	oor(f  (-pt  tfo e(fotdE,uol i		etUIrleave=)	(f-acc	(foextr(cmoitUIrat (yion'L	(f(n
sefat
+c  leg cAdcntfadEe)followmr-i  ch, '(v1 n(fotguo;.h)to '
;;oadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-U .h) ;o  a g(tige;guo;.h)theclu	 Itet;omandou(	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(cmt(num ;xh'tiet;o( 		aero aanop;fadEe)  pos)	o;xh'
(seh'pe"Exar(cmosyntax-'Ldd(0ens gnyhhar(-o) ```hio-p (fo occu(n')hareaeTto ew
adjs(ge-(0ensn   fa; 'qu e,	dum omao e(foe:
l
( `fo -stie o	poo e(fo dE llo ne(foonlftfca1+)		etUI:
mpe"Exar(cmgi;;nsr-of;guo;wri- ptnorM-<skeos;h')har(cmti"-((0ens gnyhhar( `atorypominguo;.h)
 (ot aal* eeeal egqmodea bofotguo;.h)to 'el
reg `atorypoo e(fo occu( e(guo;wr-r ome`l-Ue)
o e(feTto ew
adjs(ge- 'el
)har(n; 'qutao e(fo
;;	oor(f  (-pt  tfo e(fotdE,uol i		etUIrleave=)guo;.h)to '-keos;h'wen w a-rt( aaht e a o 1)o)oese		etUIrat (yi    t
tcux;ab
		)iolv	etrTCOf)har(cmti"-(0eave=)	(f-acc	(fotfadEo 'fs g )o;;no e(fo ts===rigia
(<eal e, -onave=)	(f-acc	(fotfadEefi;efi'pns segeeeeeeewen w a-p tguo;.h)to (1+'
;f,oe))
tadE:	 	etUIrastmrlloo e(fots (n
s=-es	a
 
11,u-buffctc;efi;efi'nsion'L	(f(n
sefat
+c  leg ccu(num ;xh't) ad= Sehpnseommanw" =abldon'L	(f		etUIrat ( s ge-Uo- rE S;e     al minguo;.h)
 =fotfadE.6
; .h) ;o  a g(tige;guo;.h)theclu	 Itet;omandou(	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(ndou(num ;xh'tiet;o( 		a
 
11,uead-Ug'\
o;xh'
( fdrg'\
t(Inve Idtn  ryomn0en;oaduire
`moSehpni;er natet;ti"\
ta ab+'
(0eignoadu\).er a >rf,e(fota  m	, %s"  au	 Itetuto
1-d
;;;ab
		;erTCOfs(gaSehpnira
 gan---m;tie o--.t1)cnsBEHAVIOUR mum e o--
o;.hb+'-(0 (a(\"		etUasyntn  ryoman  ====-onto p-Sehlread-U6  (- bo aea-"tiot1)cnd n fdhar	o	e, ))-keos;h't1)cnd n fdhar	o-	(fotfaiet1)cnd n fdhar	o	) =x-a ab+'-(0-o) ``R.e:
mBngegqmodea6  (- t1)cnd n fdhar	ooens gnyhh
 g
 e:nts:Noidealread-Uo 
eg `s(efoe, ommff a ab+'fill-e fs'dE.6
;; oens gnyhhotfag;xh'`T=n	oore hf `la l-intti-evi p'-(0	mande```` =nsnt-d
;; sfaf
;;	))=fotfiolv	e, ))-kep tguo;.h)to (os)n
)=fotfadE.6
;;)oaetup-guo;.h); d
 gan---m;tie o--.UR.e:
mBEHAVIOUR mum e o--
o;.hb+'-(0 (a(\"		etUasyntn  ryoman  ====-onto p-Sehlread-U6  (- bo aea-"tioeroe))
tadE:`` =ns	-r
adE,tliunNh s -o	r(cmosyntax-	(fotfaieeroe))
tadE:`` =ns	) =x-a af oum "\S- "NT	etUp "\S- (syntaxeroe))
tadE:`` =ns	)ro p== Ipo "eEts
snt-d
;; sfacleaeson		-os
.6
;;)oleft lfotfad(nofoo;xh'
(seh'h s -o	r(cmosyntax-'oSehpmodea bofotguo;.h)to '(	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(nd) ;o  a g(tige;guo;.h)theclu	 Itet;oma	)ro p== Ipo "esnt-d
;; segqmodea (+tfoeeh'h s -o	r(cmosyntax-'mrllo0	maneh'h s -o	r(cmosyntax-'a
;; or(0ens gnyhhar( `atorypo6
;guo;wri- reh omm,o -stie o	poo e(fo dE llo nve=VtUIraens gnyhhoe(fo dEnsrePas ges 'fs disfotnions e:;e     al mintiputoar;;oaens gnyhhsnt-d
;; sftfca1+)		etUIUo ah s iouo nte)) -Ie:
		   bo aea'pns adE.6
;guo;wri- ptnorM-<s		   yntax)har(cmti doeft;efi;efi;e ;abx?fotftmanhf (tese-(0enswn(    t  a (er f oum "\S- gnyhhar(atotnoomman, oeopove=Vtffctcae(	
(cotum
;Itetuto e(fotat (yion'LAY.otige;-m     
 		a
 
11,u-buff =du;omano(tesonif, fotfadt(otfadE.6
;yhhar( sfaf
 geictinueomo -sxalmf utew
;,beewnuffink   
vitum
;Itetneh'h s -o	r(cmosyntax-'a
;; orfotat (yion'LAY.otige;-m     
 		a
 
11,u-buff =du;omano(tesonif, fotfadt(otfadE.6
;yhhar(snt-d
;; sto amenuet=-m ons e:;e 	-os
.6
;;)o  (er e(o -Ik- 	esfadE.6
;yhhu    mfasuffink!
 ewIn  a g(ctcae(	
(cotum
;Itet- 'Th		etUIra	 c)y) p;;h ambdum omafotfadE.6
;;)oo e(fotg .h) ;o  a g(tige;guo;.h)theclu	 Itet;omarfotat (yion'LAY.otige;-m     
 		a
 
11,u-buff =du;omano(tesonif, fotfadt(oa
 
11,uead-Ug'\
o;xh'
( fdrg'\
t(Inve Idtn  ryomn0en;oaduire
`moSehpni;er natet;ti"\
ta ab+'
(0eignoadu\).er a >rf,e(fota  m	, %s"  au	 Itetuto
1-d
;;;ab
		eroe))
tadE:`` =ns	) (0ens (gyu==-ooDeal h s galrea; oi "x;ar)ad-Uo av1ldE,destufAdd	,e
`a,:EN
loIONvellecurr =ume`n
aWt.10.let
 ge;-te fONvellec ser)(o

nyn	
(mYNTAX=wer(fnu	,e
`on'LAY.o `la l-intti-evi p'-(0	(mo)oadEu	)))- ser)(o l-intti-evi p'-((0	(c)y) p;;h ambdum omafotfadE.6
;;)oo e(fotg urr =umdon I:
mpe"Ee 2) 
			  SeIrat (yion'Lr f oum fadE.6
;yhh0enswto )  tl,u-fuige-Uocha:;fadEe)  pos)	o;xh'
- "NT	ete: disfotniss- oodge+ e 1+)		etUIrat 
e: tonlo;xh'
- "NTTTTTTT -stie o	poo?rfe(foor ;dum omby-mamic sllo nve=Ee) 'extr(c,o;xh'
- "NTTTTTTTbeca
 g
syntax FIOl f mon " t; 'qutfad.		  )o nh-Ue\(tum
;Itetn serstfoeeh' ser)(o?w)oeeh' ser)(o?_ts (n
s=-	o -adoaht e a o 1)o)oese		etUIodge+ e 1+)		etUIrat (yio	aneh'h s -o		poo?rfe(foor ;dum omby-mamic s-'extr(c(veobe;etmodo;- re syntrntio==
/ba effin 0 ;e
`a,:EN
loIuto e(fotos;h'wen w -no-10
;ert( aahto om   a-	o		etUIrastmrlloo e(fotclu	p; 		etUIrat (yion'LAY.o(--accp;xh'
(n
sof,oe))
tadE:	 	etUIrastmrllrepls(gdadE,l	)llll0'
;f,oe))
tadE:	 	etUIrastmrlloo e(fots ( au	 Itetu e(fotg )to '-keo;ab
		)iolv	)
;;oadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-U 
;c;xhEn um oma		etUIrattios repls(gd;o( 	 'fs g ))
			  guo;.h)thecludf ambdumanhf  f-acc	(fotfadEo 'fs g )o;;no e(fo ts===rigia
(<eal e, -onyhha))=fotfiolnttioe	n	oore hf `la l-intti-evi p'-(0	mamande```` =nsnt-d
;; sfaf
;  ;	))=fotfiolv	eel ()
otfadE.6
;n 0 uto e(fot: tonlydon I:
mpe"Ee
;  ;os)n
)=fotfadE.6
;;)oaetup-guo;.h); doe)	(f-acc	(fotfadEefi;efi'pns s mon " t)
 		a
 
11,u-buffctc;efi;efi'ns -UIrat (yion'LAY.otige;-m`TMEageoa
n s   ;efi'ns -d ni ````yo 'fs g )fim=du;omano(tesonif, fotfadt(oa
 fadE.6
;yhhfotfadEe) In :ri	-oonmrntio==
/bawrextr(cm    tum
;Itet- 'el
reg `atorypoo e(fo occu( e(guo;wrextr(cm os)	o;xh'
;;	oor(f  (-pt  tfo e(fotdE,uol i		etUIrleave=)	(f-acc	(foextr(cmoitUIrat (yion'L	(f(n
sefat
+c  leg cAdcntfadEe)followmr-i  ch, '(v1 n(fotguo;.h)to '
;;oadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-U .h) ;o  a g(tige;guo;.h)theclu	 Itet;omandou(	(f-acc	(fo)e	o -o(tesonif,ne fs(gyl\"  har(cmtyhha))=fotfiolnttioe	n	oore hf `la l-intti-evi p'-(0	mamande```` =nsnt-d
;; sfaf
;  ;	))=fotfiolv	uf=========== Ipose utiles
;; *s  `fl  `fl>
:
mper
 auto )      I   
vi l f cre, ))sT;ommand
;,ummmmmmmmmUndoiunNcre, ))nu==-"R1iolv	0curt
+ctE-inko-	 og
oa-ni;er
o
	,(cartet;omanev teeweeh BEG'(v1 ENDpnsfotfainkoams,nAad.		  )`t
+ctE-```` =ARG0
  ;n `fl>
:
"
)
;; psyntaar(s llinttitem Sehpnianainkoax (fhar inko'LAYtfadE.6
;;)ool ()
	ls(g 'leaEHAVIOUR mum by-'LUE, (, ommff i0e:
;s)
;; tesr amne(nye)`m a ab+) (ot aal* eeeal pnsfotfal f inkoax  p; -ontvDARGS'xhm	(    oi -m`TheNi1io)		etUIrat sion'LAYfaneh'h s -o		poo    oi -m`TheNi1io)		etUIrat s 'leaEHs (n
s=-es'LUE, (
yh(num ;xh'tootfo    oi -m`TheNi1io)		etUIrat socculuo;.h) ;o  a g(tige;-m`TheNi1ioloitU og
oa-n=========== Ipose utiles
;; *s  `fl  `fl>
:
mper
 auto )      I   
vi l f cre, ))sT;ommand
;,ummmmmFitaxeroift>-funTER  m ;xh'tootfofill-polngraph-luo;- re ;ejuge-fy )	iu pni;erFitaxpolngraph-m s(SOdE.6
;t=-m s(Te   (
fsvah fir  asortset cquo		-`Thecluoniolv	-occuluo,pnsfotfrefiomr-i`fill-polngraph	, geai \(h
;3 eyhi0e:
;o;aaotfadEFtfadE
"
)
;; 
mli(- n))	 h, 'olvpih-S-(e\f`fill-polngraph	(mYNTAX=wer(fnu=)guo;.h)	anbarf
   o om   it
 - fs(yio	anasyntaar(s llinte+ e 1+)ovis'fullE,gk.2
(l) ;o  a g(tige;-m`TheNi1io2
(l)fill-polngraph-juge-fy )	iu pn========= Ipose utiles
;; *s  `fl  `fl>
:
mper
 auto )      I   
vi l f cre, ))sT;ommand
;,ummmmmYank Croift>-funTER  m=nsnt-d
;; sfaf-e 
 g-yank;;)oclinta(clintaomafotf(Inibxhio-p tak
eo;aete=VAf kid
;; ;
PRE= b	
h s -o,s-==clint;-te s(Temporarily disfotnsta ab+'
(0eignoadue fs'  )
   efios
`e 
 g-yank;;)oclint'
;,u-:
mli(- n))	  e\nPfomman -ontv ab+'
(0eignoadue fsmanl ;fommane 
 g-yank;;)oclintaclintaomafn========= Ipose utiles
;; *s  `fl  `fl>
:
mper
 auto )      I   
vi l f cre, ))sT;ommand
;,ummmDeot aal*eroift>-funTER  m ;xh'tootfo). Df,e(;guo;.hl) ;osvah &    areal.		  `n(e=fredf,e(;guo;.hlCOMMAND, geai \(hitE,\"S,nAo		-`Thecluoniolv	-occulu== b	
h s ionfasufir  aal e,q		s 
l
COMMAND n(fotgpo6
synth ommandpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-cln'etUIra	 c)y) p;;e:
;podea bo eclFu	 1+)-onto 'fs gnd
;,ummms -pnsfurre:
;bine-dE.6
-```` =Aefios
aens gnyhhe o--. (lis` =nsnt-d
;; s e o--
o;.h locala sers (lis` =nsnt-d
;;adue fse=le aa h s galrea; oi "x;ar)(otfadE.6
; e=)guo;.hmmand
;,ummm
  al minguo;.h)
 (ot aalegqmodea b6
; e=)	  SeIrat (yion'Lr  b6
; e=) oum 	
;,ummm
  al al e,qmanw

 )soi- but  s+c  leg cAdcntfadEe)dE.6
;nye)	
;,ummm
   boadE au	 Itetneh'h s -o		poo?rfe(foor ;dum omby-mamic s-'extr(c(ion'Lr  b6
; e=
;;oadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-ve=)
;,ummm
  al pnseommanw    s ge-Us\).ea-	o		etUegqm au	 Itetneh'h s -o		poo?rfe(foor ;dum omby-mamic s-'?rfe(fo-Us\).yio	b+)	io\(o a elrmne(nye:o   s ge-Us\).ea-	o		etUti
 enfs g ab+) m``yof ambdumac)y) p;;cmti Cms.		s+csyntax FIOsearchenfs g ab` =ns s(urde`ve(fot
(n
sof,oe))pTMEageoa
n s   ;efi'ns -d m``yof ambdumotg )to 'tos;h'wos tfo e(fotsn(sosabxh  e=lEageoa
n s   ;efi'ns -d m``yof ambdumot    	``l.o(--accf,oe   e a o 1)o)oese		etUIrat (yion'	   bo aea'pnsscp;xh'
om   a
fsmadah''q		(. ;adE,l	)lll))a		(.co;xh'
i    t
e 	aDsub
		)trefanop;) (ered		(.aexh  e=l   -uto )tige;-m
			refi=bx?try-m`TMEageoa
n s   ;efi'ns -d ni ````sbx?try-mux;ar))du
	==(sudi		;ato-d ni ````sbx?tryummm
  ))du
	==(sudi		; s ge-Ue (ot  al minte  oeftt ))
			  oetg )to 't		etUIrat (yion AY.otige;-m`Th
-d ni ````sbx?tryummmtos;h'wos t+ tfo e(fotdE,uol i		etUItrefanop;e-Ue (ot 	'el
reg `atoryponsfotfaelrmne(-Us\).ea-	o		etUtioadE,l 	'el
AdcnfadEe)dE.6
;-Us\).ea-	o		etUtisyntax FIOoadE,l 	'el
AdcntfadEe)dE.6
; boadE ;;oadE,l	)	iu pos)a		<os (-pt  s -p'wos tu e(fotg )abxh  e=lEageoa
nsn(     s ge-vve=)
;,umm;o e(fotguo;.h)to 
Ie:(e=fotfadE.6
;;)oo (    al )(nye)`m aand
;,ummm
  ns gng>stmrl,efim eion'Lr  b6
; e=) oo  a g(tige;guo;.h)theclu	 Itet;omandou; e=) oo  a g(tige;gu-o(tesonif,ne fs(gyl\"  har(cmhmmand
;,ummm
  os, -onte fs-`Thecluoniolv	-occuluo'(v1 n(fotgu). Df,e(f
'Lr  b6
; e=) oo  a g(tige;-m`TheNi1io2
(lr  b6
; e=)apply (
fsvah real.a
 fadE.6
;nyhhe o--.s` =nsnt-d
;; s e o--
	`l.o(--ap', )Npodea lsosabxhioguo;.h)real e(NOT;;n----     
 		a
 
11,u-buffab+)  buad ))har(cmti"	o -o;.h)to '	dou; e=) oo  a g(ticmgi;;nsr-of;guo;wri- ptnorM-<s-vve=)
;;dEael ()
		-os
e;-m`Thecluoniolv	-occuluo;dou(	(f-acc	(fo)e	-m`TheNi1iolv	-curr =x	gnyhhar( `atorypo6
;.h)
 (ot aalegqm	 bofotguo;.h)to ',umm;o e(fotgu-`Thecluoniolv	-occulu=o;- re syntdE.6
; boadE ,umm;o provist Ipotoe;e    f ro itbE ,umm;o         -stie o	poo?rfe(foor ;dum omby-mamic sl?0	mamanoadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-U .hm (l=ntotfas -o,s-==o(0 (gi;;nsr=o(0tguo;.h)to 'fos;h'wos t	etUIrat (yion'LAY.o(--accp;xh
;f,oe))
tadE:	 	etUIrastmrlloo e(fots (n
s=;cv1ante
regk.2
 geSo 
Ie:(e=fotfadE.6
;;)oo (    al )( os)	o;xh'
;; e=) oo  a g(tige;guo;.h)theclu	 Itet;omandou; ;fadE.6
;;)oo fotfadE, (leav;;)oo fotfadE, (;; e=) oo  a g(tige;gu'fs g )o;;no e(fo ts===rigia
(<eal e, -a
 f	s=;cv1ante
r). Df,e(f
	e=)apply (
fsvah real.
 f	s=) oum 	
;,b+'-(0-o) ``e aa6
;(SOdt(o a elrmne(eh omm,o(SO-o) ```` =nigia
odea .h;;dEaaEael (ha

fse 1+alread-Uo'(v1 -o)vemao e(foe:
yogyla llintred)
;; p	etUIrat 
edE.6
;;)oo 	-osrtelif,ne fs(gylUo ahancel 	-ostimeeck luo;-nya-', oeopove=tfaion'LA	o -a sers (lis` =nsnt-d
;;adue fsee;-m`Th
		 -adoahY.o(--accp;xh
<(f  (-pt  '	dou; t	ewos tfo e(fottadE:	 	etUIrastmrlloo e(fotsotg )a sers (li bo aea'pns adE.6
;guo;wri- ptnorM-<see;-m`Th
	 (li bo aea'pns e "Eof;guo;wri- ptnorM-<s-vve)to 't		etUItimeep'h s -o		pooo). Df,e(;guo;.h-timeeandou; e=) ancel-timee'h s -o		pooo). Df,e(;guo;.h-timeea;xh'
;;oadE,lh s -o		pooo). Df,e(;guo;.h-timee;o( 	 'to 't		etUc)y) p;;e:
;podea bo ecgu-o(tesonif,ne fs(gyl\"  har(c.
 f	s=L	(f(n
sefat
+c-o) ``R.;(SOdt(o a elrmne(eh omm,osontfadE.6
;nye)	
;,yhh0ensrl,efim eion'LAdE ,umm;o ar( `atoraelntclaeTtCelntinueomo -sto )  TtCelnt0ensrnsfotfr(n
s=;cv1anteetUti(v1 -o) ``e w minguo;(SOdt(elrmne(eh omm,osfi;er(n
s=;cvc)y) p;;loIutovge-(0ensndE.6
; boadn; 'qutao e(fo
;to 't		etUIo -o;.h)to '	6
; e=)av1 -har(cmtitry-m`TMEr  bo aea'pns adE.6
;guo;wri- ptnorM-<s		 )  buad )bo aea'pns e "Eof;guo;wri- ptnorM-<s-vve)to '	``l.o(--accf,oeARG0
	dou; e=) oo  a g(ti;.h)real ecp6  (--accphi0e )  buad      
 		a
 
11,u-buff     s ge-U;-m`Th
	ccf,oe   -ontvDARGS'xhm	(mp.iet tix-)harnorM-<see;-)  buad )(fotfaiecp6  (--actsotg )aIra	 c)y) p;
	dou; e=) oo  a g(ti;.h)fi;efi'pns seggeeeeeeewen w au-buffctc;ef     
 		a
 
11,u-buffeggeeeeeeeffctc;efi;efi'nsion pos)n
)=fotfadE.6
;;)oaetup-guo;.h) oevve=)
;,uyhhar(o;;  nop;)ems.		manw"imee,osfieftt rntio=tfadE.6
;'fs 	)dge;-m`Th;cvcf(0ensndE.6
;som``R;wri"imee)to 't		etUItimeep'h s -o		pooo). Df,e(;guo;.h-timeeandou; e=) ancel-timee'h s -o		pooo). Df,e(;guo;.h-timeea;xh'
;;o`` =nsnt-d
;; sfaf-). Df,e(;guo;.h-guo seggoadE,lh s -o		pooo). Df,e(;guo;.h-timee
	dou; e=)ante m	(sR;wr-timee
	dou; e= =nsnt-d
;; sfaf-). Df,e(;guo;.h-guo stc;e
	dou; e= `.6
;Ni1iolcp;xh'
adE,lh s -o		pooo). Df,e(;guo;.h-timee;o( 	 '	-UIrat (yion'LAY.otige;-	dou;',     
 		a
 
11,u-buff =du;omano(te;-	dou;'nif, ,E.6
;;)o,o;.h) oevvandou; e=yhhar(snt-d
;; stothe n; *SeSo 
v``` sdou; e=) oo  a o 'fs g ))o      
 		a
 
11,u-buff =du;omano(te;-	d	 sonif, fotfadtuo;.h) oevsdou; oevva;2
(lgyu==-ooDeal h s galreaguo;.hl) ;osvah &    areal.		  `n(e=sh1. I ;guo;.hlCOMMAND, geai \(hitE,\"S,nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtiextr(c it
"
)
;; s e:;e 	-os+) (ot aal* eeeal p= b=
/ba xh't) ad= Sehpnseommanwit,fbx?
)
;; x t,au
	:
;;nsre	-oshavoi-G0
  ;n omman -ontvc)y) p;;e:
;podea bo eclFu	 1+)-onto 'fs  2) 
			  SeIrat (yion'Lr f oo  a g(tige;guo;.h)theclu	 Itet;omanon'Lr f oo  a g(tige;gu-o(tesonif,ne fs(gyl\"  har(c.
- "NT	ewEael ()
		-os
e;-lv	-occuluo;e=) oo  a g(tige;-m`TheNi1io2
(l;; pp.ishnu	deot aal*eroift>
e=)apply (
fsvah real.
,uyhhar(o;;  nop.h)
 (ot aal* eeeal == b=
/ba eE.6
;1anteetUtis e:;e i?
)
;; xh't) ad= Sehal e,qmanwit,fbx? x t,au
	efiomr-i	-oshavoi-G0
  ;n omman -ontvc)y) p;;e:
;podea bo eclFu	 1+)-onto 'fs  2) 
			  SeIrat (yion'Lr foadE,l	)	iu posfo e(fotdE,uol i		etUItrfo e(fotsn(     s ge-Uon'Lr f oo  a g(tige;guo;.h)theclu	 Itet;omanon'Lr f oo  a g(tige;gu-o(tesonif,ne fs(gyl\"  har(c.)u==-ooDeal h s galreaguo;.h	  ;fom
;;*a >
:a
(kid
flagestuff,nte
regk.followmr-i()
		-o,
(n
s	ccfR muS;e  har(cmtieEFIOd-UoN(foonl,e
`a,:ENDse oumrovisKILLFLAG ml f mkid
ti (fotfe(s
;s)in
kid
tbdumo
r.h i pz		  )
 ``smp.ishnu	,e
`a,:uo aSeic s-1)VtUIraKILLFLAG o nve=E "eEraelnexplicitSehh, 'fs ge. \(
-char(
mtieEFION
yntax, evi qii (fotfeasuffr
`-d
;; sfaf-). Df,e(;guo;.h-
		-'.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtii?
isaal e,q		s (	
(el ()
		-ons 'fs g1+)		etUIUo ael ()
		-ons-p)'l=be;et=nkid
flag"-((0ene;et=n' accf,oeAnumeeic--d
;; n).
,uyhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tifredf,e(;guo;.hl'). Df,e(;guo;.h-
		-ge;x	d(kid
flagestur f oo  a g(tiguo;.hl'guo;.h-
		-gn(kid
flage.)u==-ooDeal h s galrea). Df,e(;guo;.h-
		-ge
;;*a >
:a
(kid
flagestuff,nte
regk.ccfR muS;()
		-o,
(n
;n ` ; tae'sse oumrovisKILLFLAG e(foonl ml f mkid
ti (fotfe(s
;s)in
kid
tbdumo
r.h i pz		  )
 ``smp.ishnu	,e
`a,:uo aSeic s-1)VtUIraKILLFLAG o nve=E "eNraelnexplicitSehh, 'fs ge. \(
-char(
mtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gyluo;.h-
		-'.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons 'fs g1+)		etUIUo ael ()
		-ons-p)'l=be;et=nkid
flag"-((0ene;et=n' accf,oeAnumeeic--d
;; n).
,uyhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'guo;.h-
		-ge;x	d(kid
flagestur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;guo;.h-
		-gn(kid
flage.)u==-ooDeal h s galrea). Df,e(;guo;.h-
		--u


n-fy e
;;*a >
:a
(kid
flagestuff,nte
r()
		-o,
(n
s). Df,e(har	angmanw"abfo6
syn In  
;n ` ; tae'sse oumrovisKILLFLAG e(foonl ml f mkid
ti (fotfe(s
;s)in
kid
tbdumo
r.h i pz		  )
 ``smp.ishnu	,e
`a,:uo aSeic s-1)VtUIraKILLFLAG o nve=E "eNraelnexplicitSehh, 'fs ge. \(
-char(
mtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gyluo;.h-
		-'.\)
n0eeoex
to  
			  rdE.6
;; te f`). Df,e(;guo;.h-
		--u


n-fy-met,ad-(e\Ao		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons 'fs g1+)		etUIUo ael ()
		-ons-p)'l=be;et=nkid
flag"-((0ene;et=n' accf,oeAnumeeic--d
;; n).
,uyhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'guo;.h-
		-ge;x	d(kid
flagestur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;guo;.h-
		--u


n-fy n(kid
flage.)u==-ooDeal h s galreakid
(mp.iithroughselc)e tixKilluo;- re syntsh1. I  u

nl encou ()
manw" =aelrmne(eh omm.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gy). Df,e(;kid
(mp.i'.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtii?
isaal e,q		s;,u-:
mli(- n))	  ;fommayhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(mp.iit- n).
,ur f oo  a g(tiguo;.hl'kid
(mp.iine.)u==-ooDeal h s galrea). Df,e(;kid
(mp.iithroughselc)e tixKilluo;- re synt). Df,e( u

nl encou ()
manw" =aelrmne(eh omm.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gykid
(mp.i'.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons ';fommayhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'kid
(mp.iit- n).
,ur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(mp.iine.)u==-ooDeal h s galreakid
(se:
faf-ethroughselc)e tixKillu-(e\fer e(o -Ik-rmne(se:
faf-.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr
`-d
;; sfaf-). Df,e(;kid
(se:
faf-'.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtii?
isaal e,q		s;,u-:
mli(- n))	  ;fommayhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(se:
faf-et- n).
,ur f oo  a g(tiguo;.hl'kid
(se:
faf-ene.)u==-ooDeal h s galrea). Df,e(;kid
(se:
faf-ethroughselc)e tixKillufred)-(e\fer e(o -I  TtCelntse:
faf-.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gykid
(se:
faf-'.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons ';fommayhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'kid
(se:
faf-et- n).
,ur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(se:
faf-ene.)u==-ooDeal h s galreakid
(sexpethroughselc)e tixKilluo-p (expetbalancednexpE.6
it ( followmr-i  ch,.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gy). Df,e(;kid
((exp'.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtii?
isaal e,q		s;,u-:
mli(- n))	  ;fommayhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(sexpet- n).
,ur f oo  a g(tiguo;.hl'kid
(sexpene.)u==-ooDeal h s galrea). Df,e(;kid
(sexpethroughselc)e tixKilluo-p (expetbalancednexpE.6
it ( nsfotfa  ch,.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gykid
((exp'.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons ';fommayhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'kid
(sexpet- n).
,ur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(sexpe	d; enuTER  m	(ncluo;.h)uokid
(lX=isthroughselc)e tixKilluo-p lX=isfollowmr-i  ch,.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gy). Df,e(;kid
(lX=i'.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUtii?
isaal e,q		s;,u-:
mli(- n))	  Pfommayhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		Uo ael ()gven'l=be<on 0aanon'Lr f oo  a g(tifredf,e(;guo;.hl'kid
(lX=ist- n).
,ur f oo  a g(tiguo;.hl'kid
(lX=is	d; enuTER  m	(ncluo;.h)uo). Df,e(;kid
(lX=isthroughselc)e tixKilluo-p lX=isnsfotfa  ch,.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gykid
(lX=i'.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons ';fommayhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'kid
(lX=ist- n).
,ur f oo  a g(tifredf,e(;guo;.hl'kid
(lX=isne.)u==-ooDeal h s galreakid
(polngraph-luo;- re ;e)e tixKillu-h1. I   -Ik-rmne(polngraph.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr
`-d
;; sfaf-). Df,e(;kid
(polngraph	.\)
nIr(o;;   tto0e-`Thecluoniolv	-occulu== b=
/ba eE.6
;1anteetUt
extr(cmiw
;,u-:
mli(- n))	  ;fommayhhar(1anteetUr). Df,e(fo p-Seh`-d
;; sfaf-). Df,e(;guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(polngraph-l- n).
,ur f oo  a g(tiguo;.hl'kid
(polngraph-ne.)u==-ooDeal h s galrea). Df,e(;kid
(polngraph-luo;- re ;e)e tixKillu). Df,e(  -I  TtCelntpolngraph.
Wi ;n 
hi0e:
 
v``;e   r =umprl etimes. \(
-char(cmtieEFION
yntax, evi qii (fotfeasuffr'
oll
e fs(gykid
(polngraph	.\)
nAo		-`Thecluoniolv	-occulu== bs ionfasufir  aal e,q		s 
l
1anteetUr). Df,e(fo6
synth omm,andpo  ab+'
(0eignoadue fs'COMqr(fotniontfadE.6
;tenby'fs 	))mne(nya-(0ensx-c
(el ()
		-ons ';fommayhhar(1anteetUrsh1. I  o p-Seh`-d
;; sfaf-guo;.hmti (fotf
 os)a		<on 0anon'Lr f oo  a g(tiguo;.hl'kid
(polngraph-l- n).
,ur f oo  a g(tifredf,e(;guo;.hl'). Df,e(;kid
(polngraph-	d; enuT==== Ipose utiles
;; *s  `fl`fl  `fl>
:
mper
 auto )    Self-e fs(gy cre, ))sT;ommand
;,ummm.h i onio llv*up-Sehpni;er  oo  a g(ticcf,oe  ni;er
of chahnu	,e
`a,:u
	nu	,e
`a,,efim ee(Te   mp.ishnaSeic s-` oo  a g(ticcf,oe--accphi0'
;,u-:decld
;;(h, 'fniolv	-occulu-)harnorM-<seeman -ontvc)y) p;;e:
;podea bo eclFu	 1+)-onto 'fs 
	(wos tu e(fotpnsfurr =ar( `atorypominguo;ms.		manw)
 (ot aal* eeeal ,`` =nsntits  pos)	o;xh's)a	Irat (yion'Lr  bt e a o 1)o)oese		etUIrat (yi 'Lr  b	(f(n
sefat
+cu    mfasuishn0ensrnsfotfefim eion'Lr (s
;s-excunsBEHion'Lr  btsh1. I ;t tix-DARGS'xhm	(mp.iet tix--1u;a
fsv(n
so om   a-	o		etUIrastmrlloo e(os (-pt  s -p)d; enuTER  m	(ncluo;.h)uo;.h)fi;efi'pns sev(n
su    mfuo;- re ;eastmrlloo e(folv	-occuluo'numsdou; 
 		a
 
11,u-buff cp6  (--accphi0e ou; odge+ e 1+)		etUIrat  Itet;omanon"Setw)
 (ot aal* eeeal to 'fs geouhpnsp.ii`` =nsnt-d 
hi0e:
s(e\
f
= (ctylleo gesyhhar(cmti, ))-ffr'ftt nuet=-m onUo ahre)oo 	
n 0 ftt  "eEtneCms.		s(e\
f
PREFIX-LENGTHar(cmullintti  al mintipno-p lo e(foof
PREFIXs 
l
NUMar(cmullintti  al mintipn1. To leaEHAr(fnu	tmrlloo e(fin```` =nti
 g
rl ee(fooumeeic effin(foouSeh-d
;;'
(0eeooniolv	ION-SOURCE,ooniolv	ION-PREFIX-FUNC	IONtUIraNON-PREFIX-oniolv	IONto 'fs geouhp   tM`alre effinye)vi p;;	 (```` o 'fs geouhleft in```` =ns`` =o;ms.		manw eeeal 's
tmrlloo e(oseg a VALsyhhar(cm
= (ctyll(SOdo;ms.		manw eeeal -keo (-pt  p   ; 'qut)fix-cln'  o, ))-ffr')
 (ot aal* eeeal == b=
/baeman: tonlyc)y) p;;eIra	 c)y) p;;h ambdum omafotfadE.6
;;)oo e(fot	,e
`a,:u
	c)y) p;;ntew. (lislit
 yCms.		s+chre)oo tneeman: tonlyc)y) p;ev(n
sIra	 c)y) p;;hmakh)theclu	 aetup-guo;.h) oeffctc;efttpnsfurr =ve=Elloprle, ))n1))	 to 'fs geou pos)
;c;xhEn um oma		etUIr ambdum omac)y) p;;l) pos)
;c;xhEn um oma		etUIrodge+ e 1+)		etUIrat  odge+ e 1+)		etUIrat ( pos)
;c;xhEn um oma		etUIrfotfar ambdum omahighlotfa-fotf( pos)
;c;xhEn um oma		etUIrutoarity 100( pos)
;c;xhEn um oma		etUIr
 		a
 
11,u-buff 
 		a
 
11,u-buff( pos)
;c;xhEn um oma		etUIr
 		a
 
11,cp6  (--accphi0 cp6  (--accphi0pnsfurr =ve=Ema		etUIkeymap "\S-n -ontvmap;hmakh)spolse-keymapocculuo;.h);c;xhEn um oma		etUIrkeymap mapoion'Lr (set-keymap(pole eion'Lr  map "\S- s=) oum 	(
e  )		etUIrat (yion'LAY.ot\S- s=)TheMEageoa
n ))o``` =nsnt-d
;; mp.-"i	)ut     d      
 		a
 
11,u-buffevsdo ab+'
(0eignoadu\).eoa
nmapoilsosabxhioguo; oi "x;ar)a h s galrea; oi "x;ar)u\).eoa
nmapoilsaa h s galrea\).eoa
nmapootpnsfurr =tmodc)y) p;;loIa ab+ globpushdc)y) p;; h s galrea   .o```` tax-a e
`a,:"x;ar)a	d; e-
nynt)n1))	 to 'fs geou po
;c;xhEn um oma		etUIrattios") ))ietnn;f oum "\S(synmbven'astmrlloo e(fo pos)
;c;xhEn um oma		etUIruttmrlloo e(foastmrlloo e(fot "\S(s e(feTtmrlloo e(fo
;f,oe))
tadE:	 	etUIrastmrlloo e(fots "\S(t)
;c;xhEn um oma		etUIruttmrlloo e(forefanop;) (ered	seeman;c;xhEn um oma		etUIr
 		a
 
11s
-d ni ````sbnn;f oum "\S(synSehynm))
;c;xhEn um oma		etUIr
 		a
 
11,ynmtaar(s 		a
 
11s
0;o( 		ae"\S(synmbven'ynm))
;c;xhEn um oma		etUIr
 		a
 
11,ynmtynm)-a e
`a,:rof chahnu	n 0 f)y) p;ev(v	-curr =xuTER  m	(ncluo;.h)uo;.hguo;.h)theclu	 vc)y) p;;;*a >
:a
(keep(popupestuff,nte
r)
 (ot aal* eeeal ,`Uo ahleanson		E.6
;iw
\
f
KEEP-POPUPMrmne(foonlyIutovge-(deot aal*ne(eo		-op "x frame
asso'fn(foeothe = (ctyl
;,u-:		etUIov,oe))pTMEageoa
n s   ;efi'ns -d m``yof ambdumotgpos)
guo;.h)theclu	 vc)y) p;n s   ;efi'ns -d m``yof ambdumot  s)
guo;.h)theclu	 Itet;omanon
adE,lh s -o		pooo  .o```` tax)
guoqdc)y) p;; h s galrea   .o```` tax-a =xuTER  m	(ncluo;.h)uo;.hfadE.6
;;)oo e(f-luo;- re ;e (-pt  ;er
of cha)
 (ot aal* eeeal = eeealppmr-i  ch,.
\(ou ort Ipotoe fs(g'o tnepo "eEts, ftt  s:rof chesrl,erUo om\)"non
adE,lo e(f-lo
; boadn(o e(fot	,e
`a,:an(     s gsI  TtCmr-i	t POINTeman -ont  .o```` tax-
,ur f ate=V'fnofoo;xh'

;; psynta    s gsI eeealppmr-iPOINT(el cludmr-izeroloo e(fo pos)n
sIra	 c)y) p;` tax)
    s gs-ib;o dfo to 'fs gnd
;,ubalasyntao   .o```` tax-
,ur 
;,ub		etUIov,oe))n s   Ir ambdum omac)y) p;2
(lr  b6
; (EFtfwV'fnofo ooa
 
11,ue
;; psynta    s gsI)oo fot	t POINTemas)n
sIra	 c)y) p;` tax)
    s gs-ib;(1-  (-pt  s  'fs gnd
;,ubalasyntao   .o```` tax-
,ur 
;,ub		etUI serstv,oe))n s   Ir ambdum omac)y) p;2
(lr  b6
; lr  b6
; (=trfo e(fotsn(    s  'fs gnd
;,u6
; (EFtfwV'fnofo ooa
 
11,ue
;; psynta    s gsI  TtCmr-i	t POINTemas)n
sIra	 c)y) p;` tax)
    s gs-ib;o dfo (1+'olv	-occuluo;.h)alasyntao   .o```` tax-
,ur 
;,ub		etUI serstv,oe))n s   Ir ambdum omac)y) p;2
(lr  b6
; lr  b6
; (=trfo e(fot  TtCel  s  'fs gnd
;,u6
; (EFtfwV'fnofo ooa
 ,u6
; -a =xuTER  m	(ncluo;.h)uo    s gs-ib;(  TtCeoa-ni;er
of chaasyntlntinueomo -sto   s gsIteeween START'(v1 END
"
)
;;  s   ;efi'nsIteeween START'(v1 ENDeman -ontvc` tax)
    s gs-ib;  TtCeoa-n gnd
;,u6
  .o```` tax-
,ur b+)	il.6
; eeeal =a ab+ globalasyntao  ` tax-
,ur 
;b		etUIov,oe))n s   Ir ambdum omac)y) p;2
(lr  b6
bpushdc
  .o```` tax-tpnsfurr =rof chahnu	 eeeal =a ab+ glo  .o```` tax-a euTER  m	(ncluo;.h)uo;.h-m`TheNi1iolluo;- re ;e eeeal = og
oa-ni;er
o
	,(ca
e;-lv	-occuluohpnsp.ii`` =nsnt-dIra	manw f
`-d
;; sfaf-    oi -m`TheNi1io)		etUIrat s-(e\Ao		)
 (ot aal* eeeal =h, 'fs gegby
= (ctyllwillu)ehleft alftt,
Uo ah s iouo n(cmtarb	
h s ionfd
;; eal? x t, h, 'fni)
 `so as
 (li=ns	d; ey=o;- re syntdr ))iethfa  ch,.
\
f
BEG'(v1 ENDpd
;;h, 'fs ge-e fs(gh s iouo n(cteeween BEG'(v1
ENDpd
;;-m`TheN- re  e  oeftt of
BEG'o
;ENDpis;h, 'fs ge-enye)(n
sei  al mintipn`  ch,nmax'"\
ta  ch,nmin'	-os,p) ad= Se
;,u-: oum "\S(ss ge-Ug
	 (lioa-n 
sIra	 sn( (  ch,nmax)ot "\S(s e(f(Ets

;g)ioa-n
sIra	 -Ug
	  ch,nmin(oa
 
11n -ont  .o```` tax-
,ur a,:uM`alr
= (ctylle  )	-os
.6
;;)sdEts

;eween BEG'(v1;ENDpaar(h, 'fs ge)o;xh's)a	-Ug
;;olasyntao  h s galrea   .o```` tax-f	s=): tonlystfoeeh', fotfadt(
	dou; e=)<trfo e(fotsn(    
;g)
	dou; e=)>trfo e(fot  TtCel  oa-n g	 b6
bpushdc
  .o```` tax-tpnsfurn
sIra	 c)y) p;` tax)
guoqdc)y) p;; h s galrea   .o```` tax-a =xuurn
sI
;s-excunsBEHion'Lr f oum "\S- gnyhhleaEHA
e;-lv	-occuluoh(bx? i	-oonmzeroloo e(feftts2
(lr  b6tneh'h s -o		poo    oi -m`TheNi1io)		etUIrat s 'leaEHs (
om  ah''q		(. oion p		etUI=trfo e(fot  TtCel  rfo e(fotsn(       	`` h s galrea;) ;e   itUIvvandou; e=  .o```` tax-a e
`S- gnyhh;) ;e  
e;-lv	-occuluo;e=r  b6tneh'h s -o		poo    oi -m`TheNi1io)		etUIrat s 'LUE, ((0(
om  ah''q		(. oion yhhar(snt-d
;;otfasdEtwu ortmtarbt=-m onU) ;e  itfot
(n
sor=)>tr (-pt  tfo e(fotsn(  see;-)	<os (-pt try-m`TM	etUIrat (yion'LAYee;-)  buads)a		Uo aeov,oe))n s   Irodge+ e 1+)		etUIrat ( 	osabxh  e=lEageoa
n s   Irastmrllrepls(gda( 	osabx0'
;f,oe))
tadE:Irastmrlloo e(fotsotg )'Lr f o s galrea;) ;e   itUIvg )'L	(f(n
sefat
+c)
 (ot aal* eeealpsbt=-m onso jugeOoadE,l 	's=;cvc)y) p; 
Ie:(e=fotfadE.6
;;)oo TERFAdcn asuishre   	`` h s galreage;guo;.h)theclu	 I    	`` h s galreage;gu-o(tesonif,ne fs(gyl\vvandou; e=  .o```` tax-a e
`S- gnyhhextr(cm
e;-lv	-occuluo;e=r  b6tneh'h s -o		poo    oi -m`TheNi1io)		etUIrat s 'extr(c(i(
om  ah''q		(. oion yhhar(snt-d
;;otfasdEtwu ortmtarbt=-m onextr(cmiwfot
(n
sor=)>tr (-pt  tfo e(fotsn(  see;-)	<os (-pt try-m`TM	etUIrat (yion'LAYee;-)  buads)a		Uo aeov,oe))n s   Irodge+ e 1+)		etUIrat ( 	osabxh  e=lEageoa
n s   Irastmrllrepls(gda( 	osabx0'
;f,oe))
tadE:Irastmrlloo e(fotsotg )'Lr f o s galreaextr(cmoitUIvg )'L	(f(n
sefat
+c)
 (ot aal* eeealpsbt=-m onso jugeOoadE,l 	's=;cv-`Thecluoniolv	-occulu=o;- re syntdn(     s g   	``oadE,l	)	iu posfo e(fotdE,uol   rfo e(fotsn(       	`` h s galreage;guo;.h)theclu	 I    	`` h s galreage;gu-o(tesonif,ne fs(gyl\vvandou; e=  .o```` tax-a e
`S- gnyhhask 'em;e=r  b6tneh'h s -o		poo    oi -m`TheNi1io)		etUIrat s 'Lsk(0	(m
;s-excunsBEHixh'
om  ah''q		(. oion h'
gob+'
		-gefo e(fotsn(       	`;o provistreos)n h I ;cod.		fs(gon h'
;c;xhEn um omIrfotfar(). Dgr ))i;color=. "red"     	``)a		yoor ge+ "A	-oonm os)	o;xh'?Ncre	dou; e=) oo  a g(ti;) ;e   itUIvg )'Lr f o s galreaextr(cmoitUIv       .o```` tax-a oa
 
11ntipvla l-inth s galrea   .o```` tax-furn
sIra	  h s galreage;--o(tesond;o( 		aeruTER  m	(ncluo;.h)uo -m`TheNis llinttluo;- re ;e eeeal = "Executen(ai;er
o
	,(cas llinttite-occulu==nsp.ii`` =nscugeomiznd n fIra	mans(e\
f
= (ctylleo(yhhar(cmti
 g

 atii (fotfene(nryi`` =ns	io\(ftt nuo (-pt. T
h s ionfhage-Ut.6
;)ehminguo;= (ctyll(SOy evipet
mosqui=nswillusg ))o  n u

nmtfadgu-th(e\
f
0ctemdn(/(SOSYNTAXpd
;;hhhar(cm
ndpo  ab+'
(0eignoadue fs'COMqr(fotnionro
	,(cas llinttite-occulu==suisfadEahnu	,e
`a,:EN
0cte
x t, hser)(onlanlySYNTAXph)
 =fibxhedp= b=
/ba xx t,au
	a,:u 
vitR.e:
mBnghfotfadEe)x-cln'  oe      eeeal =it chhhar(cmti ryo=ns	io\(ftt nub=
/baeman: tonlyc)y) p;;eIra	 c)y) p;;h ambdum omafotfadE.6
;;)oo e(fot	,
;;dEael ()
	-`Thecluoniolv	-occuluo' (lisub=
/baemanncluo;.h)uo;.h-m`TheNi1iolv	-curr =x,uyhhar(o;;  nop.h)
 (ot aal*l,efim eegqmod		  SeIrat (yion'Ln -ont-m`TheNi 'Lr  b	(f`fo  ab+'
(0eignoadue fs'COM r(fotnione  )	thleasteftt of
0ctemoooens gnyhhSYNTAXph)
 yhhar(cmti, ))up
yntax, eviffr'0ctemdn(hSYNTAXion'Lr f)a		Uo a		etUIrat (yion'LAY.ot\S- s==)TheMEageoa
n ))o``` =nsnt-d
;; mp.-"i	)ut     d       
 		a
 
11,u-buffeot\S- s==)ofe "Executen(alaxh'
adE,lael ()
	m`Th
v ab+'
(0eignoadu, ))up(cmosyntax- "Executen(alve=)	(f(n
sefat
+c` oo  a g(ti;) ;e oor ;dum omby-mamic slld;.hrmines;nye)		(fn fdhar	oo	"NT	ete: Ira	manwael ()
	e) 'extr(c ca
 gsuo;- re synt);eween fim eiore, ))sT;Uo ak-rmne(c)y) p;;loIbemao e(fo, TEichdE.6
;;s:o   s giore, ))sT;cUs\).ea-	o		etU`R.;hnu	,ast of
'?rfe(fo-Us\).Tbeca
 g
sfiore, ))sT;wu ort `atorypos -o,s-=fo
;; oum 	
(+tfoeeh'h s -o	g(ti;) ;e oor ;dum omby-mamic s 'extr(c(i(S- s==)Theh s -o	g(ti;) ;e oor ;dum omby-mamic s '?rfe(fo-Us\).ylaxh'
adE,lael ()
	'extr(c(v 	
(t'
adE,lael ()
	'LUE, (((oa
 fadE.6
f oum "\S- gnyhhar(al e,qmanegqmodea b6(seh'pe"Exar-'extr(c(ion'Lr  bel
reg `atorypoo eo-p (fo occu(n'h s -o	g(tonextr(cmnk   
vitum
;Ite ;	oor(f  (-pt  tfo e(fotdE,uol i		etUIrl            )	(f-acc	(foextr(cmoitUIrat (yio          	(f(n
sefat
+c  leg cAdcntfadEe)dE.6
; boadn;u
	keep TERFAdcno          	(f	(feponsfotfaieion'Lr  b6
noadE,l	)	iu pos (-pt  tfo e(fotsn(     s ge-U          )	(f-acc	(foge;guo;.h)theclu	 Itet;oma	)ro p== mm
  al pnseomman 
v``` sodea b6(seh'pe"Exar-'LUE, ((0        )	(f-acc	(fo;) ;e   itUI   s ge-Ue
`S- gnyhhaotfadEe) In 
Ie:(e=fotfadE.6
;;sahnu	,e
`a,:u
	nbx? x t,au
e
`S- gnyhhe
;;nsre	) ;e  havoo;e=r  b6tt )	(f-acc	(foge;guo;.h)theclu	 Itet;oma	)r=r  b6		aeruTER  m	(ncluo;.h)uo -un
    oumo,s-=emannclsvah var-
nynt oumo,s-=;;*a >
:a
(	  Sai;er
m	(COMMAND al CONDI	IONtrmne(foonl(e\
f
WHENar(cmulll(SO'i (fotf,he
; TERFAdcn wpotoenk   
viIbembnofoo=nsnt-dkey Irqufaf-eus.		  )onvoke`;e   nclsvah  "eEts
minguo;.
)
 (ot aal* eeeal  re  WHENar(c'nsfotfa(SO'dE.6
,he
; hnu	nk   

bioo fotnsfotfa(SOdE.6
;COMMAND(e\VARIABLEt Ipotoebema)  tl,u

 atigu-o(tesonssnt-dkeymap in TEich
COMMAND ypon ))ie  (- t;sa-d
;; o nve=Ee) onl( It  s:rove=Eo eo-p
elrmne(nyasufaccphi0(e\
:
fad.		  )be)onvoke aedir(e= oefr ioo r(e= o) viama)key
Irqufaf-e6
;.hkeymapx-cln'  oEFtfwVUo akg'\
t "eexecu	manwaecunsBotfamod		  Sencluo;.h)uo trap-aecunsBEHion'Ldrg'\
t(RecunsBot pp.ishoc` oo  a g(ti -un
    oumo,s-=';\
;hhhar(cm
var-
nynt-`Tb
ny;;ntew gesdisfotnhkeymap"-a e
`a,:run nclsvah  "ee
;;nsrensfotf,o(SO "ee
;;nsrei (fotfeaah CONDI	IONx,uyhhamne(foonl,u-:		etUIofoeeh'		etU'nsfotfrl            ) serstfoeyntax)  Saoeeh'		etU'i (fotf)2
(lr  b6
; lr  b6
 oumo,s-=).
,ur f oosvah-execu	e nclsvah-a e
`a,:run TERFAdcn wpotoenk   
viIbembnofo =nsnt-dkey Irqufaf-,e
`a,:" tonlye
;;nsrei (fotfeaah CONDI	IONhamne(foonl,u-:: tonlys serstfoeyntax)  Saoeeh'		etU'i (fotf)2
 oumo,s-=) "\S-n -ontvncluo;.h)uo trap-aecunsBEH c(ion'Lr  b6
(    otfa(e-d

var-
nyn)2
(lr  b6
; nclsvah-nsfurn
sIra
var-
nynto( 	 `S- gnyhhwe=tmod(nyast-d
;; s-keys)shoc`unit
 - oosvah-evge-s' effinyen `S- gnyhhre it
 nttiuo;(SdEN
loIk- 	esdkey Irqufaf-etranslnd n ftakav;;ls(gnsfurn
sIra	 unit
 - oosvah-evge-sanasyn-fy-key-Irqufaf-e(nyast-d
;; s-keys)tpnsfurn
sIra	 nclsvah (key-bioo fot(it
 -key-Irqufaf-eo( 	 fs gnd
;,ubunwioostmrtr(c on'Lr  b6
(		etUInclsvahp nclsvah-nsfurn
      )	(fsvah-execu	e nclsvah-nsfurn
      )Ira	  l-innclsvah nclsvah-agny;ntew geswork -onlobbelre lndEN
:(
n
      )Ira
var-
nynt    otf oa
 
11a,:run nclsvah  "ee
;;nsredE.6
,u-:		etUIeh'		etU'dE.6
) f oosvah-execu	e nclsvah-aaeruTER  m	(ncluo;.h)uo -un
   adE.6
;Irat (yionnnclsvah var-
nynt;*a >
:a
(	  Sai;er
m	(COMMAND al minguo;.h)
 (ot aal* eeeal  e\
f
WHENar(cmulll(SO'i (fotf,he
; TERFAdcn wpotoenk   
viIbembnofoo=nsnt-dkey Irqufaf-eus.		  )onvoke`;e   nclsvah  "eEts
minguo;.
)
 (ot aal* eeeal  re  WHENar(c'nsfotfa(SO'dE.6
,he
; hnu	nk   

bioo fotnsfotfa(SOdE.6
;COMMAND(e\VARIABLEt Ipotoebema)  tl,u

 atigu-o(tesonssnt-dkeymap in TEich
COMMAND ypon ))ie  (- t;sa-d
;; o nve=Ee) onl( It  s:rove=Eo eo-p
elrmne(nyasufaccphi0(e\
:
fad.		  )be)(onvoke adir(e= oefr ioo r(e= o) viama)key
Irqufaf-e6
;.hkeymapx-c  )	(f-acc	(fo -un
    oumo,s-=ema nclsvah var-
nynth ambdum omafotfadE.6
;;)oo e(fo(	  Sa =xuTER  m	(ncluo;.h)uo    x?fotftmanhf (tese-(ap', )Npodeestuff,nte
r'fs 	)dgetlntinueomo -se(NOT;;n---- ,efim ee"eman -ontvwos tu e(fotpnsfursI
;s-excunsBEHion'Lr fsh1. I ;t tix-p', )Npodeestu'Lr foadE,l	)	iu powos tu e(fotp; enuTER  m	(ncluo;.h)uo)mgi;;nsr-of;guo;wri(t tix-uo;- re ;e (-pt  ;er
of chae(foonl  "ePOINT(ypoo e (gi;;nsr=o(0a THING.
\(POINT(  al mintipno-p  (-pt\)e"eman: tonlytese-(aadE,lo e(f-lu e(fotpnsfsI
;s-excunsBEHion'L
gob+'
		-g (-pt  ;eman -ontn ))is2
(lr  bs sers<lo e(f-lu e(fnmax)onsfurn
     sIra	 - ))isntn ))is-of;t tixhf (tese-(orM-<seemaurn
     s=lo e(f-l`Th
n ))is2otp; enuTER  m	(ncluo;.h)uoadE.6
;guo;wri(t tix-uo;- re ;e (-pt  ;er
of chae(foonl  "ePOINT(ypominguo;(SOdt(elrmne(ehTHING.
\(POINT(  al mintipno-p  (-pt\)e"eman: tonlytese-(aadE,lo e(f-lu e(fotpnsfsI
;s-excunsBEHion'L
gob+'
		-g (-pt  ;eman -ontn ))is2
(lr  bs sersIra	 - ))isntn ))is-of;t tixhf (tese-(orM-<seemaurn
     s>lo e(f-l`Th
n ))is2oemaurn
     s<lo e(f-l	refn ))is2otp; enuTER  m	(ncluo;.h)uoe "Eof;guo;wri(t tix-uo;- re ;e (-pt  ;er
of chae(foonl  "ePOINT(ypoo eelrmne(ehTHING.
\(POINT(  al mintipno-p  (-pt\)"eman: tonlytese-(aadE,lo e(f-lu e(fotpnsfsI
;s-excunsBEHion'L
gob+'
		-g (-pt  ;eman -ontn ))is2
(lr  bs sers>lo e(f-lu e(fnmin)onsfurn
     sIra	 - ))isntn ))is-of;t tixhf (tese-(orM-<seemaurn
     s=lo e(f-l`refn ))is2otp; enuTER  m	(ncluo;.h)uowosnhf (tese--as-evge-nsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntWINDOW,==sua e 
 g-1aclint
ovge-((idge-ipp.tipno-p ovge-(nya-(0eotoebemtriggelre byaclintodeae 
 gnbx?t-==1oo eo-p oophleft corngetlnto-p glyphd-UoPOSI	ION effiWINDOWnaSeic s-ipno-p  (-o,s-==ne( `atory; hnu
Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p glyphx-cln'e: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpneman -o*ntvwos tu snhf (tese-e (-o,s-==wioofwseemaurn
   (x-y tu snhx-y s -p)emaurn
   (edge(oswioofw(i (idg-pixe;-edge(owioofwseemaurn
   (wiohx-y fwioofw(tixe;-edge(owioofwsepnsfurr =tmjugeOffr'wioofwVedge(furn
sIra`Th
vnth`ref2ms -pnsfuraurn
   (cluoh(+-l`Th
x-y)-l`Th
Vedge()TM	et`Th
wiohx-y-agnstfodx 0)2
(lr  b6
; lr  b6
h(+-l`dh
x-y)-l`Tdh
edge()TM	et`Tdh
wiohx-y-agstfody 0)2tpnsfurs tax)'e 
 g-1awos 1p; enuTER  m	(ncluo;.h)uoadoofw(tesnhf (tese-nsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;er
of chapixe;e (-o,s-==ne(nophleft lntinrngetglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntWINDOW. D al mintipno-p  (-o,s-=
ne( `atory; hnu Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p glyphx

Seemalsoc` oo  a g(tiwioofw(i (idg-pesnhf (tese-' eff
`-d
;; sfaf-frame-pesnhf (tese-'x-cln'e: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpneman -ontvx-y tu snhx-y tu snhf (tese-e (-o,s-==wioofwse(0        )edge(oswioofw(i (idg-pixe;-edge(owioofwseemaurn
  (wiohx-y fwioofw(tixe;-edge(owioofwsepnsfur(cluoh(+-l`Th
x-y)-l`Th
Vedge()TM	et`Th
wiohx-y-agstfodx 0)2
(lr  b6
; (+-l`dh
x-y)-l`Tdh
edge()TM	et`Tdh
wiohx-y-agstfody 0)2tp enuTER  m	(ncluo;.h)uoadoofw(i (idg-pesnhf (tese-nsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlnto-p o
PRE=reary; WINDOW. D al min
ipno-p  (-o,s-==ne( `atory; hnu Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p glyphx

Seemalsoc` oo  a g(tiwioofw(pesnhf (tese-' eff
`-d
;; sfaf-frame-pesnhf (tese-'xx-cln'e: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpnman -ontvx-y tu snhx-y tu snhf (tese-e (-o,s-==wioofwse(pnsfur(cluoh(+-l`Th
x-y)-ltfodx 0)2 (+-l`dh
x-y)-ltfody 0)2tp enuTER  m	(ncluo;.h)uoframe-pesnhf (tese-nsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntframe
 out 	)tix-WINDOW. D al min
ipno-p  (-o,s-==ne( `atory; hnu Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p glyphx

Seemalsoc` oo  a g(tiwioofw(pesnhf (tese-' eff
`-d
;; sfaf-wioofw(i (idg-pesnhf (tese-'x-cln'e: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpneman -ontvx-y tu snhx-y tu snhf (tese-e (-o,s-==wioofwse(0        )edge(oswioofw(i (idg-pixe;-edge(owioofwsepnsfur(cluoh(+-l`Th
x-y)-l`Th
Vedge()TMtfodx 0)2
(lr  b6
; (+-l`dh
x-y)-l`Tdh
edge()TMtfody 0)2tp enuT=== Ipose utiles
;; *s  `fl`fl  `fl>
:
mper
 auto )    Self-e fs(gyf cre, ))sT;ommand
;,ummmmCd
;eEFbility StuffT===Iutovge-(bogus
-d nilcn war)tixs
(e-d
-	  S--d nilc
r foa m	(ncluo;.h)uo cd
;eE-wioofw(offIras)(dummyt  s)
gu m	(ncluo;.h)uo cd
;eE-frame-pesnhf (tese-nsfsfsuo;- re ;earg1earg2earg3earg4tp enuTE: tonlysfn ))ip 'pesnhf (tese-pnemangu m	(ncluo;.h)uo cd
;eE-frame-pesnhf (tese-nsfsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;e;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntframe
 out 	)tix-WINDOW. D al min
ipno-p  (-o,s-==ne( `atory; hnu Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p
glyphx-csfsfs: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpnema)
;;  s  wioofw(exleEFIOtwoso,s-==in u
t;salnti;- re syn ;eman -o*ntvx-y tcd
;ute-mo,s-==fwioofw(dE,uo) '(0 . 0anon'Lr                            (-o,s-=
on'Lr                           (cluoh(wioofw(width)h(wioofw(heotfa)2
(lr  b6
; lr  b6
h              (wioofw(width)
(lr  b6
; lr  b6
h              cv-`Tb.t Ipoto gesbem0
on'Lr                           (cluoh(wioofw(hscroll) 0anon'Lr                           wioofwseemaurn
     (x
vnth 1
x-y)eemaurn
     (y
vnth 2
x-y)eemaurn
     (offIra )	(f-acc	(fo cd
;eE-wioofw(offIras)wioofwseemaurn
     (    otfa(e 
 g-pixe;- (-o,s-=seemaurn
     pixe;- (-
 
11,ue
;; os)n effi    otfae 
 gnwoso,s-==
 enftwoso,s-==in u
t;saln
11,ue
;; o;- re syntipn s  woso,s-==in pixe;o;e=r  b(set-e 
 g-poso,s-==fwioofw(frame
wioofwsnon'Lr                     (+-x-l`Th
offIra)2 (+-y-l`dh
offIra)2pnsfurn
sIra	 pixe;- (--l`dh
(e 
 g-pixe;- (-o,s-=se);e=r  b(set-e 
 g-pixe;- (-o,s-=-l`Th
    otf -l`Tdh
    otf 
on'Lr                           (cddh
    otf 
 
11,ue
;; rof chapixe;e (-o,s-=;e=r  b(setf-l`Th
pixe;- (-
h(+-l`Th
pixe;- (-
h(tfodx 0)2
)'Lr f dh
pixe;- (-

)'Lr f+TM	et`dh
pixe;- (-

)xh'
/ysframe-
		--heotfa=fwioofw(frame
wioofws) 2)eot\S- s==)ofedy 0)2t

     pixe;- (-
 enuTmangu m	(ncluo;.h)uo cd
;eE-wosnhf (tese--as-evge-nsfsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;e;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntWINDOW,==sua e 
 g-1aclint
ovge-((idge-ipp.tipno-p ovge-(nya-(0eotoebemtriggelre byaclintodeae 
 gnbx?t-==1oo eo-p oophleft corngetlnto-p glyphd-UoPOSI	ION effiWINDOWnaSeic s-ipno-p  (-o,s-==ne( `atory; hnu
Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p
glyphx-ccsfsfs: tonlywioofwVsIra	 wioofwVsIrle,q		-wioofwotpnsfsfs: tonlyte-o,s-==fos;h'woso,s-==fwioofw(tese-(wioofwotpnema)
;;  s  wioofw(exleEFIOtwoso,s-==in u
t;salnti;- re syn ;eman -o*ntvx-y tcd
;ute-mo,s-==fwioofw(dE,uo) '(0 . 0anon'Lr                            (-o,s-=
on'Lr                           (cluoh(wioofw(width)h(wioofw(heotfa)2
(lr  b6
; lr  b6
h              (wioofw(width)
(lr  b6
; lr  b6
h                      cv-`Tb.t Ipoto gesbem0
on'Lr                           (cluoh(wioofw(hscroll) 0anon'Lr                           wioofwseemaurn
     (x
vnth 1
x-y)eemaurn
     (y
vnth 2
x-y)eemaurn
     (offIra )	(f-acc	(fo cd
;eE-wioofw(offIras)wioofwseemaurn
     (    otfa(e 
 g-pixe;- (-o,s-=seemaurn
     sframe=fwioofw(frame
wioofws)emaurn
     sedge(oswioofw(edge(owioofwseemaurn
     pixe;- (-
 
11,ue
;; os)n effi    otfae 
 gnwoso,s-==
 enftwoso,s-==in u
t;saln
11,ue
;; o;- re syntipn s  woso,s-==in pixe;o;e=r  b(set-e 
 g-poso,s-==fwioofw(frame
wioofwsnon'Lr                     (+-x-l`Th
offIra)2 (+-y-l`dh
offIra)2pnsfurn
sIra	 pixe;- (--l`dh
(e 
 g-pixe;- (-o,s-=se);e=r  b(set-e 
 g-pixe;- (-o,s-=-l`Th
    otf -l`Tdh
    otf 
on'Lr                           (cddh
    otf 
 
11,ue
;; cluvertapixe;e (-o,s-==-(e\fframe-exleEFIOtipnwioofw(exleEFIO
11,ue
;; (nyas(ypocrudn effiwillufanl e.g.  "e
 enftdi ))oge-(sizeoo;xh'

;; fouts);e=r  b(set`Th
pixe;- (-TM	et`Th
pixe;- (-
h1
on'Lr                      (*ysframe-
		--widthfframe)-l`Th
edge()se);e=r  b(set`dh
pixe;- (-TM	et`dh
pixe;- (-
h1
on'Lr                      (*ysframe-
		--heotfa=frame)-lnth 1
edge()s
on'Lr                      (/ysframe-
		--heotfa=frame)-2) 
 
11,ue
;; rof chaaufakp ovge-( out 	)tix-o-p  (-o,s-=
e=r  b(set`Th
pixe;- (-TM+-l`Th
pixe;- (-
h(tfodx 0)2);e=r  b(set`dh
pixe;- (-TM+et`dh
pixe;- (-
h)ofedy 0)2t

     s tax)'e 
 g-1as tax)wioofwV (-o,s-==pixe;- (-
 
 enuTmangu m	(ncluo;.h)uo cd
;eE-adoofw(tesnhf (tese-nsfsfsuo;- re ;e (-o,s-==wioofwVdx dy  ;e;er
of chapixe;e (-o,s-==ne(nophleft corngetlntglyph-m sPOSI	IONt
exleEFIOtipnoophleft corngetlntWINDOW. D al mintipno-p  (-o,s-=
ne( `atory; hnu Irle,q		=wioofw-UoDX effiDY(h, 'fsy,e
`a,:ENDoffIras)-(e\fo-p oophleft lnto-p
glyphx-csfsfs -ontvx-y tncluo;.h)uo cd
;eE-frame-pesnhf (tese-n		 (-o,s-==wioofwVdx dy laxh'
wiohx-y f	(f-acc	(fo cd
;eE-wioofw(offIras)wioofwset

     scluoh(+-l`Th
x-y)-l`Th
wiohx-y-a
)'Lr f+TM`dh
x-y)-l`dh
wiohx-y-a 
 enuTcre,Borrow.		f(e\fsena ot.e;e(Iv-`Tmi gnI'llugFIOtitufred)  (- I'mTcre,f	)tshed...pnemangu m	(ncluo;.h)uo cd
;eE-wioofw(offIras)(;*a >
:a
(	ioofwsnon'Lr
of chaoffIras)lntWINDOW exleEFIOtipnWINDOW's=frame.
Rof chaaucluohcellu(XOFFSET . YOFFSET)nso o-p  (-o,s-==(X . Y))in
WINDOW OM rqup.tipno-p  (-o,s-==(f+TX XOFFSET)n.r f+TY YOFFSET))
y; WINDOW'S=frame.-csfsfs -o*ntvwioofwVh)ofewioofwVsIrle,q		-wioofwotpnsfsf       (e       (wioofw(edge(owioofwseemaurn
     (left    (nth 0 eseemaurn
     (ooph    (nth 1 eseemaurn
     (rotfa= 
vnth 2
eseemaurn
     (bo?t-m 
vnth 3
eseemaurn
     (xn
     (+hleft (/ys- rotfa=left)-2) 
 maurn
     (yn
     (+hooph (/ys- bo?t-m oop)-2) 
 maurn
     (w (-T   sclp.ii`sons 'fsadoofw(t scluohx y)-wioofwseemaurn
     (xoffIra 0
 maurn
     (yoffIra 0
t

     s	oorcluop ws -pnsfuraurn
 s -o*ntvf =fwioofw(frame
wioofws)emaurn
     aurn
 scy (/y1.0ysflom ssframe-
		--heotfa=f))) 
 maurn
     
sIra	 xoffIra s- x-l`Th
ws -p)emaurn
            yoffIra sflom ss--y-l`dh
w (-
 
 e            re,If Emacs 21=tmodto:e            re,- XOFFSETno-p WINDOW left margin Tidth.e            re,- YOFFSETno-p heotfa=lntheadgetlines;abs)n WINDOW. maurn
     
s	oor> emacs-major-versBEH 20anon'Lr           (-`Tg=
on'Lr             sIra	 w (-T   scluoh(+-left xoffIra) 0.0anon'Lr                   bo?t-m 
vflom sbo?t-m)2
(lr  b6
; lr  b6
h(whiynth<-l`dh
w (-
sbo?t-m)non'Lr               s	oorTheMclp.ii`sons 'fsadoofw(t w (-Twioofwsnon'Lr                       'headge(lX=isnon'Lr                   sIra	 yoffIra s+ yoffIra cy 
 e                    sIra`dh
w (- f+TM`dh
w (-
scy 
 e                  sIra	 xoffIranon'Lr                   sflotfoe+ xoffIranon'Lr                             stfoe`Th
vadoofw(margin(owioofwseemaurn
                                0)))) 
 maurn
     
sIra	 yoffIra sflo(SOy ffIra)2pt

     scluohxoffIra y ffIra)2pnuTmangu m	(ncluo;.h)uo cd
;eE-lX=i-ynmbvehf (tes tu ssnon'Lr
of cha(narrow.	
sbg ))o lX=isnnmbve- ,efi-o,s-==POS.
\(D al mintipno-p  (m ee\)"ema  (1+'(cou (-lines;lu e(fnmin) s -p)dnuTmangu aliasIr
 		a
 
11,cosnhf (tese--as-evge-nsfsf'ncluo;.h)uo cd
;eE-wosnhf (tese--as-evge-)Tmangu aliasIr
 		a
 
11,frame-pesnhf (tese-nsfsf'ncluo;.h)uo cd
;eE-frame-pesnhf (tese-)Tmangu aliasIr
 		a
 
11,adoofw(tesnhf (tese-nsfsf'ncluo;.h)uo cd
;eE-adoofw(tesnhf (tese-)
)nuT=== Ipose utiles
;; *s  `fl`fl  `fl>
:
mper
 auto )    Self-e fs(gyfyf cre, ))sT;ommand
;,ummmmmCd
;eEFbility hredsT===IIr(o;;as llinttEmacs versBEH ntew geshhhaouol i		etUhkeybioo fos hrlf===Idecint)
 `haIOtipnsimulndE(o;;me
 enfto-p
==I` oo  a g(ti -un
   adE.6
;Irat (y' hred. Soufar,    Emacs versBEH hhhaouos
  oEF fos o 'fs lyOffr'zeroloo e(fef.6
;;)sonso we=tlw;)sdhaIOtipnv``;e  !

:		etUI<= emacs-major-versBEH 21u;a
f	(f-acc	(fo simulndEtfadE.6
;bioo fos ncluo;.h)uo    la
nmap ncluo;.h)uomap "\S- s=================================='ncluo;.h)uouiu;a
f	(f-acc	(fo simulndEtfadE.6
;bioo fos  ab+'
(0eignoadu\).eoa
nmap "\S- s==================================		etUIrat (yion'Lap "\S- s=================================='		etUIrat (yion'LAY. tp enuT=== Ipose utiles
;; *s  `fl`fl  `fl>
:
mper
 auto )    Self-e fs(gyff cre, ))sT;ommand
;,Loadeus.rif,ne fs(gs	d;uleSehp-`Thede='ncluo;.h)uouiu;(rrquire='ncluo;.h)uoui(popup-frameu;(rrquire='ncluo;.h)uoui(dynamicu;(rrquire='ncluo;.h)uoui(hotkeys);(rrquire='ncluo;.h)uoui(echo);(rrquire='ncluo;.h)uoui(tooltip);(rrquire='ncluo;.h)uoui(menu);(rrquire='ncluo;.h)uoui(u-buffs)nuT=