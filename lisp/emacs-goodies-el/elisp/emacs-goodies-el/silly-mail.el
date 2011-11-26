;;; silly-mail.el --- generate bozotic mail headers

;; Compilation Copyright (C) 1993, 94, 95, 96, 97, 98, 99, 2000 Noah S. Friedman

;; Contributors: Noah Friedman, Jamie Zawinski, Jim Blandy,
;;               Thomas Bushnell, Roland McGrath,
;;               and a cast of dozens.
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: extensions, mail
;; Status: works in Emacs 19 and XEmacs.

;; $Id: silly-mail.el,v 1.3 2009-09-04 02:31:00 psg Exp $

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; To use this, put the following in your .emacs:
;;
;;    (autoload 'sm-add-random-header "silly-mail" nil t)
;;    (add-hook 'mail-setup-hook 'sm-add-random-header)

;; I solicit more randomly generated headers commands.

;; Some of the options in this program require some external packages which
;; are not a standard part of emacs, e.g. shop.el and flame.el (flame.el is
;; present in XEmacs and Emacs 18, but missing from Emacs 19).  These are
;; available from http://www.splode.com/users/friedman/software/emacs-lisp/

;;; Code:

(require 'sendmail)

(random t)

(defvar sm-mail-header-table
  '(sm-add-antipastobozoticataclysm
    (sm-add-at&t-hype           youwill             "youwill")
    sm-add-drdoom-fodder
    sm-add-emacs-name
    sm-add-emacs-taunt
    (sm-add-flame               *flame              "flame")
    (sm-add-horoscope           horoscope           "horoscope")
    (sm-add-kibology            kibologize          "kibologize")
    sm-add-meat
    sm-add-microsoft
    sm-add-nsa-fodder
    (sm-add-shopping-list       shop-string         "shop")
    sm-add-tom-swifty
    sm-add-tomato
    (sm-add-uboat-death-message uboat-death-message "uboat")
    sm-add-x-taunt
    sm-add-zippy-quote)
  "List of routines which generate silly mail headers.
Each element is either a symbol or a list.
If an element is a function, that function can be called.
If an element is a list, it is composed of three elements:
   1. A function to call which generates a header.
   2. A symbol naming a function required by the header-generator.
      If this function is not defined, the header-generator cannot run.
   3. The name of a library to load if the required function isn't defined.
      If the load fails, or if `sm-load-missing-libraries' is `nil',
      the corresponding header-generator function won't be used.")

(defvar sm-load-missing-libraries t
  "*If non-`nil', load missing libraries for header functions.
If nil, then if a library is not already loaded, the dependent
header-generating function will not be used.")

;;;###autoload
(defun sm-add-random-header ()
  "Insert a random silly mail header.
The choice of available headers is taken from sm-mail-header-table."
  (interactive)
  (funcall (sm-random-header-function)))

;;;###autoload
(defun sm-add-all-headers ()
  "Insert one of every kind of silly mail header defined.
The choice of available headers is taken from sm-mail-header-table."
  (interactive)
  (let ((fns sm-mail-header-table)
        fn)
    (while fns
      (setq fn (sm-use-header-function-p (car fns)))
      (and fn
           (funcall fn))
      (setq fns (cdr fns)))))

(defun sm-random-header-function ()
  (let ((fn nil))
    (while (null fn)
      (setq fn (sm-use-header-function-p
                (nth (random (length sm-mail-header-table))
                     sm-mail-header-table))))
    fn))


(defun sm-use-header-function-p (func)
  (cond ((consp func)
         (let ((fn (nth 0 func))
               (fbound-sym (nth 1 func))
               (lib (nth 2 func)))
           (cond ((fboundp fbound-sym)
                  fn)
                 ((and sm-load-missing-libraries
                       (load lib t)
                       (fboundp fbound-sym))
                  fn))))
        (t func)))


(defvar sm-fill-single-line-width   78)
(defvar sm-fill-multi-line-width    70)
(defvar sm-fill-indent-width         3)

(defun sm-sequence-item (sequence n)
  (cond ((or (vectorp sequence)
             (stringp sequence))
         (aref sequence n))
        ((listp sequence)
         (nth n sequence))
        (t
         (signal 'domain-error (list 'sequencep sequence)))))

(defsubst sm-random-sequence-item (sequence)
  (sm-sequence-item sequence (random (length sequence))))

(defsubst sm-random-range (lower upper)
  (+ lower (random (- upper lower))))

(defun sm-random-sequence-items (&optional sequence lower upper)
  (and (null lower)
       (setq lower 0))
  (let ((seqlen (length sequence))
        (count (if upper
                   (sm-random-range lower upper)
                 (random lower)))
        items tem)
    (while (not (zerop count))
      (setq tem (sm-sequence-item sequence (random seqlen)))
      (or (memq tem items)
          (setq items (cons tem items)
                count (1- count))))
    items))

(defun sm-put-header-fill-content (header contents)
  (let ((buf (generate-new-buffer " *sm-temp*"))
        (header-length (+ 2 (length header)))
        (single-width sm-fill-single-line-width)
        (multi-width sm-fill-multi-line-width)
        (indent-width sm-fill-indent-width)
        (do-fill (function
                  (lambda (fill-column)
                    (fill-region-as-paragraph (point-min) (point-max))
                    ;; Emacs 19 fill functions add an extra newline
                    (cond ((char-equal ?\C-j (char-after (1- (point-max))))
                           (goto-char (point-max))
                           (delete-char -1)))
                    (= (count-lines (point-min) (point-max)) 1)))))
    (save-excursion
      (set-buffer buf)
      (insert contents)
      (cond
       ((funcall do-fill (- single-width header-length)))
       (t
        (or (funcall do-fill (- single-width indent-width))
            (funcall do-fill (- multi-width indent-width)))
        (goto-char (point-min))
        (insert "\n")
        (indent-rigidly (point-min) (point-max) indent-width)))
      (setq contents (buffer-string))
      (kill-buffer buf)))
  (sm-put-header header contents))

(defsubst sm-put-random-sequence-items (header sequence &optional range)
  (sm-put-header-contents header
    (apply 'sm-random-sequence-items sequence range)))

(defsubst sm-put-header-multiline-content (header items)
  (sm-put-header-contents header
    items
    (concat "\n" (make-string sm-fill-indent-width ?\040))))

(defsubst sm-put-header-contents (header items &optional separator)
  (sm-put-header header
    (mapconcat 'identity items (or separator " "))))

(defun sm-put-random-sequence-items-to-eol (header sequence &optional sep)
  (or sep (setq sep " "))
  (let ((width (- sm-fill-single-line-width (length header) 2))
        (seqlen (length sequence))
        (len 0)
        (continuep t)
        items tem new-len)
    (while continuep
      (setq tem (sm-sequence-item sequence (random seqlen)))
      (setq newlen (+ len (length sep) (length tem)))
      (cond ((and (> newlen width)
                  (consp items))
             (setq continuep nil))
            ((memq tem items))
            (t
             (setq items (cons tem items))
             (setq len newlen))))
    (sm-put-header header (mapconcat 'identity items sep))))

;; Add the specified header to the current mail message, with the given
;; contents.  If the header already exists, its contents are replaced.
(defun sm-put-header (header contents)
  (save-excursion
    (let ((buf-mod-p (buffer-modified-p))
          (header-exists (mail-position-on-field header)))
      (if header-exists
          (let ((end (point))
                (beg (progn
                       (re-search-backward (concat header ": "))
                       (goto-char (match-end 0)))))
            (delete-region beg end)))
      (insert contents)
      (set-buffer-modified-p buf-mod-p))))

(put 'sm-put-header-fill-content 'lisp-indent-function 1)
(put 'sm-put-header-contents 'lisp-indent-function 1)
(put 'sm-put-header 'lisp-indent-function 1)


;; A private joke

(defvar sm-antipastobozoticataclysm-header
  "X-Antipastobozoticataclysm")

(defvar sm-antipastobozoticataclysm-table
  ["Bariumenemanilow"
   "When George Bush projectile vomits antipasto on the Japanese."])

(defun sm-add-antipastobozoticataclysm ()
  (interactive)
  (sm-put-header-fill-content sm-antipastobozoticataclysm-header
    (sm-random-sequence-item sm-antipastobozoticataclysm-table)))


(defvar sm-at&t-hype-header "X-AT&T-Hype")

(defun sm-add-at&t-hype ()
  (interactive)
  (require 'youwill)
  (sm-put-header-fill-content sm-at&t-hype-header (youwill)))


;; This is sort of based on the same principle as the NSA Fodder header.
;; In 1991, the MOD used to break into the FSF machines and read our email,
;; looking for security-related information.

(defvar sm-drdoom-fodder-header "X-Drdoom-Fodder")

(defvar sm-drdoom-fodder-words
  ["CERT" "crash" "crypt" "drdoom" "passwd" "security" "root" "satan"])

(defvar sm-drdoom-fodder-length-range
  (list 5 (length sm-drdoom-fodder-words)))

(defun sm-add-drdoom-fodder ()
  (interactive)
  (sm-put-random-sequence-items sm-drdoom-fodder-header
                                sm-drdoom-fodder-words
                                sm-drdoom-fodder-length-range))


(defvar sm-emacs-name-header "X-Emacs-Acronym")

;; These have been contributed by people all over the network
;; (see the file etc/JOKES or emacs.names in the Emacs 19 distribution).
;; I modified some of them.
(defvar sm-emacs-name-table
  ["Each Mail A Continued Surprise"
   "Each Manual's Audience is Completely Stupified"
   "Easily Maintained with the Assistance of Chemical Solutions"
   "Easily Mangles, Aborts, Crashes and Stupifies"
   "Eating Memory And Cycle-Sucking"
   "Editing MACroS"
   "Edwardian Manifestation of All Colonial Sins"
   "Egregious Managers Actively Court Stallman"
   "Eight Megabytes And Constantly Swapping"
   "Eleven Monkeys Asynchronously Create Slogans"
   "Elsewhere Maybe All Commands are Simple"
   "Elsewhere Maybe Alternative Civilizations Survive"
   "Elvis Masterminds All Computer Software"
   "Emacs Macht Alle Computer Schoen"
   "Emacs Made Almost Completely Screwed"
   "Emacs Maintainers Are Crazy Sickos"
   "Emacs Makes A Computer Slow"
   "Emacs Makes All Computing Simple"
   "Emacs Manuals Always Cause Senility"
   "Emacs Manuals Are Cryptic and Surreal"
   "Emacs Masquerades As Comfortable Shell"
   "Emacs May Alienate Clients and Supporters"
   "Emacs May Allow Customised Screwups"
   "Emacs May Annihilate Command Structures"
   "Emacs Means A Crappy Screen"
   "Emacs: My Alternative Computer Story"
   "Embarrassed Manual-Writer Accused of Communist Subversion"
   "Embarrassingly Mundane Advertising Cuts Sales"
   "Emetic Macros Assault Core and Segmentation"
   "Energetic Merchants Always Cultivate Sales"
   "Equine Mammals Are Considerably Smaller"
   "Eradication of Memory Accomplished with Complete Simplicity"
   "Erasing Minds Allows Complete Submission"
   "Escape Meta Alt Control Shift"
   "Esoteric Malleability Always Considered Silly"
   "Even My Aunt Crashes the System"
   "Even a Master of Arts Comes Simpler"
   "Evenings, Mornings, And a Couple of Saturdays"
   "Eventually Munches All Computer Storage"
   "Ever Made A Control-key Setup?"
   "Every Male Adolescent Craves Sex"
   "Every Mode Accelerates Creation of Software"
   "Every Mode Acknowledges Customized Strokes"
   "Every Moron Assumes CCA is Superior"
   "Everyday Material Almost Compiled Successfully"
   "Excavating Mayan Architecture Comes Simpler"
   "Excellent Manuals Are Clearly Suppressed"
   "Exceptionally Mediocre Algorithm for Computer Scientists"
   "Exceptionally Mediocre Autocratic Control System"
   "Experience the Mildest Ad Campaign ever Seen"
   "Extended Macros Are Considered Superfluous"
   "Extensibility and Modifiability Aggravate Confirmed Simpletons"
   "Extraneous Macros And Commands Stink"
   "Generally Not Used (Except by Middle Aged Computer Scientists)"]
  "EMACS acronym expansions.")

(defun sm-add-emacs-name ()
  (interactive)
  (sm-put-header sm-emacs-name-header
    (sm-random-sequence-item sm-emacs-name-table)))


;; Jim Blandy (and possibly Karl Fogel?) started this and contributed
;; most of the phrases.

(defvar sm-emacs-taunt-header "Emacs")

(defvar sm-emacs-taunt-table
  '["(setq software-quality (/ 1 number-of-authors))"
    "a Lisp interpreter masquerading as ... a Lisp interpreter!"
    "a compelling argument for pencil and paper."
    "a learning curve that you can use as a plumb line."
    "a real time environment for simulating molasses-based life forms."
    "an inspiring example of form following function... to Hell."
    "anything free is worth what you paid for it."
    "ballast for RAM."
    "because Hell was full."
    "because editing your files should be a traumatic experience."
    "because extension languages should come with the editor built in."
    "because idle RAM is the Devil's playground."
    "because one operating system isn't enough."
    "because you deserve a brk today."
    "don't cry -- it won't help."
    "don't try this at home, kids!"
    "ed  ::  20-megaton hydrogen bomb : firecracker"
    "featuring the world's first municipal garbage collector!"
    "freely redistributable; void where prohibited by law."
    "if SIGINT doesn't work, try a tranquilizer."
    "if it payed rent for disk space, you'd be rich."
    "impress your (remaining) friends and neighbors."
    "it's all fun and games, until somebody tries to edit a file."
    "it's like swatting a fly with a supernova."
    "it's not slow --- it's stately."
    "Lovecraft was an optimist."
    "more boundary conditions than the Middle East."
    "more than just a Lisp interpreter, a text editor as well!"
    "no job too big... no job."
    "or perhaps you'd prefer Russian Roulette, after all?"
    "Our Lady of Perpetual Garbage Collection"
    "resistance is futile; you will be assimilated and byte-compiled."
    "the Swiss Army of Editors."
    "the answer to the world surplus of CPU cycles."
    "the definitive fritterware."
    "the only text editor known to get indigestion."
    "the prosecution rests its case."
    "the road to Hell is paved with extensibility."
    "there's a reason it comes with a built-in psychotherapist."
    "well, why *shouldn't* you pay property taxes on your editor?"
    "where editing text is like playing Paganini on a glass harmonica."
    "you'll understand when you're older, dear."]
  "Facts about Emacs that you and your loved ones should be aware of.")

(defun sm-add-emacs-taunt ()
  (interactive)
  (sm-put-header sm-emacs-taunt-header
    (sm-random-sequence-item sm-emacs-taunt-table)))

(setq bizarre-gratuitous-variable '(miscellaneous gratuitous list))


;; Add an insulting flame into your mail headers.

(defvar sm-flame-header "X-Flame")

(defun sm-add-flame ()
  (interactive)
  (or (fboundp '*flame)
      (fboundp 'flame-string)
      (load "flame"))
  (sm-put-header-fill-content sm-flame-header
    (if (fboundp 'flame-string)
        ;; friedman's flame.el
        (flame-string)
      ;; XEmacs/Emacs-18 flame.el
      (sentence-ify (string-ify (append-suffixes-hack
                                 (flatten (*flame))))))))


(defvar sm-horoscope-header "X-Horoscope")

(defun sm-add-horoscope ()
  (interactive)
  (require 'horoscope)
  (sm-put-header-fill-content sm-horoscope-header (horoscope)))


;; Add words of wisdom from the grepmeister.

(defvar sm-kibology-header "X-Kibo-Says")

(defun sm-add-kibology ()
  (interactive)
  (require 'kibologize)
  (sm-put-header-fill-content sm-kibology-header (kibologize)))


;; Contributed by David LaMacchia <dml@topped-with-meat.com>

(defvar sm-meat-header "X-Meat")

(defvar sm-meat-table
  ["Abalone"
   "Back Bacon"
   "Bacon"
   "Beef Jerky"
   "Biltong"  ; african-style jerky, usually beef, ostrich, or antelope
   "Blood sausage"
   "Buffalo"
   "Calimari"
   "Chicken Fried Steak"
   "Chicken"
   "Clam Jerky"
   "Duck"
   "Flanken"
   "Haggis"
   "Ham"
   "Head cheese"
   "Liverwurst"
   "Lobster"
   "Long pork"
   "Molinari"
   "Olive Loaf"
   "Parma"
   "Prosciutto"
   "Ptarmigan"
   "Roo burgers"
   "Salame"
   "Spruce grouse"
   "Squirrel"
   "Swordfish"
   "Turkey Jerky"
   "Veal"
   "Venison"
   "Wallaby steak"])

(defun sm-add-meat ()
  (interactive)
  (sm-put-header sm-meat-header
    (sm-random-sequence-item sm-meat-table)))


;; From Karl Fogel <kfogel@red-bean.com>

(defvar sm-microsoft-header "Microsoft")

(defvar sm-microsoft-table
  ["I'm not laughing anymore."
   "Making the world a better place... for Microsoft."
   "Programs so large they have weather."
   "We've got the solution for the problem we sold you."
   "Where `market lock-in' means throwing away the keys."
   "Where even the version numbers aren't Y2K-compliant"
   "Where the service packs are larger than the original releases."
   "With our software, there's no limit to what you can't do!"
   "World domination wasn't enough -- we had to write bad software, too!"])

(defun sm-add-microsoft ()
  (interactive)
  (sm-put-header sm-microsoft-header
    (sm-random-sequence-item sm-microsoft-table)))


(defvar sm-nsa-header "X-NSA-Fodder")

(defun sm-add-nsa-fodder ()
  (interactive)
  (or (fboundp 'snarf-spooks) (load "spook"))
  (sm-put-random-sequence-items-to-eol sm-nsa-header (snarf-spooks)))


;; Inspiration for this came from Brian Rice, a sicko genius.

(defvar sm-shopping-list-header "X-Shopping-List")

(defvar sm-shopping-list-count '(3 . 6))
(defvar sm-shopping-list-multi-line-p t)

(defun sm-add-shopping-list (&optional item-count)
  (interactive "P")
  (require 'shop)
  (cond ((or (null item-count)
             (and (consp item-count)
                  (null (cdr item-count))))
         (setq item-count sm-shopping-list-count)))
  (let ((items (shop-string-numbered-list (if (consp item-count)
                                              (shop-random-range
                                               (car item-count)
                                               (cdr item-count))
                                            item-count))))
    (cond (sm-shopping-list-multi-line-p
           (sm-put-header-multiline-content sm-shopping-list-header
                                            (cons "" items)))
          (t
           (sm-put-header-contents sm-shopping-list-header items "; ")))))


;; Tom Swifties.  Blame for these go mainly to Noah Friedman
;; and Thomas (nee Michael) Bushnell.

(defvar sm-tom-swifty-header "X-Tom-Swifty")

(defvar sm-tom-swifty-table
  '["\"All the cherry trees are dead,\" Tom said fruitlessly."
    "\"And what should you set your PS1 shell variable to?\" Tom prompted."
    "\"Any fresh fruit in the kitchen?\" Tom asked peeringly."
    "\"C++ is the wave of the future,\" Tom said objectively."
    "\"Care for some `suan la chow show'?\" Tom asked wantonly."
    "\"Condensed chicken soup,\" was Tom's canned response."
    "\"Darling, what vegetable becomes an act of passion when misspelled?\", Tom breathed ravishingly."
    "\"Eat me,\" was Tom's biting response."
    "\"Ed is the Standard Text Editor,\" Tom sed."
    "\"Evergreens have always been my favorite,\" Tom opined."
    "\"He came at me out of the blue,\" Tom said airily."
    "\"I am writing lots of little verses,\"  Tom said blankly."
    "\"I can't drink alcohol,\" Tom said spiritually."
    "\"I can't get this fire started,\" Tom said woodenly."
    "\"I can't stand baby food,\" Tom said in a strained voice."
    "\"I can't wait to see the doctor,\" Tom said impatiently."
    "\"I don't WANNA get drunk,\" Tom wined."
    "\"I don't have any piano music,\"  Tom said listlessly."
    "\"I don't have the slightest idea how to milk this cow,\" Tom said in utter confusion."
    "\"I don't understand how square roots work,\" Tom said irrationally."
    "\"I don't want any champagne!\" Tom said, blowing his top."
    "\"I feel like I'm running around in circles,\"  Tom said squarely."
    "\"I got to get a text-processor that does my files the right way,\" Tom said awkwardly."
    "\"I guess I shouldn't have broken the mirror,\" Tom reflected."
    "\"I hate Frere Jacques,\" Tom said as he roundly denounced it."
    "\"I have no intention of traversing binary trees!\", Tom barked."
    "\"I have to finish sorting these writing utensils,\" Tom said pensively."
    "\"I hope this emulsion works,\" Tom said in suspense."
    "\"I just burned my hand in the blast furnace,\" Tom said, overwrought."
    "\"I just don't understand the number seventeen,\" Tom said randomly."
    "\"I just got some chicken wire,\" Tom said defensively."
    "\"I just poisoned myself,\" Tom lyed."
    "\"I just sharpened my pencil,\" Tom said pointedly."
    "\"I like Gregorian chants,\" Tom intoned."
    "\"I like amputations,\" Tom said disarmingly."
    "\"I like sun cartridge tapes,\" Tom said quickly."
    "\"I never get good bridge hands,\" Tom said in passing."
    "\"I only like black and white,\" Tom said monotonously."
    "\"I really like penguins,\" Tom said in a flighty voice."
    "\"I recommend listening to radio station ``WHAT'',\" Tom said quietly."
    "\"I think it's time we got married,\" Tom said engagingly."
    "\"I train dolphins,\" Tom said purposefully."
    "\"I'll have to grade your test again,\" Tom remarked."
    "\"I'm completely bankrupt,\" Tom said senselessly."
    "\"I'm fond of Pavarotti,\" Tom said menacingly."
    "\"I'm gainfully employed at the Weight-Watchers gymnasium,\" Tom said wastefully."
    "\"I'm getting fat,\" Tom said expansively."
    "\"I'm going to copy this tape,\" Tom said for the record."
    "\"I'm hardly ever aware of what I'm going to do next,\" Tom said unconsciously."
    "\"I'm having deja-vu,\" Tom said again."
    "\"I'm really bored,\" Tom said flatly."
    "\"I'm sorry I broke your window,\" Tom said painfully."
    "\"I'm sorry to hear I knocked you up,\" Tom said after a pregnant pause."
    "\"I've burned my tongue,\" Tom said distastefully."
    "\"I've finished counting the horses,\" Tom said summarily."
    "\"I've got a bucket full of forearms,\" Tom said wistfully."
    "\"I've just been drafted,\"  Tom said impressively."
    "\"I've made a complete ash of myself,\" Tom said brazenly."
    "\"IBM is up 3 points,\" Tom said, taking stock of the situation."
    "\"If only we could piece together this crime,\" Tom said in a puzzled voice."
    "\"It needs more seasoning,\" Tom said sagely."
    "\"It's patently obvious,\" Tom said licentiously."
    "\"It's really cold out here,\" Tom said in a muffled voice."
    "\"It's really windy outside,\" said Tom with gusto."
    "\"Lisp is such a symbol-minded language,\" Tom commonly said."
    "\"My feet hurt,\" Tom said pedantically."
    "\"My lenses will stay perfectly clear,\" Tom said optimistically."
    "\"My mouse buttons don't work,\" Tom said in a depressed voice."
    "\"My terminal is completely screwed up,\" Tom cursed."
    "\"On the other hand, eating at a table is more civilized,\" Tom countered."
    "\"Quick!  Change the baby's diaper,\" Tom said rashly."
    "\"Socialism is dead,\" Tom communicated."
    "\"The ASCII standard sucks,\" Tom said characteristically."
    "\"The GNU project will probably not be Posix conformant,\" Tom said noncommittally."
    "\"The judge sentenced him to the chair,\" Tom said dielectrically."
    "\"The printer is using too much toner,\"  Tom said darkly."
    "\"The rooster was decapitated,\" Tom said in a crestfallen voice."
    "\"The sequence `M-4' is equivalent to `C-u 4',\" Tom said metaphorically."
    "\"The sky is falling,\" Tom said in a crushed voiced."
    "\"The sun just rose over the cemetary,\" Tom said in mourning."
    "\"This anesthetic isn't very effective,\" Tom said unnervingly."
    "\"This awl is broken,\" Tom said pointlessly."
    "\"This is illegal, I just know it,\" Tom said with conviction."
    "\"Turn that fan off,\" Tom said coldly."
    "\"VI is much better than EMACS,\" Tom said with joy."
    "\"Wait! You need to enable interrupts first!\" Tom said preemptorally."
    "\"We'll have to take the stairs,\" Tom said in an elevated voice."
    "\"We're all out of flowers,\" Tom said lackadaisically."
    "\"We're going to sue you for that window system,\" Tom said inexorably."
    "\"We're going to use decimal notation,\" Tom said tentatively."
    "\"Well, I guess we should pitch camp,\" Tom said tentatively."
    "\"Well, it didn't increase at all,\" Tom said, nonplussed."
    "\"What is today's date?\" Tom asked in a timely fashion."
    "\"When will the Hurd be released?\" Tom asked Machingly."
    "\"Who drank the last beer?\" Tom asked, hopping mad."
    "\"You have new mail,\" Tom said in his usual delivery."
    "\"You light up my life,\" Tom said brightly."
    "\"You pinhead,\" Tom said pointedly."])

(defun sm-add-tom-swifty ()
  (interactive)
  (sm-put-header-fill-content sm-tom-swifty-header
    (sm-random-sequence-item sm-tom-swifty-table)))


;; I think Lars Bader came up with this one first.
;; Lately Jim Blandy and others have used it also.
;;
;; It's a test to see if any mailers break because they can't actually
;; implement oddly-colored tomatos, or something like that.

(defvar sm-tomato-header "Tomato")

(defvar sm-tomato-table
  ["Beige"
   "Green"
   "Heliotrope"
   "Mauve"
   "Plaid"
   "Polka-dot"])

(defun sm-add-tomato ()
  (interactive)
  (sm-put-header sm-tomato-header
    (sm-random-sequence-item sm-tomato-table)))


(defvar sm-uboat-death-message-header "X-Uboat-Death-Message")

(defun sm-add-uboat-death-message ()
  (interactive)
  (require 'uboat)
  (sm-put-header-fill-content sm-uboat-death-message-header
    (uboat-death-message)))


;; Most of these came from the unix-haters mailing list.
;; Jamie Zawinski added more later.

(defvar sm-x-taunt-header "X-Windows")

(defvar sm-x-taunt-table
  '["a mistake carried out to perfection."
    "a moment of convenience, a lifetime of regret."
    "a terminal disease."
    "all the problems and twice the bugs."
    "complex nonsolutions to simple nonproblems."
    "dissatisfaction guaranteed."
    "don't get frustrated without it."
    "even not doing anything would have been better than nothing."
    "even your dog won't like it."
    "flaky and built to stay that way."
    "flawed beyond belief."
    "foiled again."
    "form follows malfunction."
    "garbage at your fingertips."
    "graphics hacking :: Roman numerals : sqrt (pi)"
    "ignorance is our most important resource."
    "it could be worse, but it'll take time."
    "it could happen to you."
    "it was hard to write; it should be hard to use."
    "let it get in *your* way."
    "live the nightmare."
    "more than enough rope."
    "never had it, never will."
    "no hardware is safe."
    "power tools for power fools."
    "power tools for power losers."
    "putting new limits on productivity."
    "simplicity made complex."
    "some voids are better left unfilled."
    "sometimes you fill a vacuum and it still sucks."
    "the art of incompetence."
    "the cutting edge of obsolescence."
    "the defacto substandard."
    "the first fully modular software disaster."
    "the joke that kills."
    "the problem for your problem."
    "there's got to be a better way."
    "warn your friends about it."
    "you'd better sit down."
    "you'll envy the dead."]
  "What users said as they collapsed.")

(defun sm-add-x-taunt ()
  (interactive)
  (sm-put-header sm-x-taunt-header
    (sm-random-sequence-item sm-x-taunt-table)))


;; Yow!  Am I quoted in your EMAIL yet?

(defvar sm-zippy-quote-header "X-Zippy-Says")

(defun sm-add-zippy-quote ()
  (interactive)
  (or (fboundp 'yow) (load "yow"))
  (sm-put-header-fill-content sm-zippy-quote-header (yow)))

(provide 'silly-mail)

;;; silly-mail.el ends here.
