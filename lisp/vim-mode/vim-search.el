;;; vim-search.el - Search und substitute commands for ex-mode.

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;; TODO:
;;
;;  - the substitute command should be more interactive and especially an operation
;;    without the 'g' option should highlight all future occurences

;;; Code:

(eval-when-compile (require 'cl))
(require 'vim-macs)
(require 'vim-ex)

(defcustom vim:interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'vim-ex-mode)

(defcustom vim:search-case 'smart
  "The case behaviour of the search command."
  :type '(radio (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'vim-ex-mode)

(defcustom vim:substitute-case nil
  "The case behaviour of the search command."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'vim-ex-mode)

(defcustom vim:search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'vim-ex-mode)

(defcustom vim:search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'vim-ex-mode)

(defcustom vim:substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'vim-ex-mode)

(defcustom vim:substitute-interactive-replace t
  "If t and substitute patterns are highlighted the replacement is shown interactively."
  :type 'boolean
  :group 'vim-ex-mode)

(defconst vim:search-keymap (make-sparse-keymap)
  "Keymap used in search-ex-mode.")

(defvar vim:search-history nil
  "The history for the search command.")

(defvar vim:search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar vim:search-count nil
  "The count if the current search.")

(defvar vim:search-start-point nil
  "The point where the search started.")

(defvar vim:search-overlay nil
  "The overlay for the current search result.")

(defvar vim:search-pattern nil
  "The actual search pattern.")

(defvar vim:search-match-beg nil
  "The beginning position of the last match.")

(defvar vim:search-match-end nil
  "The end position of the last match.")

(defvar vim:substitute-pattern nil
  "The actual replacement.")

(defvar vim:substitute-replacement nil
  "The actual replacement.")

(defface vim:search '((t :inherit isearch))
  "Face for interactive search."
  :group 'vim-ex-mode)

(defface vim:lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'vim-ex-mode)

(defface vim:substitute '(( ((supports :underline))
                            :underline t
                            :foreground "red"))
  "Face for interactive replacement text."
  :group 'vim-ex-mode)


(define-key vim:search-keymap [return] #'vim:search-exit)
(define-key vim:search-keymap (kbd "RET") #'vim:search-exit)
(define-key vim:search-keymap (kbd "C-g") #'vim:search-abort)
(define-key vim:search-keymap [up] #'previous-history-element)
(define-key vim:search-keymap [down] #'next-history-element)
(define-key vim:search-keymap (kbd "ESC ESC ESC") #'vim:search-abort)
(define-key vim:search-keymap (kbd "\d") #'vim:ex-delete-backward-char)

;; A pattern.
(defstruct (vim:pattern
            (:constructor nil)
            (:constructor vim:make-pattern
                          (&key ((:regex re))
                                ((:case-fold ca) nil)
                                (whole-line t)
                           &aux (regex (vim:regex-without-case re))
                                (case-fold (vim:regex-case re ca)))))
  regex      ;; The pattern itself.
  case-fold  ;; The case for this pattern.
  whole-line ;; If non-nil the pattern matches the whole line,
             ;; otherwise only the first occurrence.
  )

(defun vim:regex-without-case (re)
  "Returns the regular expression without all occurrences of \\c and \\C."
  (replace-regexp-in-string
   "\\\\."
   #'(lambda (txt)
       (if (member (aref txt 1) '(?c ?C))
           ""
         txt))
   re t t))

(defun vim:regex-case (re default-case)
  "Returns the case as implied by \\c or \\C in regular expression `re'.
If \\c appears anywhere in the pattern, the pattern is case
insenstive, if \\C appears the pattern is case sensitive. Only
the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the
case specified by `default-case' is used. `default-case' should be either
'sensitive, 'insensitive or 'smart. In the latter case the pattern will be
case-sensitive if and only if it contains an upper-case letter, otherwise it
will be case-insensitive."
  (let ((start 0)
        recase)
    (while (and (not recase)
                (string-match "\\\\." re start))
      (case (aref re (1- (match-end 0)))
        (?c (setq recase 'insensitive))
        (?C (setq recase 'sensitive))
        (t (setq start (match-end 0)))))
    (or recase
        (case default-case
          ((sensitive insensitive) default-case)
          (smart (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
          (t nil)))))

;; The lazy-highlighting framework.
(vim:deflocalvar vim:active-highlights-alist nil
  "An alist of currently active highlights."
  )


(defstruct (vim:hl
            (:constructor vim:make-highlight))
  name       ;; The name of this highlight.
  pattern    ;; The search pattern.
  face       ;; The face for this highlights.
  window     ;; The window where this highlight has been started.
  beg        ;; The minimal position for the highlighting.
  end        ;; The maximal position for the highlighting.
  update-hook ;; Hook to be called when the lazy highlighting.
  match-hook ;; Hook to be called when a single lazy highlight pattern has been setup.
  overlays   ;; The currently active overlays.
  )

(defun* vim:make-hl (name &key
                          (face 'vim:lazy-highlight)
                          (win (selected-window))
                          (beg nil)
                          (end nil)
                          (update-hook nil)
                          (match-hook nil))
  "Creates new highlighting object with a certain `name'."
  (unless (symbolp name) (error "Excepted symbol as name of highlight."))
  (when (assoc name vim:active-highlights-alist)
    (vim:delete-hl name))
  (when (null vim:active-highlights-alist)
    (add-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll nil t)
    (add-hook 'window-size-change-functions #'vim:hl-update-highlights-resize nil))
  (push (cons name (vim:make-highlight :name name
                                       :pattern nil
                                       :face face
                                       :overlays nil
                                       :window win
                                       :beg beg
                                       :end end
                                       :update-hook update-hook
                                       :match-hook match-hook))
        vim:active-highlights-alist))


(defun vim:delete-hl (name)
  "Removes the highlighting object with a certain `name'."
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (vim:hl-overlays hl))
      (setq vim:active-highlights-alist
            (assq-delete-all name vim:active-highlights-alist))
      (vim:hl-update-highlights))
    (when (null vim:active-highlights-alist)
      (remove-hook 'window-scroll-functions #'vim:hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions #'vim:hl-update-highlights-resize))))


(defun vim:hl-active-p (name)
  "Returns t iff the highlight with a certain name is active."
  (and (assoc name vim:active-highlights-alist) t))


(defun vim:hl-change (name new-pattern)
  "Sets the regular expression of the highlighting object with
name `name' to `new-regex'."
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-pattern hl)
            (if (zerop (length new-pattern))
                nil
              new-pattern))
      (vim:hl-idle-update))))


(defun vim:hl-set-region (name beg end)
  (let ((hl (cdr-safe (assoc name vim:active-highlights-alist))))
    (when hl
      (setf (vim:hl-beg hl) beg
            (vim:hl-end hl) end)
      (vim:hl-idle-update))))


(defun* vim:hl-update-highlights ()
  "Updates the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr vim:active-highlights-alist))
    (let ((old-ovs (vim:hl-overlays hl))
          new-ovs
          (pattern (vim:hl-pattern hl))
          (face (vim:hl-face hl))
          (match-hook (vim:hl-match-hook hl))
          result)
      (condition-case lossage
          (progn
            (when pattern
              (dolist (win (if (eq vim:interactive-search-highlight 'all-windows)
                               (get-buffer-window-list (current-buffer) nil t)
                             (list (vim:hl-window hl))))
                (let ((begin (max (window-start win)
                                  (or (vim:hl-beg hl) (point-min))))
                      (end (min (window-end win)
                                (or (vim:hl-end hl) (point-max))))
                      last-line)
                  (when (< begin end)
                    (save-excursion
                      (goto-char begin)
                      ;; set the overlays for the current highlight, reusing old overlays
                      ;; (if possible)
                      (while (and (vim:search-find-next-pattern pattern)
                                  (< (match-beginning 0) (match-end 0))
                                  (<= (match-end 0) end))
                        (when (or (vim:pattern-whole-line pattern)
                                  (not (equal (line-number-at-pos (match-beginning 0)) last-line)))
                          (setq last-line (line-number-at-pos (match-beginning 0)))
                          (push (if old-ovs
                                    (progn
                                      (move-overlay (car old-ovs)
                                                    (match-beginning 0)
                                                    (match-end 0))
                                      (overlay-put (car old-ovs) 'face face)
                                      (pop old-ovs))
                                  (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                                    (overlay-put ov 'face face)
                                    (overlay-put ov 'vim:hl (vim:hl-name hl))
                                    (overlay-put ov 'priority 1000)
                                    ov))
                                new-ovs)
                          (when match-hook (funcall match-hook (car new-ovs)))
                          )))))))

            (mapc #'delete-overlay old-ovs)
            (setf (vim:hl-overlays hl) new-ovs)
            (if (or (null pattern) new-ovs)
                (setq result t)
              ;; maybe the match could just not be found somewhere else?
              (save-excursion
                (goto-char (vim:hl-beg hl))
                (if (and (vim:search-find-next-pattern pattern)
                         (< (match-end 0) (vim:hl-end hl)))
                    (setq result (format "Match in line %d" (line-number-at-pos (match-beginning 0))))
                  (setq result "No match")))))

        (invalid-regexp
         (setq result (cadr lossage)))

        (search-failed
         (setq result (nth 2 lossage)))

        (error
         (setq result (format "%s" lossage))))

      (when (vim:hl-update-hook hl)
        (funcall (vim:hl-update-hook hl) result)))))


(defvar vim:hl-update-timer nil
  "Time used for updating highlights.")


(defun vim:hl-idle-update ()
  "Triggers the timer to update the highlights in the current buffer."
  (when (and vim:interactive-search-highlight
             vim:active-highlights-alist)
    (when vim:hl-update-timer
      (cancel-timer vim:hl-update-timer))
    (setq vim:hl-update-timer
          (run-at-time 0.1 nil
                       #'vim:hl-do-update-highlight
                       (current-buffer)))))


(defun* vim:hl-do-update-highlight (&optional buffer)
  "Timer function, updating the highlights."
  (with-current-buffer buffer
    (vim:hl-update-highlights))
  (setq vim:hl-update-timer nil))


(defun vim:hl-update-highlights-scroll (win begin)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer)
    (vim:hl-idle-update)))


(defun vim:hl-update-highlights-resize (frame)
  "Updates highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (vim:hl-idle-update)))))


;; Interactive search.
(defun vim:search-next ()
  "Search for the next occurrence of pattern."
  (let ((retry t))
    (setq isearch-success nil
          isearch-error nil)
    (condition-case lossage
        (progn
          (while retry
            (let ((search-result (vim:find-next)))
              (case search-result
                ((t) (setq isearch-success t
                           isearch-wrapped nil))
                ((nil) (setq isearch-success nil
                             isearch-wrapped nil))
                (t (setq isearch-success t
                         isearch-wrapped t))))
            (setq isearch-success (vim:find-next))
            ;; Clear RETRY unless we matched some invisible text
            ;; and we aren't supposed to do that.
            (when (or (eq search-invisible t)
                      (not isearch-success)
                      (bobp) (eobp)
                      (= (match-beginning 0) (match-end 0))
                      (not (isearch-range-invisible
                            (match-beginning 0) (match-end 0))))
              (setq retry nil)))
          (setq isearch-just-started nil))

      (invalid-regexp
       (setq isearch-error (cadr lossage)))

      (search-failed
       (setq isearch-error (nth 2 lossage)))

      (error
       (setq isearch-error (format "%s" lossage))))

    (cond
     (isearch-success
      (setq isearch-other-end (if (eq vim:search-direction 'forward) (match-beginning 0) (match-end 0))))
     ((not isearch-error)
      (setq isearch-error "No match")))
    (if isearch-wrapped
        (if isearch-error
            (setq isearch-message (concat "Wrapped, " isearch-error))
          (setq isearch-message "Wrapped"))
      (setq isearch-message isearch-error))))


(defun vim:find-next ()
  "Searches the next occurrence w.r.t. actual search data,
possibly wrapping and eob or bob."
  (if (not (zerop (length (vim:pattern-regex vim:search-pattern))))
      (let (wrapped
            result
            (retry t))
        (save-excursion
          (while retry
            (setq retry (not wrapped))
            (cond
             ;; normal search
             ((vim:search-find-next-pattern vim:search-pattern
                                            vim:search-direction)
              (setq vim:search-match-beg (match-beginning 0)
                    vim:search-match-end (match-end 0)
                    result (if wrapped 1 t)
                    retry nil))

             ;; wrap and eob and bob
             ((not wrapped)
              (goto-char (case vim:search-direction
                           ('forward (point-min))
                           ('backward (point-max))))
              (setq wrapped t))

             ;; already wrapped, search failed
             (t
              (setq vim:search-match-beg nil vim:search-match-end nil
                    result nil
                    retry nil))))
          result))
    t))


(defun* vim:search-find-next-pattern (pattern &optional
                                              (direction 'forward))
  "Looks for the next occurrence of pattern in a certain direction."
  (let ((case-fold-search (eq (vim:pattern-case-fold pattern) 'insensitive)))
    (case direction
      ('forward (re-search-forward (vim:pattern-regex pattern) nil t))
      ('backward (re-search-backward (vim:pattern-regex pattern) nil t))
      (t (error "Unknown search direction: %s" direction)))))


(defun vim:search-update ()
  "Updates the highlighting and ex-info for the actual search pattern."
  (vim:ex-set-info isearch-message)
  (when vim:search-interactive
    (when isearch-success
      (goto-char vim:search-match-beg)
      (if vim:search-overlay
          (move-overlay vim:search-overlay
                        vim:search-match-beg
                        vim:search-match-end)
        (setq vim:search-overlay (make-overlay vim:search-match-beg vim:search-match-end))
        (overlay-put vim:search-overlay 'priority 1001)
        (overlay-put vim:search-overlay 'face 'vim:search)))
    (when vim:search-highlight-all
      (vim:hl-change 'vim:search (and isearch-success vim:search-pattern)))))


(defun vim:search-start-session ()
  "Called to initialize ex-mode for interactive search."
  (vim:ex-setup)
  (remove-hook 'minibuffer-setup-hook #'vim:search-start-session)
  (add-hook 'after-change-functions #'vim:search-update-pattern nil t)
  (add-hook 'minibuffer-exit-hook #'vim:search-stop-session)
  (when (and vim:search-interactive vim:search-highlight-all)
    (with-current-buffer vim:ex-current-buffer
      (vim:make-hl 'vim:search))))

(defun vim:search-stop-session ()
  "Stops interactive search."
  (with-current-buffer vim:ex-current-buffer
    ;; TODO: this is a bad fix to remove duplicates.
    ;;       The duplicates exist because isearch-range-invisible
    ;;       may add a single overlay multiple times if we are
    ;;       in an unlucky situation of overlapping overlays. This
    ;;       happens in our case because of the overlays that are
    ;;       used for (lazy) highlighting. Perhaps it would be better
    ;;       to disable those overlays temporarily before calling
    ;;       isearch-range-invisible.
    ;;  the following code is equivalent to
    ;; (setq isearch-opened-overlays nil
    ;;       (remove-duplicates isearch-opened-overlays))
    (let (ovs)
      (dolist (ov isearch-opened-overlays)
        (unless (member ov ovs) (push ov ovs)))
      (setq isearch-opened-overlays ovs))
    (isearch-clean-overlays))
  (remove-hook 'minibuffer-exit-hook #'vim:search-stop-session)
  (remove-hook 'after-change-functions #'vim:search-update-pattern t)
  (when vim:search-overlay
    (delete-overlay vim:search-overlay)
    (setq vim:search-overlay nil))
  (vim:ex-teardown))

(defun vim:search-update-pattern (beg end range)
  "Called to update the current search pattern."
  (unless vim:ex-update-info
    (setq vim:search-pattern (vim:make-pattern :regex (vim:ex-contents)
                                               :case-fold vim:search-case))
    (with-current-buffer vim:ex-current-buffer
      (with-selected-window vim:ex-current-window
        (goto-char vim:search-start-point)
        (save-excursion
          (dotimes (i (or vim:search-count 1))
            (if (eq vim:search-direction 'backward)
                (backward-char)
              (forward-char))
            (vim:search-next)
            (when vim:search-match-beg
              (goto-char vim:search-match-beg))))
        (vim:search-update)))))


(defun vim:search-exit ()
  "Exits interactive search, lazy highlighting keeps active."
  (interactive)
  (vim:search-stop-session)
  (exit-minibuffer))

(defun vim:search-abort ()
  "Aborts interactive search, disables lazy highlighting."
  (interactive)
  (vim:search-stop-session)
  (vim:delete-hl 'vim:search)
  (abort-recursive-edit))


(defun vim:start-search (direction count)
  "Starts a new search in a certain direction."
  ;; store buffer and window where the search started
  (let ((vim:ex-current-buffer (current-buffer))
        (vim:ex-current-window (selected-window)))
    (setq vim:search-count count)
    (setq vim:search-direction direction)
    (setq vim:search-start-point (point))

    (condition-case err
        (progn
          ;; ensure minibuffer is initialized accordingly
          (add-hook 'minibuffer-setup-hook #'vim:search-start-session)
          ;; read the search string
          (let ((minibuffer-local-map vim:search-keymap))
            (when (read-string (case vim:search-direction
                                 ('forward "/")
                                 ('backward "?"))
                               nil 'vim:search-history)
              (goto-char vim:search-start-point)
              (vim:add-jump)
              (if vim:search-match-beg
                  (goto-char vim:search-match-beg)
                (vim:find-next))
              (vim:adjust-point))))
      (quit
       (vim:search-stop-session)
       (vim:delete-hl 'vim:search)
       (goto-char vim:search-start-point)
       (vim:adjust-point)
       (signal (car err) (cdr err))))))



;; Search commands.
(vim:defmotion vim:motion-search-next (exclusive count)
  "Goes to the next occurrence."
  (setq vim:search-start-point (point))
  (vim:add-jump)
  (dotimes (i (or count 1))
    (case vim:search-direction
      ('backward (backward-char))
      (t (forward-char)))
    (vim:search-next)
    (if isearch-success
       (progn
         (when (and vim:search-highlight-all
                    (not (vim:hl-active-p 'vim:search)))
           (vim:make-hl 'vim:search)
           (vim:hl-change 'vim:search vim:search-pattern))
         (goto-char vim:search-match-beg))
      (goto-char vim:search-start-point))
    (when (or isearch-error isearch-wrapped) (ding))
    (when isearch-message
      (let (message-log-max)
        (message "%s" isearch-message)))))

(vim:defmotion vim:motion-search-next-reverse (exclusive count)
  "Goes the the previous occurrence."
  (let ((vim:search-direction
         (if (eq vim:search-direction 'backward) 'forward 'backward)))
    (vim:motion-search-next :count count)))

(vim:defmotion vim:motion-search-fwd (exclusive count)
  "Starts a forward search."
  (vim:start-search 'forward count))

(vim:defmotion vim:motion-search-bwd (exclusive count)
  "Starts a forward search."
  (vim:start-search 'backward count))


(defun vim:start-word-search (unbounded direction count)
  "Searches for the word under point.

If the first argument `unbounded' is nil the search matches only
at word boundaries, otherwise it matches anywhere.

The second argument `direction' should be either 'forward or
'backward determining the search direction.

The search matches the `count'-th occurrence of the word."
  ;; TODO: use thing-at-point ???
  (condition-case nil
      (goto-char (vim:motion-bwd-word-end :count 1))
    (error nil))

  (re-search-forward (concat "\\<[" vim:word "]+\\>"))

  (setq vim:search-count count
        vim:search-pattern (vim:make-pattern
                            :regex (if unbounded
                                       (regexp-quote (match-string 0))
                                     (concat "\\<" (regexp-quote (match-string 0)) "\\>"))
                            :case-fold (case vim:search-case
                                         ((sensitive smart) 'sensitive)
                                         (insensitive 'insensitive)))
        vim:search-direction direction)
  (vim:delete-hl 'vim:search)
  (vim:motion-search-next :count count))


(vim:defmotion vim:search-word (exclusive count)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'forward count))


(vim:defmotion vim:search-word-backward (exclusive count)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search nil 'backward count))


(vim:defmotion vim:search-unbounded-word (exclusive count)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'forward count))


(vim:defmotion vim:search-unbounded-word-backward (exclusive count)
  "Searches the next occurence of word under the cursor."
  (vim:start-word-search t 'backward count))


;; Substitute
(defun vim:ex-pattern-argument-activate ()
  (with-selected-window vim:ex-current-window
    (with-current-buffer vim:ex-current-buffer
      (vim:make-hl 'vim:substitute
                   :update-hook #'vim:ex-pattern-update-ex-info
                   :match-hook (and vim:substitute-interactive-replace
                                    #'vim:ex-pattern-update-replacement))
      (vim:ex-pattern-argument-update))))

(defun vim:ex-pattern-argument-deactivate ()
  (vim:ex-set-info nil)
  (with-selected-window vim:ex-current-window
    (with-current-buffer vim:ex-current-buffer
      (vim:delete-hl 'vim:substitute))))

(defun vim:ex-pattern-argument-update ()
  (when vim:substitute-highlight-all
    (multiple-value-bind (pattern replacement flags)
        (vim:parse-substitute vim:ex-arg)
      (setq flags (append flags nil))
      (with-selected-window vim:ex-current-window
        (with-current-buffer vim:ex-current-buffer
          (setq vim:substitute-pattern
                (and pattern
                     (vim:make-pattern :regex pattern
                                       :whole-line (memq ?g flags)
                                       :case-fold (or (and (memq ?i flags) 'insensitive)
                                                      (and (memq ?I flags) 'sensitive)
                                                      vim:substitute-case
                                                      vim:search-case)))
                vim:substitute-replacement replacement)
          (vim:hl-set-region 'vim:substitute
                             ;; first line
                             (if (car-safe vim:ex-range)
                                 (save-excursion
                                   (goto-line (car vim:ex-range))
                                   (line-beginning-position))
                               (line-beginning-position))
                             ;; last line
                             (if (car-safe vim:ex-range)
                                 (save-excursion
                                   (goto-line (or (cdr vim:ex-range)
                                                  (car vim:ex-range)))
                                   (line-end-position))
                               (line-end-position)))
          (vim:hl-change 'vim:substitute vim:substitute-pattern))))))

(defun vim:ex-pattern-update-ex-info (result)
  "Updates the ex-info string."
  (vim:ex-set-info (case result
                     ((t nil) nil)
                     (t result))))

(defun vim:ex-pattern-update-replacement (overlay)
  "Updates the replacement display."
  (let ((repl (vim:match-substitute-replacement vim:substitute-replacement)))
    (put-text-property 0 (length repl)
                       'face 'vim:substitute
                       repl)
    (overlay-put overlay 'after-string repl)))


(vim:define-arg-handler 'substitute
                        :activate 'vim:ex-pattern-argument-activate
                        :deactivate 'vim:ex-pattern-argument-deactivate
                        :update 'vim:ex-pattern-argument-update)


(vim:defcmd vim:cmd-substitute (motion argument:substitute nonrepeatable)
  "The VIM substitutde command: [range]s/pattern/replacement/flags"
  (vim:cmd-nohighlight)
  (multiple-value-bind (pattern replacement flags) (vim:parse-substitute argument)
    (unless pattern (error "No pattern given."))
    (unless replacement (error "No replacement given."))
    (vim:add-jump)
    (setq flags (append flags nil))
    (lexical-let* ((replacement replacement)
                   (first-line (if motion (vim:motion-first-line motion) (line-number-at-pos (point))))
                   (last-line (if motion (vim:motion-last-line motion) (line-number-at-pos (point))))
                   (whole-line (and flags (memq ?g flags)))
                   (confirm (and flags (memq ?c flags)))
                   (ignore-case (and flags (memq ?i flags)))
                   (dont-ignore-case (and flags (memq ?I flags)))
                   (pattern (vim:make-pattern :regex pattern
                                              :whole-line whole-line
                                              :case-fold (or (and ignore-case 'insensitive)
                                                             (and dont-ignore-case 'sensitive)
                                                             vim:substitute-case
                                                             vim:search-case)))
                   (regex (vim:pattern-regex pattern))
                   (last-point (point))
                   (overlay (make-overlay (point) (point)))
                   (next-line (line-number-at-pos (point)))
                   (nreplaced 0))
      (let ((case-fold-search (eq 'insensitive (vim:pattern-case-fold pattern)))
            (case-replace case-fold-search))
        (unwind-protect
            (if whole-line
                ;; this one is easy, just use the built in function
                (vim:perform-replace regex replacement confirm t nil nil nil
                                     (save-excursion
                                       (goto-line first-line)
                                       (line-beginning-position))
                                     (save-excursion
                                       (goto-line last-line)
                                       (line-end-position)))
              (if confirm
                  (progn
                    ;; this one is more difficult, we have to do the
                    ;; highlighting and questioning on our own
                    (overlay-put overlay 'face
                                 (if (facep 'isearch)
                                     'isearch 'region))
                    (map-y-or-n-p #'(lambda (x)
                                      (set-match-data x)
                                      (move-overlay overlay (match-beginning 0) (match-end 0))
                                      (concat "Query replacing "
                                              (match-string 0)
                                              " with "
                                              (vim:match-substitute-replacement replacement case-fold-search)
                                              ": "))
                                  #'(lambda (x)
                                      (set-match-data x)
                                      (replace-match replacement case-fold-search)
                                      (incf nreplaced)
                                      (setq last-point (point)))
                                  #'(lambda ()
                                      (let ((end (save-excursion
                                                   (goto-line last-line)
                                                   (line-end-position))))
                                        (goto-line next-line)
                                        (beginning-of-line)
                                        (when (and (> end (point))
                                                   (re-search-forward regex end t nil))
                                          (setq last-point (point))
                                          (setq next-line (1+ (line-number-at-pos (point))))
                                          (match-data))))))

                ;; just replace the first occurences per line
                ;; without highlighting and asking
                (goto-line first-line)
                (beginning-of-line)
                (while (and (<= (line-number-at-pos (point)) last-line)
                            (re-search-forward regex (save-excursion
                                                       (goto-line last-line)
                                                       (line-end-position))
                                               t nil))
                  (incf nreplaced)
                  (replace-match replacement)
                  (setq last-point (point))
                  (forward-line)
                  (beginning-of-line)))

              (goto-char last-point)
              (if (= nreplaced 1)
                  (message "Replaced 1 occurence")
                (message "Replaced %d occurences" nreplaced)))

          ;; clean-up the overlay
          (delete-overlay overlay))))))


(defun vim:parse-substitute (text)
  (when (string-match "\\`\\s-*/\\(\\(?:[^/]\\|\\\\.\\)+\\)\\(?:/\\(\\(?:[^/]\\|\\\\.\\)*\\)\\(?:/\\([giIc]*\\)\\)?\\)?\\s-*\\'"
                      text)
    (let* ((pattern (match-string 1 text))
           (replacement (match-string 2 text))
           (flags (match-string 3 text))
           newrepl
           (idx 0) (n (length replacement)))

      ;; handle escaped chars
      (while (< idx n)
        (if (and (= (aref replacement idx) ?\\)
                 (< (1+ idx) n))
            (let ((c (aref replacement (1+ idx))))
              (case c
                (?n (push ?\n newrepl))
                (?t (push ?\t newrepl))
                (?r (push ?\r newrepl))
                ((?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?\\)
                 (push ?\\ newrepl)
                 (push c newrepl))
                (t (push c newrepl)))
              (incf idx 2))
          (push (aref replacement idx) newrepl)
          (incf idx)))

      (values pattern (apply #'string (reverse newrepl)) flags))))

;; Related commands.
(vim:defcmd vim:cmd-nohighlight (nonrepeatable)
  "Disables the active search highlightings."
  (vim:delete-hl 'vim:search)
  (vim:delete-hl 'vim:substitute))


(provide 'vim-search)

;;; vim-search.el ends here
