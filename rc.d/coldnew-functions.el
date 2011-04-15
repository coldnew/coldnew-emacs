;;
(eval-when-compile (require 'cl))

;;;;;;;; Packages Import
(require 'coldnew-macro)


(defun show-buffer-major-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string major-mode))

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
	(if (string= "comm" (car attr))
	    (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defun get-current-line ()
  "Current line string"
  (buffer-substring (save-excursion (beginning-of-line) (point))
		    (save-excursion (end-of-line) (point))))

(defun line-length ()
  "Length of a line in number of characters"
  (length (buffer-substring (save-excursion (beginning-of-line) (point))
			    (save-excursion (end-of-line) (point)))))

;;;;;;;; Date and time
(defun day-of-week (year month day)
  "Returns the day of the week as an integer.
   Monday is 1."
  (nth 6 (decode-time (encode-time 0 0 0 day month year))))

(defun day-of-week-in-string (year month day)
  "Return the day of the week as day name."
  (let* ((day-names '("Sunday" "Monday" "Tuesday" "Wednesday"
		      "Thursday" "Friday" "Saturday"))
	 (day-index (nth 6 (decode-time (encode-time 0 0 0 day month year)))))
    (nth day-index day-names)))

;;;;;;;; Convertion
(defun unix->dos (buf)
  "Convert buffer file from unix file to dos file."
  (let* (current-buf (current-buffer))
    (if (not (eq current-buf buf))
	(switch-to-buffer buf))
    (goto-char(point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n")))
  )

(defun dos->unix (buf)
  "Convert buffer file from dos file to unix file."
  (let* (current-buf (current-buffer))
    (if (not (eq current-buf buf))
	(switch-to-buffer buf))
    (goto-char(point-min))
    (while (search-forward "\r" nil t) (replace-match "")))
  )

;;;;;;;; Math
(defun factorial (n)
  "Calculate n!"
  (assert (and (integerp n) (not (minusp n))))
  (cond
   ((or (= n 1) (= n 0))
    1)
   (t
    (* n (factorial (1- n)))))
  )

;;;;;;;; String
(defun string-empty? (str)
  "Return t if string is empty."
  (string= "" str))

;;;;;;;; Terminal
;; TODO: Need to review
(defun run-program-in-terminal (prg &optional use-existing)
  ""
  (let ((buffer (concat "*" prg "*")))
    (when (not (and use-existing
		    (let ((buf (get-buffer buffer)))
		      (and buf (buffer-name (switch-to-buffer buffer)))
		      )))
      (ansi-term prg prg))))

;;;;;; TODO: Need to review
;;;; Enable APIS
;; Perl	http://perldoc.perl.org/search.html?q=XYZ
;; PHP	http://us.php.net/XYZ
;; LSL	http://wiki.secondlife.com/wiki/XYZ
;; AutoHotkey	http://www.autohotkey.com/docs/commands/XYZ.htm
;; Wikipedia	http://en.wikipedia.org/wiki/XYZ
;; Google	http://www.google.com/search?q=XYZ
;; bing	http://www.bing.com/search?q=XYZ
;; Wolfram|Alpha	http://www.wolframalpha.com/input/?i=XYZ
;;
(defun lookup-word-definition (url)
  "Look up the current word's definition in a browser.
If a region is active (a phrase), lookup that phrase."
  (let (myword myurl)
    (setq myword
	  (if (and transient-mark-mode mark-active)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (thing-at-point 'symbol)))

    (setq myword (replace-regexp-in-string " " "%20" myword))
    (setq myurl (concat url myword))

    (browse-url myurl)
    ;; (w3m-browse-url myurl) ;; if you want to browse using w3m
    ))





(provide 'coldnew-functions)
;; coldnew-functions.el ends here.
