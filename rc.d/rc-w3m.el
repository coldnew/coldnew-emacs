;;;
(provide 'rc-w3m)

(when (require 'w3m nil 'noerror)
  (require 'w3m-lnum nil 'noerror)
  (setq browse-url-browser-function 'w3m-browse-url) ; Use w3m as default web-browser
  (setq w3m-view-this-url-new-session-in-background t)
  (setq w3m-tab-width 8)
  (setq w3m-use-cookies t)
  (setq w3m-use-toolbar nil)
  (setq w3m-default-display-inline-image t)
  (setq w3m-default-toggle-inline-images t)
  (setq w3m-use-mule-ucs t)
  ;;  (add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
  (setq w3m-coding-system 'utf-8
	w3m-file-coding-system 'utf-8
	w3m-file-name-coding-system 'utf-8
	w3m-input-coding-system 'utf-8
	w3m-output-coding-system 'utf-8
	w3m-terminal-coding-system 'utf-8)
  (setq w3m-search-default-engine "google-tw")
  (eval-after-load "w3m-search"
    '(progn
       (add-to-list 'w3m-search-engine-alist
		    '("google-tw" "http://www.google.com.tw/search?q=%s&ie=utf-8&oe=utf-8" nil))
       (add-to-list 'w3m-search-engine-alist
		    '("zh.wikipedia" "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
       ;;     (add-to-list 'w3m-uri-replace-alist
       ;;		  '("\\`my:" w3m-search-uri-replace "My engine"))
       ))

  ;; (add-hook 'w3m-mode-hook
  ;; 	    '(lambda ()
  ;; 	       '(color-theme-xp))
  ;; 	    ))

  (add-hook 'w3m-mode-hook
	    '(lambda ()
	       ;;     (setq (make-local-variable background-color) "white")

	       ))

  ;; (cond
  ;;  ( (mac-p ))
  ;;  (  (linux-p )
  ;;     ))

  ;; Rename w3m buffer
  (add-hook 'w3m-display-hook
	    (lambda (url)
	      (rename-buffer
	       (format "*w3m: %s*" (or w3m-current-title
				       w3m-current-url)) t)))

;;;; Functions

  (defun my-w3m-go-to-linknum ()
    "Turn on link numbers and ask for one to go to."
    (interactive)
    (let ((active w3m-link-numbering-mode))
      (when (not active) (w3m-link-numbering-mode))
      (unwind-protect
	  (w3m-move-numbered-anchor (read-number "Anchor number: "))
	(when (not active) (w3m-link-numbering-mode)))))

  ;;;;
  (defun w3m-download-with-curl (loc)
    (define-key w3m-mode-map "c"
      (lambda (dir)
	(interactive "Save to: ")
	(cd dir)
	(start-process "curl" "*curl*" "curl" "-O" "-s" (w3m-anchor)))))


  ;;;;
  (defun w3m-copy-url-at-point ()
    (interactive)
    (let ((url (w3m-anchor)))
      (if (w3m-url-valid url)
	  (kill-new (w3m-anchor))
	(message "No URL at point!"))))


  ;; Look up Google technology news.
  (defun w3m-news-google-technology ()
    "Look up Google technology news."
    (interactive)
    (w3m-goto-url-new-session "http://news.google.cn/nwshp?tab=wn&ned=ccn&topic=t")
    )

  ;; Google desktop with in Emacs.
  ;; the search url is different with personal, so have to modify it.
  (defun w3m-search-google-desktop ()
    "Use Google Desktop search for WHAT."
    (interactive)
    (w3m-goto-url-new-session "http://127.0.0.1:39183/?hl=zh_CN&s=j_P1JWE_tLZqLNLCShE7FJ8X80E")
    )


  ;; Gmail
  (defun w3m-auto-logon-gmail ()
    "Auto logon gmail"
    (interactive)
    (w3m-goto-url-new-session "http://mail.google.com/mail/h/h4339zs3i3b6/?zy=n&f=1")
    )

  ;; Search translate result from Dict.com
  (defun w3m-search-dict-cn (what)
    "Use Google Code search for WHAT."
    (interactive "sDict: ")
    (w3m-goto-url-new-session (concat "http://dict.cn/search/?q="
				      (w3m-url-encode-string what 'gbk))))

  ;; Google code search
  (defun w3m-search-google-code (what)
    "Use Google Code search for WHAT."
    (interactive "sGoogle Code: ")
    (w3m-goto-url-new-session (concat "http://www.google.com/codesearch?hl=zh-CN&lr=&q="
				      (w3m-url-encode-string what))))

  ;; Google Lucky
  (defun w3m-search-google-lucky (what)
    "Use Google Lucky search for WHAT."
    (interactive "sGoogle Lucky: ")
    (w3m-goto-url-new-session (concat "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q="
				      (w3m-url-encode-string what))))

  ;; Google image search.
  (defun w3m-search-google-image (what)
    "Use Google image search for WHAT."
    (interactive "sGoogle Image: ")
    (w3m-goto-url-new-session (concat "http://images.google.com/images?sa=N&tab=wi&q="
				      (w3m-url-encode-string what))))

  ;; Google news search
  (defun w3m-search-google-news (what)
    "Use Google news search for WHAT."
    (interactive "sGoogle News: ")
    (w3m-goto-url-new-session (concat "http://news.google.cn/news?hl=zh-CN&ie=GB2312&ned=ccn&q="
				      (w3m-url-encode-string what 'gbk))))

  ;; Google blog search
  (defun w3m-search-google-blog (what)
    "Use Google blog search for WHAT."
    (interactive "sGoogle Blog: ")
    (w3m-goto-url-new-session (concat
			       "http://blogsearch.google.com/blogsearch?hl=zh-CN&ie=UTF-8&oe=UTF-8&q="
			       (w3m-url-encode-string what 'utf-8))))

  ;; Google group search
  (defun w3m-search-google-group (what)
    "Use Google group search for WHAT."
    (interactive "sGoogle Group: ")
    (w3m-goto-url-new-session (concat "http://groups.google.com/groups?hl=zh-CN&ie=UTF-8&oe=UTF-8&q="
				      (w3m-url-encode-string what 'utf-8))))


  ;; Search file (like pdf, chm) use Google Search syntax
  ;; But now just type name.
  ;; Example, if you want search pdf or chm about Emacs, just type emacs pdf|chm
  (defun w3m-search-google-file (file)
    "Use google to search for a file named FILE."
    (interactive "sGoogle Search for file: ")
    (w3m-goto-url-new-session
     (concat "http://www.google.cn/search?q="
	     (w3m-url-encode-string
	      (concat "+intitle:(\"index of\"\|\"last modified\"\|\"parent of\") -inurl:htm -inurl:html -inurl:php -inurl:asp "
		      file) 'utf-8))))

  ;; Search mp3 and download it from mp3.baidu.com
  (defun w3m-search-baidu-mp3 (what)
    "Use emacs wiki to search for WHAT."
    (interactive "sBai Du Mp3 Search: ")
    (w3m-goto-url-new-session (concat
			       "http://mp3.baidu.com/m?f=ms&tn=baidump3&ct=134217728&lf=&rn=&lm=0&word="
			       (w3m-url-encode-string what 'gbk))))

  ;; Search in EmacsWiki
  (defun w3m-search-emacs-wiki (what)
    "Use emacs wiki to search for WHAT."
    (interactive "sEmacs Wiki Search: ")
    (w3m-goto-url-new-session (concat "http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q="
				      (w3m-url-encode-string what))))

  ;; Google search in page that write in Chines.
  (defun w3m-search-google-chinese (what)
    "Use google to search for WHAT that write in chinese."
    (interactive "sGoogle Search (chinese): ")
    (w3m-goto-url-new-session (concat "http://www.google.cn/search?&hl=zh-CN&lr=lang_zh-CN%7Clang_zh-TW&inlang=zh-CN&q="
				      (w3m-url-encode-string what))))

  ;; Google CN Search.
  (defun w3m-search-google-cn (what)
    "Use google to search for WHAT."
    (interactive "sGoogle Search (CN): ")
    (w3m-goto-url-new-session (concat "http://www.google.cn/search?q="
				      (w3m-url-encode-string what))))

  ;; Google english search.
  (defun w3m-search-google-en (what)
    "Use google to search for WHAT."
    (interactive "sGoogle Search (EN): ")
    (w3m-goto-url-new-session (concat "http://www.google.com/search?q="
				      (w3m-url-encode-string what))))


  )
