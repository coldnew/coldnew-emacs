;; my google-analytics code
(defvar google-analytics-code
  "<script>
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
	 (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
	 m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
	 })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', 'UA-42122243-1', 'auto');
ga('send', 'pageview');
</script>")

;; export init.org to init.html
(require 'org)
(require 'ox-html)
(defun export-init-to-html()
  (setq org-html-postamble-format google-analytics-code)
  (find-file "init.org")
  (org-html-export-to-html))

;; export init.el to init.el.html
(require 'htmlize)
(defun export-init-el-to-html ()
  (add-hook 'htmlize-after-hook
	    '(lambda ()
	       (while (search-forward "</body>" nil t)
		 (replace-match (format "\n%s\n</body>" google-analytics-code) nil t))))
  (setq htmlize-output-type 'inline-css)
  (set-background-color "#202020")
  (set-foreground-color "#c6cccc")
  (htmlize-file "init.el"))

;;;; export all docs
(defun generate-doc-files ()
  (export-init-to-html)
  (export-init-el-to-html))