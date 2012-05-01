;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (customize-object) "eieio-custom" "eieio-custom.el"
;;;;;;  (20381 41620))
;;; Generated autoloads from eieio-custom.el

(autoload 'customize-object "eieio-custom" "\
Customize OBJ in a custom buffer.
Optional argument GROUP is the sub-group of slots to display.

\(fn OBJ &optional GROUP)" nil nil)

;;;***

;;;### (autoloads (eieio-describe-generic eieio-describe-constructor
;;;;;;  eieio-describe-class eieio-browse) "eieio-opt" "eieio-opt.el"
;;;;;;  (20381 41620))
;;; Generated autoloads from eieio-opt.el

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that object.
Optional HEADERFCN should be called to insert a few bits of info first.

\(fn CLASS &optional HEADERFCN)" t nil)

(autoload 'eieio-describe-constructor "eieio-opt" "\
Describe the constructor function FCN.
Uses `eieio-describe-class' to describe the class being constructed.

\(fn FCN)" t nil)

(defalias 'describe-generic 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

;;;***

;;;### (autoloads nil "eieio-xml" "eieio-xml.el" (20381 41620))
;;; Generated autoloads from eieio-xml.el

(defmethod object-write-xml ((this eieio-default-superclass) &optional comment) "\
Write object THIS out to the current stream as XML.
  If optional COMMENT is non-nil, include comments when outputting
this object.
@todo - support arbitrary schema output" (when comment (princ "<!-- Object ") (princ (object-name-string this)) (princ " -->
<!-- ") (princ comment) (princ " -->
")) (let* ((cl (object-class this)) (cv (class-v cl))) (princ (make-string (* eieio-print-depth 2) 32)) (princ "<object>
") (let ((eieio-print-depth (+ eieio-print-depth 1))) (princ (make-string (* eieio-print-depth 2) 32)) (princ "<name>") (princ (object-name-string this)) (princ "</name>
") (princ (make-string (* eieio-print-depth 2) 32)) (princ "<class>") (princ (symbol-name (class-constructor (object-class this)))) (princ "</class>
") (let ((publa (aref cv class-public-a)) (publd (aref cv class-public-d))) (while publa (when (slot-boundp this (car publa)) (let ((i (class-slot-initarg cl (car publa))) (v (eieio-oref this (car publa)))) (unless (or (not i) (equal v (car publd))) (princ (make-string (* eieio-print-depth 2) 32)) (princ "<slot>
") (princ (make-string (+ (* eieio-print-depth 2) 2) 32)) (princ "<name>") (princ (symbol-name i)) (princ "</name>
") (princ (make-string (+ (* eieio-print-depth 2) 2) 32)) (princ "<value>") (let ((eieio-print-depth (+ eieio-print-depth 2)) (o (eieio-oref this (car publa)))) (eieio-xml-override-prin1 o)) (princ "</value>
") (princ (make-string (* eieio-print-depth 2) 32)) (princ "</slot>
")))) (setq publa (cdr publa) publd (cdr publd))))) (princ (make-string (* eieio-print-depth 2) 32)) (princ "</object>
")))

;;;***

;;;### (autoloads (enable-visual-studio-bookmarks) "linemark" "linemark.el"
;;;;;;  (20381 41620))
;;; Generated autoloads from linemark.el

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("chart.el" "eieio-base.el" "eieio-datadebug.el"
;;;;;;  "eieio-doc.el" "eieio-speedbar.el" "eieio.el") (20381 41622
;;;;;;  52771))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
