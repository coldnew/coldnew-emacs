;;; coldnew-variables.el ---
(eval-when-compile (require 'cl))


;;;; ---------------------------------------------------------------------------
;;;; Variables setting
;;;; ---------------------------------------------------------------------------

;; Operation system testing
(defvar mac-p     (eq system-type 'darwin)
  "Return nil if OS is not Mac.")
(defvar linux-p   (and (eq system-type 'gnu/linux) (not (eq system-type 'drawin)))
  "Return nil if OS is not Linux.")
(defvar cygwin-p  (eq system-type 'cygwin)
  "Return nil if OS is not CygWin.")
(defvar windows-p (eq system-type 'windows-nt)
  "Return nil if OS is not Windows.")

(defvar linux-64bit-p (and (string-match (rx bos "x86_64") system-configuration) linux-p)
  "Return nil if OS is not 64-bit linux.")

(defvar linux-32bit-p (and (string-match (rx bos "x86-") system-configuration) linux-p)
  "Return nil if OS is not 32-bit linux.")

;; User testing
(defvar root-p (zerop (user-real-uid))
  "Return nil if user is not root user.")

;; resolution testing
(defvar display-1280x800-p   (and (= (display-pixel-width) 1280) (= (display-pixel-height) 800))
  "Return nil if current display's resolution is not 1280x800")

(defvar display-1280x1024-p  (and (= (display-pixel-width) 1280) (= (display-pixel-height) 1024))
  "Return nil if current display's resolution is not 1280x1024")

(defvar display-1920x1080-p  (and (= (display-pixel-width) 1920) (= (display-pixel-height) 1080))
  "Return nil if current display's resolution is not 1920x1080")


(provide 'coldnew-variables)
;; coldnew-variables.el ends here.
