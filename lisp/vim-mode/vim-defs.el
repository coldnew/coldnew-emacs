;;; vim-defs.el - Core variables.

;; Copyright (C) 2009, 2010, 2011 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'vim-macs)

(defvar vim:repeat-events nil
  "The sequence of events for the repeat command.")

(vim:deflocalvar vim:current-register nil
  "The register of the current command.")

(vim:deflocalvar vim:current-cmd-count nil
  "The count of the current command.")

(vim:deflocalvar vim:current-cmd nil
  "The node of the current command.")

(vim:deflocalvar vim:current-cmd-arg nil
  "The argument of the current command.")

(vim:deflocalvar vim:current-motion-count nil
  "The count of the current motion.")

(vim:deflocalvar vim:current-motion nil
  "The node of the current motion.")

(vim:deflocalvar vim:current-motion-arg nil
  "The argument of the current motion.")

(vim:deflocalvar vim:current-motion-type nil
  "The type of the current motion (inclusive, exclusive,
  linewise).")

(vim:deflocalvar vim:current-force-motion-type nil
  "The forced type of the motion of current command (inclusive,
  exclusive, linewise).")

(vim:deflocalvar vim:current-key-sequence nil
  "The key-sequence of the current command.")


(provide 'vim-defs)

;;; vim-defs.el ends here
