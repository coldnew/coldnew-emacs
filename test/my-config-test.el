;;; my-config-test.el --- Comprehensive tests for my/ functions -*- lexical-binding: t -*-

;; Copyright (C) 2010-2026 Yen-Chin, Lee <coldnew.tw@gmail.com>

;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                    Version 2, December 2024
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.

;;; Commentary:

;; Comprehensive unit tests for all my/ prefixed functions in init.el
;; Run with: make test or M-x ert

;;; Code:

(require 'ert)

;; ============================================================================
;; Section: Buffer/Text Functions
;; ============================================================================

;; --- my/untabify-buffer ---

(ert-deftest my/untabify-buffer-removes-tabs ()
  "Test that my/untabify-buffer removes tabs from buffer."
  (with-temp-buffer
    (insert "a\tb\tc")
    (my/untabify-buffer)
    (should-not (string-match-p "\t" (buffer-string)))))

(ert-deftest my/untabify-buffer-preserves-content ()
  "Test that my/untabify-buffer preserves text content."
  (with-temp-buffer
    (insert "hello\tworld")
    (my/untabify-buffer)
    (should (string-match-p "hello" (buffer-string)))
    (should (string-match-p "world" (buffer-string)))))

(ert-deftest my/untabify-buffer-no-op-on-clean-buffer ()
  "Test that my/untabify-buffer handles already clean buffer."
  (with-temp-buffer
    (insert "hello world")
    (my/untabify-buffer)
    (should (string= "hello world" (buffer-string)))))

;; --- my/indent-whole-buffer ---

(ert-deftest my/indent-whole-buffer-runs-without-error ()
  "Test that my/indent-whole-buffer runs without error."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test ()\n(+ 1 1))")
    (my/indent-whole-buffer)
    (should t)))

;; --- my/cleanup-buffer ---

(ert-deftest my/cleanup-buffer-removes-trailing-whitespace ()
  "Test that my/cleanup-buffer removes trailing whitespace."
  (with-temp-buffer
    (insert "hello world   \ntest  ")
    (my/cleanup-buffer)
    (should-not (string-match-p "world   $" (buffer-string)))
    (should-not (string-match-p "test  $" (buffer-string)))))

(ert-deftest my/cleanup-buffer-untabs-buffer ()
  "Test that my/cleanup-buffer untabs buffer."
  (with-temp-buffer
    (insert "hello\tworld")
    (my/cleanup-buffer)
    (should-not (string-match-p "\t" (buffer-string)))))

;; --- my/insert-U200B-char ---

(ert-deftest my/insert-U200B-char-inserts-zwsp ()
  "Test that my/insert-U200B-char inserts zero-width space."
  (with-temp-buffer
    (my/insert-U200B-char)
    (should (string= (buffer-string) "\ufeff"))))

(ert-deftest my/insert-U200B-char-appends ()
  "Test that my/insert-U200B-char appends to existing content."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (my/insert-U200B-char)
    (should (string= (buffer-string) "hello\ufeff"))))

;; --- my/insert-empty-line ---

(ert-deftest my/insert-empty-line-adds-line-after ()
  "Test that my/insert-empty-line adds line after current."
  (with-temp-buffer
    (insert "line1")
    (my/insert-empty-line)
    (should (string= (buffer-string) "line1\n"))))

;; --- my/insert-lorem ---

(ert-deftest my/insert-lorem-inserts-lorem-text ()
  "Test that my/insert-lorem inserts lorem ipsum text."
  (with-temp-buffer
    (my/insert-lorem)
    (should (string-match-p "Lorem ipsum" (buffer-string)))
    (should (string-match-p "consectetur adipisicing" (buffer-string)))))

;; --- my/delete-word ---

(ert-deftest my/delete-word-deletes-forward ()
  "Test that my/delete-word deletes word forward."
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (my/delete-word 1)
    (should (string= (buffer-string) " world"))))

(ert-deftest my/delete-word-respects-argument ()
  "Test that my/delete-word respects argument."
  (with-temp-buffer
    (insert "one two three")
    (goto-char 1)
    (my/delete-word 2)
    (should (string= (buffer-string) " three"))))

(ert-deftest my/delete-word-at-end ()
  "Test that my/delete-word handles end of buffer."
  (with-temp-buffer
    (insert "hello")
    (goto-char (point-max))
    (my/delete-word 1)
    (should t)))

;; --- my/file-info ---

(ert-deftest my/file-info-returns-nil-for-temp-buffer ()
  "Test that my/file-info returns nil for temp buffer."
  (with-temp-buffer
    (should-not (my/file-info))))

;; --- my/what-face ---

(ert-deftest my/what-face-returns-string ()
  "Test that my/what-face returns a string message."
  (with-temp-buffer
    (insert "plain text")
    (goto-char 1)
    (let ((result (my/what-face (point))))
      (should (stringp result)))))

(ert-deftest my/what-face-message-contains-face-or-position ()
  "Test that my/what-face produces message about face or position."
  (with-temp-buffer
    (insert "plain text")
    (goto-char 1)
    (let ((result (my/what-face (point))))
      (should (or (string-match-p "Face:" result)
                  (string-match-p "No face at" result))))))

(ert-deftest my/what-face-on-comments ()
  "Test my/what-face on comment text."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "; comment")
    (goto-char 2)
    (let ((result (my/what-face (point))))
      (should (stringp result))
      (should (> (length result) 0)))))

;; ============================================================================
;; Section: LLM Provider Functions
;; ============================================================================

(ert-deftest my/llm-get-provider-is-function ()
  "Test that my/llm-get-provider is a function."
  (should (fboundp 'my/llm-get-provider)))

(ert-deftest my/llm-get-provider-unknown-returns-nil ()
  "Test that my/llm-get-provider returns nil for unknown provider."
  (should-not (my/llm-get-provider 'nonexistent)))

;; ============================================================================
;; Section: File Operations
;; ============================================================================

;; --- my/set-file-executable ---

(ert-deftest my/set-file-executable-makes-file-executable ()
  "Test that my/set-file-executable adds execute bit to file."
  (let ((temp-file (make-temp-file "test-exec-" nil nil "echo test")))
    (find-file temp-file)
    (unwind-protect
        (progn
          (my/set-file-executable)
          (should (= #o100 (logand (file-modes temp-file) #o100))))
      (kill-buffer)
      (delete-file temp-file))))

;; ============================================================================
;; Section: Font Functions
;; ============================================================================

(ert-deftest my/font-exist-p-nil-for-nonexistent ()
  "Test that my/font-exist-p returns nil for nonexistent font."
  (should-not (my/font-exist-p "DefinitelyNotAFont12345")))

(ert-deftest my/font-exist-p-nil-for-nil ()
  "Test that my/font-exist-p handles nil input."
  (should-not (my/font-exist-p nil)))

(ert-deftest my/font-exist-p-nil-for-empty ()
  "Test that my/font-exist-p handles empty string."
  (should-not (my/font-exist-p "")))

;; --- my/font-size functions ---

(ert-deftest my/emacs-step-font-size-is-function ()
  "Test that my/emacs-step-font-size is defined."
  (should (fboundp 'my/emacs-step-font-size)))

(ert-deftest my/increase-emacs-font-size-is-function ()
  "Test that my/increase-emacs-font-size is defined."
  (should (fboundp 'my/increase-emacs-font-size)))

(ert-deftest my/decrease-emacs-font-size-is-function ()
  "Test that my/decrease-emacs-font-size is defined."
  (should (fboundp 'my/decrease-emacs-font-size)))

;; ============================================================================
;; Section: Font-lock Annotation Functions
;; ============================================================================

(ert-deftest my/font-lock-comment-annotations-is-function ()
  "Test that my/font-lock-comment-annotations is a function."
  (should (fboundp 'my/font-lock-comment-annotations)))

(ert-deftest my/font-lock-comment-annotations-runs-without-error ()
  "Test that my/font-lock-comment-annotations runs without error."
  (with-temp-buffer
    (emacs-lisp-mode)
    (my/font-lock-comment-annotations)
    (should t)))

;; ============================================================================
;; Section: CC Mode Functions
;; ============================================================================

(ert-deftest my/cc-mode/highlight-if-0-is-function ()
  "Test that my/cc-mode/highlight-if-0 is defined."
  (should (fboundp 'my/cc-mode/highlight-if-0)))

(ert-deftest my/cc-mode/highlight-if-0-hook-is-function ()
  "Test that my/cc-mode/highlight-if-0-hook is defined."
  (should (fboundp 'my/cc-mode/highlight-if-0-hook)))

(ert-deftest my/c-kill-defun-is-function ()
  "Test that my/c-kill-defun is defined."
  (should (fboundp 'my/c-kill-defun)))

;; ============================================================================
;; Section: ERT Test Helper Functions
;; ============================================================================

(ert-deftest my/ert-run-tests-for-current-file-is-function ()
  "Test that my/ert-run-tests-for-current-file is defined."
  (should (fboundp 'my/ert-run-tests-for-current-file)))

(ert-deftest my/ert-run-all-tests-is-function ()
  "Test that my/ert-run-all-tests is defined."
  (should (fboundp 'my/ert-run-all-tests)))

;; ============================================================================
;; Section: Utility Functions
;; ============================================================================

(ert-deftest my/quick-folding-source-is-function ()
  "Test that my/quick-folding-source is defined."
  (should (fboundp 'my/quick-folding-source)))

(ert-deftest my/save-buffer-always-is-function ()
  "Test that my/save-buffer-always is defined."
  (should (fboundp 'my/save-buffer-always)))

(ert-deftest my/minibuffer-keyboard-quit-is-function ()
  "Test that my/minibuffer-keyboard-quit is defined."
  (should (fboundp 'my/minibuffer-keyboard-quit)))

(ert-deftest my/set-mark-mode/rectangle-mark-mode-is-function ()
  "Test that my/set-mark-mode/rectangle-mark-mode is defined."
  (should (fboundp 'my/set-mark-mode/rectangle-mark-mode)))

(ert-deftest my/copy-and-comment-is-function ()
  "Test that my/copy-and-comment is defined."
  (should (fboundp 'my/copy-and-comment)))

(ert-deftest my/file-reopen-as-root-is-function ()
  "Test that my/file-reopen-as-root is defined."
  (should (fboundp 'my/file-reopen-as-root)))

(ert-deftest my/delete-current-buffer-file-is-function ()
  "Test that my/delete-current-buffer-file is defined."
  (should (fboundp 'my/delete-current-buffer-file)))

(ert-deftest my/rename-current-buffer-file-is-function ()
  "Test that my/rename-current-buffer-file is defined."
  (should (fboundp 'my/rename-current-buffer-file)))

(ert-deftest my/clone-file-and-open-is-function ()
  "Test that my/clone-file-and-open is defined."
  (should (fboundp 'my/clone-file-and-open)))

(ert-deftest my/eval-buffer-until-error-is-function ()
  "Test that my/eval-buffer-until-error is defined."
  (should (fboundp 'my/eval-buffer-until-error)))

(ert-deftest my/reload-init-is-function ()
  "Test that my/reload-init is defined."
  (should (fboundp 'my/reload-init)))

(ert-deftest my/other-window-or-split-is-function ()
  "Test that my/other-window-or-split is defined."
  (should (fboundp 'my/other-window-or-split)))

(ert-deftest my/swap-window-positions-is-function ()
  "Test that my/swap-window-positions is defined."
  (should (fboundp 'my/swap-window-positions)))

(ert-deftest my/nuke-all-buffers-is-function ()
  "Test that my/nuke-all-buffers is defined."
  (should (fboundp 'my/nuke-all-buffers)))

;; ============================================================================
;; Run Tests in Batch Mode
;; ============================================================================

(when noninteractive
  (ert-run-tests-batch-and-exit))

;;; my-config-test.el ends here
