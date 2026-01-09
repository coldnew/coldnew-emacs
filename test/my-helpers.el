;;; my-helpers.el --- Helper functions for testing -*- lexical-binding: t -*-

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

;; Common helper functions and macros for testing.

;;; Code:

;; Helper: Assert with custom message
(defmacro my/assert (form &optional msg)
  "Assert FORM is true, with optional MSG."
  (let ((msg (or msg "Assertion failed")))
    `(unless ,form (error "%s: %s" ,msg (prin1-to-string ',form)))))

;; Helper: Assert equal with message
(defmacro my/assert-equal (expected actual &optional msg)
  "Assert EXPECTED equals ACTUAL."
  (let ((msg (or msg "Values not equal")))
    `(let ((e ,expected) (a ,actual))
       (unless (equal e a)
         (error "%s: expected %s, got %s" ,msg e a)))))

;; Helper: Capture output from function
(defun my/capture-output (func &rest args)
  "Capture stdout from FUNC called with ARGS."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (apply func args)
      (buffer-string))))

;; Helper: Create temporary buffer with content
(defmacro my/with-temp-buffer-content (content &rest body)
  "Create temp buffer with CONTENT and evaluate BODY."
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;; Helper: Simulate file with content
(defmacro my/with-temp-file (content &rest body)
  "Create temp file with CONTENT and evaluate BODY."
  (let ((file (gensym "temp-file-")))
    `(let ((,file (make-temp-file "emacs-test-" nil ".el")))
       (unwind-protect
           (progn
             (write-region ,content nil ,file)
             (with-temp-buffer
               (insert-file-contents ,file)
               (goto-char (point-min))
               ,@body))
         (delete-file ,file)))))

;; Helper: Mock a function temporarily
(defmacro my/with-mocked (func return-value &rest body)
  "Mock FUNC to return RETURN-VALUE while evaluating BODY."
  (let ((orig-func (gensym "orig-func-")))
    `(let ((,orig-func (symbol-function ',func)))
       (unwind-protect
           (progn
             (fset ',func (lambda (&rest _) ,return-value))
             ,@body)
         (fset ',func ,orig-func)))))

;; Helper: Time execution
(defun my/time-it (func &rest args)
  "Time execution of FUNC with ARGS. Returns (result . duration)."
  (let ((start (current-time)))
    (let ((result (apply func args)))
      (cons result (float-time (time-subtract (current-time) start))))))

;; Helper: Skip if feature not available
(defmacro my/skip-unless-feature (feature)
  "Skip test if FEATURE is not available."
  `(unless (featurep ,feature)
     (skip-unless (featurep ,feature))))

;; Helper: Repeat test multiple times
(defmacro my/repeat-test (n &rest body)
  "Evaluate BODY N times for stress testing."
  (let ((i (gensym "i-")))
    `(let ((,n ,n))
        (dotimes (,i ,n)
          ,@body))))

(provide 'my-helpers)

;;; my-helpers.el ends here
