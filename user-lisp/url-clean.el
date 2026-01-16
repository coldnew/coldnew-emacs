;;; url-clean.el --- Clean tracking parameters from URLs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Yen-Chin, Lee <coldnew.tw@gmail.com>

;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; Keywords: url, tracking, clean, privacy
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; url-clean provides utilities to remove tracking parameters from URLs.
;;
;; Supported tracking parameters:
;; - Google Analytics: utm_source, utm_medium, utm_campaign, utm_term, utm_content
;; - Facebook: fbclid
;; - Google Ads: gclid, gclsrc, dclid, msclkid
;; - Mailchimp: mc_eid, mc_cid
;; - HubSpot: _hsenc, _hsmi, hsCtaTracking
;; - Yandex: yclid
;; - Others: ref, ref_, ref_src, ref_url, _openstat, wickedid, ttclid, irclickid
;;
;; Usage:
;;   (require 'url-clean)
;;   (url-clean "https://example.com?utm_source=telegram") ; => "https://example.com"
;;
;;   M-x url-clean-at-point     ; Clean URL at cursor
;;   M-x url-clean-region       ; Clean all URLs in region
;;   M-x url-clean-from-kill-ring ; Clean URL from kill ring

;;; Code:

(require 'url-parse)
(require 'url-util)

(defvar url-clean-parameters
  '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content"
    "fbclid" "gclid" "gclsrc" "dclid" "msclkid"
    "ref" "ref_" "ref_src" "ref_url"
    "mc_eid" "mc_cid"
    "_hsenc" "_hsmi" "hsCtaTracking"
    "yclid" "_openstat"
    "wickedid" "ttclid" "irclickid")
  "URL parameters to remove for tracking cleanup.")

(defun url-clean (url)
  "Remove tracking parameters from URL.
Returns cleaned URL string, or nil if URL is invalid."
  (let ((parsed (url-generic-parse-url url)))
    (when parsed
      (let* ((query (url-query parsed))
             (new-query (delq nil
                              (mapcar (lambda (pair)
                                        (unless (member (car pair) url-clean-parameters)
                                          pair))
                                      query))))
        (setf (url-query parsed) new-query)
        (url-recreate-url parsed)))))

(defun url-clean-text (text)
  "Clean all URLs in TEXT and return cleaned string."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward url-nonrelative-link nil t)
      (let ((url (match-string 0)))
        (let ((cleaned (url-clean url)))
          (when cleaned
            (replace-match cleaned)))))
    (buffer-string)))

;;;###autoload
(defun url-clean-at-point ()
  "Clean URL at point and copy to kill ring."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (when url
      (let ((cleaned (url-clean url)))
        (when cleaned
          (kill-new cleaned)
          (message "Cleaned URL: %s" cleaned))))))

;;;###autoload
(defun url-clean-region (beg end)
  "Clean all URLs in region."
  (interactive "r")
  (let ((text (buffer-substring beg end))
        (count 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward url-nonrelative-link nil t)
        (let ((url (match-string 0)))
          (let ((cleaned (url-clean url)))
            (when cleaned
              (replace-match cleaned)
              (setq count (1+ count))))))
      (delete-region beg end)
      (insert (buffer-string)))
    (message "Cleaned %d URL(s)" count)))

;;;###autoload
(defun url-clean-from-kill-ring ()
  "Clean URL from kill ring and save cleaned version."
  (interactive)
  (let ((url (current-kill 0)))
    (when (string-match-p "\\`https?://" url)
      (let ((cleaned (url-clean url)))
        (when cleaned
          (kill-new cleaned)
          (message "Cleaned and saved: %s" cleaned))))))

(provide 'url-clean)
;;; url-clean.el ends here
