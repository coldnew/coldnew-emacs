#!/usr/bin/env guile
!#

(use-modules (system foreign))
(use-modules ((ice-9 format)))
;; https://groups.google.com/forum/#!msg/shlug/G3BwUz4pLXo/pICx2GbpddwJ
;; (define-module ()
;;   #:use-module (system foreign)
;;   #:export (doprint))

(define libgcin-im-client (dynamic-link "/usr/lib/hime/libhime-im-client.so"))

(define )
;; (define libtest (dynamic-link "/tmp/libtest.so"))

;; (define do_fib
;;   (pointer->procedure int (dynamic-func "fib" libtest)
;;		      (list int)))

;; (define do_print
;;   (pointer->procedure void (dynamic-func "do_print" libtest)
;;		      '()))

;; (do_print)

;; (format #t "~d" (do_fib 10))



;; (define doprint
;;   (pointer->procedure int (dynamic-func "do_print" libtest)
;;		      '()))

;;http://www.newsmth.net/nForum/#!article/Emacs/99489
