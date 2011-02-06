;;; zencoding-trie.el --- A trie data structure built for zencoding-mode
;;
;; Copyright (C) 2009, Rudolf Olah
;;
;; Author: Rudolf Olah <omouse@gmail.com>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;

(defun make-zencoding-trie-node ()
  "Creates a vector of two elements. The first element is some value,
the second element is a character table for storing more branches of the
trie. The value is initially NIL."
  (vector nil (make-char-table 'trie-table)))

(defun zencoding-trie-node-value (node)
  (aref node 0))

(defun zencoding-trie-node-set-value (node value)
  (aset node 0 value))

(defun zencoding-trie-node-branches (node)
  (aref node 1))

(defun zencoding-trie-node-branch (node branch-key)
  (aref (zencoding-trie-node-branches node) branch-key))
(defun zencoding-trie-node-create-branch (node branch-key)
  (aset (zencoding-trie-node-branches node)
        branch-key
        (make-zencoding-trie-node)))

(defun zencoding-trie-node-brancher (node branch-key)
  (let ((branch (aref (zencoding-trie-node-branches node) branch-key)))
    (if branch
        branch
      ;; branch doesn't exist, so create it and then return it.
      (zencoding-trie-node-create-branch node branch-key))))

(defun zencoding-trie-traverse (s trie i f)
  (if (null trie)
      nil
    (let ((branch (funcall f trie (aref s (1- i)))))
      (if (or (null branch) (= i (length s)))
          branch
        (zencoding-trie-traverse s branch (1+ i) f)))))

(defun zencoding-trie-retrieve (s trie)
  (zencoding-trie-traverse s trie 1 'zencoding-trie-node-branch))

(defun zencoding-trie-insert (s value trie)
  "`s' is the string used to navigate the trie (each character is a node
in the trie). `value' is the value we want to insert. `trie' is the node
where we start the insertion."
  (zencoding-trie-node-set-value
   (zencoding-trie-traverse s trie 1 'zencoding-trie-node-brancher)
   value))

(provide 'zencoding-trie)

;; test code
(let ((x (make-zencoding-trie-node)))
  (zencoding-trie-insert "ha" 1 x)
  (zencoding-trie-insert "hi" 2 x)
  (zencoding-trie-retrieve "ha" x))
