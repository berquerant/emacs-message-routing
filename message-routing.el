;;; message-routing.el --- Route messages to buffer -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Package-Requires: ((cl-lib "1.0"))
;; Created: 17 Dec 2022
;; Version: 0.2.1
;; Keywords: message
;; URL: https://github.com/berquerant/emacs-message-routing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Please see README.md from the same repository for documentation.

;;; Code:

(require 'cl-lib)

(defgroup message-routing nil
  "Message routing."
  :prefix "message-routing-"
  :group 'message-routing)

(defcustom message-routing-routes nil
  "Routing keys and destinations (alist).
Key is regex, destination is buffer name.
If `format'-ed arguments of `message' matches to key,
insert it into the buffer without inserting `*Messages*' buffer.

e.g.

  (setq message-routing-routes '((\"^tmp-a\" . \"*TmpA*\")
                                 (\"^tmp\" . \"*Tmp*\")))
  (message \"tmp-a:hello\")

then insert \"tmp-a:hello\" into \"*TmpA*\" buffer.

  (message \"tmp:hello\")

then insert \"tmp:hello\" into \"*TmpA*\" and \"*Tmp*\" buffers.

  (message \"hello\")

then insert \"hello\" into \"*Messages*\" buffer."
  :type '(alist :key-type string :value-type string)
  :version "27.1")

(defun message-routing--select-buffer-name-list-with-routes (msg routes)
  (cl-loop for x in routes
           when (string-match-p (car x) msg)
           collect (cdr x)))

(defun message-routing--select-buffer-name-list (msg)
  "Find `buffer-name' appropreate for MSG."
  (message-routing--select-buffer-name-list-with-routes msg message-routing-routes))

(defun message-routing--insert-buffer (buffer-name msg)
  (with-current-buffer (get-buffer-create buffer-name)
    (insert msg)))

(defun message-routing--insert-buffer-list (buffer-name-list msg)
  (cl-loop for x in buffer-name-list
           do (message-routing--insert-buffer x msg)))

(defun message-routing--message-advice-around (orig-func &rest args)
  (let* ((msg (apply #'format args))
         (buffer-name-list (message-routing--select-buffer-name-list msg)))
    (if buffer-name-list (message-routing--insert-buffer-list buffer-name-list (concat msg "\n"))
      (apply orig-func (list msg)))))

(defun message-routing--advice-add ()
  (advice-add #'message :around #'message-routing--message-advice-around))

;;;###autoload
(defun message-routing-setup ()
  "Enable message-routing."
  (message-routing--advice-add))

(provide 'message-routing)
;;; message-routing.el ends here
