;;; message-routing.el --- Route messages to buffer -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Package-Requires: ((cl-lib "1.0"))
;; Created: 17 Dec 2022
;; Version: 0.1.0
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

  (setq message-routing-routes '((\"^tmp\" . \"*TmpBuf*\")))
  (message \"tmp:hello\")

then insert \"tmp:hello\" into \"*TmpBuf*\" buffer.

  (message \"hello\")

then insert \"hello\" into \"*Messages*\" buffer."
  :type '(alist :key-type string :value-type string)
  :version "30.0.50")

(defun message-routing--select-buffer-name (msg)
  "Find `buffer-name' appropreate for MSG."
  (cl-loop for x in message-routing-routes
           when (string-match-p (car x) msg)
           return (cdr x)))

(defun message-routing--insert-buffer (buffer-name msg)
  (with-current-buffer (get-buffer-create buffer-name)
    (insert msg)))

(defun message-routing--message-advice-override (orig-func &rest args)
  (let* ((msg (apply #'format args))
         (buffer-name (message-routing--select-buffer-name msg)))
    (if buffer-name (message-routing--insert-buffer buffer-name (concat msg "\n"))
      (apply orig-func (list msg)))))

(defun message-routing--advice-add ()
  (advice-add #'message :around #'message-routing--message-advice-override))

(defun message-routing--advice-remove ()
  (advice-remove #'message #'message-routing--message-advice-override))

;;;###autoload
(defun message-routing-setup ()
  "Enable message-routing."
  (message-routing--advice-add))

(provide 'message-routing)
;;; message-routing.el ends here
