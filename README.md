# message-routing

Route messages to buffer.

# Usage

``` emacs-lisp
(setq message-routing-routes '(("^tmp" . "*TmpBuf*")))
(message-routing-setup)
(message "tmp:hello") ; insert "tmp:hello" into *TmpBuf* buffer
(message "hello") ; insert "hello" into *Messages* buffer
```
