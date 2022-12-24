# message-routing

Route messages to buffer.

# Usage

``` emacs-lisp
(setq message-routing-routes '(("^tmp" . "*Tmp*") ("^tmp-a" . "*TmpA*")))
(message-routing-setup)
(message "tmp-a:hello") ; insert into *Tmp* and *TmpA* buffers
(message "tmp:hello") ; insert into *Tmp* buffer
(message "hello") ; insert into *Messages* buffer
```
