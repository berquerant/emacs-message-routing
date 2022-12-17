# message-routing

Route messages to buffer.

# Usage

``` emacs-lisp
(setq message-routing-routes '(("^tmp" . "*Tmp*") ("^tmp-a" . "*TmpA*")))
(message-routing-setup)
(message "tmp-a:hello") ; insert into *TmpA* buffer
(message "tmp:hello") ; insert *Tmp* and *TmpA* buffers
(message "hello") ; insert into *Messages* buffer
```
