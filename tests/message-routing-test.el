;;; message-routing-test.el --- Unittest -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'message-routing)

(defmacro test-message-routing--select-buffer-name-list-with-routes
    (name want msg routes)
  (let ((test-name (format "test-message-routing--select-buffer-name-list-with-routes-%s" name)))
    `(progn
       (ert-deftest ,(read test-name)
           ()
         (should (equal ,want
                        (message-routing--select-buffer-name-list-with-routes
                         ,msg
                         ,routes)))))))

(test-message-routing--select-buffer-name-list-with-routes "nil-routes" nil "message" nil)
(test-message-routing--select-buffer-name-list-with-routes "nil-message" nil "" '(("^z" . "*Z*")))
(test-message-routing--select-buffer-name-list-with-routes "matched-1" '("*Z*") "zero" '(("^z" . "*Z*")))
(test-message-routing--select-buffer-name-list-with-routes "not-matched" nil "zero" '(("^a" . "*A*")))
(test-message-routing--select-buffer-name-list-with-routes "matched-2"
                                                           '("*A*" "*AA*")
                                                           "aart" '(("^a" . "*A*")
                                                                    ("^aa" . "*AA*")))
(test-message-routing--select-buffer-name-list-with-routes "matched-2-ignore-1"
                                                           '("*A*" "*AA*")
                                                           "aart" '(("^a" . "*A*")
                                                                    ("^z" . "*Z*")
                                                                    ("^aa" . "*AA*")))

(provide 'message-routing-test)
;;; message-routing-test.el ends here
