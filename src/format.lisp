(in-package :cl-user)
(defpackage :slack-nippo.format
  (:use :cl
        :annot.doc
        :slack-nippo.message
        :slack-nippo.markdown
        :slack-nippo.trello)
  (:shadowing-import-from :slack-nippo.message
                          :value))
(in-package :slack-nippo.format)

(syntax:use-syntax :annot)

(defun format-message (message)
  (let ((md (make-md 'p (message-text message))))
    (format-md md t)))

@export
(defun format-messages (messages)
  (dolist (message messages)
    (when (equal (message-type message) "message")
      (let ((subtype (message-subtype message)))
        (cond
          ((null subtype)
           (format-message message)
           (fresh-line)))))))

