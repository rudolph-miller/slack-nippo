(in-package :cl-user)
(defpackage :slack-nippo.format
  (:use :cl
        :annot.doc
        :slack-nippo.user
        :slack-nippo.message
        :slack-nippo.markdown
        :slack-nippo.trello)
  (:shadow :user)
  (:shadowing-import-from :slack-nippo.message
                          :value))
(in-package :slack-nippo.format)

(syntax:use-syntax :annot)

(defparameter *stream* *standard-output*)

(defun %format-md (md)
  (format-md md *stream*))
  
(defun format-message (message)
  (let* ((user-name (user-name (message-user message)))
         (text (message-text message))
         (content (if (find #\Newline text)
                      (format nil "~a: ~%~%~a~%" user-name text)
                      (format nil "~a: ~a" user-name text)))
         (md (make-md 'li1 content)))
    (%format-md md)))

@export
(defun format-messages (messages)
  (let ((tasks (partition-tasks messages)))
    (%format-md (make-md 'h1 "Tasks"))

    (let ((new (getf tasks :new)))
      (%format-md (make-md 'h2 "New"))

      (unless new (%format-md (make-md 'p "No New Tasks")))

      (dolist (task new)
        (%format-md (make-md 'li1
                            (format nil "[~a](~a)" (cdr task) (car task))))))
      
    (let ((done (getf tasks :done)))
      (%format-md (make-md 'h2 "Done"))

      (unless done (%format-md (make-md 'p "No Done Tasks")))

      (dolist (task done)
        (%format-md (make-md 'li1
                            (format nil "[~a](~a)" (cdr task) (car task)))))))
  
  (%format-md (make-md 'h1 "Log"))
                      
  (dolist (message (reverse messages))
    (when (equal (message-type message) "message")
      (let ((subtype (message-subtype message)))
        (cond
          ((null subtype)
           (format-message message)))))))

