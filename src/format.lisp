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

(defun format-message (message)
  (let* ((user-name (user-name (message-user message)))
         (text (message-text message))
         (content (if (find #\Newline text)
                      (format nil "~a: ~%~%~a~%" user-name text)
                      (format nil "~a: ~a" user-name text))))
    (li1 content)))

(defun format-tasks (messages)
  (let ((tasks (partition-tasks messages)))
    (h1 "Tasks")

    (let ((new (getf tasks :new)))
      (h2 "New")

      (unless new (%format-md (make-md 'p "No New Tasks")))

      (dolist (task new)
        (li1 (format nil "[~a](~a)" (cdr task) (car task)))))

    (let ((done (getf tasks :done)))
      (h2 "Done")

      (unless done (p "No Done Tasks"))

      (dolist (task done)
        (li1 (format nil "[~a](~a)" (cdr task) (car task)))))))

(defun format-logs (messages)
  (h1 "Logs")

  (dolist (message (reverse messages))
    (when (equal (message-type message) "message")
      (let ((subtype (message-subtype message)))
        (cond
          ((null subtype)
           (format-message message)))))))


@export
(defun format-messages (messages &optional (stream t))
  (let ((*stream* stream))
    (format-tasks messages)
    (format-logs messages)))
