(in-package :cl-user)
(defpackage :slack-nippo.format
  (:use :cl
        :annot.doc
        :slack-nippo.user
        :slack-nippo.message
        :slack-nippo.markdown
        :slack-nippo.trello)
  (:shadow :user
           :id
           :task)
  (:shadowing-import-from :slack-nippo.message
                          :value))
(in-package :slack-nippo.format)

(syntax:use-syntax :annot)

(defun format-task-events (messages)
  (let* ((tasks (extract-tasks messages))
         (all (getf tasks :all))
         (new (getf tasks :new))
         (done (getf tasks :done)))
    (h1 "Tasks")

    (h2 "All")
    (unless all (p "No Tasks"))
    (dolist (task all)
      (li1
       (if (find-task task done)
           (format nil "~~~~[~a](~a)~~~~" (task-url task) (task-title task))
           (format nil "[~a](~a)" (task-url task) (task-title task)))))

    (h2 "New")
    (unless new (p "No New Tasks"))
    (dolist (task new)
      (li1 (format nil "[~a](~a)" (task-url task) (task-title task))))

    (h2 "Done")
    (unless done (p "No Done Tasks"))
    (dolist (task done)
      (li1 (format nil "[~a](~a)" (task-url task) (task-title task))))))

(defun format-log (message)
  (let* ((user-name (user-name (message-user message)))
         (text (message-text message))
         (content (if (find #\Newline text)
                      (format nil "~a: ~%~%~a~%" user-name text)
                      (format nil "~a: ~a" user-name text))))
    (li1 content)))

(defun format-logs (messages)
  (h1 "Logs")

  (dolist (message (reverse messages))
    (when (equal (message-type message) "message")
      (let ((subtype (message-subtype message)))
        (cond
          ((null subtype)
           (format-log message)))))))


@export
(defun format-messages (messages &optional (stream t))
  (let ((*stream* stream))
    (format-task-events messages)
    (format-logs messages)))
