(in-package :cl-user)
(defpackage :slack-nippo.trello
  (:use :cl
        :annot.doc
        :annot.class
        :slack-nippo.message)
  (:import-from :anaphora
               :it
                :aand)
  (:import-from :ppcre
                :scan
                :scan-to-strings))
(in-package :slack-nippo.trello)

(syntax:use-syntax :annot)

@export
(defparameter *trello-token* (uiop:getenv "TRELLO_TOKEN"))

(defparameter *tasks* nil)

@export
@export-structure
(defstruct (task (:constructor %make-task))
  id
  title
  url)

@export
(defun find-task (id-or-task &optional (tasks *tasks*))
  (let ((id (etypecase id-or-task
              (string id-or-task)
              (task (task-id id-or-task)))))
    (dolist (task tasks)
      (when (equal id (task-id task))
        (return-from find-task
          task)))
    nil))

@export
@export-structure
(defun make-task (id title url)
  (let ((task (find-task id)))
    (if task
        task
        (let ((new-task (%make-task :id id
                                    :title title
                                    :url url)))
          (push new-task *tasks*)
          new-task))))

@export
@export-structure
(defstruct (task-event (:constructor %make-task-event))
  task
  type)

(defun make-task-event (task type)
  (%make-task-event :task task
                    :type type))

(defparameter +task-types+
  (list (cons :new "New card \"(.*)\" added to list \".*|To Do\"")
        (cons :done "Card moved: \"(.*)\" from list \"Doing\" to list \"Done\"")))

@export
(defun task-event-message-p (message)
  (when (aand (car (message-attachments message))
              (scan "https://trello.com" (attachment-text it))
              (equal (message-type message) "message")
              (equal (message-subtype message) "bot_message"))
    t))

(defun parse-url-and-title (url-and-title)
  (multiple-value-bind (match strings)
      (scan-to-strings "<(.*)\\|(.*)>"
                       url-and-title)
    (when (and match
               (= (length strings) 2))
      (let ((url (elt strings 0))
            (title (elt strings 1)))
        (multiple-value-bind (match strings)
            (scan-to-strings "http[|s]://trello.com/c/(.*)/?" url)
          (when (and match
                     (= (length strings) 1))
            (let ((id (elt strings 0)))
              (values id url title))))))))

@export
(defun extract-task-event (message)
  (unless (task-event-message-p message)
    (return-from extract-task-event nil))
  (let* ((attachments (message-attachments message))
         (attachment (car attachments))
         (text (attachment-text attachment)))
    (dolist (type +task-types+)
      (multiple-value-bind (match strings)
          (scan-to-strings (cdr type) text)
        (when (and match
                   (= (length strings) 1)
                   (elt strings 0))
          (multiple-value-bind (id title url)
              (parse-url-and-title (elt strings 0))
            (let ((task (make-task id title url)))
              (return-from extract-task-event
                (make-task-event task (car type))))))))))

@export
(defun extract-tasks (messages)
  (loop for message in messages
        with result = (list :new (list) :done (list))
        for task-event = (extract-task-event message)
        when task-event
          do (push (task-event-task task-event)
                   (getf result (task-event-type task-event)))
        finally (return (append (list :all *tasks*)
                                result))))
