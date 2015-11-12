(in-package :cl-user)
(defpackage :slack-nippo.trello
  (:use :cl
        :annot.doc
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
(defun task-p (message)
  (when (aand (car (message-attachments message))
              (scan "https://trello.com" (attachment-text it))
              (equal (message-type message) "bot_message"))
    t))

(defun split-url-and-title (url-and-title)
  (multiple-value-bind (match strings)
      (scan-to-strings "<(.*)\\|(.*)>"
                       url-and-title)
    (when (and match
               (= (length strings) 2))
      (cons (elt strings 0)
            (elt strings 1)))))

(defparameter +task-types+
  (list (cons :new "New card \"(.*)\" added to list \".*|To Do\"")
        (cons :done "Card moved: \"(.*)\" from list \"Doing\" to list \"Done\"")))

(defun detect-task-type (message)
  (let* ((attachments (message-attachments message))
         (attachment (car attachments))
         (text (attachment-text attachment)))
    (dolist (type +task-types+)
      (multiple-value-bind (match strings)
          (scan-to-strings (cdr type) text)
        (when (and match (= (length strings) 1))
          (return-from detect-task-type
            (values (car type)
                    (split-url-and-title (elt strings 0)))))))))

(defun partition-tasks (messages)
  (let (news dones)
    (dolist (message messages)
      (when (task-p message)
        (multiple-value-bind (type url-and-title)
            (detect-task-type message)
          (unless url-and-title
            (print type)
            (print message))
          (case type
            (:new (push url-and-title news))
            (:done (push url-and-title dones))))))
    (list :news news
          :dones dones)))
