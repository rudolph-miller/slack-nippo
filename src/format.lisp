(in-package :cl-user)
(defpackage :slack-nippo.format
  (:use :cl
        :annot.doc
        :slack-nippo.user
        :slack-nippo.message
        :slack-nippo.markdown
        :slack-nippo.trello)
  (:import-from :local-time
                :timestamp-to-unix
                :unix-to-timestamp
                :format-timestring)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:shadow :user
           :id
           :name)
  (:shadowing-import-from :slack-nippo.message
                          :value))
(in-package :slack-nippo.format)

(syntax:use-syntax :annot)

@export
@doc
"In seconds"
(defparameter +hourly-index-interval+
  (* 60 60 3))

(defun format-card-events (messages)
  (let* ((cards (extract-cards messages))
         (all (getf cards :all))
         (new (getf cards :new))
         (done (getf cards :done)))
    (h1 "Tasks")

    (h2 "To Do")
    (unless all (p "No Cards"))
    (dolist (card all)
      (li1
       (if (find-card card done)
           (format nil "~~~~[~a](~a)~~~~" (card-name card) (card-url card))
           (format nil "[~a](~a)" (card-name card) (card-url card)))))

    (h2 "New")
    (unless new (p "No New Cards"))
    (dolist (card new)
      (li1 (format nil "[~a](~a)" (card-name card) (card-url card))))

    (h2 "Done")
    (unless done (p "No Done Cards"))
    (dolist (card done)
      (li1 (format nil "[~a](~a)" (card-name card) (card-url card))))))

(defun format-trello-log (message)
  (let ((card-event (extract-card-event message)))
    (when card-event
      (let ((card (card-event-card card-event))
            (operation-name (when card-event
                              (case (card-event-type card-event)
                                (:new "ADD")
                                (:doing "DOING")
                                (:done "DONE")))))
        (li1 (format nil
                     "trello: ~a [~a](~a)"
                     operation-name
                     (card-name card)
                     (card-url card)))))))

(defun format-message-log (message)
  (let* ((user-name (user-name (message-user message)))
         (text (message-text message))
         (image-name-and-url (extract-image-name-and-url message))
         (content (cond
                    (image-name-and-url
                     (format nil "![~a](~a)"
                             (car image-name-and-url)
                             (cdr image-name-and-url)))
                    ((find #\Newline text)
                     (format nil "~%~%~a~%" text))
                    (t (format nil "~a" text)))))
    (li1 (format nil "~a: ~a" user-name content))))

(defun extract-image-name-and-url (message)
  (let ((text (message-text message)))
    (cond
      ((equal (message-subtype message) "file_share")
       (multiple-value-bind (match strings)
           (scan-to-strings "<@.*\\|.*> uploaded a file: <(.*\.[png|jpg|jpeg|gif])\\|(.*\.[png|jpg|jpeg|gif])>" text)
         (when (and match
                    (= (length strings) 2))
           (cons (elt strings 1)
                 (elt strings 0)))))
      (t
       (multiple-value-bind (match strings)
           (scan-to-strings "<(.*\.[png|jpg|jpeg|gif])>" text)
         (when (and match
                    (= (length strings) 1))
           (cons "Image"
                 (elt strings 0))))))))

(defun format-log (message)
  (cond
    ((card-event-message-p message) (format-trello-log message))
    ((or (equal (message-subtype message) "file_share")
         (null (message-subtype message)))
     (format-message-log message))))

(defun format-logs (messages)
  (h1 "Logs")
  (let ((current-round 0))
    (dolist (message (reverse messages))
      (when (equal (message-type message) "message")
        (let* ((timestamp (message-ts message))
               (round (truncate (timestamp-to-unix timestamp)
                                +hourly-index-interval+)))
          (when (> round current-round)
            (setf current-round round)
            (h2 (format-timestring
                 nil
                 (unix-to-timestamp (* current-round +hourly-index-interval+))
                 :format '((:hour 2 #\0) (:min 2 #\0)))))
          (format-log message))))))

@export
(defun format-messages (messages &optional (stream t))
  (let ((*stream* stream))
    (format-card-events messages)
    (format-logs messages)))
