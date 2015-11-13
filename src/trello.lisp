(in-package :cl-user)
(defpackage :slack-nippo.trello
  (:use :cl
        :annot.doc
        :annot.class
        :slack-nippo.util
        :slack-nippo.message
        :quri)
  (:import-from :anaphora
                :it
                :aand)
  (:import-from :ppcre
                :scan
                :scan-to-strings))
(in-package :slack-nippo.trello)

(syntax:use-syntax :annot)

@export
(defparameter *trello-key* (uiop:getenv "TRELLO_KEY"))

@export
(defparameter *trello-token* (uiop:getenv "TRELLO_TOKEN"))

@export
(defparameter *trello-username* (uiop:getenv "TRELLO_USERNAME"))

(defparameter +endpoint+ "https://trello.com/1")

@export
(defparameter *cards* nil)

(defun fetch-trello (path &optional additional-parameters)
  (let ((uri (uri (format nil "~a~a" +endpoint+ path)))
        (parameters (append additional-parameters
                            (list (cons "key" *trello-key*)
                                  (cons "token" *trello-token*)))))
    (setf (uri-query-params uri) parameters)
    (let ((response
            (jojo:parse
             (babel:octets-to-string
              (dex:get (render-uri uri) :force-binary t))
             :as :alist)))
      response)))

(defstruct (board (:constructor %make-board))
  id
  name)

(defun make-board (board)
  (%make-board :id (assoc-value "id" board)
               :name (assoc-value "name" board)))

(defun get-all-boards (&optional (username *trello-username*))
  (assert username)
  (let ((boards (fetch-trello (format nil "/members/~a/boards" username))))
    (when boards
      (mapcar #'make-board boards))))

(defun get-board (name)
  (let ((boards (get-all-boards)))
    (dolist (board boards)
      (when (equal (board-name board) name)
        (return-from get-board
          board)))
    nil))

(defstruct (trello-list (:constructor %make-trello-list))
  id
  name)

(defun make-trello-list (list)
  (%make-trello-list :id (assoc-value "id" list)
              :name (assoc-value "name" list)))

(defun get-all-lists (board-or-name)
  (let* ((board (etypecase board-or-name
                  (string (get-board board-or-name))
                  (board board-or-name)))
         (lists (fetch-trello (format nil "/boards/~a/lists" (board-id board)))))
    (when lists
      (mapcar #'make-trello-list lists))))


(defun get-list (name board)
  (let ((lists (get-all-lists board)))
    (dolist (list lists)
      (when (equal (trello-list-name list) name)
        (return-from get-list
          list)))
    nil))

@export
@export-structure
(defstruct (card (:constructor %make-card))
  id
  name
  url)

@export
(defun find-card (id-or-card &optional (cards *cards*))
  (let ((id (etypecase id-or-card
              (string id-or-card)
              (card (card-id id-or-card)))))
    (dolist (card cards)
      (when (equal id (card-id card))
        (return-from find-card
          card)))
    nil))

@export
(defun make-card (id name url &optional force-update)
  (let ((card (find-card id)))
    (if card
        (if force-update
            (progn (setf (card-name card) name)
                   (setf (card-url card) url)
                   card)
            card)
        (let ((new-card
                (%make-card :id id
                            :name name
                            :url url)))
          (push new-card *cards*)
          new-card))))

@export
(defun get-cards (list-or-name board)
  (let* ((list (etypecase list-or-name
                 (string (get-list list-or-name board))
                 (trello-list list-or-name)))
         (cards (fetch-trello (format nil "/lists/~a/cards" (trello-list-id list)))))
    (when cards
      (mapcar #'(lambda (card)
                  (make-card (assoc-value "id" card)
                             (assoc-value "name" card)
                             (assoc-value "shortUrl" card)
                             t))
              cards))))

@export
@export-structure
(defstruct (card-event (:constructor %make-card-event))
  card
  type)

(defun make-card-event (card type)
  (%make-card-event :card card
                    :type type))

(defparameter +card-types+
  (list (cons :new "New card \"(.*)\" added to list \".*|To Do\"")
        (cons :doing "Card moved: \"(.*)\" from list \"To Do\" to list \"Doing\"")
        (cons :pending "Card moved: \"(.*)\" from list \"Doing\" to list \"To Do\"")
        (cons :revert "Card moved: \"(.*)\" from list \"Done\" to list \"To Do\"")
        (cons :done "Card moved: \"(.*)\" from list \".*\" to list \"Done\"")))

@export
(defun card-event-message-p (message)
  (when (aand (car (message-attachments message))
              (scan "https://trello.com" (attachment-text it))
              (equal (message-type message) "message")
              (equal (message-subtype message) "bot_message"))
    t))

(defun parse-url-and-name (url-and-name)
  (multiple-value-bind (match strings)
      (scan-to-strings "<(.*)\\|(.*)>"
                       url-and-name)
    (when (and match
               (= (length strings) 2))
      (let ((url (elt strings 0))
            (name (elt strings 1)))
        (multiple-value-bind (match strings)
            (scan-to-strings "http[|s]://trello.com/c/(.*)/?" url)
          (when (and match
                     (= (length strings) 1))
            (let ((id (elt strings 0)))
              (values id name url))))))))

@export
(defun extract-card-event (message)
  (unless (card-event-message-p message)
    (return-from extract-card-event nil))
  (let* ((attachments (message-attachments message))
         (attachment (car attachments))
         (text (attachment-text attachment)))
    (dolist (type +card-types+)
      (multiple-value-bind (match strings)
          (scan-to-strings (cdr type) text)
        (when (and match
                   (= (length strings) 1)
                   (elt strings 0))
          (multiple-value-bind (id name url)
              (parse-url-and-name (elt strings 0))
            (let ((card (make-card id name url)))
              (return-from extract-card-event
                (make-card-event card (car type))))))))))

@export
(defun extract-cards (messages)
  (loop for message in messages
        with result = (list :new (list) :done (list))
        for card-event = (extract-card-event message)
        when card-event
          do (push (card-event-card card-event)
                   (getf result (card-event-type card-event)))
        finally (return (append (list :all *cards*)
                                result))))
