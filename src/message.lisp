(in-package :cl-user)
(defpackage :slack-nippo.message
  (:use :cl
        :annot.doc
        :annot.class
        :slack-nippo.util
        :slack-nippo.fetch
        :slack-nippo.channel
        :slack-nippo.user)
  (:import-from :local-time
                :unix-to-timestamp)
  (:shadow :id
           :name
           :user))
(in-package :slack-nippo.message)

(syntax:use-syntax :annot)

@export
@export-structure
(defstruct (message (:constructor %make-message))
  type
  subtype
  ts
  user
  text
  attachments
  starred-p)

(defun parse-ts (timestamp)
  (let ((integer (parse-integer timestamp :end (position #\. timestamp))))
    (when integer
      (unix-to-timestamp integer))))

@export
(defun make-message (message)
  (%make-message :type (assoc-value "type" message)
                 :subtype (assoc-value "subtype" message)
                 :ts (parse-ts (assoc-value "ts" message))
                 :user (let ((id (assoc-value "user" message)))
                         (get-user id))
                 :text (assoc-value "text" message)
                 :attachments (mapcar #'(lambda (attachment)
                                          (make-attachment attachment))
                                      (assoc-value "attachments" message))
                 :starred-p (assoc-value "is_starred" message)))


@export
@export-structure
(defstruct (attachment (:constructor %make-attachment))
  fallback
  color
  pretext
  author-name
  author-link
  author-icon
  title
  title-link
  text
  fields
  image-url
  thumb-url)

@export
(defun make-attachment (attachment)
  (%make-attachment :fallback (assoc-value "fallback" attachment)
                    :color (assoc-value "color" attachment)
                    :pretext (assoc-value "pretext" attachment)
                    :author-name (assoc-value "author_name" attachment)
                    :author-link (assoc-value "author_link" attachment)
                    :author-icon (assoc-value "author_icon" attachment)
                    :title (assoc-value "title" attachment)
                    :title-link (assoc-value "title_link" attachment)
                    :text (assoc-value "text" attachment)
                    :fields (mapcar #'make-attachment-field
                                    (assoc-value "fields" attachment))
                    :image-url (assoc-value "image_url" attachment)
                    :thumb-url (assoc-value "thumb_url" attachment)))

@export
@export-structure
(defstruct (attachment-field (:constructor %make-attachment-field))
  title
  value
  short)

@export
(defun make-attachment-field (attachment-field)
  (%make-attachment-field :title (assoc-value "title" attachment-field)
                          :value (assoc-value "value" attachment-field)
                          :short (assoc-value "short" attachment-field)))

@export
(defun get-messages (channel &key latest oldest (count 100))
  (declare (ignore latest oldest))
  (let* ((channel-id (channel-id channel))
         (params (append `(("channel" . ,channel-id))
                         (when count
                           `(("count" . ,(write-to-string count))))))
         (response (fetch "/channels.history" params))
         (messages (assoc-value "messages" response)))
    (mapcar #'make-message messages)))
