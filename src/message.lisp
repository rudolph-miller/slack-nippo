(in-package :cl-user)
(defpackage :slack-nippo.message
  (:use :cl
        :annot.doc
        :annot.class
        :slack-nippo.util
        :slack-nippo.fetch
        :slack-nippo.channel))
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

@export
(defun make-message (message)
  (%make-message :type (assoc-value "type" message)
                 :subtype (assoc-value "subtype" message)
                 :ts (assoc-value "ts" message)
                 :user (assoc-value "user" message)
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
                    :fields (assoc-value "fields" attachment)
                    :image-url (assoc-value "image_url" attachment)
                    :thumb-url (assoc-value "thumb_url" attachment)))

@export
(defun get-messages (channel &key latest oldest count)
  (declare (ignore latest oldest))
  (let* ((channel-id (channel-id channel))
         (params (append `(("channel" . ,channel-id))
                         (when count
                           `(("count" . ,(write-to-string count))))))
         (messages (assoc-value "messages" (fetch "/channels.history" params))))
    (mapcar #'(lambda (message) (make-message message))
            messages)))
