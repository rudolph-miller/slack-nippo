(in-package :cl-user)
(defpackage :slack-nippo.channel
  (:use :cl
        :annot.doc
        :annot.class
        :slack-nippo.util
        :slack-nippo.fetch))
(in-package :slack-nippo.channel)

(syntax:use-syntax :annot)

@export
@export-structure
(defstruct (channel (:constructor %make-channel))
  id name)

@export
(defun make-channel (channel)
  (%make-channel :id (assoc-value "id" channel)
                 :name (assoc-value "name" channel)))

(defun get-all-channels ()
  (let ((channels (assoc-value "channels" (fetch "/channels.list"))))
    (mapcar #'(lambda (channel) (make-channel channel))
            channels)))

@export
(defun get-channel (name)
  (let ((channels (get-all-channels)))
    (dolist (channel channels)
      (when (equal (channel-name channel) name)
        (return-from get-channel channel)))
    nil))
