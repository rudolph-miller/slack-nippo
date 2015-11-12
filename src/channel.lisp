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
(defstruct channel
  id name)

(defun get-all-channels ()
  (let ((channels (assoc-value "channels" (fetch "/channels.list"))))
    (mapcar #'(lambda (channel)
                (make-channel :id (assoc-value "id" channel)
                              :name (assoc-value "name" channel)))
            channels)))

(defun get-channel (name)
  (let ((channels (get-all-channels)))
    (dolist (channel channels)
      (when (equal (channel-name channel) name)
        (return-from get-channel channel)))
    nil))
