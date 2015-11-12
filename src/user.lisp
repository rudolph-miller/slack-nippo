(in-package :cl-user)
(defpackage :slack-nippo.user
  (:use :cl
        :annot.class
        :annot.doc
        :slack-nippo.util
        :slack-nippo.fetch))
(in-package :slack-nippo.user)

(syntax:use-syntax :annot)

@export
@export-structure
(defstruct (user (:constructor %make-user))
  id
  name)

@export
(defun make-user (user)
  (%make-user :id (assoc-value "id" user)
              :name (assoc-value "name" user)))

(defparameter *users* nil)

@export
(defun get-all-users (&optional (cache t))
  (if (and cache *users*)
      *users*
      (let ((users (assoc-value "members" (fetch "/users.list"))))
        (setf *users*
              (mapcar #'(lambda (user) (make-user user))
                      users)))))

@export
(defun get-user (id)
  (find id (get-all-users)
        :key #'user-id
        :test #'equal))
