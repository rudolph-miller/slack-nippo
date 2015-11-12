(in-package :cl-user)
(defpackage :slack-nippo.markdown
  (:use :cl
        :annot.class
        :annot.doc))
(in-package :slack-nippo.markdown)

(syntax:use-syntax :annot)

@export
@export-class
(defclass md ()
  ((value :reader value
          :initarg :value)))

(defun make-md (type value)
  (make-instance type :value value))

(defparameter *stream* nil)

(defgeneric %format-md (md))

(defmacro defmd (type &body body)
  `(progn
     (defclass ,type (md) ())
     (defmethod %format-md ((object ,type))
       ,@body)))

(defmd h1
  (format *stream* "# ~a" (value object)))

(defmd h2
  (format *stream* "## ~a" (value object)))

(defmd h3
  (format *stream* "### ~a" (value object)))

(defun format-md (md &optional stream)
  (let ((*stream* stream))
    (%format-md md)))
