(in-package :cl-user)
(defpackage :slack-nippo.markdown
  (:use :cl
        :annot.class
        :annot.doc))
(in-package :slack-nippo.markdown)

(syntax:use-syntax :annot)

@export
(defparameter *stream* nil)

@export
(defun h1 (value)
  (format *stream* "~%# ~a~%~%" value))

@export
(defun h2 (value)
  (format *stream* "~%## ~a~%~%" value))

@export
(defun h3 (value)
  (format *stream* "~%### ~a~%~%" value))

@export
(defun li1 (value)
  (format *stream* "- ~a~%" value))

@export
(defun li2 (value)
  (format *stream* "~t- ~a~%" value))

@export
(defun li3 (value)
  (format *stream* "~t~t- ~a~%" value))

@export
(defun p (value)
  (format *stream* "~a~%" value))
