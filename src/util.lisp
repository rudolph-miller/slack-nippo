(in-package :cl-user)
(defpackage :slack-nippo.util
  (:use :cl
        :annot.doc))
(in-package :slack-nippo.util)

(syntax:use-syntax :annot)

@export
(defun assoc-value (item alist)
  (cdr (assoc item alist :test #'equal)))
   
