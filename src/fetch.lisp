(in-package :cl-user)
(defpackage :slack-nippo.fetch
  (:use :cl
        :annot.doc))
(in-package :slack-nippo.fetch)

(syntax:use-syntax :annot)

(defparameter +endpoint+ "https://slack.com/api")
