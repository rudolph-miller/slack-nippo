(in-package :cl-user)
(defpackage :slack-nippo.user
  (:use :cl
        :annot.class
        :annot.doc
        :slack-nippo.util
        :slack-nippo.fetch))
(in-package :slack-nippo.user)

(syntax:use-syntax :annot)
