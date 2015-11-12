(in-package :cl-user)
(defpackage :slack-nippo.fetch
  (:use :cl
        :annot.doc
        :slack-nippo.util
        :quri)
  (:import-from :anaphora
                :it
                :aand))
(in-package :slack-nippo.fetch)

(syntax:use-syntax :annot)

@doc
"Endpoint of Slack"
(defparameter +endpoint+ "https://slack.com/api")

@export
@doc
"Token for Slack API"
(defparameter *token* (uiop:getenv "SLACK_TOKEN"))

@export
(defun fetch (path &optional additional-parameters)
  (let ((uri (uri (format nil "~a~a" +endpoint+ path)))
        (parameters (append additional-parameters
                            (list (cons "token" *token*)))))
    (setf (uri-query-params uri) parameters)
    (let ((response
            (jojo:parse
             (flexi-streams:octets-to-string
              (dex:get (render-uri uri) :force-binary t))
             :as :alist)))
      (if (aand (assoc-value "ok" response) (print it))
          response
          (let ((error (assoc-value "error" response)))
            (error "ERROR: ~a" (or error "NO ERROR MESSAGE")))))))
