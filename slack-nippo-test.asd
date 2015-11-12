#|
  This file is a part of slack-nippo project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage slack-nippo-test-asd
  (:use :cl :asdf))
(in-package :slack-nippo-test-asd)

(defsystem slack-nippo-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/slack-nippo"
  :depends-on (:slack-nippo
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "slack-nippo"))))
  :description "Test system for slack-nippo."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
