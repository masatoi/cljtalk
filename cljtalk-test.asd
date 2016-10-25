#|
  This file is a part of cljtalk project.
|#

(in-package :cl-user)
(defpackage cljtalk-test-asd
  (:use :cl :asdf))
(in-package :cljtalk-test-asd)

(defsystem cljtalk-test
  :author ""
  :license ""
  :depends-on (:cljtalk
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cljtalk"))))
  :description "Test system for cljtalk"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
