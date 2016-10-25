#|
  This file is a part of cljtalk project.
|#

(in-package :cl-user)
(defpackage cljtalk-asd
  (:use :cl :asdf))
(in-package :cljtalk-asd)

(defsystem cljtalk
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:wiz-util
               :clgplot
               :cffi
               :cl-libworld
               :cl-ppcre
               :parse-number
               :mgl-user)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "mel-filter-bank" :depends-on ("package"))
                 (:file "dct" :depends-on ("package"))
                 (:file "count-phoneme" :depends-on ("package"))
                 (:file "make-input-features" :depends-on ("package"))
                 (:file "cljtalk" :depends-on ("package")))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cljtalk-test))))
