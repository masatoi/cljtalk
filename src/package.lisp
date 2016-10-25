;;; -*- coding:utf-8; mode:lisp; -*-

(in-package :cl-user)

(defpackage cljtalk
  (:use :cl :wiz :cffi :cl-libworld :mgl :mgl-mat :mgl-user)
  (:shadowing-import-from :wiz :append1 :m+ :m- :m* :group :max-position :while :last1)
  (:shadowing-import-from :alexandria :shuffle)
  (:shadowing-import-from :mgl-mat :mat))
