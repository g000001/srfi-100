;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-100
  (:use)
  (:export
   :define-lambda-object))

(defpackage :srfi-100.internal
  (:use :srfi-100 :cl :named-readtables :fiveam
        :mbe :quasiquote1)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :lambda :member :assoc :map :loop))
