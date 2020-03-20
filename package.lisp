;;;; package.lisp

(cl:in-package :cl-user)


(defpackage "https://github.com/g000001/srfi-100"
  (:use)
  (:export define-lambda-object))


(defpackage "https://github.com/g000001/srfi-100#internals"
  (:use
   "https://github.com/g000001/srfi-100"
   cl
   named-readtables
   fiveam
   mbe
   quasiquote1)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-23" 
   error)
  (:shadow lambda member assoc map loop))


;;; *EOF*
