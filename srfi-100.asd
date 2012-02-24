;;;; srfi-100.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-100
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :quasiquote1
               :srfi-23
               :mbe)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-100")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-100))))
  (load-system :srfi-100)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-100.internal :srfi-100))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
