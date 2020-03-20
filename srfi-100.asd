;;;; srfi-100.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-100
  :version "20200321"
  :description "SRFI 100 for CL: define-lambda-object"
  :long-description "SRFI 100 for CL: define-lambda-object
https://srfi.schemers.org/srfi-100"
  :author "Joo ChurlSoo"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (fiveam
               named-readtables
               quasiquote1
               srfi-16
               srfi-23
               mbe)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-100")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-100))))
  (let ((name "https://github.com/g000001/srfi-100")
        (nickname :srfi-100))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-100))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-100#internals")))
    (eval
     (read-from-string
        "
      (or (let ((result (run 'srfi-100)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
