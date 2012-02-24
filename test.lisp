(cl:in-package :srfi-100.internal)
(in-readtable :quasiquote)

(def-suite srfi-100)

(in-suite srfi-100)

(defmacro == (a => b)
  (declare (ignore =>))
  `(is (equal ,a ',b)))

(defmacro === (a => b)
  (declare (ignore =>))
  `(is (equalp ,a ',b)))

(test :define-lambda-object
  (defparameter *color* 'black)

  (define-lambda-object ppoint (x) y)

  (define-lambda-object (cpoint ppoint)
      (x) y
      (,color-init *color*) (,area-init (* x y))
      (`,color *color*) (`,area (* x y)))

  (defparameter pp (make-ppoint 10 20))
  ;; 1
  (== (funcall pp 'x) => 10)
  ;; 2
  (== (funcall pp 'y) => 20)
  (funcall pp 'x 11)                ;*
  ;; 3
  (== (funcall pp 'x) => 11)
  ;; 4
  (signals (cl:error) (funcall pp 'y 22))

  (defparameter ap (make-cpoint 10 20))
  (== (map ap '(x y color-init color area-init area))
      => (10 20 black black 200 200))
  (funcall ap 'x 30)              ;*
  ;; 5
  (== (map ap '(x y color-init color area-init area))
      => (30 20 black black 200 600))
  (setq *color* 'white)             ;*
  ;; 6
  (== (map ap '(x y color-init color area-init area))
      => (30 20 black white 200 600))

  (setq *color* 'white)

  (define-lambda-object (cpoint2 ppoint)
      (x) y
      ((,,color) *color*)
      (`,area (* x y))
      (,set/add (lambda (i j) (set! x (+ i x)) (set! y (+ j y)))) )

  (defparameter tp (make-cpoint2 10 15))
  (== (map tp '(x y color area))
      => (10 15 white 150))

  (defparameter cp (make-cpoint2 15 20))
  (funcall cp 'color 'white)
  (== (map cp '(x y color area))
      => (15 20 white 300))
  (funcall cp 'color 'brown)      ;*
  (funcall (funcall cp 'set/add) 5 10)    ;*
  ;; 9
  (== (map cp '(x y color area))
      => (20 30 brown 600))
  ;; 10
  (== (map tp '(x y color area))
      => (10 15 brown 150))
  ;; 11
  (== (cpoint2? ap) => nil)
  ;; 12
  (== (cpoint2? tp) => T)
  ;; 13
  (== (cpoint2? cp) => T)
  ;; 14
  (== (ppoint? cp) => T)

  (define-lambda-object (spoint (ppoint))
      (x 0)
    (y x)
    (z x)
    ('stack '())
    (`,pop (if (null? stack)
               (error 'spoint "null stack" stack)
               (let ((s (car stack))) (set! stack (cdr stack)) s) ))
    (,push (lambda (s) (set! stack (cons s stack)))) )

  (defparameter sp (make-spoint))
  (== (map sp '(x y z))
      => (0 0 0))

  (defparameter sp (make-spoint 5 55))
  (== (map sp '(x y z))
      => (5 55 5))

  (defparameter sp (make-spoint-by-name 'z 100 'stack (list 'sunflower)))
  (== (map sp '(x y z))
      => (0 0 100))
  (funcall (funcall sp 'push) 'rose) ;*
  (funcall (funcall sp 'push) 'lily) ;*
  (== (funcall sp 'pop)
      => lily)
  (== (funcall sp 'pop)
      => rose)
  (== (funcall sp 'pop)
      => sunflower)
  (signals (cl:error)
    (funcall sp 'pop) )
  (signals (cl:error)
    (funcall sp 'stack) )

  (define-lambda-object (epoint (spoint) (cpoint2))
      ((x) 5) ((y) 10) ((z) 15) ((planet) "earth")
      (,,color "brown")
      (',stack '())
      (`,area (* x y))
      (`,volume (* x y z))
      (`,pop (if (null? stack)
                 (error 'spoint "null stack" stack)
                 (let ((s (car stack))) (set! stack (cdr stack)) s) ))
      (,push (lambda (s) (set! stack (cons s stack))))
      (,adbmal (lambda (f) (funcall
                            f x y z color planet (* x y) (* x y z))))
      (,set/add
       (srfi-16:case-lambda
        ((i j) (cond
                 ((and (string? i) (string? j))
                  (set! color i) (set! planet j))
                 ((and (number? i) (number? j))
                  (set! x (+ i x)) (set! y (+ j y)))
                 (:else (error 'epoint "set/add: wrong data type" i j)) ))
        ((i j k) (set! x (+ i x)) (set! y (+ j y)) (set! z (+ k z))) )))

  (defparameter ep (make-epoint-by-name 'planet "jupiter"))
  (=== (funcall (funcall ep 'adbmal) #'vector)
       => #(5 10 15 "brown" "jupiter" 50 750))
  (defparameter tp (make-epoint 10 15 20))
  (=== (funcall (funcall tp 'adbmal) #'vector)
       => #(10 15 20 "brown" "earth" 150 3000))
  (== (map (lambda (o) (funcall o 'x))
           (list pp ap cp sp ep) )
      => (11 30 20 0 5))
  (== (map (lambda (p) (funcall p ep))
           (list #'ppoint? #'cpoint2? #'spoint? #'epoint?) )
      => (T T T T))

  (funcall (funcall ep 'set/add) "red" "mars") ;*
  (== (funcall (funcall ep 'adbmal) #'list)
      => (5 10 15 "red" "mars" 50 750))
  (== (funcall (funcall tp 'adbmal) #'list)
      => (10 15 20 "red" "earth" 150 3000))
  (funcall (funcall ep 'set/add) 5 10) ;*
  (== (funcall (funcall ep 'adbmal) #'list)
      => (10 20 15 "red" "mars" 200 3000))
  (funcall (funcall ep 'set/add) 10 30 50) ;*
  (== (map ep '(x y z area volume))
      => (20 50 65 1000 65000))
  (== (map cp '(x y area))
      => (20 30 600))
  (funcall (funcall cp 'set/add) 20 50) ;*
  (== (map cp '(x y area))
      => (40 80 3200))
  (signals (cl:error)
    (funcall (funcall cp 'set/add) 10 100 1000) )
  (is-true (functionp #'epoint))
  (is-true (every #'functionp (epoint :parent)))
  ;; *doted list*
  (is-true (destructuring-bind (a . b)
                               (epoint :constructor)
             (and (functionp a)
                  (functionp b) )))
  (== (epoint :read-write-field)
      => (x y z planet))
  (== (epoint :read-only-field)
      => (color area volume pop push adbmal set/add))
  (== (epoint :required-field)
      => ())
  (== (epoint :optional-field)
      => ((x 5) (y 10) (z 15) (planet "earth")))
  (== (epoint :common-field)
      => ((color "brown")))
  (== (epoint :hidden-field)
      => ((stack '())))
  (== (epoint :virtual-field)
      => ((area (* x y))
          (volume (* x y z))
          (pop (if (null? stack)
                   (error 'spoint "null stack" stack)
                   (let ((s (car stack)))
                     (set! stack (cdr stack)) s)))))
  (== (epoint :automatic-field)
      => ((color "brown")
          (area (* x y))
          (volume (* x y z))
          (pop
           (if (null? stack)
               (error 'spoint "null stack" stack)
               (let ((s (car stack))) (set! stack (cdr stack)) s) ))
          (stack '())
          (push (lambda (s) (set! stack (cons s stack))))
          (adbmal (lambda (f)
                    (funcall f
                             x
                             y
                             z
                             color
                             planet
                             (* x y)
                             (* x y z) )))
          (set/add
           (srfi-16:case-lambda
            ((i j)
             (cond
               ((and (string? i) (string? j))
                (set! color i) (set! planet j))
               ((and (number? i) (number? j))
                (set! x (+ i x)) (set! y (+ j y)))
               (:else
                (error 'epoint "set/add: wrong data type" i j) )))
            ((i j k)
             (set! x (+ i x))
             (set! y (+ j y))
             (set! z (+ k z)) ))))))

(test :error1
  (signals (cl:error)
    (define-lambda-object (cpoint ppoint) x y color)))

(test :error-cp
  (signals (cl:error)
    (define-function ap (make-cpoint 3 33 'black))))

(test :2
  (define-lambda-object o1 (x))
  (define-lambda-object p1 (y))
  (define-lambda-object (o2 o1) (x))
  (define-lambda-object (o3 (o1)) (x))
  (is-true (eq (car (o2 :parent)) #'o1))
  (is-true (eq (car (o3 :parent)) #'o1))
  (define-lambda-object (o4 (o1) (p1)) (x) (y))
  (is-true (o4? (make-o4 1 2)))
  (is-true (o1? (make-o4 1 2)))
  (is-true (p1? (make-o4 1 2)))
  (is-false (o2? (make-o4 1 2))))

;;; eof
