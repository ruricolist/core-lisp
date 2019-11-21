(cl:defpackage :core-lisp/test
  (:use :core-lisp :5am))
(cl:in-package :core-lisp/test)

(def-suite core-lisp)
(in-suite core-lisp)

(cl:defun run-tests ()
  "Entry point for running tests."
  ;; NB Has to be a CL function so ASDF can call it by name.
  (run! 'core-lisp))

(test assure
  (is (numberp (assure <number> 42))))

(defun iota-h (x lim)
  (if (eq x lim)
      (list x)
      (cons x (iota-h (+ x 1) lim))))

(test rfn
  (is (equal '(1 2 3 4 5) (iota-h 1 5))))

(test abstract-class
  (finishes (cl:eval '(defclass <expr> () () (:abstractp t)))))
