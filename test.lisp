(cl:defpackage :core-lisp/test
  (:use :core-lisp :5am)
  (:import-from :vernacular/tests :with-imports*)
  (:import-from :overlord/tests :with-temp-db :touch)
  (:local-nicknames (:v :vernacular)))
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

(def-suite islisp)

(in-suite islisp)

(test hello-islisp
  (is (equal "hello world"
             (with-imports* (m :from "tests/islisp.lsp" :binding (hello))
               hello))))

(test islisp-dont-be-shadowed
  (is (equal '(:right :right :right)
             (with-imports* (m :from "tests/dont-be-shadowed.lsp"
                               :binding (syms (xyz :as #'expand-xyz)))
               (cl:destructuring-bind (x y z) syms
                 (cl:eval
                  `(let ((,x :wrong)) (cl:declare (ignorable ,x))
                     (flet ((,y () :wrong)) (cl:declare (cl:ignore #',y))
                       (macrolet ((,z () :wrong))
                         ,(expand-xyz nil nil))))))))))

;;; TODO.
;; (test islisp-imports
;;   (is (equal '(:var :fn :macro)
;;              (require-default "tests/imports.lsp"))))

(test islisp-auto-alias
  (is (equal '(0 1)
             (v:require-default "tests/shadowing.lsp"))))

(test islisp-hygiene
  (touch #1="tests/hygiene.lsp")
  ;; Not the desired results, just the ones we expect.
  (cl:handler-bind ((cl:warning #'cl:muffle-warning))
    (is (equal '(4 6 :ERROR 4 16 :ERROR)
               (v:require-default #1#)))))

(test islisp-globals-can-close
  "Test that globals defined with `defglobal' close over themselves."
  (with-imports* (m :from "tests/globals-can-close.lsp" :binding (x))
    (is (eql x (funcall x)))))

(test islisp-phasing
  "Test that state is not preserved across rebuilds."
  (v:require-as nil #1="tests/phasing.lsp")
  (with-imports* (m :from #1# :binding (#'inc-count))
    (is (= (inc-count) 0))))
