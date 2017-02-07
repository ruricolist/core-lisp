(defpackage :core-lisp/test
  (:use :cl :5am)
  (:import-from :asdf
    :system-relative-pathname)
  (:import-from :uiop
    :native-namestring
    :run-program)
  (:import-from :overlord
    :with-imports
    :require-as))
(in-package :core-lisp/test)

(def-suite core-lisp)
(in-suite core-lisp)

(defun touch (file)
  (let* ((file (system-relative-pathname :core-lisp/test file))
         (file-string (native-namestring file)))
    (run-program `("touch" ,file-string))
    file-string))

(test hello-islisp
  (touch #1="test/islisp.lsp")
  (is (equal "hello world"
             (with-imports (m :from #1# :binding (hello))
               hello))))

(test islisp-dont-be-shadowed
  (touch #1="test/dont-be-shadowed.lsp")
  (is (equal '(:right :right :right)
             (with-imports (m :from #1# :binding (syms (xyz #'expand-xyz)))
               (destructuring-bind (x y z) syms
                 (eval
                  `(let ((,x :wrong)) (declare (ignorable ,x))
                     (flet ((,y () :wrong)) (declare (ignore #',y))
                       (macrolet ((,z () :wrong))
                         ,(expand-xyz nil nil))))))))))

(test islisp-imports
  (touch "test/exports.lsp")
  (touch #1="test/imports.lsp")
  (is (equal '(:var :fn :macro) (require-as nil #1#))))

(test islisp-auto-alias
  (touch #1="test/shadowing.lsp")
  (is (equal '(0 1) (require-as nil #1#))))

(test islisp-hygiene
  (touch #1="test/hygiene.lsp")
  ;; Not the desired results, just the ones we expect.
  (handler-bind ((warning #'muffle-warning))
    (is (equal '(4 6 :ERROR 4 16 :ERROR) (require-as nil #1#)))))

(test islisp-globals-can-close
  "Test that globals defined with `defglobal' close over themselves."
  (touch #1="test/globals-can-close.lsp")
  (with-imports (m :from #1# :binding (x))
    (is (eql x (funcall x)))))

(test islisp-phasing
  "Test that state is not preserved across rebuilds."
  (touch #1="test/phasing.lsp")
  (overlord:require-as :core-lisp #1#)
  (with-imports (m :from #1# :binding (#'inc-count))
    (is (= (inc-count) 0))))

(defun run-tests ()
  (run! 'core-lisp))
