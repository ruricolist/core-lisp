(in-package :core-lisp)

(cl:defparameter *compile-top-level* t)

(cl:defun package-globals (package)
  (cl:let* ((package-name (package-name package))
            (prefix (format nil "~a::" package-name)))
    (loop for sym being the present-symbols of *core-lisp-global-package*
          for name = (symbol-name sym)
          when (serapeum:string^= prefix name)
            collect sym)))

(cl:defun reset-package-globals (package)
  (loop with global-package = *core-lisp-global-package*
        for sym in (package-globals package)
        do (unintern sym global-package)
        finally (return package)))

(cl:defun read-module (source stream)
  (cl:let* ((use-list (list (find-package :core-lisp)))
            (package (overlord:reset-file-package source :use-list use-list))
            (readtable (named-readtables:find-readtable 'core-lisp)))
    ;; I can't come up with an example where this could matter, but
    ;; it's probably better to start clean.
    (reset-package-globals package)
    `(module-progn
       ,@(overlord:slurp-stream stream
                                :readtable readtable
                                :package package))))

(cl:defmacro module-progn (&body body)
  ;; Variable-only at the moment.
  (cl:let* ((export-forms
              (loop for form in body
                    if (and (consp form)
                            (eql (first form) :export))
                      collect form))
            (export-default-form
              (loop for form in body
                    if (and (consp form) (eql (first form) :export-default))
                      return form))
            (meta-forms
              (append export-forms (list export-default-form)))
            (body
              (remove-if (lambda (form)
                           (member form meta-forms))
                         body))
            (exports
              (append
               (loop for form in export-forms
                     append (rest form))
               (and export-default-form
                    `((:default ,(second export-default-form)))))))
    (assert (not (and export-forms export-default-form)))
    `(progn
       ,@body
       (setq overlord/specials:*module*
             ,(with-unique-names (ht)
                `(let ((,ht (make-hash-table)))
                   ,@(loop for export in exports
                           for (key form) = (etypecase export
                                              (cl:symbol
                                               (list (alexandria:make-keyword export)
                                                     export))
                                              ((cl:cons (cl:eql function)
                                                        (cl:cons symbol cl:null))
                                               (list (alexandria:make-keyword (second export))
                                                     `(function ,(second export))))
                                              ((cl:cons (cl:eql macro-function)
                                                        (cl:cons symbol cl:null))
                                               (list (alexandria:make-keyword (second export))
                                                     `(macro-function ',(second export))))
                                              ((cl:cons (cl:eql :default) (cl:cons cl:t cl:null))
                                               (list (first export) (second export))))
                           collect `(setf (gethash ,key ,ht) ,form))
                   ,ht))))))

(defmacro import (m &rest args)
  `(macrolet ((overlord/shadows:defmacro (name args &body body)
                  (declare (ignore args body))
                ;; ISLISP's defmacro supports neither &environment
                ;; nor &whole nor even &body.
                (let ((key (alexandria:make-keyword name))
                      (env nil))
                  `(defmacro ,name (&rest args)
                     (list 'funcall
                           '(overlord:module-ref* ,',m ',key)
                           (list 'quote (cons ',name args))
                           ,env))))
              (overlord/shadows:defun (name args &body body)
                  (list* 'defun name args body))
              (overlord/shadows:defalias (name expr)
                `(let ((fn ,expr))
                   (defun ,name (&rest args)
                     (apply fn args))))
              (overlord/shadows:def (name init)
                (list 'defglobal name init))
              (overlord/shadows:define-symbol-macro (name expr)
                (list 'define-symbol-macro name expr)))
     (overlord:import ,m ,@args)))
