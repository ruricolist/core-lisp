(in-package :core-lisp)

(cl:defparameter *compile-top-level* t)

(cl:defun read-module (source stream)
  (cl:let* ((use-list (find-package :core-lisp))
            (cl:*package* (overlord:ensure-file-package source :use-list use-list)))
    (loop with eof = "eof"
          for form = (cl:read stream nil eof)
          until (cl:eq form eof)
          collect form into forms
          finally (return `(module-progn ,@forms)))))

(cl:defmacro module-progn (&body body)
  ;; Variable-only at the moment.
  (cl:let* ((export-forms
              (loop for form in body
                    if (and (consp form)
                            (eql (first form) :export))
                      collect form))
            (exports
              (loop for form in export-forms
                    append (rest form)))
            (body
              (remove-if (lambda (form)
                           (member form export-forms))
                         body)))
    `(progn
       ,@body
       (setq overlord/specials:*module*
             ,(with-unique-names (ht)
                `(let ((,ht (make-hash-table)))
                   ,@(loop for var in exports
                           for key = (alexandria:make-keyword var)
                           collect `(setf (gethash ,key ,ht) ,var))
                   ,ht))))))
