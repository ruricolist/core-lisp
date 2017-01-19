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
              (loop for form in export-forms
                    append (rest form)))
            (default-export
              (second export-default-form)))
    (assert (not (and export-forms export-default-form)))
    `(progn
       ,@body
       (setq overlord/specials:*module*
             ,(if export-default-form
                  default-export
                  (with-unique-names (ht)
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
                                                         `(macro-function ',(second export)))))
                               collect `(setf (gethash ,key ,ht) ,form))
                       ,ht)))))))
