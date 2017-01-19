(in-package :core-lisp)

(cl:defun sharp-quote (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(function ,(cl:read stream cl:t cl:nil cl:t)))

(named-readtables:defreadtable core-lisp
  (:merge :standard)
  (:dispatch-macro-char #\# #\' #'sharp-quote))
