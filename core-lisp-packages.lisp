(in-package :cl-user)

(defpackage :core-lisp-global-definitions (:use))

(defpackage :core-lisp
  (:use :common-lisp)
  (:shadow
   = /= >= <= > < + * -
   abs and append apply aref arithmetic-error-operands arithmetic-error-operation
   array-dimensions assoc atan atanh block
   call-next-method car case catch cdr ceiling cerror char= char/= char< char> char<= char>=
   characterp class class-of close cond cons consp cos cosh
   defclass defconstant defgeneric define-condition defmacro defmethod defun
   elt eq eql equal error exp expt file-length file-position finish-output flet
   float floatp floor format funcall function functionp
   gcd gensym get-internal-real-time get-internal-run-time get-output-stream-string
   get-universal-time go
   identity if ignore-errors input-stream-p integerp internal-time-units-per-second isqrt
   labels lambda lcm length let let* list listp log
   map-into mapc mapcan mapcar mapcon mapl maplist max member min mod
   next-method-p nil not open-stream-p output-stream-p nreverse null numberp
   or probe-file progn quote
   read read-byte read-char read-line return-from reverse round
   setf setq sin sinh sqrt stream-error-stream streamp
   string= string/= string< string> string>= string<= stringp subseq symbolp
   t tagbody tan tanh the throw truncate unwind-protect vector write-byte

   ; extras

   caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
   cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
   )
  (:export
   <arithmetic-error> <basic-array> <basic-vector> <built-in-class>
   <character> <cons> <control-error> <division-by-zero> <domain-error>
   <end-of-stream> <error>
   <float> <floating-point-overflow> <floating-point-underflow> <function>
   <general-vector> <generic-function> <integer> <list> <null> <number> <object>
   <parse-error> <program-error>
   <serious-condition> <simple-error>
   <standard-class> <standard-generic-function> <standard-object>
   <storage-exhausted> <stream> <stream-error> <string> <symbol>
   <unbound-variable> <undefined-entity> <undefined-function>
   &rest *most-positive-float* *most-negative-float* *pi*
   = /= >= <= > < + * -
   abs and append apply aref arithmetic-error-operands arithmetic-error-operation
   array-dimensions assoc assure atan atan2 atanh
   basic-array-p basic-array*-p basic-vector-p block
   call-next-method car case case-using catch cdr ceiling cerror
   char= char/= char< char> char<= char>= char-index characterp
   class class-of close cond condition-continuable cons consp continue-condition
   convert cos cosh create create-array create-list create-string
   create-string-input-stream create-string-output-stream create-vector
   defclass defconstant defdynamic defgeneric defglobal define-condition
   defmacro defmethod defun div domain-error-expected-class domain-error-object
   dynamic dynamic-let
   elt eq eql equal error error-output exp expt
   file-length file-position finish-output flet float floatp floor for
   format format-char format-float format-fresh-line format-integer format-object format-tab
   funcall function functionp
   garef gcd general-array*-p general-vector-p generic-function-p gensym
   get-internal-real-time get-internal-run-time get-output-stream-string
   get-universal-time go
   identity if ignore-errors initialize-object input-stream-p instancep integerp
   internal-time-units-per-second isqrt
   labels lambda lcm length let let* list listp log
   map-into mapc mapcan mapcar mapcon mapl maplist max member min mod
   next-method-p nil not nreverse null numberp
   open-stream-p open-input-file open-io-file open-output-file
   open-stream-p or output-stream-p
   parse-number preview-char probe-file progn property quote quotient
   read read-byte read-char read-line reciprocal remove-property
   return-from reverse round
   set-aref set-car set-cdr set-dynamic set-elt set-file-position set-garef set-property setf setq
   signal-condition simple-error-format-arguments simple-error-format-string
   sin sinh sqrt standard-input standard-output stream-error-stream stream-ready-p streamp
   string= string/= string< string> string>= string<=
   string-append string-index stringp subclassp subseq symbolp
   t tagbody tan tanh the throw truncate undefined-entity-name unwind-protect vector
   while with-error-output with-handler
   with-open-input-file with-open-io-file with-open-output-file
   with-standard-input with-standard-output write-byte

   ; extras

   alias function-alias block-alias tag-alias

   import-variable import-symbol-macro import-function import-macro
   with-imported-variables with-imported-symbol-macros
   with-imported-functions with-imported-macros with-imported-block

   &body &environment &whole
   caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr
   cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr
   
   in-package macroexpand macroexpand-1 macrolet symbol-macrolet
   ))

(defpackage :core-lisp-user
  (:use :core-lisp))
