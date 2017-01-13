(in-package :core-lisp)

(cl:defun get-alias (name namespace env)
  (cl:or (cl:cadr (cl:assoc name (macroexpand namespace env)))
         (get name namespace)))

(cl:defun eget-alias (name namespace kind env)
  (cl:or (cl:cadr (cl:assoc name (macroexpand namespace env)))
         (get name namespace)
         (cl:error "No ~S available for symbol ~S." kind name)))

(cl:defmacro define-alias-namespace (namespace accessor)
  `(cl:progn
     (define-symbol-macro ,namespace ())
     (-defmacro- ,accessor (&environment env name)
       `',(eget-alias name ',namespace ',accessor env))))

(define-alias-namespace %aliases% alias)
(define-alias-namespace %function-aliases% function-alias)
(define-alias-namespace %block-aliases% block-alias)

(-import-macro- import-variable -import-variable- (name alias))
(-import-macro- import-symbol-macro -import-symbol-macro- (name alias))
(-import-macro- import-function -import-function- (fname falias &rest lambda-list))
(-import-macro- import-generic-function -import-generic-function- (fname falias &rest lambda-list))
(-import-macro- import-macro -import-macro- (fname falias &rest lambda-list))
(-import-macro- with-imported-variables -with-imported-variables- (bindings &body body))
(-import-macro- with-imported-symbol-macros -with-imported-symbol-macros- (bindings &body body))
(-import-macro- with-imported-functions -with-imported-functions- (bindings &body body))
(-import-macro- with-imported-macros -with-imported-macros- (bindings &body body))
(-import-macro- with-imported-block -with-imported-block- (binding &body body))
(-import-macro- defmacro -defmacro- (macro-name lambda-list &body body))

(defmacro lambda (&environment env lambda-list &body body)
  (multiple-value-bind
      (new-lambda-list new-body)
      (process-lambda env lambda-list body)
    `(cl:lambda ,new-lambda-list ,new-body)))

(defmacro defglobal (name &body form)
  (assert (cl:null (cl:cdr form)))
  (cl:let ((alias (global name)))
    `(cl:progn
       (defparameter ,alias ,(cl:car form))
       (import-variable ,name ,alias))))

(defmacro defconstant (name &body form)
  (assert (cl:null (cl:cdr form)))
  (cl:let ((alias (global name)))
    `(cl:progn
       (cl:defconstant ,alias ,(cl:car form))
       (import-variable ,name ,alias))))

(defmacro defun (&environment env name lambda-list &body body)
  (cl:let ((function-alias (global name)))
    (multiple-value-bind
        (new-lambda-list new-body)
        (process-lambda env lambda-list body)
      `(cl:progn
         (cl:defun ,function-alias ,new-lambda-list ,new-body)
         (import-function ,name ,function-alias ,lambda-list)))))

(defmacro defgeneric (&whole w &environment env name lambda-list &body options)
  (unless (every #'cl:consp options)
    (cl:error "Illegal options in DEFGENERIC form: ~S." w))
  (cl:let ((old-aliases (macroexpand '%aliases% env))
           (function-alias (global name))
           (new-lambda-list (parse-lambda-list lambda-list)))
    `(cl:progn
       (cl:defgeneric ,function-alias ,new-lambda-list
         ,@(loop for option in options
                 if (cl:eq (cl:car option) :method)
                 collect (loop for (element . body) on (cl:cdr option)
                               until (cl:listp element)
                               collect element into qualifiers
                               finally
                               (multiple-value-bind
                                   (new-lambda-list new-aliases)
                                   (parse-lambda-list element :specializers cl:t)
                                 (return
                                  `(:method ,@qualifiers ,new-lambda-list
                                    ,(expand-method-body qualifiers new-aliases old-aliases body)))))
                 else collect option))
       (import-function ,name ,function-alias ,lambda-list))))

(defmacro defmethod (&whole w &environment env name &body method-rest)
  (cl:let ((old-aliases (macroexpand '%aliases% env))
           (function-alias (global name)))
    (loop for (element . body) on method-rest
          until (cl:listp element)
          collect element into qualifiers
          finally
          (multiple-value-bind
              (new-lambda-list new-aliases)
              (parse-lambda-list element :specializers cl:t)
            (return 
             `(cl:progn
                (cl:unless (fboundp ',function-alias)
                  (cl:error "DEFMETHOD must be preceded by a DEFGENERIC: ~S." ',w))
                (cl:defmethod ,function-alias ,@qualifiers ,new-lambda-list
                  ,(expand-method-body qualifiers new-aliases old-aliases body))
                ',name))))))

(defmacro defclass (&whole w class-name superclasses &body options)
  (expand-defclass w class-name superclasses options))

(defmacro define-condition (&whole w class-name superclasses &body options)
  (expand-defclass w class-name superclasses options cl:t))

(defmacro let (bindings &body body)
  (loop for (var form) in bindings
        for alias = (copy-symbol var)
        collect `(,alias ,form) into new-bindings
        collect (cl:list var alias) into new-aliases
        finally (return `(cl:let ,new-bindings
                           (with-imported-variables ,new-aliases ,@body)))))

(defmacro let* (bindings &body body)
  (cl:if bindings
    `(let (,(cl:car bindings))
       (let* ,(cl:cdr bindings) ,@body))
    `(cl:progn ,@body)))

(defmacro flet (&environment env bindings &body body)
  (loop for (fun lambda-list . fun-body) in bindings
        for alias = (copy-symbol fun)
        collect (multiple-value-bind
                    (new-lambda-list new-body)
                    (process-lambda env lambda-list fun-body)
                  `(,alias ,new-lambda-list ,new-body)) into new-bindings
        collect (cl:list fun alias) into new-aliases
        finally (return `(cl:flet ,new-bindings
                           (with-imported-functions ,new-aliases ,@body)))))

(defmacro labels (&environment env bindings &body body)
  (loop with new-aliases = (loop for (fun) in bindings
                                 collect (cl:list fun (copy-symbol fun)))
        for (cl:nil lambda-list . fun-body) in bindings
        for (cl:nil alias) in new-aliases
        collect (multiple-value-bind
                    (new-lambda-list new-body)
                    (process-lambda env lambda-list fun-body)
                  `(,alias ,new-lambda-list (with-imported-functions ,new-aliases
                                              ,new-body)))
        into new-bindings
        finally (return `(cl:labels ,new-bindings
                           (with-imported-functions ,new-aliases
                             ,@body)))))

(defmacro block (&whole w name &body body)
  (unless (cl:symbolp name)
    (cl:error "Invalid block name ~S in ~S." name w))
  (cl:let ((alias (copy-symbol name)))
    `(cl:block ,name
       (with-imported-block (,name ,alias) ,@body))))

(defmacro return-from (&whole w name form)
  (unless (cl:symbolp name)
    (cl:error "Invalid block name ~S in ~S." name w))
  `(cl:return-from ,name ,form))

(defmacro catch (tag-form &body forms)
  `(cl:catch ,tag-form ,@forms))

(defmacro throw (tag-form result-form)
  `(cl:throw ,tag-form ,result-form))

(defmacro tagbody (&body body)
  `(cl:tagbody ,@body))

(defmacro go (tag)
  `(cl:go ,tag))

(defmacro unwind-protect (form &body cleanup-forms)
  `(cl:unwind-protect ,form ,@cleanup-forms))

(defmacro defdynamic (name &body form)
  (assert (cl:null (cl:cdr form)))
  `(cl:progn
     (cl:setf (symbol-value ',name) ,(cl:car form))
     ',name))

(defmacro dynamic (var)
  `(symbol-value ',var))

(defmacro set-dynamic (form var)
  `(cl:setf (symbol-value ',var) ,form))

(defmacro dynamic-let (bindings &body body)
  (assert (cl:and (cl:listp bindings) (every #'cl:null (cl:mapcar #'cl:cddr bindings))))
  `(progv
       ',(cl:mapcar #'cl:car bindings)
       (cl:list ,@(cl:mapcar #'cl:cadr bindings))
     ,@body))

(defmacro function (&environment env var)
  (cl:cond ((cl:symbolp var)
            (cl:let ((alias (get-alias var '%function-aliases% env)))
              (cl:if alias
                `(cl:function ,alias)
                `(cl:function ,var))))
           ((cl:and (cl:consp var) (cl:eq (cl:car var) 'lambda)) var)
           (cl:t `(cl:function ,var))))
(cl:defun sharp-bang (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  `(function ,(cl:read stream cl:t cl:nil cl:t)))
(set-dispatch-macro-character #\# #\! #'sharp-bang)

(import-function functionp cl:functionp (obj))
(import-function apply cl:apply (function &rest args))
(import-function funcall cl:funcall (function &rest args))

(import-variable t cl:t)
(import-variable nil cl:nil)

(import-function eq cl:eq (obj1 obj2))
(import-function eql cl:eql (obj1 obj2))

(defun equal (obj1 obj2)
  (cl:or (cl:equal obj1 obj2)
         (cl:if (cl:and (cl:stringp obj1) (cl:stringp obj2))
           (cl:string= obj1 obj2)
           (cl:equalp obj1 obj2))))

(import-function not cl:not (obj))
(import-macro and cl:and (&rest forms))
(import-macro or cl:or (&rest forms))

(defmacro quote (object) `(cl:quote ,object))

(defmacro setq (&rest forms) `(cl:setq ,@forms))
(import-macro setf cl:setf (&rest forms))

(defmacro if (test-form then-form &rest else-form)
  (assert (cl:or (cl:null else-form) (cl:null (cl:cdr else-form))))
  `(cl:if ,test-form ,then-form ,@else-form))

(import-macro cond cl:cond (&rest forms))

(defmacro case (keyform &body clauses)
  `(cl:case ,keyform ,@(loop for (key . forms) in clauses
                             if (cl:eq key 't) collect `(cl:t ,@forms)
                             else collect `(,key ,@forms))))

(defmacro case-using (predform keyform &body clauses)
  (rebinding (predform keyform)
    `(cl:cond ,@(loop for clause in clauses
                      if (cl:eq (cl:car clause) 't) collect clause
                      else collect `((find ,keyform ',(cl:car clause) :test ,predform)
                                     ,@(cl:cdr clause))))))

(defmacro progn (&body forms)
  `(cl:progn ,@forms))

(defmacro while (test-form &body body)
  `(loop while ,test-form do (cl:progn ,@body)))

(defmacro for (iteration-specs end-spec &body body)
  (cl:let ((block-name (cl:gensym))
           (tag (cl:gensym))
           (steps (loop for (var cl:nil . step) in iteration-specs
                        do (assert (cl:null (cl:cdr step)))
                        when step collect (list* var (copy-symbol var) step))))
    `(cl:block ,block-name
       (let ,(loop for (var init) in iteration-specs
                   collect `(,var ,init))
         (cl:tagbody
          ,tag
          (when ,(cl:car end-spec)
            (cl:return-from ,block-name (cl:progn ,@(cl:cdr end-spec))))
          (cl:progn ,@body
            (cl:let ,(loop for (cl:nil temp step) in steps
                           collect `(,temp ,step))
              (cl:setq ,@(loop for (var temp) in steps
                               nconc (cl:list var temp)))))
          (cl:go ,tag))))))

(defun generic-function-p (object)
  (typep object 'generic-function))

(define-method-combination nil ()
  ((primary () :required cl:t))
  `(call-method ,(first primary) ,(rest primary)))

(defgeneric create (class &rest initargs)
  (:method (class &rest initargs)
   (declare (dynamic-extent initargs))
   (cl:if (subtypep class (load-time-value (find-class 'condition)))
     (cl:apply #'make-condition class initargs)
     (cl:apply #'make-instance class initargs))))

(import-generic-function initialize-object initialize-instance (instance &rest args))
(import-function class-of cl:class-of (object))
(import-function instancep typep (obj class))
(import-function subclassp subtypep (class1 class2))

(defmacro class (name) `(find-class ',name))

(defmacro the (class-name form) `(cl:the ,class-name ,form))

(defmacro assure (class-name form)
  (rebinding (form)
    `(cl:progn
       (check-type ,form ,class-name)
       ,form)))

(cl:defgeneric convert-function (obj type)
  (:method ((obj character) (type (cl:eql '<character>))) obj)
  (:method ((obj character) (type (cl:eql '<integer>))) (char-code obj))
  (:method ((obj character) (type (cl:eql '<symbol>))) (intern (string obj)))
  
  (:method ((obj integer) (type (cl:eql '<character>))) (code-char obj))
  (:method ((obj integer) (type (cl:eql '<integer>))) obj)
  (:method ((obj integer) (type (cl:eql '<float>))) (coerce obj 'cl:float))
  (:method ((obj integer) (type (cl:eql '<string>))) (princ-to-string obj))

  (:method ((obj cl:float) (type (cl:eql '<float>))) obj)
  (:method ((obj cl:float) (type (cl:eql '<string>))) (princ-to-string obj))

  (:method ((obj symbol) (type (cl:eql '<symbol>))) obj)
  (:method ((obj symbol) (type (cl:eql '<string>))) (symbol-name obj))

  (:method ((obj string) (type (cl:eql '<integer>))) (parse-integer obj))
  (:method ((obj string) (type (cl:eql '<float>))) (assure cl:float (read-from-string obj)))
  (:method ((obj string) (type (cl:eql '<symbol>))) (intern obj))
  (:method ((obj string) (type (cl:eql '<string>))) obj)
  (:method ((obj string) (type (cl:eql '<general-vector>))) obj)
  (:method ((obj string) (type (cl:eql '<list>))) (coerce obj 'cl:list))

  (:method ((obj cl:vector) (type (cl:eql '<general-vector>))) obj)
  (:method ((obj cl:vector) (type (cl:eql '<list>))) (coerce obj 'cl:list))
  (:method ((obj cl:list) (type (cl:eql '<general-vector>))) (coerce obj 'cl:vector))
  (:method ((obj cl:list) (type (cl:eql '<list>))) obj))

(defmacro convert (obj class-name)
  `(convert-function ,obj ',class-name))

(import-function symbolp cl:symbolp (obj))
(import-function property get (symbol property-name &rest obj))
(defun set-property (value symbol indicator)
  (cl:setf (get symbol indicator) value))
(defun remove-property (symbol property-name)
  (prog1 (get symbol property-name)
    (remprop symbol property-name)))
(import-function gensym cl:gensym ())

(import-function numberp cl:numberp (obj))
(defun parse-number (string)
  (assure number (read-from-string string)))
(import-function = cl:= (x1 x2))
(import-function /= cl:/= (x1 x2))
(import-function >= cl:>= (x1 x2))
(import-function <= cl:<= (x1 x2))
(import-function > cl:> (x1 x2))
(import-function < cl:< (x1 x2))
(import-function + cl:+ (&rest args))
(import-function * cl:* (&rest args))
(import-function - cl:- (arg &rest args))
(import-function quotient cl:/ (dividend divisor &rest more-divisors))
(import-function reciprocal cl:/ (x))
(import-function max cl:max (number &rest more-numbers))
(import-function min cl:min (number &rest more-numbers))
(import-function abs cl:abs (number))
(import-function exp cl:exp (x))
(import-function log cl:log (x))
(import-function expt cl:expt (x1 x2))
(import-function sqrt cl:sqrt (x))
(import-variable *pi* pi)
(import-function sin cl:sin (x))
(import-function cos cl:cos (x))
(import-function tan cl:tan (x))
(import-function atan cl:atan (x))
(import-function atan2 cl:atan (x y))
(import-function sinh cl:sinh (x))
(import-function cosh cl:cosh (x))
(import-function tanh cl:tanh (x))
(import-function atanh cl:atanh (x))
(import-variable *most-positive-float* most-positive-long-float)
(import-variable *most-negative-float* most-negative-long-float)
(import-function floatp cl:floatp (x))
(import-function float cl:float (x))
(import-function floor cl:floor (x))
(import-function ceiling cl:ceiling (x))
(import-function truncate cl:truncate (x))
(import-function round cl:round (x))
(import-function integerp cl:integerp (x))
(import-function div cl:floor (z1 z2))
(import-function mod cl:mod (z1 z2))
(import-function gcd cl:gcd (z1 z2))
(import-function lcm cl:lcm (z1 z2))
(import-function isqrt cl:isqrt (z))

(import-function characterp cl:characterp (obj))
(import-function char= cl:char= (char1 char2))
(import-function char/= cl:char/= (char1 char2))
(import-function char< cl:char< (char1 char2))
(import-function char> cl:char> (char1 char2))
(import-function char<= cl:char<= (char1 char2))
(import-function char>= cl:char<= (char1 char2))

(import-function consp cl:consp (obj))
(import-function cons cl:cons (obj1 obj2))

(import-function car cl:car (cons))
(import-function cdr cl:cdr (cons))
(defun set-car (object cons)
  (cl:setf (cl:car cons) object))
(defun set-cdr (object cons)
  (cl:setf (cl:cdr cons) object))
(import-function caaaar cl:caaaar (cons))
(import-function caaadr cl:caaadr (cons))
(import-function caaar cl:caaar (cons))
(import-function caadar cl:caadar (cons))
(import-function caaddr cl:caaddr (cons))
(import-function caadr cl:caadr (cons))
(import-function caar cl:caar (cons))
(import-function cadaar cl:cadaar (cons))
(import-function cadadr cl:cadadr (cons))
(import-function cadar cl:cadar (cons))
(import-function caddar cl:caddar (cons))
(import-function cadddr cl:cadddr (cons))
(import-function caddr cl:caddr (cons))
(import-function cadr cl:cadr (cons))
(import-function cdaaar cl:cdaaar (cons))
(import-function cdaadr cl:cdaadr (cons))
(import-function cdaar cl:cdaar (cons))
(import-function cdadar cl:cdadar (cons))
(import-function cdaddr cl:cdaddr (cons))
(import-function cdadr cl:cdadr (cons))
(import-function cdar cl:cdar (cons))
(import-function cddaar cl:cddaar (cons))
(import-function cddadr cl:cddadr (cons))
(import-function cddar cl:cddar (cons))
(import-function cdddar cl:cdddar (cons))
(import-function cddddr cl:cddddr (cons))
(import-function cdddr cl:cdddr (cons))
(import-function cddr cl:cddr (cons))

(import-function null cl:null (obj))

(import-function listp cl:listp (obj))
(defun create-list (i &rest initial-element)
  (cl:if initial-element
    (make-list i :initial-element (cl:car initial-element))
    (make-list i)))
(import-function list cl:list (&rest objects))
(import-function reverse cl:reverse (list))
(import-function nreverse cl:nreverse (list))
(import-function append cl:append (&rest lists))
(import-function member cl:member (obj list))
(import-function mapcar cl:mapcar (function list &rest more-lists))
(import-function mapc cl:mapc (function list &rest more-lists))
(import-function mapcan cl:mapcan (function list &rest more-lists))
(import-function maplist cl:maplist (function list &rest more-lists))
(import-function mapl cl:mapl (function list &rest more-lists))
(import-function mapcon cl:mapcon (function list &rest more-lists))
(import-function assoc cl:assoc (obj assoc-list))

(import-function basic-arrayp-p arrayp (obj))
(defun basic-array*-p (obj)
  (cl:and (arrayp obj) (cl:> (array-rank obj) 1)))
(defun general-array*-p (obj)
  (cl:and (arrayp obj) (cl:> (array-rank obj) 1)))
(defun create-array (dimensions &rest initial-element)
  (cl:if initial-element
    (make-array dimensions :initial-element (cl:car initial-element))
    (make-array dimensions)))
(import-function aref cl:aref (array &rest indexes))
(import-function garef cl:aref (array &rest indexes))
(defun set-aref (obj array &rest indexes)
  (cl:setf (cl:apply #'cl:aref array indexes) obj))
(defun set-garef (obj array &rest indexes)
  (cl:setf (cl:apply #'cl:aref array indexes) obj))
(import-function array-dimensions cl:array-dimensions (array))

(import-function basic-vector-p vectorp (obj))
(import-function general-vector-p vectorp (obj))
(import-function create-vector create-array (i &rest initial-element))
(import-function vector cl:vector (&rest objects))

(import-function stringp cl:stringp (obj))
(defun create-string (i &rest initial-element)
  (cl:if initial-element
    (make-string i :initial-element (cl:car initial-element))
    (make-string i)))
(import-function string= cl:string= (s1 s2))
(import-function string/= cl:string/= (s1 s2))
(import-function string< cl:string< (s1 s2))
(import-function string> cl:string> (s1 s2))
(import-function string>= cl:string>= (s1 s2))
(import-function string<= cl:string<= (s1 s2))
(defun char-index (char string &rest start-position)
  (cl:if start-position
    (position char string :start (cl:car start-position))
    (position char string)))
(defun string-index (substring string &rest start-position)
  (cl:if start-position
    (search substring string :start2 (cl:car start-position))
    (search substring string)))
(defun string-append (&rest strings)
  (cl:apply #'concatenate 'string strings))

(import-function length cl:length (sequence))
(import-function elt cl:elt (sequence z))
(defun set-elt (obj sequence z)
  (cl:setf (cl:elt sequence z) obj))
(import-function subseq cl:subseq (sequence z1 z2))
(import-function map-into cl:map-into (destination function &rest sequences))

(import-function streamp cl:streamp (obj))
(import-function open-stream-p cl:open-stream-p (obj))
(import-function input-stream-p cl:input-stream-p (obj))
(import-function output-stream-p cl:output-stream-p (obj))
(defun standard-input () *standard-input*)
(defun standard-output () *standard-output*)
(defun error-output () *error-output*)

(defmacro with-standard-input (stream-form &body body)
  `(cl:let ((*standard-input* ,stream-form)) ,@body))

(defmacro with-standard-output (stream-form &body body)
  `(cl:let ((*standard-output* ,stream-form)) ,@body))

(defmacro with-error-output (stream-form &body body)
  `(cl:let ((*error-output* ,stream-form)) ,@body))

(cl:defun element-type (element-class)
  (cl:if element-class
    (cl:let ((element-type (cl:car element-class)))
      (cl:if (cl:numberp element-type)
        `(unsigned-byte ,element-type)
        element-type))
    'character))

(defun open-input-file (filename &rest element-class)
  (open filename :direction :input :element-type (element-type element-class)))

(defun open-output-file (filename &rest element-class)
  (open filename :direction :output :element-type (element-type element-class)))

(defun open-io-file (filename &rest element-class)
  (open filename :direction :io :element-type (element-type element-class)))

(defmacro with-open-input-file (spec &body body)
  (destructuring-bind (name filename &rest element-class) spec
    (cl:let ((alias (copy-symbol name)))
      `(with-open-file ,(cl:if element-class
                          `(,alias ,filename
                                   :direction :input
                                   :element-type ,(cl:car element-class))
                          `(,alias ,filename :direction :input))
         (with-imported-variables ((,name ,alias))
           ,@body)))))

(defmacro with-open-output-file (spec &body body)
  (destructuring-bind (name filename &rest element-class) spec
    (cl:let ((alias (copy-symbol name)))
      `(with-open-file ,(cl:if element-class
                          `(,alias ,filename
                                   :direction :output
                                   :element-type ,(cl:car element-class))
                          `(,alias ,filename :direction :output))
         (with-imported-variables ((,name ,alias))
           ,@body)))))

(defmacro with-open-io-file (spec &body body)
  (destructuring-bind (name filename &rest element-class) spec
    (cl:let ((alias (copy-symbol name)))
      `(with-open-file ,(cl:if element-class
                          `(,alias ,filename
                                   :direction :io
                                   :element-type ,(cl:car element-class))
                          `(,alias ,filename :direction :io))
         (with-imported-variables ((,name ,alias))
           ,@body)))))

(import-function close cl:close (stream))
(import-function finish-output cl:finish-output (stream))

(import-function create-string-input-stream make-string-input-stream (string))
(import-function create-string-output-stream make-string-output-stream ())
(import-function get-output-stream-string cl:get-output-stream-string (stream))

(import-function read cl:read (&rest args))
(import-function read-char cl:read-char (&rest args))
(defun preview-char (&rest args)
  (cl:apply #'peek-char cl:nil args))
(import-function read-line cl:read-line (&rest args))
(import-function stream-ready-p listen (input-stream))

(import-function format cl:format (output-stream format-string &rest args))
(defun format-char (output-stream char)
  (write-char char output-stream))
(defun format-float (output-stream float)
  (cl:format output-stream "~G" float))
(import-function format-fresh-line fresh-line (output-stream))
(defun format-integer (output-stream integer radix)
  (write integer :stream output-stream :base radix))
(defun format-object (output-stream obj escape-p)
  (write obj :stream output-stream :escape escape-p))
(defun format-tab (output-stream column)
  (pprint-tab :line column 1 output-stream))

(import-function read-byte cl:read-byte (input-stream &rest args))
(import-function write-byte cl:write-byte (z output-stream))

(import-function probe-file cl:probe-file (filename))
(import-function file-position cl:file-position (stream))
(import-function set-file-position cl:file-position (stream z))
(defun file-length (filename element-class)
  (with-open-input-file (stream filename element-class) (cl:file-length stream)))

(import-function error cl:error (error-string &rest args))

(defvar *condition-continuables* '())

(cl:defun -signal-condition- (condition continuable)
  (cl:if continuable
    (cl:let* ((continuable
               (cl:if (cl:eq continuable 'cl:t)
                 "Continue with no special action."
                 continuable))
              (*condition-continuables*
               (acons condition continuable
                      *condition-continuables*)))
      (restart-case (signal condition)
        (continue (value)
                  :report (cl:lambda (stream) (cl:format stream continuable))
                  value)))
    (signal condition)))

(import-function signal-condition -signal-condition- (condition continuable))

(defun cerror (continue-string error-string &rest args)
  (-signal-condition-
   (make-instance '<simple-error>
                  :format-string error-string
                  :format-arguments args)
   (cl:apply #'cl:format cl:nil continue-string args)))

(import-macro ignore-errors cl:ignore-errors (&rest forms))

(defun condition-continuable (condition)
  (cl:cdr (cl:assoc condition *condition-continuables*)))

(defun continue-condition (condition &rest value)
  (cl:let ((restart (find-restart 'continue condition)))
    (cl:apply #'invoke-restart restart value)))

(defmacro with-handler (handler &body body)
  `(handler-bind ((cl:t ,handler)) ,@body))

(import-function arithmetic-error-operation cl:arithmetic-error-operation (error))
(import-function arithmetic-error-operands cl:arithmetic-error-operands (error))
(import-function domain-error-object type-error-datum (error))
(import-function domain-error-expected-class type-error-expected-type (error))
(import-function simple-error-format-string simple-condition-format-control (error))
(import-function simple-error-format-arguments simple-condition-format-arguments (error))
(import-function stream-error-stream cl:stream-error-stream (error))
(import-function undefined-entity-name cell-error-name (error))

(import-function identity cl:identity (obj))

(import-function get-universal-time cl:get-universal-time ())
(import-function get-internal-run-time cl:get-internal-run-time ())
(import-function get-internal-real-time cl:get-internal-real-time ())
(defun internal-time-units-per-second () cl:internal-time-units-per-second)
