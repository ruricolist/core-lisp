(in-package :core-lisp)

(cl:defmacro with-unique-names (names &body body)
  `(let ,(loop for name in names
               collect `(,name (cl:gensym ,(symbol-name name))))
     ,@body))

(cl:defmacro rebinding (vars &body body)
  (loop for var in vars
        for name = (cl:gensym (symbol-name var))
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                           (with-unique-names ,vars
                             `(let (,,@temps) ,,@body))))))

(defvar *core-lisp-global-package*
  (find-package "CORE-LISP-GLOBAL-DEFINITIONS"))

(cl:defgeneric global (name)
  (:method ((symbol cl:symbol))
   (intern (cl:format cl:nil "~A::~S"
                      (package-name (symbol-package symbol))
                      symbol)
           *core-lisp-global-package*))
  (:method ((name cl:cons))
   `(cl:setf ,(global (cl:cadr name)))))

(cl:setf (find-class '<object>)                    (find-class 'cl:t)
         (find-class '<basic-array>)               (find-class 'array)
         (find-class '<basic-vector>)              (find-class 'cl:vector)
         (find-class '<general-vector>)            (find-class 'cl:vector)
         (find-class '<string>)                    (find-class 'string)
         (find-class '<built-in-class>)            (find-class 'built-in-class)
         (find-class '<character>)                 (find-class 'character)
         (find-class '<function>)                  (find-class 'cl:function)
         (find-class '<generic-function>)          (find-class 'generic-function)
         (find-class '<standard-generic-function>) (find-class 'standard-generic-function)
         (find-class '<list>)                      (find-class 'cl:list)
         (find-class '<cons>)                      (find-class 'cl:cons)
         (find-class '<null>)                      (find-class 'cl:null)
         (find-class '<symbol>)                    (find-class 'symbol)
         (find-class '<number>)                    (find-class 'number)
         (find-class '<float>)                     (find-class 'cl:float)
         (find-class '<integer>)                   (find-class 'integer)
         (find-class '<serious-condition>)         (find-class 'serious-condition)
         (find-class '<error>)                     (find-class 'cl:error)
         (find-class '<arithmetic-error>)          (find-class 'arithmetic-error)
         (find-class '<division-by-zero>)          (find-class 'division-by-zero)
         (find-class '<floating-point-overflow>)   (find-class 'floating-point-overflow)
         (find-class '<floating-point-underflow>)  (find-class 'floating-point-underflow)
         (find-class '<control-error>)             (find-class 'control-error)
         (find-class '<parse-error>)               (find-class 'parse-error)
         (find-class '<program-error>)             (find-class 'program-error)
         (find-class '<domain-error>)              (find-class 'type-error)
         (find-class '<undefined-entity>)          (find-class 'cell-error)
         (find-class '<unbound-variable>)          (find-class 'unbound-variable)
         (find-class '<undefined-function>)        (find-class 'undefined-function)
         (find-class '<simple-error>)              (find-class 'simple-error)
         (find-class '<stream-error>)              (find-class 'stream-error)
         (find-class '<end-of-stream>)             (find-class 'end-of-file)
         (find-class '<storage-exhausted>)         (find-class 'storage-condition)
         (find-class '<standard-class>)            (find-class 'standard-class)
         (find-class '<standard-object>)           (find-class 'standard-object)
         (find-class '<stream>)                    (find-class 'stream))

(defvar *core-lisp-lambda-list-keywords*
  '(&body &environment &rest &whole))

(defvar *lambda-list-keyword-map*
  (loop for keyword in *core-lisp-lambda-list-keywords*
        collect (cl:cons (intern (cl:subseq (symbol-name keyword) 1) :keyword) keyword)))

(defvar *error-ll*)

(cl:defun ll-keywordp (symbol)
  (cl:let ((symbol-name (symbol-name symbol)))
    (cl:eql (when (cl:> (cl:length symbol-name) 0)
              (cl:aref symbol-name 0))
            #\&)))

(cl:defun ll-element-err (element)
  (cl:error "Unexpected element ~S in lambda list ~S." element *error-ll*))

(cl:defun ll-end-err (&optional tail)
  (cl:if tail
    (cl:error "Unexpected tail ~S in lambda-list ~S." tail *error-ll*)
    (cl:error "Unexpected NIL / end in lambda list ~S." *error-ll*)))

(cl:defun ll-duplicate-err (var)
  (cl:error "Duplicate variable ~S in lambda list ~S." var *error-ll*))

(cl:defun ll-keyword (keyword)
  (cl:if (cl:and (cl:symbolp keyword) (keywordp keyword))
    (cl:let ((ll-keyword (cl:cdr (cl:assoc keyword *lambda-list-keyword-map*))))
      (cl:if ll-keyword ll-keyword (ll-element-err keyword)))
    keyword))

(cl:defun symbol-not-null (element)
  (cl:if (cl:and element (cl:symbolp element)) element
    (ll-element-err element)))

(cl:defun var-symbol (element)
  (cl:if element
    (cl:if (cl:and (cl:symbolp element)
                   (cl:not (keywordp element))
                   (cl:not (ll-keywordp element)))
      element (ll-element-err element))
    (ll-end-err)))

(cl:defun gf-name (name)
  (cl:if (cl:consp name)
    (cl:cond ((cl:eq (cl:car name) 'cl:setf) name)
             ((cl:eq (cl:car name) 'setf) `(cl:setf ,(cl:cadr name)))
             (cl:t (cl:error "Illegal function name ~S." name)))
    name))

(cl:defun check-specializer (element)
  (cl:if (cl:and (cl:car element)
                 (cl:symbolp (cl:car element))
                 (cl:not (keywordp (cl:car element)))
                 (cl:not (ll-keywordp (cl:car element)))
                 (cl:null (cl:cddr element)))
    element (ll-element-err element)))

(cl:defun check-duplicate (var aliases)
  (when (cl:member var aliases :key #'cl:car)
    (ll-duplicate-err var)))

(cl:defun parse-element (rest)
  (unless (cl:listp rest) (ll-end-err rest))
  (cl:if (cl:null rest) '()
    (cl:let ((element (cl:car rest)))
      (cl:if (cl:consp element)
        (cl:list 'specializer
                 (check-specializer element)
                 (cl:cdr rest))
        (cl:let ((element (symbol-not-null element)))
          (cl:if (cl:or (keywordp element) (ll-keywordp element))
            (cl:list element
                     (var-symbol (cl:cadr rest))
                     (cl:cddr rest))
            (cl:list 'var element (cl:cdr rest))))))))

(cl:defun parse-lambda-list (ll &key ((:error-ll *error-ll*) ll) (specializers cl:nil) (rest-keys '(&rest)))
  (loop for (kind var-spec rest) = (parse-element ll) then (parse-element rest)
        for var = (cl:if (cl:eq kind 'specializer)
                    (cl:if specializers (cl:car var-spec)
                      (ll-element-err var))
                    var-spec)
        for alias = (when kind (copy-symbol var))
        while kind do (check-duplicate var aliases)
        unless (cl:member kind '(var specializer)) collect (ll-keyword kind) into new-lambda-list end
        if (cl:eq kind 'specializer)
        collect (cl:cons alias (cl:cdr var-spec)) into new-lambda-list
        and collect (cl:list var alias) into aliases
        else
        collect alias into new-lambda-list
        and collect (cl:list var alias) into aliases
        while (cl:member kind '(var specializer))
        finally
        (when kind
          (unless (cl:member (ll-keyword kind) rest-keys)
            (ll-element-err kind))
          (when rest (ll-end-err rest)))
        (return (values new-lambda-list aliases))))

(cl:defun parse-macro-lambda-list (ll &aux (*error-ll* ll))
  (cl:let ((rest-ll ll)
           (element (parse-element ll))
           whole-var whole-alias env-var env-alias)
    (when element
      (destructuring-bind (kind var rest) element
        (when (cl:member kind '(:whole &whole))
          (cl:setq rest-ll rest
                   whole-var var
                   whole-alias (copy-symbol var)
                   element (parse-element rest))))
      (when element
        (destructuring-bind (kind var rest) element
          (when (cl:member kind '(:environment &environment))
            (cl:setq rest-ll rest
                     env-var var
                     env-alias (copy-symbol var)
                     element (parse-element rest))))
        (when (cl:and (cl:or whole-var env-var) (cl:eq whole-var env-var))
          (ll-duplicate-err whole-var))
        (multiple-value-bind
            (new-lambda-list aliases)
            (parse-lambda-list rest-ll :error-ll ll :rest-keys '(&rest &body))
          (check-duplicate env-var aliases)
          (check-duplicate whole-var aliases)
          (values
           (nconc (when whole-var (cl:list '&whole whole-alias))
                  (when env-var (cl:list '&environment env-alias))
                  new-lambda-list)
           (nconc (when whole-var (cl:list (cl:list whole-var whole-alias)))
                  (when env-var (cl:list (cl:list env-var env-alias)))
                  aliases)))))))

(cl:defun err-illegal-slot-spec (slot-spec w)
  (cl:error "Invalid slot spec ~S in ~S." slot-spec w))

(cl:defun parse-slot-spec (w slot-spec)
  (cl:cond ((cl:null slot-spec) (err-illegal-slot-spec slot-spec w))
           ((cl:symbolp slot-spec) (cl:list slot-spec '() '()))
           ((cl:consp slot-spec)
            (unless (cl:and (cl:car slot-spec) (cl:symbolp (cl:car slot-spec)))
              (err-illegal-slot-spec slot-spec w))
            (loop for (key value) on (cl:cdr slot-spec) by #'cl:cddr
                  nconc (cl:if (cl:member key '(:accessor :reader :writer))
                          `(,key ,(global value))
                          `(,key ,value)) into new-slot-spec
                  when (cl:member key '(:accessor :reader))
                  collect value into readers
                  when (cl:member key '(:accessor :writer))
                  collect (cl:if (cl:eq key :accessor)
                            `(cl:setf ,value) (gf-name value)) into writers
                  finally (return (cl:list `(,(cl:car slot-spec) ,@new-slot-spec) readers writers))))
           (cl:t (err-illegal-slot-spec slot-spec w))))

(cl:defmacro -import-variable- (name alias)
  `(cl:progn
     (define-symbol-macro ,name ,alias)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:setf (get ',name '%aliases%) ',alias)
       ',name)))

(cl:defmacro -import-symbol-macro- (name alias)
  `(cl:progn
     (define-symbol-macro ,name ,alias)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (cl:setf (get ',name '%aliases%) ',alias)
       ',name)))

(cl:defmacro -import-function- (fname falias &optional (lambda-list '() lambda-list-p))
  (assert (cl:or (cl:and (cl:symbolp fname) (cl:symbolp falias))
                 (cl:and (cl:consp fname) (cl:consp falias))))
  (cl:let ((name (cl:if (cl:symbolp fname) fname (cl:cadr fname)))
           (alias (cl:if (cl:symbolp falias) falias (cl:cadr falias))))
    `(cl:progn
       ,(cl:if lambda-list-p
          (loop for (element . rest) on lambda-list
                for restp = (cl:member element '(:rest &rest))
                until restp
                collect element into required-vars
                finally (return
                         (cl:if restp
                           (cl:let ((rest-var (cl:car rest)))
                             (cl:if (cl:symbolp fname)
                               `(cl:defmacro ,fname (,@required-vars &rest ,rest-var)
                                  `(,',falias ,,@required-vars ,@,rest-var))
                               `(defsetf ,name (,@(cl:cdr required-vars) &rest ,rest-var)
                                         (,(cl:car required-vars))
                                  `(cl:setf (,',alias ,,@(cl:cdr required-vars) ,@,rest-var)
                                            ,,(cl:car required-vars)))))
                           (cl:if (cl:symbolp fname)
                             `(cl:defmacro ,fname (,@lambda-list)
                                `(,',falias ,,@lambda-list))
                             `(defsetf ,name (,@(cl:cdr lambda-list)) (,(cl:car lambda-list))
                                `(cl:setf (,',alias ,,@(cl:cdr lambda-list))
                                          ,,(cl:car lambda-list)))))))
          (cl:let ((rest-var (cl:gensym)))
            (cl:if (cl:symbolp fname)
              `(cl:defmacro ,fname (&rest ,rest-var)
                 `(,',falias ,@,rest-var))
              (cl:let ((store-var (cl:gensym)))
                `(defsetf ,fname (&rest ,rest-var) (,store-var)
                   `(cl:setf (,',alias ,@,rest-var) ,,store-var))))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (cl:setf (get ',name '%function-aliases%) ',alias))
       ',fname)))

(cl:defmacro -import-generic-function- (fname falias &optional (lambda-list '() lambda-list-p))
  (assert (cl:or (cl:and (cl:symbolp fname) (cl:symbolp falias))
                 (cl:and (cl:consp fname) (cl:consp falias))))
  (cl:let ((name (cl:if (cl:symbolp fname) fname (cl:cadr fname)))
           (alias (cl:if (cl:symbolp falias) falias (cl:cadr falias))))
    `(cl:progn
       ,(cl:if lambda-list-p
          `(defgeneric ,fname (,@lambda-list))
          `(defgeneric ,fname (&rest ,(cl:gensym))))
       (cl:setf (cl:fdefinition ',(global fname)) (cl:fdefinition ',falias))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (cl:setf (get ',name '%function-aliases%) ',alias))
       ',fname)))

(cl:defmacro -import-macro- (fname falias &optional (lambda-list '() lambda-list-p))
  (cl:if lambda-list-p
    (loop with whole-var
          with env-var
          with body-var
          with rest-var
          for (elm1 elm2) on lambda-list
          for element = (ll-keyword elm1)
          if (cl:eq element '&whole) do (cl:setq whole-var elm2)
          else if (cl:eq element '&environment) do (cl:setq env-var elm2)
          else if (cl:eq element '&body) do (cl:setq body-var elm2)
          else if (cl:eq element '&rest) do (cl:setq rest-var elm2)
          else unless (cl:member element (cl:list whole-var env-var body-var rest-var))
          collect element into required-vars
          finally
          (cl:let ((new-whole-var (cl:if whole-var whole-var 'whole-form))
                   (new-env-var (cl:if env-var env-var 'environment)))
            (return
             `(cl:progn
                (cl:defmacro ,fname (&whole ,new-whole-var &environment ,new-env-var
                                            ,@required-vars
                                            ,@(when body-var `(&body ,body-var))
                                            ,@(when rest-var `(&rest ,rest-var)))
                  (declare (ignore ,@required-vars
                                   ,@(when body-var (cl:list body-var))
                                   ,@(when rest-var (cl:list rest-var))))
                  (cl:funcall (macro-function ',falias ,new-env-var) ,new-whole-var ,new-env-var))
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (cl:setf (get ',fname '%function-aliases%) ',falias))
                ',fname))))
    (cl:let ((whole-var (cl:gensym)) (env-var (cl:gensym)) (body-var (cl:gensym)))
      `(cl:progn
         (cl:defmacro ,fname (&whole ,whole-var &environment ,env-var &body ,body-var)
           (declare (ignore ,body-var))
           (cl:funcall (macro-function ',falias ,env-var) ,whole-var ,env-var))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (cl:setf (get ',fname '%function-aliases%) ',falias)
           ',fname)))))

(cl:defmacro -with-imported-variables- ((&rest bindings) &body body &environment env)
  (cl:let ((old-aliases (macroexpand '%aliases% env)))
    `(symbol-macrolet ((%aliases% ,(revappend bindings old-aliases)))
       (declare (ignorable %aliases%))
       (symbol-macrolet ,bindings ,@body))))

(cl:defmacro -with-imported-symbol-macros- ((&rest bindings) &body body &environment env)
  (cl:let ((old-aliases (macroexpand '%aliases% env)))
    `(symbol-macrolet ((%aliases% ,(revappend bindings old-aliases)))
       (declare (ignorable %aliases%))
       (symbol-macrolet ,bindings ,@body))))

(cl:defmacro -with-imported-functions- ((&rest bindings) &body body &environment env)
  (cl:let ((old-aliases (macroexpand '%function-aliases% env)))
    `(symbol-macrolet ((%function-aliases% ,(revappend bindings old-aliases)))
       (declare (ignorable %function-aliases%))
       (cl:macrolet ,(loop for (fun alias) in bindings
                           collect `(,fun (&rest args) `(,',alias ,@args)))
         ,@body))))

(cl:defvar -arguments-for-local-macros-)

(cl:defmacro expand-local-macro (name &environment env)
  `',(macroexpand `(,name ,@-arguments-for-local-macros-) env))

(cl:defmacro -with-imported-macros- ((&rest bindings) &body body &environment env)
  (cl:let ((old-aliases (macroexpand '%function-aliases% env)))
    `(symbol-macrolet ((%function-aliases% ,(revappend bindings old-aliases)))
       (declare (ignorable %function-aliases%))
       (cl:macrolet ,(loop for (macro alias) in bindings
                           collect `(,macro (&rest -arguments-for-local-macros-) (expand-local-macro ,alias)))
         ,@body))))

(cl:defmacro -with-imported-block- ((block-name block-alias) &body body &environment env)
  (cl:let ((old-aliases (macroexpand '%block-aliases% env)))
    `(cl:block ,block-name
       (symbol-macrolet ((%block-aliases% ,(cl:cons `(,block-name ,block-alias) old-aliases)))
         ,@body))))

(cl:defmacro -defmacro- (macro-name lambda-list &body body)
  (cl:let ((macro-alias (global macro-name)))
    (multiple-value-bind
        (new-lambda-list new-aliases)
        (parse-macro-lambda-list lambda-list)
      `(cl:progn
         (cl:defmacro ,macro-alias ,new-lambda-list
           (-with-imported-variables- ,new-aliases ,@body))
         (-import-macro- ,macro-name ,macro-alias ,lambda-list)))))

(cl:defun expand-body (new-aliases old-aliases body)
  `(symbol-macrolet ((%aliases% ,(revappend new-aliases old-aliases)))
     (declare (ignorable %aliases%))
     (symbol-macrolet ,new-aliases ,@body)))

(cl:defun process-lambda (env lambda-list body)
  (cl:let ((old-aliases (macroexpand '%aliases% env)))
    (multiple-value-bind
        (new-lambda-list new-aliases)
        (parse-lambda-list lambda-list)
      (values new-lambda-list (expand-body new-aliases old-aliases body)))))

(cl:defun expand-method-body (qualifiers new-aliases old-aliases body)
  (cl:let ((form (expand-body new-aliases old-aliases body)))
    (cl:if (cl:or (cl:null qualifiers) (cl:member :around qualifiers))
      `(-with-imported-functions- ((call-next-method cl:call-next-method)
                                   (next-method-p cl:next-method-p))
         ,form)
      form)))

(cl:defun expand-defclass (w class-name superclasses options &optional (conditionp cl:nil))
  (unless (cl:and (cl:symbolp class-name)
                  (cl:listp superclasses)
                  (every #'cl:symbolp superclasses)
                  options
                  (cl:listp (cl:car options))
                  (cl:listp (cl:cdr options))
                  (loop for option in (cl:cdr options)
                        always (cl:and (cl:consp option)
                                       (cl:car option)
                                       (cl:symbolp (cl:car option)))))
    (cl:error "Malformed definition: ~S." w))
  (loop for slot-spec in (cl:car options)
        for (new-slot-spec readers writers) = (parse-slot-spec w slot-spec)
        collect new-slot-spec into new-slot-specs
        nconc readers into reader-gfs
        nconc writers into writer-gfs
        finally (return `(cl:progn
                           ,@(loop for gf in reader-gfs
                                   collect `(defgeneric ,gf (object)))
                           ,@(loop for gf in writer-gfs
                                   collect `(defgeneric ,gf (new-value object)))
                           (,(cl:if conditionp 'cl:define-condition 'cl:defclass)
                            ,class-name ,superclasses ,new-slot-specs ,@(cl:cdr options))))))
