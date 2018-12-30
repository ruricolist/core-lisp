(asdf:defsystem #:core-lisp
  :name "Core Lisp"
  :author "Pascal Costanza"
  :description "Hygiene-compatible Lisp dialect embedded in CL."
  :maintainer "Paul M. Rodriguez <pmr@ruricolist.com>"
  :licence "
Copyright (c) 2009 Pascal Costanza
Copyright (c) 2017 Paul M. Rodriguez

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
"
  :components
  ((:file "core-lisp-packages")
   (:file "core-lisp-boot" :depends-on ("core-lisp-packages"))
   (:file "core-lisp" :depends-on ("core-lisp-boot"))
   (:file "readtable" :depends-on ("core-lisp-packages"))
   (:file "lang" :depends-on ("core-lisp")))
  :in-order-to ((asdf:test-op (asdf:test-op #:core-lisp/test)))
  :depends-on (:vernacular :global-vars :alexandria :named-readtables))

(asdf:defsystem #:core-lisp/test
  :depends-on (:core-lisp :overlord :uiop :fiveam)
  :perform (asdf:test-op (o c) (uiop:symbol-call :core-lisp/test :run-tests))
  :components
  ((:file "test")))
