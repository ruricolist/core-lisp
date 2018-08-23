This is a fork of Pascal Costanza’s [Core Lisp][], a
hygiene-compatible Lisp dialect. It has been forked to work
with [Vernacular][], an experimental module system for Common
Lisp. It serves to show that Vernacular can, in fact, be used to build
Racket-style language towers, despite Common Lisp’s lack of hygiene,
“simply” by embedding a hygiene-compatible language within Common
Lisp.

Core Lisp is an implementation of [ISLISP][], a standardized Lisp
dialect “culturally compatible” with Common Lisp. That is, while it is
not an exact subset of Common Lisp, it contains no features that make
it unsuitable for implementation in Common Lisp.

Core Lisp provides a degree of hygiene equivalent to that provided
by [explicit renaming][] or [syntactic closures][]. That’s a lot of
hygiene, but not all the hygiene. You might want to read
*[Towards the Essence of Hygiene][essence]*, to understand where this
approach falls short. (This approach being the one the author terms
“binder renaming with gensym”.)

[ISLISP]: http://islisp.info/
[Overlord]: http://github.com/ruricolist/overlord
[Vernacular]: http://github.com/ruricolist/vernacular
[Core Lisp]: http://www.p-cos.net/core-lisp.html
[syntactic closures]: https://en.wikipedia.org/wiki/Syntactic_closure
[explicit renaming]: http://dl.acm.org/citation.cfm?id=1317269
[essence]: http://michaeldadams.org/papers/hygiene/
