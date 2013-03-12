OCamlMin
========

Implementation of OCamlMin language based on min-caml project.

Changelog
=========
* 11 III 2013
  * Closure conversion module
* 23 II 2013
  * Constant folding
  * Unused definitions elimination
  * Syntax for closure converted expressions
* 19 II 2013
  * β-reduction
  * nested let expressions flattening
  * small functions inlining
* 18 II 2013
  * α-conversion
* 15 II 2013
  * Conversion to K-Normal form
* 10 II 2013
  * Added simple case expressions to language
  * Added fail / handle expressions to language
  * Compilation of pattern matching finished
* 9 II 2013
  * Switched to pairs instead of tuples (to simplify compilation of pattern matching)
  * Added guards to function clauses (needed for pattern matching compilation)
* 1 II 2013
  * Added type inference
* 31 I 2013
  * Syntax of type expressions
* 30 I 2013
  * Syntax of expressions
