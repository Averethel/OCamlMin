OCamlMin
========

Implementation of OCamlMin language based on min-caml project.

Changelog
=========
* 5 VII 2013
  * Rewritten register allocation to use x86 assembly
* 22 VI 2013
  * Switched to x86 assembly instead of SPARC
* 11 V 2013
  * Initial version of code emitter
* 10 V 2013
  * Added Virtual and Concrete SPARC assembly syntax
* 3 V 2013
  * Initial version of register allocation
* 26 IV 2013
  * Initial version of virtual machine code generation
  * 13-bit immidiate optimization
* 7 IV 2013
  * Minor bugfixes in K-Normalization
  * Restricted equality to work only on primitive types
* 31 III 2013
  * α-conversion on typed AST
  * β-reduction on typed AST
  * Nested let flattening on typed AST
  * Inlining on typed AST
  * Constant folding on typed AST
  * Eliminating unused definitions on typed AST
  * Closure conversion on typed AST
* 29 III 2013
  * K-Normalized syntax is now type annotated
  * K-Normalization works on typed AST
* 24 III 2013
  * Added typed abstract syntax tree
  * Type inference now generated typed AST instead of just type
  * Pattern matching compiler works on typed ASTs
* 17 III 2013
  * SPARC assembly syntax defined
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
