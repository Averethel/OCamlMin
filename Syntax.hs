module Syntax (
  CaseClause(..),
  Constant(..),
  Constructor(..),
  UnaryPrim(..),
  BinaryPrim(..),
  Pattern(..),
  FunClause(..),
  Expr(..),
  arity,
  constructors
) where
  import Syntax.Constant
  import Syntax.Constructor
  import Syntax.UnaryPrim
  import Syntax.BinaryPrim
  import Syntax.Pattern
  import Syntax.Expr
