module Syntax (
  Constant(..),
  Constructor(..),
  UnaryPrim(..),
  BinaryPrim(..),
  Pattern(..),
  FunClause(..),
  CaseClause(..),
  Expr(..),
  arity
) where
  import Syntax.Constant
  import Syntax.Constructor
  import Syntax.UnaryPrim
  import Syntax.BinaryPrim
  import Syntax.Pattern
  import Syntax.Expr
