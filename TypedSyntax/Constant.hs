module TypedSyntax.Constant where
  import Syntax.Constant
  import Types

  import Utils.Iseq

  type TypedConstant = (Constant, Type)

  pprTypedConstant :: TypedConstant -> Iseq
  pprTypedConstant (c, t) = iConcat [ pprConstant c, iStr " : ", pprType t ]
