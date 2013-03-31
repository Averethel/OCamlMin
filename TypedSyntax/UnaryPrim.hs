module TypedSyntax.UnaryPrim where
  import Syntax.UnaryPrim
  import Types

  import Utils.Iseq

  type TypedUnaryPrim = (UnaryPrim, Type)

  pprTypedUnaryPrim :: TypedUnaryPrim -> Iseq
  pprTypedUnaryPrim (u, t) = iConcat [ pprUnaryPrim u, iStr " : ", pprType t]