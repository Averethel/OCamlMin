module TypedSyntax.BinaryPrim where
  import Syntax.BinaryPrim
  import Types

  import Utils.Iseq

  type TypedBinaryPrim = (BinaryPrim, Type)

  pprTypedBinaryPrim :: TypedBinaryPrim -> Iseq
  pprTypedBinaryPrim (u, t) = iConcat [ pprBinaryPrim u, iStr " : ", pprType t]
