module ClosureConvert (
  ClosureConvert.closureConvert,
  Program(..)
) where
  import ClosureConvert.CSyntax
  import ClosureConvert.ClosureConvert

  import KNormal.KSyntax

  closureConvert :: KExpr -> IO Program
  closureConvert = ClosureConvert.ClosureConvert.closureConvert []
