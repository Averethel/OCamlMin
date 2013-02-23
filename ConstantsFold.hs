module ConstantsFold (constantsFold) where
  import qualified ConstantsFold.ConstantsFold as CF
  import ConstantsFold.Env

  import KNormal.KSyntax

  constantsFold :: KExpr -> KExpr
  constantsFold = CF.constantsFold emptyEnv
