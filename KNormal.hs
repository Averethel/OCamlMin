module KNormal (
  convertToKNormal,
  KExpr(..)
) where
  import KNormal.Counter
  import KNormal.KNormalize
  import KNormal.KSyntax

  import TypedSyntax

  import Control.Monad.State

  convertToKNormal :: TypedExpr -> KExpr
  convertToKNormal e = fst $ runState (kNormalize e) emptyState