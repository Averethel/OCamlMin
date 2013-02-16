module KNormal (
  convertToKNormal,
  KExpr(..)
) where
  import KNormal.Counter
  import KNormal.KNormalize
  import KNormal.KSyntax

  import Syntax

  import Control.Monad.State

  convertToKNormal :: Expr -> KExpr
  convertToKNormal e = fst $ runState (kNormalize e) emptyState