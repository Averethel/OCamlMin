module Inline (inline, defaultTreshold) where
  import Inline.Env
  import qualified Inline.Inline as I

  import KNormal.KSyntax

  defaultTreshold :: Integer
  defaultTreshold = 0

  inline :: Integer -> KExpr -> IO KExpr
  inline = I.inline emptyEnv
