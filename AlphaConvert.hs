module AlphaConvert (alphaConvert) where
  import qualified AlphaConvert.AlphaConvert as AC
  import AlphaConvert.Counter
  import AlphaConvert.Env
  import KNormal.KSyntax

  import Control.Monad.State

  alphaConvert :: KExpr -> KExpr
  alphaConvert e = fst $ runState (AC.alphaConvert emptyEnv e) emptyState