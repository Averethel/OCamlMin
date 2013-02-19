module AlphaConvert (alphaConvert, alphaConvertWithEnv) where
  import qualified AlphaConvert.AlphaConvert as AC
  import AlphaConvert.Counter
  import AlphaConvert.Env
  import KNormal.KSyntax

  import Control.Monad.State

  alphaConvert :: KExpr -> KExpr
  alphaConvert = alphaConvertWithEnv emptyEnv

  alphaConvertWithEnv :: Env -> KExpr -> KExpr
  alphaConvertWithEnv env e = fst $ runState (AC.alphaConvert env e) emptyState
